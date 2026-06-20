# mod_itinerary.R
# Module Shiny — Navigateur d'itinéraire voyage
#
# Utilisation dans une app :
#   UI     : mod_itinerary_ui("id")
#   Server : mod_itinerary_server("id", voyage_data)
#
# voyage_data : liste produite par build_voyage_hawaii.R et chargée depuis S3.
#               Structure attendue :
#                 $itineraires       — list(hawaii, oahu, bigisland, kauai) de bodies Markdown
#                 $highlights        — list(hawaii, oahu, ...) de tibbles (stem, title, lat, lng, tags, body)
#                 $sections          — list(oahu = list("28 juil." = stems[], ...), ...)
#                 $stem_to_section   — list(oahu = list(stem = heading, ...), ...)
#                 $itinerary_stems   — list(stem_fichier = tab_key, ...) pour liens croisés
#                 $s3_attach_prefix  — clé S3 de base pour les images (sans main_folder)

# ── Helpers internes ──────────────────────────────────────────────────────────

#' Vrai si les tags contiennent "hébergement"
#' @noRd
.it_is_hebergement <- function(tags) {
  any(stringr::str_detect(as.character(unlist(tags)), "hébergement"))
}

#' Vrai si le stem du fichier contient "aeroport" (sans accent)
#' @noRd
.it_is_aeroport <- function(stem) {
  stringr::str_detect(tolower(stem), "aeroport")
}

#' Formater les titres de sections avec dates en "MM/JJ — titre"
#' Ex. : "27 juil. — Arrivée" → "07/27 — Arrivée"
#' @noRd
.it_date_heading <- function(headings) {
  purrr::map_chr(headings, function(h) {
    if      (stringr::str_detect(h, "juil\\.")) mo <- "07"
    else if (stringr::str_detect(h, "août")) mo <- "08"
    else if (stringr::str_detect(h, "juin"))    mo <- "06"
    else if (stringr::str_detect(h, "mai"))     mo <- "05"
    else return(h)
    day  <- stringr::str_extract(h, "^\\d+")
    rest <- stringr::str_replace(h, "^\\d+(er)?\\s+\\S+\\s+—\\s+", "")
    paste0(mo, "/", sprintf("%02d", as.integer(day)), " — ", trimws(rest))
  })
}


#' Rendre un body Markdown Obsidian en HTML
#'
#' Pipeline :
#'   1. Markdown → HTML (GFM via commonmark)
#'   2. Images Obsidian ![[img.ext|opt]] → téléchargement S3 + <img> local
#'   3. Wikilinks [[note|alias]] et [[note]] → <a class="wikilink">
#'
#' @param body           Texte Markdown brut
#' @param img_dir        Dossier local où stocker les images téléchargées (NULL = ignorer)
#' @param img_url_prefix Préfixe URL pour servir les images locales (NULL = ignorer)
#' @param s3_prefix      Clé S3 de base des pièces jointes (sans main_folder)
#' @noRd
.it_render_body <- function(body,
                             img_dir        = NULL,
                             img_url_prefix = NULL,
                             s3_prefix      = NULL) {

  html <- commonmark::markdown_html(body, extensions = TRUE, hardbreaks = TRUE)

  # ── Images Obsidian ────────────────────────────────────────────────────────
  if (!is.null(img_dir) && !is.null(img_url_prefix) && !is.null(s3_prefix)) {

    # Extraire les noms de fichiers référencés (syntaxe ![[file.ext]] ou ![[file.ext|400]])
    refs <- stringr::str_match_all(html, "!\\[\\[([^|\\]]+)(?:\\|[^\\]]*)?\\]\\]")[[1]]

    if (nrow(refs) > 0) {
      for (img_file in unique(refs[, 2])) {
        local_path <- file.path(img_dir, img_file)
        if (!file.exists(local_path)) {
          s3_key <- paste0(s3_prefix, "/", img_file)
          tryCatch(
            # main_folder = FALSE : s3_prefix contient déjà le chemin complet
            s3db::s3download_HL(s3_key, local_path, main_folder = FALSE),
            error = function(e) message("⚠️  Image introuvable sur S3 : ", img_file)
          )
        }
      }
    }

    html <- stringr::str_replace_all(
      html,
      "!\\[\\[([^|\\]]+)(?:\\|[^\\]]*)?\\]\\]",
      paste0('<img src="/', img_url_prefix, '/\\1"',
             ' style="max-width:100%;border-radius:4px;margin:8px 0;">')
    )

  } else {
    # Contexte sans S3 (ex : test local) — supprimer les balises image
    html <- stringr::str_replace_all(html, "!\\[\\[([^\\]]+)\\]\\]", "")
  }

  # ── Wikilinks ──────────────────────────────────────────────────────────────
  # [[note|alias]]
  html <- stringr::str_replace_all(
    html,
    "\\[\\[([^|\\]]+)\\|([^\\]]+)\\]\\]",
    '<a href="#" class="wikilink" data-wikilink="\\1">\\2</a>'
  )
  # [[note]]
  html <- stringr::str_replace_all(
    html,
    "\\[\\[([^\\]]+)\\]\\]",
    '<a href="#" class="wikilink" data-wikilink="\\1">\\1</a>'
  )

  html
}

#' Découper un body Markdown en intro (avant ###) et sections nommées
#'
#' @return list(intro = character, sections = named list of character)
#' @noRd
.it_split_body <- function(body) {
  lines       <- strsplit(body, "\n")[[1]]
  heading_idx <- which(stringr::str_starts(lines, "### "))

  if (length(heading_idx) == 0) return(list(intro = body, sections = list()))

  intro <- if (heading_idx[1] > 1) {
    paste(lines[seq_len(heading_idx[1] - 1)], collapse = "\n")
  } else {
    ""
  }

  sections <- list()
  for (i in seq_along(heading_idx)) {
    start   <- heading_idx[i]
    end     <- if (i < length(heading_idx)) heading_idx[i + 1] - 1 else length(lines)
    heading <- stringr::str_remove(lines[start], "^### ")
    sections[[heading]] <- paste(lines[start:end], collapse = "\n")
  }
  list(intro = intro, sections = sections)
}

# ── Constantes UI (CSS / JS / icônes) ────────────────────────────────────────

#' @noRd
.it_css <- "
  a.wikilink {
    color: #2c7bb6; text-decoration: none;
    border-bottom: 1px dashed #2c7bb6; cursor: pointer;
  }
  a.wikilink:hover { color: #1a5276; border-bottom-style: solid; }
  .it-body {
    padding: 1rem 1.5rem;
    max-width: 860px;
  }
  .it-body table {
    width: 100%; border-collapse: collapse;
    margin-bottom: 1rem; font-size: 0.9rem;
  }
  .it-body th, .it-body td {
    border: 1px solid #dee2e6; padding: 0.35rem 0.75rem;
    text-align: left; vertical-align: top;
  }
  .it-body th { background-color: #f8f9fa; font-weight: 600; }
  .it-body blockquote {
    border-left: 3px solid #adb5bd;
    padding: 0.25rem 0 0.25rem 1rem;
    color: #6c757d; margin: 0.5rem 0;
  }
  .it-body input[type='checkbox'] { margin-right: 0.4rem; }
  .it-hl-header {
    display: flex; justify-content: space-between;
    align-items: center; margin-bottom: 0.5rem;
  }
  .bslib-sidebar-layout > .sidebar {
    position: sticky; top: 0;
    height: 100vh; overflow: hidden;
  }
  .bslib-sidebar-layout > .sidebar > .sidebar-content {
    display: flex; flex-direction: column;
    height: 100%; overflow: hidden; padding: 1rem;
  }
  .it-hl-body { flex: 1; min-height: 0; overflow-y: auto; padding-right: 0.25rem; }
  .it-map-section { flex-shrink: 0; margin-top: 0.5rem; }
"

#' @noRd
.it_js <- function(module_id) {
  paste0("
$(document).on('click', 'a.wikilink', function(e) {
  e.preventDefault();
  var stem = $(this).attr('data-wikilink');
  Shiny.setInputValue('", module_id, "-wikilink_clicked', stem, {priority: 'event'});
  setTimeout(function() {
    var sb = document.querySelector('.bslib-sidebar-layout > .sidebar');
    if (sb) sb.scrollTop = 0;
  }, 80);
});

/* Ouvre le sidebar bslib en cliquant son bouton toggle si nécessaire */
Shiny.addCustomMessageHandler('it_sidebar_open', function(msg) {
  var wrapper = document.getElementById(msg.id);
  if (!wrapper) return;
  var layout = wrapper.querySelector('.bslib-sidebar-layout');
  if (!layout) return;
  if (layout.classList.contains('sidebar-collapsed')) {
    var btn = layout.querySelector('.collapse-toggle');
    if (btn) btn.click();
  }
});

/* Ferme le sidebar bslib en cliquant son bouton toggle si nécessaire */
Shiny.addCustomMessageHandler('it_sidebar_close', function(msg) {
  var wrapper = document.getElementById(msg.id);
  if (!wrapper) return;
  var layout = wrapper.querySelector('.bslib-sidebar-layout');
  if (!layout) return;
  if (!layout.classList.contains('sidebar-collapsed')) {
    var btn = layout.querySelector('.collapse-toggle');
    if (btn) btn.click();
  }
});

/* Redimensionne la carte leaflet (height = '220px' | '420px', etc.) */
Shiny.addCustomMessageHandler('it_map_resize', function(msg) {
  var el = document.getElementById(msg.id);
  if (!el) return;
  el.style.height = msg.height;
  setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 60);
});
")
}

#' @noRd
.it_icons <- leaflet::awesomeIconList(
  selected    = leaflet::makeAwesomeIcon("map-marker", library = "fa", markerColor = "blue",      iconColor = "white"),
  day_point   = leaflet::makeAwesomeIcon("map-marker", library = "fa", markerColor = "red",       iconColor = "white"),
  hotel       = leaflet::makeAwesomeIcon("bed",        library = "fa", markerColor = "orange",    iconColor = "white"),
  aeroport    = leaflet::makeAwesomeIcon("plane",      library = "fa", markerColor = "cadetblue", iconColor = "white"),
  point_unsel = leaflet::makeAwesomeIcon("map-marker", library = "fa", markerColor = "gray",   iconColor = "white")
)

# ── UI ────────────────────────────────────────────────────────────────────────

#' UI du module navigateur d'itinéraire
#'
#' @param id ID du module Shiny
#'
#' @return Un `tagList` bslib compatible, à insérer dans n'importe quelle
#'   `page_*` bslib (page_navbar, page_fluid, etc.).
#' @export
mod_itinerary_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(.it_js(id))),
      shiny::tags$style(shiny::HTML(.it_css))
    ),

    shiny::div(
      id = ns("it_layout"),   # repère pour les handlers JS it_sidebar_open/close

      bslib::layout_sidebar(

      sidebar = bslib::sidebar(
        position = "right",
        id       = ns("highlight_sidebar"),
        open     = "closed",
        width    = 420,

        shiny::div(
          class = "it-hl-header",
          shiny::h6("\U0001f4cc Highlight", class = "m-0"),
          shiny::actionLink(ns("close_sidebar"), shiny::icon("xmark"), class = "text-muted")
        ),
        shiny::uiOutput(ns("hl_title")),
        shiny::hr(class = "mt-1 mb-2"),
        shiny::div(
          class = "it-hl-body it-body ps-0",
          shiny::uiOutput(ns("hl_content"))
        ),
        shiny::div(
          class = "it-map-section",
          shiny::div(
            class = "d-flex align-items-center gap-2 mb-1",
            shiny::hr(class = "mt-0 mb-0 flex-grow-1"),
            shiny::actionLink(
              ns("expand_map"),
              shiny::icon("expand"),
              class = "text-muted",
              style = "font-size:0.75rem; line-height:1;"
            )
          ),
          leaflet::leafletOutput(ns("hl_map"), height = "220px")
        )
      ),

      bslib::navset_tab(
        id = ns("tabs"),
        bslib::nav_panel(title = "\U0001f3dd️ Hawaii",    value = "hawaii",
                         shiny::div(class = "it-body", shiny::uiOutput(ns("tab_hawaii")))),
        bslib::nav_panel(title = "\U0001f3d9️ Oahu", value = "oahu",
          shiny::div(
            class = "it-body",
            shiny::uiOutput(ns("tab_oahu_intro")),
            shiny::uiOutput(ns("tab_oahu_picker")),
            shiny::uiOutput(ns("tab_oahu_section"))
          )
        ),
        bslib::nav_panel(title = "\U0001f30b Big Island", value = "bigisland",
          shiny::div(
            class = "it-body",
            shiny::uiOutput(ns("tab_bigisland_intro")),
            shiny::uiOutput(ns("tab_bigisland_picker")),
            shiny::uiOutput(ns("tab_bigisland_section"))
          )
        ),
        bslib::nav_panel(title = "\U0001f33f Kauaʿi", value = "kauai",
          shiny::div(
            class = "it-body",
            shiny::uiOutput(ns("tab_kauai_intro")),
            shiny::uiOutput(ns("tab_kauai_picker")),
            shiny::uiOutput(ns("tab_kauai_section"))
          )
        )
      )
    )        # fin layout_sidebar
    )        # fin div#it_layout
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

#' Server du module navigateur d'itinéraire
#'
#' @param id          ID du module Shiny (doit correspondre à `mod_itinerary_ui`)
#' @param voyage_data Liste ou reactive contenant les données du voyage.
#'   Produite par `build_voyage_hawaii.R` et chargée depuis S3.
#'
#' @export
mod_itinerary_server <- function(id, voyage_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Accepte une reactive ou une valeur fixe
    vd <- if (shiny::is.reactive(voyage_data)) voyage_data else shiny::reactive(voyage_data)

    # ── Images : dossier temp par session ──────────────────────────────────────
    # Chaque session reçoit son propre dossier isolé pour les images S3
    img_dir    <- file.path(tempdir(), session$token, "it_img")
    img_prefix <- paste0("it_img_", session$token)
    dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)
    shiny::addResourcePath(img_prefix, img_dir)

    # ── Rendu d'un onglet ──────────────────────────────────────────────────────
    .render_tab <- function(tab_key) {
      body <- vd()$itineraires[[tab_key]]
      shiny::HTML(.it_render_body(body, img_dir, img_prefix, vd()$s3_attach_prefix))
    }

    # ── Résolution d'un wikilink cliqué ───────────────────────────────────────
    # Cherche d'abord dans les highlights, puis dans les itinéraires (liens croisés)
    .resolve <- function(stem) {
      # 1. Highlights géolocalisés
      all_hl <- dplyr::bind_rows(vd()$highlights)
      hl     <- dplyr::filter(all_hl, .data$stem == !!stem)
      if (nrow(hl) > 0) {
        return(list(
          title = hl$title[[1]],
          html  = .it_render_body(hl$body[[1]], img_dir, img_prefix, vd()$s3_attach_prefix),
          lat   = hl$lat[[1]],
          lng   = hl$lng[[1]]
        ))
      }

      # 2. Liens croisés entre onglets (ex : [[Itineraire Oahu...]] depuis l'onglet Hawaii)
      it_stems <- vd()$itinerary_stems
      if (!is.null(it_stems) && stem %in% names(it_stems)) {
        tab_key <- it_stems[[stem]]
        body    <- vd()$itineraires[[tab_key]]
        return(list(
          title = stem,
          html  = .it_render_body(body, img_dir, img_prefix, vd()$s3_attach_prefix),
          lat   = NA_real_,
          lng   = NA_real_
        ))
      }

      # 3. Introuvable
      list(
        title = stem,
        html  = paste0("<p class='text-muted'><em>Note introuvable : <code>",
                       stem, "</code></em></p>"),
        lat   = NA_real_,
        lng   = NA_real_
      )
    }

    # ── Onglets ────────────────────────────────────────────────────────────────
    output$tab_hawaii <- shiny::renderUI(.render_tab("hawaii"))

    # ── Oahu : intro + sélecteur de journée (dates MM/JJ) ─────────────────────
    split_oahu <- shiny::reactive(.it_split_body(vd()$itineraires[["oahu"]]))

    output$tab_oahu_intro <- shiny::renderUI({
      shiny::HTML(.it_render_body(split_oahu()$intro, img_dir, img_prefix, vd()$s3_attach_prefix))
    })
    output$tab_oahu_picker <- shiny::renderUI({
      choices_raw <- names(split_oahu()$sections)
      shiny::req(length(choices_raw) > 0)
      # Valeur = heading original, label affiché = date formatée
      choices <- stats::setNames(choices_raw, .it_date_heading(choices_raw))
      shiny::selectInput(ns("sel_oahu"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_oahu_section <- shiny::renderUI({
      shiny::req(input$sel_oahu)
      shiny::HTML(.it_render_body(
        split_oahu()$sections[[input$sel_oahu]],
        img_dir, img_prefix, vd()$s3_attach_prefix
      ))
    })

    # ── Big Island : intro + sélecteur de journée ─────────────────────────────
    split_big <- shiny::reactive(.it_split_body(vd()$itineraires[["bigisland"]]))

    output$tab_bigisland_intro <- shiny::renderUI({
      shiny::HTML(.it_render_body(split_big()$intro, img_dir, img_prefix, vd()$s3_attach_prefix))
    })
    output$tab_bigisland_picker <- shiny::renderUI({
      choices <- names(split_big()$sections)
      shiny::req(length(choices) > 0)
      shiny::selectInput(ns("sel_bigisland"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_bigisland_section <- shiny::renderUI({
      shiny::req(input$sel_bigisland)
      shiny::HTML(.it_render_body(
        split_big()$sections[[input$sel_bigisland]],
        img_dir, img_prefix, vd()$s3_attach_prefix
      ))
    })

    # ── Kauai : intro + sélecteur de journée ──────────────────────────────────
    split_kauai <- shiny::reactive(.it_split_body(vd()$itineraires[["kauai"]]))

    output$tab_kauai_intro <- shiny::renderUI({
      shiny::HTML(.it_render_body(split_kauai()$intro, img_dir, img_prefix, vd()$s3_attach_prefix))
    })
    output$tab_kauai_picker <- shiny::renderUI({
      choices <- names(split_kauai()$sections)
      shiny::req(length(choices) > 0)
      shiny::selectInput(ns("sel_kauai"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_kauai_section <- shiny::renderUI({
      shiny::req(input$sel_kauai)
      shiny::HTML(.it_render_body(
        split_kauai()$sections[[input$sel_kauai]],
        img_dir, img_prefix, vd()$s3_attach_prefix
      ))
    })

    # ── État ───────────────────────────────────────────────────────────────────
    rv <- shiny::reactiveValues(selected_stem = NULL, map_expanded = FALSE)

    # ── Carte de base ──────────────────────────────────────────────────────────
    output$hl_map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
    })

    # ── Mise à jour carte ──────────────────────────────────────────────────────
    # Logique :
    #   Aucune sélection         → tous les points de l'onglet (gris) + hôtel (orange)
    #   Sélection dans une ###   → journée (rouge) + sélection (bleu) + hôtel (orange)
    #   Sélection hors section   → sélection (bleu) + hôtel (orange)
    shiny::observe({
      tab <- shiny::req(input$tabs)
      df  <- vd()$highlights[[tab]]
      sel <- rv$selected_stem

      if (is.null(df) || nrow(df) == 0) return()

      df <- dplyr::mutate(df,
        is_hotel    = purrr::map_lgl(tags, .it_is_hebergement),
        is_aeroport = .it_is_aeroport(.data$stem)
      )

      stem_to_sec <- vd()$stem_to_section[[tab]]

      df_show <- if (!is.null(sel) && sel %in% names(stem_to_sec)) {
        day_stems <- vd()$sections[[tab]][[stem_to_sec[[sel]]]]
        df |>
          dplyr::filter(.data$stem %in% day_stems | is_hotel | is_aeroport) |>
          dplyr::mutate(icon_key = dplyr::case_when(
            is_aeroport         ~ "aeroport",
            .data$stem %in% sel ~ "selected",
            is_hotel            ~ "hotel",
            TRUE                ~ "day_point"
          ))
      } else if (!is.null(sel)) {
        df |>
          dplyr::filter(.data$stem %in% sel | is_hotel | is_aeroport) |>
          dplyr::mutate(icon_key = dplyr::case_when(
            is_aeroport         ~ "aeroport",
            .data$stem %in% sel ~ "selected",
            TRUE                ~ "hotel"
          ))
      } else {
        dplyr::mutate(df, icon_key = dplyr::case_when(
          is_aeroport ~ "aeroport",
          is_hotel    ~ "hotel",
          TRUE        ~ "point_unsel"
        ))
      }

      if (nrow(df_show) == 0) return()
      pad <- if (nrow(df_show) == 1) 0.05 else 0.02

      leaflet::leafletProxy(ns("hl_map"), session) |>
        leaflet::clearMarkers() |>
        leaflet::addAwesomeMarkers(
          data  = df_show,
          lat   = ~lat, lng = ~lng,
          icon  = ~.it_icons[icon_key],
          popup = ~paste0("<b>", title, "</b>"),
          label = ~title
        ) |>
        leaflet::fitBounds(
          lng1 = min(df_show$lng) - pad, lat1 = min(df_show$lat) - pad,
          lng2 = max(df_show$lng) + pad, lat2 = max(df_show$lat) + pad
        )
    })

    # Titre initial : vide (aucun highlight sélectionné)
    output$hl_title <- shiny::renderUI(NULL)

    # Changement d'onglet -> reinitialiser la selection
    shiny::observeEvent(input$tabs, {
      rv$selected_stem <- NULL
      session$sendCustomMessage("it_sidebar_close", list(id = ns("it_layout")))
    }, ignoreInit = TRUE)

    # -- Clic wikilink --
    shiny::observeEvent(input$wikilink_clicked, {
      rv$selected_stem <- input$wikilink_clicked
      result <- .resolve(input$wikilink_clicked)
      output$hl_title   <- shiny::renderUI(
        shiny::p(result$title, class = "text-muted small mb-0 mt-0 text-truncate")
      )
      output$hl_content <- shiny::renderUI(shiny::HTML(result$html))
      session$sendCustomMessage("it_sidebar_open", list(id = ns("it_layout")))
    })

    # -- Fermeture sidebar --
    shiny::observeEvent(input$close_sidebar, {
      session$sendCustomMessage("it_sidebar_close", list(id = ns("it_layout")))
    })

    # ── Agrandissement carte ───────────────────────────────────────────────────
    shiny::observeEvent(input$expand_map, {
      rv$map_expanded <- !rv$map_expanded
      height <- if (rv$map_expanded) "420px" else "220px"
      session$sendCustomMessage("it_map_resize", list(id = ns("hl_map"), height = height))
    })

    # ── Nettoyage fin de session ───────────────────────────────────────────────
    session$onSessionEnded(function() {
      shiny::removeResourcePath(img_prefix)
      unlink(img_dir, recursive = TRUE)
    })
  })
}
