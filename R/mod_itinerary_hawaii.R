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

`%||%` <- function(x, y) if (is.null(x)) y else x

# Retire la partie "|alias" d'un stem Obsidian ([[note|alias]] → "note")
.stem_key <- function(x) sub("\\|.*$", "", x)

# Journée réservée "Ne pas faire" — présente dans le Day Planner, absente des onglets île
.dp_npf <- "⛔ Ne pas faire"

# Garantit que la journée réservée existe (en dernier) dans le plan
.dp_ensure_npf <- function(plan) {
  if (!.dp_npf %in% names(plan$days))
    plan$days[[.dp_npf]] <- character(0)
  plan
}

#' Sauvegarde un plan île sur S3
#' @noRd
.dp_save <- function(ile, plan) {
  s3db::s3saveRDS_HL(plan, paste0("obsidianr/plan_", ile, ".rds"))
}

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

#' Mettre à jour les marqueurs de la carte (marqueur sélectionné toujours au-dessus)
#' @noRd
.it_map_update <- function(proxy, df_show) {
  if (nrow(df_show) == 0) return(invisible(NULL))
  pad         <- if (nrow(df_show) == 1) 0.05 else 0.02
  df_other    <- dplyr::filter(df_show, .data$icon_key != "selected")
  df_selected <- dplyr::filter(df_show, .data$icon_key == "selected")
  proxy <- leaflet::clearMarkers(proxy)
  if (nrow(df_other) > 0)
    proxy <- leaflet::addAwesomeMarkers(proxy,
      data = df_other, lat = ~lat, lng = ~lng,
      icon = ~.it_icons[icon_key],
      popup = ~paste0("<b>", title, "</b>"), label = ~title)
  if (nrow(df_selected) > 0)
    proxy <- leaflet::addAwesomeMarkers(proxy,
      data    = df_selected, lat = ~lat, lng = ~lng,
      icon    = ~.it_icons[icon_key],
      popup   = ~paste0("<b>", title, "</b>"), label = ~title,
      options = leaflet::markerOptions(zIndexOffset = 1000))
  leaflet::fitBounds(proxy,
    lng1 = min(df_show$lng) - pad, lat1 = min(df_show$lat) - pad,
    lng2 = max(df_show$lng) + pad, lat2 = max(df_show$lat) + pad)
}

#' Rendre la liste d'items planifiés pour un onglet île
#' @noRd
.it_plan_items_ui <- function(ile, day, rv_plans, df) {
  day_stems <- rv_plans[[ile]]$days[[day]] %||% character(0)

  if (length(day_stems) == 0) {
    return(shiny::p("Aucun item planifié pour cette journée.", class = "text-muted"))
  }

  make_link <- function(stem) {
    key   <- .stem_key(stem)
    title <- if (!is.null(df) && key %in% df$stem) df$title[df$stem == key][[1]] else key
    shiny::div(class = "py-1 border-bottom",
      shiny::tags$a(href = "#", class = "wikilink", `data-wikilink` = key, title)
    )
  }

  shiny::tagList(lapply(day_stems, make_link))
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
#' @param base_path      Préfixe du chemin de l'app (ex: "/hawaii" derrière un reverse proxy)
#' @noRd
.it_render_body <- function(body,
                             img_dir        = NULL,
                             img_url_prefix = NULL,
                             s3_prefix      = NULL,
                             base_path      = "") {

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
      paste0('<img src="', base_path, '/', img_url_prefix, '/\\1"',
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

$(document).on('click', 'a.doc-link', function(e) {
  e.preventDefault();
  var file = $(this).attr('data-doc');
  Shiny.setInputValue('", module_id, "-doc_clicked', file, {priority: 'event'});
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

/* Ouvre un PDF dans un nouvel onglet */
Shiny.addCustomMessageHandler('it_open_url', function(msg) {
  window.open(msg.url, '_blank');
});
")
}

#' @noRd
.docs_hawaii <- list(
  list(label = "Assurance voyage — Annie (MC Elite World BNC)", file = "assurance-annie-bnc.pdf"),
  list(label = "Assurance voyage — Hugo (Amex Gold Scotia)",    file = "assurance-hugo-amex.pdf"),
  list(label = "Assurance auto",                                      file = "assurance-auto.pdf"),
  list(label = "Passeport — Hugo",                              file = "passport-hugo.pdf"),
  list(label = "Passeport — Annie",                             file = "passport-annie.pdf"),
  list(label = "Passeport — Léa",                         file = "passport-lea.pdf"),
  list(label = "Réservations (auto, hôtels, vols)",        file = "reservations.pdf")
)

#' @noRd
.it_icons <- leaflet::awesomeIconList(
  selected    = leaflet::makeAwesomeIcon("map-marker", library = "fa", markerColor = "red",       iconColor = "white"),
  day_point   = leaflet::makeAwesomeIcon("map-marker", library = "fa", markerColor = "green",     iconColor = "white"),
  hotel       = leaflet::makeAwesomeIcon("bed",        library = "fa", markerColor = "purple",    iconColor = "white"),
  aeroport    = leaflet::makeAwesomeIcon("plane",      library = "fa", markerColor = "purple",    iconColor = "white"),
  point_unsel = leaflet::makeAwesomeIcon("map-marker", library = "fa", markerColor = "gray",      iconColor = "white")
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
        ),
        bslib::nav_panel(title = "\U0001f4c5 Day Planner", value = "dayplanner",
          shiny::div(class = "it-body", shiny::uiOutput(ns("dp_content")))
        ),
        bslib::nav_panel(title = "\U0001f4c4 Documents", value = "docs",
          shiny::div(class = "it-body", shiny::uiOutput(ns("tab_docs")))
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
    app_base <- sub("/$", "", shiny::isolate(session$clientData$url_pathname))

    # ── Rendu d'un onglet ──────────────────────────────────────────────────────
    .render_tab <- function(tab_key) {
      body <- vd()$itineraires[[tab_key]]
      shiny::HTML(.it_render_body(body, img_dir, img_prefix, vd()$s3_attach_prefix, app_base))
    }

    # ── Résolution d'un wikilink cliqué ───────────────────────────────────────
    # Cherche d'abord dans les highlights, puis dans les itinéraires (liens croisés)
    .resolve <- function(stem) {
      stem <- .stem_key(stem)
      # 1. Highlights géolocalisés
      all_hl <- dplyr::bind_rows(vd()$highlights)
      hl     <- dplyr::filter(all_hl, .data$stem == !!stem)
      if (nrow(hl) > 0) {
        return(list(
          title = hl$title[[1]],
          html  = .it_render_body(hl$body[[1]], img_dir, img_prefix, vd()$s3_attach_prefix, app_base),
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
          html  = .it_render_body(body, img_dir, img_prefix, vd()$s3_attach_prefix, app_base),
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
      shiny::HTML(.it_render_body(split_oahu()$intro, img_dir, img_prefix, vd()$s3_attach_prefix, app_base))
    })
    output$tab_oahu_picker <- shiny::renderUI({
      days <- setdiff(names(rv_plans$oahu$days), .dp_npf)
      shiny::req(length(days) > 0)
      choices <- stats::setNames(days, .it_date_heading(days))
      shiny::selectInput(ns("sel_oahu"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_oahu_section <- shiny::renderUI({
      shiny::req(input$sel_oahu)
      .it_plan_items_ui("oahu", input$sel_oahu, rv_plans, vd()$highlights[["oahu"]])
    })

    # ── Big Island : intro + sélecteur de journée ─────────────────────────────
    split_big <- shiny::reactive(.it_split_body(vd()$itineraires[["bigisland"]]))

    output$tab_bigisland_intro <- shiny::renderUI({
      shiny::HTML(.it_render_body(split_big()$intro, img_dir, img_prefix, vd()$s3_attach_prefix, app_base))
    })
    output$tab_bigisland_picker <- shiny::renderUI({
      days <- setdiff(names(rv_plans$bigisland$days), .dp_npf)
      shiny::req(length(days) > 0)
      choices <- stats::setNames(days, .it_date_heading(days))
      shiny::selectInput(ns("sel_bigisland"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_bigisland_section <- shiny::renderUI({
      shiny::req(input$sel_bigisland)
      .it_plan_items_ui("bigisland", input$sel_bigisland, rv_plans, vd()$highlights[["bigisland"]])
    })

    # ── Kauai : intro + sélecteur de journée ──────────────────────────────────
    split_kauai <- shiny::reactive(.it_split_body(vd()$itineraires[["kauai"]]))

    output$tab_kauai_intro <- shiny::renderUI({
      shiny::HTML(.it_render_body(split_kauai()$intro, img_dir, img_prefix, vd()$s3_attach_prefix, app_base))
    })
    output$tab_kauai_picker <- shiny::renderUI({
      days <- setdiff(names(rv_plans$kauai$days), .dp_npf)
      shiny::req(length(days) > 0)
      choices <- stats::setNames(days, .it_date_heading(days))
      shiny::selectInput(ns("sel_kauai"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_kauai_section <- shiny::renderUI({
      shiny::req(input$sel_kauai)
      .it_plan_items_ui("kauai", input$sel_kauai, rv_plans, vd()$highlights[["kauai"]])
    })

    # ── État ───────────────────────────────────────────────────────────────────
    rv <- shiny::reactiveValues(selected_stem = NULL, map_expanded = FALSE, hl_result = NULL)

    output$hl_title <- shiny::renderUI({
      res <- rv$hl_result
      if (is.null(res)) return(NULL)
      shiny::p(res$title, class = "text-muted small mb-0 mt-0 text-truncate")
    })
    output$hl_content <- shiny::renderUI({
      res <- rv$hl_result
      if (is.null(res)) return(NULL)
      shiny::HTML(res$html)
    })

    # ── Carte de base ──────────────────────────────────────────────────────────
    output$hl_map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
    })

    # ── Mise à jour carte ──────────────────────────────────────────────────────
    # Logique onglets île  : journée .md (vert) + sélection (rouge) + hôtel/aéroport (mauve)
    # Logique dayplanner   : journée plan (vert) + sélection (rouge) + hôtel/aéroport (mauve)
    shiny::observe({
      tab <- shiny::req(input$tabs)
      sel <- rv$selected_stem

      # ── Branche Day Planner ──────────────────────────────────────────────────
      if (tab == "dayplanner") {
        ile  <- input$dp_island %||% "oahu"
        day  <- input$dp_day
        df   <- vd()$highlights[[ile]]
        if (is.null(df) || nrow(df) == 0 || is.null(day)) return()

        df <- dplyr::mutate(df,
          is_hotel    = purrr::map_lgl(tags, .it_is_hebergement),
          is_aeroport = .it_is_aeroport(.data$stem)
        )

        day_stems <- .stem_key(rv_plans[[ile]]$days[[day]] %||% character(0))

        df_show <- df |>
          dplyr::filter(.data$stem %in% c(sel, day_stems) | is_hotel | is_aeroport) |>
          dplyr::mutate(icon_key = dplyr::case_when(
            is_aeroport           ~ "aeroport",
            .data$stem %in% sel   ~ "selected",
            is_hotel              ~ "hotel",
            TRUE                  ~ "day_point"
          ))

        .it_map_update(leaflet::leafletProxy(ns("hl_map"), session), df_show)
        return()
      }

      # ── Branche onglets île ──────────────────────────────────────────────────
      ile <- tab
      if (!ile %in% c("oahu", "bigisland", "kauai")) return()

      sel_day <- switch(ile,
        oahu      = input$sel_oahu,
        bigisland = input$sel_bigisland,
        kauai     = input$sel_kauai
      )

      df <- vd()$highlights[[ile]]
      if (is.null(df) || nrow(df) == 0) return()

      df <- dplyr::mutate(df,
        is_hotel    = purrr::map_lgl(tags, .it_is_hebergement),
        is_aeroport = .it_is_aeroport(.data$stem)
      )

      day_stems <- .stem_key(if (!is.null(sel_day)) rv_plans[[ile]]$days[[sel_day]] %||% character(0) else character(0))

      df_show <- df |>
        dplyr::filter(.data$stem %in% c(sel, day_stems) | is_hotel | is_aeroport) |>
        dplyr::mutate(icon_key = dplyr::case_when(
          is_aeroport             ~ "aeroport",
          .data$stem %in% sel     ~ "selected",
          is_hotel                ~ "hotel",
          TRUE                    ~ "day_point"
        ))

      .it_map_update(leaflet::leafletProxy(ns("hl_map"), session), df_show)
    })

    # Changement d'onglet -> reinitialiser la selection
    shiny::observeEvent(input$tabs, {
      rv$selected_stem <- NULL
      rv$hl_result     <- NULL
      session$sendCustomMessage("it_sidebar_close", list(id = ns("it_layout")))
    }, ignoreInit = TRUE)

    # -- Clic wikilink --
    shiny::observeEvent(input$wikilink_clicked, {
      rv$selected_stem <- input$wikilink_clicked
      rv$hl_result     <- .resolve(input$wikilink_clicked)
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

    # ── Day Planner ───────────────────────────────────────────────────────────

    rv_plans <- shiny::reactiveValues(
      oahu      = .dp_ensure_npf(s3db::s3readRDS_HL("obsidianr/plan_oahu.rds")),
      bigisland = .dp_ensure_npf(s3db::s3readRDS_HL("obsidianr/plan_bigisland.rds")),
      kauai     = .dp_ensure_npf(s3db::s3readRDS_HL("obsidianr/plan_kauai.rds"))
    )

    dp_ile <- shiny::reactive(input$dp_island %||% "oahu")

    dp_plan <- shiny::reactive(rv_plans[[dp_ile()]])

    dp_day_names <- shiny::reactive(names(dp_plan()$days))

    output$dp_content <- shiny::renderUI({
      shiny::tagList(
        shiny::div(class = "row g-2 mb-2",
          shiny::div(class = "col-md-4",
            shiny::selectInput(ns("dp_island"), "Île :",
              choices = c("Oahu" = "oahu", "Big Island" = "bigisland", "Kauaʿi" = "kauai"),
              width = "100%"
            )
          )
        ),
        shiny::uiOutput(ns("dp_day_row")),
        shiny::hr(class = "my-2"),
        shiny::uiOutput(ns("dp_table"))
      )
    })

    output$dp_day_row <- shiny::renderUI({
      days <- dp_day_names()
      if (length(days) == 0) return(shiny::p("Aucune journée.", class = "text-muted"))
      current    <- shiny::isolate(input$dp_day)
      selected   <- if (!is.null(current) && current %in% days) current else days[[1]]
      is_special <- isTRUE(selected == .dp_npf)
      shiny::div(class = "d-flex gap-1 align-items-end mb-2",
        shiny::div(style = "flex:1;",
          shiny::selectInput(ns("dp_day"), "Journée :", choices = days,
                             selected = selected, width = "100%")
        ),
        if (!is_special) shiny::actionButton(ns("dp_day_up"),     shiny::icon("arrow-up"),
                            class = "btn-sm btn-outline-secondary", title = "Monter"),
        if (!is_special) shiny::actionButton(ns("dp_day_down"),   shiny::icon("arrow-down"),
                            class = "btn-sm btn-outline-secondary", title = "Descendre"),
        if (!is_special) shiny::actionButton(ns("dp_day_rename"), shiny::icon("pencil"),
                            class = "btn-sm btn-outline-secondary", title = "Renommer"),
        if (!is_special) shiny::actionButton(ns("dp_day_add"),    shiny::icon("plus"),
                            class = "btn-sm btn-outline-success",   title = "Ajouter"),
        if (!is_special) shiny::actionButton(ns("dp_day_delete"), shiny::icon("trash"),
                            class = "btn-sm btn-outline-danger",    title = "Supprimer")
      )
    })

    shiny::observeEvent(input$dp_day_up, {
      ile  <- dp_ile()
      day  <- shiny::req(input$dp_day)
      days <- rv_plans[[ile]]$days
      idx  <- which(names(days) == day)
      if (length(idx) == 0 || idx <= 1) return()
      ord        <- seq_along(days)
      ord[c(idx - 1, idx)] <- ord[c(idx, idx - 1)]
      rv_plans[[ile]]$days <- days[ord]
      .dp_save(ile, rv_plans[[ile]])
    })

    shiny::observeEvent(input$dp_day_down, {
      ile  <- dp_ile()
      day  <- shiny::req(input$dp_day)
      days <- rv_plans[[ile]]$days
      idx  <- which(names(days) == day)
      if (length(idx) == 0 || idx >= length(days)) return()
      ord        <- seq_along(days)
      ord[c(idx, idx + 1)] <- ord[c(idx + 1, idx)]
      rv_plans[[ile]]$days <- days[ord]
      .dp_save(ile, rv_plans[[ile]])
    })

    shiny::observeEvent(input$dp_day_rename, {
      shiny::showModal(shiny::modalDialog(
        title = "Renommer la journée",
        shiny::textInput(ns("dp_rename_text"), "Nouveau nom :", value = input$dp_day, width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("dp_rename_confirm"), "Renommer", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$dp_rename_confirm, {
      shiny::removeModal()
      ile      <- dp_ile()
      old_name <- shiny::req(input$dp_day)
      new_name <- trimws(input$dp_rename_text)
      shiny::req(nchar(new_name) > 0, old_name != new_name)
      days <- rv_plans[[ile]]$days
      names(days)[names(days) == old_name] <- new_name
      rv_plans[[ile]]$days <- days
      .dp_save(ile, rv_plans[[ile]])
    })

    shiny::observeEvent(input$dp_day_add, {
      shiny::showModal(shiny::modalDialog(
        title = "Nouvelle journée",
        shiny::textInput(ns("dp_add_text"), "Nom :", width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("dp_add_confirm"), "Ajouter", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$dp_add_confirm, {
      shiny::removeModal()
      ile      <- dp_ile()
      day      <- input$dp_day
      new_name <- trimws(input$dp_add_text)
      shiny::req(nchar(new_name) > 0)
      days <- rv_plans[[ile]]$days
      idx  <- if (!is.null(day) && day %in% names(days)) which(names(days) == day) else length(days)
      before <- if (idx > 0) days[seq_len(idx)] else list()
      after  <- if (idx < length(days)) days[seq(idx + 1, length(days))] else list()
      rv_plans[[ile]]$days <- c(before, stats::setNames(list(character(0)), new_name), after)
      .dp_save(ile, rv_plans[[ile]])
    })

    shiny::observeEvent(input$dp_day_delete, {
      shiny::showModal(shiny::modalDialog(
        title = "Supprimer la journée",
        paste0('Supprimer "', input$dp_day, '" ? Les highlights seront désassignés.'),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("dp_delete_confirm"), "Supprimer", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$dp_delete_confirm, {
      shiny::removeModal()
      ile  <- dp_ile()
      day  <- shiny::req(input$dp_day)
      days <- rv_plans[[ile]]$days
      rv_plans[[ile]]$days <- days[names(days) != day]
      .dp_save(ile, rv_plans[[ile]])
    })

    output$dp_table <- shiny::renderUI({
      ile  <- dp_ile()
      day  <- shiny::req(input$dp_day)
      df   <- vd()$highlights[[ile]]
      plan <- dp_plan()
      shiny::req(!is.null(df) && nrow(df) > 0)

      other_stems   <- .stem_key(unlist(plan$days[names(plan$days) != day], use.names = FALSE))
      current_stems <- .stem_key(plan$days[[day]] %||% character(0))
      fixed_stems   <- unique(c(
        df$stem[purrr::map_lgl(df$tags, .it_is_hebergement)],
        df$stem[.it_is_aeroport(df$stem)]
      ))

      df_avail <- dplyr::filter(df,
        !.data$stem %in% other_stems,
        !.data$stem %in% fixed_stems
      )

      if (nrow(df_avail) == 0) {
        return(shiny::p("Aucun highlight disponible.", class = "text-muted"))
      }

      choice_names <- lapply(seq_len(nrow(df_avail)), function(i) {
        s <- df_avail$stem[i]
        shiny::tags$a(href = "#", class = "wikilink", `data-wikilink` = s, df_avail$title[i])
      })

      label <- if (isTRUE(day == .dp_npf)) "\U26d4 À ne pas faire" else "\U0001f5fa️ Highlights disponibles"
      shiny::tagList(
        shiny::h6(label, class = "mb-1"),
        shiny::checkboxGroupInput(ns("dp_selected"), NULL,
          choiceNames  = choice_names,
          choiceValues = df_avail$stem,
          selected     = intersect(current_stems, df_avail$stem)
        )
      )
    })

    shiny::observeEvent(input$dp_selected, {
      ile <- dp_ile()
      day <- shiny::req(input$dp_day)
      rv_plans[[ile]]$days[[day]] <- input$dp_selected %||% character(0)
      .dp_save(ile, rv_plans[[ile]])
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ── Documents PDF ─────────────────────────────────────────────────────────
    doc_dir    <- file.path(tempdir(), session$token, "it_docs")
    doc_prefix <- paste0("it_docs_", session$token)
    dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)
    shiny::addResourcePath(doc_prefix, doc_dir)

    output$tab_docs <- shiny::renderUI({
      items <- purrr::map(.docs_hawaii, function(d) {
        shiny::div(
          class = "d-flex justify-content-between align-items-center py-2 border-bottom",
          shiny::span(d$label),
          shiny::tags$a(
            href = "#", class = "btn btn-sm btn-outline-primary doc-link",
            `data-doc` = d$file,
            shiny::icon("file-pdf"), " Voir"
          )
        )
      })
      shiny::tagList(shiny::h6("\U0001f4c4 Documents du voyage", class = "mb-3"), items)
    })

    shiny::observeEvent(input$doc_clicked, {
      doc_file   <- input$doc_clicked
      local_path <- file.path(doc_dir, doc_file)
      if (!file.exists(local_path)) {
        tryCatch(
          s3db::s3download_HL(paste0("docs/hawaii/", doc_file), local_path, main_folder = FALSE),
          error = function(e) message("⚠️  Document introuvable sur S3 : ", doc_file)
        )
      }
      if (file.exists(local_path)) {
        session$sendCustomMessage("it_open_url",
          list(url = paste0(app_base, "/", doc_prefix, "/", doc_file)))
      }
    })

    # ── Nettoyage fin de session ───────────────────────────────────────────────
    session$onSessionEnded(function() {
      shiny::removeResourcePath(img_prefix)
      unlink(img_dir, recursive = TRUE)
      shiny::removeResourcePath(doc_prefix)
      unlink(doc_dir, recursive = TRUE)
    })
  })
}
