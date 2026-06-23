# mod_itinerary_hawaii_m.R
# Module Shiny — Navigateur d'itinéraire voyage (version mobile)
#
# Utilisation dans une app :
#   UI     : mod_itinerary_m_ui("id")
#   Server : mod_itinerary_m_server("id", voyage_data)
#
# Différences vs mod_itinerary_hawaii.R (desktop) :
#   - Pas de layout_sidebar() — les highlights s'ouvrent dans un modalDialog
#   - Le modal est découpé en deux : texte scrollable (haut) + carte (bas)
#   - La carte est un renderLeaflet complet (pas de proxy) — re-render à chaque sélection
#   - CSS adapté mobile : padding réduit, modal plein écran sur petit écran

# ── CSS ───────────────────────────────────────────────────────────────────────

#' @noRd
.itm_css <- "
  a.wikilink {
    color: #2c7bb6; text-decoration: none;
    border-bottom: 1px dashed #2c7bb6; cursor: pointer;
  }
  a.wikilink:hover { color: #1a5276; border-bottom-style: solid; }

  .itm-body {
    padding: 0.75rem 1rem;
  }
  .itm-body table {
    width: 100%; border-collapse: collapse;
    margin-bottom: 1rem; font-size: 0.85rem;
  }
  .itm-body th, .itm-body td {
    border: 1px solid #dee2e6; padding: 0.3rem 0.5rem;
    text-align: left; vertical-align: top;
  }
  .itm-body th { background-color: #f8f9fa; font-weight: 600; }
  .itm-body blockquote {
    border-left: 3px solid #adb5bd;
    padding: 0.25rem 0 0.25rem 0.75rem;
    color: #6c757d; margin: 0.5rem 0;
  }
  .itm-body input[type='checkbox'] { margin-right: 0.4rem; }
  .itm-body img { max-width: 100%; border-radius: 4px; margin: 6px 0; }

  /* Modal : plein écran sur mobile */
  @media (max-width: 576px) {
    .modal-dialog { margin: 0; max-width: 100%; }
    .modal-content { height: 100dvh; border-radius: 0; }
  }

  /* Layout interne du modal : texte haut + carte bas */
  .itm-modal-body {
    display: flex;
    flex-direction: column;
    height: calc(82vh - 120px);
  }
  .itm-modal-text {
    flex: 1;
    overflow-y: auto;
    min-height: 80px;
    padding-right: 0.25rem;
  }
  .itm-modal-map {
    flex-shrink: 0;
    margin-top: 0.75rem;
  }
"

# ── JS ────────────────────────────────────────────────────────────────────────

#' @noRd
.itm_js <- function(module_id) {
  paste0("
$(document).on('click', 'a.wikilink', function(e) {
  e.preventDefault();
  var stem = $(this).attr('data-wikilink');
  Shiny.setInputValue('", module_id, "-wikilink_clicked', stem, {priority: 'event'});
});

$(document).on('click', 'a.doc-link', function(e) {
  e.preventDefault();
  var file = $(this).attr('data-doc');
  Shiny.setInputValue('", module_id, "-doc_clicked', file, {priority: 'event'});
});

/* Ouvre un PDF dans un nouvel onglet */
Shiny.addCustomMessageHandler('it_open_url', function(msg) {
  window.open(msg.url, '_blank');
});

/* Redéclenche le resize après l'animation du modal Bootstrap
   pour que leaflet recalcule ses dimensions */
$(document).on('shown.bs.modal', function() {
  window.dispatchEvent(new Event('resize'));
});
")
}

# ── UI ────────────────────────────────────────────────────────────────────────

#' UI du module navigateur d'itinéraire (mobile)
#'
#' @param id ID du module Shiny
#' @export
mod_itinerary_m_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(.itm_js(id))),
      shiny::tags$style(shiny::HTML(.itm_css))
    ),

    bslib::navset_tab(
      id = ns("tabs"),

      bslib::nav_panel(title = "\U0001f3dd️ Hawaii", value = "hawaii",
        shiny::div(class = "itm-body", shiny::uiOutput(ns("tab_hawaii")))
      ),

      bslib::nav_panel(title = "\U0001f3d9️ Oahu", value = "oahu",
        shiny::div(
          class = "itm-body",
          shiny::uiOutput(ns("tab_oahu_intro")),
          shiny::uiOutput(ns("tab_oahu_picker")),
          shiny::uiOutput(ns("tab_oahu_section"))
        )
      ),

      bslib::nav_panel(title = "\U0001f30b Big Island", value = "bigisland",
        shiny::div(
          class = "itm-body",
          shiny::uiOutput(ns("tab_bigisland_intro")),
          shiny::uiOutput(ns("tab_bigisland_picker")),
          shiny::uiOutput(ns("tab_bigisland_section"))
        )
      ),

      bslib::nav_panel(title = "\U0001f33f Kauaʿi", value = "kauai",
        shiny::div(
          class = "itm-body",
          shiny::uiOutput(ns("tab_kauai_intro")),
          shiny::uiOutput(ns("tab_kauai_picker")),
          shiny::uiOutput(ns("tab_kauai_section"))
        )
      ),
      bslib::nav_panel(title = "\U0001f4c4 Documents", value = "docs",
        shiny::div(class = "itm-body", shiny::uiOutput(ns("tab_docs")))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

#' Server du module navigateur d'itinéraire (mobile)
#'
#' @param id          ID du module Shiny (doit correspondre à `mod_itinerary_m_ui`)
#' @param voyage_data Liste ou reactive contenant les données du voyage.
#'
#' @export
mod_itinerary_m_server <- function(id, voyage_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vd <- if (shiny::is.reactive(voyage_data)) voyage_data else shiny::reactive(voyage_data)

    # ── Images : dossier temp par session ─────────────────────────────────────
    img_dir    <- file.path(tempdir(), session$token, "itm_img")
    img_prefix <- paste0("itm_img_", session$token)
    dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)
    shiny::addResourcePath(img_prefix, img_dir)
    app_base <- sub("/$", "", shiny::isolate(session$clientData$url_pathname))

    # ── Helper rendu d'onglet ─────────────────────────────────────────────────
    .render_tab <- function(tab_key) {
      body <- vd()$itineraires[[tab_key]]
      shiny::HTML(.it_render_body(body, img_dir, img_prefix, vd()$s3_attach_prefix, app_base))
    }

    # ── Résolution d'un wikilink ──────────────────────────────────────────────
    .resolve <- function(stem) {
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
      it_stems <- vd()$itinerary_stems
      if (!is.null(it_stems) && stem %in% names(it_stems)) {
        tab_key <- it_stems[[stem]]
        return(list(
          title = stem,
          html  = .it_render_body(vd()$itineraires[[tab_key]], img_dir, img_prefix, vd()$s3_attach_prefix, app_base),
          lat   = NA_real_,
          lng   = NA_real_
        ))
      }
      list(
        title = stem,
        html  = paste0("<p class='text-muted'><em>Note introuvable : <code>", stem, "</code></em></p>"),
        lat   = NA_real_,
        lng   = NA_real_
      )
    }

    # ── Onglets ───────────────────────────────────────────────────────────────
    output$tab_hawaii <- shiny::renderUI(.render_tab("hawaii"))

    # Oahu
    split_oahu <- shiny::reactive(.it_split_body(vd()$itineraires[["oahu"]]))
    output$tab_oahu_intro <- shiny::renderUI({
      shiny::HTML(.it_render_body(split_oahu()$intro, img_dir, img_prefix, vd()$s3_attach_prefix, app_base))
    })
    output$tab_oahu_picker <- shiny::renderUI({
      choices_raw <- names(split_oahu()$sections)
      shiny::req(length(choices_raw) > 0)
      choices <- stats::setNames(choices_raw, .it_date_heading(choices_raw))
      shiny::selectInput(ns("sel_oahu"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_oahu_section <- shiny::renderUI({
      shiny::req(input$sel_oahu)
      shiny::HTML(.it_render_body(
        split_oahu()$sections[[input$sel_oahu]], img_dir, img_prefix, vd()$s3_attach_prefix, app_base
      ))
    })

    # Big Island
    split_big <- shiny::reactive(.it_split_body(vd()$itineraires[["bigisland"]]))
    output$tab_bigisland_intro <- shiny::renderUI({
      shiny::HTML(.it_render_body(split_big()$intro, img_dir, img_prefix, vd()$s3_attach_prefix, app_base))
    })
    output$tab_bigisland_picker <- shiny::renderUI({
      choices <- names(split_big()$sections)
      shiny::req(length(choices) > 0)
      shiny::selectInput(ns("sel_bigisland"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_bigisland_section <- shiny::renderUI({
      shiny::req(input$sel_bigisland)
      shiny::HTML(.it_render_body(
        split_big()$sections[[input$sel_bigisland]], img_dir, img_prefix, vd()$s3_attach_prefix, app_base
      ))
    })

    # Kauai
    split_kauai <- shiny::reactive(.it_split_body(vd()$itineraires[["kauai"]]))
    output$tab_kauai_intro <- shiny::renderUI({
      shiny::HTML(.it_render_body(split_kauai()$intro, img_dir, img_prefix, vd()$s3_attach_prefix, app_base))
    })
    output$tab_kauai_picker <- shiny::renderUI({
      choices <- names(split_kauai()$sections)
      shiny::req(length(choices) > 0)
      shiny::selectInput(ns("sel_kauai"), label = "Journée :", choices = choices, width = "100%")
    })
    output$tab_kauai_section <- shiny::renderUI({
      shiny::req(input$sel_kauai)
      shiny::HTML(.it_render_body(
        split_kauai()$sections[[input$sel_kauai]], img_dir, img_prefix, vd()$s3_attach_prefix, app_base
      ))
    })

    # ── État ──────────────────────────────────────────────────────────────────
    rv <- shiny::reactiveValues(selected_stem = NULL)

    shiny::observeEvent(input$tabs, {
      rv$selected_stem <- NULL
    }, ignoreInit = TRUE)

    # ── Clic wikilink → modal ─────────────────────────────────────────────────
    shiny::observeEvent(input$wikilink_clicked, {
      rv$selected_stem <- input$wikilink_clicked
      result <- .resolve(input$wikilink_clicked)

      shiny::showModal(shiny::modalDialog(
        title     = shiny::tagList(shiny::icon("map-pin"), " ", result$title),
        size      = "l",
        easyClose = TRUE,
        footer    = NULL,

        shiny::div(
          class = "itm-modal-body",

          # Haut : body du highlight (scrollable)
          shiny::div(
            class = "itm-modal-text itm-body",
            shiny::HTML(result$html)
          ),

          # Bas : carte (hauteur fixe)
          shiny::div(
            class = "itm-modal-map",
            leaflet::leafletOutput(ns("modal_map"), height = "240px")
          )
        )
      ))
    })

    # ── Carte du modal ────────────────────────────────────────────────────────
    # renderLeaflet complet (pas de proxy) — re-render à chaque changement de sélection.
    # Logique identique au module desktop :
    #   journée connue  → points du jour (rouge) + sélection (bleu) + hôtel + aéroport
    #   hors section    → sélection (bleu) + hôtel + aéroport
    output$modal_map <- leaflet::renderLeaflet({
      sel <- rv$selected_stem
      tab <- shiny::req(input$tabs)
      df  <- vd()$highlights[[tab]]

      base_map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

      if (is.null(df) || nrow(df) == 0) return(base_map)

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

      if (nrow(df_show) == 0) return(base_map)
      pad <- if (nrow(df_show) == 1) 0.05 else 0.02

      base_map |>
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

    # ── Documents PDF ─────────────────────────────────────────────────────────
    doc_dir    <- file.path(tempdir(), session$token, "itm_docs")
    doc_prefix <- paste0("itm_docs_", session$token)
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

    # ── Nettoyage fin de session ──────────────────────────────────────────────
    session$onSessionEnded(function() {
      shiny::removeResourcePath(img_prefix)
      unlink(img_dir, recursive = TRUE)
      shiny::removeResourcePath(doc_prefix)
      unlink(doc_dir, recursive = TRUE)
    })
  })
}
