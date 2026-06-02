# test_itinerary_hawaii.R
# App Shiny — Navigateur d'itinéraire Hawaii 2026
# Dépendances : shiny, bslib, leaflet, commonmark, stringr, dplyr, purrr, tibble

devtools::load_all()

library(shiny)
library(bslib)
library(leaflet)
library(commonmark)
library(stringr)
library(dplyr)
library(purrr)

# ── Chemins ───────────────────────────────────────────────────────────────────

VAULT         <- "C:/R/obsidian_notes"
ITINERARY_DIR <- file.path(
  VAULT, "2-Area/Intérêts/Voyages/itinéraire/itinéraire_vault/Hawaii"
)
ATTACH_DIR    <- file.path(VAULT, "3-Références/6-Fourre-tout/attachments")

addResourcePath("attachments", ATTACH_DIR)

# ── Index du vault ────────────────────────────────────────────────────────────

notes          <- read_notes(VAULT)
stems          <- tools::file_path_sans_ext(basename(notes$path))
wikilink_index <- setNames(notes$path, stems)

# ── Lecture des 4 corps d'itinéraire ─────────────────────────────────────────

.read_itinerary_body <- function(filename) {
  parse_note(file.path(ITINERARY_DIR, filename))$body
}

body_hawaii    <- .read_itinerary_body("Itinéraire Hawaii.md")
body_oahu      <- .read_itinerary_body("Itineraire Oahu - notes de planification.md")
body_bigisland <- .read_itinerary_body("Itineraire Big Island - journees types par region.md")
body_kauai     <- .read_itinerary_body("Itineraire Kauai - journees types par region.md")

# ── Helpers ───────────────────────────────────────────────────────────────────

#' Vrai si les tags contiennent "hébergement"
.is_hebergement <- function(tags) {
  any(stringr::str_detect(as.character(unlist(tags)), "hébergement"))
}

#' Extraire les stems de wikilinks d'un texte (exclut les images ![[...]])
.extract_wikilinks <- function(text) {
  m1 <- stringr::str_match_all(text, "(?<!!)\\[\\[([^|\\]]+)\\|[^\\]]+\\]\\]")[[1]]
  m2 <- stringr::str_match_all(text, "(?<!!)\\[\\[([^\\]]+)\\]\\]")[[1]]
  unique(c(
    if (nrow(m1) > 0) trimws(m1[, 2]) else character(0),
    if (nrow(m2) > 0) trimws(m2[, 2]) else character(0)
  ))
}

#' Parser un body en sections ### → liste nommée (heading → stems[])
#'
#' Les wikilinks présents avant le premier ### sont ignorés pour la carte
#' (hébergements et liens hors-journée) mais restent accessibles via
#' highlights_by_tab.
parse_body_sections <- function(body) {
  lines       <- strsplit(body, "\n")[[1]]
  heading_idx <- which(stringr::str_starts(lines, "### "))
  if (length(heading_idx) == 0) return(list())

  sections <- list()
  for (i in seq_along(heading_idx)) {
    start   <- heading_idx[i]
    end     <- if (i < length(heading_idx)) heading_idx[i + 1] - 1 else length(lines)
    heading <- stringr::str_remove(lines[start], "^### ")
    stems_i <- .extract_wikilinks(paste(lines[start:end], collapse = "\n"))
    if (length(stems_i) > 0) sections[[heading]] <- stems_i
  }
  sections
}

#' Construire le lookup inverse stem → section heading
build_stem_to_section <- function(sections) {
  result <- list()
  for (sec_name in names(sections)) {
    for (s in sections[[sec_name]]) {
      if (is.null(result[[s]])) result[[s]] <- sec_name  # première occurrence
    }
  }
  result
}

#' Construire le dataframe de highlights géolocalisés pour un onglet
build_tab_highlights <- function(body) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  stems_all <- .extract_wikilinks(body)

  rows <- purrr::map(stems_all, function(s) {
    if (!s %in% names(wikilink_index)) return(NULL)
    note <- parse_note(wikilink_index[[s]])
    loc  <- note$meta[["location"]]
    lat  <- if (!is.null(loc) && length(loc) >= 1) suppressWarnings(as.numeric(loc[[1]])) else NA_real_
    lng  <- if (!is.null(loc) && length(loc) >= 2) suppressWarnings(as.numeric(loc[[2]])) else NA_real_
    if (is.na(lat) || is.na(lng)) return(NULL)
    tags <- as.character(unlist(note$meta[["tags"]])) %||% character(0)
    tibble::tibble(
      stem  = s,
      title = note$meta[["title"]] %||% s,
      lat   = lat,
      lng   = lng,
      tags  = list(tags)
    )
  })
  dplyr::bind_rows(rows)
}

#' Convertir un body Markdown (Obsidian) en HTML
render_body <- function(body) {
  html <- commonmark::markdown_html(body, extensions = TRUE)
  html <- stringr::str_replace_all(
    html,
    "!\\[\\[([^\\]]+)\\]\\]",
    '<img src="attachments/\\1" style="max-width:100%;border-radius:4px;margin:8px 0;">'
  )
  html <- stringr::str_replace_all(
    html,
    "\\[\\[([^|\\]]+)\\|([^\\]]+)\\]\\]",
    '<a href="#" class="wikilink" data-wikilink="\\1">\\2</a>'
  )
  html <- stringr::str_replace_all(
    html,
    "\\[\\[([^\\]]+)\\]\\]",
    '<a href="#" class="wikilink" data-wikilink="\\1">\\1</a>'
  )
  html
}

#' Résoudre un stem → liste(html, lat, lng)
resolve_wikilink <- function(stem) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  if (stem %in% names(wikilink_index)) {
    note <- parse_note(wikilink_index[[stem]])
    loc  <- note$meta[["location"]]
    lat  <- if (!is.null(loc) && length(loc) >= 1) suppressWarnings(as.numeric(loc[[1]])) else NA_real_
    lng  <- if (!is.null(loc) && length(loc) >= 2) suppressWarnings(as.numeric(loc[[2]])) else NA_real_
    list(html = render_body(note$body), lat = lat, lng = lng)
  } else {
    list(
      html = paste0("<p class='text-muted'><em>Note introuvable : <code>", stem, "</code></em></p>"),
      lat  = NA_real_,
      lng  = NA_real_
    )
  }
}

# ── Données pré-calculées au démarrage ───────────────────────────────────────

highlights_by_tab <- list(
  hawaii    = build_tab_highlights(body_hawaii),
  oahu      = build_tab_highlights(body_oahu),
  bigisland = build_tab_highlights(body_bigisland),
  kauai     = build_tab_highlights(body_kauai)
)

# Sections ### par onglet : heading → stems[]
sections_by_tab <- list(
  hawaii    = parse_body_sections(body_hawaii),
  oahu      = parse_body_sections(body_oahu),
  bigisland = parse_body_sections(body_bigisland),
  kauai     = parse_body_sections(body_kauai)
)

# Lookup inverse : stem → section heading (première occurrence)
stem_to_section_by_tab <- purrr::map(sections_by_tab, build_stem_to_section)

# ── Icônes Leaflet ────────────────────────────────────────────────────────────
# selected   : point cliqué             — bleu
# day_point  : autres points de la même journée — rouge
# hotel      : hébergement (toujours visible)   — orange
# point_unsel: point sans sélection active      — gris

ICONS <- awesomeIconList(
  selected    = makeAwesomeIcon("map-marker", library = "fa", markerColor = "blue",   iconColor = "white"),
  day_point   = makeAwesomeIcon("map-marker", library = "fa", markerColor = "red",    iconColor = "white"),
  hotel       = makeAwesomeIcon("bed",        library = "fa", markerColor = "orange", iconColor = "white"),
  point_unsel = makeAwesomeIcon("map-marker", library = "fa", markerColor = "gray",   iconColor = "white")
)

# ── JS ────────────────────────────────────────────────────────────────────────

JS_WIKILINKS <- "
$(document).on('click', 'a.wikilink', function(e) {
  e.preventDefault();
  var stem = $(this).attr('data-wikilink');
  Shiny.setInputValue('wikilink_clicked', stem, {priority: 'event'});
  setTimeout(function() {
    var sb = document.querySelector('.bslib-sidebar-layout > .sidebar');
    if (sb) sb.scrollTop = 0;
  }, 80);
});
"

# ── CSS ───────────────────────────────────────────────────────────────────────

CSS_CUSTOM <- "
  a.wikilink {
    color: #2c7bb6;
    text-decoration: none;
    border-bottom: 1px dashed #2c7bb6;
    cursor: pointer;
  }
  a.wikilink:hover {
    color: #1a5276;
    border-bottom-style: solid;
  }
  .itinerary-body {
    padding: 1rem 1.5rem;
    max-width: 860px;
  }
  .itinerary-body table {
    width: 100%;
    border-collapse: collapse;
    margin-bottom: 1rem;
    font-size: 0.9rem;
  }
  .itinerary-body th,
  .itinerary-body td {
    border: 1px solid #dee2e6;
    padding: 0.35rem 0.75rem;
    text-align: left;
    vertical-align: top;
  }
  .itinerary-body th { background-color: #f8f9fa; font-weight: 600; }
  .itinerary-body blockquote {
    border-left: 3px solid #adb5bd;
    padding: 0.25rem 0 0.25rem 1rem;
    color: #6c757d;
    margin: 0.5rem 0;
  }
  .itinerary-body input[type='checkbox'] { margin-right: 0.4rem; }
  .highlight-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 0.5rem;
  }
  .bslib-sidebar-layout > .sidebar {
    position: sticky;
    top: 0;
    height: 100vh;
    overflow: hidden;
  }
  .bslib-sidebar-layout > .sidebar > .sidebar-content {
    display: flex;
    flex-direction: column;
    height: 100%;
    overflow: hidden;
    padding: 1rem;
  }
  .highlight-body {
    flex: 1;
    min-height: 0;
    overflow-y: auto;
    padding-right: 0.25rem;
  }
  .highlight-map-section { flex-shrink: 0; margin-top: 0.5rem; }
"

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_fluid(
  title = "Itinéraire Hawaii 2026",
  theme = bs_theme(bootswatch = "flatly"),

  tags$head(
    tags$script(HTML(JS_WIKILINKS)),
    tags$style(HTML(CSS_CUSTOM))
  ),

  layout_sidebar(

    sidebar = sidebar(
      position = "right",
      id       = "highlight_sidebar",
      open     = "closed",
      width    = 420,

      div(
        class = "highlight-header",
        h6("\U0001f4cc Highlight", class = "m-0"),
        actionLink("close_sidebar", icon("xmark"), class = "text-muted")
      ),
      hr(class = "mt-0 mb-2"),
      div(
        class = "highlight-body itinerary-body ps-0",
        uiOutput("highlight_content")
      ),
      div(
        class = "highlight-map-section",
        hr(class = "mb-1 mt-0"),
        leafletOutput("highlight_map", height = "220px")
      )
    ),

    navset_tab(
      id = "main_tabs",
      nav_panel(title = "\U0001f3dd️ Hawaii",    value = "hawaii",
                div(class = "itinerary-body", uiOutput("tab_hawaii"))),
      nav_panel(title = "\U0001f3d9️ Oahu",      value = "oahu",
                div(class = "itinerary-body", uiOutput("tab_oahu"))),
      nav_panel(title = "\U0001f30b Big Island", value = "bigisland",
                div(class = "itinerary-body", uiOutput("tab_bigisland"))),
      nav_panel(title = "\U0001f33f Kauaʿi",    value = "kauai",
                div(class = "itinerary-body", uiOutput("tab_kauai")))
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Onglets ──────────────────────────────────────────────────────────────────
  output$tab_hawaii    <- renderUI({ HTML(render_body(body_hawaii)) })
  output$tab_oahu      <- renderUI({ HTML(render_body(body_oahu)) })
  output$tab_bigisland <- renderUI({ HTML(render_body(body_bigisland)) })
  output$tab_kauai     <- renderUI({ HTML(render_body(body_kauai)) })

  # ── État ─────────────────────────────────────────────────────────────────────
  rv <- reactiveValues(selected_stem = NULL)

  # ── Carte de base ─────────────────────────────────────────────────────────────
  output$highlight_map <- renderLeaflet({
    leaflet() |> addProviderTiles(providers$CartoDB.Positron)
  })

  # ── Mise à jour de la carte ───────────────────────────────────────────────────
  # Logique :
  #   Pas de sélection  → tous les points de l'onglet en gris + hôtel orange
  #   Sélection connue  → points de la journée (rouge) + sélection (bleu) + hôtel (orange)
  #   Sélection hors ### → sélection (bleu) + hôtel (orange) seulement
  observe({
    tab <- req(input$main_tabs)
    df  <- highlights_by_tab[[tab]]
    sel <- rv$selected_stem

    if (is.null(df) || nrow(df) == 0) return()

    df <- dplyr::mutate(df, is_hotel = purrr::map_lgl(tags, .is_hebergement))

    stem_to_sec <- stem_to_section_by_tab[[tab]]

    df_show <- if (!is.null(sel) && sel %in% names(stem_to_sec)) {
      # Sélection dans une journée connue
      day_stems <- sections_by_tab[[tab]][[stem_to_sec[[sel]]]]
      df |>
        dplyr::filter(stem %in% day_stems | is_hotel) |>
        dplyr::mutate(
          icon_key = dplyr::case_when(
            stem %in% sel ~ "selected",
            is_hotel      ~ "hotel",
            TRUE          ~ "day_point"
          )
        )
    } else if (!is.null(sel)) {
      # Sélection hors section (ex: hôtel, lien parent)
      df |>
        dplyr::filter(stem %in% sel | is_hotel) |>
        dplyr::mutate(
          icon_key = dplyr::case_when(
            stem %in% sel ~ "selected",
            TRUE          ~ "hotel"
          )
        )
    } else {
      # Aucune sélection : tous les points
      dplyr::mutate(df, icon_key = ifelse(is_hotel, "hotel", "point_unsel"))
    }

    if (nrow(df_show) == 0) return()

    padding <- if (nrow(df_show) == 1) 0.05 else 0.02

    leafletProxy("highlight_map", session) |>
      clearMarkers() |>
      addAwesomeMarkers(
        data  = df_show,
        lat   = ~lat,
        lng   = ~lng,
        icon  = ~ICONS[icon_key],
        popup = ~paste0("<b>", title, "</b>"),
        label = ~title
      ) |>
      fitBounds(
        lng1 = min(df_show$lng) - padding, lat1 = min(df_show$lat) - padding,
        lng2 = max(df_show$lng) + padding, lat2 = max(df_show$lat) + padding
      )
  })

  # Changement d'onglet → réinitialiser la sélection
  observeEvent(input$main_tabs, {
    rv$selected_stem <- NULL
  }, ignoreInit = TRUE)

  # ── Clic wikilink ─────────────────────────────────────────────────────────────
  observeEvent(input$wikilink_clicked, {
    rv$selected_stem <- input$wikilink_clicked
    result <- resolve_wikilink(input$wikilink_clicked)
    output$highlight_content <- renderUI({ HTML(result$html) })
    toggle_sidebar("highlight_sidebar", open = TRUE)
  })

  # ── Fermeture sidebar ─────────────────────────────────────────────────────────
  observeEvent(input$close_sidebar, {
    rv$selected_stem <- NULL
    toggle_sidebar("highlight_sidebar", open = FALSE)
  })
}

# ── Lancement ─────────────────────────────────────────────────────────────────

shinyApp(ui, server)
