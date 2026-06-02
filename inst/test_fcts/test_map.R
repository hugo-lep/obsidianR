# test_map.R
# App Shiny minimaliste — carte des highlights du vault
# Dépendances : shiny, bslib, leaflet, dplyr, purrr

devtools::load_all()

library(shiny)
library(bslib)
library(leaflet)

# ── Préparation des données (une seule fois au démarrage) ─────────────────────

vault <- "C:/R/obsidian_notes"

notes      <- read_notes(vault)
highlights <- read_highlights(notes = notes)

# Choix pour les filtres
continents <- c("Tous", sort(unique(na.omit(highlights$continent))))
pays_dispo <- c("Tous", sort(unique(na.omit(highlights$pays))))
tags_dispo <- c("Tous", sort(unique(unlist(highlights$tags))))

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Vault — Highlights",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 280,

    selectInput("continent", "Continent", choices = continents, selected = "Tous"),
    selectInput("pays",      "Pays",      choices = pays_dispo, selected = "Tous"),
    selectInput("tag",       "Tag",       choices = tags_dispo, selected = "Tous"),

    checkboxInput("deja_vue", "Déjà vus seulement", value = FALSE),

    hr(),
    textOutput("n_highlights")
  ),

  card(
    full_screen = TRUE,
    leafletOutput("map", height = "600px")
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  filtered <- reactive({
    df <- highlights

    if (input$continent != "Tous") df <- df |> dplyr::filter(continent == input$continent)
    if (input$pays      != "Tous") df <- df |> dplyr::filter(pays      == input$pays)
    if (input$tag       != "Tous") df <- df |> dplyr::filter(purrr::map_lgl(tags, ~ input$tag %in% .x))
    if (input$deja_vue)            df <- df |> dplyr::filter(deja_vue  == TRUE)

    df |> dplyr::mutate(
      couleur  = ifelse(deja_vue, "#2d8a4e", "#2c7bb6"),
      tags_str = unname(purrr::map_chr(tags, ~ paste(.x, collapse = " · "))),
      # Fil géographique par ligne (rowwise via pmap)
      geo_str  = unname(purrr::pmap_chr(
        list(ville, admin, pays, continent),
        ~ paste(na.omit(c(..1, ..2, ..3, ..4)), collapse = " · ")
      )),
      # Premier paragraphe du corps, nettoyé
      description = unname(purrr::map_chr(body, ~ {
        texte <- stringr::str_split(.x, "\n\n|\n---|\n##")[[1]][1]
        texte <- gsub("\\[\\[|\\]\\]", "", texte)   # enlever [[ et ]] simplement
        texte <- gsub("\\*\\*|\\*|`", "", texte)     # enlever gras/italique/code
        stringr::str_trim(stringr::str_trunc(texte, 280))
      }))
    )
  })

  # Carte de base
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -30, lat = 30, zoom = 2)
  })

  # Marqueurs mis à jour selon les filtres
  observe({
    df <- filtered()

    leafletProxy("map") |>
      clearMarkers() |>
      addCircleMarkers(
        data        = dplyr::select(df, lat, lng, title, geo_str, tags_str, description, couleur),
        lat         = ~lat,
        lng         = ~lng,
        radius      = 6,
        color       = ~couleur,
        fillColor   = ~couleur,
        fillOpacity = 0.8,
        stroke      = TRUE,
        weight      = 1,
        popup       = ~paste0(
          "<b>", title, "</b><br>",
          "<small>", geo_str, "</small><br>",
          "<small><i>", tags_str, "</i></small><br><br>",
          description
        )
      )
  })

  output$n_highlights <- renderText({
    paste0(nrow(filtered()), " highlight(s) affiché(s)")
  })
}

# ── Lancement ─────────────────────────────────────────────────────────────────

shinyApp(ui, server)
