# test_mod_itinerary_hawaii_m.R
# Test du module mod_itinerary_hawaii_m (version mobile) en isolation
#
# Pour tester en format téléphone sur ordinateur :
#   1. Lancer l'app (RunApp ou source)
#   2. Ouvrir dans le navigateur (pas le viewer RStudio)
#   3. Chrome : F12 → icône téléphone (Device Toolbar) → choisir un modèle
#      ex. iPhone 12 Pro, Samsung Galaxy S20, ou "Responsive" avec largeur ~390px
#
# Charge le .rds depuis S3 et monte une app Shiny minimale autour du module.
# Prérequis : avoir lancé inst/script/build_voyage_hawaii.R au moins une fois.

devtools::load_all()

library(shiny)
library(bslib)
library(s3db)
s3_connection_HL()

# ── Données depuis S3 ─────────────────────────────────────────────────────────

voyage_data <- s3readRDS_HL("itinéraires/hawaii_2026.rds")

message("✅ voyage_data chargé — build : ", format(voyage_data$meta$built_at, "%Y-%m-%d %H:%M"))

# ── App ───────────────────────────────────────────────────────────────────────

ui <- page_fluid(
  title  = "Test — mod_itinerary_hawaii_m",
  theme  = bs_theme(bootswatch = "flatly"),
  # padding réduit pour simuler un écran étroit
  shiny::div(style = "max-width: 430px; margin: 0 auto;",
    mod_itinerary_m_ui("hawaii_m")
  )
)

server <- function(input, output, session) {
  mod_itinerary_m_server("hawaii_m", voyage_data)
}

shinyApp(ui, server)

