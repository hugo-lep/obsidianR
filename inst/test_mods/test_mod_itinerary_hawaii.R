# test_mod_itinerary_hawaii.R
# Test du module mod_itinerary_hawaii en isolation
#
# Charge le .rds depuis S3 et monte une app Shiny minimale autour du module.
# Permet de valider le module avant intégration dans l'app voyage.
#
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
  title = "Test — mod_itinerary_hawaii",
  theme = bs_theme(bootswatch = "flatly"),
  mod_itinerary_ui("hawaii")
)

server <- function(input, output, session) {
  mod_itinerary_server("hawaii", voyage_data)
}

shinyApp(ui, server)
