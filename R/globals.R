# Déclaration des variables globales utilisées en NSE (dplyr/tidyverse)
# Supprime les NOTEs R CMD CHECK "no visible binding for global variable"

utils::globalVariables(c(
  # build_lieu_index
  "meta", "path", "type_val", "nom",
  # read_highlights
  "country", "geo", "lat", "lng",
  # mod_itinerary_server
  ".data", "is_hotel", "tags"
))
