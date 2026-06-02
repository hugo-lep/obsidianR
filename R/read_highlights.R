# Mapping niveau lieu/* → colonne géographique dans le dataframe highlights
.niveau_to_col <- c(
  "lieu/continent" = "continent",
  "lieu/pays"      = "pays",
  "lieu/province"  = "admin",
  "lieu/état"      = "admin",
  "lieu/région"    = "admin",
  "lieu/île"       = "admin",
  "lieu/ville"     = "ville",
  "lieu/secteur"   = "ville"
)

#' Résoudre les colonnes géographiques d'un highlight via l'index de lieux
#'
#' @param country_list Liste de wikilinks (ex: `["[[Amérique]]", "[[Canada]]"]`)
#' @param index Tibble retourné par `build_lieu_index()`
#'
#' @return Liste nommée : continent, pays, admin, ville (NA si absent)
.resolve_geo <- function(country_list, index) {
  noms    <- gsub("\\[\\[|\\]\\]", "", as.character(unlist(country_list)))
  matched <- index[index$nom %in% noms, ]

  result <- list(
    continent = NA_character_,
    pays      = NA_character_,
    admin     = NA_character_,
    ville     = NA_character_
  )

  for (i in seq_len(nrow(matched))) {
    col <- .niveau_to_col[matched$niveau[i]]
    # Prend la première valeur trouvée pour chaque niveau
    if (!is.na(col) && is.na(result[[col]])) {
      result[[col]] <- matched$nom[i]
    }
  }

  result
}

#' Lire les notes highlight (voyage/highlight)
#'
#' Lit toutes les notes de type `voyage/highlight` et retourne un dataframe
#' consolidé avec coordonnées GPS, tags, et colonnes géographiques structurées
#' (continent, pays, admin, ville) résolues via l'index des notes `lieu/*`.
#'
#' @param vault_path Chemin vers le dossier vault. Si NULL, lu depuis config.yml.
#' @param notes Tibble déjà lu par `read_notes()`. Si NULL, relu depuis `vault_path`.
#'
#' @return Un tibble avec une ligne par highlight valide (lat/lng non manquants).
#' @export
read_highlights <- function(vault_path = NULL, notes = NULL) {
  `%||%` <- function(x, y) if (is.null(x)) y else x

  if (is.null(notes)) notes <- read_notes(vault_path)

  index <- build_lieu_index(notes)

  notes |>
    dplyr::filter(
      purrr::map_lgl(meta, ~ identical(.x[["Type"]], "voyage/highlight"))
    ) |>
    dplyr::mutate(
      title    = purrr::map2_chr(meta, path, ~ .x[["title"]] %||% basename(.y)),
      lat      = purrr::map_dbl(meta, ~ {
        loc <- .x[["location"]]
        if (length(loc) >= 1) as.numeric(loc[[1]]) else NA_real_
      }),
      lng      = purrr::map_dbl(meta, ~ {
        loc <- .x[["location"]]
        if (length(loc) >= 2) as.numeric(loc[[2]]) else NA_real_
      }),
      tags     = purrr::map(meta, ~ as.character(unlist(.x[["tags"]])) %||% character(0)),
      deja_vue = purrr::map_lgl(meta, ~ isTRUE(.x[["deja_vue"]])),
      country  = purrr::map(meta, ~ .x[["country"]] %||% list()),
      # Résolution géographique via l'index
      geo       = purrr::map(country, ~ .resolve_geo(.x, index)),
      continent = purrr::map_chr(geo, ~ .x$continent %||% NA_character_),
      pays      = purrr::map_chr(geo, ~ .x$pays      %||% NA_character_),
      admin     = purrr::map_chr(geo, ~ .x$admin     %||% NA_character_),
      ville     = purrr::map_chr(geo, ~ .x$ville     %||% NA_character_)
    ) |>
    dplyr::select(-geo) |>
    dplyr::filter(!is.na(lat), !is.na(lng))
}
