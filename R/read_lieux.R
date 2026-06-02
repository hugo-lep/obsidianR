#' Construire un index des notes de lieu
#'
#' Lit toutes les notes dont le `Type` commence par `lieu/` et retourne
#' un tableau de correspondance nom → niveau géographique. Utilisé par
#' `read_highlights()` pour résoudre les colonnes continent, pays, admin, ville.
#'
#' @param notes Tibble retourné par `read_notes()`.
#'
#' @return Un tibble avec deux colonnes :
#'   - `nom` : nom du fichier sans extension (= nom du wikilink)
#'   - `niveau` : valeur du champ `Type` (ex: "lieu/pays", "lieu/ville")
#' @export
build_lieu_index <- function(notes) {
  `%||%` <- function(x, y) if (is.null(x)) y else x

  notes |>
    dplyr::mutate(
      type_val = purrr::map_chr(meta, ~ .x[["Type"]] %||% ""),
      nom      = tools::file_path_sans_ext(basename(path))
    ) |>
    dplyr::filter(stringr::str_starts(type_val, "lieu/")) |>
    dplyr::select(nom, niveau = type_val)
}

#' Lire les notes de lieu
#'
#' Lit toutes les notes de lieu du vault, organisées par continent.
#'
#' @param vault_path Chemin vers le dossier vault. Si NULL, lu depuis config.yml.
#' @param continent Filtrer par continent (ex: "Amerique", "Europe"). Si NULL, tous.
#'
#' @return Un tibble avec une ligne par note lieu.
#' @export
read_lieux <- function(vault_path = NULL, continent = NULL) {

}
