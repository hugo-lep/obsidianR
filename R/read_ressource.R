#' Lire les notes ressource (livres, formations)
#'
#' Lit toutes les notes de type `note ressource` du vault.
#'
#' @param vault_path Chemin vers le dossier vault. Si NULL, lu depuis config.yml.
#'
#' @return Un tibble avec une ligne par note : `title`, `auteur`, `tags`,
#'   `date_publication`, `body`, `path`.
#' @export
read_ressource <- function(vault_path = NULL) {

}
