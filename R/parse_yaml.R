#' Parser le frontmatter YAML et le corps d'une note Markdown
#'
#' Lit un fichier `.md` et retourne une liste avec trois éléments :
#' `path` (chemin), `meta` (frontmatter parsé) et `body` (corps Markdown).
#'
#' @param path Chemin vers le fichier `.md`.
#'
#' @return Une liste avec `path` (character), `meta` (list), `body` (character).
#' @export
parse_note <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

  # Repérer les délimiteurs --- du frontmatter
  delimiters <- which(trimws(lines) == "---")

  if (length(delimiters) >= 2) {
    yaml_raw  <- paste(lines[(delimiters[1] + 1):(delimiters[2] - 1)], collapse = "\n")
    body_lines <- lines[(delimiters[2] + 1):length(lines)]
    meta <- tryCatch(
      yaml::yaml.load(yaml_raw),
      error = function(e) {
        warning("Erreur YAML dans : ", path, "\n", conditionMessage(e))
        list()
      }
    )
  } else {
    meta       <- list()
    body_lines <- lines
  }

  list(
    path = path,
    meta = if (is.null(meta)) list() else meta,
    body = paste(body_lines, collapse = "\n")
  )
}

#' Lire tous les fichiers .md d'un dossier (récursif)
#'
#' Ignore automatiquement les dossiers `.trash` et `.obsidian`.
#'
#' @param dir Chemin vers le dossier à lire.
#' @param recursive Logical. Si TRUE, lit les sous-dossiers. Défaut : TRUE.
#'
#' @return Un tibble avec une ligne par fichier : `path`, `meta` (list-col), `body`.
#' @export
read_notes <- function(dir, recursive = TRUE) {
  paths <- unname(fs::dir_ls(dir, regexp = "\\.md$", recurse = recursive))

  # Exclure .trash et .obsidian
  paths <- paths[!stringr::str_detect(paths, "[\\/]\\.trash[\\/]|[\\/]\\.obsidian[\\/]")]

  notes <- purrr::map(paths, function(p) {
    tryCatch(
      parse_note(p),
      error = function(e) {
        warning("Impossible de lire : ", p, "\n", conditionMessage(e))
        list(path = p, meta = list(), body = "")
      }
    )
  })

  dplyr::tibble(
    path = purrr::map_chr(notes, "path"),
    meta = purrr::map(notes, "meta"),
    body = purrr::map_chr(notes, "body")
  )
}
