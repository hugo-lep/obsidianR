#' Helpers S3 — accès au vault via OVH S3
#'
#' Fonctions utilitaires pour lire les fichiers du vault depuis S3
#' (mode web) via s3db / paws.storage.
#'
#' @name s3_helpers
NULL

#' Lister les fichiers du vault sur S3
#'
#' @param bucket Nom du bucket S3. Si NULL, lu depuis config.yml.
#' @param prefix Préfixe (dossier) dans le bucket. Si NULL, lu depuis config.yml.
#'
#' @return Un tibble avec les clés S3 des fichiers du vault.
#' @export
s3_list_vault <- function(bucket = NULL, prefix = NULL) {

}

#' Lire un fichier Markdown depuis S3
#'
#' @param key Clé S3 du fichier (chemin relatif dans le bucket).
#' @param bucket Nom du bucket S3. Si NULL, lu depuis config.yml.
#'
#' @return Contenu brut du fichier (character).
#' @export
s3_read_note <- function(key, bucket = NULL) {

}
