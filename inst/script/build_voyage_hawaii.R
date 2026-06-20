# build_voyage_hawaii.R
# Script de build — Voyage Hawaii 2026
#
# Lit le vault Obsidian local, extrait les données voyage (itinéraires,
# highlights géolocalisés, sections par journée) et sauvegarde sur S3.
#
# À lancer depuis la racine du package obsidianR.
# Prérequis : s3db configuré (variables d'environnement bucket/key/secret/endpoint)
#
# Sortie S3 :
#   itinéraires/hawaii_2026.rds

devtools::load_all()

library(s3db)
library(stringr)
library(dplyr)
library(purrr)

s3_connection_HL()
# ── Chemins ───────────────────────────────────────────────────────────────────

VAULT         <- "C:/R/obsidian_notes"
ITINERARY_DIR <- file.path(
  VAULT, "2-Area/Intérêts/Voyages/itinéraire/itinéraire_vault/Hawaii"
)

# Préfixe S3 des pièces jointes (vault miroir via Rclone — images déjà présentes)
S3_ATTACH_PREFIX <- "obsidian_notes/3-Références/6-Fourre-tout/attachments"

# Destination S3 du .rds
S3_OUTPUT <- "itinéraires/hawaii_2026.rds"

# ── Index du vault ────────────────────────────────────────────────────────────

message("📖 Lecture du vault...")
notes          <- read_notes(VAULT)
wikilink_index <- setNames(notes$path, tools::file_path_sans_ext(basename(notes$path)))
message("   → ", nrow(notes), " notes indexées")

# ── Helpers ───────────────────────────────────────────────────────────────────
# TODO : déplacer dans obsidianR quand le module sera stabilisé

#' Extraire les stems de wikilinks d'un texte (exclut ![[images]])
.extract_wikilinks <- function(text) {
  m1 <- stringr::str_match_all(text, "(?<!!)\\[\\[([^|\\]]+)\\|[^\\]]+\\]\\]")[[1]]
  m2 <- stringr::str_match_all(text, "(?<!!)\\[\\[([^\\]]+)\\]\\]")[[1]]
  unique(c(
    if (nrow(m1) > 0) trimws(m1[, 2]) else character(0),
    if (nrow(m2) > 0) trimws(m2[, 2]) else character(0)
  ))
}

#' Parser un body en sections ### → liste nommée (heading → stems[])
.parse_body_sections <- function(body) {
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

#' Construire le lookup inverse stem → section heading (première occurrence)
.build_stem_to_section <- function(sections) {
  result <- list()
  for (sec_name in names(sections)) {
    for (s in sections[[sec_name]]) {
      if (is.null(result[[s]])) result[[s]] <- sec_name
    }
  }
  result
}

#' Construire le tibble de highlights géolocalisés pour un onglet
#'
#' Parcourt tous les wikilinks du body, résout chaque stem dans l'index,
#' retient uniquement les notes avec coordonnées GPS. Inclut le body de la
#' note pour affichage dans le module (pas besoin de relire le vault depuis
#' l'app web).
.build_tab_highlights <- function(body) {
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
      tags  = list(tags),
      body  = note$body
    )
  })

  dplyr::bind_rows(rows)
}

# ── Lecture des itinéraires ───────────────────────────────────────────────────

message("📋 Extraction des itinéraires...")

.read_body <- function(filename) {
  parse_note(file.path(ITINERARY_DIR, filename))$body
}

bodies <- list(
  hawaii    = .read_body("Itinéraire Hawaii.md"),
  oahu      = .read_body("Itineraire Oahu - notes de planification.md"),
  bigisland = .read_body("Itineraire Big Island - journees types par region.md"),
  kauai     = .read_body("Itineraire Kauai - journees types par region.md")
)

message("   → ", length(bodies), " itinéraires lus")

# ── Highlights filtrés par onglet ─────────────────────────────────────────────

message("🗺️  Extraction des highlights géolocalisés...")

highlights_by_tab <- purrr::map(bodies, .build_tab_highlights)

purrr::iwalk(highlights_by_tab, ~ message("   → ", .y, " : ", nrow(.x), " highlights"))

# ── Sections ### par onglet ───────────────────────────────────────────────────

message("📅 Parsing des sections par journée...")

sections_by_tab        <- purrr::map(bodies, .parse_body_sections)
stem_to_section_by_tab <- purrr::map(sections_by_tab, .build_stem_to_section)

purrr::iwalk(sections_by_tab, ~ message("   → ", .y, " : ", length(.x), " journées"))

# ── Assemblage ────────────────────────────────────────────────────────────────

voyage_data <- list(
  meta = list(
    voyage   = "Hawaii 2026",
    built_at = Sys.time(),
    version  = "1.0"
  ),
  itineraires        = bodies,
  highlights         = highlights_by_tab,
  sections           = sections_by_tab,
  stem_to_section    = stem_to_section_by_tab,
  s3_attach_prefix   = S3_ATTACH_PREFIX
)

# ── Sauvegarde S3 ─────────────────────────────────────────────────────────────

message("☁️  Sauvegarde sur S3 : ", S3_OUTPUT)
s3saveRDS_HL(voyage_data, S3_OUTPUT)
s3readRDS_HL(S3_OUTPUT)
message("✅ Build terminé — ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

