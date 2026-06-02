# test_parse.R
# Test interactif de parse_note() et read_notes()
# Lancer les blocs un par un dans RStudio

devtools::load_all()

`%||%` <- function(x, y) if (is.null(x)) y else x

vault <- "C:/R/obsidian_notes"

# ── 1. Tester parse_note() sur une seule note highlight ───────────────────────

note <- parse_note(
  file.path(vault, "0-Inbox/claude/Foret Ouareau - Boucle du Lac Blanc (escarpements, Notre-Dame-de-la-Merci, Lanaudiere).md")
)

note$meta        # frontmatter complet (liste)
note$meta$Type   # "voyage/highlight"
note$meta$location  # coordonnées GPS
note$meta$tags      # tags
note$meta$country   # pays / région
note$body           # corps Markdown

# ── 2. Tester parse_note() sur une note ressource ────────────────────────────

note_res <- parse_note(file.path(vault, "0-Inbox/100 baggers.md"))

note_res$meta
note_res$meta$`Type de note`   # "note ressource"
note_res$meta$Auteur
note_res$meta$MOC

# ── 3. Tester parse_note() sur une note permanente ───────────────────────────

note_perm <- parse_note(
  file.path(vault, "0-Inbox/accepter un cadeau c'est endosser les sacrifices de l'autre.md")
)

note_perm$meta
note_perm$meta$`Type de note`  # "note permanente"
note_perm$body

# ── 4. Lire tout le vault ─────────────────────────────────────────────────────
# ⚠️ ~3200 fichiers — prend quelques secondes

notes <- read_notes(vault)

nrow(notes)     # nombre total de notes
names(notes)    # colonnes : path, meta, body

# ── 5. Explorer le tibble ─────────────────────────────────────────────────────

# Extraire le champ "Type de note" pour chaque note
notes$type_note <- purrr::map(notes$meta, ~ .x[["Type de note"]])

# Distribution des types de notes
notes |>
  dplyr::mutate(type = purrr::map_chr(meta, ~ paste(.x[["Type de note"]], collapse = ", "))) |>
  dplyr::count(type, sort = TRUE)

# ── 6. Filtrer les highlights ─────────────────────────────────────────────────

highlights <- notes |>
  dplyr::filter(
    purrr::map_chr(meta, ~ .x[["Type"]] %||% "") == "voyage/highlight"
  )

nrow(highlights)

# Voir les 5 premiers
highlights |>
  dplyr::mutate(
    title    = purrr::map2_chr(meta, path, ~ .x[["title"]] %||% basename(.y)),
    location = purrr::map(meta, ~ .x[["location"]]),
    tags     = purrr::map(meta, ~ .x[["tags"]])
  ) |>
  dplyr::select(title, location, tags) |>
  head(5)
