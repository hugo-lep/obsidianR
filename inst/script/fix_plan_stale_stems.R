# fix_plan_stale_stems.R
# Identifie et corrige les stems stales dans les plans S3.
#
# Un stem est "stale" s'il est assigné à une journée mais n'existe plus
# dans les highlights du vault (note renommée, supprimée, ou alias mal parsé).
#
# Usage :
#   1. Sourcer ce script — il affiche les stems stales par île
#   2. Remplir la section "Corrections manuelles" si nécessaire
#   3. Re-sourcer pour appliquer les corrections

devtools::load_all()

library(s3db)
s3_connection_HL()

# ── Chargement ────────────────────────────────────────────────────────────────

voyage_data <- s3readRDS_HL("itinéraires/hawaii_2026.rds")

iles <- c("oahu", "bigisland", "kauai")

plans <- setNames(
  lapply(iles, function(ile) s3readRDS_HL(paste0("obsidianr/plan_", ile, ".rds"))),
  iles
)

# ── Diagnostic ────────────────────────────────────────────────────────────────

stale <- list()

for (ile in iles) {
  valid_stems <- voyage_data$highlights[[ile]]$stem
  all_assigned <- sub("\\|.*$", "", unlist(plans[[ile]]$days, use.names = FALSE))
  bad <- setdiff(all_assigned, valid_stems)
  if (length(bad) > 0) {
    stale[[ile]] <- bad
    message("⚠️  ", ile, " — stems introuvables :")
    for (s in bad) message("     • ", s)
  } else {
    message("✅ ", ile, " — aucun stem stale")
  }
}

# ── Corrections manuelles ─────────────────────────────────────────────────────
# Format : corrections[["ile"]] <- c("vieux stem" = "nouveau stem", ...)
# Laisser vide si aucune correction à faire.

corrections <- list(
  oahu      = c(),
  bigisland = c(),
  kauai     = c(
    "Poipu Beach - Kauai (plage populaire, phoques moines, South Shore)" =
      "Poipu Beach - Kauai (tortues de mer, surfeurs, coucher de soleil)"
  )
)

# ── Application ───────────────────────────────────────────────────────────────

apply_corrections <- function(plan, mapping) {
  if (length(mapping) == 0) return(plan)
  plan$days <- lapply(plan$days, function(stems) {
    stems <- sub("\\|.*$", "", stems)  # strip alias au passage
    idx <- stems %in% names(mapping)
    stems[idx] <- unname(mapping[stems[idx]])
    stems
  })
  plan
}

any_saved <- FALSE
for (ile in iles) {
  if (length(corrections[[ile]]) == 0) next
  plans[[ile]] <- apply_corrections(plans[[ile]], corrections[[ile]])
  s3_key <- paste0("obsidianr/plan_", ile, ".rds")
  s3saveRDS_HL(plans[[ile]], s3_key)
  message("💾 ", s3_key, " mis à jour")
  any_saved <- TRUE
}

if (!any_saved) message("ℹ️  Aucune correction appliquée.")
message("✅ Terminé.")
