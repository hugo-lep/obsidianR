# build_plan_hawaii.R
# Initialise les fichiers de planification (plan_*.rds) pour chaque île.
#
# Comportement :
#   - Si le plan existe déjà sur S3 → ignoré
#   - Si le plan n'existe pas        → créé vide (aucune journée, aucun highlight)
#                                       Les journées sont ensuite gérées dans le Day Planner.
#
# Pour forcer la recréation : passer force = TRUE (efface les modifications du day planner).
#
# Sorties S3 :
#   hawaii/obsidianr/plan_oahu.rds
#   hawaii/obsidianr/plan_bigisland.rds
#   hawaii/obsidianr/plan_kauai.rds

devtools::load_all()

library(s3db)
s3_connection_HL()

force <- FALSE  # TRUE pour recréer même si le plan existe déjà

# ── Initialisation ────────────────────────────────────────────────────────────

iles <- c("oahu", "bigisland", "kauai")

for (ile in iles) {
  s3_key <- paste0("obsidianr/plan_", ile, ".rds")

  if (!force) {
    existing <- tryCatch(s3readRDS_HL(s3_key), error = function(e) NULL)
    if (!is.null(existing)) {
      message("⏭️  ", s3_key, " existe déjà — ignoré (force = FALSE)")
      next
    }
  }

  plan <- list(days = list())
  s3saveRDS_HL(plan, s3_key)
  message("☁️  ", s3_key, " — plan vide créé")
}

message("✅ Terminé.")
