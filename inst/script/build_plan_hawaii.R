# build_plan_hawaii.R
# Crée les fichiers de planification (plan_*.rds) pour chaque île
# à partir des sections déjà parsées dans hawaii_2026.rds.
#
# À lancer une seule fois pour initialiser les plans.
# Les modifications faites dans le day planner écrasent ensuite ces fichiers.
#
# Sorties S3 :
#   hawaii/obsidianr/plan_oahu.rds
#   hawaii/obsidianr/plan_bigisland.rds
#   hawaii/obsidianr/plan_kauai.rds

devtools::load_all()

library(s3db)
s3_connection_HL()

# ── Chargement ────────────────────────────────────────────────────────────────

voyage_data <- s3readRDS_HL("itinéraires/hawaii_2026.rds")  # main_folder = "hawaii/" par défaut
message("✅ voyage_data chargé — build : ", format(voyage_data$meta$built_at, "%Y-%m-%d %H:%M"))

# ── Construction et sauvegarde ────────────────────────────────────────────────

iles <- c("oahu", "bigisland", "kauai")

for (ile in iles) {
  plan <- list(
    days        = voyage_data$sections[[ile]],
    island_wide = character(0)
  )

  s3_key <- paste0("obsidianr/plan_", ile, ".rds")
  s3saveRDS_HL(plan, s3_key)
  message("☁️  ", s3_key, " — ", length(plan$days), " journée(s)")
}

message("✅ Build terminé.")
