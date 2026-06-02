# sync_vault.R
# Synchronise le vault Obsidian local vers S3 via Rclone
# Direction : laptop → S3 (unidirectionnel)
# Usage     : sourcer ce fichier ou lancer run_sync() dans la console

# ── Configuration ────────────────────────────────────────────────────────────

vault_local   <- "C:/R/obsidian_notes"            # Vault local
rclone_remote <- "obsidian"                       # Nom du remote Rclone (rclone config)
rclone_exe    <- "C:/Users/hugo_/AppData/Local/Microsoft/WinGet/Packages/Rclone.Rclone_Microsoft.Winget.Source_8wekyb3d8bbwe/rclone-v1.74.1-windows-amd64/rclone.exe"
s3_bucket    <- "avnumbers"                       # Bucket S3
s3_prefix    <- "obsidian_notes"                  # Dossier dans le bucket
log_dir      <- "C:/R/packages/obsidianR/logs"   # Dossier pour les logs

# ── Fonction principale ───────────────────────────────────────────────────────

#' Synchronise le vault Obsidian vers S3
#'
#' @param dry_run Logical. Si TRUE, simule la synchro sans rien transférer.
#' @param verbose Logical. Si TRUE, affiche la progression dans la console.
run_sync <- function(dry_run = FALSE, verbose = TRUE) {

  # Vérifications préalables
  if (!dir.exists(vault_local)) {
    stop("Vault introuvable : ", vault_local)
  }

  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
    message("Dossier logs créé : ", log_dir)
  }

  # Destination S3 (format Rclone : remote:bucket/chemin)
  s3_dest <- paste0(rclone_remote, ":", s3_bucket, "/", s3_prefix)

  # Fichier de log horodaté
  log_file <- file.path(
    log_dir,
    paste0("sync_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
  )

  # Arguments Rclone
  args <- c(
    "sync",
    vault_local,
    s3_dest,
    "--exclude", ".obsidian/**",   # Config Obsidian exclue
    "--exclude", ".trash/**",      # Corbeille Obsidian exclue
    "--log-file", log_file,
    "--log-level", "INFO"
  )

  if (verbose) args <- c(args, "--progress")
  if (dry_run) args <- c(args, "--dry-run")

  # Affichage de ce qui va se passer
  mode_label <- if (dry_run) "[DRY RUN] " else ""
  message(mode_label, "Synchro : ", vault_local, " → s3:", s3_bucket, "/", s3_prefix, "/")
  message("Log : ", log_file)
  if (dry_run) message("Mode simulation — aucun fichier ne sera transféré.")

  # Lancement de Rclone
  status <- system2(rclone_exe, args = args)

  # Résultat
  if (status == 0) {
    message("✓ Synchro terminée avec succès.")
  } else {
    warning("Rclone a retourné le code d'erreur : ", status, ". Consulter le log : ", log_file)
  }

  invisible(status)
}

# ── Lancement rapide ──────────────────────────────────────────────────────────

# Simuler la synchro (aucun fichier transféré) :
# run_sync(dry_run = TRUE)

# Lancer la vraie synchro :
# run_sync()

