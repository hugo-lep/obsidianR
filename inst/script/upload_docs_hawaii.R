# upload_docs_hawaii.R
# Envoie les PDFs de inst/docs/ vers S3 : docs/hawaii/
#
# Utilisation :
#   1. Déposer les PDFs dans inst/docs/  (dossier gitignore)
#   2. Sourcer ce script
#
# Convention de nommage suggérée :
#   assurance-annie-bnc.pdf
#   assurance-hugo-amex.pdf
#   assurance-auto.pdf
#   passport-hugo.pdf
#   passport-annie.pdf
#   passport-lea.pdf
#   reservation-auto.pdf
#   reservation-hotel-oahu.pdf
#   reservation-vol.pdf

devtools::load_all()

library(s3db)
s3_connection_HL()

DOCS_LOCAL  <- here::here("inst/docs")
S3_PREFIX   <- "docs/hawaii"

pdfs <- list.files(DOCS_LOCAL, pattern = "\\.pdf$", full.names = TRUE)

if (length(pdfs) == 0) {
  message("Aucun PDF trouvé dans inst/docs/")
} else {
  message("📄 ", length(pdfs), " PDF(s) à envoyer...")
  for (f in pdfs) {
    s3_key <- paste0(S3_PREFIX, "/", basename(f))
    s3db::s3upload_HL(f, s3_key, main_folder = FALSE)
    message("   ✓ ", basename(f), " → s3:", s3_key)
  }
  message("✅ Upload terminé.")
}
