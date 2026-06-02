# Plan de projet — Vault Obsidian → S3 → Explorer & App Voyage

> Document évolutif. Mettre à jour au fur et à mesure que les étapes sont complétées.  
> Statuts : ⬜ À faire · 🔄 En cours · ✅ Complété · ❌ Bloqué

---

## Phase 1 — Synchronisation Obsidian ↔ S3

### Étape 1.1 — Préparation S3
- ⬜ Créer un bucket S3 dédié (ex: `hugo-obsidian-vault`)
- ⬜ Créer un utilisateur IAM avec permissions minimales (lecture/écriture sur ce bucket uniquement)
- ⬜ Générer et stocker les credentials IAM de façon sécurisée (ne jamais committer dans un repo)
- ⬜ Définir la structure du bucket :
  ```
  hugo-obsidian-vault/
  ├── vault/          ← copie miroir du vault Obsidian
  └── app-voyage/     ← pour la Phase 2
      └── modifications/
  ```

### Étape 1.2 — Configuration Rclone (laptop → S3)
- ✅ Installer Rclone sur le laptop si pas déjà fait
- ✅ Configurer un remote S3 dans Rclone (`rclone config`) — remote nommé `obsidian`, OVH BHS
- ✅ Tester la synchronisation manuelle (dry run : 3244 fichiers, 192 MB)
- ✅ Valider que les fichiers `.md`, `.png`, `.pdf` et la structure des dossiers sont bien préservés
- ✅ **Sécurité :** dossier `.obsidian/` exclu de la synchro

### Étape 1.3 — Automatisation de la synchro (laptop)
- ✅ Décider du mode de synchro : manuel (commande R) — `run_sync()` dans RStudio
- ✅ Option A — `inst/script/sync_vault.R` + `run_sync()`, logs horodatés automatiques
- ⬜ Option B — Exécutable `.bat` standalone (à faire plus tard)

### Étape 1.4 — Synchronisation vers le bureau (S3 → Desktop)
- ⬜ Installer Rclone sur le bureau
- ⬜ Utiliser les mêmes credentials IAM (ou un profil séparé en lecture seule si le bureau est moins sécurisé)
- ⬜ Configurer la synchro S3 → local sur le bureau :
  ```bash
  rclone sync s3:hugo-obsidian-vault/vault /chemin/local/vault --progress
  ```
- ⬜ Synchro **bi-directionnelle** avec `rclone bisync` (laptop ↔ S3 ↔ bureau)
- ⬜ Faire un premier `rclone bisync --resync` pour initialiser l'état de référence
- ⬜ **Sécurité conflits :** ne pas modifier la même note sur les deux machines sans synchro préalable — `bisync` ne fusionne pas le contenu, il détecte les conflits
- ⬜ Automatisation sur le bureau (même logique qu'étape 1.3)

### Considérations sécurité (Phase 1)
- Les credentials AWS ne doivent pas être en clair dans des scripts versionnés
- Utiliser `rclone config` avec chiffrement du fichier de config (`rclone config encryption set`)
- Option : bucket S3 privé + chiffrement côté serveur (SSE-S3 ou SSE-KMS)
- ⬜ Ajouter `--backup-dir s3:hugo-obsidian-vault/vault-backup/YYYY-MM-DD` à la commande Rclone pour archiver les fichiers écrasés ou supprimés
- ⬜ Rétention : **7 jours** — le ménage se fait automatiquement **à chaque synchro** (avant la synchro Rclone, le script supprime les dossiers `vault-backup/` de plus de 7 jours)

---

## Phase 1.5 — Standardisation du vault (préalable au vault explorer)

> Pas question de tout refaire — l'objectif est de définir un standard pour les nouvelles notes et de laisser le vault explorer révéler les incohérences dans les existantes.

### Étape 1.5.1 — Conventions de frontmatter
- ✅ Templates Obsidian déjà en place pour les types principaux : **ressource**, **permanente**, **lieu**, **highlight**
- ✅ Les nouvelles notes suivent ces templates automatiquement
- ⬜ Seule la section **Projets** reste à clarifier (voir étape 1.5.3)

### Étape 1.5.2 — Audit via le vault explorer
- ⬜ Une fois le vault explorer fonctionnel, analyser les stats de couverture frontmatter :
  - % de notes avec tags
  - % de notes avec `created`
  - Types de notes sans structure homogène
- ⬜ Identifier les zones à nettoyer en priorité (Area/Casquettes = candidat principal)
- ⬜ Nettoyer progressivement — pas en masse, mais au fil des consultations

### Étape 1.5.3 — Améliorer la section Projets
- ⬜ Revoir l'usage de la section P (Projets) — actuellement sous-utilisée
- ⬜ Distinction claire : **projet** (livrable avec fin) vs **casquette** (responsabilité continue comme avnumbers)
- ⬜ Les méga-projets continus (ex: avnumbers) vont dans `casquettes/`, pas dans `projets/`
- ⬜ Créer/mettre à jour le template de note projet avec ce frontmatter :
  ```yaml
  status: idée          # intention vague, pas encore défini
  # status: actif       # en cours (objectif : 1-3 max simultanément)
  # status: slow-burn   # avance au fil de la motivation
  # status: suspend     # en pause délibérée
  # status: bloqué      # attend quelque chose d'externe
  derniere_action: YYYY-MM-DD
  prochaine_action: "décrire ici quoi faire quand on reprend"
  energie_requise: faible / moyenne / élevée
  ```
- ⬜ Appliquer ce frontmatter aux projets existants
- ⬜ Les projets terminés → archivés dans `archive/` (pas un sous-dossier de `projets/`)

### Étape 1.5.4 — Weekly review
- ⬜ Retrouver le template de weekly review existant dans le vault
- ⬜ Définir une pratique régulière (hebdomadaire) : mettre à jour les statuts, choisir 1-3 projets actifs
- ⬜ **Skill Claude Code prévu** : `/weekly-review` — charge tous les projets actifs depuis le vault et guide la revue une par une (à créer quand le vault explorer sera fonctionnel)

---

## Phase 2 — Package R `vault-explorer` (core)

> Le package est la fondation — agnostique public/privé. Les apps Shiny séparées décident quoi exposer et à qui.  
> Structure sur disque : `vault/` et `vault-explorer/` sont des dossiers frères, jamais imbriqués.

### Étape 2.1 — Création du package
- ✅ Choisir le nom définitif du package : `obsidianR`
- ✅ Créer avec `usethis::create_package()` — package `obsidianR` initialisé
- ✅ Fichier `inst/config.yml` : `vault.source`, `vault.local_path`, `s3_bucket`, `s3_prefix`, `rclone_remote`
- ✅ Structure des fonctions par type de note :
  ```
  R/
  ├── read_highlights.R    ← notes géographiques (lat, lng, tags, country)
  ├── read_lieux.R         ← notes lieu (par continent)
  ├── read_permanent.R     ← notes permanentes (concepts)
  ├── read_ressource.R     ← notes ressources (livres, formations)
  ├── parse_yaml.R         ← parsing frontmatter commun
  └── s3_helpers.R         ← accès S3 via s3db + paws.storage (images)
  ```

### Étape 2.2 — Fonctions core
- ⬜ `parse_yaml()` : extraire frontmatter YAML + corps Markdown de n'importe quelle note
- ⬜ `read_highlights()` : dataframe consolidé (lat, lng, tags list-col, country list-col, content, title)
- ⬜ `read_lieux()` : lecture récursive par continent
- ⬜ `read_permanent()` et `read_ressource()` : à développer selon besoins
- ⬜ Mode source : `config.yml` → local (disque) ou web (S3 via `s3db` + `paws.storage`)

### Étape 2.3 — App locale de test (`inst/app/`)
- ⬜ App bslib minimale dans le package pour tester les fonctions
- ⬜ Navigation dans les notes, rendu Markdown, recherche plein-texte
- ⬜ Gestion des images : `addResourcePath()` en local, URLs pré-signées via `paws.storage` en mode web
- ⬜ Jamais déployée sur VPS — usage personnel uniquement

### Étape 2.4 — Apps Shiny séparées (projets indépendants)
- ⬜ **App vault web/PWA** : projet séparé, importe le package, ajoute `protegR2`, `config.yml` source: web
- ⬜ **App voyage** : projet séparé, importe le package, ajoute `protegR2` + Leaflet, mobile-first

### Étape 2.5 — Gestion des images
- ⬜ Identifier la syntaxe utilisée dans les notes : wiki-link Obsidian `![[image.png]]` ou Markdown standard `![](image.png)` — ou les deux
- ⬜ Si wiki-link : convertir `![[image.png]]` → `![](chemin/image.png)` avant rendu HTML
- ⬜ Mode local : `shiny::addResourcePath()` pour servir le dossier d'images comme ressource statique
- ⬜ Mode web/S3 : générer des URLs pré-signées via `paws.storage` pour chaque image référencée
- ⬜ Images web (URL externe) : rendues nativement par le Markdown, aucun traitement requis
- ⬜ PDFs : hors scope pour l'instant

### Étape 2.6 — Fonctionnalités avancées
- ⬜ Recherche plein-texte (avec `stringr` ou index lunr.js via `htmlwidgets`)
- ⬜ Parsing des frontmatter YAML (`yaml` package)
- ⬜ Filtrage par tag, date, dossier
- ⬜ Thème dark/light avec bslib


---

## Phase 3 — App Voyage (Shiny + VPS)

> Basée sur le vault, orientée partage et collaboration légère.  
> Sécurisée via `protegR2`. **Support mobile obligatoire** (format téléphone / PWA).

### Étape 3.1 — Structure S3 pour l'app voyage
- ⬜ Créer les dossiers S3 selon la structure suivante :
  ```
  bucket/
  ├── vault/                        ← sync Obsidian complet (Rclone)
  └── vault_voyage/                 ← dossier app (géré par s3db / paws)
      ├── highlights/               ← notes highlight publiées
      ├── lieux/                    ← notes lieu publiées
      │   ├── Amerique/
      │   ├── Europe/
      │   └── ...
      ├── users/
      │   ├── hugo/
      │   │   ├── preferences.yml   ← config_user : favoris, itinéraires, vue carte, filtres
      │   │   └── itineraires/
      │   └── {username}/           ← prêt pour multi-user
      └── modifications/            ← suggestions des visiteurs
  ```
- ⬜ `preferences.yml` par utilisateur contient : zoom/centre carte par défaut, filtres par défaut, favoris, itinéraires sauvegardés
- ⬜ Lu/écrit via `yaml` package + `s3db` (avec override de folder vers `vault_voyage/`)
- ⬜ Prévu pour brancher sur `config_user` de `protegR2` quand cette fonctionnalité sera implémentée
- ⬜ Script de publication : copier highlights et lieux depuis le vault vers S3 via Rclone

### Étape 3.2 — Parsing des notes (structure YAML connue)

Structure frontmatter des **notes highlight** :
- `location` : liste YAML standard (ex: `[45.516, -73.379]`) → parsé directement comme vecteur numérique par le package `yaml`
- `country` : liste YAML → vecteur R `c("Amerique", "Canada", "Québec - province", "Montréal")`
- `tags` : hiérarchiques avec `/` comme séparateur (ex: `plein_air/paddleboard`)

Structure frontmatter des **notes lieu** :
- Peu de propriétés formelles
- Contenu général + bloc Dataview (à ignorer ou afficher tel quel dans l'app)

Tâches :
- ⬜ Fonction R pour lire un fichier `.md` et extraire le frontmatter YAML (`yaml` package)
- ⬜ Parser le champ `location` : vecteur YAML `[lat, lng]` → deux colonnes numériques `lat` / `lng`
- ⬜ Parser `country` comme vecteur → permettre filtre par niveau (continent, pays, région, ville)
- ⬜ Parser les tags hiérarchiques → filtre par préfixe (ex: `startsWith(tag, "plein_air/")`)
- ⬜ Construire un dataframe consolidé de tous les highlights (une ligne par note)

### Étape 3.3 — App Shiny de base
- ⬜ Créer un nouveau projet Shiny pour l'app voyage
- ⬜ Intégrer `protegR2` dès le départ (gestion utilisateurs/mots de passe)
- ⬜ Choisir un layout bslib **mobile-first** (ex: `page_navbar` responsive, ou `page_fillable`)
- ⬜ Carte Leaflet alimentée par le dataframe highlights (un marqueur par note)
- ⬜ Filtres : tags complets pour l'instant — prévu deux filtres en cascade (parent → sous-tag) dans une version future
- ⬜ Clic sur un marqueur → affichage de la note highlight + lien vers la note lieu associée
- ⬜ Valider le rendu sur mobile (taille tactile des éléments, lisibilité)

### Étape 3.4 — Système de suggestions multi-utilisateur
- ⬜ Chaque visiteur peut soumettre une suggestion (formulaire Shiny)
- ⬜ La suggestion est sauvegardée comme nouveau fichier dans `app-voyage/modifications/`
- ⬜ Nommage des fichiers : `suggestion_{user}_{YYYYMMDD_HHMMSS}.txt`
- ⬜ L'admin (Hugo) révise les suggestions depuis S3 et décide d'intégrer ou non dans Obsidian
- ⬜ Aucune modification directe des notes originales par les visiteurs

### Étape 3.5 — Déploiement sur VPS
- ⬜ Configurer Shiny Server (ou Posit Connect) sur le VPS
- ⬜ Gérer les credentials S3 de façon sécurisée sur le VPS (variables d'environnement, IAM role)
- ⬜ Configurer un reverse proxy (nginx) + HTTPS

---

## Fonctionnalités envisagées — App Voyage

> À prioriser et planifier au moment du développement de l'app. Toutes basées sur les données existantes (coordonnées, tags, country, contenu, images).

- **Carte interactive** — marqueurs cliquables par type d'activité, filtres par tag/pays, popup avec la note complète
- **Itinéraire de voyage** — sélectionner et ordonner des highlights, tracé sur la carte avec distances et temps estimés
- **Bucket list** — marquer des highlights "à faire" vs "fait", progression visuelle par région ou type d'activité
- **Heatmap d'activités** — densité géographique des notes, visualiser les zones bien documentées vs les angles morts
- **Exploration par tag en cascade** — drill-down : `plein_air` → sous-types → spots sur la carte
- **Comparateur de destinations** — deux régions côte à côte, highlights, activités disponibles, notes
- **Générateur de suggestions** — "3 jours au Québec, j'aime la randonnée" → propose des highlights géographiquement proches et cohérents
- **Timeline personnelle** — voyages en ordre chronologique (si dates présentes), carte + galerie d'images
- **Vue galerie** — grille photo des images d'une région, navigation visuelle plutôt que cartographique
- **Partage d'itinéraire** — lien vers un itinéraire sauvegardé, lisible sans compte (lecture publique optionnelle)
- **Stats personnelles** — pays, régions, types d'activités, nombre de highlights — dashboard "explorer profile"
- **Export PDF** — document imprimable d'un itinéraire avec carte statique, liste des stops et notes

### PWA (vraiment plus tard)
- Transformer l'app voyage en PWA installable sur téléphone
- Permettre l'enregistrement de données personnelles liées à un highlight : billets d'entrée (PDF/image), notes perso, souvenirs — stockés dans `users/{username}/` sur S3
- Ces données sont privées par utilisateur, jamais dans le vault Obsidian

---

## Pistes futures (hors scope actuel)

- **Notes permanentes** : grand dossier de concepts dans le vault — usage R à définir plus tard (graph de connaissances, recherche sémantique, résumés automatiques ?)
- **Corporate memory TEA** : même stack (Obsidian → Rclone → S3 → app R/bslib + protegR2 en lecture seule), pour documenter les connaissances des techniciens en entretien d'aéronefs. Éditeur autonome dans la compagnie (Obsidian), accès uniforme en lecture pour tous les TEA. Court terme : employeur actuel. Potentiel via avnumbers : modèle reproductible pour d'autres organisations aéronautiques.

---

## Questions ouvertes / décisions à prendre

| # | Question | Décision |
|---|---|---|
| 1 | Bureau : lecture seule depuis S3 ou bi-directionnel ? | Bi-directionnel (`rclone bisync`) |
| 2 | Vault Explorer : source locale, S3 ou toggle ? | `config.yml` (local/web) — package R, pas de toggle UI |
| 3 | Versioning S3 activé pour le vault ? | `--backup-dir` dans Rclone (dossier archive) |
| 4 | Notes voyage : dossier dédié dans Obsidian ou tag ? | Deux dossiers : `highlights/` (un seul) + `lieux/` (un par continent) |
| 5 | Authentification sur l'app voyage (public ou protégée) ? | `protegR2` — protégée |

---

## Journal des décisions

| Date | Décision prise |
|---|---|
| 2026-05-12 | Initialisation du plan |
| 2026-05-12 | Vault séparé du projet R — pointé via `config.yml` |
| 2026-05-12 | Auth via `protegR2` (package privé) pour vault web/PWA et app voyage |
| 2026-05-12 | App voyage : support mobile obligatoire (PWA / format téléphone) |
| 2026-05-12 | Bureau bi-directionnel via `rclone bisync` |
| 2026-05-12 | Backup via `--backup-dir` Rclone (pas de versioning OVH) |
| 2026-05-12 | Vault Explorer = package R — config.yml pilote la source (local/web) |
| 2026-05-12 | Version web = projet Shiny séparé qui importe le package + protegR2 |
| 2026-05-12 | Rétention backup : 7 jours, ménage automatique à chaque synchro |
| 2026-05-12 | config_user = `preferences.yml` par user sur S3, format YAML, branché sur protegR2 plus tard |
| 2026-05-12 | Templates Obsidian déjà en place pour les 4 types de notes principaux |
| 2026-05-12 | Package R core agnostique — apps Shiny séparées gèrent public/privé |
| 2026-05-12 | vault/ et vault-explorer/ sont des dossiers frères, jamais imbriqués |
