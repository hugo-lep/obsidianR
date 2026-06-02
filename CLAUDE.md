# CLAUDE.md — Contexte général du projet

## Profil utilisateur

**Utilisateur :** Hugo Lepage  
**Background :** Technicien en entretien d'aéronefs, maintenant en révision de tâches de maintenance et planification (saisie dans un système de gestion de maintenance).  
**Stack de prédilection :** R, Shiny, bslib — projets de data science personnels et professionnels.  
**Infrastructure :** VPS personnel, bucket S3 (stockage cloud), laptop principal + ordinateur de bureau (usage secondaire, double écran).

---

## Vue d'ensemble du projet

Ce projet se découpe en trois phases :

### Phase 1 — Infrastructure & Synchronisation

Synchroniser le vault Obsidian (local sur le laptop) vers S3 via Rclone, puis vers le bureau. Le vault reste **séparé** de tout projet R — les apps y pointent via un chemin de config, elles ne le contiennent pas.

### Phase 2 — Vault Explorer (package R core)

Un **package R** (`vault-explorer`) contenant toute la logique de lecture et parsing du vault. Le package est agnostique — il ne sait pas si l'usage est privé ou public. Les apps Shiny qui l'importent décident quoi exposer et à qui.

Fonctions principales par type de note : `read_highlights()`, `read_lieux()`, `read_permanent()`, `read_ressource()`, etc. + helpers S3 (`s3db`, `paws`).

Les apps sont des **projets Shiny séparés** qui importent le package :
- **App locale** (`inst/app/`) : explorateur personnel, aucune auth, jamais sur VPS.
- **App vault web/PWA** (projet séparé) : déployée sur VPS, sécurisée via `protegR2`.
- **App voyage** (projet séparé) : public/multi-user, protegR2, mobile obligatoire.

### Phase 3 — App Voyage (usage étendu)

Utilisation des notes du vault comme source de données pour une application R Shiny dédiée aux itinéraires de voyage, avec carte interactive et système de suggestions multi-utilisateur. Déployée sur le VPS, sécurisée via `protegR2`, **avec support mobile obligatoire** (PWA / format téléphone).

---

## Stack technique

| Composant | Outil |
|---|---|
| Prise de notes | Obsidian (vault local Markdown) |
| Synchronisation | Rclone (local ↔ S3) |
| Stockage cloud | AWS S3 (bucket personnel) |
| `vault-explorer` (package) | Package R core — lecture/parsing de tous les types de notes |
| App locale (test package) | `inst/app/` dans le package, `config.yml` source: local, sans auth |
| App vault web/PWA | Projet Shiny séparé, importe le package + `protegR2` |
| App voyage | Projet Shiny séparé, importe le package + `protegR2`, mobile |
| Authentification | `protegR2` (package privé Hugo) |
| App voyage | R + Shiny + Leaflet + `protegR2` |
| Serveur (phases web) | VPS personnel |

---

## Architecture des CLAUDE.md

- Ce fichier = **contexte global** (architecture, stack, décisions transversales)
- Chaque projet R (obsidianR, app-voyage, etc.) aura son propre **CLAUDE.md léger** qui complète le global sans le répéter
- Le CLAUDE.md de projet est lu uniquement quand on travaille dans ce projet

---

## Règles pour Claude

- Tu agis comme **programmeur-analyste** : ton rôle est d'aider Hugo à structurer, planifier et coder.
- Toujours proposer du code R en priorité (Shiny, bslib, tidyverse, etc.).
- Ne jamais modifier, créer ou supprimer des fichiers sans **autorisation explicite** de Hugo.
- Tenir `plan.md` à jour au fur et à mesure que les étapes avancent.
- Prioriser la **sécurité** quand il s'agit de synchronisation de données personnelles (credentials S3, chiffrement, permissions).
- Le vault Explorer local est **local only** — ne pas proposer de déploiement VPS pour cette variante.
- La version web/PWA du vault Explorer et l'app voyage utilisent toutes les deux `protegR2` pour la gestion des utilisateurs et des mots de passe.
- `protegR2` est un package **privé** de Hugo — compatible avec `bslib::page_*`. Ne pas suggérer d'alternatives d'auth sans raison valable.
- L'app voyage doit impérativement supporter le **format mobile** (téléphone) — choisir les composants bslib en conséquence (`page_navbar`, layouts responsives, etc.).
- **Skills Claude Code** : mentionner proactivement quand une tâche répétitive ou un pattern de travail observé pourrait faire l'objet d'un skill Claude Code (slash command). Ne pas attendre que Hugo le demande.
- Skills déjà identifiés à créer : `/weekly-review` (revue des projets actifs depuis le vault), `/new-highlight` (créer une note highlight), `/synchro-vault` (lancer Rclone), `/build-highlights-rds` (rebuilder le dataframe highlights)
- **Automatisations** : noter les opportunités d'automatisation (scripts R, shell, tâches planifiées) au fil du développement — à implémenter plus tard.
