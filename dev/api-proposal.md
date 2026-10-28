# kcloud.r : proposition d'API

Statut : proposition du 2026-10-27, implémentée en v0.1.0 (sauf `s3` et
`rclone`) le 2026-10-28.

## 1. Objectif et périmètre

kcloud.r donne **une seule API** pour lire et écrire des fichiers de données,
quel que soit l'endroit où ils sont stockés : disque local, Nextcloud, S3,
ou n'importe quel remote rclone.

Il remplace ce qui reste de fairify dans nos packages :

| fairify            | kcloud.r                                  |
|--------------------|-------------------------------------------|
| `getDataPath()`    | `cloud_path()` (et `cloud_url()`)         |
| `cleanDataCache()` | `cloud_cache_clean()`                     |
| `loadConfig()`     | `load_config()` + `cloud_from_config()`   |

Hors périmètre : la versionnement des données, la synchronisation
bidirectionnelle et la gestion des droits de partage Nextcloud.

## 2. Principes

1. **Un objet storage**, créé une fois (par un constructeur ou depuis la
   config), puis passé en premier argument de chaque fonction, ce qui
   permet le pipe. Il n'y a pas d'état global caché.
2. **Des backends en classes S3** : `cloud_local`, `cloud_nextcloud`,
   `cloud_s3`, `cloud_rclone`, qui héritent tous de `cloud_storage`. Pour
   ajouter un backend, on écrit un constructeur et quatre méthodes
   (section 6).
3. **Des chemins distants toujours relatifs** à la racine du storage, avec
   `/` comme séparateur. Un `/` final désigne un dossier, comme dans
   fairify.
4. **Pas de secrets dans les fichiers** : un mot de passe ou une clé ne
   passe que par une variable d'environnement (section 5.3).
5. **Un cœur léger et rapide** : httr2, xml2 et yaml. Nextcloud et S3
   sont implémentés directement sur httr2 : il signe les requêtes S3
   (`req_auth_aws_v4()`) et les parallélise (`req_perform_parallel()`).
   On se passe donc d'aws.s3 et de paws (section 6). rclone n'est
   utilisé que s'il est installé.
6. **Linux, Windows et macOS** : `file.path()`, `tools::R_user_dir()`,
   `system2()` avec arguments, aucun chemin `/home`.

## 3. Constructeurs

```r
cloud_local(root, read_only = FALSE, name = NULL)

cloud_nextcloud(
  url,                 # lien public https://host/s/TOKEN, ou https://host
  user = NULL,         # NULL = lien public
  password = NULL,     # préférer password_env
  password_env = "KCLOUD_NEXTCLOUD_PASSWORD",
  root = "",           # sous-dossier (mode authentifié)
  read_only = NULL,    # NULL = TRUE pour un lien public, FALSE sinon
  name = NULL,
  cache = NULL
)

cloud_s3(
  bucket,
  prefix = "",
  region = NULL,       # NULL = AWS_DEFAULT_REGION
  endpoint = NULL,     # S3 non-AWS (MinIO, Ceph, OVH...)
  anonymous = FALSE,   # bucket public (ex. DRIAS dans radis.api)
  read_only = FALSE,
  name = NULL,
  cache = NULL
)

cloud_rclone(remote, read_only = FALSE, name = NULL, cache = NULL)
# remote = "monremote:bucket/prefix", configuré avec `rclone config`
```

Arguments communs :

- `name` sert à identifier le storage dans les messages et nomme son
  sous-dossier de cache. Par défaut, il est calculé à partir de
  l'hôte et du token, ou du bucket et du préfixe :
  `nextcloud.inrae.fr_r9NidM2JSpyHYZx`.
- `cache` est le dossier du cache. `max_age` (7 jours par défaut) et
  `max_size` (10 Go par défaut) règlent la fraîcheur et la taille du
  cache ; ces deux arguments existent aussi pour chaque constructeur et
  dans la config (section 4.3).
- `read_only` est vérifié : `cloud_upload()` et `cloud_delete()` échouent
  si le storage est en lecture seule. Ça remplace le
  `write_results` forcé à `FALSE` de fairify.

Chaque constructeur renvoie une liste de classe
`c("cloud_<backend>", "cloud_storage")`, avec une méthode `print()`
qui n'affiche jamais de secret.

## 4. Opérations

Ce sont des génériques S3 qui dispatchent sur la classe du storage. Les
`...` de chemin sont collés avec `/`, comme dans `getDataPath()`.

### 4.1 Lecture

```r
cloud_path(storage, ..., max_age = NULL)
```

Renvoie un **chemin local** lisible par n'importe quelle fonction de
lecture (`read.csv()`, `readxl::read_excel()`, `terra::rast()`...) :

- avec `cloud_local`, c'est `file.path(root, path)`, sans copie ;
- avec les autres backends, le fichier (ou le dossier, si le chemin finit
  par `/`) est téléchargé dans le cache, puis on renvoie son chemin dans
  le cache.

`max_age` (en jours) fixe la durée pendant laquelle un fichier en cache
est considéré comme frais (section 4.3). `NULL` reprend la valeur du
storage.

```r
cloud_url(storage, ...)
```

Renvoie l'URL distante, sans rien télécharger. C'est l'équivalent de
`getDataPath(use_cache = FALSE)`. Elle n'existe pas pour `cloud_local` ni
`cloud_rclone` (erreur explicite).

```r
cloud_download(storage, path, path_local, overwrite = FALSE)
```

Copie un fichier ou un dossier distant vers un emplacement local choisi,
sans passer par le cache. Renvoie `path_local` de façon invisible.

### 4.2 Écriture et inspection

```r
cloud_upload(storage, path_local, path, overwrite = FALSE)
cloud_delete(storage, path)                      # v0.2
cloud_exists(storage, path)                      # vectorisé -> logical
cloud_list(
  storage, path = "", recursive = FALSE, pattern = NULL,
  max_age = 0, workers = NULL   # 4 pour Nextcloud, 16 sinon
)
```

`cloud_list()` renvoie un data.frame avec les colonnes
`path`, `is_dir`, `size` (en octets), `modified` (POSIXct UTC) et `etag`.
Les optimisations pour les gros buckets sont détaillées en section 4.4.

`cloud_upload()` accepte un dossier local : il est alors envoyé de façon
récursive. Les dossiers distants manquants sont créés (MKCOL sur
Nextcloud). Cette fonction remplace le `upload_rclone()` d'OptirrigPTGE
pour les cas simples. Les gros transferts parallèles restent le travail
du backend rclone.

### 4.3 Cache

```r
cloud_cache_dir(storage)                  # dossier du cache de ce storage
cloud_cache_clean(storage, path = NULL)   # NULL = tout le cache du storage
```

L'emplacement du cache se décide dans cet ordre, du prioritaire au moins
prioritaire :

1. l'argument `cache` du constructeur ;
2. la clé `cache` de la config ;
3. la variable d'environnement `KCLOUD_CACHE` ;
4. `tools::R_user_dir("kcloud.r", "cache")`.

Chaque storage a ensuite son sous-dossier `<cache>/<name>/`.

Le cache est **persistant** : il survit d'une session R à l'autre, alors
que celui de fairify était dans le dossier temporaire. Il est réglé par
deux paramètres, définis par storage (dans le constructeur ou la
config) :

**Fraîcheur, `max_age`** (en jours, 7 par défaut) :

- tant qu'un fichier a moins de `max_age`, on le lit dans le cache sans
  aucune requête réseau ;
- au-delà, une **requête légère** (PROPFIND sur Nextcloud, HEAD sur S3)
  donne l'ETag distant, que l'on compare à celui noté. S'il est le même,
  rien n'est retéléchargé et le compteur de fraîcheur repart à zéro.
  Sinon, on télécharge la nouvelle version. Cette méthode marche aussi
  pour les dossiers, dont Nextcloud met l'ETag à jour quand leur contenu
  change.
- si le serveur ne répond pas, on utilise la copie en cache, avec un
  avertissement.
- `max_age = 0` vérifie à chaque appel (une requête légère), et
  `max_age = Inf` ne vérifie jamais, comme fairify.

On ne renouvelle donc jamais un fichier à l'aveugle quand il est
périmé : on vérifie d'abord, ce qui ne coûte qu'une requête quand le
fichier n'a pas changé.

**Taille, `max_size`** (10 Go par défaut, `Inf` pour désactiver) : après
chaque téléchargement, si le cache du storage dépasse `max_size`, on
supprime d'abord les fichiers lus le moins récemment (LRU), jusqu'à
repasser sous 80 % de la limite.

Pour chaque fichier en cache, un petit fichier voisin `<fichier>.kcloud`
(en JSON) enregistre son ETag, sa date de vérification et sa date de
dernière lecture. On utilise un fichier par fichier, et non un index
commun, pour que **plusieurs workers puissent écrire en même temps** sans
corrompre le cache (cluster PTGE, `lapply_workers()`). Un fichier
téléchargé est d'abord écrit dans un fichier temporaire, puis renommé :
un téléchargement interrompu ne laisse jamais de fichier tronqué.

### 4.4 Lister de gros buckets

Lister fait bien partie du package : `cloud_path()` en a besoin pour les
dossiers, `cloud_exists()` aussi, et on s'en sert pour découvrir les
fichiers (DRIAS, sorties PTGE). C'est souvent l'opération la plus lente
sur un gros bucket. Voici pourquoi, et ce que fait kcloud.r.

**La limite de S3.** L'API S3 (ListObjectsV2) renvoie au plus 1 000 clés
par requête. Les pages s'enchaînent ensuite par un jeton de
continuation, donc forcément l'une après l'autre. Pour 1 million d'objets
sous un même préfixe, il faut 1 000 requêtes successives, soit environ
2 à 5 minutes (ordre de grandeur, à mesurer). Le seul filtre qu'applique
le serveur est le préfixe : un filtre par motif se fait toujours côté
client.

**Les optimisations, de la plus rentable à la moins rentable :**

1. **Préfixe le plus précis possible.** `cloud_list(st, "drias/ALADIN/")`
   ne demande au serveur que cette partie du bucket.
2. **Liste non récursive par défaut**, avec `delimiter = "/"` : on ne
   reçoit que les sous-dossiers directs, pas les millions de clés en
   dessous.
3. **Liste récursive parallèle par préfixe.** On liste d'abord les
   sous-dossiers, puis chacun d'eux en parallèle
   (`req_perform_parallel()`, `workers` requêtes simultanées). Pagination
   comprise, on passe d'environ 1 000 requêtes successives à environ
   1 000 / `workers` vagues. Cela marche bien parce que nos buckets sont
   rangés en arborescence (modèle/scénario/variable). rclone fait la même
   chose.
4. **Analyse XML vectorisée.** On fait un seul `xml2::xml_find_all()` par
   page, et les colonnes sont construites comme des vecteurs. aws.s3 et
   paws, eux, convertissent chaque clé en liste R puis assemblent les
   lignes, ce qui est très lent au-delà de 100 000 clés.
5. **Filtre `pattern` appliqué page par page.** On ne garde en mémoire que
   les lignes retenues.
6. **Cache de la liste.** Avec `max_age > 0`, le data.frame est mis en
   cache dans `<cache>/<name>/.lists/`, en fichier rds, indexé par
   préfixe et récursivité. radis.api le fait déjà à la main pour DRIAS
   (`drias_cache`).
7. **`cloud_exists()` vectorisé.** Au-delà de quelques dizaines de
   chemins, il fait une seule liste du dossier parent commun au lieu
   d'une requête HEAD par chemin. C'est utile pour vérifier quels
   scénarios PTGE sont déjà calculés.
8. **Délégation à rclone** pour les très gros volumes, s'il est installé
   (`cloud_rclone()`, ou `engine = "rclone"` sur un storage s3).
   `rclone lsjson --fast-list` est écrit en Go et reste le plus rapide.

Ce qui ne fait pas partie de la v1 : S3 Inventory (un listing quotidien
du bucket fourni par le serveur, que tous les hébergeurs ne proposent
pas) et la lecture directe de Parquet distant avec arrow, sans passer par
le téléchargement.

Les transferts massifs (`cloud_upload()` d'un dossier, `cloud_path()`
d'un dossier S3) utilisent le même parallélisme, avec des requêtes GET et
PUT simultanées.

## 5. Configuration (recommandation pour le point 3)

### 5.1 Format

C'est le même `config.yml` qu'aujourd'hui, au format du package
`config` (profils `default`, etc.), avec une nouvelle section `storage:`
qui contient **un ou plusieurs storages nommés** :

```yaml
default:
  storage:
    data:
      backend: nextcloud
      url_env: OPTIRRIGDATASETS_DATA_URL
      url: "https://nextcloud.inrae.fr/s/r9NidM2JSpyHYZx"
    outputs:
      backend: s3
      bucket: optirrig-ptge
      prefix: v05/
      endpoint: https://s3.example.org
  templates:
    # ... le reste de la config du package, qui ne change pas

local:
  storage:
    data:
      backend: local
      root: ~/data/OptirrigDatasets
```

Pourquoi des storages nommés ? OptirrigPTGE lit ses entrées sur Nextcloud
et écrit ses sorties sur S3. Une seule section `data:`, comme
aujourd'hui, ne permet pas de décrire ces deux storages.

Pour passer du mode distant au mode local, il y a deux façons, et aucune
ne demande de toucher au code :

- activer un profil avec `R_CONFIG_ACTIVE=local` ;
- ou mettre un `config.yml` utilisateur dans le projet, qui redéfinit
  `storage$data`.

### 5.2 Fonctions

```r
load_config(
  path_default,               # inst/config.yml du package appelant
  path_user = "config.yml",   # cherché en remontant jusqu'à la racine
  profile = Sys.getenv("R_CONFIG_ACTIVE", "default")
)

cloud_from_config(cfg, name = "data")
# cfg : la config entière (liste), sa section storage, ou un fichier .yml
```

`load_config()` reprend le comportement de `fairify::loadConfig()` (on
fusionne la config par défaut et la config utilisateur), avec deux
différences :

- **plus de `findFairifiedPackage()`** : le package appelant passe
  explicitement `path_default` ;
- **plus de validation de `data$mode`**, puisque c'est
  `cloud_from_config()` qui valide la section storage.

On la met dans kcloud.r parce que c'est elle qui lit la section storage.
Elle évite aussi à chaque package de garder kutils ou fairify uniquement
pour ça.

### 5.3 Secrets et valeurs par variable d'environnement

**Règle générale** : toute clé `x` d'un storage peut être remplacée par
`x_env: NOM_DE_VARIABLE`. Si cette variable est définie et non vide, sa
valeur l'emporte sur `x`. Ça règle l'issue #26 d'OptirrigDatasets : l'URL
publique peut sortir du dépôt, `url` gardant une valeur par défaut ou
pouvant être retirée.

Les secrets (`password`, `secret_access_key`) **ne peuvent venir que
d'une variable d'environnement**. `cloud_from_config()` refuse un fichier
où un secret est écrit en clair. Les variables par défaut sont :

| backend   | variables                                             |
|-----------|-------------------------------------------------------|
| nextcloud | `KCLOUD_NEXTCLOUD_USER`, `KCLOUD_NEXTCLOUD_PASSWORD`  |
|           | (mot de passe d'application Nextcloud)                |
| s3        | `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,         |
|           | `AWS_DEFAULT_REGION`, `AWS_S3_ENDPOINT`               |
| rclone    | aucune : c'est la config rclone qui s'en charge       |

Pour une valeur donnée, on prend dans cet ordre (du prioritaire au moins
prioritaire) : l'argument du constructeur, puis la variable
d'environnement, puis la clé du fichier de config.

### 5.4 Lecture de l'ancien format (transition)

`cloud_from_config()` sait aussi lire l'ancienne section
`data: {mode, local, remote}` de fairify, et la convertit en
`cloud_local` ou `cloud_nextcloud`. Elle émet alors un message unique qui
invite à migrer. Grâce à ça, les `config.yml` utilisateurs déjà en place
continuent de marcher pendant la migration. Cette lecture sera retirée en
v1.0.

## 6. Ajouter un backend

Les fonctions exportées de la section 4 sont toutes construites sur des
génériques internes, vectorisés pour paralléliser les requêtes :

```r
backend_get(storage, path, path_local, workers)  # télécharge, -> ETags
backend_put(storage, path_local, path, workers)  # envoie des fichiers
backend_mkdir(storage, path, workers)            # crée des dossiers
backend_delete(storage, path)                    # supprime
backend_stat(storage, path, workers)             # listing + `exists`
backend_ls(storage, path, recursive, workers)    # listing (section 4.2)
```

Les dossiers, le cache, `max_age`, `max_size` et `read_only` sont gérés
une seule fois, dans le cœur. Un backend peut surcharger
`backend_get_dir()` quand il a mieux à proposer que de télécharger les
fichiers un par un : par exemple `?accept=zip` sur Nextcloud, ou
`rclone copy`.

**Limitation du débit.** Le serveur Nextcloud de l'INRAE répond HTTP 429
(Too Many Requests) quand on enchaîne les requêtes, y compris une liste
récursive séquentielle d'un gros dossier. Chaque requête est donc
réessayée jusqu'à 5 fois, en attendant le délai `Retry-After` donné par le
serveur, ou en espaçant les essais de façon exponentielle. Par défaut,
on n'envoie que 4 requêtes simultanées à Nextcloud (option
`kcloud.r.workers`).

Implémentation de chaque backend :

| backend   | transport                     | dossier en lecture        |
|-----------|-------------------------------|---------------------------|
| local     | `file.copy()`, `list.files()` | chemin direct             |
| nextcloud | WebDAV via httr2 (GET, PUT,   | `?accept=zip` puis unzip  |
|           | PROPFIND, MKCOL), xml2        |                           |
| s3        | httr2 + `req_auth_aws_v4()`,  | liste puis GET parallèles |
|           | xml2                          |                           |
| rclone    | `system2("rclone", ...)`      | `rclone copy`             |

URLs WebDAV pour Nextcloud :

- lien public `https://host/s/TOKEN` :
  `https://host/public.php/dav/files/TOKEN/<path>` ;
- compte authentifié :
  `https://host/remote.php/dav/files/<user>/<root>/<path>`.

## 7. Exemples

Utilisation directe :

```r
library(kcloud.r)

st <- cloud_nextcloud("https://nextcloud.inrae.fr/s/r9NidM2JSpyHYZx")
d <- readxl::read_excel(cloud_path(st, "Raw/PRESTI/Maize/obs.xlsx"))
dir_raw <- cloud_path(st, "Raw/PRESTI/Soybean/2023_soja_Lav/")
cloud_list(st, "Raw/PRESTI")
cloud_cache_clean(st)
```

Migration d'OptirrigDatasets. Seuls `R/getDataPath.R` et
`R/loadConfig.R` changent. Les fonctions internes gardent leur nom, pour
que les fichiers de Florian n'aient pas à bouger :

```r
loadConfig <- function(path_user = "config.yml") {
  kcloud.r::load_config(
    system.file("config.yml", package = "OptirrigDatasets"),
    path_user = path_user
  )
}

getDataPath <- function(path, ..., cfg = loadConfig()) {
  storage <- kcloud.r::cloud_from_config(cfg, "data")
  kcloud.r::cloud_path(storage, path, ...)
}
```

Sorties d'OptirrigPTGE vers S3 :

```r
st_out <- cloud_from_config(load_config(path_default), "outputs")
cloud_upload(st_out, path_outputs, "scenarios/")
```

## 8. Dépendances

- **Imports** : httr2 (>= 1.1.0, pour `req_auth_aws_v4()`), xml2, yaml,
  config (lui-même ne dépend que de yaml), jsonlite, tools, utils.
- **Suggests** : testthat (>= 3.0.0), withr.
- **Outil système facultatif** : rclone.

À comparer avec fairify : plus de bookdown, devtools, pkgdown, httr ni
urltools.

## 9. Tests

- Le backend local est testé en entier sur des dossiers temporaires
  (`withr::local_tempdir()`).
- Nextcloud et S3 sont testés avec des réponses HTTP simulées
  (`httr2::local_mocked_responses()`), sans aucun réseau.
- Des tests d'intégration réels ne tournent que si `KCLOUD_TEST_NEXTCLOUD`
  ou `KCLOUD_TEST_S3` sont définies ; ils sont sautés sur CRAN et en CI
  par défaut.
- Un test de portabilité vérifie qu'aucune source ne contient de chemin
  `/home` ni d'appel `system()`.

## 10. Feuille de route

| version | contenu                                     | débloque           |
|---------|---------------------------------------------|--------------------|
| 0.1.0   | cœur, cache (`max_age`, `max_size`), config | retrait de fairify |
|         | et ancien format, `local`, `nextcloud`      | (OptirrigDatasets) |
|         | (lien public et compte), liste parallèle,   |                    |
|         | cache des listes, `cloud_exists()` vectorisé|                    |
| 0.2.0   | `s3` sur httr2                              | radis.api, PTGE    |
| 0.3.0   | `rclone`                                    | gros transferts    |

## 11. Décisions

| # | question                    | décision (2026-10-27)                    |
|---|-----------------------------|------------------------------------------|
| 1 | nom du package              | kcloud.r (dépôt GitHub renommé)          |
| 2 | API                         | API neuve ; chaque package garde ses     |
|   |                             | fonctions internes de même nom           |
| 3 | config                      | `storage:` avec des storages nommés,     |
|   |                             | clés `x_env`, secrets uniquement en      |
|   |                             | variables d'environnement                |
| 4 | feuille de route            | local et nextcloud, puis s3, puis rclone |
| 5 | préfixe des fonctions       | `cloud_*`, comme le nom du package       |
| 6 | `load_config()`             | dans kcloud.r                            |
| 7 | client S3                   | httr2 natif (`req_auth_aws_v4()`), ni    |
|   |                             | aws.s3 ni paws ; rclone pour les très    |
|   |                             | gros volumes                             |
| 8 | politique du cache          | `max_age` (7 jours, puis requête         |
|   |                             | conditionnelle) + `max_size` (10 Go,     |
|   |                             | LRU)                                     |

Alternatives écartées pour le client S3 :

- **aws.s3** : il n'est plus maintenu (dernière version en 2020),
  repose sur httr, ne parallélise pas, et son analyse XML devient lente
  au-delà de 100 000 clés.
- **paws.storage** : il est maintenu, mais c'est une grosse dépendance,
  il ne parallélise pas non plus et il convertit les réponses en listes
  R imbriquées.
- **arrow** (`S3FileSystem`) : il liste vite, en C++, mais c'est une
  dépendance énorme. On le garde pour une éventuelle lecture directe de
  Parquet.
