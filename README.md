# georefGN

<!-- badges: start -->
[![R-CMD-check](https://github.com/[À COMPLÉTER]/georefGN/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/[À COMPLÉTER]/georefGN/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Géoréférencement automatique d'occurrences d'herbier et GBIF à partir des descriptions de localités, avec GeoNames et les itinéraires des collecteurs.**

*English summary below.*

Une grande partie des spécimens d'herbier publiés sur GBIF n'a pas de coordonnées, seulement un texte de localité (« 10 km N de Man », « forêt classée de Yapo »). `georefGN` leur attribue des coordonnées avec une incertitude, en deux phases :

1. **Doublons d'herbier.** Un spécimen sans coordonnées reprend celles d'un doublon géoréférencé (même collecteur, même numéro de collecte).
2. **Phase collecteur.** Si le même collecteur a récolté au même lieu GeoNames à ± 30 jours et que cette récolte est géoréférencée, ses coordonnées sont reprises. Les lieux sont comparés par identifiant GeoNames, quelle que soit la graphie (variantes, noms historiques). Les spécimens sans date sont datés par leur **numéro de collecte**, à partir des numéros datés du même collecteur qui l'encadrent (au plus 100 numéros par jour).
3. **Phase textuelle.** Les toponymes du texte sont retrouvés dans GeoNames et ses noms alternatifs. Les homonymes sont départagés par l'unité administrative déclarée (`stateProvince`, `county`), puis en préférant les localités et les lieux les plus peuplés, ou par la combinaison la plus compacte quand il y a plusieurs toponymes. Les expressions de distance et de direction sont appliquées, et l'incertitude dépend du type d'entité (localité, cours d'eau, forêt classée…).

Les coordonnées GBIF d'origine sont contrôlées au préalable : les inversions latitude/longitude et les erreurs de signe sont corrigées quand la correction est univoque.

Le paquet fournit aussi :

- une **validation par masquage répété**, avec intervalle de confiance et comparaison appariée avec/sans phase collecteur ;
- une **comparaison des métriques UICN du critère B** (EOO, AOO, localités) calculées avec [ConR](https://github.com/gdauby/ConR) avant et après géoréférencement.

## Installation

```r
# install.packages("remotes")
remotes::install_github("[À COMPLÉTER]/georefGN")
```

Paquets facultatifs : `ConR` (métriques UICN), `CoordinateCleaner` et `countrycode` (contrôle du pays), `stringdist` (recherche approchée, déconseillée), `writexl` (export Excel).

## Utilisation

```r
library(georefGN)

# 1. Gazetteer GeoNames (fichiers par pays + noms alternatifs complets, CC BY 4.0)
pays <- c("CI", "GN", "LR", "SL", "ML", "BF", "GH", "BJ", "TG")
download_geonames(pays, dir = "geonames")
gaz <- read_gazetteer("geonames", countries = pays)

# 2. Données GBIF : data.frame, résultat de rgbif::occ_data() ou occ_search().
#    Ne filtrez pas sur hasCoordinate : on veut justement les enregistrements sans coordonnées.
# gbif_raw <- rgbif::occ_data(scientificName = ..., limit = ...)

# 3. Géoréférencement
p   <- georef_params(countries = pays)        # paramètres par défaut de l'article
res <- georeference(gbif_raw, gaz, p)
res$georeferenced          # enregistrements géoréférencés (source, confiance, incertitude)
res$conr_ready             # ddlat, ddlon, tax pour ConR
write_georef(res, "sorties")

# 4. Validation : 10 tirages disjoints de 300 enregistrements masqués
v <- validate_georef(gbif_raw, gaz, p, n_sample = 300, n_rep = 10)
plot(v)
write_validation(v, "sorties")
# même chose en masquant aussi les doublons d'herbier du spécimen testé
v2 <- validate_georef(gbif_raw, gaz, p, mask_duplicates = TRUE)

# 5. Effet sur les métriques UICN (nécessite ConR)
cmp <- compare_conr(res)
cmp$summary_sets
write_conr(cmp, "sorties/comparaison_conr.xlsx")
```

Un exemple fictif, qui tourne sans téléchargement :

```r
ex  <- georef_example()
gaz <- read_gazetteer(ex$geonames_dir, countries = "CI")
res <- georeference(ex$occurrences, gaz, georef_params(countries = "CI"))
```

Le script `inst/scripts/workflow_complet.R` enchaîne toutes les étapes ; il est aussi accessible par `system.file("scripts", "workflow_complet.R", package = "georefGN")`.

## Sources et niveaux de confiance

| `geo_source` | Méthode | `geo_confiance` |
|---|---|---|
| `GBIF` | coordonnées d'origine (éventuellement corrigées) | `originale` / `corrigee` |
| `Doublon_herbier` | coordonnées d'un doublon du même spécimen (même collecteur, même numéro) | `haute` |
| `Collecteur_palier1` | coordonnées d'une récolte du même collecteur au même lieu à ± 30 jours | `haute` |
| `GeoNames_texte` | toponymes du texte | `haute` (≥ 2 toponymes dont une localité), `moyenne` (1 toponyme) ou `basse (entite etendue)` (cours d'eau, zone, relief étendu) |
| `GeoNames_texte_offset` | toponyme + distance et direction | `moyenne (distance/direction)` |
| `GeoNames_flou` | recherche approchée (désactivée par défaut) | `moyenne (flou)` / `basse (flou)` |

## Précautions

- Un point de la phase textuelle est le centre d'une localité, pas le lieu de récolte. Dans la validation de l'article, son erreur médiane est d'environ 12 km : ces points conviennent à l'EOO et au nombre de localités, **pas à l'AOO en mailles de 2 × 2 km** sans vérification.
- Les points de la phase collecteur sont beaucoup plus précis (erreur médiane inférieure à 1 m dans la validation), mais environ 9 % sont à plus de 25 km de la position réelle.
- La recherche approchée (`fuzzy = TRUE`) est déconseillée : dans la validation, ses points avaient une erreur médiane de 225 km.
- Vérifiez `res$to_check` à la main, ainsi que tout changement de catégorie UICN qui repose sur des points reconstitués.

## Citation

Voir `citation("georefGN")`. Merci de citer aussi GeoNames (CC BY 4.0) et le DOI de votre téléchargement GBIF.

## English summary

`georefGN` georeferences occurrence records (e.g. GBIF herbarium specimens) that only have a textual locality. It (1) reuses the coordinates of georeferenced records of the same collector at the same GeoNames place within ± 30 days, dating undated specimens from their collection numbers, and (2) matches the place names of the locality text to the GeoNames gazetteer and its alternate names, resolving homonyms and applying distance-direction offsets. It also checks original coordinates for lat/lon swaps and sign errors, validates the method by repeated masking of known coordinates, and compares IUCN criterion B metrics computed with ConR before and after georeferencing.

## Licence

Code : MIT. Données GeoNames : CC BY 4.0.
