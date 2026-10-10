# =====================================================================
# Workflow complet avec georefGN : gazetteer, géoréférencement, validation,
# effet sur les métriques UICN (ConR). Adapter les chemins et les pays.
# =====================================================================
library(georefGN)

pays   <- c("CI", "GN", "LR", "SL", "ML", "BF", "GH", "BJ", "TG")
sortie <- "sorties"

# 1. GeoNames (une seule fois) puis lecture du gazetteer
if (!dir.exists("geonames")) download_geonames(pays, dir = "geonames")
gaz <- read_gazetteer("geonames", countries = pays)

# 2. Données GBIF : objet `gbif_raw` (data.frame ou résultat de rgbif::occ_data())
if (!exists("gbif_raw")) stop("Charger d'abord les données GBIF dans `gbif_raw`.")

# 3. Géoréférencement (paramètres de l'article)
p   <- georef_params(countries = pays)
res <- georeference(gbif_raw, gaz, p)
write_georef(res, sortie)

# 4. Validation par tirages répétés, avec puis sans les doublons d'herbier comme ancres
v  <- validate_georef(gbif_raw, gaz, p, n_sample = 300, n_rep = 10)
write_validation(v, sortie, prefix = "validation_v4_")
v2 <- validate_georef(gbif_raw, gaz, p, n_sample = 300, n_rep = 10, mask_duplicates = TRUE)
write_validation(v2, sortie, prefix = "validation_v4_sans_doublons_")

# 5. Métriques UICN avant / après (ConR)
if (requireNamespace("ConR", quietly = TRUE)) {
  cmp <- compare_conr(res)
  write_conr(cmp, file.path(sortie, "comparaison_conr.xlsx"))
  # comparaison facultative à une évaluation de référence (une ligne par taxon)
  # ref <- readxl::read_excel("uicn_reference.xlsx")
  # compare_reference(cmp, ref, tax_col = "tax", cat_col = "Category_Final")
}
