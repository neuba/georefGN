# georefGN 0.5.0

Corrections issues de la relecture de la version 0.4.0.

* **Clé du collecteur** (`parse_collectors()`, `collector_key()`) : nom de famille complet et initiale du
  prénom. Reconnaît « Aké Assi LA », « Jongkind CCH », « AKE ASSI Laurent » ; l'ordre « Nom Prénom » /
  « Prénom Nom » est tranché par les noms déjà vus dans les données ; les homonymes de nom (Koné M., Koné D.)
  sont séparés et les mentions sans initiale ambiguës sont écartées. Une récolte d'équipe sert d'ancre à
  chacun de ses membres (`team_anchors`).
* **Doublons d'herbier** (`duplicate_step()`) : étape explicite avant le rapprochement par itinéraire
  (même collecteur, même numéro complet), avec sa propre source `Doublon_herbier`. Elle s'applique aussi
  aux enregistrements sans texte de localité.
* **Unités administratives** (`match_admin()`) : `stateProvince` et `county` sont reconnus parmi les
  unités ADM1/ADM2 de GeoNames ; les homonymes situés dans l'unité déclarée sont préférés.
* **Type d'entité** : pour un même nom, les localités passent avant reliefs, zones et cours d'eau ;
  incertitude minimale propre au type (cours d'eau 25 km, zones 15 km, reliefs étendus 15 km, capitale 8 km,
  chef-lieu 5 km) et confiance « basse (entite etendue) ». Avec plusieurs toponymes, la position est le
  barycentre des localités de la combinaison retenue.
* **Numéros de collecte** au format année-numéro (« 1975-123 », « 2005/45 ») ; `parse_record_number()`
  renvoie aussi le numéro complet (`full`).
* Validation : la variante « sans collecteur » est désormais l'appariement textuel seul ; la part résolue
  par les doublons (`pct_doublon`) est rapportée à part.
* Corrigé : plantage sur un fichier de noms alternatifs vide ; plantages quand aucun enregistrement ne
  reste à géoréférencer par le texte.

# georefGN 0.4.0

Première version sous forme de paquet, reprenant la version 4 des scripts de l'article.

* Phase collecteur limitée au palier 1 : même collecteur, même lieu GeoNames (`geonameid`), ± 30 jours.
  Les paliers « ± 90 jours » et « trajectoire du collecteur », qui ajoutaient des faux positifs, ont été retirés.
* Datation des spécimens sans date par le numéro de collecte (doublon daté ou numéros datés encadrants,
  au plus 100 numéros par jour), avec un contrôle par redatation.
* Gazetteer enrichi des fichiers complets de noms alternatifs de GeoNames (variantes, noms historiques).
* Contrôle et correction des coordonnées d'origine (inversions latitude/longitude, erreurs de signe).
* Validation par masquage répété (tirages disjoints, IC 95 %, différence appariée avec/sans collecteur),
  avec option de masquage des doublons d'herbier.
* Comparaison des métriques UICN du critère B avec ConR et comparaison à une évaluation de référence.
