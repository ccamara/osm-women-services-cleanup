# Data cleanup and renaming according to https://wiki.openstreetmap.org/wiki/Import_information_and_care_points_for_women_and_LGTBI_collectives_in_Catalunya#Data_Preparation


# Dependencies ------------------------------------------------------------

library(tidyverse)
library(readxl)

# 1. J007_PuntsInformacioYAtencioDones -------------------------------------

# Uncomment below to Fetch latest data from City council.
# download.file("https://opendata-ajuntament.barcelona.cat/data/dataset/c808aef7-3c0b-4a2f-a45f-f0b99ffc90b6/resource/9078c2db-a1b3-4d9d-9e57-d52d906222de/download",
#                          destfile = "data/J007_raw.csv")

J007_raw = read.csv(file = "data/J007_raw.csv")


J007 = J007_raw %>%
  select(CODI_EQUIPAMENT, EQUIPAMENT, TIPUS_VIA, NOM_CARRER, NUM_CARRER_1,
         CODI_POSTAL, POBLACIO, LATITUD, LONGITUD, TELEFON_NUM, HORARI_OBSERVACIONS) %>%
  rename(ref = CODI_EQUIPAMENT,
         name = EQUIPAMENT,
         `addr:housenumber` = NUM_CARRER_1,
         `addr:postcode` = CODI_POSTAL,
         `addr:city` = POBLACIO,
         `contact:phone` = TELEFON_NUM,
         description = HORARI_OBSERVACIONS,
         latitude = LATITUD,
         longitude = LONGITUD) %>%
  mutate(TIPUS_VIA = as.character(TIPUS_VIA)) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "Av", "Avinguda de")) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "C", "Carrer de")) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "G.V.", "Gran via de les")) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "Pl", "Plaça")) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "Av", "Avinguda de")) %>%
  mutate(`addr:street` = as.character(paste(TIPUS_VIA, NOM_CARRER,
                                            sep = " "))) %>%
  select(-TIPUS_VIA, -NOM_CARRER)

J007$description = gsub("<br>", "", J007$description)

# TODO: contruct opening_times column.

# TODO: replace de by d' if they are followed by vocal. The following code is an
# unsuccessful attempt.
# J007$`addr:street` = gsub("(de )[AEIOU]", "d'", J007$`addr:street`)

# Add fixed values.
J007$amenity = "social_facility"
J007$`social_facility:for` = "woman"
J007$source = "Ajuntament de Barcelona"
J007$`source:date` = "2019-01-29"
J007$`contact:phone` = paste("+34", J007$`contact:phone`, sep = " ")
J007$`addr:city` = "Barcelona"

write.csv(J007, file = "output/J007_clean.csv", row.names = FALSE)

# 2. Serveis i oficines d'informació i atenció a les dones -----------------

# Uncomment below to Fetch latest data from City council.
# download.file("https://analisi.transparenciacatalunya.cat/resource/9uvn-hv6j.csv",
#                          destfile = "data/9uvn-hv6j_raw.csv")


oficines_raw = read.csv(file = "data/9uvn-hv6j_raw.csv")

oficines = oficines_raw %>%
  select(nom_del_centre, tipus_de_via, adre_a, codi_postal, poblaci, tel_fon,
         correu_electr_nic, titularitat, latitud, longitud) %>%
  rename(name = nom_del_centre,
         `addr:postcode` = codi_postal,
         `addr:city` = poblaci,
         `contact:phone` = tel_fon,
         `contact:email` = correu_electr_nic,
         operator = titularitat,
         latitude = latitud,
         longitude = longitud)

# TODO: split address.

oficines$amenity = "social_facility"
oficines$`social_facility:for` = "woman"
oficines$source = "Generalitat de Catalunya"
oficines$`source:date` = "2018-05-03"

write.csv(oficines, file = "output/oficines_clean.csv")


# 3. The LGBTI Comprehensive Services Services Network of Catalonia  -------
#
# Uncomment below to Fetch latest data from City council.
# download.file("https://analisi.transparenciacatalunya.cat/resource/utrz-jf79.csv",
#                          destfile = "data/LGBTI_raw.csv")


LGBTI_raw = read.csv(file = "data/LGBTI_raw.csv")

LGBTI = LGBTI_raw %>%
  select(denominaci, adre_a, municipi, cp, tel_fon, latitud, longitud) %>%
  rename(name = denominaci,
         `addr:street` = adre_a,
         `addr:city` = municipi,
         `addr:postcode` = cp,
         `contact:phone` = tel_fon,
         latitude = latitud,
         longitude = longitud)

# TODO: split address.

LGBTI$amenity = "social_facility"
LGBTI$`social_facility:for` = "LGBTI"
LGBTI$source = "Generalitat de Catalunya"
LGBTI$`source:date` = "2019-01-21"

write.csv(LGBTI, file = "output/LGBTI_clean.csv")


#  4. Guide of entities of women of Catalonia  ----------------------------

# Uncomment below to Fetch latest data source.
# Source: Guia d'entitats de dones de Catalunya
# download.file("https://analisi.transparenciacatalunya.cat/resource/amca-cgrp.csv",
#                          destfile = "data/entitats_dones_raw.csv")


entitats_dones_raw = read.csv(file = "data/entitats_dones_raw.csv")

entitats_dones = entitats_dones_raw %>%
  select(organisme, tel_fon, email, web, artesania, coeducaci,
         comunicaci_societat_de_la, cultura_i_arts, diversitat_funcional,
         dones_grans, drets_humans_i_cooperaci,
         drets_sexuals_i_reproductius, ecofeminismes,
         economia_feminista, emprenedoria_i_direcci_d, esports,
         inserci_laboral_i_ocupaci, interculturalitat_i_migraci,
         jur_dic_legal, lgtbi, lleure, m_n_rural, pensaments_feministes_i,
         promoci_d_activitats_i, salut, sororitat_i_ajuda_m_tua) %>%
  rename(name = organisme,
         `contact:phone` = tel_fon,
         `contact:email` = email,
         `contact:website` = web)

# Generate a description column with all the items marked as "Sí" in several
# columns.
entitats_dones_description = entitats_dones %>%
  select(-`contact:phone`, -`contact:email`, -`contact:website`) %>%
  gather(key, value, -name) %>%
  filter(value == "Sí") %>%
  mutate(value = key) %>%
  spread(key, value) %>%
  unite(description, 2:22, sep = "; ")

entitats_dones_description$description <- gsub("NA; ", "",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("; NA", "",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("coeducaci", "coeducació",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("comunicaci_societat_de_la", "comunicació, societat de la informació i TIC",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("cultura_i_arts", "cultura i arts",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("diversitat_funcional", "diversitat funcional",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("dones_grans", "dones grans",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("drets_humans_i_cooperaci", "drets humans i cooperació",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("drets_sexuals_i_reproductius", "drets sexuals i reproductius",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("emprenedoria_i_direcci_d", "emprenedoria i direcció d'empreses",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("inserci_laboral_i_ocupaci", "inserció laboral i ocupació",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("interculturalitat_i_migraci", "interculturalitat i migració",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("jur_dic_legal", "jurídic i legal",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("m_n_rural", "món rural",
                                               entitats_dones_description$description)

entitats_dones_description$description <- gsub("pensaments_feministes_i", "pensaments feministes",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("promoci_d_activitats_i", "promoció d'actitats i reivindicació dels drets de les dones",
                                               entitats_dones_description$description)
entitats_dones_description$description <- gsub("sororitat_i_ajuda_m_tua", "sororitat i ajuda mútua",
                                               entitats_dones_description$description)

entitats_dones = entitats_dones %>%
  left_join(entitats_dones_description) %>%
  select(name, `contact:phone`, `contact:email`, `contact:website`,
         description) %>%
  mutate(amenity = "social_facility") %>%
  mutate(`social_facility:for` = "woman") %>%
  mutate(source = "Generalitat de Catalunya") %>%
  mutate(`source:date` = "2019-03-29") %>%
  mutate(`contact:phone` = paste("+34", `contact:phone`, sep = " "))

# Add address from manual file created by @Lanxana.
entitats_dones_address = read_excel("data/entitats_dones_depurada.xlsx",
                                     sheet = 1) %>%
  select(Id, Entitat)

entitats_dones_address2 = read_excel("data/entitats_dones_depurada.xlsx",
                                     sheet = 2) %>%
  select(Id, `addr:street`, `addr:housenumber`, `addr:block`, `addr:floor`,
         `addr:housename`, `addr:postcode`, `addr:city`, `addr:place`)

entitats_dones_address = entitats_dones_address %>%
  left_join(entitats_dones_address2, by = "Id")

# Add geocode from manuallly geocoded.
entitats_dones_geocoded = read.csv(file = "data/entitats_dones_geocodificades.csv",
                                   sep = ";", encoding = "Latin-1") %>%
  select(Id, latitud, longitud)

entitats_dones_address = entitats_dones_address %>%
  left_join(entitats_dones_geocoded, by = "Id")

entitats_dones_full = entitats_dones %>%
  left_join(entitats_dones_address, by = c("name" = "Entitat"))

write.csv(entitats_dones_full, file = "output/d4.entitats_dones.csv")
