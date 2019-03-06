# Data cleanup and renaming according to https://wiki.openstreetmap.org/wiki/Import_information_and_care_points_for_women_and_LGTBI_collectives_in_Catalunya#Data_Preparation


# Dependencies ------------------------------------------------------------

library(dplyr)


# J007_PuntsInformacioYAtencioDones ---------------------------------------

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

# Serveis i oficines d'informació i atenció a les dones -------------------

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

oficines$amenity = "social_facility"
oficines$`social_facility:for` = "woman"
oficines$source = "Generalitat de Catalunya"
oficines$`source:date` = "2018-05-03"

write.csv(oficines, file = "output/oficines_clean.csv")
