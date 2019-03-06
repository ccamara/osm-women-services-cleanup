library(dplyr)

# J007_PuntsInformacioYAtencioDones ---------------------------------------

# Uncomment below to Fetch latest data from City council.
# download.file("https://opendata-ajuntament.barcelona.cat/data/dataset/c808aef7-3c0b-4a2f-a45f-f0b99ffc90b6/resource/9078c2db-a1b3-4d9d-9e57-d52d906222de/download",
#                          destfile = "data/J007_raw.csv")


J007_raw = read.csv(file = "data/J007_raw.csv")

# Data cleanup and renaming according to https://wiki.openstreetmap.org/wiki/Import_information_and_care_points_for_women_and_LGTBI_collectives_in_Catalunya#Data_Preparation

J007 = J007_raw %>%
  select(CODI_EQUIPAMENT, EQUIPAMENT, TIPUS_VIA, NOM_CARRER, NUM_CARRER_1,
         CODI_POSTAL, POBLACIO, LATITUD, LONGITUD, TELEFON_NUM, HORARI_OBSERVACIONS) %>%
  rename(ref = CODI_EQUIPAMENT,
         name = EQUIPAMENT,
         `addr:housenumber` = NUM_CARRER_1,
         `addr:postcode` = CODI_POSTAL,
         `addr:city` = POBLACIO,
         `contact:phone` = TELEFON_NUM,
         description = HORARI_OBSERVACIONS) %>%
  mutate(TIPUS_VIA = as.character(TIPUS_VIA)) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "Av", "Avinguda de")) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "C", "Carrer de")) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "G.V.", "Gran via de les")) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "Pl", "Plaça")) %>%
  mutate(TIPUS_VIA = replace(TIPUS_VIA, TIPUS_VIA == "Av", "Avinguda de")) %>%
  mutate(`addr:street` = as.character(paste(TIPUS_VIA, NOM_CARRER,
                                            sep = " "))) %>%
  select(-TIPUS_VIA, -NOM_CARRER)

# TODO: contruct opening_times column.

# TODO: replace de by d' if they are followed by vocal. The following code is an
# unsuccessful attempt.
# J007$`addr:street` = gsub("(de )[AEIOU]", "d'", J007$`addr:street`)

# Add fixed values.
J007$amenity = "social_facility"
J007$`social_facility:for` = "woman"
J007$source = "Ajuntament de Barcelona"
J007$`source:date` = "2019-01-29"

write.csv(J007, file = "output/J007_clean.csv", row.names = FALSE)

