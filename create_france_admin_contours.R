library(tidyverse)
library(sf)
library(DBI)


con <- dbConnect(
  RPostgres::Postgres(),
  host = "postgresql-taconet.alwaysdata.net",
  dbname = "taconet_albopictus",
  port = 5432,
  user = "******",
  password = "******"
)

communes = st_read("https://www.data.gouv.fr/api/1/datasets/r/00c0c560-3ad1-4a62-9a29-c34c98c3701e","a_com2022")
departements = st_read("https://www.data.gouv.fr/api/1/datasets/r/00c0c560-3ad1-4a62-9a29-c34c98c3701e","a_dep2022")
regions = st_read("https://www.data.gouv.fr/api/1/datasets/r/00c0c560-3ad1-4a62-9a29-c34c98c3701e","a_reg2022")

st_crs(communes) = 2154
st_crs(departements) = 2154
st_crs(regions) = 2154

communes <- communes %>% dplyr::filter(!(reg %in% c("01","02","03","04","06"))) %>% dplyr::select(codgeo, dep, reg, libgeo)
departements <- departements %>% dplyr::filter(!(reg %in% c("01","02","03","04","06"))) %>% dplyr::select(dep, reg, libgeo)
regions <- regions %>% dplyr::filter(!(reg %in% c("01","02","03","04","06"))) %>% dplyr::select(reg, libgeo)

st_write(communes,"/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/modelops/data/communes_simplified.gpkg", append = F)
st_write(departements,"/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/modelops/data/departements_simplified.gpkg", append = F)
st_write(regions,"/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/modelops/data/regions_simplified.gpkg", append = F)




st_write(communes, dsn = con, layer = "communes",append = FALSE)
st_write(departements, dsn = con, layer = "departements",append = FALSE)




##########################################
### create table of colonization of albopictus in France
##########################################

france_colonisation_albo <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/shp_alto_colonization/transfer_9935526_files_4caef5ee/France_albopictus_year_colonization.shp")

france_colonisation_albo <- france_colonisation_albo %>%
  st_drop_geometry() %>%
  rename(codgeo = insee) %>%
  mutate(presence = ifelse(presence==1, TRUE, FALSE)) %>%
  mutate(year_col = as.Date(paste0(year_col,"-01-01")))

st_write(france_colonisation_albo, dsn = con, layer = "albopictus_france_colonisation_albo",append = FALSE)

DBI:::dbSendQuery(con,'create view albopictus_france_colonisation_geo AS select b.codgeo, b.libgeo, a.presence, a.year_col, b.geometry from albopictus_france_colonisation_albo a left join communes b ON a.codgeo = b.codgeo')

##########################################
### create table of surveillance of arbovirose in France
##########################################


df_arbo_spf <- read.csv("https://odisse.santepubliquefrance.fr/api/explore/v2.1/catalog/datasets/arboviroses-donnees-declaration-obligatoire/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%2C")

df_arbo_spf <- df_arbo_spf %>%
  rename(date = Mois, type_surveillance = Type.surveillance, dep = Département.Code, departement = Département, arbovirose = Arbovirose,  nb_cas_importes = Nombre.de.cas.importés, nb_cas_autochtones = Nombre.de.cas.autochtones, reg  = Région.Code, region = Région) %>%
  mutate(date = as.Date(paste0(date,"-15")))

st_write(df_arbo_spf, dsn = con, layer = "albopictus_spf_surveillance_arboviroses",append = FALSE)

DBI:::dbSendQuery(con,'create view albopictus_spf_surveillance_arboviroses_geo AS select b.dep, b.libgeo, a.date, a.type_surveillance, a.arbovirose, a.nb_cas_importes, a.nb_cas_autochtones,  a.reg, a.region, b.geometry from albopictus_spf_surveillance_arboviroses a left join departements b ON a.dep = b.dep')

##########################################
### create table of IRIS - herault
##########################################

iris_herault <- read_sf("https://www.herault-data.fr/api/explore/v2.1/catalog/datasets/georef-herault-iris/exports/geojson?lang=fr&timezone=Europe%2FBerlin")

iris_herault <- iris_herault %>%
  dplyr::select(nom_officiel_iris,code_officiel_commune, nom_officiel_commune, code_officiel_iris, nom_officiel_iris_minuscule, type)

st_write(iris_herault, dsn = con, layer = "iris_herault",append = FALSE)

##########################################
### create table of local climate zone + human population
##########################################

lcz_montpellier <- read_sf("/home/ptaconet/IDG_OMEES/data/mmm/MMM_MMM_LCZ/MOS_CLIMAT_M3M_LCZ.shp")

merged_lcz <- lcz_montpellier %>%
  group_by(LCZ_Class) %>%
  summarise(do_union = TRUE) %>%   # dissolve by zone
  st_cast("MULTIPOLYGON") %>%      # break multipolygons into individual parts
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(surf_ha = st_area(.)/10000) %>%
  mutate(surf_ha = as.numeric(surf_ha)) %>%
  mutate(ID = seq(1,nrow(.),1)) %>%
  relocate(ID,1)


# Etablir les correspondances avec codification urban atlas

lcz_to_ua <- read.csv("/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/arbocarto_2025/LCZ_to_UA_long_labels.csv") %>%
  rename(code_urban_altas = UA.Code, LCZ_Class = LCZ.Code ) %>%
  mutate(LCZ_Class = gsub("LCZ ","",LCZ_Class))

f <- system.file("data/configK.Rda", package = "arbocartoR")
load(f)
configK <- configK %>%
  rename(code_urban_altas=CODE) %>%
  left_join(lcz_to_ua) %>%
  filter(!is.na(LCZ_Class)) %>%
  group_by(LCZ_Class) %>%
  summarise(nbL_ha = mean(nbL_ha), p_gite_anthro = mean(p_gite_anthro), Klfix=mean(Klfix), Klvar=mean(Klvar)) %>%
  add_row(LCZ_Class = "E/F", nbL_ha = 0, p_gite_anthro = 0, Klfix = 0, Klvar = 0) %>%
  add_row(LCZ_Class = "H", nbL_ha = 0, p_gite_anthro = 0, Klfix = 0, Klvar = 0) %>%
  add_row(LCZ_Class = "Dw", nbL_ha = 0, p_gite_anthro = 0, Klfix = 0, Klvar = 0)


merged_lcz <- merged_lcz %>%
  left_join(configK)


mmm_popfine <- read_sf("/home/ptaconet/IDG_OMEES/data/mmm/MMM_MMM_PopFine/MMM_MMM_PopFine.shp") %>%
  st_intersection(merged_lcz) %>%
  st_drop_geometry() %>%
  group_by(ID) %>%
  summarise(pop = sum(pop_2021)) %>%
  ungroup()

merged_lcz <- left_join(merged_lcz,mmm_popfine) %>%
  mutate(pop = ifelse(is.na(pop), 0, pop))


st_write(merged_lcz, dsn = con, layer = "mmm_local_climate_zone",append = FALSE)
