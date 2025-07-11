library(httr)


## Source : https://www.data.gouv.fr/fr/datasets/donnees-climatologiques-de-base-quotidiennes/#/resources

liste_departements <- c(33,34,35,64)

for(i in 1:length(liste_departements)){
# données 2024-2025
httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",liste_departements[i],"_latest-2024-2025_RR-T-Vent.csv.gz"),httr::write_disk(file.path("data","raw","meteofrance",paste0("dpt_",liste_departements[i],"_2024_2025_RR-T-Vent.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))
httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",liste_departements[i],"_latest-2024-2025_autres-parametres.csv.gz"),httr::write_disk(file.path("data","raw","meteofrance",paste0("dpt_",liste_departements[i],"_2024_2025_autres_parametres.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))

# donnees historiques
httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",liste_departements[i],"_previous-1950-2023_RR-T-Vent.csv.gz"),httr::write_disk(file.path("data","raw","meteofrance",paste0("dpt_",liste_departements[i],"_historique_RR-T-Vent.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))
httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",liste_departements[i],"_previous-1950-2023_autres-parametres.csv.gz"),httr::write_disk(file.path("data","raw","meteofrance",paste0("dpt_",liste_departements[i],"_historique_autres_parametres.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))

}


# pour les données SYNOP :
# # donnees 2022 à 2024
# fun_extract_meteo_data <- function(month,year,path){
#
#   data <- paste0("synop.",as.character(year),as.character(month),".csv.gz")
#   url <- paste0("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/Archive/",data)
#
#   httr::GET(url,httr::write_disk(file.path(path,data)),httr::progress(),config = list(maxredirs=-1))
#
# }
#
# months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
# years <- c("2023","2024")
#
# for(i in 1:length(years)){
#   for(j in 1:length(months)){
#     fun_extract_meteo_data(months[j],years[i],"data_meteofrance")
#   }
# }

## Données DRIAS changement climatique : https://drias-climat.fr/
