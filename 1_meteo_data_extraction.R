library(httr)
#
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

## Source : https://www.data.gouv.fr/fr/datasets/donnees-climatologiques-de-base-quotidiennes/#/resources

# données 2023-2024
httr::GET("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_34_latest-2023-2024_RR-T-Vent.csv.gz",httr::write_disk(file.path("data_meteofrance","dpt_34_2023_2024.csv.gz")),httr::progress(),config = list(maxredirs=-1))


# donnees historiques
httr::GET("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_34_previous-1950-2022_RR-T-Vent.csv.gz",httr::write_disk(file.path("data_meteofrance","dpt_34_historique.csv.gz")),httr::progress(),config = list(maxredirs=-1))
