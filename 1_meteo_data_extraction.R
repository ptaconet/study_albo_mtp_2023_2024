library(httr)

fun_extract_meteo_data <- function(month,year,path){

  data <- paste0("synop.",as.character(year),as.character(month),".csv.gz")
  url <- paste0("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/Archive/",data)

  httr::GET(url,httr::write_disk(file.path(path,data)),httr::progress(),config = list(maxredirs=-1))

}

months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
years <- c("2022","2023","2024")

for(i in 1:length(years)){
  for(j in 1:length(months)){
    fun_extract_meteo_data(months[j],years[i],"data_meteofrance")
  }
}
