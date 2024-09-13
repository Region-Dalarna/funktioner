# ========================================================================================================
#
# Skript för att starta en OTP-server för att kunna köra Swecos skript för tillgänglighet med kollektiva
# färdmedel. Servern behöver sättas upp innan den kan sättas igång, instruktioner för hur detta görs finns
# på G:\Samhällsanalys\GIS\projekt\SWECO\OTP. 
# 
# OTP-servern körs på en serveryta som nås via fjärrskrivbord. Därefter kan skripten som ligger i samma
# mapp som instruktionerna ovan köras. 
#
# Viktiga funktioner i denna fil är:
#
#      otp_starta_server() - startar en otp-server i en cmd-process, denna cmd-process ligger kvar till vi kör otp_stang_server()
#     
#      otp_stang_server() - stänger en otp-server som är igång, om någon är igång
# 
#      otp_bygg_ny_graf() - bygger en ny otp-graf vilket behöver göras med uppdaterad
#                           gtfs-data, eller om man vill läsa in en uppdaterad 
#                           openstreetmap-karta.
#
#
# Peter Möller, Region Dalarna, juni 2024
#
# ========================================================================================================
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse, 
       rvest)


gtfs_operatorer_sverige_nyckeltabell_hamta <- function(){
  
  # hämta nyckeltabell för regioner och operatörer så att man kan hämta en operatör med
  # regionkod
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse, 
         rvest)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  
  # URL till sidan
  url_trafiklab <- "https://www.trafiklab.se/sv/api/gtfs-datasets/gtfs-regional/"
  
  # Läs in HTML-innehållet från sidan
  page <- read_html(url_trafiklab)
  
  # Hitta tabellen med data för operatörerna
  retur_df <- page %>%
    html_node("table") %>%  # Välj första tabellen
    html_table() %>% 
    select(operator = Operator, operatorkod = Abbreviation) %>% 
    mutate(region = str_extract(operator, "\\(([^)]+)\\)"),  # Extrahera regionen
           operator = str_remove(operator, "\\s*\\([^)]*\\)"),
           region = str_replace_all(region, "[()]", ""),
           region = ifelse(is.na(region) & str_detect(operator, "Länstrafiken "), operator %>% str_remove("Länstrafiken "), region),
           region = region %>% skapa_kortnamn_lan() %>% str_to_title()) %>% 
    filter(!is.na(region),
           operatorkod != "sjostadstrafiken")
  
  regionnyckel <- hamtaregtab() %>%
    filter(nchar(regionkod) == 2 & regionkod != "00") %>% 
    mutate(region = region %>% skapa_kortnamn_lan())
  
  retur_df <- retur_df %>% 
    left_join(regionnyckel, by = "region")
  
  # Visa tabellen
  return(retur_df)
} # slut funktion


# ================================ gtfs calendar ====================================

gtfs_fyll_calendar_dagar <- function(calendar_dates_df){
  
  retur_df <- calendar_dates_df %>% 
    mutate(datum = as.Date(paste0(str_sub(date, 1,4), "-",
                                  str_sub(date, 5,6), "-",
                                  str_sub(date, 7,8))),
           veckodag = weekdays(datum),
           weekday = c("Sunday", "Monday", "Tuesday",     # Convert dates to weekdays
                       "Wednesday", "Thursday", "Friday",
                       "Saturday")[as.numeric(format(datum, "%w"))+1]) %>% 
    pivot_wider(names_from = weekday, values_from = exception_type) %>%
    mutate(across(c("Sunday", "Monday", "Tuesday",     # Convert dates to weekdays
                    "Wednesday", "Thursday", "Friday",
                    "Saturday"), as.numeric)) %>% 
    replace(is.na(.), 0) %>%
    #mutate(service_id = service_id %>% as.integer()) %>% 
    group_by(service_id) %>% 
    summarise(monday = max(Monday),
              tuesday = max(Tuesday),
              wednesday = max(Wednesday),
              thursday = max(Thursday),
              friday = max(Friday),
              saturday = max(Saturday),
              sunday = max(Sunday),
              start_date = min(date),
              end_date = max(date)) %>% 
    ungroup()
  return(retur_df)
  
}

gtfs_fyll_calendar_dates_fran_calendar <- function(calendar_df, 
                                                   skickade_service_id = NA, 
                                                   exception_kol = "1"){
  
  # om man skickar med service_id så kör vi bara på dem, annars hela datasetet
  if (!is.na(skickade_service_id)) { 
    filtrerad_df <- calendar_df %>% filter(service_id %in% skickade_service_id)
  } else {
    filtrerad_df <- calendar_df
  }
  
  # vi skapar ett longdataset utifrån calendar_df med alla möjliga datum
  retur_brutto_df <- filtrerad_df %>% 
    mutate(start_date = as.Date(paste0(str_sub(start_date, 1,4), "-",
                                       str_sub(start_date, 5,6), "-",
                                       str_sub(start_date, 7,8))),
           end_date = as.Date(paste0(str_sub(end_date, 1,4), "-",
                                     str_sub(end_date, 5,6), "-",
                                     str_sub(end_date, 7,8)))) %>%
    group_by(service_id) %>%
    summarise(date = list(seq.Date(from = min(start_date), to = max(end_date), by = "day"))) %>%
    unnest(cols = c(date)) %>%
    mutate(veckodag = c("Sunday", "Monday", "Tuesday",     # Convert dates to weekdays
                        "Wednesday", "Thursday", "Friday",
                        "Saturday")[as.numeric(format(date, "%w"))+1] %>% tolower())
  
  
  # vi skapar ett dataset där varje service_id har en vektor med de veckodagar 
  # som ska vara kvar
  service_veckodagar_df <- filtrerad_df %>%
    pivot_longer(cols = monday:sunday, names_to = "weekday", values_to = "value") %>%
    filter(value == 1) %>%
    group_by(service_id) %>%
    summarise(weekday_vector = list(weekday), .groups = "drop")
  
  # vi lägger ihop dessa två dataset och filtrerar på de veckodagar som finns i 
  # den vektor vi skapade i service_veckodagar_df ovan
  retur_df <- retur_brutto_df %>%
    inner_join(service_veckodagar_df, by = "service_id") %>%
    rowwise() %>%
    filter(veckodag %in% weekday_vector) %>%
    ungroup() %>%
    mutate(exception_type = exception_kol,
           date = date %>% as.character() %>% str_remove_all("-")) %>%
    select(service_id, date, exception_type)
  
  return(retur_df)
  
}
