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
#      starta_otp_server() - startar en otp-server i en cmd-process, denna cmd-process ligger kvar till vi kör stang_otp_server()
#     
#      stang_otp_server() - stänger en otp-server som är igång, om någon är igång
# 
#      bygg_ny_otp_graf()       
#
#
# Peter Möller, Region Dalarna, juni 2024
#
# ========================================================================================================
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse, 
       httr)

# kolla process-id (pid) för processer som är igång med programmet som skickas med parametern
# program_fil (default = "java.exe")
hamta_pid_for_processer <- function(program_fil = "java.exe") {    # ska vara en exefil
  
  pid <- system(paste0('tasklist /FI "IMAGENAME eq ', program_fil,  '" /FO CSV'), intern = TRUE)
  
  if (all(pid != "INFO: No tasks are running which match the specified criteria.")) {
    # Extrahera PIDs från output
    pid_list <- read_csv(paste(pid, collapse = "\n"), show_col_types = FALSE)
    
    # Filter out rows that don't have a valid PID (e.g., header rows)
    pid_list <- pid_list[grep("java.exe", pid_list$`Image Name`), ]
    
  } else pid_list <- NULL
  
  return(pid_list)  
}

# Funktion för att hitta och döda en process (default = "java.exe")
stang_otp_server <- function(program_fil = "java.exe", 
                             undanta_pid = NA,                 # vektor med pid-id för processer som INTE ska stängas
                             stang_pid = NA                    # vektor med pid_id som SKA stängas, trumfar undantag ovan
                             ) {

  if (!all(is.na(stang_pid))) {
   stang_lista <- stang_pid 
    
  } else {
    pid_list <- hamta_pid_for_processer()
    
    # om man vill låta några processer vara igång
    if (!all(is.na(undanta_pid))) {
      pid_list <- pid_list[!pid_list$PID %in% undanta_pid,]
    }
    
    stang_lista <- pid_list$PID
  }
  
  # Avsluta alla Java-processer (du kan justera detta om du har flera Java-processer och vill identifiera rätt process)
  if (length(stang_lista) > 0) {
    for (pid in stang_lista) {
      system(paste('taskkill /PID', pid, '/F'))
    }
    message("Servern har stängts.")
  } else message("Inga processer har stängts.")
  
}

# Funktion för att testa om en otp-server är igång
testa_otp_server <- function(otp_url = 'http://localhost:8801',
                             bara_text_konsol = FALSE){
  
  # Skicka en GET-förfrågan till OTP-servern
  response <- GET(otp_url)
  
  # Kontrollera statuskoden
  if (status_code(response) == 200) {
    if (bara_text_konsol){
      cat("OTP-servern är igång och tillgänglig.\n")
    } else return(TRUE)
  } else {
    if (bara_text_konsol){
      cat("OTP-servern är inte tillgänglig. Statuskod:", status_code(response), "\n")
    } else return(FALSE)
  }
}

# Funktion för att starta en otp-server (kräver att den är uppsatt på korrekt sätt innan)
starta_otp_server <- function() {
  
  old_http_proxy <- Sys.getenv("http_proxy")
  old_https_proxy <- Sys.getenv("https_proxy")
  # vi behöver stänga av proxy för att otp-lösningen ska fungera
  Sys.setenv(http_proxy = "")
  Sys.setenv(https_proxy = "")

  # för att kolla vilka pid-id:n som eventuellt redan körs för java.exe, så att vi kan stänga av enbart de processer vi startar nedan
  fore_processer <- hamta_pid_for_processer() 
  if(!is.null(fore_processer)) fore_processer <- fore_processer %>% dplyr::pull(PID)
  
  # Starta servern i ett nytt fönster på Windows
  #shell('start cmd /c "cd /d C:/otp/ && java -Xmx6G -jar otp-2.3.0-shaded.jar --load --serve --port 8801 --securePort 8802 ./data_otp"')
  shell('start cmd /c "cd /d C:/otp/ && java -Xmx6G -XX:+UseParallelGC -jar otp-2.3.0-shaded.jar --load --serve --port 8801 --securePort 8802 ./data_otp"')
  
  Sys.sleep(2)            # för att processerna ska hinna igång och kunna fångas upp av nästa rad i skriptet
  efter_processer <- hamta_pid_for_processer() %>% dplyr::pull(PID)
  
  if(length(fore_processer) > 0) efter_processer <- efter_processer[!efter_processer %in% fore_processer]
  if(length(efter_processer) == 0) efter_processer <- NULL
  #Sys.setenv(http_proxy = old_http_proxy)
  #Sys.setenv(https_proxy = old_https_proxy)
  
  return(efter_processer)
}
  
skapa_isokroner_otp <- function(
    datum = "2024-06-10",         # datum för tillgänglighetskörningen
    tid = "08:00",                # tid för tillgänglighetskörningen
    iso_intervaller = c(60, 90),  # hur många isokron-polygoner vill vi ha och med vilka tidsintervall (anges i minuter)
    transport_mode = 'TRANSIT',   # transportsätt, dessa finns: WALK, TRANSIT, BICYCLE, CAR, BICYCLE_RENT, CAR_PARK, CAR_RENT
    malpunkter_sf,                # sf-punktobjekt som man vill göra isokroner för
    geo_kolumn = "geometry",      # namn på geometri-kolumn
    namn_kolumn = NULL            # namn på målpunktens namn, om NA så saknas det namn
    
  ){

  # ====================== otp - skapa isokroner =====================
  #
  # Skript skapat av Swecos skript med syfte att skapa isokroner från ett valfritt antal punkter.
  # För att köra skriptet behöver man starta en otp-server, som också behöver sättas upp innan den kan sättas igång, 
  # instruktioner för hur detta görs finns på G:\Samhällsanalys\GIS\projekt\SWECO\OTP 
  # 
  # OTP-servern körs på en serveryta som nås via fjärrskrivbord. Därefter kan skripten som ligger i samma
  # mapp som instruktionerna ovan köras. 
  #
  # Peter Möller, Region Dalarna, juni 2024
  #
  # ========================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse, 
         httr,
         terra,
         curl, 
         sf,
         rjson,
         hms,
         lubridate,
         tictoc)
  
  #source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_otp.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R")
  
  otp_isochrone <- 'http://localhost:8801/otp/traveltime/isochrone'
  
  starta_otp_server()

  # skapa funktioner =======================================
  
  result <- list()
  
  create_isochrone_query <- function(fromPlace, request_datetime){
    modes <- transport_mode
    time_local <- strftime(request_datetime, format="%Y-%m-%dT%H:%M:%SZ", tz=Sys.timezone())
    query_iso <- str_glue('{otp_isochrone}?batch=true&location={fromPlace}&modes={modes}&time={time_local}&arriveBy=false')
    # 3.5h max, med 15min intervaller
    for (time in iso_intervaller){
      query_iso <- str_glue('{query_iso}&cutoff={time}M')
    } 
    return (query_iso)
  }
  
  success <- function(response) {
    url <- response$url
    location_info <- grep('location', str_split(url, '&')[[1]], value=TRUE)
    malpunkt <- str_split(location_info, '=')[[1]][2]
    lat <- str_split(malpunkt, ',')[[1]][1]
    lon <- str_split(malpunkt, ',')[[1]][2]
    
    malpunkt_namn <- punkter_queries %>%
      filter(punkt_lat == !!lat & punkt_lon == !!lon) %>%
      pull(malpunkt)
    
    if(length(malpunkt_namn) == 0) {
      malpunkt_namn <- "Saknar namn"  # Om ingen matchning hittas
    }
    
    responseData <- list(malpunkt_namn = malpunkt_namn,
                         malpunkt = malpunkt, 
                         content = response$content)
    
    # # skriv till Rds fil:
    # rds_file <- tempfile(fileext = '.Rds')
    # saveRDS(responseData, rds_file)
    # rds_files <<- append(rds_files, rds_file)
    
    # eller lagra i result
    result <<- append(result, list(responseData))
  }
  
  failure <- function(str){
    cat(paste("Failed request:", str), file = stderr())
  }
  
  # skapa ett sf-objekt som heter malpunkter utifrån skickad sf
  malpunkter <- malpunkter_sf %>% 
    select(any_of(c("geometry" = geo_kolumn, "malpunkt_namn" = namn_kolumn)))
  
  if (is.null(namn_kolumn)) malpunkter <- malpunkter %>% mutate(malpunkt_namn = NA)
  
  punkter_queries <- pmap(malpunkter, ~ {
    # startpunkt data
    lonlat <- st_coordinates(..1)
    fromlon <- lonlat[1]
    fromlat <- lonlat[2]
    punkt_koord <- str_glue('{fromlat},{fromlon}')
    malpunkt <- ..2
    
    # skapa query
    # Använd as.POSIXct för att skapa datum-tid-sträng med rätt format och tidszon (den som den lokala datorn använder)
    iso_datetime <- as.POSIXct(paste(datum, tid), format="%Y-%m-%d %H:%M", tz=Sys.timezone())
    query_iso <- create_isochrone_query(punkt_koord, iso_datetime)             # skapa en query med från koordinater och en datum/tidsträng
    
    # lagra i dataframe
    data <- data.frame(malpunkt = malpunkt, punkt_koord = punkt_koord, punkt_lon = as.character(fromlon),
                       punkt_lat = as.character(fromlat), query=query_iso)
    #iso_queries <<- rbind(iso_queries, data)
    
  }) %>% list_rbind()
  
  
  # lägg queries till curl multi_run
  walk(punkter_queries$query, ~ multi_add(new_handle(url = .x), done=success, fail=failure))
  
  # kör requests
  tic()
  result <- list()
  multi_run()
  toc()
  
  
  # cont <- result[[1]]
  source("G:/skript/gis/otp/test/test_extrahera_isokron.R", encoding = "utf-8")
  list_sf <- map(result, ~ extrahera_iso_geo(.x)) %>% flatten
  
  return(list_sf)

}

bygg_ny_otp_graf <- function(otp_jar_mapp = "c:/otp/",
                             otp_data_mapp = "c:/otp/data_otp/") {
  
  # om man till exempel vill uppdatera sin graf med ny gtfs-data så måste man 
  # också uppdatera grafen som används i beräkningarna, filen heter otp-2.3.0-shaded.jar och uppdateras 
  # när man bygger en ny graf
  
  graf_fil <- paste0(otp_data_mapp, "graph.obj")
  start_tid <- Sys.time()         # hämtar tid innan vi kör igång uppdateringen
  
  # sätt ihop kommandot rätt med korrekta mappar, som vi sedan ska köra i cmd
  build_cmd <- paste0("java -Xms2G -Xmx8G -XX:+UseParallelGC -jar ", otp_jar_mapp, "otp-2.3.0-shaded.jar --build --save ", otp_data_mapp)
  
  # kör detta i kommandotolken
  system(build_cmd)
  
  process_tid <- difftime(Sys.time(), start_time, units = "mins")
  
  # Skriv ut tiden i minuter
  cat("Processen tog", round(process_tid, 2), "minuter att köra.\n\n")
  # kontrollera att graf-filen har uppdaterats
  graf_fil_revtid <- file.info(graf_fil)$mtime
  
  if (graf_fil_revtid > start_tid) {
    cat("Grafen (", graf_fil, ") har uppdaterats framgångsrikt.\n")
  } else {
    cat("Grafen (", graf_fil, ") har inte kunnat uppdaterats på grund av att något problem har inträffat. Se felmeddelanden i konsolen.\n")
  }
} # slut funktion bygg_ny_otp_graf()
