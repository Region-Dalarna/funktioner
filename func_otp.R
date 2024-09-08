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
       httr)

# ================ skript för att hantera en lokal otp-server  ============================


otp_hamta_pid_for_processer <- function(program_fil = "java.exe") {    # ska vara en exefil
  # kolla process-id (pid) för processer som är igång med programmet som skickas med parametern
  # program_fil (default = "java.exe")
  
  pid <- system(paste0('tasklist /FI "IMAGENAME eq ', program_fil,  '" /FO CSV'), intern = TRUE)
  
  if (all(pid != "INFO: No tasks are running which match the specified criteria.")) {
    # Extrahera PIDs från output
    pid_list <- read_csv(paste(pid, collapse = "\n"), show_col_types = FALSE)
    
    # Filter out rows that don't have a valid PID (e.g., header rows)
    pid_list <- pid_list[grep("java.exe", pid_list$`Image Name`), ]
    
  } else pid_list <- NULL
  
  return(pid_list)  
}

otp_stang_server <- function(program_fil = "java.exe", 
                             undanta_pid = NA,                 # vektor med pid-id för processer som INTE ska stängas
                             stang_pid = NA                    # vektor med pid_id som SKA stängas, trumfar undantag ovan
                             ) {
  # Funktion för att hitta och döda en process (default = "java.exe")
  
  if (!all(is.na(stang_pid))) {
   stang_lista <- stang_pid 
    
  } else {
    pid_list <- otp_hamta_pid_for_processer()
    
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

otp_testa_server <- function(otp_url = 'http://localhost:8801',
                             bara_text_konsol = FALSE){
  # Funktion för att testa om en otp-server är igång
  proxy_paslagen <- otp_testa_http_proxy()
  # stäng av proxy
  if (proxy_paslagen) otp_stang_av_http_proxy()
  
  # Skicka en GET-förfrågan till OTP-servern
  response <- GET(otp_url)
  
  # Kontrollera statuskoden
  if (status_code(response) == 200) {
    if (bara_text_konsol){
      cat("OTP-servern är igång och tillgänglig.\n")
      if (proxy_paslagen) otp_satt_pa_http_proxy()    # sätt på proxy om den var påslagen när funktionen anropades
    } else {
      if (proxy_paslagen) otp_satt_pa_http_proxy()    # sätt på proxy om den var påslagen när funktionen anropades
      return(TRUE)
    }
      
  } else {
    if (bara_text_konsol){
      cat("OTP-servern är inte tillgänglig. Statuskod:", status_code(response), "\n")
      if (proxy_paslagen) otp_satt_pa_http_proxy()    # sätt på proxy om den var påslagen när funktionen anropades
      return(TRUE)
    } else {
      if (proxy_paslagen) otp_satt_pa_http_proxy()    # sätt på proxy om den var påslagen när funktionen anropades
      return(FALSE)
    }
  }
}

otp_starta_server <- function(vanta_tills_klar = TRUE) {
  # Funktion för att starta en otp-server (kräver att den är uppsatt på korrekt sätt innan)
  # om vanta_tills_klar är TRUE så körs inte skriptet vidare förrän otp-servern
  #                        är helt klar att köra vilket kan ta upp till någon minut
  
  # vi behöver stänga av proxy för att otp-lösningen ska fungera
  otp_stang_av_http_proxy()

  # för att kolla vilka pid-id:n som eventuellt redan körs för java.exe, så att vi kan stänga av enbart de processer vi startar nedan
  fore_processer <- otp_hamta_pid_for_processer() 
  if(!is.null(fore_processer)) fore_processer <- fore_processer %>% dplyr::pull(PID)
  
  # Starta servern i ett nytt fönster på Windows
  #shell('start cmd /c "cd /d C:/otp/ && java -Xmx6G -jar otp-2.3.0-shaded.jar --load --serve --port 8801 --securePort 8802 ./data_otp"')
  shell('start cmd /c "cd /d C:/otp/ && java -Xmx6G -XX:+UseParallelGC -jar otp-2.3.0-shaded.jar --load --serve --port 8801 --securePort 8802 ./data_otp"')
  
  Sys.sleep(2)            # för att processerna ska hinna igång och kunna fångas upp av nästa rad i skriptet
  efter_processer <- otp_hamta_pid_for_processer() %>% dplyr::pull(PID)
  
  if(length(fore_processer) > 0) efter_processer <- efter_processer[!efter_processer %in% fore_processer]
  if(length(efter_processer) == 0) efter_processer <- NULL
  
  # vänta max 1 minut (60 sekunder) och kolla varannan sekund om servern är up and running
  if (vanta_tills_klar) otp_vanta_tills_otp_server_ar_klar(timeout = 60,
                                                           interval = 2)
  
  # här sätter vi på proxy igen så att source till github etc. fungerar igen
  otp_satt_pa_http_proxy()
  return(efter_processer)
}


otp_vanta_tills_otp_server_ar_klar <- function(timeout = 60, # I sekunder, 60 = timeout på 1 minut
                                           interval = 2  # Kontrollintervall på 2 sekunder 
){
  # Funktion för att vänta tills otp-servern är uppe och klar för körningar innan
  # koden fortsätter, så att det inte blir fel för att otp-servern inte är riktigt klar ännu.
  # standard är att den kollar av varannan sekund (interval = 2) och att den väntar i 1 minut
  # (timeout = 60), sekunder. Är den inte klar då så stoppas koden med ett felmeddelande.
  
  start_time <- Sys.time()
  
  repeat {
    result <- tryCatch({
      otp_testa_server()
    }, error = function(e) {
      #message("Error: ", e$message)
      FALSE
    })
    
    if (result) {
      cat("OTP-servern är klar att användas.\n\n")
      return(invisible())
    }
    
    if (as.numeric(Sys.time() - start_time, units = "secs") > timeout) {
      stop("Timeout: misslyckades med att starta otp-server inom 1 minut.\n\n")
    }
    
    Sys.sleep(interval)
  }
}

otp_bygg_ny_graf <- function(otp_jar_mapp = "c:/otp/",
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
  
  process_tid <- difftime(Sys.time(), start_tid, units = "mins")
  
  # Skriv ut tiden i minuter
  cat("Processen tog", round(process_tid, 2), "minuter att köra.\n\n")
  # kontrollera att graf-filen har uppdaterats
  graf_fil_revtid <- file.info(graf_fil)$mtime
  
  if (graf_fil_revtid > start_tid) {
    cat("Grafen (", graf_fil, ") har uppdaterats framgångsrikt.\n")
  } else {
    cat("Grafen (", graf_fil, ") har inte kunnat uppdaterats på grund av att något problem har inträffat. Se felmeddelanden i konsolen.\n")
  }
} # slut funktion otp_bygg_ny_graf()

otp_satt_pa_http_proxy <- function(){
  # för att kunna köra otp som lokal server behöver man stänga av proxy
  # denna funktion sätter på den igen så att man kan source:a skript från github etc. igen
  Sys.setenv(http_proxy = "http://mwg.ltdalarna.se:9090")
  Sys.setenv(https_proxy = "http://mwg.ltdalarna.se:9090")
}
  
otp_stang_av_http_proxy <- function() {
  # för att kunna köra otp som lokal server behöver man stänga av proxy
  Sys.setenv(http_proxy = "")
  Sys.setenv(https_proxy = "")
}

otp_testa_http_proxy <- function() {
  # testa om proxy är på (TRUE) eller av (FALSE)
  if (Sys.getenv("http_proxy") == "http://mwg.ltdalarna.se:9090") return(TRUE) else return(FALSE)
}
# ================ Funktion för att skapa isokroner  ============================

skapa_isokroner_otp <- function(
    datum = "2024-06-10",         # datum för tillgänglighetskörningen
    tid = "08:00",                # tid för tillgänglighetskörningen
    iso_intervaller = c(60, 90),  # hur många isokron-polygoner vill vi ha och med vilka tidsintervall (anges i minuter)
    transport_mode = 'TRANSIT',   # transportsätt, dessa finns: WALK, TRANSIT, BICYCLE, CAR, BICYCLE_RENT, CAR_PARK, CAR_RENT
    malpunkter_sf,                # sf-punktobjekt som man vill göra isokroner för
    geo_kolumn = "geometry",      # namn på geometri-kolumn
    namn_kolumn = NULL            # namn på målpunktens namn, om NA så saknas det namn
    
  ){

  #  otp - skapa isokroner 
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
  
  otp_stang_av_http_proxy()
  # start server om den inte redan är igång
  if (!otp_testa_server()) otp_starta_server()

  # skapa funktioner 
  
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
      dplyr::pull(malpunkt)
    
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
    select(any_of(c("malpunkt_namn" = namn_kolumn, "geometry" = geo_kolumn)))
  
  if (is.null(namn_kolumn)) malpunkter <- malpunkter %>% mutate(malpunkt_namn = NA)
  
  punkter_queries <- pmap(malpunkter, ~ {
    # startpunkt data
    lonlat <- st_coordinates(..2)
    fromlon <- lonlat[1]
    fromlat <- lonlat[2]
    punkt_koord <- str_glue('{fromlat},{fromlon}')
    malpunkt <- ..1
    
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
  #source("G:/skript/gis/otp/test/test_extrahera_isokron.R", encoding = "utf-8")
  list_sf <- map(result, ~ otp_extrahera_iso_geo(.x)) %>% flatten
  
  otp_satt_pa_http_proxy()
  return(list_sf)

}


otp_extrahera_iso_geo <- function(cont) {
  # Konvertera raw data till JSON
  json_data <- fromJSON(rawToChar(cont$content))
  
  # Extrahera geometridata från json_data
  features <- json_data$features
  
  # Funktion för att hantera varje feature
  multipolygons <- map(features, function(feature) {
    coords <- feature$geometry$coordinates
    if (feature$geometry$type == "MultiPolygon") {
      polygons <- map(coords, function(polygon) {
        map(polygon, function(ring) {
          matrix(unlist(ring), ncol = 2, byrow = TRUE)
        })
      })
      st_multipolygon(polygons)
    } else if (feature$geometry$type == "Polygon") {
      polygons <- map(coords, function(ring) {
        matrix(unlist(ring), ncol = 2, byrow = TRUE)
      })
      st_polygon(polygons)
    } else {
      NULL
    }
  })
  
  # Filtrera bort eventuella NULL-värden
  multipolygons <- compact(multipolygons)
  
  # hämta tidvärden för isokronerna, så att vi kan lägga in dem i det färdiga sf-objektet
  time_values <- map_chr(features, ~ .x$properties$time)
  
  
  # Skapa ett sf-objekt
  retur_sf <- st_sf(
    geometry = st_sfc(multipolygons),
    crs = 4326  # Se till att använda rätt CRS om du vet vilken som är korrekt
  ) %>% 
    mutate(tid = time_values %>% as.numeric,
           malpunkt_namn = cont$malpunkt_namn,
           malpunkt_bara = cont$malpunkt_bara)
  
  retur_sf <- list(retur_sf)
  names(retur_sf) <- cont$malpunkt_namn
  
  return(retur_sf)
}

# ================================ gtfs-skript ====================================

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
