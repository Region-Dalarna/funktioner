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

#========== paket =========
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               sf,
               httr,
               keyring,
               RPostgres,
               glue,
               dplyr)

#========== Skapa tabellstruktur ===========
skapa_tabeller <- function(con){
  tryCatch({
    
    # Skapa schema om det inte finns
    dbExecute(con, "CREATE SCHEMA IF NOT EXISTS gtfs;")
    dbExecute(con, "CREATE SCHEMA IF NOT EXISTS gtfs_historisk;")
    
    # agency
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs.agency (
                      agency_id VARCHAR PRIMARY KEY,
                      agency_name VARCHAR NOT NULL,
                      agency_url VARCHAR NOT NULL,
                      agency_timezone VARCHAR NOT NULL,
                      agency_lang VARCHAR,
                      agency_phone VARCHAR,
                      agency_fare_url VARCHAR
                  );")
    
    # routes
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs.routes (
                      route_id VARCHAR PRIMARY KEY,
                      agency_id VARCHAR REFERENCES gtfs.agency(agency_id),
                      route_short_name VARCHAR NOT NULL,
                      route_long_name VARCHAR NOT NULL,
                      route_desc VARCHAR,
                      route_type INTEGER NOT NULL,
                      route_url VARCHAR,
                      route_color VARCHAR,
                      route_text_color VARCHAR
                  );")
    # routes - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_routes_route_short_name ON gtfs.routes (route_short_name);")
    
    # calendar_dates
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs.calendar_dates (
                      service_id VARCHAR,
                      date DATE,
                      exception_type INTEGER,
                      PRIMARY KEY (service_id, date)
                  );")
    
    # shapes_line
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs.shapes_line (
                      shape_id VARCHAR,
                      geometry GEOMETRY(Linestring, 3006),
                      antal_punkter INTEGER,
                      max_dist FLOAT,
                      PRIMARY KEY (shape_id)
                  );")
    # shapes_line - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry ON gtfs.shapes_line USING GIST (geometry);")
    
    # trips
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs.trips (
                      trip_id VARCHAR PRIMARY KEY,
                      route_id VARCHAR REFERENCES gtfs.routes(route_id),
                      service_id VARCHAR NOT NULL,
                      trip_headsign VARCHAR,
                      direction_id INTEGER,
                      shape_id VARCHAR
                  );")
    # trips - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trips_shape_id ON gtfs.trips (shape_id);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trips_route_id ON gtfs.trips (route_id);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trips_service_id ON gtfs.trips (service_id);")
    
    # stops
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs.stops (
                      stop_id VARCHAR PRIMARY KEY,
                      hpl_id VARCHAR,
                      stop_name VARCHAR NOT NULL,
                      stop_lat FLOAT NOT NULL,
                      stop_lon FLOAT NOT NULL,
                      location_type INTEGER,
                      parent_station VARCHAR,
                      platform_code VARCHAR,
                      geometry GEOMETRY(Point, 3006)
                  );")
    # stops - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stops_geometry ON gtfs.stops USING GIST (geometry);")
    
    # stop_times
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs.stop_times (
                      trip_id VARCHAR,
                      arrival_time VARCHAR, -- OBS! Kan bli problem med att använda TIME för tider större än 24:00, därför VARCHAR
                      departure_time VARCHAR, -- OBS! Kan bli problem med att använda TIME för tider större än 24:00, därför VARCHAR
                      stop_id VARCHAR,
                      stop_sequence INTEGER,
                      stop_headsign VARCHAR,
                      pickup_type INTEGER,
                      drop_off_type INTEGER,
                      shape_dist_traveled FLOAT,
                      timepoint INTEGER,
                      PRIMARY KEY (trip_id, stop_id, stop_sequence),
                      FOREIGN KEY (stop_id) REFERENCES gtfs.stops(stop_id),
                      FOREIGN KEY (trip_id) REFERENCES gtfs.trips(trip_id)
                  );")
    # stop_times - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id ON gtfs.stop_times (trip_id);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id ON gtfs.stop_times (stop_id);")
    
    #Linjeklassificering
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs.linjeklassificering (
                    route_short_name VARCHAR PRIMARY KEY,
                    klassificering VARCHAR NOT NULL
                  );")
    
    # Tabeller med historisk data
    # agency
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.agency (
                      agency_id VARCHAR,
                      agency_name VARCHAR NOT NULL,
                      agency_url VARCHAR NOT NULL,
                      agency_timezone VARCHAR NOT NULL,
                      agency_lang VARCHAR,
                      agency_phone VARCHAR,
                      agency_fare_url VARCHAR,
                      version INTEGER,
                      PRIMARY KEY (agency_id, version)
                  );")
    
    # routes
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.routes (
                      route_id VARCHAR,
                      agency_id VARCHAR,
                      route_short_name VARCHAR NOT NULL,
                      route_long_name VARCHAR NOT NULL,
                      route_desc VARCHAR,
                      route_type INTEGER NOT NULL,
                      route_url VARCHAR,
                      route_color VARCHAR,
                      route_text_color VARCHAR,
                      version INTEGER,
                      PRIMARY KEY (route_id, version),
                      FOREIGN KEY (agency_id, version) REFERENCES gtfs_historisk.agency(agency_id, version)
                  );")
    
    # calendar_dates
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.calendar_dates (
                      service_id VARCHAR,
                      date DATE,
                      exception_type INTEGER,
                      version INTEGER,
                      PRIMARY KEY (service_id, version, date)
                  );")
    
    # shapes_line
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.shapes_line (
                      shape_id VARCHAR,
                      geometry GEOMETRY(Linestring, 3006),
                      antal_punkter INTEGER,
                      max_dist FLOAT,
                      version INTEGER,
                      PRIMARY KEY (shape_id, version)
                  );")
    # shapes_line - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry_historisk ON gtfs_historisk.shapes_line USING GIST (geometry);")
    
    # trips
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.trips (
                      trip_id VARCHAR,
                      route_id VARCHAR,
                      service_id VARCHAR NOT NULL,
                      trip_headsign VARCHAR,
                      direction_id INTEGER,
                      shape_id VARCHAR,
                      version INTEGER,
                      PRIMARY KEY (trip_id, version),
                      FOREIGN KEY (route_id, version) REFERENCES gtfs_historisk.routes(route_id, version)
                  );")
    # trips - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trips_route_id_historisk ON gtfs_historisk.trips (route_id, version);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trips_shape_id_historisk ON gtfs_historisk.trips (shape_id);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trips_service_id_historisk ON gtfs_historisk.trips (service_id);")
    
    # stops
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.stops (
                      stop_id VARCHAR,
                      hpl_id VARCHAR,
                      stop_name VARCHAR NOT NULL,
                      stop_lat FLOAT NOT NULL,
                      stop_lon FLOAT NOT NULL,
                      location_type INTEGER,
                      parent_station VARCHAR,
                      platform_code VARCHAR,
                      geometry GEOMETRY(Point, 3006),
                      version INTEGER,
                      PRIMARY KEY (stop_id, version)
                  );")
    # stops - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stops_geometry_historisk ON gtfs_historisk.stops USING GIST (geometry);")
    
    # stop_times
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.stop_times (
                      trip_id VARCHAR,
                      arrival_time VARCHAR, -- OBS! Kan bli problem med att använda TIME för tider större än 24:00, därför VARCHAR
                      departure_time VARCHAR, -- OBS! Kan bli problem med att använda TIME för tider större än 24:00, därför VARCHAR
                      stop_id VARCHAR,
                      stop_sequence INTEGER,
                      stop_headsign VARCHAR,
                      pickup_type INTEGER,
                      drop_off_type INTEGER,
                      shape_dist_traveled FLOAT,
                      timepoint INTEGER,
                      version INTEGER,
                      PRIMARY KEY (trip_id, version, stop_sequence),
                      FOREIGN KEY (trip_id, version) REFERENCES gtfs_historisk.trips(trip_id, version),
                      FOREIGN KEY (stop_id, version) REFERENCES gtfs_historisk.stops(stop_id, version)
                  );")
    # stop_times - index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id_historisk ON gtfs_historisk.stop_times (trip_id, version);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id_historisk ON gtfs_historisk.stop_times (stop_id, version);")
    
    #Linjeklassificering
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.linjeklassificering (
                    route_short_name VARCHAR,
                    klassificering VARCHAR NOT NULL,
                    version INTEGER,
                    PRIMARY KEY (route_short_name, version)
                  );")
    
    # versionshantering
    dbExecute(con, "CREATE TABLE IF NOT EXISTS gtfs_historisk.versions (
                      version INTEGER PRIMARY KEY,
                      start_date DATE,
                      end_date DATE
                  );")
  }, error = function(e){
    stop(paste("Ett fel inträffade vid skapandet av tabeller: ", e$message))
  })
}

#========== Ladda hem GTFS ===========
#Returnerar all data i en lista med dataframes
ladda_hem_gtfs <- function(){
  # Lägg allt inom tryCatch för att skicka vidare ett fel till huvudskriptet om något blir fel under körning
  tryCatch({
    
    #Sökvägen till mappen för nedladdning - ändra sen till "icke" getwd()
    data_input <- paste0(getwd(), "/data")
    
    ### url for GTFS
    
    # ange operatör
    rkm = "dt" # !!!!!! Specify RKM. Available values : sl, ul, sormland, otraf, krono, klt, gotland, blekinge, skane, halland, vt, varm, orebro, vl, dt, xt, dintur, sj
    
    # dagens datum
    datum <- str_remove_all(Sys.Date(), "-")
    
    # ELLER datum senaste nedladdning <- används för testning för att slippa ladda ner en ny fil
    #datum <- "20240605"
    
    # skapa hela sökvägen
    sokvag_datum <- paste0(data_input, "/trafiklab_", rkm, "_", datum)
    # print(sokvag_datum)
    #=========== Start kommenteringen som sedan skall tas bort, för att slippa ladda hem varje gång ==========
    #skapa sökvägen till den nedladddade GTFS-filen med rkm och datum
    gtfs_regional_fil <- paste0(sokvag_datum, ".zip")
    
    # skapa och hämta url:en till gtfs-feeden
    url_regional <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", key_get("API_trafiklab_token", "GTFS_Regional"))
    
    GET(url_regional, write_disk(gtfs_regional_fil, overwrite=TRUE))
    
    # Zippa upp csv-filerna och lägg i undermapp
    unzip(gtfs_regional_fil, exdir = sokvag_datum)
    
    
    #=========== Slut kommenteringen som sedan skall tas bort, för att slippa ladda hem varje gång ==========
    
    #Läs in filerna - glöm inte colClasses = 'character' för att undvika problem med IDn
    # Konvertera datatyper för de fält som INTE är VARCHAR i databasen med mutate 
    routes <- read.csv2(paste0(sokvag_datum, "/routes.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        route_type = as.integer(route_type)
      )
    # Stops, extrahera hpl_id från stop_id och lägg till som kolumn.
    stops <- read.csv2(paste0(sokvag_datum, "/stops.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        hpl_id = substr(stop_id, 8, 13),
        stop_lat = as.numeric(stop_lat),
        stop_lon = as.numeric(stop_lon),
        location_type = as.integer(location_type)
      )
    
    stop_times <- read.csv2(paste0(sokvag_datum, "/stop_times.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        stop_sequence = as.integer(stop_sequence),
        pickup_type = as.integer(pickup_type),
        drop_off_type = as.integer(drop_off_type),
        shape_dist_traveled = as.numeric(shape_dist_traveled),
        timepoint = as.integer(timepoint)
      )
    
    trips <- read.csv2(paste0(sokvag_datum, "/trips.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        direction_id = as.integer(direction_id)
      )
    
    calendar_dates <- read.csv2(paste0(sokvag_datum, "/calendar_dates.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        date = as.Date(date, format = "%Y%m%d"),
        exception_type = as.integer(exception_type)
      )
    
    shapes <- read.csv2(paste0(sokvag_datum, "/shapes.txt"), sep = ",", encoding="UTF-8", stringsAsFactors=FALSE, colClasses = 'character') %>%
      mutate(
        shape_pt_sequence = as.integer(shape_pt_sequence),
        shape_dist_traveled = as.numeric(shape_dist_traveled)
      )
    
    agency = read.csv2(paste0(sokvag_datum, "/agency.txt"),
                       sep = ",", encoding="UTF-8", stringsAsFactors=FALSE, colClasses = 'character')
    
    feed_info = read.csv2(paste0(sokvag_datum, "/feed_info.txt"),
                          sep = ",", encoding="UTF-8", stringsAsFactors=FALSE, colClasses = 'character')
    
    
    #Returnera en lista med alla dataframes
    return(list(routes = routes, stops = stops, stop_times = stop_times, trips = trips, calendar_dates = calendar_dates, shapes = shapes, agency = agency, feed_info = feed_info))
  }, error = function(e){
    stop(paste("Ett fel inträffade vid nedladdning och upppackning av GTFS-data: ", e$message))
  })
  
}



