

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, 
               dplyr, 
               data.table, 
               purrr, 
               zip,
               tidyverse,
               mapview,
               sf,
               rvest,
               RPostgres,
               glue,
               httr,
               keyring) # finns zip i tidyverse?

gtfs_operatorer_sverige_nyckeltabell_hamta <- function(tabort_na = TRUE){
  
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
  
  if (tabort_na) retur_df <- retur_df %>% filter(!is.na(regionkod))
  
  # Visa tabellen
  return(retur_df)
} # slut funktion


# ================================ gtfs calendar ====================================

gtfs_fyll_calendar_dagar <- function(calendar_dates_df){
  
  retur_df <- calendar_dates_df %>% 
    mutate(datum = if (!inherits(date, "Date")) {
      as.Date(paste0(str_sub(date, 1, 4), "-",
                     str_sub(date, 5, 6), "-",
                     str_sub(date, 7, 8)))
    } else {
      date
    },
    veckodag = weekdays(datum),
    weekday = c("Sunday", "Monday", "Tuesday",     # Convert dates to weekdays
                "Wednesday", "Thursday", "Friday",
                "Saturday")[as.numeric(format(datum, "%w")) + 1]) %>% 
    pivot_wider(names_from = weekday, values_from = exception_type) %>%
    mutate(across(c("Sunday", "Monday", "Tuesday",     # Convert dates to weekdays
                    "Wednesday", "Thursday", "Friday",
                    "Saturday"), as.numeric)) %>% 
    replace(is.na(.), 0) %>%
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

# =================== Hämtar GTFS-dataset ===================

# Huvudfunktion för nedladdning och bearbetning av GTFS-data
#
# För att köra skriptet krävs att man har installerat paketet keyring() och där sparat en 
# service som heter "API_trafiklab_token", där användarnamn är "GTFS_Regional", "GTFS_Sverige_2" 
# eller "GTFS_Sweden_3" beroende på vilket dataset man vill hämta. Själva token läggs som 
# lösenord. Man måste skapa en användare hos Trafiklab för att få tokens som man använder för 
# att ladda ned data. Detta skapar man här: https://developer.trafiklab.se/login och det är 
# kostnadsfritt. 
#
# Parametrar i funktionen:
#   - gtfs_dataset: Specificerar dataset att ladda ner. Kan vara en RKM-kod (ex. "skane") eller "sverige_2" eller "sweden_3".
#   - spara_filmap: Sökväg för att spara filerna. Om ingen sökväg anges, sparas filerna temporärt.
#   - test_mode: Om TRUE, testas endast URL-åtkomst, ingen nedladdning sker (default = FALSE).
#
# Beskrivning:
# Funktionen hämtar GTFS-data baserat på angivet dataset och sparar filerna i en angiven katalog (om specificerad).
# Om test_mode är satt till TRUE, utförs en kontroll av URL:ens tillgänglighet utan att data hämtas.
# Filen packas upp och innehållet bearbetas för att generera en lista av dataframes med GTFS-information.
#
# Retur:
# Returnerar en lista med dataframes (routes, stops, stop_times, trips, calendar_dates, shapes, agency, feed_info).
# shapes är NULL om shapes.txt saknas i datasetet.
#
# Felhantering:
# Hanterar fel under nedladdning och bearbetning, och ger ett felmeddelande vid problem.
# 
# Förbättringspotential:
#   - använda länskod för att hämta regionala dataset, funkar backar därför till en tidigare version!
#   - funktionalitet för att ladda ner länsversioner av sweden_3 eller sverige_2? Så att man får med all trafik i länet och inte bara den regionala operatören

hamta_gtfs_data <- function(gtfs_dataset = "20",   # 
                            spara_filmap = NA, 
                            test_mode = FALSE) {
  
  # kontrollera att gtfs_dataset bara är ett värde
  if (length(gtfs_dataset) > 1) stop("Parametern gtfs_dataset kan för närvarande bara ha ett värde. Korrigera parametern och försök igen.")
  
  tryCatch({
    datum <- str_remove_all(Sys.Date(), "-")
    
    # Retrieve API key from keyring based on dataset
    if (tolower(gtfs_dataset) == "sweden_3" | tolower(gtfs_dataset) == "sweden3") {
      api_key <- key_get("API_trafiklab_token", "GTFS_Sweden_3")
      url <- paste0("https://opendata.samtrafiken.se/gtfs-sweden/sweden.zip?key=", api_key)
    } else if (tolower(gtfs_dataset) == "sverige_2" | tolower(gtfs_dataset) == "sverige2") {
      api_key <- key_get("API_trafiklab_token", "GTFS_Sverige_2")
      url <- paste0("https://api.resrobot.se/gtfs/sweden.zip?key=", api_key)
    } else {
      # hämta en nyckel som översätter länskoder till operatörskoder (som finns hos Trafiklab)
      regionnyckel <- gtfs_operatorer_sverige_nyckeltabell_hamta()
      # om gtfs-dataset
      rkm <- if (gtfs_dataset %in% regionnyckel$operatorkod) gtfs_dataset else regionnyckel$operatorkod[gtfs_dataset == regionnyckel$regionkod]
      
      api_key <- key_get("API_trafiklab_token", "GTFS_Regional")
      url <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", api_key)
    }
    
    # Test mode: only check if the URL is accessible
    if (test_mode) {
      if (gtfs_dataset == "sverige_2") {
        # Use GET request for sverige_2 as it does not support HEAD requests
        response <- GET(url)
      } else {
        # Use HEAD request for others
        response <- HEAD(url)
      }
      
      if (response$status_code == 200) {
        message("Test framgångsrikt: URL:en finns och fungerar.")
      } else {
        stop("URL:en fungerar inte, felkod: ", response$status_code)
      }
      return(invisible())  # Exits function without further execution
    }
    
    # Filnamn och sökväg för nedladdning om en sökväg anges
    if (!is.na(spara_filmap)) {
      if (!dir.exists(spara_filmap)) {
        dir.create(spara_filmap, recursive = TRUE)
      }
      gtfs_fil <- paste0(spara_filmap, "/trafiklab_", gtfs_dataset, "_", datum, ".zip")
    } else {
      gtfs_fil <- tempfile(fileext = ".zip")
    }
    
    GET(url, write_disk(gtfs_fil, overwrite = TRUE))
    unzip_dir <- if (!is.na(spara_filmap)) paste0(spara_filmap, "/", gtfs_dataset, "_", datum) else tempfile()
    unzip(gtfs_fil, exdir = unzip_dir)
    
    # Lista alla filer i zip-arkivet
    zip_innehall <- zip_list(gtfs_fil)$filename
    
    # skapa tom lista som vi fyller på med information nedan
    gtfs_lista <- list()
    
    # Läs in och bearbeta gtfs-filerna
    routes <- read.csv2(file.path(unzip_dir, "routes.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(route_type = as.integer(route_type))
    gtfs_lista <- c(gtfs_lista, list(routes = routes))     # fyll på gtfs_lista med detta dataset
    
    stops <- read.csv2(file.path(unzip_dir, "stops.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        hpl_id = substr(stop_id, 8, 13),
        stop_lat = as.numeric(stop_lat),
        stop_lon = as.numeric(stop_lon),
        location_type = as.integer(location_type)
      )
    gtfs_lista <- c(gtfs_lista, list(stops = stops))      # fyll på gtfs_lista med detta dataset
    
     <- read.csv2(file.path(unzip_dir, "stop_times.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        stop_sequence = as.integer(stop_sequence),
        pickup_type = as.integer(pickup_type),
        drop_off_type = as.integer(drop_off_type),
        shape_dist_traveled = if ("shape_dist_traveled" %in% names(.)) suppressWarnings(as.numeric(shape_dist_traveled)) else NA_real_,
        timepoint = if ("timepoint" %in% names(.)) suppressWarnings(as.integer(timepoint)) else NA_integer_
      )
    gtfs_lista <- c(gtfs_lista, list(stop_times = stop_times))     # fyll på gtfs_lista med detta dataset
    
    
    trips <- read.csv2(file.path(unzip_dir, "trips.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        direction_id = if ("direction_id" %in% names(.)) suppressWarnings(as.integer(direction_id)) else NA_integer_
      )
    gtfs_lista <- c(gtfs_lista, list(trips = trips))     # fyll på gtfs_lista med detta dataset
    
    
    calendar_dates <- read.csv2(file.path(unzip_dir, "calendar_dates.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
      mutate(
        date = as.Date(date, format = "%Y%m%d"),
        exception_type = as.integer(exception_type)
      )
    gtfs_lista <- c(gtfs_lista, list(calendar_dates = calendar_dates))     # fyll på gtfs_lista med detta dataset
    
    
    # Lägg bara till om shapes finns i datasetet
    if (file.exists(file.path(unzip_dir, "shapes.txt"))) {
      shapes <- read.csv2(file.path(unzip_dir, "shapes.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character') %>%
        mutate(
          shape_pt_sequence = if ("shape_pt_sequence" %in% names(.)) as.integer(shape_pt_sequence) else NA_integer_,
          shape_dist_traveled = if ("shape_dist_traveled" %in% names(.)) suppressWarnings(as.numeric(shape_dist_traveled)) else NA_real_
        )
      gtfs_lista <- c(gtfs_lista, list(shapes = shapes))     # fyll på gtfs_lista med detta dataset
      
    } 
    
    if (file.exists(file.path(unzip_dir, "agency.txt"))) {
      agency <- read.csv2(file.path(unzip_dir, "agency.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character')
      gtfs_lista <- c(gtfs_lista, list(agency = agency))     # fyll på gtfs_lista med detta dataset
      
    }
    
    if (file.exists(file.path(unzip_dir, "feed_info.txt"))) {
      feed_info <- read.csv2(file.path(unzip_dir, "feed_info.txt"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = 'character')
      gtfs_lista <- c(gtfs_lista, list(feed_info = feed_info))     # fyll på gtfs_lista med detta dataset
    }
    
    # Return as a list of data frames
    return(gtfs_lista)
    
  }, error = function(e) {
    stop(paste("Följande fel har uppstått under nedladdning och bearbetning av gtfs-datasetet: ", e$message))
  })
}    

# Example usage
# sweden_3 <- hamta_gtfs_data("sweden_3", test_mode = TRUE)
# 
# sverige_2 <- hamta_gtfs_data("sverige_2", test_mode = TRUE)
# 
# dt <- hamta_gtfs_data("dt", test_mode = TRUE)   
#
# skane <- hamta_gtfs_data("skane")

# ========== Plotta GTFS-data ==========

# Hjälpfunktion för att visualisera "shapes" från ett GTFS-dataset som linjer på en karta
# Argument:
#   - gtfs_data: Ett objekt returnerat av funktionen för att hämta GTFS-data. Innehåller GTFS-data som bearbetats till dataframes.
#
# Beskrivning:
# Funktionen skapar en karta med linjer baserade på shapes-data från ett GTFS-dataset. Shapes-data 
# konverteras till ett sf-objekt (spatial format) och kombineras per shape_id till linjesegment.
# Om GTFS-datasetet saknar shapes-information (exempelvis "gtfs_sverige_2"), avbryts funktionen med ett felmeddelande.
#
# Retur:
# En interaktiv karta (via mapview) som visar linjer för varje shape_id i shapes-data.
# Färgerna för linjerna representeras unikt per shape_id och en legend inkluderas.
#
# Felhantering:
# Kontrollerar om shapes-data finns. Om inte, stoppas funktionen med ett meddelande till användaren.


mapview_gtfs <- function(gtfs_data) {
  
  # Check if shapes data exists
  if (is.null(gtfs_data$shapes)) {
    stop("The GTFS data does not contain shapes information. Unable to create map.") # gtfs_sverige_2 does not have shapes.txt
  }
  shapes <- gtfs_data$shapes
  
  shapes <- shapes %>%
    mutate(
      shape_pt_lat = as.numeric(shape_pt_lat),
      shape_pt_lon = as.numeric(shape_pt_lon)
    )
  
  shapes_sf <- st_as_sf(shapes, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
  
  shapes_lines <- shapes_sf %>%
    group_by(shape_id) %>%
    summarize(
      geometry = st_cast(st_combine(geometry), "LINESTRING"),
      .groups = 'drop'
    )
  
  mapview(shapes_lines, zcol = "shape_id", legend = TRUE)
}

# Example usage

# mapview_gtfs(skane)
