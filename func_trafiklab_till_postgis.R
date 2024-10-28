if (!require("pacman")) install.packages("pacman")
p_load(sf,
       data.table,
       rio,
       glue,
       openxlsx,
       tidyverse, 
       mapview,
       RPostgres,
       keyring,
       httr)


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

versionshantering <- function (con, gtfs_data){
  tryCatch({
    # Hämta sista datumet i calendar_dates från databasen
    sista_datum_db <- dbGetQuery(con, "SELECT MAX(date) AS sista_datum FROM gtfs.calendar_dates;")
    sista_datum_db <- as.Date(sista_datum_db$sista_datum[1])
    
    # Temporärt för att testa nytt slutdatum
    # sista_datum_db <- as.Date('2024-06-05')
    
    # Hämta sista datumet från calendar_dates i datasetet
    sista_datum_gtfs_data <- max(gtfs_data$calendar_dates$date)
    
    # Om sista datumet i db och datasetet inte är samma betyder det att det är en ny version
    if(is.na(sista_datum_db) || sista_datum_db != sista_datum_gtfs_data){
      
      # Hämta versionsnummer för den senaste versionen i db
      senaste_version <- dbGetQuery(con, "SELECT MAX(version) AS senaste_version FROM gtfs_historisk.versions;")
      senaste_version <- senaste_version$senaste_version[1]
      
      # Om det inte finns någon tidigare version, sätt ny_version till 1
      if(is.na(senaste_version)){
        ny_version <- 1
      } else {
        ny_version <- senaste_version + 1
      }
      
      # Om det finns en tidigare version, uppdatera dess slutdatum och flytta över data från gtfs gtfs_historisk
      if(!is.na(senaste_version)){
        dbExecute(con, glue("UPDATE gtfs_historisk.versions SET end_date = {sista_datum_db} WHERE version = {senaste_version};"))
        
        # Flytta data från gtfs-schemat till gtfs_historisk tillsammans med versionsnumret
        dbExecute(con, glue("INSERT INTO gtfs_historisk.agency SELECT agency_id, agency_name, agency_url, agency_timezone, agency_lang, agency_phone, agency_fare_url, {senaste_version} FROM gtfs.agency;"))
        dbExecute(con, glue("INSERT INTO gtfs_historisk.routes SELECT route_id, agency_id, route_short_name, route_long_name, route_desc, route_type, route_url, route_color, route_text_color, {senaste_version} FROM gtfs.routes;"))
        dbExecute(con, glue("INSERT INTO gtfs_historisk.calendar_dates SELECT service_id, date, exception_type, {senaste_version} FROM gtfs.calendar_dates;"))
        dbExecute(con, glue("INSERT INTO gtfs_historisk.shapes_line SELECT shape_id, geometry, antal_punkter, max_dist, {senaste_version} FROM gtfs.shapes_line;"))
        dbExecute(con, glue("INSERT INTO gtfs_historisk.stops SELECT stop_id, hpl_id, stop_name, stop_lat, stop_lon, location_type, parent_station, platform_code, geometry, {senaste_version} FROM gtfs.stops;"))
        dbExecute(con, glue("INSERT INTO gtfs_historisk.trips SELECT trip_id, route_id, service_id, trip_headsign, direction_id, shape_id, {senaste_version} FROM gtfs.trips;"))
        dbExecute(con, glue("INSERT INTO gtfs_historisk.stop_times SELECT trip_id, arrival_time, departure_time, stop_id, stop_sequence, stop_headsign, pickup_type, drop_off_type, shape_dist_traveled, timepoint, {senaste_version} FROM gtfs.stop_times;"))
        dbExecute(con, glue("INSERT INTO gtfs_historisk.linjeklassificering SELECT route_short_name, klassificering, {senaste_version} FROM gtfs.linjeklassificering;"))
        
        # Skapa vyer för historisk data - anrop sker i versionshantering()
        skapa_vyer_historisk_hallplats(con)
        skapa_vyer_historisk_linjer(con)
      }
      
      # Kontrollera om sista_datum_db är NA och sätt start_datum_gtfs_data korrekt
      if(is.na(sista_datum_db)){
        start_datum_gtfs_data <- min(gtfs_data$calendar_dates$date)
      } else {
        start_datum_gtfs_data <- sista_datum_db + 1
      }
      dbExecute(con, glue("INSERT INTO gtfs_historisk.versions (version, start_date) VALUES ({ny_version}, '{start_datum_gtfs_data}');"))
    }
  }, error = function(e){
    stop(glue("Ett fel inträffade vid versionshanteringen: {e$message}"))
  })
}

radera_gamla_versioner <- function(con, antal_ar){
  tryCatch({
    # Hämta datumet för 3 år sedan
    tre_ar_sedan <- Sys.Date() - antal_ar * 365
    
    # Hämta de versioner som är äldre än 3 år
    gamla_versioner <- dbGetQuery(con, glue("SELECT version FROM gtfs_historisk.versions WHERE end_date < '{tre_ar_sedan}';"))
    
    # Loopa igenom de gamla versionerna och ta bort dem från historiska tabeller
    for(version in gamla_versioner$version){
      dbExecute(con, glue("DELETE FROM gtfs_historisk.stop_times WHERE version = {version};"))
      dbExecute(con, glue("DELETE FROM gtfs_historisk.trips WHERE version = {version};"))
      dbExecute(con, glue("DELETE FROM gtfs_historisk.stops WHERE version = {version};"))
      dbExecute(con, glue("DELETE FROM gtfs_historisk.shapes_line WHERE version = {version};"))
      dbExecute(con, glue("DELETE FROM gtfs_historisk.calendar_dates WHERE version = {version};"))
      dbExecute(con, glue("DELETE FROM gtfs_historisk.routes WHERE version = {version};"))
      dbExecute(con, glue("DELETE FROM gtfs_historisk.agency WHERE version = {version};"))
      dbExecute(con, glue("DELETE FROM gtfs_historisk.linjeklassificering WHERE version = {version};"))
      
      # Ta bort versionen från versionstabellen
      dbExecute(con, glue("DELETE FROM gtfs_historisk.versions WHERE version = {version};"))
    }
  }, error = function(e){
    stop(glue("Ett fel inträffade vid radering av gamla versioner: {e$message}"))
  })
}

ladda_upp_till_databas <- function(con, gtfs_data){
  # Felhantera uppladdningen
  tryCatch({
    
    # Truncate tabeller innan uppladdning
    dbExecute(con, "TRUNCATE TABLE gtfs.stops RESTART IDENTITY CASCADE;")
    dbExecute(con, "TRUNCATE TABLE gtfs.routes RESTART IDENTITY CASCADE;")
    dbExecute(con, "TRUNCATE TABLE gtfs.calendar_dates RESTART IDENTITY CASCADE;")
    dbExecute(con, "TRUNCATE TABLE gtfs.trips RESTART IDENTITY CASCADE;")
    dbExecute(con, "TRUNCATE TABLE gtfs.shapes_line RESTART IDENTITY CASCADE;")
    dbExecute(con, "TRUNCATE TABLE gtfs.stop_times RESTART IDENTITY CASCADE;")
    dbExecute(con, "TRUNCATE TABLE gtfs.agency RESTART IDENTITY CASCADE;")
    
    #Spatiala tabeller
    # Lägg till stops
    sf_stops <- st_as_sf(gtfs_data$stops, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>% 
      st_transform(3006) %>% 
      st_set_geometry("geometry")
    st_write(obj = sf_stops, dsn = con, Id(schema = "gtfs", table = "stops"), geomtype = "POINT", delete_layer = FALSE, append = TRUE)
    rm(sf_stops)
    
    # Lägg till shapes
    sf_shapes <- st_as_sf(gtfs_data$shapes, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326, remove = FALSE) %>% 
      st_transform(3006) %>% 
      st_set_geometry("geometry")
    
    # Lägg till shapes som linjer och lägg till antal_punkter i linjen samt största avståndet mellan två punkter
    # # Konvertera shape_dist_traveled till numerisk typ om det är en teckensträng
    # sf_shapes$shape_dist_traveled <- as.numeric(sf_shapes$shape_dist_traveled)
    # sf_shapes$shape_pt_sequence <- as.numeric(sf_shapes$shape_pt_sequence)
    
    # Gör LINESTRINGS av alla punkter och lägg till fält för antal_punkter och maxdistans mellan 2 punkter, kan användas för att hitta linjer med glapp
    sf_shapes_line <- sf_shapes %>% 
      group_by(shape_id) %>%
      summarize(
        geometry = st_combine(geometry) %>% st_cast("LINESTRING", safe = TRUE),
        antal_punkter = n(),  # Antalet punkter per shape_id
        max_dist = max(shape_dist_traveled - lag(shape_dist_traveled, default = first(shape_dist_traveled)))  # Maximal distans mellan på varandra följande punkter
      )
    st_write(obj = sf_shapes_line, dsn = con, Id(schema = "gtfs", table = "shapes_line"), geomtype = "LINESTRING", delete_layer = FALSE, append = TRUE)
    
    
    # Icke spatiala tabeller
    # # Lägg till agency
    dbWriteTable(con, Id(schema = "gtfs", table = "agency"), gtfs_data$agency, append = TRUE, row.names = FALSE)
    
    # Lägg till calendar_dates
    dbWriteTable(con, Id(schema = "gtfs", table = "calendar_dates"), gtfs_data$calendar_dates, append = TRUE, row.names = FALSE)
    
    # Lägg till routes
    dbWriteTable(con, Id(schema = "gtfs", table = "routes"), gtfs_data$routes, append = TRUE, row.names = FALSE)
    
    # Lägg till trips
    dbWriteTable(con, Id(schema = "gtfs", table = "trips"), gtfs_data$trips, append = TRUE, row.names = FALSE)
    
    # Lägg till stop_times
    dbWriteTable(con, Id(schema = "gtfs", table = "stop_times"), gtfs_data$stop_times, append = TRUE, row.names = FALSE)
    
    # Dessa vet jag inte om de är så nödvändiga, bortkommenterade så länge
    # # Lägg till calendar
    #dbWriteTable(con, Id(schema = "gtfs", table = "calendar"), gtfs_data$calendar, overwrite = TRUE)
    # 
    
    # 
    # # Lägg till feed_info
    #dbWriteTable(con, Id(schema = "gtfs", table = "feed_info"), gtfs_data$feed_info, overwrite = TRUE)
    
    # # Lägg till transfer
    
  }, error = function(e){
    stop(paste("Ett fel inträffade vid uppladdningen av data till databasen: ", e$message))
  })
  
}

skapa_tabell_linjeklassificering <- function(con) {
  
  # OBS!!! Om nya undantag läggs till eller satsen som # Infoga klassificeringar behöver ändras se till att lägga undantagen först.
  # Exempel 39 - 'Landsbygdstrafik' måste ligga före 1 - 99 'Stadstrafik'
  
  tryCatch({
    # Rensa tabellen innan vi fyller på den
    dbExecute(con, "TRUNCATE gtfs.linjeklassificering;")
    
    # Infoga klassificeringar
    dbExecute(con, "
    INSERT INTO gtfs.linjeklassificering (route_short_name, klassificering)
    SELECT DISTINCT ON (route_short_name) route_short_name,
           CASE
             WHEN route_short_name ~ '^[0-9]+$' THEN
               CASE
                 WHEN CAST(route_short_name AS INTEGER) = 39 THEN 'Landsbygdstrafik'
                 WHEN CAST(route_short_name AS INTEGER) IN (42, 46, 241, 360, 500) THEN 'Utomlänstrafik'
                 WHEN CAST(route_short_name AS INTEGER) BETWEEN 1 AND 99 THEN 'Stadstrafik'
                 WHEN CAST(route_short_name AS INTEGER) BETWEEN 151 AND 154 THEN 'Stadstrafik'
                 WHEN CAST(route_short_name AS INTEGER) BETWEEN 101 AND 199 THEN 'Stråktrafik'
                 WHEN CAST(route_short_name AS INTEGER) BETWEEN 100 AND 400 THEN 'Landsbygdstrafik'
                 WHEN CAST(route_short_name AS INTEGER) BETWEEN 430 AND 800 THEN 'Flextrafik'
                 WHEN CAST(route_short_name AS INTEGER) BETWEEN 900 AND 1000 THEN 'Stängd skoltrafik'
                 ELSE 'Okänd'
               END
             WHEN route_short_name = 'Tåg' THEN 'Tåg'
             ELSE 'Okänd'
           END AS klassificering
    FROM gtfs.routes
    ORDER BY route_short_name, route_id;
  ")
    
  }, error = function(e) {
    # Skriver ut felmeddelandet och returnerar NULL
    stop(paste("Ett fel inträffade vid skapandet av linjeklassificeringen:", e$message))
  })
}

skapa_vyer_hallplats <- function(con) {
  # Felhantering
  tryCatch({
    # Droppa de materialiserade vyerna om de finns
    dbExecute(con, "DROP MATERIALIZED VIEW IF EXISTS gtfs.vy_hallplats_avgangar CASCADE;")
    dbExecute(con, "DROP MATERIALIZED VIEW IF EXISTS gtfs.vy_hallplatslage_avgangar CASCADE;")
    
    # Skapa materialiserad vy för hållplatslägen med avgångar, rutter och klassificeringar
    sql_hallplatslage_avgangar = "
      CREATE MATERIALIZED VIEW gtfs.vy_hallplatslage_avgangar AS
      WITH senaste_version AS ( -- Hämtar startdatum för den senaste versionen så att enbart trafik from detta datum tas med
          SELECT start_date
          FROM gtfs_historisk.versions
          ORDER BY start_date DESC
          LIMIT 1
      ),
      dagliga_avgangar AS ( -- Beräkna antalet avgångar/dag och hållplatsläge
          SELECT
              st.stop_id,
              cd.date,
              COUNT(DISTINCT st.trip_id) AS antal_avgangar
          FROM 
              gtfs.stop_times st
          JOIN 
              gtfs.trips t ON st.trip_id = t.trip_id
          JOIN 
              gtfs.calendar_dates cd ON t.service_id = cd.service_id
          JOIN 
              senaste_version sv ON cd.date >= sv.start_date
          WHERE
              cd.exception_type = 1
          GROUP BY 
              st.stop_id, cd.date
      ),
      avgangar_vecka AS ( -- De dagliga avgångarna summeras per vardag/lördag/söndag för varje vecka
          SELECT
              stop_id,
              DATE_TRUNC('week', date) AS vecka_start,
              SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
              SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
              SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
              SUM(antal_avgangar) AS total_veckans_avgangar
          FROM
              dagliga_avgangar
          GROUP BY
              stop_id, DATE_TRUNC('week', date)
      ),
      normal_vecka AS ( -- Rangordnar veckorna utefter det totala antalet avgångar i syfte att hitta den vecka med flest som då anses vara en 'normal vecka'
          SELECT
              stop_id,
              vecka_start,
              total_veckans_avgangar,
              total_veckodag_avgangar,
              total_lordag_avgangar,
              total_sondag_avgangar,
              ROW_NUMBER() OVER (PARTITION BY stop_id ORDER BY total_veckans_avgangar DESC) AS rank
          FROM
              avgangar_vecka
          ORDER BY
              stop_id, total_veckans_avgangar DESC
      ),
      routes_per_stop AS ( -- Slår ihop rutter och dess klassificeringar som trafikerar varje hållplatsläge
          SELECT
              st.stop_id,
              STRING_AGG(DISTINCT r.route_short_name::TEXT, ',') AS linjer,
              STRING_AGG(DISTINCT lc.klassificering, ',') AS linjetyper
          FROM 
              gtfs.stop_times st
          JOIN 
              gtfs.trips t ON st.trip_id = t.trip_id
          JOIN 
              gtfs.routes r ON t.route_id = r.route_id
          JOIN
              gtfs.linjeklassificering lc ON r.route_short_name = lc.route_short_name
          GROUP BY
              st.stop_id
      )
      SELECT -- Slutliga sammanställningen av all information som samlats/skapats i de föregående undersatserna (CTE:er)
          st.stop_id,
          st.hpl_id,
          st.stop_name,
          st.platform_code,
          st.parent_station,
          ROUND(total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
          total_lordag_avgangar AS antal_lordag_avgangar,
          total_sondag_avgangar AS antal_sondag_avgangar,
          ROUND(total_veckans_avgangar / 7.0, 2) AS genomsnitt_dagliga_avgangar,
          rs.linjer,
          rs.linjetyper,
          st.geometry
      FROM
          normal_vecka nv
      JOIN
          gtfs.stops st ON nv.stop_id = st.stop_id
      LEFT JOIN
          routes_per_stop rs ON st.stop_id = rs.stop_id
      WHERE
          nv.rank = 1;"
    dbExecute(con, sql_hallplatslage_avgangar)
    
    # Skapa materialiserad vy för hållplatser
    sql_hallplats_avgangar = "
      CREATE MATERIALIZED VIEW gtfs.vy_hallplats_avgangar AS --Slår ihop informationen från alla hållplatslägen som hör till en hållplats
      SELECT
          nva.parent_station AS stop_id,
          h.hpl_id,
          h.stop_name,
          SUM(nva.genomsnitt_veckodag_avgangar) AS genomsnitt_veckodag_avgangar,
          SUM(nva.antal_lordag_avgangar) AS antal_lordag_avgangar,
          SUM(nva.antal_sondag_avgangar) AS antal_sondag_avgangar,
          SUM(nva.genomsnitt_dagliga_avgangar) AS genomsnitt_dagliga_avgangar,
          ARRAY_TO_STRING(
            ARRAY(
              SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjer, ',' ORDER BY nva.linjer), ','))
            ORDER BY unnest
          ), ', '
          ) AS linjer,
          ARRAY_TO_STRING(
            ARRAY(
              SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjetyper, ',' ORDER BY nva.linjetyper), ','))
            ORDER BY unnest
          ), ', '
          ) AS linjetyper,
          h.geometry
      FROM
          gtfs.vy_hallplatslage_avgangar nva
      JOIN
          gtfs.stops h ON nva.parent_station = h.stop_id
      WHERE
          nva.parent_station IS NOT NULL
      GROUP BY
          nva.parent_station, h.hpl_id, h.stop_name, h.geometry;
    "
    dbExecute(con, sql_hallplats_avgangar)
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av vyer:", e$message))
  })
}

skapa_vyer_linjer <- function(con) {
  tryCatch({
    # Droppa den materialiserade vyn om den redan finns
    dbExecute(con, "DROP MATERIALIZED VIEW IF EXISTS gtfs.vy_linjer_avgangar_alla CASCADE;")
    dbExecute(con, "DROP MATERIALIZED VIEW IF EXISTS gtfs.vy_linjer_avgangar_vanligaste CASCADE;")
    
    # Skapa materialiserad vy för alla linjer med rutter, antal trips och dagliga avgångar
    sql_alla_linjer_avgangar = "
      CREATE MATERIALIZED VIEW gtfs.vy_linjer_avgangar_alla AS
      WITH senaste_version AS ( -- Hämtar startdatum för den senaste versionen så att enbart trafik from detta datum tas med
          SELECT start_date
          FROM gtfs_historisk.versions
          ORDER BY start_date DESC
          LIMIT 1
      ),
      dagliga_avgangar AS ( -- Beräkna antalet avgångar/dag och linje (shapes_line)
          SELECT
              t.shape_id,
              t.route_id,
              cd.date,
              COUNT(DISTINCT t.trip_id) AS antal_avgangar
          FROM 
              gtfs.trips t
          JOIN 
              gtfs.calendar_dates cd ON t.service_id = cd.service_id
          JOIN 
              senaste_version sv ON cd.date >= sv.start_date
          WHERE
              cd.exception_type = 1
          GROUP BY 
              t.shape_id, t.route_id, cd.date
      ),
      avgangar_vecka AS ( -- De dagliga avgångarna summeras per vardag/lördag/söndag för varje vecka
          SELECT
              shape_id,
              route_id,
              DATE_TRUNC('week', date) AS vecka_start,
              SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
              SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
              SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
              SUM(antal_avgangar) AS total_veckans_avgangar
          FROM
              dagliga_avgangar
          GROUP BY
              shape_id, route_id, DATE_TRUNC('week', date)
      ),
      normal_vecka AS ( -- Rangordnar veckorna utefter det totala antalet avgångar i syfte att hitta den vecka med flest som då anses vara en 'normal vecka'
          SELECT
              shape_id,
              route_id,
              vecka_start,
              total_veckans_avgangar,
              total_veckodag_avgangar,
              total_lordag_avgangar,
              total_sondag_avgangar,
              ROW_NUMBER() OVER (PARTITION BY shape_id, route_id ORDER BY total_veckans_avgangar DESC) AS rank
          FROM
              avgangar_vecka
          ORDER BY
              shape_id, route_id, total_veckans_avgangar DESC
      ),
      shapes_info AS ( -- Kopplar samman shapes_line och routes och deras klassificeringar via trips
          SELECT
              sl.shape_id,
              sl.antal_punkter,
              sl.max_dist,
              sl.geometry,
              r.route_id,
              r.route_short_name,
              r.route_long_name,
              lc.klassificering
          FROM
              gtfs.shapes_line sl
          JOIN
              gtfs.trips t ON sl.shape_id = t.shape_id
          JOIN
              gtfs.routes r ON t.route_id = r.route_id
          LEFT JOIN
              gtfs.linjeklassificering lc ON r.route_short_name = lc.route_short_name
      )
      SELECT -- Sammanställer all information
          si.shape_id,
          si.route_short_name AS linjenummer,
          si.route_long_name,
          si.klassificering,
          si.antal_punkter,
          si.max_dist AS max_avstand_punkter,
          si.geometry,
          ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
          nv.total_lordag_avgangar AS antal_lordag_avgangar,
          nv.total_sondag_avgangar AS antal_sondag_avgangar,
          ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_veckans_avgangar,
          COUNT(DISTINCT t.trip_id) AS antal_turer
      FROM 
          normal_vecka nv
      JOIN
          shapes_info si ON nv.shape_id = si.shape_id AND nv.route_id = si.route_id
      LEFT JOIN
          gtfs.trips t ON si.shape_id = t.shape_id
      WHERE
          nv.rank = 1
      GROUP BY
          si.shape_id, si.route_short_name, si.route_long_name, si.klassificering, si.antal_punkter, si.max_dist, si.geometry, nv.total_veckodag_avgangar, nv.total_lordag_avgangar, nv.total_sondag_avgangar, nv.total_veckans_avgangar;
    "
    dbExecute(con, sql_alla_linjer_avgangar)
    
    # Skapa materialiserad vy för den vanligaste linjen för varje rutt
    sql_vanligaste_linjen = "
      CREATE MATERIALIZED VIEW gtfs.vy_linjer_avgangar_vanligaste AS
      WITH vanligaste_linje_per_route AS (
          SELECT 
              linjenummer,
              klassificering,
              shape_id,
              antal_turer,
              genomsnitt_veckodag_avgangar,
              antal_lordag_avgangar,
              antal_sondag_avgangar,
              genomsnitt_veckans_avgangar,
              antal_punkter,
              max_avstand_punkter,
              geometry,
              ROW_NUMBER() OVER (PARTITION BY linjenummer ORDER BY antal_turer DESC) AS rank
          FROM 
              gtfs.vy_linjer_avgangar_alla
      )
      SELECT 
          linjenummer,
          klassificering,
          shape_id,
          antal_turer,
          genomsnitt_veckodag_avgangar,
          antal_lordag_avgangar,
          antal_sondag_avgangar,
          genomsnitt_veckans_avgangar,
          antal_punkter,
          max_avstand_punkter,
          geometry
      FROM 
          vanligaste_linje_per_route
      WHERE 
          rank = 1;
    "
    dbExecute(con, sql_vanligaste_linjen)
    
    message("Materialiserad vy 'gtfs.vy_alla_linjer_avgangar' skapad framgångsrikt.")
    
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av vyn:", e$message))
  })
}

skapa_vyer_historisk_hallplats <- function(con) {
  tryCatch({
    # Droppa de materialiserade vyerna om de finns
    dbExecute(con, "DROP MATERIALIZED VIEW IF EXISTS gtfs_historisk.vy_historisk_hallplatslage_avgangar CASCADE;")
    dbExecute(con, "DROP MATERIALIZED VIEW IF EXISTS gtfs_historisk.vy_historisk_hallplats_avgangar CASCADE;")
    
    # Skapa materialiserad vy för hållplatser
    # Se till att ta med version i alla JOINS
    sql_hallplats_avgangar = "
      CREATE MATERIALIZED VIEW gtfs_historisk.vy_historisk_hallplatslage_avgangar AS
      WITH dagliga_avgangar AS (
          SELECT
              st.stop_id,
              cd.date,
              v.version,
              COUNT(DISTINCT st.trip_id) AS antal_avgangar
          FROM 
              gtfs_historisk.stop_times st
          JOIN 
              gtfs_historisk.trips t ON st.trip_id = t.trip_id AND st.version = t.version
          JOIN 
              gtfs_historisk.calendar_dates cd ON t.service_id = cd.service_id AND t.version = cd.version
          JOIN 
              gtfs_historisk.versions v ON t.version = v.version
          WHERE
              cd.exception_type = 1
          GROUP BY 
              st.stop_id, cd.date, v.version
      ),
      avgangar_vecka AS (
          SELECT
              stop_id,
              version,
              DATE_TRUNC('week', date) AS vecka_start,
              SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
              SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
              SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
              SUM(antal_avgangar) AS total_veckans_avgangar
          FROM
              dagliga_avgangar
          GROUP BY
              stop_id, version, DATE_TRUNC('week', date)
      ),
      normal_vecka AS (
          SELECT
              stop_id,
              version,
              vecka_start,
              total_veckans_avgangar,
              total_veckodag_avgangar,
              total_lordag_avgangar,
              total_sondag_avgangar,
              ROW_NUMBER() OVER (PARTITION BY stop_id, version ORDER BY total_veckans_avgangar DESC) AS rank
          FROM
              avgangar_vecka
      ),
      routes_per_stop AS (
          SELECT
              st.stop_id,
              v.version,
              STRING_AGG(DISTINCT r.route_short_name::TEXT, ', ') AS linjer,
              STRING_AGG(DISTINCT lc.klassificering, ', ') AS linjetyper
          FROM 
              gtfs_historisk.stop_times st
          JOIN 
              gtfs_historisk.trips t ON st.trip_id = t.trip_id AND st.version = t.version
          JOIN 
              gtfs_historisk.routes r ON t.route_id = r.route_id AND t.version = r.version
          LEFT JOIN
              gtfs_historisk.linjeklassificering lc ON r.route_short_name = lc.route_short_name AND r.version = lc.version
          JOIN
              gtfs_historisk.versions v ON t.version = v.version
          GROUP BY
              st.stop_id, v.version
      )
      SELECT 
          DISTINCT ON (st.stop_id, v.version)
          CONCAT(st.stop_id, '_', v.version) AS unique_id, -- Skapa en unik identifierare
          st.stop_id,
          st.hpl_id,
          st.stop_name,
          st.platform_code,
          st.parent_station,
          ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
          nv.total_lordag_avgangar AS antal_lordag_avgangar,
          nv.total_sondag_avgangar AS antal_sondag_avgangar,
          ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_dagliga_avgangar,
          rs.linjer,
          rs.linjetyper,
          ST_SetSRID(st.geometry, 3006) AS geometry, -- Se till att SRID är korrekt
          v.start_date AS startdatum,
          v.end_date AS slutdatum,
          v.version
      FROM
          normal_vecka nv
      JOIN
          gtfs_historisk.stops st ON nv.stop_id = st.stop_id AND nv.version = st.version
      LEFT JOIN
          routes_per_stop rs ON st.stop_id = rs.stop_id AND nv.version = rs.version
      JOIN
          gtfs_historisk.versions v ON nv.version = v.version
      WHERE
          nv.rank = 1;
    "
    dbExecute(con, sql_hallplats_avgangar)
    
    # Skapa materialiserad vy för hållplatser
    sql_hallplats_avgangar = "
      CREATE MATERIALIZED VIEW gtfs_historisk.vy_historisk_hallplats_avgangar AS
      SELECT
          CONCAT(nva.parent_station, '_', nva.version) AS unique_id, -- Skapa en unik identifierare
          nva.parent_station AS stop_id,
          h.hpl_id,
          h.stop_name,
          nva.startdatum,
          nva.slutdatum,
          nva.version,
          SUM(nva.genomsnitt_veckodag_avgangar) AS genomsnitt_veckodag_avgangar,
          SUM(nva.antal_lordag_avgangar) AS antal_lordag_avgangar,
          SUM(nva.antal_sondag_avgangar) AS antal_sondag_avgangar,
          SUM(nva.genomsnitt_dagliga_avgangar) AS genomsnitt_dagliga_avgangar,
          ARRAY_TO_STRING(
            ARRAY(
              SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjer, ', ' ORDER BY nva.linjer), ', '))
            ORDER BY unnest
          ), ', '
          ) AS linjer,
          ARRAY_TO_STRING(
            ARRAY(
              SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjetyper, ', ' ORDER BY nva.linjetyper), ', '))
            ORDER BY unnest
          ), ', '
          ) AS linjetyper,
          ST_SetSRID(h.geometry, 3006) AS geometry -- Se till att SRID är korrekt
      FROM
          gtfs_historisk.vy_historisk_hallplatslage_avgangar nva
      JOIN
          gtfs_historisk.stops h ON nva.parent_station = h.stop_id AND nva.version = h.version
      WHERE
          nva.parent_station IS NOT NULL
      GROUP BY
          nva.parent_station, h.hpl_id, h.stop_name, nva.startdatum, nva.slutdatum, h.geometry, nva.version;
    "
    dbExecute(con, sql_hallplats_avgangar)
    
    message("Materialiserade vyer skapade framgångsrikt.")
    
  }, error = function(e) {
    stop(glue("Ett fel inträffade vid skapandet av materialiserade vyer: {e$message}"))
  })
}

skapa_vyer_historisk_linjer <- function(con) {
  tryCatch({
    # Droppa de materialiserade vyerna om de finns
    dbExecute(con, "DROP MATERIALIZED VIEW IF EXISTS gtfs_historisk.vy_historisk_linjer_avgangar_alla CASCADE;")
    dbExecute(con, "DROP MATERIALIZED VIEW IF EXISTS gtfs_historisk.vy_historisk_linjer_avgangar_vanligaste CASCADE;")
    
    # Skapa materialiserad vy för alla linjer med rutter, antal trips och dagliga avgångar
    sql_alla_linjer_avgangar = "
      CREATE MATERIALIZED VIEW gtfs_historisk.vy_historisk_linjer_avgangar_alla AS
      WITH dagliga_avgangar AS (
          SELECT
              t.shape_id,
              t.route_id,
              cd.date,
              t.version,
              COUNT(DISTINCT t.trip_id) AS antal_avgangar
          FROM 
              gtfs_historisk.trips t
          JOIN 
              gtfs_historisk.calendar_dates cd ON t.service_id = cd.service_id AND t.version = cd.version
          WHERE
              cd.exception_type = 1
          GROUP BY 
              t.shape_id, t.route_id, cd.date, t.version
      ),
      avgangar_vecka AS (
          SELECT
              shape_id,
              route_id,
              version,
              DATE_TRUNC('week', date) AS vecka_start,
              SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
              SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
              SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
              SUM(antal_avgangar) AS total_veckans_avgangar
          FROM
              dagliga_avgangar
          GROUP BY
              shape_id, route_id, version, DATE_TRUNC('week', date)
      ),
      normal_vecka AS (
          SELECT
              shape_id,
              route_id,
              version,
              vecka_start,
              total_veckans_avgangar,
              total_veckodag_avgangar,
              total_lordag_avgangar,
              total_sondag_avgangar,
              ROW_NUMBER() OVER (PARTITION BY shape_id, route_id, version ORDER BY total_veckans_avgangar DESC) AS rank
          FROM
              avgangar_vecka
      ),
      shapes_info AS (
          SELECT
              sl.shape_id,
              sl.antal_punkter,
              sl.max_dist,
              sl.geometry,
              r.route_id,
              r.route_short_name,
              r.route_long_name,
              lc.klassificering,
              r.version
          FROM
              gtfs_historisk.shapes_line sl
          JOIN
              gtfs_historisk.trips t ON sl.shape_id = t.shape_id AND sl.version = t.version
          JOIN
              gtfs_historisk.routes r ON t.route_id = r.route_id AND t.version = r.version
          LEFT JOIN
              gtfs_historisk.linjeklassificering lc ON r.route_short_name = lc.route_short_name AND r.version = lc.version
      )
      SELECT 
          CONCAT(si.shape_id, '_', si.version) AS unique_id,
          si.shape_id,
          si.route_short_name AS linjenummer,
          si.route_long_name,
          si.klassificering,
          si.antal_punkter,
          si.max_dist AS max_avstand_punkter,
          si.geometry,
          ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
          nv.total_lordag_avgangar AS antal_lordag_avgangar,
          nv.total_sondag_avgangar AS antal_sondag_avgangar,
          ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_veckans_avgangar,
          COUNT(DISTINCT t.trip_id) AS antal_turer,
          si.version,
          v.start_date AS startdatum,
          v.end_date AS slutdatum
      FROM 
          normal_vecka nv
      JOIN
          shapes_info si ON nv.shape_id = si.shape_id AND nv.route_id = si.route_id AND nv.version = si.version
      LEFT JOIN
          gtfs_historisk.trips t ON si.shape_id = t.shape_id AND si.version = t.version
      JOIN
          gtfs_historisk.versions v ON si.version = v.version
      WHERE
          nv.rank = 1
      GROUP BY
          si.shape_id, si.route_short_name, si.route_long_name, si.klassificering, si.antal_punkter, si.max_dist, si.geometry, si.version, v.start_date, v.end_date, nv.total_veckodag_avgangar, nv.total_lordag_avgangar, nv.total_sondag_avgangar, nv.total_veckans_avgangar;
    "
    dbExecute(con, sql_alla_linjer_avgangar)
    
    # Skapa materialiserad vy för den vanligaste linjen för varje rutt
    sql_vanligaste_linjen = "
      CREATE MATERIALIZED VIEW gtfs_historisk.vy_historisk_linjer_avgangar_vanligaste AS
      WITH vanligaste_linje_per_route AS (
          SELECT 
              unique_id,
              linjenummer,
              klassificering,
              shape_id,
              antal_turer,
              genomsnitt_veckodag_avgangar,
              antal_lordag_avgangar,
              antal_sondag_avgangar,
              genomsnitt_veckans_avgangar,
              antal_punkter,
              max_avstand_punkter,
              geometry,
              version,
              startdatum,
              slutdatum,
              ROW_NUMBER() OVER (PARTITION BY linjenummer, version ORDER BY antal_turer DESC) AS rank
          FROM 
              gtfs_historisk.vy_historisk_linjer_avgangar_alla
      )
      SELECT 
          unique_id,
          linjenummer,
          klassificering,
          shape_id,
          antal_turer,
          genomsnitt_veckodag_avgangar,
          antal_lordag_avgangar,
          antal_sondag_avgangar,
          genomsnitt_veckans_avgangar,
          antal_punkter,
          max_avstand_punkter,
          geometry,
          version,
          startdatum,
          slutdatum
      FROM 
          vanligaste_linje_per_route
      WHERE 
          rank = 1;
    "
    dbExecute(con, sql_vanligaste_linjen)
    
    message("Materialiserade vyer skapade framgångsrikt.")
    
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av materialiserade vyer:", e$message))
  })
}
