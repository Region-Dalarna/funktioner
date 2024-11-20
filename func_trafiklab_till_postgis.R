


skapa_tabeller <- function(con, schema = schema_namn) {
  tryCatch({
    
    # Create schemas if they do not exist
    dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS {schema};"))
    dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS {schema}_historisk;"))
    
    # agency
    dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema}.agency (
                      agency_id VARCHAR PRIMARY KEY,
                      agency_name VARCHAR NOT NULL,
                      agency_url VARCHAR NOT NULL,
                      agency_timezone VARCHAR NOT NULL,
                      agency_lang VARCHAR,
                      agency_phone VARCHAR,
                      agency_fare_url VARCHAR
                  );"))
    
    # routes
    dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema}.routes (
                      route_id VARCHAR PRIMARY KEY,
                      agency_id VARCHAR REFERENCES {schema}.agency(agency_id),
                      route_short_name VARCHAR NOT NULL,
                      route_long_name VARCHAR NOT NULL,
                      route_desc VARCHAR,
                      route_type INTEGER NOT NULL,
                      route_url VARCHAR,
                      route_color VARCHAR,
                      route_text_color VARCHAR
                  );"))
    # routes - index
    dbExecute(con, glue::glue("CREATE INDEX IF NOT EXISTS idx_routes_route_short_name ON {schema}.routes (route_short_name);"))
    
    # calendar_dates
    dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema}.calendar_dates (
                      service_id VARCHAR,
                      date DATE,
                      exception_type INTEGER,
                      PRIMARY KEY (service_id, date)
                  );"))
    
    # shapes_line
    dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema}.shapes_line (
                      shape_id VARCHAR,
                      geometry GEOMETRY(Linestring, 3006),
                      antal_punkter INTEGER,
                      max_dist FLOAT,
                      PRIMARY KEY (shape_id)
                  );"))
    # shapes_line - index
    dbExecute(con, glue::glue("CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry ON {schema}.shapes_line USING GIST (geometry);"))
    
    # trips
    dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema}.trips (
                      trip_id VARCHAR PRIMARY KEY,
                      route_id VARCHAR REFERENCES {schema}.routes(route_id),
                      service_id VARCHAR NOT NULL,
                      trip_headsign VARCHAR,
                      direction_id INTEGER,
                      shape_id VARCHAR
                  );"))
    # trips - index
    dbExecute(con, glue::glue("CREATE INDEX IF NOT EXISTS idx_trips_shape_id ON {schema}.trips (shape_id);"))
    dbExecute(con, glue::glue("CREATE INDEX IF NOT EXISTS idx_trips_route_id ON {schema}.trips (route_id);"))
    dbExecute(con, glue::glue("CREATE INDEX IF NOT EXISTS idx_trips_service_id ON {schema}.trips (service_id);"))
    
    # stops
    dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema}.stops (
                      stop_id VARCHAR PRIMARY KEY,
                      hpl_id VARCHAR,
                      stop_name VARCHAR NOT NULL,
                      stop_lat FLOAT NOT NULL,
                      stop_lon FLOAT NOT NULL,
                      location_type INTEGER,
                      parent_station VARCHAR,
                      platform_code VARCHAR,
                      geometry GEOMETRY(Point, 3006)
                  );"))
    # stops - index
    dbExecute(con, glue::glue("CREATE INDEX IF NOT EXISTS idx_stops_geometry ON {schema}.stops USING GIST (geometry);"))
    
    # stop_times
    dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema}.stop_times (
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
                      FOREIGN KEY (stop_id) REFERENCES {schema}.stops(stop_id),
                      FOREIGN KEY (trip_id) REFERENCES {schema}.trips(trip_id)
                  );"))
    # stop_times - index
    dbExecute(con, glue::glue("CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id ON {schema}.stop_times (trip_id);"))
    dbExecute(con, glue::glue("CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id ON {schema}.stop_times (stop_id);"))
    
    #Linjeklassificering
    dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema}.linjeklassificering (
                    route_short_name VARCHAR PRIMARY KEY,
                    klassificering VARCHAR NOT NULL
                  );"))
    
    # Tables for historical data
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.agency (
                          agency_id VARCHAR,
                          agency_name VARCHAR NOT NULL,
                          agency_url VARCHAR NOT NULL,
                          agency_timezone VARCHAR NOT NULL,
                          agency_lang VARCHAR,
                          agency_phone VARCHAR,
                          agency_fare_url VARCHAR,
                          version INTEGER,
                          PRIMARY KEY (agency_id, version)
                      );"))
    
    # routes
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.routes (
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
                          FOREIGN KEY (agency_id, version) REFERENCES {schema_namn}_historisk.agency(agency_id, version)
                      );"))
    
    # calendar_dates
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.calendar_dates (
                          service_id VARCHAR,
                          date DATE,
                          exception_type INTEGER,
                          version INTEGER,
                          PRIMARY KEY (service_id, version, date)
                      );"))
    
    # shapes_line
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.shapes_line (
                          shape_id VARCHAR,
                          geometry GEOMETRY(Linestring, 3006),
                          antal_punkter INTEGER,
                          max_dist FLOAT,
                          version INTEGER,
                          PRIMARY KEY (shape_id, version)
                      );"))
    # shapes_line - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry_historisk ON {schema_namn}_historisk.shapes_line USING GIST (geometry);"))
    
    # trips
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.trips (
                          trip_id VARCHAR,
                          route_id VARCHAR,
                          service_id VARCHAR NOT NULL,
                          trip_headsign VARCHAR,
                          direction_id INTEGER,
                          shape_id VARCHAR,
                          version INTEGER,
                          PRIMARY KEY (trip_id, version),
                          FOREIGN KEY (route_id, version) REFERENCES {schema_namn}_historisk.routes(route_id, version)
                      );"))
    # trips - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_route_id_historisk ON {schema_namn}_historisk.trips (route_id, version);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_shape_id_historisk ON {schema_namn}_historisk.trips (shape_id);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_service_id_historisk ON {schema_namn}_historisk.trips (service_id);"))
    
    # stops
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.stops (
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
                      );"))
    # stops - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stops_geometry_historisk ON {schema_namn}_historisk.stops USING GIST (geometry);"))
    
    # stop_times
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.stop_times (
                          trip_id VARCHAR,
                          arrival_time VARCHAR,
                          departure_time VARCHAR,
                          stop_id VARCHAR,
                          stop_sequence INTEGER,
                          stop_headsign VARCHAR,
                          pickup_type INTEGER,
                          drop_off_type INTEGER,
                          shape_dist_traveled FLOAT,
                          timepoint INTEGER,
                          version INTEGER,
                          PRIMARY KEY (trip_id, version, stop_sequence),
                          FOREIGN KEY (trip_id, version) REFERENCES {schema_namn}_historisk.trips(trip_id, version),
                          FOREIGN KEY (stop_id, version) REFERENCES {schema_namn}_historisk.stops(stop_id, version)
                      );"))
    # stop_times - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id_historisk ON {schema_namn}_historisk.stop_times (trip_id, version);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id_historisk ON {schema_namn}_historisk.stop_times (stop_id, version);"))
    
    # Linjeklassificering
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.linjeklassificering (
                        route_short_name VARCHAR,
                        klassificering VARCHAR NOT NULL,
                        version INTEGER,
                        PRIMARY KEY (route_short_name, version)
                      );"))
    
    # Versions table
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_namn}_historisk.versions (
                          version INTEGER PRIMARY KEY,
                          start_date DATE,
                          end_date DATE
                      );"))
    
  }, error = function(e){
    stop(glue("Ett fel inträffade vid skapandet av tabeller: {e$message}"))
  })
}
    

versionshantering <- function(con, gtfs_data, schema = schema_namn) {
  tryCatch({
    dbExecute(con, glue::glue("
  CREATE TABLE IF NOT EXISTS {schema}_historisk.versions (
    version INTEGER PRIMARY KEY,
    start_date DATE,
    end_date DATE
  );
"))
    
    # Check if the historical table "versions" exists
    table_exists_query <- dbGetQuery(con, glue::glue(
      "SELECT EXISTS (
        SELECT FROM information_schema.tables 
        WHERE table_schema = '{schema}_historisk' 
        AND table_name = 'versions'
      );"
    ))
    
    if (!table_exists_query$exists[1]) {
      stop(glue::glue("Table {schema}_historisk.versions does not exist"))
    }
    
    # Retrieve the last date in calendar_dates from the database
    sista_datum_db <- dbGetQuery(con, glue::glue("SELECT MAX(date) AS sista_datum FROM {schema}.calendar_dates;"))
    sista_datum_db <- as.Date(sista_datum_db$sista_datum[1])
    
    # Retrieve the last date from calendar_dates in the dataset
    sista_datum_gtfs_data <- max(gtfs_data$calendar_dates$date)
    
    # If the last date in the db and the dataset is not the same, it's a new version
    if (is.na(sista_datum_db) || sista_datum_db != sista_datum_gtfs_data) {
      
      # Retrieve the latest version number in the database
      senaste_version <- dbGetQuery(con, glue::glue("SELECT MAX(version) AS senaste_version FROM {schema}_historisk.versions;"))
      senaste_version <- senaste_version$senaste_version[1]
      
      # If no previous version exists, set ny_version to 1
      if (is.na(senaste_version)) {
        ny_version <- 1
      } else {
        ny_version <- senaste_version + 1
      }
      
      # If a previous version exists, update its end_date and transfer data from schema to schema_historisk
      if (!is.na(senaste_version)) {
        dbExecute(con, glue::glue("UPDATE {schema}_historisk.versions SET end_date = '{sista_datum_db}' WHERE version = {senaste_version};"))
        
        # Transfer data from schema schema to schema_historisk with version number
        dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.agency SELECT agency_id, agency_name, agency_url, agency_timezone, agency_lang, agency_phone, agency_fare_url, {senaste_version} FROM {schema}.agency;"))
        dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.routes SELECT route_id, agency_id, route_short_name, route_long_name, route_desc, route_type, route_url, route_color, route_text_color, {senaste_version} FROM {schema}.routes;"))
        dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.calendar_dates SELECT service_id, date, exception_type, {senaste_version} FROM {schema}.calendar_dates;"))
        dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.shapes_line SELECT shape_id, geometry, antal_punkter, max_dist, {senaste_version} FROM {schema}.shapes_line;"))
        dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.stops SELECT stop_id, hpl_id, stop_name, stop_lat, stop_lon, location_type, parent_station, platform_code, geometry, {senaste_version} FROM {schema}.stops;"))
        dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.trips SELECT trip_id, route_id, service_id, trip_headsign, direction_id, shape_id, {senaste_version} FROM {schema}.trips;"))
        dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.stop_times SELECT trip_id, arrival_time, departure_time, stop_id, stop_sequence, stop_headsign, pickup_type, drop_off_type, shape_dist_traveled, timepoint, {senaste_version} FROM {schema}.stop_times;"))
        dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.linjeklassificering SELECT route_short_name, klassificering, {senaste_version} FROM {schema}.linjeklassificering;"))
        
        # Create views for historical data - called in versionshantering()
        skapa_vyer_historisk_hallplats(con, schema = schema_namn)
        skapa_vyer_historisk_linjer(con, schema = schema_namn)
      }
      
      # Check if sista_datum_db is NA and set start_datum_gtfs_data correctly
      if (is.na(sista_datum_db)) {
        start_datum_gtfs_data <- min(gtfs_data$calendar_dates$date)
      } else {
        start_datum_gtfs_data <- sista_datum_db + 1
      }
      dbExecute(con, glue::glue("INSERT INTO {schema}_historisk.versions (version, start_date) VALUES ({ny_version}, '{start_datum_gtfs_data}');"))
    }
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid versionshanteringen: {e$message}"))
  })
}

radera_gamla_versioner <- function(con, antal_ar, schema = schema_namn) {
  tryCatch({
    # Calculate the date 3 years ago (or specified number of years)
    tre_ar_sedan <- Sys.Date() - antal_ar * 365
    
    # Fetch versions that are older than the specified number of years
    gamla_versioner <- dbGetQuery(con, glue::glue("SELECT version FROM {schema}_historisk.versions WHERE end_date < '{tre_ar_sedan}';"))
    
    # Loop through the old versions and delete them from the historical tables
    for (version in gamla_versioner$version) {
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.stop_times WHERE version = {version};"))
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.trips WHERE version = {version};"))
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.stops WHERE version = {version};"))
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.shapes_line WHERE version = {version};"))
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.calendar_dates WHERE version = {version};"))
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.routes WHERE version = {version};"))
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.agency WHERE version = {version};"))
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.linjeklassificering WHERE version = {version};"))
      
      # Remove the version from the versions table
      dbExecute(con, glue::glue("DELETE FROM {schema}_historisk.versions WHERE version = {version};"))
    }
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid radering av gamla versioner: {e$message}"))
  })
}

ladda_upp_till_databas <- function(con, gtfs_data, schema = schema_namn) {
  # Error handling for data upload
  tryCatch({
    
    # List of tables to truncate
    tables_to_truncate <- c("stops", "routes", "calendar_dates", "trips", "shapes_line", "stop_times", "agency")
    
    # Truncate tables before uploading new data
    lapply(tables_to_truncate, function(table) {
      dbExecute(con, glue::glue("TRUNCATE TABLE {schema}.{table} RESTART IDENTITY CASCADE;"))
    })
    
    # Spatial tables
    # Add stops (only if the data exists)
    if (!is.null(gtfs_data$stops)) {
      sf_stops <- st_as_sf(gtfs_data$stops, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>% 
        st_transform(3006) %>% 
        st_set_geometry("geometry")
      st_write(obj = sf_stops, dsn = con, Id(schema = schema, table = "stops"), geomtype = "POINT", delete_layer = FALSE, append = TRUE)
      rm(sf_stops)
    }
    
    print("stops fixad")
    
    # Add shapes (only if the data exists)
    if (!is.null(gtfs_data$shapes)) {
      sf_shapes <- st_as_sf(gtfs_data$shapes, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326, remove = FALSE) %>% 
        st_transform(3006) %>% 
        st_set_geometry("geometry")
      
      print("shapes fixad")
      
      # Add shapes as lines and include fields for number of points and max distance between points
      sf_shapes_line <- sf_shapes %>% 
        group_by(shape_id) %>%
        summarize(
          geometry = st_combine(geometry) %>% st_cast("LINESTRING", safe = TRUE),
          antal_punkter = n(),  # Number of points per shape_id
          max_dist = max(shape_dist_traveled - lag(shape_dist_traveled, default = first(shape_dist_traveled)))  # Max distance between consecutive points
        )
      st_write(obj = sf_shapes_line, dsn = con, Id(schema = schema_namn, table = "shapes_line"), geomtype = "LINESTRING", delete_layer = FALSE, append = TRUE)
    }
    
    print("Spatial tables uploaded")
    
    # Non-spatial tables (add only if they exist in gtfs_data)
    non_spatial_tables <- c("agency", "calendar_dates", "routes", "trips", "stop_times")
    lapply(non_spatial_tables, function(table) {
      if (!is.null(gtfs_data[[table]])) {
        dbWriteTable(con, Id(schema = schema, table = table), gtfs_data[[table]], append = TRUE, row.names = FALSE)
        
        print(glue::glue("{table} uploaded"))
      }
    })
    
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid uppladdningen av data till databasen: ", e$message))
  })
}


skapa_tabell_linjeklassificering <- function(con, schema = schema_namn) {
  
  tryCatch({
    # Clear the table before inserting new data
    dbExecute(con, glue::glue("TRUNCATE {schema}.linjeklassificering;"))
    
    # Insert classifications
    dbExecute(con, glue::glue("
    INSERT INTO {schema}.linjeklassificering (route_short_name, klassificering)
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
    FROM {schema}.routes
    ORDER BY route_short_name, route_id;
    "))
    
  }, error = function(e) {
    # Print the error message and return NULL
    stop(paste("Ett fel inträffade vid skapandet av linjeklassificeringen:", e$message))
  })
}

skapa_vyer_hallplats <- function(con, schema = schema_namn) {
  # Error handling
  tryCatch({
    # Drop the materialized views if they exist
    dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.vy_hallplats_avgangar CASCADE;"))
    dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.vy_hallplatslage_avgangar CASCADE;"))
    
    # Create materialized view for stops with departures, routes, and classifications
    sql_hallplatslage_avgangar = glue::glue("
      CREATE MATERIALIZED VIEW {schema}.vy_hallplatslage_avgangar AS
      WITH senaste_version AS ( -- Fetches start date for the latest version so that only traffic from this date is included
          SELECT start_date
          FROM {schema}_historisk.versions
          ORDER BY start_date DESC
          LIMIT 1
      ),
      dagliga_avgangar AS ( -- Calculate the number of departures/day and stop position
          SELECT
              st.stop_id,
              cd.date,
              COUNT(DISTINCT st.trip_id) AS antal_avgangar
          FROM 
              {schema}.stop_times st
          JOIN 
              {schema}.trips t ON st.trip_id = t.trip_id
          JOIN 
              {schema}.calendar_dates cd ON t.service_id = cd.service_id
          JOIN 
              senaste_version sv ON cd.date >= sv.start_date
          WHERE
              cd.exception_type = 1
          GROUP BY 
              st.stop_id, cd.date
      ),
      avgangar_vecka AS ( -- Daily departures are summed per weekday/Saturday/Sunday for each week
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
      normal_vecka AS ( -- Ranks the weeks based on total number of departures to find the week with the most, considered as a 'normal week'
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
      routes_per_stop AS ( -- Combines routes and their classifications that serve each stop
          SELECT
              st.stop_id,
              STRING_AGG(DISTINCT r.route_short_name::TEXT, ',') AS linjer,
              STRING_AGG(DISTINCT lc.klassificering, ',') AS linjetyper
          FROM 
              {schema}.stop_times st
          JOIN 
              {schema}.trips t ON st.trip_id = t.trip_id
          JOIN 
              {schema}.routes r ON t.route_id = r.route_id
          JOIN
              {schema}.linjeklassificering lc ON r.route_short_name = lc.route_short_name
          GROUP BY
              st.stop_id
      )
      SELECT -- Final summary of all information collected/created in the previous CTEs
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
          {schema}.stops st ON nv.stop_id = st.stop_id
      LEFT JOIN
          routes_per_stop rs ON st.stop_id = rs.stop_id
      WHERE
          nv.rank = 1;
    ")
    dbExecute(con, sql_hallplatslage_avgangar)
    
    # Create materialized view for stops
    sql_hallplats_avgangar = glue::glue("
  CREATE MATERIALIZED VIEW {schema}.vy_hallplats_avgangar AS -- Combines information from all stop locations associated with a stop
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
      {schema}.vy_hallplatslage_avgangar nva
  JOIN
      {schema}.stops h ON nva.parent_station = h.stop_id
  WHERE
      nva.parent_station IS NOT NULL
  GROUP BY
      nva.parent_station, h.hpl_id, h.stop_name, h.geometry;
")
    dbExecute(con, sql_hallplats_avgangar)
    
    
  }, error = function(e) {
    # Print error message
    stop(paste("Ett fel inträffade vid skapandet av vyer för hållplats:", e$message))
  })
}

skapa_vyer_linjer <- function(con, schema = schema_namn) {
  tryCatch({
    # Drop the materialized view if it exists
    dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.vy_linjer_avgangar_alla CASCADE;"))
    dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.vy_linjer_avgangar_vanligaste CASCADE;"))
    
    # Create materialized view for all lines with routes, trips, and daily departures
    sql_alla_linjer_avgangar = glue::glue("
      CREATE MATERIALIZED VIEW {schema}.vy_linjer_avgangar_alla AS
      WITH senaste_version AS ( -- Fetches the start date for the latest version so that only traffic from this date is included
          SELECT start_date
          FROM {schema}_historisk.versions
          ORDER BY start_date DESC
          LIMIT 1
      ),
      dagliga_avgangar AS ( -- Calculate the number of daily departures per line (shapes_line)
          SELECT
              t.shape_id,
              t.route_id,
              cd.date,
              COUNT(DISTINCT t.trip_id) AS antal_avgangar
          FROM 
              {schema}.trips t
          JOIN 
              {schema}.calendar_dates cd ON t.service_id = cd.service_id
          JOIN 
              senaste_version sv ON cd.date >= sv.start_date
          WHERE
              cd.exception_type = 1
          GROUP BY 
              t.shape_id, t.route_id, cd.date
      ),
      avgangar_vecka AS ( -- Sum daily departures per weekday/Saturday/Sunday for each week
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
      normal_vecka AS ( -- Rank weeks based on total departures to find the week with the most, considered a 'normal week'
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
      shapes_info AS ( -- Join shapes_line and routes with their classifications via trips
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
              {schema}.shapes_line sl
          JOIN
              {schema}.trips t ON sl.shape_id = t.shape_id
          JOIN
              {schema}.routes r ON t.route_id = r.route_id
          LEFT JOIN
              {schema}.linjeklassificering lc ON r.route_short_name = lc.route_short_name
      )
      SELECT -- Summarize all information
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
          {schema}.trips t ON si.shape_id = t.shape_id
      WHERE
          nv.rank = 1
      GROUP BY
          si.shape_id, si.route_short_name, si.route_long_name, si.klassificering, si.antal_punkter, si.max_dist, si.geometry, nv.total_veckodag_avgangar, nv.total_lordag_avgangar, nv.total_sondag_avgangar, nv.total_veckans_avgangar;
    ")
    dbExecute(con, sql_alla_linjer_avgangar)
    
    # Create materialized view for the most common line for each route
    sql_vanligaste_linjen = glue::glue("
      CREATE MATERIALIZED VIEW {schema}.vy_linjer_avgangar_vanligaste AS
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
              {schema}.vy_linjer_avgangar_alla
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
    ")
    dbExecute(con, sql_vanligaste_linjen)
    
    message(glue::glue("Materialiserad vy '{schema}.vy_alla_linjer_avgangar' skapad framgångsrikt."))
    
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av vyn:", e$message))
  })
}

skapa_vyer_historisk_hallplats <- function(con, schema = schema_namn) {
  tryCatch({
    # Drop the materialized views if they exist
    dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.vy_historisk_hallplatslage_avgangar CASCADE;"))
    dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.vy_historisk_hallplats_avgangar CASCADE;"))
    
    # Create materialized view for stops
    sql_hallplats_avgangar = glue::glue("
      CREATE MATERIALIZED VIEW {schema}.vy_historisk_hallplatslage_avgangar AS
      WITH dagliga_avgangar AS (
          SELECT
              st.stop_id,
              cd.date,
              v.version,
              COUNT(DISTINCT st.trip_id) AS antal_avgangar
          FROM 
              {schema}.stop_times st
          JOIN 
              {schema}.trips t ON st.trip_id = t.trip_id AND st.version = t.version
          JOIN 
              {schema}.calendar_dates cd ON t.service_id = cd.service_id AND t.version = cd.version
          JOIN 
              {schema}.versions v ON t.version = v.version
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
              {schema}.stop_times st
          JOIN 
              {schema}.trips t ON st.trip_id = t.trip_id AND st.version = t.version
          JOIN 
              {schema}.routes r ON t.route_id = r.route_id AND t.version = r.version
          LEFT JOIN
              {schema}.linjeklassificering lc ON r.route_short_name = lc.route_short_name AND r.version = lc.version
          JOIN
              {schema}.versions v ON t.version = v.version
          GROUP BY
              st.stop_id, v.version
      )
      SELECT 
          DISTINCT ON (st.stop_id, v.version)
          CONCAT(st.stop_id, '_', v.version) AS unique_id, -- Create a unique identifier
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
          ST_SetSRID(st.geometry, 3006) AS geometry, -- Ensure the SRID is correct
          v.start_date AS startdatum,
          v.end_date AS slutdatum,
          v.version
      FROM
          normal_vecka nv
      JOIN
          {schema}.stops st ON nv.stop_id = st.stop_id AND nv.version = st.version
      LEFT JOIN
          routes_per_stop rs ON st.stop_id = rs.stop_id AND nv.version = rs.version
      JOIN
          {schema}.versions v ON nv.version = v.version
      WHERE
          nv.rank = 1;
    ")
    dbExecute(con, sql_hallplats_avgangar)
    
    # Create materialized view for historical stops
    sql_hallplats_avgangar = glue::glue("
      CREATE MATERIALIZED VIEW {schema}.vy_historisk_hallplats_avgangar AS
      SELECT
          CONCAT(nva.parent_station, '_', nva.version) AS unique_id, -- Create a unique identifier
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
          ST_SetSRID(h.geometry, 3006) AS geometry -- Ensure the SRID is correct
      FROM
          {schema}.vy_historisk_hallplatslage_avgangar nva
      JOIN
          {schema}.stops h ON nva.parent_station = h.stop_id AND nva.version = h.version
      WHERE
          nva.parent_station IS NOT NULL
      GROUP BY
          nva.parent_station, h.hpl_id, h.stop_name, nva.startdatum, nva.slutdatum, h.geometry, nva.version;
    ")
    dbExecute(con, sql_hallplats_avgangar)
    
    message("Materialiserade vyer skapade framgångsrikt.")
    
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid skapandet av materialiserade vyer: {e$message}"))
  })
}

skapa_vyer_historisk_linjer <- function(con, schema = schema_namn) {
  tryCatch({
    # Drop the materialized views if they exist
    dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.vy_historisk_linjer_avgangar_alla CASCADE;"))
    dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.vy_historisk_linjer_avgangar_vanligaste CASCADE;"))
    
    # Create materialized view for all lines with routes, trips, and daily departures
    sql_alla_linjer_avgangar = glue::glue("
      CREATE MATERIALIZED VIEW {schema}.vy_historisk_linjer_avgangar_alla AS
      WITH dagliga_avgangar AS (
          SELECT
              t.shape_id,
              t.route_id,
              cd.date,
              t.version,
              COUNT(DISTINCT t.trip_id) AS antal_avgangar
          FROM 
              {schema}.trips t
          JOIN 
              {schema}.calendar_dates cd ON t.service_id = cd.service_id AND t.version = cd.version
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
              {schema}.shapes_line sl
          JOIN
              {schema}.trips t ON sl.shape_id = t.shape_id AND sl.version = t.version
          JOIN
              {schema}.routes r ON t.route_id = r.route_id AND t.version = r.version
          LEFT JOIN
              {schema}.linjeklassificering lc ON r.route_short_name = lc.route_short_name AND r.version = lc.version
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
          {schema}.trips t ON si.shape_id = t.shape_id AND si.version = t.version
      JOIN
          {schema}.versions v ON si.version = v.version
      WHERE
          nv.rank = 1
      GROUP BY
          si.shape_id, si.route_short_name, si.route_long_name, si.klassificering, si.antal_punkter, si.max_dist, si.geometry, si.version, v.start_date, v.end_date, nv.total_veckodag_avgangar, nv.total_lordag_avgangar, nv.total_sondag_avgangar, nv.total_veckans_avgangar;
    ")
    dbExecute(con, sql_alla_linjer_avgangar)
    
    # Create materialized view for the most common line per route
    sql_vanligaste_linjen = glue::glue("
      CREATE MATERIALIZED VIEW {schema}.vy_historisk_linjer_avgangar_vanligaste AS
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
              {schema}.vy_historisk_linjer_avgangar_alla
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
    ")
    dbExecute(con, sql_vanligaste_linjen)
    
    message("Materialiserade vyer skapade framgångsrikt.")
    
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid skapandet av materialiserade vyer: {e$message}"))
  })
}
