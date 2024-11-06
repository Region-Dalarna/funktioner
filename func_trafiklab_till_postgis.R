


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

skapa_tabeller <- function(con, schema_name, data_list) {
  tryCatch({
    # Skapa schema om det inte finns
    dbExecute(con, glue("CREATE SCHEMA IF NOT EXISTS {schema_name};"))
    dbExecute(con, glue("CREATE SCHEMA IF NOT EXISTS {schema_name}_historisk;"))
    
    # agency
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}.agency (
                      agency_id VARCHAR PRIMARY KEY,
                      agency_name VARCHAR NOT NULL,
                      agency_url VARCHAR NOT NULL,
                      agency_timezone VARCHAR NOT NULL,
                      agency_lang VARCHAR,
                      agency_phone VARCHAR,
                      agency_fare_url VARCHAR
                  );"))
    
    # routes
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}.routes (
                      route_id VARCHAR PRIMARY KEY,
                      agency_id VARCHAR REFERENCES {schema_name}.agency(agency_id),
                      route_short_name VARCHAR NOT NULL,
                      route_long_name VARCHAR NOT NULL,
                      route_desc VARCHAR,
                      route_type INTEGER NOT NULL,
                      route_url VARCHAR,
                      route_color VARCHAR,
                      route_text_color VARCHAR
                  );"))
    # routes - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_routes_route_short_name ON {schema_name}.routes (route_short_name);"))
    
    # calendar_dates
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}.calendar_dates (
                      service_id VARCHAR,
                      date DATE,
                      exception_type INTEGER,
                      PRIMARY KEY (service_id, date)
                  );"))
    
    # Conditionally create shapes_line table if shapes data is available in the input data list
    if ("shapes" %in% names(data_list)) {
      dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}.shapes_line (
                        shape_id VARCHAR,
                        geometry GEOMETRY(Linestring, 3006),
                        antal_punkter INTEGER,
                        max_dist FLOAT,
                        PRIMARY KEY (shape_id)
                    );"))
      # shapes_line - index
      dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry ON {schema_name}.shapes_line USING GIST (geometry);"))
    }
    
    # trips
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}.trips (
                      trip_id VARCHAR PRIMARY KEY,
                      route_id VARCHAR REFERENCES {schema_name}.routes(route_id),
                      service_id VARCHAR NOT NULL,
                      trip_headsign VARCHAR,
                      direction_id INTEGER,
                      shape_id VARCHAR
                  );"))
    # trips - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_shape_id ON {schema_name}.trips (shape_id);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_route_id ON {schema_name}.trips (route_id);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_service_id ON {schema_name}.trips (service_id);"))
    
    # stops
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}.stops (
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
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stops_geometry ON {schema_name}.stops USING GIST (geometry);"))
    
    # stop_times
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}.stop_times (
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
                      FOREIGN KEY (stop_id) REFERENCES {schema_name}.stops(stop_id),
                      FOREIGN KEY (trip_id) REFERENCES {schema_name}.trips(trip_id)
                  );"))
    # stop_times - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id ON {schema_name}.stop_times (trip_id);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id ON {schema_name}.stop_times (stop_id);"))
    
    # Linjeklassificering
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}.linjeklassificering (
                    route_short_name VARCHAR PRIMARY KEY,
                    klassificering VARCHAR NOT NULL
                  );"))
    
    # Tables for historical data
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.agency (
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
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.routes (
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
                      FOREIGN KEY (agency_id, version) REFERENCES {schema_name}_historisk.agency(agency_id, version)
                  );"))
    
    # calendar_dates
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.calendar_dates (
                      service_id VARCHAR,
                      date DATE,
                      exception_type INTEGER,
                      version INTEGER,
                      PRIMARY KEY (service_id, version, date)
                  );"))
    
    # Conditionally create shapes_line for historical data if shapes data is available
    if ("shapes" %in% names(data_list)) {
      dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.shapes_line (
                        shape_id VARCHAR,
                        geometry GEOMETRY(Linestring, 3006),
                        antal_punkter INTEGER,
                        max_dist FLOAT,
                        version INTEGER,
                        PRIMARY KEY (shape_id, version)
                    );"))
      # shapes_line - index
      dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry_historisk ON {schema_name}_historisk.shapes_line USING GIST (geometry);"))
    }
    
    # trips
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.trips (
                      trip_id VARCHAR,
                      route_id VARCHAR,
                      service_id VARCHAR NOT NULL,
                      trip_headsign VARCHAR,
                      direction_id INTEGER,
                      shape_id VARCHAR,
                      version INTEGER,
                      PRIMARY KEY (trip_id, version),
                      FOREIGN KEY (route_id, version) REFERENCES {schema_name}_historisk.routes(route_id, version)
                  );"))
    # trips - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_route_id_historisk ON {schema_name}_historisk.trips (route_id, version);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_shape_id_historisk ON {schema_name}_historisk.trips (shape_id);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_trips_service_id_historisk ON {schema_name}_historisk.trips (service_id);"))
    
    # stops
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.stops (
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
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stops_geometry_historisk ON {schema_name}_historisk.stops USING GIST (geometry);"))
    
    # stop_times
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.stop_times (
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
                      FOREIGN KEY (trip_id, version) REFERENCES {schema_name}_historisk.trips(trip_id, version),
                      FOREIGN KEY (stop_id, version) REFERENCES {schema_name}_historisk.stops(stop_id, version)
                  );"))
    # stop_times - index
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id_historisk ON {schema_name}_historisk.stop_times (trip_id, version);"))
    dbExecute(con, glue("CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id_historisk ON {schema_name}_historisk.stop_times (stop_id, version);"))
    
    # Linjeklassificering
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.linjeklassificering (
                    route_short_name VARCHAR,
                    klassificering VARCHAR NOT NULL,
                    version INTEGER,
                    PRIMARY KEY (route_short_name, version)
                  );"))
    
    # Versions table
    dbExecute(con, glue("CREATE TABLE IF NOT EXISTS {schema_name}_historisk.versions (
                      version INTEGER PRIMARY KEY,
                      start_date DATE,
                      end_date DATE
                  );"))
  }, error = function(e){
    stop(glue("Ett fel inträffade vid skapandet av tabeller: {e$message}"))
  })
}
