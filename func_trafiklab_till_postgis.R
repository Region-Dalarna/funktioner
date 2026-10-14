# =============================================================================
# func_trafiklab_till_postgis.R  (paketversion)
# =============================================================================
# Förutsättningar för paketet:
#   - R >= 4.1, Imports: DBI, glue, sf, dplyr (+ func_gtfs.R:s imports)
#   - DESCRIPTION: Encoding: UTF-8 (filen innehåller å/ä/ö i SQL-strängar)
#   - Inga globala variabler: schema anges som argument (default "gtfs").
#     Tidigare användes den globala variabeln schema_namn som default och på
#     flera ställen hårdkodat i SQL, vilket inte fungerar i ett paket.
# =============================================================================


# ================================ hjälpfunktioner ============================

gtfs_historiskt_schema <- function(schema) paste0(schema, "_historisk")

# Kör `kod` i en transaktion, men bara om anroparen inte redan har öppnat en.
# RPostgres stödjer inte nästlade transaktioner (DBI::dbWithTransaction ger då
# "Nested transactions not supported"), så om dbBegin() misslyckas antar vi att
# anroparen äger transaktionen och kör koden i den. Anroparen sköter då själv
# commit/rollback. `kod` utvärderas lazy, först efter att transaktionen öppnats.
gtfs_transaktion <- function(con, kod) {
  egen <- tryCatch({ DBI::dbBegin(con); TRUE }, error = function(e) FALSE)
  if (!egen) return(kod)

  tryCatch({
    resultat <- kod
    DBI::dbCommit(con)
    resultat
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    stop(e)
  })
}

# Valfria tabeller (sweden_3) som finns i både aktuellt och historiskt schema
gtfs_valfria_tabeller_finns <- function(con, schema, schema_h) {
  valfria <- c("transfers", "attributions", "feed_info")
  finns <- vapply(valfria, function(t) {
    DBI::dbExistsTable(con, DBI::Id(schema = schema,   table = t)) &&
      DBI::dbExistsTable(con, DBI::Id(schema = schema_h, table = t))
  }, logical(1))
  valfria[finns]
}

# GTFS route_type -> läsbar fordonstyp. Trafiklabs feeds använder utökade
# (HVT-)koder: 100-serien = tåg, 401 = tunnelbana, 700-serien = buss (inkl. 714
# tågersättningsbuss), 900 = spårvagn, 1000 = färja, 1501 = förbeställningstrafik.
# Standardkoderna 0-4 tas med som reserv. Tabellalias i SQL måste vara "r".
# Kontrollera vid behov med:
#   SELECT route_type, COUNT(*) FROM gtfs.routes GROUP BY route_type;
gtfs_fordonstyp_sql <- function() {
  "CASE
     WHEN r.route_type BETWEEN 100 AND 199 OR r.route_type = 2 THEN 'Tåg'
     WHEN r.route_type BETWEEN 400 AND 499 OR r.route_type = 1 THEN 'Tunnelbana'
     WHEN r.route_type BETWEEN 200 AND 299 OR r.route_type BETWEEN 700 AND 799 OR r.route_type = 3 THEN 'Buss'
     WHEN r.route_type BETWEEN 900 AND 999 OR r.route_type = 0 THEN 'Spårvagn'
     WHEN r.route_type BETWEEN 1000 AND 1099 OR r.route_type = 4 THEN 'Färja'
     WHEN r.route_type BETWEEN 1500 AND 1599 THEN 'Förbeställningstrafik'
     ELSE 'Övrigt'
   END"
}

# Gemensam hantering av agency_id/agency_namn/vy_suffix för alla vyfunktioner.
# agency_namn slås upp mot agency-tabellen i grundschemat (inte _historisk).
gtfs_vy_parametrar <- function(con, schema, agency_id, agency_namn, vy_suffix) {
  if (!is.null(agency_namn)) {
    if (!is.null(agency_id)) stop("Ange antingen agency_id eller agency_namn, inte båda.", call. = FALSE)
    agency_id <- hamta_agency_id(con, schema, agency_namn)
  }
  if (!is.null(agency_id) && is.null(vy_suffix)) {
    # suffixet blir en del av vynamnet - rensa bort tecken som kräver citering
    vy_suffix <- paste0("_", gsub("[^a-z0-9_]", "_", tolower(agency_id)))
  }
  if (is.null(vy_suffix)) vy_suffix <- ""

  agency_filter <- if (!is.null(agency_id)) {
    glue::glue("AND r.agency_id = {DBI::dbQuoteString(con, agency_id)}")
  } else {
    ""
  }
  list(vy_suffix = vy_suffix, agency_filter = agency_filter)
}

# Spatialt index på en materialiserad vy (snabbar upp visning i QGIS/Origo)
gtfs_skapa_geomindex <- function(con, schema, vy) {
  DBI::dbExecute(con, glue::glue("CREATE INDEX idx_{vy}_geom ON {schema}.{vy} USING GIST (geometry);"))
}

# Begränsar en dataframe/sf till de kolumner som finns i databastabellen.
# Källdatan kan innehålla extra kolumner (t.ex. stop_code, route_color) som
# annars får append att krascha med "column does not exist".
gtfs_begransa_till_tabell <- function(con, schema, tabell, df) {
  tabellkolumner <- DBI::dbListFields(con, DBI::Id(schema = schema, table = tabell))
  saknas <- setdiff(names(df), tabellkolumner)
  if (length(saknas) > 0) {
    message(glue::glue("{tabell}: hoppar över kolumner som saknas i databastabellen: {paste(saknas, collapse = ', ')}"))
    df <- df[, setdiff(names(df), saknas), drop = FALSE]
  }
  df
}


# ================================ tabeller ===================================

skapa_tabeller <- function(con, schema = "gtfs", gtfs_dataset = "regional") {
  # Skapar databastabeller för GTFS-data i både aktuellt schema och historikschema.
  #
  # gtfs_dataset = "regional" (default) → grundstruktur
  # gtfs_dataset = "sweden_3"           → lägger till trips-kolumner
  #                                        (trip_short_name, samtrafiken_internal_trip_number)
  #                                        samt tabellerna transfers, attributions, feed_info
  #
  # Tabeller skapas med CREATE TABLE IF NOT EXISTS - säkert att köra flera gånger.
  # Allt körs i en transaktion så att en krasch inte lämnar en halv uppsättning tabeller.

  sweden_3 <- gtfs_kontrollera_dataset(gtfs_dataset)
  h <- gtfs_historiskt_schema(schema)
  kor <- function(sql) DBI::dbExecute(con, glue::glue(sql))

  tryCatch({
    gtfs_transaktion(con, {

      kor("CREATE SCHEMA IF NOT EXISTS {schema};")
      kor("CREATE SCHEMA IF NOT EXISTS {h};")

      # ════════════ AKTUELLA TABELLER ════════════

      kor("CREATE TABLE IF NOT EXISTS {schema}.agency (
             agency_id VARCHAR PRIMARY KEY,
             agency_name VARCHAR NOT NULL,
             agency_url VARCHAR NOT NULL,
             agency_timezone VARCHAR NOT NULL,
             agency_lang VARCHAR,
             agency_fare_url VARCHAR
           );")

      kor("CREATE TABLE IF NOT EXISTS {schema}.routes (
             route_id VARCHAR PRIMARY KEY,
             agency_id VARCHAR REFERENCES {schema}.agency(agency_id),
             route_short_name VARCHAR NOT NULL,
             route_long_name VARCHAR NOT NULL,
             route_desc VARCHAR,
             route_type INTEGER NOT NULL
           );")
      kor("CREATE INDEX IF NOT EXISTS idx_routes_route_short_name ON {schema}.routes (route_short_name);")

      kor("CREATE TABLE IF NOT EXISTS {schema}.calendar (
             service_id VARCHAR PRIMARY KEY,
             monday INTEGER, tuesday INTEGER, wednesday INTEGER, thursday INTEGER,
             friday INTEGER, saturday INTEGER, sunday INTEGER,
             start_date DATE,
             end_date DATE
           );")

      kor("CREATE TABLE IF NOT EXISTS {schema}.calendar_dates (
             service_id VARCHAR,
             date DATE,
             exception_type INTEGER,
             PRIMARY KEY (service_id, date)
           );")

      kor("CREATE TABLE IF NOT EXISTS {schema}.shapes_line (
             shape_id VARCHAR PRIMARY KEY,
             geometry GEOMETRY(Linestring, 3006),
             antal_punkter INTEGER,
             max_dist FLOAT
           );")
      kor("CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry ON {schema}.shapes_line USING GIST (geometry);")

      kor("CREATE TABLE IF NOT EXISTS {schema}.shapes (
             shape_id VARCHAR,
             shape_pt_lat FLOAT,
             shape_pt_lon FLOAT,
             shape_pt_sequence INTEGER,
             shape_dist_traveled FLOAT,
             PRIMARY KEY (shape_id, shape_pt_sequence)
           );")
      kor("CREATE INDEX IF NOT EXISTS idx_shapes_shape_id ON {schema}.shapes (shape_id);")

      # trips - sweden_3 har extra kolumner
      if (sweden_3) {
        kor("CREATE TABLE IF NOT EXISTS {schema}.trips (
               trip_id VARCHAR PRIMARY KEY,
               route_id VARCHAR REFERENCES {schema}.routes(route_id),
               service_id VARCHAR NOT NULL,
               trip_headsign VARCHAR,
               trip_short_name VARCHAR,
               direction_id INTEGER,
               shape_id VARCHAR,
               samtrafiken_internal_trip_number VARCHAR
             );")
      } else {
        kor("CREATE TABLE IF NOT EXISTS {schema}.trips (
               trip_id VARCHAR PRIMARY KEY,
               route_id VARCHAR REFERENCES {schema}.routes(route_id),
               service_id VARCHAR NOT NULL,
               trip_headsign VARCHAR,
               direction_id INTEGER,
               shape_id VARCHAR
             );")
      }
      kor("CREATE INDEX IF NOT EXISTS idx_trips_shape_id ON {schema}.trips (shape_id);")
      kor("CREATE INDEX IF NOT EXISTS idx_trips_route_id ON {schema}.trips (route_id);")
      kor("CREATE INDEX IF NOT EXISTS idx_trips_service_id ON {schema}.trips (service_id);")

      kor("CREATE TABLE IF NOT EXISTS {schema}.stops (
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
      kor("CREATE INDEX IF NOT EXISTS idx_stops_geometry ON {schema}.stops USING GIST (geometry);")

      # arrival_time/departure_time är VARCHAR eftersom GTFS tillåter tider över 24:00
      kor("CREATE TABLE IF NOT EXISTS {schema}.stop_times (
             trip_id VARCHAR,
             arrival_time VARCHAR,
             departure_time VARCHAR,
             stop_id VARCHAR,
             stop_sequence INTEGER,
             stop_headsign VARCHAR,
             pickup_type INTEGER,
             drop_off_type INTEGER,
             pickup_booking_rule_id VARCHAR,
             drop_off_booking_rule_id VARCHAR,
             shape_dist_traveled FLOAT,
             timepoint INTEGER,
             PRIMARY KEY (trip_id, stop_id, stop_sequence),
             FOREIGN KEY (stop_id) REFERENCES {schema}.stops(stop_id),
             FOREIGN KEY (trip_id) REFERENCES {schema}.trips(trip_id)
           );")
      kor("CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id ON {schema}.stop_times (trip_id);")
      kor("CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id ON {schema}.stop_times (stop_id);")

      kor("CREATE TABLE IF NOT EXISTS {schema}.linjeklassificering (
             route_short_name VARCHAR PRIMARY KEY,
             klassificering VARCHAR NOT NULL
           );")

      if (sweden_3) {
        kor("CREATE TABLE IF NOT EXISTS {schema}.transfers (
               from_stop_id VARCHAR, to_stop_id VARCHAR,
               transfer_type INTEGER, min_transfer_time INTEGER,
               from_trip_id VARCHAR, to_trip_id VARCHAR
             );")
        kor("CREATE INDEX IF NOT EXISTS idx_transfers_from_stop ON {schema}.transfers (from_stop_id);")
        kor("CREATE INDEX IF NOT EXISTS idx_transfers_to_stop ON {schema}.transfers (to_stop_id);")

        kor("CREATE TABLE IF NOT EXISTS {schema}.attributions (
               trip_id VARCHAR,
               organization_name VARCHAR NOT NULL,
               is_operator INTEGER
             );")
        kor("CREATE INDEX IF NOT EXISTS idx_attributions_trip_id ON {schema}.attributions (trip_id);")

        kor("CREATE TABLE IF NOT EXISTS {schema}.feed_info (
               feed_id VARCHAR PRIMARY KEY,
               feed_publisher_name VARCHAR,
               feed_publisher_url VARCHAR,
               feed_lang VARCHAR,
               feed_version VARCHAR
             );")
      }

      # ════════════ HISTORISKA TABELLER ════════════
      # (använder nu schema-argumentet - tidigare hårdkodat schema_namn)

      kor("CREATE TABLE IF NOT EXISTS {h}.agency (
             agency_id VARCHAR,
             agency_name VARCHAR NOT NULL,
             agency_url VARCHAR NOT NULL,
             agency_timezone VARCHAR NOT NULL,
             agency_lang VARCHAR,
             agency_fare_url VARCHAR,
             version INTEGER,
             PRIMARY KEY (agency_id, version)
           );")

      kor("CREATE TABLE IF NOT EXISTS {h}.routes (
             route_id VARCHAR,
             agency_id VARCHAR,
             route_short_name VARCHAR NOT NULL,
             route_long_name VARCHAR NOT NULL,
             route_desc VARCHAR,
             route_type INTEGER NOT NULL,
             version INTEGER,
             PRIMARY KEY (route_id, version),
             FOREIGN KEY (agency_id, version) REFERENCES {h}.agency(agency_id, version)
           );")

      kor("CREATE TABLE IF NOT EXISTS {h}.calendar (
             service_id VARCHAR,
             monday INTEGER, tuesday INTEGER, wednesday INTEGER, thursday INTEGER,
             friday INTEGER, saturday INTEGER, sunday INTEGER,
             start_date DATE,
             end_date DATE,
             version INTEGER,
             PRIMARY KEY (service_id, version)
           );")

      kor("CREATE TABLE IF NOT EXISTS {h}.calendar_dates (
             service_id VARCHAR,
             date DATE,
             exception_type INTEGER,
             version INTEGER,
             PRIMARY KEY (service_id, version, date)
           );")

      kor("CREATE TABLE IF NOT EXISTS {h}.shapes_line (
             shape_id VARCHAR,
             geometry GEOMETRY(Linestring, 3006),
             antal_punkter INTEGER,
             max_dist FLOAT,
             version INTEGER,
             PRIMARY KEY (shape_id, version)
           );")
      kor("CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry_historisk ON {h}.shapes_line USING GIST (geometry);")

      if (sweden_3) {
        kor("CREATE TABLE IF NOT EXISTS {h}.trips (
               trip_id VARCHAR,
               route_id VARCHAR,
               service_id VARCHAR NOT NULL,
               trip_headsign VARCHAR,
               trip_short_name VARCHAR,
               direction_id INTEGER,
               shape_id VARCHAR,
               samtrafiken_internal_trip_number VARCHAR,
               version INTEGER,
               PRIMARY KEY (trip_id, version),
               FOREIGN KEY (route_id, version) REFERENCES {h}.routes(route_id, version)
             );")
      } else {
        kor("CREATE TABLE IF NOT EXISTS {h}.trips (
               trip_id VARCHAR,
               route_id VARCHAR,
               service_id VARCHAR NOT NULL,
               trip_headsign VARCHAR,
               direction_id INTEGER,
               shape_id VARCHAR,
               version INTEGER,
               PRIMARY KEY (trip_id, version),
               FOREIGN KEY (route_id, version) REFERENCES {h}.routes(route_id, version)
             );")
      }
      kor("CREATE INDEX IF NOT EXISTS idx_trips_route_id_historisk ON {h}.trips (route_id, version);")
      kor("CREATE INDEX IF NOT EXISTS idx_trips_shape_id_historisk ON {h}.trips (shape_id);")
      kor("CREATE INDEX IF NOT EXISTS idx_trips_service_id_historisk ON {h}.trips (service_id);")

      kor("CREATE TABLE IF NOT EXISTS {h}.stops (
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
      kor("CREATE INDEX IF NOT EXISTS idx_stops_geometry_historisk ON {h}.stops USING GIST (geometry);")

      kor("CREATE TABLE IF NOT EXISTS {h}.stop_times (
             trip_id VARCHAR,
             arrival_time VARCHAR,
             departure_time VARCHAR,
             stop_id VARCHAR,
             stop_sequence INTEGER,
             stop_headsign VARCHAR,
             pickup_type INTEGER,
             drop_off_type INTEGER,
             shape_dist_traveled FLOAT,
             pickup_booking_rule_id VARCHAR,
             drop_off_booking_rule_id VARCHAR,
             timepoint INTEGER,
             version INTEGER,
             PRIMARY KEY (trip_id, version, stop_sequence),
             FOREIGN KEY (trip_id, version) REFERENCES {h}.trips(trip_id, version),
             FOREIGN KEY (stop_id, version) REFERENCES {h}.stops(stop_id, version)
           );")
      kor("CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id_historisk ON {h}.stop_times (trip_id, version);")
      kor("CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id_historisk ON {h}.stop_times (stop_id, version);")

      kor("CREATE TABLE IF NOT EXISTS {h}.linjeklassificering (
             route_short_name VARCHAR,
             klassificering VARCHAR NOT NULL,
             version INTEGER,
             PRIMARY KEY (route_short_name, version)
           );")

      kor("CREATE TABLE IF NOT EXISTS {h}.versions (
             version INTEGER PRIMARY KEY,
             start_date DATE,
             end_date DATE
           );")

      if (sweden_3) {
        kor("CREATE TABLE IF NOT EXISTS {h}.transfers (
               from_stop_id VARCHAR, to_stop_id VARCHAR,
               transfer_type INTEGER, min_transfer_time INTEGER,
               from_trip_id VARCHAR, to_trip_id VARCHAR,
               version INTEGER
             );")
        kor("CREATE INDEX IF NOT EXISTS idx_transfers_from_stop_historisk ON {h}.transfers (from_stop_id, version);")

        kor("CREATE TABLE IF NOT EXISTS {h}.attributions (
               trip_id VARCHAR,
               organization_name VARCHAR NOT NULL,
               is_operator INTEGER,
               version INTEGER
             );")
        kor("CREATE INDEX IF NOT EXISTS idx_attributions_trip_id_historisk ON {h}.attributions (trip_id, version);")

        kor("CREATE TABLE IF NOT EXISTS {h}.feed_info (
               feed_id VARCHAR,
               feed_publisher_name VARCHAR,
               feed_publisher_url VARCHAR,
               feed_lang VARCHAR,
               feed_version VARCHAR,
               version INTEGER,
               PRIMARY KEY (feed_id, version)
             );")
      }
    })
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid skapandet av tabeller: {conditionMessage(e)}"), call. = FALSE)
  })

  invisible(TRUE)
}


synka_historisk_kolumner <- function(con, schema, schema_historisk, tabell) {
  # Ser till att den historiska tabellen har alla kolumner som källtabellen har.
  # Saknade kolumner läggs till automatiskt med samma datatyp som källan.

  kall_typer <- DBI::dbGetQuery(con, glue::glue(
    "SELECT column_name, data_type
     FROM information_schema.columns
     WHERE table_schema = '{schema}' AND table_name = '{tabell}';"
  ))

  target_kolumner <- DBI::dbListFields(con, DBI::Id(schema = schema_historisk, table = tabell))
  saknade <- kall_typer[!kall_typer$column_name %in% target_kolumner, ]

  for (i in seq_len(nrow(saknade))) {
    DBI::dbExecute(con, glue::glue(
      "ALTER TABLE {schema_historisk}.{tabell} ADD COLUMN {saknade$column_name[i]} {saknade$data_type[i]};"))
    message(glue::glue("Lade till ny kolumn i {schema_historisk}.{tabell}: {saknade$column_name[i]} ({saknade$data_type[i]})"))
  }

  invisible(TRUE)
}


hamta_gemensamma_kolumner <- function(con, schema, schema_historisk, tabell) {
  # Hämtar de kolumner som finns i både aktuell och historisk tabell (utom version)

  kall_kolumner   <- DBI::dbListFields(con, DBI::Id(schema = schema, table = tabell))
  target_kolumner <- DBI::dbListFields(con, DBI::Id(schema = schema_historisk, table = tabell))

  gemensamma <- setdiff(intersect(kall_kolumner, target_kolumner), "version")
  if (length(gemensamma) == 0) stop(glue::glue("Inga gemensamma kolumner hittades för tabellen {tabell}"))
  gemensamma
}


versionshantering <- function(con, gtfs_data, schema = "gtfs", vy_operatorer = NA) {
  # Körs INNAN ladda_upp_till_databas(). Om slutdatumet i det nya datasetet skiljer
  # sig från det som ligger i databasen arkiveras den aktuella datan till
  # {schema}_historisk med ett nytt versionsnummer.
  #
  # vy_operatorer: styr vilka historiska vyer som byggs vid en versionsövergång.
  #   NA = vyer för samtliga operatörer samlade. Övriga element = operatörsnamn
  #   som ska få EGNA vyer (delsträng, case-okänsligt). NULL = inga vyer.
  #
  # Returnerar TRUE (osynligt) om en ny version skapades, annars FALSE.

  schema_h <- gtfs_historiskt_schema(schema)

  tryCatch({
    DBI::dbExecute(con, glue::glue("
      CREATE TABLE IF NOT EXISTS {schema_h}.versions (
        version INTEGER PRIMARY KEY,
        start_date DATE,
        end_date DATE
      );"))

    # Senaste datum i databasen respektive i det nya datasetet
    sista_datum_db <- as.Date(DBI::dbGetQuery(con, glue::glue(
      "SELECT MAX(date) AS sista_datum FROM {schema}.calendar_dates;"))$sista_datum[1])
    sista_datum_gtfs_data <- max(gtfs_text_till_datum(gtfs_data$calendar_dates$date))

    # Samma slutdatum = samma version, inget att göra
    if (!is.na(sista_datum_db) && sista_datum_db == sista_datum_gtfs_data) {
      return(invisible(FALSE))
    }

    senaste_version <- DBI::dbGetQuery(con, glue::glue(
      "SELECT MAX(version) AS senaste_version FROM {schema_h}.versions;"))$senaste_version[1]
    ny_version <- if (is.na(senaste_version)) 1L else as.integer(senaste_version) + 1L

    start_datum_gtfs_data <- if (is.na(sista_datum_db)) {
      min(gtfs_text_till_datum(gtfs_data$calendar_dates$date))
    } else {
      sista_datum_db + 1
    }

    tabeller <- c("agency", "routes", "calendar", "calendar_dates", "shapes_line",
                  "stops", "trips", "stop_times", "linjeklassificering",
                  gtfs_valfria_tabeller_finns(con, schema, schema_h))

    # Arkivering och ny versionsrad sker i EN transaktion - kraschar något rullas
    # allt tillbaka och körningen kan göras om utan dubblettnycklar.
    gtfs_transaktion(con, {

      if (!is.na(senaste_version)) {

        # Idempotenskoll: finns senaste_version redan arkiverad i agency?
        # (t.ex. om versions-tabellen kommit ur synk med historiktabellerna)
        agency_finns <- DBI::dbGetQuery(con, glue::glue(
          "SELECT EXISTS (SELECT 1 FROM {schema_h}.agency WHERE version = {senaste_version});"))$exists[1]

        if (agency_finns) {
          warning(glue::glue(
            "Version {senaste_version} verkar redan vara arkiverad i {schema_h} ",
            "(hittade rader i agency med version = {senaste_version}). ",
            "Hoppar över arkiveringssteget men fortsätter skapa ny version {ny_version}."
          ))
        } else {
          DBI::dbExecute(con, glue::glue(
            "UPDATE {schema_h}.versions SET end_date = '{sista_datum_db}' WHERE version = {senaste_version};"))

          # Ordningen i `tabeller` följer främmande nycklar (agency före routes osv.)
          for (tabell in tabeller) {
            synka_historisk_kolumner(con, schema, schema_h, tabell)
            kolumn_lista <- paste(hamta_gemensamma_kolumner(con, schema, schema_h, tabell), collapse = ", ")
            DBI::dbExecute(con, glue::glue(
              "INSERT INTO {schema_h}.{tabell} ({kolumn_lista}, version)
               SELECT {kolumn_lista}, {senaste_version} FROM {schema}.{tabell}
               ON CONFLICT DO NOTHING;"))
          }
        }
      }

      DBI::dbExecute(con, glue::glue(
        "INSERT INTO {schema_h}.versions (version, start_date) VALUES ({ny_version}, '{start_datum_gtfs_data}')
         ON CONFLICT (version) DO NOTHING;"))
    })

    # Historiska vyer byggs först när det finns arkiverad data (och efter commit)
    if (!is.na(senaste_version)) {
      for (op in vy_operatorer) {
        if (is.na(op)) {
          skapa_vyer_historisk_hallplats(con, schema = schema)
          skapa_vyer_historisk_linjer(con, schema = schema)
        } else {
          skapa_vyer_historisk_hallplats(con, schema = schema, agency_namn = op)
          skapa_vyer_historisk_linjer(con, schema = schema, agency_namn = op)
        }
      }
    }

    invisible(TRUE)

  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid versionshanteringen: {conditionMessage(e)}"), call. = FALSE)
  })
}


radera_gamla_versioner <- function(con, antal_ar, schema = "gtfs") {
  # Raderar historiska versioner vars slutdatum är äldre än antal_ar år.
  # Returnerar (osynligt) vektorn med raderade versionsnummer.

  schema_h <- gtfs_historiskt_schema(schema)

  tryCatch({
    grans <- Sys.Date() - round(antal_ar * 365)
    gamla_versioner <- DBI::dbGetQuery(con, glue::glue(
      "SELECT version FROM {schema_h}.versions WHERE end_date < '{grans}';"))$version

    if (length(gamla_versioner) == 0) return(invisible(integer(0)))
    versioner <- paste(gamla_versioner, collapse = ", ")

    # Ordning som respekterar främmande nycklar. "calendar" saknades tidigare i
    # raderingen, så historiska calendar-rader ackumulerades för alltid.
    tabeller <- c(gtfs_valfria_tabeller_finns(con, schema, schema_h),
                  "stop_times", "trips", "stops", "shapes_line", "calendar_dates",
                  "calendar", "routes", "agency", "linjeklassificering")

    gtfs_transaktion(con, {
      for (tabell in tabeller) {
        DBI::dbExecute(con, glue::glue("DELETE FROM {schema_h}.{tabell} WHERE version IN ({versioner});"))
      }
      DBI::dbExecute(con, glue::glue("DELETE FROM {schema_h}.versions WHERE version IN ({versioner});"))
    })

    invisible(gamla_versioner)

  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid radering av gamla versioner: {conditionMessage(e)}"), call. = FALSE)
  })
}


ladda_upp_till_databas <- function(con, gtfs_data, schema = "gtfs",
                                   gtfs_dataset = "regional", transaktion = FALSE) {
  # Laddar upp GTFS-data till databasen.
  #
  # Truncar befintliga tabeller innan uppladdning. Stops och shapes_line laddas
  # upp som spatiala objekt (SWEREF99/EPSG:3006), övriga som vanliga tabeller.
  # Kolumner som saknas i databastabellen hoppas över (med meddelande).
  #
  # gtfs_dataset = "regional" → grundtabellerna (inkl. calendar)
  # gtfs_dataset = "sweden_3" → även shapes (punktformat), transfers, attributions, feed_info
  #
  # transaktion = TRUE kör hela truncate + uppladdning i en transaktion, så att en
  #   krasch mitt i inte lämnar databasen tom. Testa mot din sf/RPostgres-version
  #   innan du slår på det som standard (sf kan öppna egna transaktioner).

  sweden_3 <- gtfs_kontrollera_dataset(gtfs_dataset)

  tabeller_att_truncate <- c("stop_times", "trips", "routes", "agency", "stops",
                             "shapes_line", "calendar", "calendar_dates")
  tabeller_bas <- c("agency", "routes", "calendar", "calendar_dates", "trips", "stop_times")
  if (sweden_3) {
    tabeller_att_truncate <- c(tabeller_att_truncate, "shapes", "transfers", "attributions", "feed_info")
    tabeller_bas          <- c(tabeller_bas, "shapes", "transfers", "attributions", "feed_info")
  }

  jobb <- function() {
    # En enda TRUNCATE för alla tabeller
    DBI::dbExecute(con, glue::glue(
      "TRUNCATE TABLE {paste0(schema, '.', tabeller_att_truncate, collapse = ', ')} RESTART IDENTITY CASCADE;"))
    message("Tabeller truncade.")

    # ── Stops (spatial) ────────────────────────────────────────────────────
    if (!is.null(gtfs_data$stops)) {
      sf_stops <- sf::st_as_sf(as.data.frame(gtfs_data$stops),
                               coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) |>
        sf::st_transform(3006)
      sf_stops <- gtfs_begransa_till_tabell(con, schema, "stops", sf_stops)
      sf::st_write(obj = sf_stops, dsn = con, DBI::Id(schema = schema, table = "stops"),
                   geomtype = "POINT", delete_layer = FALSE, append = TRUE)
      rm(sf_stops)
      message("stops uppladdad.")
    }

    # ── Shapes (spatial) ───────────────────────────────────────────────────
    # Linjerna byggs direkt från sorterade koordinatmatriser (gtfs_shapes_till_linjer)
    # istället för group_by/summarise på miljontals sf-punkter - mycket snabbare,
    # och sorteringen på shape_pt_sequence garanterar rätt linjeföljd.
    if (!is.null(gtfs_data$shapes)) {
      sf_shapes_line <- gtfs_shapes_till_linjer(gtfs_data$shapes, crs = 4326) |>
        sf::st_transform(3006)
      sf::st_write(obj = sf_shapes_line, dsn = con, DBI::Id(schema = schema, table = "shapes_line"),
                   geomtype = "LINESTRING", delete_layer = FALSE, append = TRUE)
      rm(sf_shapes_line)
      message("shapes_line uppladdad.")
    }

    # ── Icke-spatiala tabeller ─────────────────────────────────────────────
    for (tabell in tabeller_bas) {
      if (!is.null(gtfs_data[[tabell]])) {
        df <- gtfs_begransa_till_tabell(con, schema, tabell, as.data.frame(gtfs_data[[tabell]]))
        DBI::dbWriteTable(con, DBI::Id(schema = schema, table = tabell), df,
                          append = TRUE, row.names = FALSE)
        message(glue::glue("{tabell} uppladdad."))
      } else {
        message(glue::glue("{tabell} saknas i gtfs_data — hoppas över."))
      }
    }
  }

  tryCatch({
    if (transaktion) gtfs_transaktion(con, jobb()) else jobb()
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid uppladdningen av data till databasen:", conditionMessage(e)), call. = FALSE)
  })

  invisible(TRUE)
}


skapa_tabell_linjeklassificering <- function(con, schema = "gtfs") {
  # OBS: klassificeringen är Dalatrafik-specifik (hårdkodade linjenummerintervall)
  # och slår ihop operatörer med samma route_short_name (DISTINCT ON route_short_name).

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("TRUNCATE {schema}.linjeklassificering;"))

      # Regexen begränsar till max 6 siffror så att CAST AS INTEGER inte kan
      # ge overflow på orimligt långa linjenummer. {{ }} är glue-escape för { }.
      DBI::dbExecute(con, glue::glue("
        INSERT INTO {schema}.linjeklassificering (route_short_name, klassificering)
        SELECT DISTINCT ON (route_short_name) route_short_name,
               CASE
                 WHEN route_short_name ~ '^[0-9]{{1,6}}$' THEN
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
    })
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av linjeklassificeringen:", conditionMessage(e)), call. = FALSE)
  })

  invisible(TRUE)
}


hamta_agency_id <- function(con, schema = "gtfs", agency_namn) {
  # Slår upp agency_id från agency_name i GTFS agency-tabellen.
  # Matchar case-okänsligt på delsträng (ILIKE '%namn%'). Kastar fel om ingen
  # eller flera träffar hittas, så man inte filtrerar på fel/ingen operatör.

  traff <- DBI::dbGetQuery(con, glue::glue("
    SELECT agency_id::text AS agency_id, agency_name
    FROM {schema}.agency
    WHERE agency_name ILIKE {DBI::dbQuoteString(con, paste0('%', agency_namn, '%'))}
  "))

  if (nrow(traff) == 0) {
    stop(glue::glue("Ingen operatör matchande '{agency_namn}' hittades i {schema}.agency."), call. = FALSE)
  }
  if (nrow(traff) > 1) {
    stop(glue::glue("Flera operatörer matchar '{agency_namn}': {paste(traff$agency_name, collapse = ', ')}. ",
                    "Skriv en mer specifik del av namnet."), call. = FALSE)
  }

  message(glue::glue("Matchade '{agency_namn}' -> agency_id = '{traff$agency_id}' ({traff$agency_name})"))
  traff$agency_id
}


# ================================ vyer =======================================
# Prestandaprincip i alla vyer: antal avgångar räknas först per
# (hållplats/linje, service_id) och joinas SEDAN mot calendar_dates. Tidigare
# joinades stop_times/trips mot calendar_dates direkt (turer x datum), vilket ger
# en enorm mellanresultatmängd. Resultatet är identiskt eftersom varje tur har
# exakt ett service_id. Alla vyer får dessutom spatialt index.
#
# Kolumnerna "operator", "fordonstyp" och "route_type" (råa GTFS-koder, t.ex.
# "106, 700") på hållplatsvyerna är kommaseparerade textsträngar (samma mönster
# som "linjer" och "linjetyper") eftersom en hållplats kan trafikeras av flera
# operatörer och fordonstyper. Textformat är enkelt att filtrera på i QGIS (LIKE).

skapa_vyer_hallplats <- function(con, schema = "gtfs", agency_id = NULL,
                                 agency_namn = NULL, vy_suffix = NULL) {
  # Om agency_id (eller agency_namn) anges filtreras allt till den operatören,
  # annars skapas vyer för samtliga operatörer. vy_suffix styr vynamnen
  # (t.ex. "_dt"); utan vy_suffix används agency_id.

  p <- gtfs_vy_parametrar(con, schema, agency_id, agency_namn, vy_suffix)
  agency_filter  <- p$agency_filter
  fordonstyp_sql <- gtfs_fordonstyp_sql()
  vy_hallplatslage <- paste0("vy_hallplatslage_avgangar", p$vy_suffix)
  vy_hallplats     <- paste0("vy_hallplats_avgangar", p$vy_suffix)

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.{vy_hallplats} CASCADE;"))
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.{vy_hallplatslage} CASCADE;"))

      # Hållplatslägen med avgångar, linjer och klassificeringar.
      # stopp_tur = unika (hållplats, tur) - DISTINCT behövs för turer som passerar
      # samma hållplats flera gånger. Veckan med flest avgångar räknas som "normalvecka".
      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {schema}.{vy_hallplatslage} AS
        WITH stopp_tur AS (
            SELECT DISTINCT st.stop_id, st.trip_id, t.route_id, t.service_id
            FROM {schema}.stop_times st
            JOIN {schema}.trips t ON st.trip_id = t.trip_id
            JOIN {schema}.routes r ON t.route_id = r.route_id
            WHERE 1 = 1
            {agency_filter}
        ),
        avgangar_per_service AS (
            SELECT stop_id, service_id, COUNT(*) AS antal_turer
            FROM stopp_tur
            GROUP BY stop_id, service_id
        ),
        dagliga_avgangar AS (
            SELECT a.stop_id, cd.date, SUM(a.antal_turer)::BIGINT AS antal_avgangar
            FROM avgangar_per_service a
            JOIN {schema}.calendar_dates cd ON a.service_id = cd.service_id
            WHERE cd.exception_type = 1
            GROUP BY a.stop_id, cd.date
        ),
        avgangar_vecka AS (
            SELECT
                stop_id,
                DATE_TRUNC('week', date) AS vecka_start,
                SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
                SUM(antal_avgangar) AS total_veckans_avgangar
            FROM dagliga_avgangar
            GROUP BY stop_id, DATE_TRUNC('week', date)
        ),
        normal_vecka AS (
            SELECT
                stop_id, vecka_start, total_veckans_avgangar, total_veckodag_avgangar,
                total_lordag_avgangar, total_sondag_avgangar,
                ROW_NUMBER() OVER (PARTITION BY stop_id ORDER BY total_veckans_avgangar DESC) AS rank
            FROM avgangar_vecka
        ),
        routes_per_stop AS (
            SELECT
                sr.stop_id,
                STRING_AGG(DISTINCT r.route_short_name::TEXT, ',') AS linjer,
                STRING_AGG(DISTINCT lc.klassificering, ',') AS linjetyper,
                STRING_AGG(DISTINCT ag.agency_name, ',') AS operator,
                STRING_AGG(DISTINCT {fordonstyp_sql}, ',') AS fordonstyp,
                STRING_AGG(DISTINCT r.route_type::TEXT, ',') AS route_type
            FROM (SELECT DISTINCT stop_id, route_id FROM stopp_tur) sr
            JOIN {schema}.routes r ON sr.route_id = r.route_id
            LEFT JOIN {schema}.linjeklassificering lc ON r.route_short_name = lc.route_short_name
            LEFT JOIN {schema}.agency ag ON r.agency_id = ag.agency_id
            GROUP BY sr.stop_id
        )
        SELECT
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
            rs.operator,
            rs.fordonstyp,
            rs.route_type,
            st.geometry
        FROM normal_vecka nv
        JOIN {schema}.stops st ON nv.stop_id = st.stop_id
        JOIN routes_per_stop rs ON st.stop_id = rs.stop_id
        WHERE nv.rank = 1;
      "))
      gtfs_skapa_geomindex(con, schema, vy_hallplatslage)

      # Hållplats = alla hållplatslägen som hör till samma parent_station.
      # LEFT JOIN: annars tappas hållplatsläget om parent_station saknas eller
      # inte matchar något stop_id i stops.
      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {schema}.{vy_hallplats} AS
        SELECT
            COALESCE(h.stop_id, nva.stop_id) AS stop_id,
            COALESCE(h.hpl_id, nva.hpl_id) AS hpl_id,
            COALESCE(h.stop_name, nva.stop_name) AS stop_name,
            SUM(nva.genomsnitt_veckodag_avgangar) AS genomsnitt_veckodag_avgangar,
            SUM(nva.antal_lordag_avgangar) AS antal_lordag_avgangar,
            SUM(nva.antal_sondag_avgangar) AS antal_sondag_avgangar,
            SUM(nva.genomsnitt_dagliga_avgangar) AS genomsnitt_dagliga_avgangar,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjer, ',' ORDER BY nva.linjer), ',')) ORDER BY unnest), ', ') AS linjer,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjetyper, ',' ORDER BY nva.linjetyper), ',')) ORDER BY unnest), ', ') AS linjetyper,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.operator, ',' ORDER BY nva.operator), ',')) ORDER BY unnest), ', ') AS operator,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.fordonstyp, ',' ORDER BY nva.fordonstyp), ',')) ORDER BY unnest), ', ') AS fordonstyp,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.route_type, ',' ORDER BY nva.route_type), ',')) ORDER BY unnest), ', ') AS route_type,
            COALESCE(h.geometry, nva.geometry) AS geometry
        FROM {schema}.{vy_hallplatslage} nva
        LEFT JOIN {schema}.stops h ON nva.parent_station = h.stop_id
        GROUP BY
            COALESCE(h.stop_id, nva.stop_id), COALESCE(h.hpl_id, nva.hpl_id),
            COALESCE(h.stop_name, nva.stop_name), COALESCE(h.geometry, nva.geometry);
      "))
      gtfs_skapa_geomindex(con, schema, vy_hallplats)
    })
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av vyer för hållplats:", conditionMessage(e)), call. = FALSE)
  })

  invisible(TRUE)
}


skapa_vyer_historisk_hallplats <- function(con, schema = "gtfs", agency_id = NULL,
                                           agency_namn = NULL, vy_suffix = NULL) {
  # Historisk motsvarighet till skapa_vyer_hallplats(). schema = grundschemat
  # ("gtfs"); vyerna skapas i {schema}_historisk. agency_id antas vara stabilt
  # över versioner.

  p <- gtfs_vy_parametrar(con, schema, agency_id, agency_namn, vy_suffix)
  agency_filter  <- p$agency_filter
  fordonstyp_sql <- gtfs_fordonstyp_sql()
  s <- gtfs_historiskt_schema(schema)   # schema för SQL nedan
  vy_hallplatslage <- paste0("vy_historisk_hallplatslage_avgangar", p$vy_suffix)
  vy_hallplats     <- paste0("vy_historisk_hallplats_avgangar", p$vy_suffix)

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {s}.{vy_hallplats} CASCADE;"))
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {s}.{vy_hallplatslage} CASCADE;"))

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {s}.{vy_hallplatslage} AS
        WITH stopp_tur AS (
            SELECT DISTINCT st.stop_id, st.trip_id, st.version, t.route_id, t.service_id
            FROM {s}.stop_times st
            JOIN {s}.trips t ON st.trip_id = t.trip_id AND st.version = t.version
            JOIN {s}.routes r ON t.route_id = r.route_id AND t.version = r.version
            WHERE 1 = 1
            {agency_filter}
        ),
        avgangar_per_service AS (
            SELECT stop_id, version, service_id, COUNT(*) AS antal_turer
            FROM stopp_tur
            GROUP BY stop_id, version, service_id
        ),
        dagliga_avgangar AS (
            SELECT a.stop_id, a.version, cd.date, SUM(a.antal_turer)::BIGINT AS antal_avgangar
            FROM avgangar_per_service a
            JOIN {s}.calendar_dates cd ON a.service_id = cd.service_id AND a.version = cd.version
            WHERE cd.exception_type = 1
            GROUP BY a.stop_id, a.version, cd.date
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
            FROM dagliga_avgangar
            GROUP BY stop_id, version, DATE_TRUNC('week', date)
        ),
        normal_vecka AS (
            SELECT
                stop_id, version, vecka_start, total_veckans_avgangar, total_veckodag_avgangar,
                total_lordag_avgangar, total_sondag_avgangar,
                ROW_NUMBER() OVER (PARTITION BY stop_id, version ORDER BY total_veckans_avgangar DESC) AS rank
            FROM avgangar_vecka
        ),
        routes_per_stop AS (
            SELECT
                sr.stop_id,
                sr.version,
                STRING_AGG(DISTINCT r.route_short_name::TEXT, ', ') AS linjer,
                STRING_AGG(DISTINCT lc.klassificering, ', ') AS linjetyper,
                STRING_AGG(DISTINCT ag.agency_name, ', ') AS operator,
                STRING_AGG(DISTINCT {fordonstyp_sql}, ', ') AS fordonstyp,
                STRING_AGG(DISTINCT r.route_type::TEXT, ', ') AS route_type
            FROM (SELECT DISTINCT stop_id, version, route_id FROM stopp_tur) sr
            JOIN {s}.routes r ON sr.route_id = r.route_id AND sr.version = r.version
            LEFT JOIN {s}.linjeklassificering lc ON r.route_short_name = lc.route_short_name AND r.version = lc.version
            LEFT JOIN {s}.agency ag ON r.agency_id = ag.agency_id AND r.version = ag.version
            GROUP BY sr.stop_id, sr.version
        )
        SELECT
            CONCAT(st.stop_id, '_', nv.version) AS unique_id,
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
            rs.operator,
            rs.fordonstyp,
            rs.route_type,
            ST_SetSRID(st.geometry, 3006) AS geometry,
            v.start_date AS startdatum,
            v.end_date AS slutdatum,
            nv.version
        FROM normal_vecka nv
        JOIN {s}.stops st ON nv.stop_id = st.stop_id AND nv.version = st.version
        JOIN routes_per_stop rs ON st.stop_id = rs.stop_id AND nv.version = rs.version
        JOIN {s}.versions v ON nv.version = v.version
        WHERE nv.rank = 1;
      "))
      gtfs_skapa_geomindex(con, s, vy_hallplatslage)

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {s}.{vy_hallplats} AS
        SELECT
            CONCAT(COALESCE(h.stop_id, nva.stop_id), '_', nva.version) AS unique_id,
            COALESCE(h.stop_id, nva.stop_id) AS stop_id,
            COALESCE(h.hpl_id, nva.hpl_id) AS hpl_id,
            COALESCE(h.stop_name, nva.stop_name) AS stop_name,
            nva.startdatum,
            nva.slutdatum,
            nva.version,
            SUM(nva.genomsnitt_veckodag_avgangar) AS genomsnitt_veckodag_avgangar,
            SUM(nva.antal_lordag_avgangar) AS antal_lordag_avgangar,
            SUM(nva.antal_sondag_avgangar) AS antal_sondag_avgangar,
            SUM(nva.genomsnitt_dagliga_avgangar) AS genomsnitt_dagliga_avgangar,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjer, ', ' ORDER BY nva.linjer), ', ')) ORDER BY unnest), ', ') AS linjer,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjetyper, ', ' ORDER BY nva.linjetyper), ', ')) ORDER BY unnest), ', ') AS linjetyper,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.operator, ', ' ORDER BY nva.operator), ', ')) ORDER BY unnest), ', ') AS operator,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.fordonstyp, ', ' ORDER BY nva.fordonstyp), ', ')) ORDER BY unnest), ', ') AS fordonstyp,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.route_type, ', ' ORDER BY nva.route_type), ', ')) ORDER BY unnest), ', ') AS route_type,
            COALESCE(ST_SetSRID(h.geometry, 3006), nva.geometry) AS geometry
        FROM {s}.{vy_hallplatslage} nva
        LEFT JOIN {s}.stops h ON nva.parent_station = h.stop_id AND nva.version = h.version
        GROUP BY
            COALESCE(h.stop_id, nva.stop_id), COALESCE(h.hpl_id, nva.hpl_id),
            COALESCE(h.stop_name, nva.stop_name), nva.startdatum, nva.slutdatum,
            COALESCE(ST_SetSRID(h.geometry, 3006), nva.geometry), nva.version;
      "))
      gtfs_skapa_geomindex(con, s, vy_hallplats)
    })

    message("Materialiserade vyer skapade framgångsrikt.")
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid skapandet av materialiserade vyer: {conditionMessage(e)}"), call. = FALSE)
  })

  invisible(TRUE)
}


skapa_vyer_linjer <- function(con, schema = "gtfs", agency_id = NULL,
                              agency_namn = NULL, vy_suffix = NULL) {
  # Kolumnen "operator" är operatörsnamnet för linjen, "route_type" den råa
  # GTFS-koden och "fordonstyp" en läsbar grupp härledd från den.
  #
  # OBS antal_turer räknas nu per (shape, route). Tidigare joinades alla turer på
  # shape_id oavsett route, så en shape som delas av flera linjer fick samma
  # (för höga) antal_turer på alla linjer.

  p <- gtfs_vy_parametrar(con, schema, agency_id, agency_namn, vy_suffix)
  agency_filter  <- p$agency_filter
  fordonstyp_sql <- gtfs_fordonstyp_sql()
  vy_alla       <- paste0("vy_linjer_avgangar_alla", p$vy_suffix)
  vy_vanligaste <- paste0("vy_linjer_avgangar_vanligaste", p$vy_suffix)

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.{vy_alla} CASCADE;"))
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.{vy_vanligaste} CASCADE;"))

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {schema}.{vy_alla} AS
        WITH linje_tur AS (
            SELECT t.trip_id, t.shape_id, t.route_id, t.service_id
            FROM {schema}.trips t
            JOIN {schema}.routes r ON t.route_id = r.route_id
            WHERE t.shape_id IS NOT NULL
            {agency_filter}
        ),
        turer_per_service AS (
            SELECT shape_id, route_id, service_id, COUNT(*) AS antal_turer
            FROM linje_tur
            GROUP BY shape_id, route_id, service_id
        ),
        dagliga_avgangar AS (
            SELECT s.shape_id, s.route_id, cd.date, SUM(s.antal_turer)::BIGINT AS antal_avgangar
            FROM turer_per_service s
            JOIN {schema}.calendar_dates cd ON s.service_id = cd.service_id
            WHERE cd.exception_type = 1
            GROUP BY s.shape_id, s.route_id, cd.date
        ),
        avgangar_vecka AS (
            SELECT
                shape_id,
                route_id,
                DATE_TRUNC('week', date) AS vecka_start,
                SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
                SUM(antal_avgangar) AS total_veckans_avgangar
            FROM dagliga_avgangar
            GROUP BY shape_id, route_id, DATE_TRUNC('week', date)
        ),
        normal_vecka AS (
            SELECT
                shape_id, route_id, vecka_start, total_veckans_avgangar, total_veckodag_avgangar,
                total_lordag_avgangar, total_sondag_avgangar,
                ROW_NUMBER() OVER (PARTITION BY shape_id, route_id ORDER BY total_veckans_avgangar DESC) AS rank
            FROM avgangar_vecka
        ),
        turer_totalt AS (
            SELECT shape_id, route_id, SUM(antal_turer)::BIGINT AS antal_turer
            FROM turer_per_service
            GROUP BY shape_id, route_id
        )
        SELECT
            sl.shape_id,
            r.route_short_name AS linjenummer,
            r.route_long_name,
            lc.klassificering,
            ag.agency_name AS operator,
            r.route_type,
            {fordonstyp_sql} AS fordonstyp,
            sl.antal_punkter,
            sl.max_dist AS max_avstand_punkter,
            sl.geometry,
            ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
            nv.total_lordag_avgangar AS antal_lordag_avgangar,
            nv.total_sondag_avgangar AS antal_sondag_avgangar,
            ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_veckans_avgangar,
            tt.antal_turer
        FROM normal_vecka nv
        JOIN turer_totalt tt ON nv.shape_id = tt.shape_id AND nv.route_id = tt.route_id
        JOIN {schema}.shapes_line sl ON nv.shape_id = sl.shape_id
        JOIN {schema}.routes r ON nv.route_id = r.route_id
        LEFT JOIN {schema}.linjeklassificering lc ON r.route_short_name = lc.route_short_name
        LEFT JOIN {schema}.agency ag ON r.agency_id = ag.agency_id
        WHERE nv.rank = 1;
      "))
      gtfs_skapa_geomindex(con, schema, vy_alla)

      # Vanligaste linjen (flest turer) per linjenummer
      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {schema}.{vy_vanligaste} AS
        WITH vanligaste_linje_per_route AS (
            SELECT
                linjenummer, klassificering, operator, route_type, fordonstyp, shape_id,
                antal_turer, genomsnitt_veckodag_avgangar, antal_lordag_avgangar,
                antal_sondag_avgangar, genomsnitt_veckans_avgangar, antal_punkter,
                max_avstand_punkter, geometry,
                ROW_NUMBER() OVER (PARTITION BY linjenummer ORDER BY antal_turer DESC) AS rank
            FROM {schema}.{vy_alla}
        )
        SELECT
            linjenummer, klassificering, operator, route_type, fordonstyp, shape_id,
            antal_turer, genomsnitt_veckodag_avgangar, antal_lordag_avgangar,
            antal_sondag_avgangar, genomsnitt_veckans_avgangar, antal_punkter,
            max_avstand_punkter, geometry
        FROM vanligaste_linje_per_route
        WHERE rank = 1;
      "))
      gtfs_skapa_geomindex(con, schema, vy_vanligaste)
    })

    message(glue::glue("Materialiserad vy '{schema}.{vy_alla}' skapad framgångsrikt."))
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av vyn:", conditionMessage(e)), call. = FALSE)
  })

  invisible(TRUE)
}


skapa_vyer_historisk_linjer <- function(con, schema = "gtfs", agency_id = NULL,
                                        agency_namn = NULL, vy_suffix = NULL) {
  # Historisk motsvarighet till skapa_vyer_linjer(). Se den för anmärkning om antal_turer.

  p <- gtfs_vy_parametrar(con, schema, agency_id, agency_namn, vy_suffix)
  agency_filter  <- p$agency_filter
  fordonstyp_sql <- gtfs_fordonstyp_sql()
  s <- gtfs_historiskt_schema(schema)
  vy_alla       <- paste0("vy_historisk_linjer_avgangar_alla", p$vy_suffix)
  vy_vanligaste <- paste0("vy_historisk_linjer_avgangar_vanligaste", p$vy_suffix)

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {s}.{vy_alla} CASCADE;"))
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {s}.{vy_vanligaste} CASCADE;"))

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {s}.{vy_alla} AS
        WITH linje_tur AS (
            SELECT t.trip_id, t.shape_id, t.route_id, t.service_id, t.version
            FROM {s}.trips t
            JOIN {s}.routes r ON t.route_id = r.route_id AND t.version = r.version
            WHERE t.shape_id IS NOT NULL
            {agency_filter}
        ),
        turer_per_service AS (
            SELECT shape_id, route_id, service_id, version, COUNT(*) AS antal_turer
            FROM linje_tur
            GROUP BY shape_id, route_id, service_id, version
        ),
        dagliga_avgangar AS (
            SELECT s.shape_id, s.route_id, s.version, cd.date, SUM(s.antal_turer)::BIGINT AS antal_avgangar
            FROM turer_per_service s
            JOIN {s}.calendar_dates cd ON s.service_id = cd.service_id AND s.version = cd.version
            WHERE cd.exception_type = 1
            GROUP BY s.shape_id, s.route_id, s.version, cd.date
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
            FROM dagliga_avgangar
            GROUP BY shape_id, route_id, version, DATE_TRUNC('week', date)
        ),
        normal_vecka AS (
            SELECT
                shape_id, route_id, version, vecka_start, total_veckans_avgangar, total_veckodag_avgangar,
                total_lordag_avgangar, total_sondag_avgangar,
                ROW_NUMBER() OVER (PARTITION BY shape_id, route_id, version ORDER BY total_veckans_avgangar DESC) AS rank
            FROM avgangar_vecka
        ),
        turer_totalt AS (
            SELECT shape_id, route_id, version, SUM(antal_turer)::BIGINT AS antal_turer
            FROM turer_per_service
            GROUP BY shape_id, route_id, version
        )
        SELECT
            CONCAT(sl.shape_id, '_', nv.version) AS unique_id,
            sl.shape_id,
            r.route_short_name AS linjenummer,
            r.route_long_name,
            lc.klassificering,
            ag.agency_name AS operator,
            r.route_type,
            {fordonstyp_sql} AS fordonstyp,
            sl.antal_punkter,
            sl.max_dist AS max_avstand_punkter,
            sl.geometry,
            ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
            nv.total_lordag_avgangar AS antal_lordag_avgangar,
            nv.total_sondag_avgangar AS antal_sondag_avgangar,
            ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_veckans_avgangar,
            tt.antal_turer,
            nv.version,
            v.start_date AS startdatum,
            v.end_date AS slutdatum
        FROM normal_vecka nv
        JOIN turer_totalt tt ON nv.shape_id = tt.shape_id AND nv.route_id = tt.route_id AND nv.version = tt.version
        JOIN {s}.shapes_line sl ON nv.shape_id = sl.shape_id AND nv.version = sl.version
        JOIN {s}.routes r ON nv.route_id = r.route_id AND nv.version = r.version
        LEFT JOIN {s}.linjeklassificering lc ON r.route_short_name = lc.route_short_name AND r.version = lc.version
        LEFT JOIN {s}.agency ag ON r.agency_id = ag.agency_id AND r.version = ag.version
        JOIN {s}.versions v ON nv.version = v.version
        WHERE nv.rank = 1;
      "))
      gtfs_skapa_geomindex(con, s, vy_alla)

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {s}.{vy_vanligaste} AS
        WITH vanligaste_linje_per_route AS (
            SELECT
                unique_id, linjenummer, klassificering, operator, route_type, fordonstyp, shape_id,
                antal_turer, genomsnitt_veckodag_avgangar, antal_lordag_avgangar,
                antal_sondag_avgangar, genomsnitt_veckans_avgangar, antal_punkter,
                max_avstand_punkter, geometry, version, startdatum, slutdatum,
                ROW_NUMBER() OVER (PARTITION BY linjenummer, version ORDER BY antal_turer DESC) AS rank
            FROM {s}.{vy_alla}
        )
        SELECT
            unique_id, linjenummer, klassificering, operator, route_type, fordonstyp, shape_id,
            antal_turer, genomsnitt_veckodag_avgangar, antal_lordag_avgangar,
            antal_sondag_avgangar, genomsnitt_veckans_avgangar, antal_punkter,
            max_avstand_punkter, geometry, version, startdatum, slutdatum
        FROM vanligaste_linje_per_route
        WHERE rank = 1;
      "))
      gtfs_skapa_geomindex(con, s, vy_vanligaste)
    })

    message("Materialiserade vyer skapade framgångsrikt.")
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid skapandet av materialiserade vyer: {conditionMessage(e)}"), call. = FALSE)
  })

  invisible(TRUE)
}
