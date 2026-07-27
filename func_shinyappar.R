library(DBI)
library(RPostgres)
library(sf)

shiny_set_password <- function(service, losenord = NULL) {
  
  # Kontrollera att service är giltigt
  if (!grepl("^[A-Za-z0-9_]+$", service)) {
    stop("Service-namnet får bara innehålla A-Z, a-z, 0-9 och '_'.")
  }
  
  varname <- paste0(service, "_PWD")
  
  home <- Sys.getenv("HOME")
  renv_file <- file.path(home, ".Renviron")
  
  existing <- if (file.exists(renv_file)) readLines(renv_file) else character()
  
  if (is.null(losenord)) {
    
    # Finns redan ett lösenord?
    existing_match <- grep(paste0("^", varname, "="), existing, value = TRUE)
    
    # fråga om man vill skriva över befintligt lösenord om det finns ett
    if (length(existing_match) > 0) {
      cat("Det finns redan ett lösenord för tjänsten '", service, "'.\n", sep = "")
      overwrite <- tolower(readline("Vill du skriva över det? (j/n): "))
      
      if (overwrite != "j") {
        cat("✔ Inget ändrat.\n")
        return(invisible(FALSE))
      }
    }
    
    
    # Läs in nytt lösenord som användaren får skriva in
    cat("Ange lösenord för tjänsten '", service, "': ", sep = "")
    password <- readline()
  } else password <- losenord                                      # om lösenord skickas in som argument så används det istället
  
  # ta bort ev. gammal rad med samma variabel
  existing <- existing[!grepl(paste0("^", varname, "="), existing)]
  
  new_content <- c(existing, paste0(varname, "=", password))
  
  writeLines(new_content, renv_file)
  
  # Sätt rättigheter på Linux/mac
  if (.Platform$OS.type == "unix") {
    system(paste("chmod 600", shQuote(renv_file)))
  }
  
  # Uppdatera miljön direkt i sessionen
  args <- setNames(list(password), varname)
  do.call(Sys.setenv, args)
  
  cat("✔ Installerat: ", varname, " i ", renv_file, "\n", sep = "")
  return(invisible(TRUE))
}


shiny_get_password <- function(service) {
  
  if (!grepl("^[A-Za-z0-9_]+$", service)) {
    stop("Service-namnet får bara innehålla A-Z, a-z, 0-9 och '_'.")
  }
  
  varname <- paste0(service, "_PWD")
  readRenviron(file.path(Sys.getenv("HOME"), ".Renviron"))
  pw <- Sys.getenv(varname, unset = NA)
  
  if (is.na(pw) || !nzchar(pw)) {
    stop("Lösenord saknas. Variabeln '", varname, "' finns inte i miljön.")
  }
  
  pw
}

shiny_delete_password <- function(service) {
  
  if (!grepl("^[A-Za-z0-9_]+$", service)) {
    stop("Service-namnet får bara innehålla A-Z, a-z, 0-9 och '_'.")
  }
  
  varname <- paste0(service, "_PWD")
  readRenviron(file.path(Sys.getenv("HOME"), ".Renviron"))
  pw <- Sys.getenv(varname, unset = NA)
  
  if (is.na(pw) || !nzchar(pw)) {
    stop("Lösenord saknas. Variabeln '", varname, "' finns inte i miljön.")
  }
  
  home <- Sys.getenv("HOME")
  renv_file <- file.path(home, ".Renviron")
  
  if (!file.exists(renv_file)) {
    stop("Filen ", renv_file, " finns inte. Inget att ta bort.\n", sep = "")
  }
  
  existing <- readLines(renv_file)
  
  new_content <- existing[!grepl(paste0("^", varname, "="), existing)]
  
  writeLines(new_content, renv_file)
  
  cat("✔ Borttaget: ", varname, " från ", renv_file, "\n", sep = "")
}

shiny_list_passwords <- function() {
  
  home <- Sys.getenv("HOME")
  renv_file <- file.path(home, ".Renviron")
  
  if (!file.exists(renv_file)) {
    stop(".Renviron-filen finns inte på denna maskin: ", renv_file)
  }
  
  lines <- readLines(renv_file)
  
  # Välj alla variabler som slutar på _PWD
  matches <- grep("^[A-Za-z0-9_]+_PWD=", lines, value = TRUE)
  
  if (length(matches) == 0) {
    stop("Inga tjänster hittades i .Renviron.\n")
  }
  
  # Extrahera service-namnen genom att ta bort _PWD=...
  services <- sub("_PWD=.*$", "", matches)
  
  cat("Tjänster med sparade lösenord:\n")
  #for (s in services) cat(" - ", s, "\n", sep = "")
  
  return(services)
}



shiny_uppkoppling_skriv <- function(
    db_name = "geodata",
    db_host = "WFALMITVS526.ltdalarna.se",
    db_port = 5432,
    db_options = "-c search_path=public",
    db_user = "shiny_skriv"
) {
  
  tryCatch({
    # Etablera anslutningen
    con <- dbConnect(          
      RPostgres::Postgres(),
      bigint = "integer",  
      user = db_user,
      password = shiny_get_password(db_user),
      host = db_host,
      port = db_port,
      dbname = db_name,
      #timezon = "UTC",
      options=db_options)
    
    
    # Returnerar anslutningen om den lyckas
    return(con)
  }, error = function(e) {
    # Skriver ut felmeddelandet och returnerar NULL
    print(paste("Ett fel inträffade vid anslutning till databasen:", e$message))
    return(NULL)
  })
}

shiny_uppkoppling_las <- function(
    
  # 0. Funktion för att koppla upp mot databasen. Kan användas med defaultvärden enligt nedan eller egna parametrar.
  # Används av andra funktioner som default om inget eget objekt med databasuppkoppling har skickats till dessa funktioner
  # OBS! Ändra default för db_name till "geodata" sen
  
  db_name = "geodata",                  
  db_host = "WFALMITVS526.ltdalarna.se",
  db_port = 5432,
  db_options = "-c search_path=public",
  db_user = "shiny_las"
) {

  tryCatch({
    # Etablera anslutningen
    con <- dbConnect(          
        RPostgres::Postgres(),
        bigint = "integer",  
        user = db_user,
        password = shiny_get_password(db_user),
        host = db_host,
        port = db_port,
        dbname = db_name,
        #timezon = "UTC",
        options=db_options)
    
    
    # Returnerar anslutningen om den lyckas
    return(con)
  }, error = function(e) {
    # Skriver ut felmeddelandet och returnerar NULL
    print(paste("Ett fel inträffade vid anslutning till databasen:", e$message))
    return(NULL)
  })
  
}

shiny_db_list <- function(
    con,
    include_views        = TRUE,                     # inkludera VIEWs
    only_with_geometry   = FALSE,                    # endast tabeller som har geometrikolumn
    schema_like          = NULL,                     # t.ex. "karta%" (ILIKE)
    table_like           = NULL,                     # t.ex. "%kommun%" (ILIKE)
    exclude_schemas      = c("pg_catalog","information_schema", "public"),
    include_rowcount_est = FALSE                     # uppskattat antal rader (snabbt)
) {
  stopifnot(DBI::dbIsValid(con))
  
  # --- Bygg WHERE-delar utan sprintf() ---
  where_clauses <- character()
  
  # Exkludera systemscheman
  if (length(exclude_schemas)) {
    excl <- paste(DBI::dbQuoteLiteral(con, exclude_schemas), collapse = ", ")
    where_clauses <- c(where_clauses, paste0("t.table_schema NOT IN (", excl, ")"))
  }
  
  # Tabelltyp
  if (isTRUE(include_views)) {
    where_clauses <- c(where_clauses, "t.table_type IN ('BASE TABLE','VIEW')")
  } else {
    where_clauses <- c(where_clauses, "t.table_type = 'BASE TABLE'")
  }
  
  # LIKE-filter för schema/tabell
  if (!is.null(schema_like)) {
    where_clauses <- c(
      where_clauses,
      paste0("t.table_schema ILIKE ", DBI::dbQuoteLiteral(con, schema_like))
    )
  }
  if (!is.null(table_like)) {
    where_clauses <- c(
      where_clauses,
      paste0("t.table_name ILIKE ", DBI::dbQuoteLiteral(con, table_like))
    )
  }
  
  # Endast tabeller med geometrikolumn (PostGIS)
  if (isTRUE(only_with_geometry)) {
    where_clauses <- c(
      where_clauses,
      paste(
        "EXISTS (",
        " SELECT 1",
        " FROM information_schema.columns c",
        " WHERE c.table_schema = t.table_schema",
        "   AND c.table_name   = t.table_name",
        "   AND c.udt_name     = 'geometry'",
        ")",
        sep = "\n"
      )
    )
  }
  
  where_sql <- paste(where_clauses, collapse = " AND ")
  if (!nzchar(where_sql)) where_sql <- "TRUE"  # fallback
  
  # Radantal-estimat via pg_catalog (valfritt)
  rowcount_cols <- ""
  rowcount_join <- ""
  if (isTRUE(include_rowcount_est)) {
    rowcount_cols <- paste(
      "",
      ", CASE",
      "    WHEN pc.reltuples IS NULL THEN NULL",
      "    ELSE GREATEST(pc.reltuples::bigint, 0)",
      "  END AS rowcount_est",
      sep = "\n"
    )
    rowcount_join <- paste(
      "LEFT JOIN pg_catalog.pg_namespace pn",
      "  ON pn.nspname = t.table_schema",
      "LEFT JOIN pg_catalog.pg_class pc",
      "  ON pc.relnamespace = pn.oid",
      " AND pc.relname      = t.table_name",
      " AND pc.relkind IN ('r','m','v')",
      sep = "\n"
    )
  }
  
  # Lista geometri-kolumner per tabell (array)
  geomname_cols <- paste(
    "",
    ", (",
    "    SELECT array_agg(c.column_name ORDER BY c.ordinal_position)",
    "    FROM information_schema.columns c",
    "    WHERE c.table_schema = t.table_schema",
    "      AND c.table_name   = t.table_name",
    "      AND c.udt_name     = 'geometry'",
    "  ) AS geometry_columns",
    sep = "\n"
  )
  
  # Slutlig SQL (byggd med paste0/paste)
  sql <- paste(
    "SELECT",
    "  t.table_schema AS schema,",
    "  t.table_name   AS table,",
    "  t.table_type   AS type",
    geomname_cols,
    rowcount_cols,
    "FROM information_schema.tables t",
    rowcount_join,
    paste("WHERE", where_sql),
    "ORDER BY t.table_schema, t.table_name;",
    sep = "\n"
  )
  
  DBI::dbGetQuery(con, sql)
}

df_till_sf <- function(df, geom_col = "geometry", crs = 3006) {
  # så att man smidigt kan jobba med dbplyr och tbl() %>% collect() %>% df_till_sf()
  # funkar inte annars då geometrikolumnen inte behåller sin geografi men finns som EWKB som kan konverteras till geometri
    
  # funktion som konverterar en df till ett sf-objekt
  # df: data.frame med en kolumn som innehåller geometri i EWKB-format
  # geom_col: namn på kolumnen som innehåller geometrin
  df_sf <- df
  df_sf[[geom_col]] <- sf::st_as_sfc(df[[geom_col]], EWKB = TRUE)
  df_sf <- sf::st_as_sf(df_sf, sf_column_name = geom_col)
  st_crs(df_sf) <- crs
  return(df_sf)
}

# ==============================================================================
# Telemetri (shiny.telemetry) - delad helper for alla appar.
#
# Skapar ett Telemetry-objekt kopplat mot shiny_telemetry-schemat i
# sekretess-databasen. app_namn behover BARA vara appens eget namn - servern
# ("_intern"/"_publik") laggs till automatiskt baserat pa hostname, sa att
# samma app-namn pa bada servrarna aldrig blandas ihop i statistiken.
#
# Vid databasfel: varnar och returnerar NULL istallet for att krascha appen -
# en tillfallig DB-storning ska aldrig hindra en app fran att starta.
# ==============================================================================

skapa_telemetry <- function(app_namn) {
  if (!requireNamespace("shiny.telemetry", quietly = TRUE)) {
    warning("Paketet shiny.telemetry ar inte installerat - statistik loggas inte.")
    return(NULL)
  }
  
  vardnamn <- Sys.info()[["nodename"]]
  server_suffix <- if (grepl("^RP0003", vardnamn)) {
    "_intern"
  } else if (grepl("^wfalmitvs978", vardnamn)) {
    "_publik"
  } else {
    warning("Okant vardnamn '", vardnamn, "' - kan inte avgora server. ",
            "Anvander app-namnet utan server-suffix.")
    ""
  }
  
  losenord <- tryCatch(shiny_get_password("shiny_skriv_telemetry"),
                       error = function(e) NA_character_)
  if (is.na(losenord)) {
    warning("Kunde inte hamta losenord for shiny_skriv_telemetry - statistik loggas inte.")
    return(NULL)
  }
  
  data_storage <- tryCatch({
    shiny.telemetry::DataStoragePostgreSQL$new(
      username = "shiny_skriv_telemetry",
      password = losenord,
      hostname = "WFALMITVS526.ltdalarna.se",
      port     = 5432,
      dbname   = "sekretess",
      driver   = "RPostgres"
    )
  }, error = function(e) {
    warning("Kunde inte ansluta telemetri-databasen: ", conditionMessage(e))
    NULL
  })
  
  if (is.null(data_storage)) return(NULL)
  
  shiny.telemetry::Telemetry$new(
    app_name     = paste0(app_namn, server_suffix),
    data_storage = data_storage
  )
}

#' Hamtar aggregerad telemetristatistik for en app.
#'
#' @param app_namn appens namn UTAN server-suffix (t.ex. "brott")
#' @param target "publik" eller "intern" - matchar server-suffixet skapa_telemetry() lagger till
#' @param fran, till Date, avgransar perioden (NULL = ingen gransning)
hamta_telemetri_data <- function(app_namn, target = c("publik", "intern"), fran = NULL, till = NULL) {
  target <- match.arg(target)
  full_namn <- paste0(app_namn, "_", target)
  
  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  villkor <- "app_name = $1"
  params <- list(full_namn)
  if (!is.null(fran)) {
    villkor <- paste(villkor, "AND time >= $2")
    params <- c(params, list(fran))
  }
  if (!is.null(till)) {
    villkor <- paste(villkor, paste0("AND time <= $", length(params) + 1))
    params <- c(params, list(till))
  }
  
  rader <- DBI::dbGetQuery(con, paste0("
    SELECT time, session, type, details
    FROM shiny_telemetry.event_log
    WHERE ", villkor, "
    ORDER BY time
  "), params = params)
  
  tom_tabell <- function(kol_namn) {
    df <- data.frame(matrix(ncol = 2, nrow = 0))
    names(df) <- kol_namn
    df
  }
  
  if (nrow(rader) == 0) {
    return(list(
      antal_sessioner       = 0,
      antal_unika_anvandare = 0,
      flikbesok             = tom_tabell(c("flik", "antal")),
      per_veckodag          = tom_tabell(c("veckodag", "antal")),
      per_timme             = tom_tabell(c("timme", "antal")),
      per_veckodag_timme    = tom_tabell(c("veckodag", "timme", "antal"))
    ))
  }
  
  # ---- Antal sessioner ----
  antal_sessioner <- length(unique(rader$session))
  
  # ---- Unika anvandare (via login-eventens anon_user_-id) ----
  login_rader <- rader[rader$type == "login", ]
  anvandar_id <- if (nrow(login_rader) > 0) {
    vapply(login_rader$details, function(d) {
      parsed <- jsonlite::fromJSON(d)
      if (!is.null(parsed$username)) parsed$username[1] else NA_character_
    }, character(1))
  } else character(0)
  antal_unika_anvandare <- length(unique(anvandar_id[!is.na(anvandar_id)]))
  
  # ---- Flikbesok (navigation-events) ----
  nav_rader <- rader[rader$type == "navigation", ]
  flikar_valid <- character(0)
  if (nrow(nav_rader) > 0) {
    flikar <- vapply(nav_rader$details, function(d) {
      parsed <- jsonlite::fromJSON(d)
      if (!is.null(parsed$value) && length(parsed$value) > 0) parsed$value[1] else NA_character_
    }, character(1))
    flikar_valid <- flikar[!is.na(flikar)]
  }
  if (length(flikar_valid) == 0) {
    flikbesok <- tom_tabell(c("flik", "antal"))
  } else {
    tab <- table(flikar_valid)
    flikbesok <- data.frame(flik = names(tab), antal = as.integer(tab), stringsAsFactors = FALSE)
    flikbesok <- flikbesok[order(-flikbesok$antal), ]
  }
  
  # ---- Per veckodag / timme (baserat pa unika sessionsstarter) ----
  sessionsstart <- aggregate(time ~ session, data = rader, FUN = min)
  sessionsstart$veckodag <- weekdays(sessionsstart$time)
  sessionsstart$timme <- as.integer(format(sessionsstart$time, "%H"))
  
  tab_v <- table(sessionsstart$veckodag)
  per_veckodag <- data.frame(veckodag = names(tab_v), antal = as.integer(tab_v), stringsAsFactors = FALSE)
  
  tab_t <- table(sessionsstart$timme)
  per_timme <- data.frame(timme = as.integer(names(tab_t)), antal = as.integer(tab_t), stringsAsFactors = FALSE)
  
  # ---- Kombinerad veckodag x timme (for per-app-heatmap) ----
  tab_vt <- as.data.frame(table(veckodag = sessionsstart$veckodag, timme = sessionsstart$timme))
  names(tab_vt) <- c("veckodag", "timme", "antal")
  tab_vt$timme <- as.integer(as.character(tab_vt$timme))
  per_veckodag_timme <- tab_vt[tab_vt$antal > 0, ]
  
  list(
    antal_sessioner       = antal_sessioner,
    antal_unika_anvandare = antal_unika_anvandare,
    flikbesok             = flikbesok,
    per_veckodag          = per_veckodag,
    per_timme             = per_timme,
    per_veckodag_timme    = per_veckodag_timme
  )
}

#' Listar alla app+server-kombinationer som nagonsin loggat telemetri,
#' med grundlaggande mattal. Anvands for oversiktstabellen i adminportal.
hamta_telemetri_appar <- function() {
  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  df <- DBI::dbGetQuery(con, "
    SELECT app_name,
           count(DISTINCT session) AS antal_sessioner,
           min(time) AS forsta_aktivitet,
           max(time) AS senaste_aktivitet
    FROM shiny_telemetry.event_log
    GROUP BY app_name
    ORDER BY app_name
  ")
  
  if (nrow(df) == 0) return(df)
  
  # app_namn slutar alltid pa _intern eller _publik (satt av skapa_telemetry())
  df$server <- ifelse(grepl("_intern$", df$app_name), "intern",
                      ifelse(grepl("_publik$", df$app_name), "publik", NA_character_))
  df$app <- sub("_(intern|publik)$", "", df$app_name)
  
  # Unika anvandare per app maste raknas separat (kraver JSON-parsning av login-events)
  unika <- vapply(df$app_name, function(namn) {
    login_rader <- DBI::dbGetQuery(con, "
      SELECT details FROM shiny_telemetry.event_log
      WHERE app_name = $1 AND type = 'login'
    ", params = list(namn))
    if (nrow(login_rader) == 0) return(0L)
    id <- vapply(login_rader$details, function(d) {
      p <- jsonlite::fromJSON(d)
      if (!is.null(p$username)) p$username[1] else NA_character_
    }, character(1))
    length(unique(id[!is.na(id)]))
  }, integer(1))
  df$antal_unika_anvandare <- unika
  
  df[, c("app", "server", "antal_sessioner", "antal_unika_anvandare",
         "forsta_aktivitet", "senaste_aktivitet")]
}

#' Aggregerar sessionsstarter per veckodag/timme over ALLA appar - for
#' den samlade heatmapen pa Statistik-flikens forstasida.
hamta_telemetri_heatmap_alla <- function() {
  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  rader <- DBI::dbGetQuery(con, "SELECT time, session FROM shiny_telemetry.event_log")
  if (nrow(rader) == 0) return(data.frame(veckodag = character(0), timme = integer(0), antal = integer(0)))
  
  sessionsstart <- aggregate(time ~ session, data = rader, FUN = min)
  sessionsstart$veckodag <- weekdays(sessionsstart$time)
  sessionsstart$timme <- as.integer(format(sessionsstart$time, "%H"))
  
  agg <- as.data.frame(table(veckodag = sessionsstart$veckodag, timme = sessionsstart$timme))
  names(agg) <- c("veckodag", "timme", "antal")
  agg$timme <- as.integer(as.character(agg$timme))
  agg
}

#' Hamtar de senaste N rahandelserna for en app, med tolkad/lasbar
#' beskrivning istallet for rå JSON.
hamta_telemetri_handelser <- function(app_namn, target = c("publik", "intern"), antal = 200) {
  target <- match.arg(target)
  full_namn <- paste0(app_namn, "_", target)
  
  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  rader <- DBI::dbGetQuery(con, "
    SELECT time, session, type, details
    FROM shiny_telemetry.event_log
    WHERE app_name = $1
    ORDER BY time DESC
    LIMIT $2
  ", params = list(full_namn, antal))
  
  if (nrow(rader) == 0) return(rader)
  
  rader$beskrivning <- vapply(seq_len(nrow(rader)), function(i) {
    typ <- rader$type[i]
    parsed <- tryCatch(jsonlite::fromJSON(rader$details[i]), error = function(e) NULL)
    if (is.null(parsed)) return(rader$details[i])
    
    switch(typ,
           "navigation" = paste("Flik:", parsed$value[1]),
           "input"      = paste("Andrade:", parsed$id[1]),
           "login"      = "Ny session (anonym)",
           "browser"    = paste("Webblasare:", parsed$value[1]),
           rader$details[i]
    )
  }, character(1))
  
  rader[, c("time", "session", "type", "beskrivning")]
}
