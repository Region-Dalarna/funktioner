# =============================================================================
# func_kor_cron_jobb.R
# Delad logik for att exekvera ETT cron-jobb (kalla data, skriva fil,
# uppdatera senast_kord/senast_status). Anvands av bade RP0003 och
# wfalmitvs978 via tunna stub-skript (adminportal/cron/kor_cron_jobb.R
# respektive serverdrift/adminportal-remote/kor_cron_jobb.R) - se dessa
# filers kommentarer for varfor stub-monstret anvands istallet for att
# kopiera hela denna logik pa tva stallen.
# =============================================================================

ADMINPORTAL_METADATA_DB      <- "sekretess"
ADMINPORTAL_METADATA_DB_USER <- "shiny_skriv_sekretess"

TILLATNA_DB_USERS <- c(
  "standard"  = "shiny_las",
  "sekretess" = "shiny_las_sekretess"
)

kor_cron_jobb <- function(jobb_id) {
  logga <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", ..., "\n")
  
  con_meta <- shiny_uppkoppling_skriv(db_name = ADMINPORTAL_METADATA_DB, db_user = ADMINPORTAL_METADATA_DB_USER)
  if (is.null(con_meta)) stop("Kunde inte ansluta till metadatadatabasen '", ADMINPORTAL_METADATA_DB, "'.", call. = FALSE)
  on.exit(try(DBI::dbDisconnect(con_meta), silent = TRUE), add = TRUE)
  
  satt_status <- function(status) {
    DBI::dbExecute(con_meta,
                   "UPDATE adminshiny.cron_jobb SET senast_kord = now(), senast_status = $1 WHERE id = $2",
                   params = list(status, jobb_id))
  }
  
  resultat <- tryCatch({
    jobb <- DBI::dbGetQuery(con_meta, "SELECT * FROM adminshiny.cron_jobb WHERE id = $1", params = list(jobb_id))
    if (nrow(jobb) == 0) stop("Hittar inget jobb med id = ", jobb_id, call. = FALSE)
    jobb <- jobb[1, ]
    
    if (!jobb$uppkoppling %in% names(TILLATNA_DB_USERS)) {
      stop("Okand eller ej tillaten uppkoppling: ", jobb$uppkoppling, call. = FALSE)
    }
    db_user_val <- TILLATNA_DB_USERS[[jobb$uppkoppling]]
    
    con_data <- shiny_uppkoppling_las(db_name = jobb$kalla_databas, db_user = db_user_val)
    if (is.null(con_data)) stop("Kunde inte ansluta till '", jobb$kalla_databas, "' som '", db_user_val, "'.", call. = FALSE)
    on.exit(try(DBI::dbDisconnect(con_data), silent = TRUE), add = TRUE)
    
    sql <- if (!is.na(jobb$egen_sql) && nzchar(trimws(jobb$egen_sql))) {
      jobb$egen_sql
    } else {
      sprintf("SELECT * FROM %s.%s",
              DBI::dbQuoteIdentifier(con_data, jobb$kalla_schema),
              DBI::dbQuoteIdentifier(con_data, jobb$kalla_tabell))
    }
    
    data <- if (jobb$format == "gpkg") {
      sf::st_read(con_data, query = sql, quiet = TRUE)
    } else {
      DBI::dbGetQuery(con_data, sql)
    }
    
    malmapp <- file.path("/srv/shiny-server", jobb$app, "www", "nedladdning")
    if (!dir.exists(malmapp)) {
      skapad <- dir.create(malmapp, recursive = TRUE)
      if (!skapad || !dir.exists(malmapp)) {
        stop("Kunde inte skapa katalogen '", malmapp, "' - saknar appen '",
             jobb$app, "' skrivrattighet, eller finns den inte?", call. = FALSE)
      }
      logga("Skapade katalog:", malmapp)
    }
    
    bas <- tools::file_path_sans_ext(jobb$malfil)
    
    malfil_path <- switch(
      jobb$format,
      "csv" = {
        p <- file.path(malmapp, paste0(bas, ".csv"))
        utils::write.csv2(data, p, row.names = FALSE, fileEncoding = "UTF-8")
        p
      },
      "xlsx" = {
        p <- file.path(malmapp, paste0(bas, ".xlsx"))
        writexl::write_xlsx(data, p)
        p
      },
      "csv_zip" = {
        csv_namn <- paste0(bas, ".csv")
        tmp_csv  <- file.path(tempdir(), csv_namn)
        utils::write.csv2(data, tmp_csv, row.names = FALSE, fileEncoding = "UTF-8")
        p <- file.path(malmapp, paste0(bas, ".zip"))
        if (file.exists(p)) file.remove(p)
        utils::zip(zipfile = p, files = tmp_csv, flags = "-j")
        file.remove(tmp_csv)
        p
      },
      "gpkg" = {
        p <- file.path(malmapp, paste0(bas, ".gpkg"))
        if (file.exists(p)) file.remove(p)
        sf::st_write(data, dsn = p, layer = bas, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
        p
      },
      stop("Okant format: ", jobb$format, call. = FALSE)
    )
    
    logga("OK - skrev", nrow(data), "rader till", malfil_path)
    "OK"
  }, error = function(e) {
    logga("FEL:", conditionMessage(e))
    paste0("FEL: ", conditionMessage(e))
  })
  
  satt_status(resultat)
}