

shiny_set_password <- function(service) {
  
  # Kontrollera att service är giltigt
  if (!grepl("^[A-Za-z0-9_]+$", service)) {
    stop("Service-namnet får bara innehålla A-Z, a-z, 0-9 och '_'.")
  }
  
  varname <- paste0(service, "_PWD")
  
  home <- Sys.getenv("HOME")
  renv_file <- file.path(home, ".Renviron")
  
  existing <- if (file.exists(renv_file)) readLines(renv_file) else character()
  
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
  readRenviron("~/.Renviron")
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
  readRenviron("~/.Renviron")
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
  for (s in services) cat(" - ", s, "\n", sep = "")
  
  return(services)
}



shiny_uppkoppling_skriv <- function(
    databas = "geodata",
    db_host = "WFALMITVS526.ltdalarna.se",
    db_port = 5432,
    db_options = "-c search_path=public",
    db_user = "shiny_skriv"
) {
  
  shiny_uppkoppling_db(service_name = "databas_adm", 
                 db_name = databas,
                 db_host = db_host,
                 db_port = db_port,
                 db_options = db_options)
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
    con <- suppress_specific_warning(
      dbConnect(          
        RPostgres::Postgres(),
        bigint = "integer",  
        user = db_user,
        password = shiny_get_password(db_user),
        host = db_host,
        port = db_port,
        dbname = db_name,
        #timezon = "UTC",
        options=db_options),
      "Invalid time zone 'UTC', falling back to local time.")
    
    
    # Returnerar anslutningen om den lyckas
    return(con)
  }, error = function(e) {
    # Skriver ut felmeddelandet och returnerar NULL
    print(paste("Ett fel inträffade vid anslutning till databasen:", e$message))
    return(NULL)
  })
  
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
