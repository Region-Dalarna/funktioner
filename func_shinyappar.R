

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
