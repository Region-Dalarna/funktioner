# =============================================================================
# func_gtfs.R  (paketversion)
# =============================================================================
# Förutsättningar för paketet:
#   - R >= 4.1 (använder den inbyggda pipen |>)
#   - Imports: httr, data.table, dplyr, tidyr, sf, rvest, DBI, glue, keyring,
#              gtfstools, jsonlite
#   - Suggests: mapview
#   - Inga library()/pacman/source() i funktionerna - allt ska ligga i
#     DESCRIPTION/NAMESPACE. Observera att gtfs_operatorer_sverige_nyckeltabell_hamta()
#     med med_regionnamn = TRUE kräver hamtaregtab() och skapa_kortnamn_lan()
#     (tidigare inlästa via source() av func_API.R) - de måste finnas i paketet.
# =============================================================================

.gtfs_veckodagar <- c("monday", "tuesday", "wednesday", "thursday",
                      "friday", "saturday", "sunday")
# index 1 = söndag, motsvarar format(datum, "%w") + 1 (språkoberoende)
.gtfs_veckodagar_w <- c("sunday", "monday", "tuesday", "wednesday",
                        "thursday", "friday", "saturday")


# ================================ hjälpfunktioner ============================

# Kontrollerar gtfs_dataset och returnerar TRUE om det är sweden_3
gtfs_kontrollera_dataset <- function(gtfs_dataset) {
  if (length(gtfs_dataset) != 1 || !gtfs_dataset %in% c("regional", "sweden_3")) {
    stop("gtfs_dataset måste vara 'regional' eller 'sweden_3'", call. = FALSE)
  }
  gtfs_dataset == "sweden_3"
}

# GTFS-datum ("YYYYMMDD" som text) -> Date. Date-objekt returneras oförändrade.
gtfs_text_till_datum <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x), format = "%Y%m%d")
}

# Date -> "YYYYMMDD" (så som GTFS-standarden kräver vid skrivning till zip).
# Text returneras oförändrad.
gtfs_datum_till_text <- function(x) {
  if (inherits(x, "Date")) format(x, "%Y%m%d") else as.character(x)
}

# Konverterar de kolumner som finns till heltal/decimaltal (saknade hoppas över)
gtfs_konvertera <- function(df, heltal = character(), decimal = character()) {
  for (k in intersect(heltal, names(df))) df[[k]] <- suppressWarnings(as.integer(df[[k]]))
  for (k in intersect(decimal, names(df))) df[[k]] <- suppressWarnings(as.numeric(df[[k]]))
  df
}

# Tomma strängar -> NA i alla textkolumner (ersätter dplyr::na_if över across)
gtfs_tom_till_na <- function(df) {
  for (k in names(df)[vapply(df, is.character, logical(1))]) {
    df[[k]][which(df[[k]] == "")] <- NA_character_
  }
  df
}

# Läser en GTFS-textfil med fread (snabbare än read.csv2 och hanterar BOM).
# Alla kolumner läses som text och typkonverteras sedan explicit.
gtfs_las_fil <- function(mapp, filnamn, obligatorisk = FALSE) {
  sokvag <- file.path(mapp, filnamn)
  if (!file.exists(sokvag)) {
    if (obligatorisk) stop("Filen ", filnamn, " saknas i GTFS-datasetet.", call. = FALSE)
    return(NULL)
  }
  data.table::fread(sokvag, sep = ",", colClasses = "character",
                    encoding = "UTF-8", data.table = FALSE, showProgress = FALSE)
}

# Bygger linjegeometrier (en per shape_id) från shapes-punkter.
# Punkterna sorteras på shape_pt_sequence (annars blir linjerna fel om filen
# inte är sorterad) och shapes med färre än två punkter utelämnas.
# Returnerar ett sf-objekt med shape_id, antal_punkter, max_dist och geometry.
gtfs_shapes_till_linjer <- function(shapes, crs = 4326) {
  df <- data.frame(
    shape_id = as.character(shapes$shape_id),
    lon      = as.numeric(shapes$shape_pt_lon),
    lat      = as.numeric(shapes$shape_pt_lat),
    sekv     = as.integer(shapes$shape_pt_sequence),
    dist     = if ("shape_dist_traveled" %in% names(shapes)) as.numeric(shapes$shape_dist_traveled) else NA_real_,
    stringsAsFactors = FALSE
  )
  df <- df[stats::complete.cases(df[, c("shape_id", "lon", "lat")]), ]
  df <- df[order(df$shape_id, df$sekv, method = "radix"), ]

  idx <- split(seq_len(nrow(df)), df$shape_id)
  idx <- idx[lengths(idx) >= 2]
  if (length(idx) == 0) stop("Inga shapes med minst två punkter hittades.", call. = FALSE)

  linjer <- lapply(idx, function(i) sf::st_linestring(cbind(df$lon[i], df$lat[i])))
  max_dist <- vapply(idx, function(i) {
    d <- df$dist[i]
    if (all(is.na(d))) NA_real_ else max(c(0, diff(d)), na.rm = TRUE)
  }, numeric(1))

  sf::st_sf(shape_id      = names(idx),
            antal_punkter = as.integer(lengths(idx)),
            max_dist      = unname(max_dist),
            geometry      = sf::st_sfc(unname(linjer), crs = crs))
}


# ================== nyckeltabell operatörer (Trafiklab) ======================

gtfs_operatorer_sverige_nyckeltabell_hamta <- function(tabort_na = TRUE,
                                                       med_regionnamn = TRUE) {
  # Hämtar tabellen med regionala operatörer från Trafiklab så att en operatör
  # kan hämtas med länskod. Trafiklab har tagit bort länsnamnen ur tabellen,
  # därför kopplas länskoden på manuellt via operatörskoden.
  #
  # med_regionnamn = FALSE hoppar över uppslaget av länsnamn (som kräver
  # hamtaregtab()) - det behövs inte för att bara översätta länskod -> operatörskod.

  operatorkod_till_regionkod <- c(
    sl = "01", ul = "03", sormland = "04", otraf = "05", jlt = "06",
    krono = "07", klt = "08", gotland = "09", blekinge = "10", skane = "12",
    halland = "13", vt = "14", varm = "17", orebro = "18", vastmanland = "19",
    dt = "20", xt = "21", dintur = "22", jamtland = "23", vasterbotten = "24",
    norrbotten = "25"
  )

  url_trafiklab <- "https://www.trafiklab.se/sv/api/gtfs-datasets/gtfs-regional/"

  retur_df <- rvest::read_html(url_trafiklab) |>
    rvest::html_element("table") |>
    rvest::html_table() |>
    dplyr::select(operator = Operator, operatorkod = Abbreviation) |>
    dplyr::mutate(regionkod = unname(operatorkod_till_regionkod[operatorkod]))

  if (med_regionnamn) {
    regionnyckel <- hamtaregtab() |>
      dplyr::filter(nchar(regionkod) == 2, regionkod != "00") |>
      dplyr::mutate(region = skapa_kortnamn_lan(region))
    retur_df <- dplyr::left_join(retur_df, regionnyckel, by = "regionkod")
  }

  if (tabort_na) retur_df <- dplyr::filter(retur_df, !is.na(regionkod))

  retur_df
}


# ================================ gtfs calendar ==============================

# Skapar calendar-tabell utifrån calendar_dates.
# En veckodag flaggas bara om det finns minst ett datum med exception_type == 1
# (tillagd trafik) på den veckodagen. Tidigare användes exception_type-värdet
# direkt, vilket gav värdet 2 (= inställd trafik) i veckodagskolumnerna.
gtfs_fyll_calendar_dagar <- function(calendar_dates_df) {

  calendar_dates_df |>
    dplyr::mutate(datum    = gtfs_text_till_datum(date),
                  veckodag = .gtfs_veckodagar_w[as.integer(format(datum, "%w")) + 1L]) |>
    dplyr::group_by(service_id) |>
    dplyr::summarise(
      monday     = as.integer(any(veckodag == "monday"    & exception_type == 1L)),
      tuesday    = as.integer(any(veckodag == "tuesday"   & exception_type == 1L)),
      wednesday  = as.integer(any(veckodag == "wednesday" & exception_type == 1L)),
      thursday   = as.integer(any(veckodag == "thursday"  & exception_type == 1L)),
      friday     = as.integer(any(veckodag == "friday"    & exception_type == 1L)),
      saturday   = as.integer(any(veckodag == "saturday"  & exception_type == 1L)),
      sunday     = as.integer(any(veckodag == "sunday"    & exception_type == 1L)),
      start_date = min(datum),
      end_date   = max(datum),
      .groups = "drop"
    )
}

# Skapar calendar_dates utifrån calendar (ett datum per service_id och aktiv dag).
# Returnerar date som Date och exception_type som heltal, i samma format som
# hamta_gtfs_data() returnerar.
gtfs_fyll_calendar_dates_fran_calendar <- function(calendar_df,
                                                   skickade_service_id = NULL,
                                                   exception_kol = 1L) {

  # om man skickar med service_id så kör vi bara på dem, annars hela datasetet
  if (!is.null(skickade_service_id) && !all(is.na(skickade_service_id))) {
    calendar_df <- dplyr::filter(calendar_df, service_id %in% skickade_service_id)
  }

  # vilka veckodagar är aktiva för varje service_id
  aktiva_df <- calendar_df |>
    dplyr::select(service_id, dplyr::all_of(.gtfs_veckodagar)) |>
    tidyr::pivot_longer(-service_id, names_to = "veckodag", values_to = "aktiv") |>
    dplyr::filter(as.integer(aktiv) == 1L) |>
    dplyr::select(-aktiv)

  # alla datum mellan start och slut per service_id, behåll bara aktiva veckodagar
  # (vektoriserad join istället för rowwise-filter)
  calendar_df |>
    dplyr::transmute(service_id,
                     start = gtfs_text_till_datum(start_date),
                     slut  = gtfs_text_till_datum(end_date)) |>
    dplyr::mutate(date = Map(function(a, b) seq.Date(a, b, by = "day"), start, slut)) |>
    tidyr::unnest(date) |>
    dplyr::mutate(veckodag = .gtfs_veckodagar[(as.integer(format(date, "%w")) + 6L) %% 7L + 1L]) |>
    dplyr::inner_join(aktiva_df, by = c("service_id", "veckodag")) |>
    dplyr::transmute(service_id, date, exception_type = as.integer(exception_kol))
}


# =================== Hämtar GTFS-dataset =====================================

# Huvudfunktion för nedladdning och bearbetning av GTFS-data
#
# För att köra funktionen krävs paketet keyring med en sparad service som heter
# "API_trafiklab_token", där användarnamn är "GTFS_Regional", "GTFS_Sverige_2"
# eller "GTFS_Sweden_3" beroende på vilket dataset man vill hämta. Själva token
# läggs som lösenord. Man skapar token (kostnadsfritt) hos Trafiklab:
# https://developer.trafiklab.se/login
#
# Parametrar:
#   - gtfs_dataset: länskod (t.ex. "20" eller 20), operatörskod (t.ex. "skane"),
#                   "sverige_2" eller "sweden_3".
#   - spara_filmap: mapp att spara zip och uppackade filer i. NA = temporärt
#                   (städas bort automatiskt när funktionen är klar).
#   - test_mode:    TRUE = testa bara att URL:en svarar, ingen nedladdning.
#
# Retur: lista med dataframes (routes, stops, stop_times, trips, calendar_dates,
# calendar, samt shapes, agency, feed_info, attributions, transfers om de finns).
# Datum är Date-objekt och numeriska fält har riktiga typer.
hamta_gtfs_data <- function(gtfs_dataset = "20",
                            spara_filmap = NA,
                            test_mode = FALSE) {

  if (length(gtfs_dataset) != 1) {
    stop("Parametern gtfs_dataset kan för närvarande bara ha ett värde. Korrigera parametern och försök igen.")
  }
  gtfs_dataset <- as.character(gtfs_dataset)
  dataset_lc   <- tolower(gtfs_dataset)

  tryCatch({
    datum <- format(Sys.Date(), "%Y%m%d")

    if (dataset_lc %in% c("sweden_3", "sweden3")) {
      api_key  <- keyring::key_get("API_trafiklab_token", "GTFS_Sweden_3")
      url      <- paste0("https://opendata.samtrafiken.se/gtfs-sweden/sweden.zip?key=", api_key)
      kan_head <- TRUE
    } else if (dataset_lc %in% c("sverige_2", "sverige2")) {
      api_key  <- keyring::key_get("API_trafiklab_token", "GTFS_Sverige_2")
      url      <- paste0("https://api.resrobot.se/gtfs/sweden.zip?key=", api_key)
      kan_head <- FALSE   # sverige_2 stödjer inte HEAD-anrop
    } else {
      # länskoder ska vara tvåsiffriga ("03" och inte "3")
      if (grepl("^[0-9]+$", gtfs_dataset)) gtfs_dataset <- sprintf("%02d", as.integer(gtfs_dataset))

      # översätt länskod -> operatörskod (som Trafiklab använder i URL:en)
      regionnyckel <- gtfs_operatorer_sverige_nyckeltabell_hamta(med_regionnamn = FALSE)
      rkm <- if (dataset_lc %in% regionnyckel$operatorkod) dataset_lc else
        regionnyckel$operatorkod[regionnyckel$regionkod == gtfs_dataset]
      if (length(rkm) != 1) {
        stop("Hittade inte exakt en regional operatör för '", gtfs_dataset,
             "'. Ange en giltig länskod eller operatörskod.")
      }

      api_key  <- keyring::key_get("API_trafiklab_token", "GTFS_Regional")
      url      <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", api_key)
      kan_head <- TRUE
    }

    # Testläge: kontrollera bara att URL:en fungerar
    if (test_mode) {
      svar <- if (kan_head) httr::HEAD(url) else
        httr::GET(url, httr::add_headers(Range = "bytes=0-0"))
      if (!httr::status_code(svar) %in% c(200L, 206L)) {
        stop("URL:en fungerar inte, felkod: ", httr::status_code(svar))
      }
      message("Test framgångsrikt: URL:en finns och fungerar.")
      return(invisible(TRUE))
    }

    # Filnamn/sökvägar. Temporära filer städas bort när funktionen avslutas.
    spara <- !is.null(spara_filmap) && !is.na(spara_filmap)
    if (spara) {
      if (!dir.exists(spara_filmap)) dir.create(spara_filmap, recursive = TRUE)
      gtfs_fil  <- file.path(spara_filmap, paste0("trafiklab_", gtfs_dataset, "_", datum, ".zip"))
      unzip_dir <- file.path(spara_filmap, paste0(gtfs_dataset, "_", datum))
    } else {
      gtfs_fil  <- tempfile(fileext = ".zip")
      unzip_dir <- tempfile("gtfs_")
      on.exit(unlink(c(gtfs_fil, unzip_dir), recursive = TRUE), add = TRUE)
    }

    svar <- httr::GET(url, httr::write_disk(gtfs_fil, overwrite = TRUE))
    # utan denna kontroll sparas ett felmeddelande (t.ex. 403 vid fel token) som "zip"
    httr::stop_for_status(svar, task = "ladda ned GTFS-datasetet")
    utils::unzip(gtfs_fil, exdir = unzip_dir)

    las <- function(fil, obligatorisk = FALSE) gtfs_las_fil(unzip_dir, fil, obligatorisk)

    gtfs_lista <- list()

    # routes
    gtfs_lista$routes <- las("routes.txt", TRUE) |>
      gtfs_konvertera(heltal = "route_type")

    # stops
    stops <- las("stops.txt", TRUE) |>
      gtfs_konvertera(heltal = "location_type", decimal = c("stop_lat", "stop_lon"))
    stops$hpl_id <- substr(stops$stop_id, 8, 13)
    gtfs_lista$stops <- stops

    # stop_times
    stop_times <- las("stop_times.txt", TRUE) |>
      gtfs_konvertera(heltal  = c("stop_sequence", "pickup_type", "drop_off_type", "timepoint"),
                      decimal = "shape_dist_traveled")
    if (!"shape_dist_traveled" %in% names(stop_times)) stop_times$shape_dist_traveled <- NA_real_
    if (!"timepoint" %in% names(stop_times))           stop_times$timepoint <- NA_integer_
    gtfs_lista$stop_times <- stop_times

    # trips
    gtfs_lista$trips <- las("trips.txt", TRUE) |>
      gtfs_konvertera(heltal = "direction_id")
    if (!"direction_id" %in% names(gtfs_lista$trips)) gtfs_lista$trips$direction_id <- NA_integer_

    # calendar_dates och calendar. Vissa dataset saknar den ena eller har en
    # calendar som bara innehåller nollor - då fylls den från den andra.
    calendar_dates <- las("calendar_dates.txt")
    calendar       <- las("calendar.txt")

    if (!is.null(calendar_dates)) {
      calendar_dates <- calendar_dates |>
        dplyr::mutate(date = gtfs_text_till_datum(date),
                      exception_type = as.integer(exception_type))
    }
    if (!is.null(calendar)) {
      calendar <- calendar |>
        gtfs_konvertera(heltal = .gtfs_veckodagar) |>
        dplyr::mutate(start_date = gtfs_text_till_datum(start_date),
                      end_date   = gtfs_text_till_datum(end_date))
      if (sum(unlist(calendar[.gtfs_veckodagar]), na.rm = TRUE) == 0) calendar <- NULL
    }
    if (is.null(calendar_dates) && !is.null(calendar)) {
      calendar_dates <- gtfs_fyll_calendar_dates_fran_calendar(calendar)
    }
    if (is.null(calendar_dates)) {
      stop("Datasetet saknar både användbar calendar.txt och calendar_dates.txt.")
    }
    if (is.null(calendar)) calendar <- gtfs_fyll_calendar_dagar(calendar_dates)

    gtfs_lista$calendar_dates <- calendar_dates
    gtfs_lista$calendar       <- calendar

    # shapes - bara om de finns. Koordinaterna konverteras till tal här så att
    # alla efterföljande steg (databas, kartor) kan lita på typerna.
    shapes <- las("shapes.txt")
    if (!is.null(shapes)) {
      shapes <- gtfs_konvertera(shapes,
                                heltal  = "shape_pt_sequence",
                                decimal = c("shape_pt_lat", "shape_pt_lon", "shape_dist_traveled"))
      if (!"shape_dist_traveled" %in% names(shapes)) shapes$shape_dist_traveled <- NA_real_
      gtfs_lista$shapes <- shapes
    }

    # övriga filer - om de finns
    for (fil in c("agency", "feed_info", "attributions", "transfers")) {
      x <- las(paste0(fil, ".txt"))
      if (!is.null(x)) gtfs_lista[[fil]] <- x
    }

    gtfs_lista

  }, error = function(e) {
    stop(paste("Följande fel har uppstått under nedladdning och bearbetning av gtfs-datasetet: ", conditionMessage(e)),
         call. = FALSE)
  })
}

# Exempel:
# sweden_3 <- hamta_gtfs_data("sweden_3", test_mode = TRUE)
# dt       <- hamta_gtfs_data("dt", test_mode = TRUE)
# skane    <- hamta_gtfs_data("skane")


# ========== Plotta GTFS-data ==========

# Visualiserar "shapes" från ett GTFS-dataset som linjer på en karta (mapview).
# Datasetet måste innehålla shapes (sverige_2 saknar shapes.txt).
mapview_gtfs <- function(gtfs_data) {

  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("Paketet 'mapview' krävs för mapview_gtfs() men är inte installerat.", call. = FALSE)
  }
  if (is.null(gtfs_data$shapes)) {
    stop("GTFS-datat innehåller ingen shapes-information. Kan inte skapa karta.", call. = FALSE)
  }

  shapes_lines <- gtfs_shapes_till_linjer(gtfs_data$shapes, crs = 4326)

  mapview::mapview(shapes_lines, zcol = "shape_id", legend = TRUE)
}

# mapview_gtfs(skane)


# ========== r5r hjälpfunktioner =============================================

# Validera GTFS-data med Google GTFS-validator och skriv ut alla fel och
# varningar. Kräver att gtfs-validator.jar finns (och Java).
#
#  gtfs            GTFS-objekt (lista eller dt_gtfs) eller sökväg till zip-fil
#  output_path     Mapp där valideringsrapporten (report.json) sparas
#  validator_path  Sökväg till gtfs-validator.jar
#  return          Valideringsrapporten som lista (invisibly)
gtfs_validera <- function(gtfs,
                          output_path    = "validering",
                          validator_path = file.path(getwd(), "gtfstools", "gtfs-validator-v6.0.0.jar")) {

  if (is.list(gtfs) && !inherits(gtfs, "dt_gtfs")) gtfs <- gtfstools::as_dt_gtfs(gtfs)

  gtfstools::validate_gtfs(gtfs, output_path = output_path, validator_path = validator_path)

  rapport <- jsonlite::read_json(file.path(output_path, "report.json"))
  for (notice in rapport$notices) {
    if (notice$severity %in% c("ERROR", "WARNING")) {
      message(notice$severity, " - ", notice$code, " - ", notice$totalNotices, " fel")
    }
  }
  invisible(rapport)
}


# Minimal rensning innan skrivning till databas.
#
# Fixar endast genuina datafel som hindrar uppladdning:
# - Säkerställer numeriska koordinater
# - Konverterar tomma strängar till NA i heltalkolumner (sweden_3)
# - Normaliserar stop_sequence så den börjar på 1 per tur
#
# Koordinatfiltrering och analytisk rensning sker istället i gtfs_rensa_for_r5r()
# inför analys, så att databasen speglar källdatan så nära som möjligt.
gtfs_rensa_for_db <- function(gtfs, gtfs_dataset = "regional") {

  sweden_3 <- gtfs_kontrollera_dataset(gtfs_dataset)

  gtfs$stops <- gtfs_konvertera(gtfs$stops, decimal = c("stop_lat", "stop_lon"))

  # Normalisera stop_sequence till 1..n per tur. Sortera på tur och sekvens och
  # numrera med rle() - betydligt snabbare än group_by/row_number på stora tabeller.
  st <- gtfs$stop_times
  st <- st[order(st$trip_id, st$stop_sequence, method = "radix"), ]
  st$stop_sequence <- sequence(rle(st$trip_id)$lengths)
  gtfs$stop_times <- st

  # Endast sweden_3: tomma strängar -> NA och rätt typer
  if (sweden_3) {
    if (!is.null(gtfs$transfers)) {
      gtfs$transfers <- gtfs$transfers |>
        gtfs_tom_till_na() |>
        gtfs_konvertera(heltal = c("transfer_type", "min_transfer_time"))
    }
    if (!is.null(gtfs$attributions)) {
      gtfs$attributions <- gtfs$attributions |>
        gtfs_tom_till_na() |>
        gtfs_konvertera(heltal = "is_operator")
    }
    if (!is.null(gtfs$calendar)) {
      gtfs$calendar <- gtfs$calendar |>
        gtfs_tom_till_na() |>
        gtfs_konvertera(heltal = .gtfs_veckodagar)
    }
    if (!is.null(gtfs$feed_info)) {
      gtfs$feed_info <- gtfs_tom_till_na(gtfs$feed_info)
    }
  }

  gtfs
}


# Rensa GTFS för r5r.
#
# Tar bort anropsstyrd trafik (1501), konverterar tågkoder, tar bort parent
# stations och stops med ogiltiga koordinater, attributions och transfers, samt
# sätter feed_info-datum dynamiskt. Alla beroende tabeller kaskadfiltreras så
# att inga föräldralösa routes, shapes, services eller agencies blir kvar.
gtfs_rensa_for_r5r <- function(gtfs) {

  # Ta bort anropsstyrd trafik (1501) och konvertera tågkoder till standardkoden 2
  gtfs$routes <- gtfs$routes |>
    dplyr::filter(route_type != 1501L) |>
    dplyr::mutate(route_type = dplyr::if_else(route_type %in% c(100L, 103L, 106L),
                                              2L, as.integer(route_type)))
  gtfs$trips <- dplyr::filter(gtfs$trips, route_id %in% gtfs$routes$route_id)

  # Ta bort parent stations + stops med ogiltiga koordinater
  gtfs$stops <- gtfs$stops |>
    dplyr::filter(is.na(location_type) | location_type == 0,
                  dplyr::between(stop_lat, 55, 70),
                  dplyr::between(stop_lon, 10, 25)) |>
    dplyr::select(-dplyr::any_of(c("location_type", "parent_station", "platform_code", "hpl_id")))

  # stop_times: bara kvarvarande stops/trips. shape_dist_traveled tas bort
  # (orsakar valideringsfel i r5r)
  gtfs$stop_times <- gtfs$stop_times |>
    dplyr::filter(stop_id %in% gtfs$stops$stop_id, trip_id %in% gtfs$trips$trip_id) |>
    dplyr::select(-dplyr::any_of("shape_dist_traveled"))
  if (!is.null(gtfs$shapes)) {
    gtfs$shapes <- dplyr::select(gtfs$shapes, -dplyr::any_of("shape_dist_traveled"))
  }

  # Oanvändbara tabeller
  gtfs$attributions <- NULL
  gtfs$transfers    <- NULL

  # Trips utan stop_times bort, därefter kaskad till övriga tabeller
  gtfs$trips <- dplyr::filter(gtfs$trips, trip_id %in% gtfs$stop_times$trip_id)
  gtfs$routes <- dplyr::filter(gtfs$routes, route_id %in% gtfs$trips$route_id)

  kvarv_service <- unique(gtfs$trips$service_id)
  if (!is.null(gtfs$calendar)) {
    gtfs$calendar <- dplyr::filter(gtfs$calendar, service_id %in% kvarv_service)
  }
  gtfs$calendar_dates <- dplyr::filter(gtfs$calendar_dates, service_id %in% kvarv_service)

  if (!is.null(gtfs$shapes)) {
    gtfs$shapes <- dplyr::filter(gtfs$shapes, shape_id %in% gtfs$trips$shape_id)
  }
  if (!is.null(gtfs$agency) && "agency_id" %in% names(gtfs$routes) &&
      any(!is.na(gtfs$routes$agency_id))) {
    gtfs$agency <- dplyr::filter(gtfs$agency, agency_id %in% gtfs$routes$agency_id)
  }

  # Sätt feed_info-datum dynamiskt från datan (GTFS-format YYYYMMDD)
  if (!is.null(gtfs$feed_info) && nrow(gtfs$feed_info) > 0 && nrow(gtfs$calendar_dates) > 0) {
    gtfs$feed_info$feed_start_date <- gtfs_datum_till_text(min(gtfs$calendar_dates$date, na.rm = TRUE))
    gtfs$feed_info$feed_end_date   <- gtfs_datum_till_text(max(gtfs$calendar_dates$date, na.rm = TRUE))
  }

  gtfs
}


# Hämta GTFS från databas, förbered för r5r och spara körklar zip
#
# Läser GTFS-tabellerna från databasen, kör gtfs_rensa_for_r5r() och skriver en
# körklar GTFS-zip som kan användas direkt med r5r::setup_r5().
#
#  con         Databasanslutning (DBI)
#  schema      Schema att läsa från
#  output_zip  Sökväg till zip-fil som skapas. NULL = ingen zip skrivs, bara
#              GTFS-objektet returneras.
#  datum       Valfritt datum (Date eller "YYYY-MM-DD"). Behåller bara turer vars
#              service_id är aktivt på detta datum ELLER SENARE (exception_type 1).
#              NULL = ingen datumfiltrering (r5r väljer rätt turer vid körning).
#  return      Rensat GTFS-objekt (invisibly om zip skrivs)
gtfs_hamta_for_r5r <- function(con,
                               schema     = "gtfs",
                               output_zip = NULL,
                               datum      = NULL) {

  las <- function(tabell) {
    id <- DBI::Id(schema = schema, table = tabell)
    if (!DBI::dbExistsTable(con, id)) return(NULL)
    DBI::dbReadTable(con, id)
  }

  # stops: hämta alla kolumner utom geometrin. stop_lat/stop_lon finns redan i
  # tabellen, så det behövs ingen (förlustbringande) omräkning från SWEREF99.
  las_stops <- function() {
    id <- DBI::Id(schema = schema, table = "stops")
    if (!DBI::dbExistsTable(con, id)) return(NULL)
    kol <- setdiff(DBI::dbListFields(con, id), "geometry")
    DBI::dbGetQuery(con, paste0(
      "SELECT ", paste(DBI::dbQuoteIdentifier(con, kol), collapse = ", "),
      " FROM ", DBI::dbQuoteIdentifier(con, id)))
  }

  # ── Läs in tabeller ─────────────────────────────────────────────────────────
  gtfs <- list(
    routes         = las("routes"),
    trips          = las("trips"),
    stop_times     = las("stop_times"),
    stops          = las_stops(),
    calendar       = las("calendar"),
    calendar_dates = las("calendar_dates"),
    shapes         = las("shapes"),
    agency         = las("agency"),
    feed_info      = las("feed_info"),
    transfers      = las("transfers"),
    attributions   = las("attributions")
  )
  gtfs <- gtfs[!vapply(gtfs, is.null, logical(1))]

  # regional-datasetet laddar bara upp shapes_line, så shapes-tabellen är tom.
  # En tom shapes.txt (och trips som pekar på obefintliga shape_id) vill vi inte ha.
  if (!is.null(gtfs$shapes) && nrow(gtfs$shapes) == 0) {
    gtfs$shapes <- NULL
    gtfs$trips$shape_id <- NULL
    message("Tabellen shapes är tom - shapes utelämnas och shape_id tas bort från trips.")
  }

  # ── Valfri datumfiltrering ───────────────────────────────────────────────────
  if (!is.null(datum)) {
    datum  <- as.Date(datum)
    aktiva <- gtfs$calendar_dates |>
      dplyr::filter(as.Date(date) >= datum, exception_type == 1L) |>
      dplyr::pull(service_id) |>
      unique()
    gtfs$trips <- dplyr::filter(gtfs$trips, service_id %in% aktiva)
  }

  # ── Rensa för r5r ────────────────────────────────────────────────────────────
  gtfs <- gtfs_rensa_for_r5r(gtfs)

  # ── Spara körklar zip ────────────────────────────────────────────────────────
  if (!is.null(output_zip)) {
    # GTFS kräver datum som YYYYMMDD - Date-kolumner skulle annars skrivas som YYYY-MM-DD
    skriv <- gtfs
    if (!is.null(skriv$calendar)) {
      skriv$calendar <- dplyr::mutate(skriv$calendar,
                                      dplyr::across(dplyr::any_of(c("start_date", "end_date")), gtfs_datum_till_text))
    }
    skriv$calendar_dates <- dplyr::mutate(skriv$calendar_dates, date = gtfs_datum_till_text(date))

    gtfs_dt <- gtfstools::as_dt_gtfs(skriv)
    gtfstools::write_gtfs(gtfs_dt, output_zip)
    message("Körklar GTFS-zip skriven: ", output_zip)
    return(invisible(gtfs))
  }

  gtfs
}


# Filtrera ett GTFS-dataset geografiskt (alla turer som berör polygonen tas med
# i sin helhet).
#
#  gtfs_data               GTFS-objekt (lista eller dt_gtfs)
#  polygon                 sf/sfc-objekt med CRS
#  buffert_km              buffert runt polygonen i km
#  gtfs_dataset            "regional" eller "sweden_3" (påverkar bara validering)
#  fullstandiga_operatorer vektor med operatörsnamn (delsträng, case-okänsligt)
#                          vars linjer ALLTID tas med i sin helhet, oavsett om de
#                          ligger inom polygonen. NULL = ren geografisk filtrering.
gtfs_filtrera_geografiskt <- function(gtfs_data, polygon, buffert_km = 0,
                                      gtfs_dataset = "regional",
                                      fullstandiga_operatorer = NULL) {

  gtfs_kontrollera_dataset(gtfs_dataset)
  if (!inherits(polygon, "sf") && !inherits(polygon, "sfc")) {
    stop("polygon måste vara ett sf- eller sfc-objekt", call. = FALSE)
  }
  if (is.na(sf::st_crs(polygon))) {
    stop("polygon saknar CRS - sätt det med sf::st_set_crs() först", call. = FALSE)
  }
  if (!inherits(gtfs_data, "dt_gtfs")) {
    message("gtfs_data är inte av typen dt_gtfs — konverterar med as_dt_gtfs()")
    gtfs_data <- gtfstools::as_dt_gtfs(gtfs_data)
  }

  # Radurval med logisk vektor fungerar både för data.frame och data.table
  # (och kräver inte att paketet är data.table-aware)
  rader <- function(df, lgl) df[lgl, ]

  # Buffra polygon om buffert_km > 0 (i SWEREF99 TM så att meter stämmer)
  if (buffert_km > 0) {
    polygon <- polygon |>
      sf::st_transform(3006) |>
      sf::st_buffer(dist = buffert_km * 1000)
    message(glue::glue("Polygon buffrad med {buffert_km} km"))
  }
  polygon_union <- sf::st_union(sf::st_transform(polygon, 4326))

  # 1. Stops inom polygon
  stops <- as.data.frame(gtfs_data$stops)
  stops_sf <- sf::st_as_sf(stops[, c("stop_id", "stop_lon", "stop_lat")],
                           coords = c("stop_lon", "stop_lat"), crs = 4326,
                           na.fail = FALSE)
  inom_polygon <- lengths(sf::st_intersects(stops_sf, polygon_union)) > 0
  stops_ids_geografi <- stops$stop_id[inom_polygon]
  if (length(stops_ids_geografi) == 0) {
    stop("Inga stops hittades inom angiven polygon — kontrollera CRS och polygonens utbredning", call. = FALSE)
  }
  message(glue::glue("{length(stops_ids_geografi)} stops hittades inom polygon"))

  # 2. Filtrera nerifrån och upp, geografiskt
  stop_times <- gtfs_data$stop_times
  trips      <- gtfs_data$trips
  routes     <- gtfs_data$routes

  trips_ids_geografi <- unique(stop_times$trip_id[stop_times$stop_id %in% stops_ids_geografi])
  route_ids_geografi <- unique(trips$route_id[trips$trip_id %in% trips_ids_geografi])

  # 2b. Hitta operatörer som ska tas med i sin helhet, oavsett geografi
  route_ids_fullstandiga <- character(0)
  if (length(fullstandiga_operatorer) > 0) {
    op_txt <- paste(fullstandiga_operatorer, collapse = ", ")

    if (is.null(gtfs_data$agency)) {
      warning(glue::glue(
        "gtfs_data$agency saknas (NULL) — kan inte matcha operatörerna '{op_txt}'. ",
        "Inget undantag tillämpas, endast geografisk filtrering körs."
      ))
    } else {
      agency <- as.data.frame(gtfs_data$agency)
      # delsträngsmatch utan regex (operatörsnamn kan innehålla specialtecken)
      namn_lc <- tolower(agency$agency_name)
      traff <- Reduce(`|`, lapply(tolower(fullstandiga_operatorer),
                                  function(n) grepl(n, namn_lc, fixed = TRUE)))
      agency_ids_fullstandiga <- as.character(agency$agency_id[traff])

      if (length(agency_ids_fullstandiga) == 0) {
        warning(glue::glue(
          "Ingen operatör matchande '{op_txt}' hittades i gtfs_data$agency (tillgängliga namn: ",
          "{paste(unique(agency$agency_name), collapse = ', ')}) — inget undantag tillämpas."
        ))
      } else {
        # Jämför som character för att undvika typmismatch (t.ex. '01' vs 1)
        route_ids_fullstandiga <- routes$route_id[as.character(routes$agency_id) %in% agency_ids_fullstandiga]

        if (length(route_ids_fullstandiga) == 0) {
          warning(glue::glue(
            "Operatör(er) '{op_txt}' hittades i agency-tabellen ",
            "(agency_id: {paste(agency_ids_fullstandiga, collapse = ', ')}) men INGA routes matchade ",
            "i gtfs_data$routes$agency_id. Kontrollera att agency_id-typerna stämmer överens ",
            "mellan agency- och routes-tabellerna."
          ))
        } else {
          message(glue::glue(
            "{length(route_ids_fullstandiga)} routes tas med i sin helhet för: ",
            "{paste(unique(agency$agency_name[traff]), collapse = ', ')}"
          ))
        }
      }
    }
  }

  # 3. Slå ihop geografisk filtrering med de fullständiga operatörernas hela nät
  route_ids <- union(route_ids_geografi, route_ids_fullstandiga)
  trips_filtrerade <- rader(trips, trips$route_id %in% route_ids)
  trips_ids        <- unique(trips_filtrerade$trip_id)

  stop_times_filtrerade <- rader(stop_times, stop_times$trip_id %in% trips_ids)
  stops_ids <- union(stops_ids_geografi, unique(stop_times_filtrerade$stop_id))

  service_ids <- unique(trips_filtrerade$service_id)
  shape_ids   <- unique(trips_filtrerade$shape_id)
  agency_ids  <- unique(routes$agency_id[routes$route_id %in% route_ids])

  message(glue::glue(
    "{length(trips_ids)} trips | {length(route_ids)} routes | ",
    "{length(service_ids)} service_ids | {length(shape_ids)} shapes | ",
    "{length(stops_ids)} stops totalt (inkl. fullständiga operatörer)"
  ))

  # 4. Bygg filtrerat gtfs-objekt
  gtfs_filtrerat <- gtfs_data
  gtfs_filtrerat$stops      <- rader(gtfs_data$stops, gtfs_data$stops$stop_id %in% stops_ids)
  gtfs_filtrerat$stop_times <- stop_times_filtrerade
  gtfs_filtrerat$trips      <- trips_filtrerade
  gtfs_filtrerat$routes     <- rader(routes, routes$route_id %in% route_ids)

  if (!is.null(gtfs_data$agency)) {
    gtfs_filtrerat$agency <- rader(gtfs_data$agency, gtfs_data$agency$agency_id %in% agency_ids)
  }
  if (!is.null(gtfs_data$calendar_dates)) {
    gtfs_filtrerat$calendar_dates <- rader(gtfs_data$calendar_dates,
                                           gtfs_data$calendar_dates$service_id %in% service_ids)
  }
  # calendar filtreras för båda dataset (tidigare bara för sweden_3, vilket lämnade
  # kvar service_id utan turer i regional)
  if (!is.null(gtfs_data$calendar)) {
    gtfs_filtrerat$calendar <- rader(gtfs_data$calendar, gtfs_data$calendar$service_id %in% service_ids)
  }
  if (!is.null(gtfs_data$shapes)) {
    gtfs_filtrerat$shapes <- rader(gtfs_data$shapes, gtfs_data$shapes$shape_id %in% shape_ids)
  }
  # En transfer måste ha BÅDA stoppen kvar (tidigare "eller", vilket ger trasig
  # referensintegritet)
  if (!is.null(gtfs_data$transfers)) {
    gtfs_filtrerat$transfers <- rader(gtfs_data$transfers,
                                      gtfs_data$transfers$from_stop_id %in% stops_ids & gtfs_data$transfers$to_stop_id %in% stops_ids)
  }
  # attributions utan trip_id gäller hela feeden och behålls
  if (!is.null(gtfs_data$attributions)) {
    gtfs_filtrerat$attributions <- rader(gtfs_data$attributions,
                                         gtfs_data$attributions$trip_id %in% c(trips_ids, "", NA))
  }
  # feed_info gäller hela feeden och filtreras inte

  gtfs_filtrerat
}
