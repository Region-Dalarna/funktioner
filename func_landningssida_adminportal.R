# =============================================================================
# func_landningssida_adminportal.R
# -----------------------------------------------------------------------------
# Funktioner for att hantera VILKA appar/rapporter som visas pa Shiny-
# landningssidorna, samt vilken ikon och exkluderingsstatus varje app/rapport
# har. Anvands av adminportalens Exkludering-/Ikoner-flikar.
#
# Skiljer sig fran generera_landningssida.R (som BYGGER sjalva HTML-sidan
# utifran filsystemet + exkluderings-/ikonfiler) - den har filen ar
# ADMINGRANSSNITTET som skriver till den delade Postgres-databasen
# (adminshiny.landningssida_app / _exkludering / _ikon), som i sin tur
# antingen las lokalt (RP0003, ingen SSH/natverk behovs) eller synkas via
# LISTEN/NOTIFY till lokala filer pa wfalmitvs978 (se
# adminportal_lyssnare.R i serverdrift-repot).
#
# Anvands fran TVA olika sammanhang, darfor placerad har i funktioner-repot
# (publikt, sa source() via raw.githubusercontent.com fungerar utan
# autentisering) istallet for i adminportal eller serverdrift (privata):
#   1. Adminportal-appen (RP0003) - sourcas via en tunn loader-fil i
#      adminportal/R/func_landningssida_adminportal.R
#   2. Fristaende skript pa wfalmitvs978 (app-synk vid deploy/avpublicera,
#      nattligt cron-skyddsnat) - sourcas direkt via URL
#
# BADA sammanhangen ska ALLTID hamta samma version harifran - aldrig en
# lokal kopia - for att undvika att de glider isar over tid.
# =============================================================================


# ==============================================================================
# Funktioner for att hantera landningssidans exkluderingslistor och
# ikonkopplingar. Via databasen for "publik" (LISTEN/NOTIFY-lyssnaren pa
# wfalmitvs978 synkar automatiskt till lokala filer dar). Via lokala
# sudo-anrop for "intern" (adminportal kor redan PA RP0003, sa ingen
# databas-indirektion behovs dar - bara direkt anrop till skripten).
#
# ERSATTER de gamla SSH-baserade funktionerna med samma namn - dessa
# forutsatter INTE langre nagon SSH-konfiguration (~/.ssh/config).
# ==============================================================================

.landningssida_validera_target <- function(target) {
  giltiga <- c("publik", "intern")
  if (length(target) != 1 || !target %in% giltiga) {
    stop(
      "Ogiltigt varde for target: '", paste(target, collapse = ", "), "'.\n",
      "Giltiga varden ar: ", paste(giltiga, collapse = " eller "), ".",
      call. = FALSE
    )
  }
  target
}

.landningssida_validera_namn <- function(namn) {
  if (!is.character(namn) || length(namn) == 0 || any(!nzchar(namn))) {
    stop("namn maste vara en icke-tom character vector.", call. = FALSE)
  }
  ogiltiga <- namn[!grepl("^[a-zA-Z0-9_-]+$", namn)]
  if (length(ogiltiga) > 0) {
    stop("Ogiltiga app-namn (endast bokstaver/siffror/_/- tillatna): ",
         paste(ogiltiga, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# ---- Lokala sudo-anrop for intern (ingen databas/SSH inblandad) ------------
.landningssida_lokalt_anrop <- function(skript, args = character(0)) {
  resultat <- system2("sudo", args = c(skript, args), stdout = TRUE, stderr = TRUE)
  status <- attr(resultat, "status")
  if (!is.null(status) && status != 0) {
    stop("Lokalt anrop misslyckades (", skript, "):\n",
         paste(resultat, collapse = "\n"), call. = FALSE)
  }
  resultat
}

# ==============================================================================
# Kurerad lista over relevanta ikoner ur Tabler-biblioteket (https://tabler.io/icons).
# Fullstandig bladdring: valfri giltig "ti-xxx"-klass funkar aven om den inte
# star med har - detta ar bara en praktisk startpunkt, inte en begransning.
# ==============================================================================
.LANDNINGSSIDA_IKONER_KURERADE <- c(
  "ti-map"          = "Karta / geografi",
  "ti-users"        = "Befolkning / grupper",
  "ti-school"       = "Utbildning / skola",
  "ti-shield"       = "Brott / sakerhet",
  "ti-briefcase"    = "Naringsliv / arbete",
  "ti-building"     = "Organisation / myndighet",
  "ti-chart-bar"    = "Statistik / analys (stapeldiagram)",
  "ti-chart-line"   = "Statistik / analys (linjediagram)",
  "ti-chart-pie"    = "Statistik / analys (cirkeldiagram)",
  "ti-virus"        = "Epidemiologi / halsa",
  "ti-heart"        = "Halsa / vard",
  "ti-home"         = "Bostad / hushall",
  "ti-car"          = "Transport / trafik",
  "ti-bus"          = "Kollektivtrafik",
  "ti-leaf"         = "Miljo / hallbarhet",
  "ti-coin"         = "Ekonomi",
  "ti-file-text"    = "Rapport / dokument (standard for rapporter)",
  "ti-book"         = "Utredning / kunskap",
  "ti-calendar"     = "Tidsserie / prognos",
  "ti-database"     = "Data / register",
  "ti-globe"        = "Internationellt / omvarld",
  "ti-tool"         = "Verktyg / admin",
  "ti-settings"     = "Installningar / konfiguration",
  "ti-apps"         = "Ovrigt (standard for appar)"
)

# ==============================================================================
# Validering av ikonklasser mot Tablers faktiska webfont-CSS. Hamtas en gang
# per app-session och cachas i minnet - inte vid varje enskild koppling.
# Om halntningen misslyckas (natverksglapp) tillats klassen optimistiskt,
# med en varning i loggen, sa ett tillfalligt CDN-problem aldrig blockerar
# en admin fran att spara en giltig ikon de redan vet fungerar.
# ==============================================================================

.ikon_cache_env <- new.env(parent = emptyenv())

.landningssida_hamta_giltiga_ikonklasser <- function() {
  if (!is.null(.ikon_cache_env$klasser)) return(.ikon_cache_env$klasser)

  css_url <- "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@latest/dist/tabler-icons.min.css"
  css <- tryCatch(
    paste(readLines(css_url, warn = FALSE), collapse = "\n"),
    error = function(e) NULL
  )
  if (is.null(css)) return(NULL)

  matchningar <- regmatches(css, gregexpr("\\.ti-[a-z0-9-]+(?=:before)", css, perl = TRUE))[[1]]
  klasser <- unique(sub("^\\.", "", matchningar))
  .ikon_cache_env$klasser <- klasser
  klasser
}

.landningssida_ikon_giltig <- function(ikon) {
  giltiga <- .landningssida_hamta_giltiga_ikonklasser()
  if (is.null(giltiga)) {
    warning("Kunde inte hamta Tablers ikonlista (natverksfel) - tillater '", ikon, "' optimistiskt.")
    return(TRUE)
  }
  ikon %in% giltiga
}

# ==============================================================================
# Exkludering
# ==============================================================================

#' Lista bara det som star i exkluderingslistan
#' @param target "publik" eller "intern"
landningssida_lista_exkluderade <- function(target = c("publik", "intern")) {
  target <- .landningssida_validera_target(target)

  if (target == "publik") {
    con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    rader <- DBI::dbGetQuery(con, "
      SELECT namn FROM adminshiny.landningssida_exkludering
      WHERE server = 'publik' ORDER BY namn
    ")$namn
  } else {
    rader <- .landningssida_lokalt_anrop("/usr/local/bin/hantera_exkludering.sh", "lista")
    rader <- rader[nzchar(trimws(rader))]
  }

  if (length(rader) == 0) {
    message("Exkluderingslistan for ", target, " ar tom.")
    return(invisible(character(0)))
  }
  rader
}

#' Lagg till en eller flera appar/rapporter i exkluderingslistan
#' @param target "publik" eller "intern"
#' @param namn character vector med mappnamn som ska exkluderas
#' @param andrad_av valfri identifierare for vem som gjorde andringen (bara publik, loggas i DB)
landningssida_exkludera <- function(target = c("publik", "intern"), namn, andrad_av = NA_character_) {
  target <- .landningssida_validera_target(target)
  .landningssida_validera_namn(namn)
  
  con <- shiny_uppkoppling_skriv(db_name = "sekretess", db_user = "shiny_skriv_sekretess")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (n in namn) {
    DBI::dbExecute(con, "
      INSERT INTO adminshiny.landningssida_exkludering (server, namn, tillagd_av)
      VALUES ($1, $2, $3)
      ON CONFLICT (server, namn) DO NOTHING
    ", params = list(target, n, andrad_av))
  }
  
  if (target == "intern") {
    resultat <- system2("/usr/local/bin/generera_landningssida.sh", stdout = TRUE, stderr = TRUE)
    status <- attr(resultat, "status")
    if (!is.null(status) && status != 0) {
      stop("generera_landningssida.sh misslyckades: ", paste(resultat, collapse = "\n"), call. = FALSE)
    }
    message(length(namn), " app(ar) tillagda i exkluderingslistan (intern). Landningssidan ar regenererad.")
  } else {
    message(length(namn), " app(ar) tillagda i exkluderingslistan (publik). ",
            "Andringen synkas automatiskt av adminportal-lyssnare-tjansten inom nagra sekunder.")
  }
  invisible(TRUE)
}

#' Ta bort en eller flera appar/rapporter fran exkluderingslistan
#' @param target "publik" eller "intern"
#' @param namn character vector med mappnamn som inte langre ska exkluderas
landningssida_inkludera <- function(target = c("publik", "intern"), namn) {
  target <- .landningssida_validera_target(target)
  .landningssida_validera_namn(namn)
  
  con <- shiny_uppkoppling_skriv(db_name = "sekretess", db_user = "shiny_skriv_sekretess")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  borttagna <- 0
  for (n in namn) {
    borttagna <- borttagna + DBI::dbExecute(con, "
      DELETE FROM adminshiny.landningssida_exkludering
      WHERE server = $1 AND namn = $2
    ", params = list(target, n))
  }
  
  if (target == "intern") {
    resultat <- system2("/usr/local/bin/generera_landningssida.sh", stdout = TRUE, stderr = TRUE)
    status <- attr(resultat, "status")
    if (!is.null(status) && status != 0) {
      stop("generera_landningssida.sh misslyckades: ", paste(resultat, collapse = "\n"), call. = FALSE)
    }
    message(borttagna, " app(ar) borttagna fran exkluderingslistan (intern). Landningssidan ar regenererad.")
  } else {
    message(borttagna, " app(ar) borttagna fran exkluderingslistan (publik). ",
            "Andringen synkas automatiskt inom nagra sekunder.")
  }
  invisible(TRUE)
}

# ==============================================================================
# Ikoner
# ==============================================================================

#' Lista aktuella ikonkopplingar (bara overrides, inte standardgissningar)
#' @param target "publik" eller "intern"
landningssida_ikoner_lista <- function(target = c("publik", "intern")) {
  target <- .landningssida_validera_target(target)

  if (target == "publik") {
    con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    df <- DBI::dbGetQuery(con, "
      SELECT namn, ikon FROM adminshiny.landningssida_ikon
      WHERE server = 'publik' ORDER BY namn
    ")
  } else {
    rader <- .landningssida_lokalt_anrop("/usr/local/bin/hantera_ikon.sh", "lista_overrides")
    rader <- rader[grepl("=", rader)]
    if (length(rader) == 0) {
      df <- data.frame(namn = character(0), ikon = character(0))
    } else {
      delar <- strsplit(rader, "=")
      df <- data.frame(
        namn = vapply(delar, `[`, character(1), 1),
        ikon = vapply(delar, `[`, character(1), 2),
        stringsAsFactors = FALSE
      )
    }
  }

  if (nrow(df) == 0) {
    message("Inga manuella ikonkopplingar for ", target, ".")
  }
  df
}

#' Koppla en specifik ikon till en app eller rapport
#' @param target "publik" eller "intern"
#' @param namn mappnamnet
#' @param ikon en giltig Tabler-ikonklass, t.ex. "ti-map"
#' @param andrad_av valfri identifierare (bara publik, loggas i DB)
landningssida_ikoner_koppla <- function(target = c("publik", "intern"), namn, ikon, andrad_av = NA_character_) {
  target <- .landningssida_validera_target(target)
  stopifnot(is.character(namn), length(namn) == 1, nzchar(namn))
  stopifnot(is.character(ikon), length(ikon) == 1, nzchar(ikon))
  if (!grepl("^ti-", ikon)) {
    stop("Ikonklassen maste borja med 'ti-' (t.ex. 'ti-map'). Fick: '", ikon, "'", call. = FALSE)
  }
  if (!.landningssida_ikon_giltig(ikon)) {
    stop("Okand ikonklass: '", ikon, "'. Kontrollera stavningen pa https://tabler.io/icons.", call. = FALSE)
  }
  
  con <- shiny_uppkoppling_skriv(db_name = "sekretess", db_user = "shiny_skriv_sekretess")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO adminshiny.landningssida_ikon (server, namn, ikon, satt_av)
    VALUES ($1, $2, $3, $4)
    ON CONFLICT (server, namn) DO UPDATE SET ikon = EXCLUDED.ikon, satt_tid = now()
  ", params = list(target, namn, ikon, andrad_av))
  
  if (target == "intern") {
    resultat <- system2("/usr/local/bin/generera_landningssida.sh", stdout = TRUE, stderr = TRUE)
    status <- attr(resultat, "status")
    if (!is.null(status) && status != 0) {
      stop("generera_landningssida.sh misslyckades: ", paste(resultat, collapse = "\n"), call. = FALSE)
    }
    message("Ikon kopplad (intern): ", namn, " -> ", ikon, ". Landningssidan ar regenererad.")
  } else {
    message("Ikon kopplad (publik): ", namn, " -> ", ikon,
            ". Andringen synkas automatiskt inom nagra sekunder.")
  }
  invisible(TRUE)
}

#' Ta bort en manuell ikonkoppling (atergar till standardgissningen)
#' @param target "publik" eller "intern"
#' @param namn mappnamnet vars koppling ska tas bort
landningssida_ikoner_ta_bort_koppling <- function(target = c("publik", "intern"), namn) {
  target <- .landningssida_validera_target(target)
  .landningssida_validera_namn(namn)
  
  con <- shiny_uppkoppling_skriv(db_name = "sekretess", db_user = "shiny_skriv_sekretess")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  borttagna <- 0
  for (n in namn) {
    borttagna <- borttagna + DBI::dbExecute(con, "
      DELETE FROM adminshiny.landningssida_ikon
      WHERE server = $1 AND namn = $2
    ", params = list(target, n))
  }
  
  if (target == "intern") {
    resultat <- system2("/usr/local/bin/generera_landningssida.sh", stdout = TRUE, stderr = TRUE)
    status <- attr(resultat, "status")
    if (!is.null(status) && status != 0) {
      stop("generera_landningssida.sh misslyckades: ", paste(resultat, collapse = "\n"), call. = FALSE)
    }
    message(borttagna, " ikonkoppling(ar) borttagna (intern). Landningssidan ar regenererad.")
  } else {
    message(borttagna, " ikonkoppling(ar) borttagna (publik). ",
            "Andringen synkas automatiskt inom nagra sekunder.")
  }
  invisible(TRUE)
}

# ==============================================================================
# Synkar landningssida_app mot verkliga filsystemet pa DENNA server. Kors
# lokalt (aldrig over natverk) - vid deploy/avpublicera, och som nattligt
# skyddsnat via cron for appar vars repo annu inte har det nya deploy.yml-
# steget.
#
# Skriver ALDRIG till landningssida_exkludering/landningssida_ikon - bara
# app-existens och status. Se separat diskussion om varfor de halls atskilda.
# ==============================================================================
landningssida_synka_app_lista <- function(target = c("publik", "intern"),
                                          shiny_rot = "/srv/shiny-server",
                                          rapport_rot = "/srv/rapporter") {
  target <- .landningssida_validera_target(target)

  hittade <- list()

  if (dir.exists(shiny_rot)) {
    appmappar <- list.dirs(shiny_rot, recursive = FALSE, full.names = FALSE)
    appmappar <- appmappar[!startsWith(appmappar, ".")]
    appmappar <- appmappar[vapply(appmappar, function(m) {
      any(file.exists(file.path(shiny_rot, m, c("ui.R", "app.R"))))
    }, logical(1))]
    if (length(appmappar) > 0) {
      hittade[[length(hittade) + 1]] <- data.frame(namn = appmappar, typ = "app", stringsAsFactors = FALSE)
    }
  }

  if (dir.exists(rapport_rot)) {
    rapportmappar <- list.dirs(rapport_rot, recursive = FALSE, full.names = FALSE)
    rapportmappar <- rapportmappar[!startsWith(rapportmappar, ".")]
    rapportmappar <- rapportmappar[file.exists(file.path(rapport_rot, rapportmappar, "index.html"))]
    if (length(rapportmappar) > 0) {
      hittade[[length(hittade) + 1]] <- data.frame(namn = rapportmappar, typ = "rapport", stringsAsFactors = FALSE)
    }
  }

  funna <- if (length(hittade) > 0) do.call(rbind, hittade) else data.frame(namn = character(0), typ = character(0))

  con <- shiny_uppkoppling_skriv(db_name = "sekretess", db_user = "shiny_skriv_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen for app-synk.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # ---- Lagg till/uppdatera funna appar och rapporter --------------------------
  tillagda <- 0
  aterkomna <- 0
  for (i in seq_len(nrow(funna))) {
    resultat <- DBI::dbGetQuery(con, "
      INSERT INTO adminshiny.landningssida_app (server, namn, typ, status, senast_sedd)
      VALUES ($1, $2, $3, 'aktiv', now())
      ON CONFLICT (server, namn) DO UPDATE
        SET status = 'aktiv', senast_sedd = now(), typ = EXCLUDED.typ
      RETURNING (xmax = 0) AS ny_rad
    ", params = list(target, funna$namn[i], funna$typ[i]))
    if (isTRUE(resultat$ny_rad)) tillagda <- tillagda + 1 else aterkomna <- aterkomna + 1
  }

  # ---- Markera som borttagna de som tidigare var aktiva men inte langre finns ----
  # Loop med skalar $1/$2-bindning per rad - samma sakra monster som
  # landningssida_inkludera(), for att undvika array-bindningsbuggen fran
  # DELETE ... = ANY($1).
  aktiva_i_db <- DBI::dbGetQuery(con, "
    SELECT namn FROM adminshiny.landningssida_app
    WHERE server = $1 AND status = 'aktiv'
  ", params = list(target))$namn

  saknas <- setdiff(aktiva_i_db, funna$namn)
  for (namn in saknas) {
    DBI::dbExecute(con, "
      UPDATE adminshiny.landningssida_app
      SET status = 'borttagen', borttagen_tid = now()
      WHERE server = $1 AND namn = $2
    ", params = list(target, namn))
  }

  message("App-synk (", target, "): ", nrow(funna), " funna (", tillagda, " nya), ",
          length(saknas), " markerade borttagna.")
  invisible(list(funna = nrow(funna), nya = tillagda, borttagna = length(saknas)))
}

#' Hamta en samlad oversikt over appar/rapporter for en server, med
#' exkluderingsstatus och ikon som kolumner. Anvands av "Appar och
#' rapporter"-fliken.
#'
#' @param target "publik" eller "intern"
#' @param visa_borttagna om TRUE, inkludera aven status = 'borttagen'
#'   (standard: FALSE, visa bara aktiva - enligt tidigare beslut)
landningssida_app_oversikt <- function(target = c("publik", "intern"), visa_borttagna = FALSE) {
  target <- .landningssida_validera_target(target)
  
  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  status_villkor <- if (visa_borttagna) "" else "AND a.status = 'aktiv'"
  
  DBI::dbGetQuery(con, sprintf("
    SELECT
      a.namn,
      a.typ,
      a.status,
      a.senast_sedd,
      (e.namn IS NOT NULL) AS exkluderad,
      i.ikon
    FROM adminshiny.landningssida_app a
    LEFT JOIN adminshiny.landningssida_exkludering e
      ON e.server = a.server AND e.namn = a.namn
    LEFT JOIN adminshiny.landningssida_ikon i
      ON i.server = a.server AND i.namn = a.namn
    WHERE a.server = $1 %s
    ORDER BY a.typ, a.namn
  ", status_villkor), params = list(target))
}