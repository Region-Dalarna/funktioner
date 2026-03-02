
if (!require("pacman")) install.packages("pacman")
p_load(sf,
       data.table,
       tcltk,
       rio,
       glue,
       terra,
       openxlsx,
       mapview,
       RPostgres,
       keyring,
       httr,
       tidyverse)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")

options(scipen = 999)

# ===================================== hantera GIS i R ===============================================

kartifiera <- function(skickad_df, 
                       geom_nyckel, 
                       tatortspunkter = TRUE                   # används bara om det är tätorter, om TRUE returneras punkter, annars polygoner
){
  
  kartifiera_regionkoder <- unique(skickad_df[[geom_nyckel]])
  geom_nyckel_langd <- nchar(kartifiera_regionkoder) %>% unique()
  
  if (length(geom_nyckel_langd) > 1) {
    print("Skickad df:s geom_nyckel kan bara innehålla värden av samma typ. Kontrollera att så är fallet och försök igen.") 
  } else {
    
    # här bestäms vilken karttyp vi har att göra med
    if (geom_nyckel_langd == 2) kartifiera_karttyp <- "lan"
    if (geom_nyckel_langd == 4 & all(str_detect(kartifiera_regionkoder, "^[:digit:]+$"))) kartifiera_karttyp <- "kommun" 
    if (geom_nyckel_langd == 4 & !all(str_detect(kartifiera_regionkoder, "^[:digit:]+$"))) kartifiera_karttyp <- "nuts2"
    #if (geom_nyckel_langd == 9 & !all(str_detect(kartifiera_regionkoder, "^[:digit:]+$"))) kartifiera_karttyp <- "deso"
    if (geom_nyckel_langd == 9 & all(str_sub(kartifiera_regionkoder, 5, 5) %in% c("A", "B", "C"))) kartifiera_karttyp <- "deso"
    #if (geom_nyckel_langd == 8 & !all(str_detect(kartifiera_regionkoder, "^[:digit:]+$"))) kartifiera_karttyp <- "regso"
    if (geom_nyckel_langd == 8 & all(str_sub(kartifiera_regionkoder, 5, 5) %in% c("R"))) kartifiera_karttyp <- "regso"
    if (geom_nyckel_langd == 9 & all(str_sub(kartifiera_regionkoder, 5, 5) %in% c("T"))) kartifiera_karttyp <- "tatorter"
    
    if (kartifiera_karttyp == "tatorter" & tatortspunkter) kartifiera_karttyp <- "tatortspunkter"
    
    # vi hämtar gislagret för aktuell karttyp, för de geom_nyckelkoder som skickats med
    gis_lager <- hamta_karta(karttyp = kartifiera_karttyp, regionkoder = kartifiera_regionkoder)
    
    # här lägger vi till rader (dvs. tabeller) som ska vara hämtbara från geodatabasen med hamta_karta()-funktionen
    tabell_df <- hamta_karttabell()
    
    vald_karta <- tabell_df %>%
      mutate(ord_match = map_lgl(sokord, ~ kartifiera_karttyp %in% .)) %>% 
      filter(ord_match)
    
    # om medskickade kartyp inte finns bland sökorden får pg_tabell värdet "finns ej" och då körs inte skriptet nedan
    if (nrow(vald_karta) == 0) pg_tab_idkol <- "finns ej" else pg_tab_idkol <- vald_karta$id_kol
    
    join_sf <- skickad_df %>% 
      left_join(gis_lager, by = setNames(pg_tab_idkol, geom_nyckel)) %>% 
      st_as_sf()
    
    return(join_sf)
  } # slut if-sats om det finns fler längder på geom_nyckel
}

hamta_karttabell <- function(){
  
  # här lägger vi till nya kolumner om det behövs
  kolumn_namn <- c("namn", "id_kol", "lankol", "kommunkol", "sokord")
  
  antal_kol <- length(kolumn_namn)                            # räkna kolumnnamn i vektorn som vi skapar ovan
  karttabell_df <- as.data.frame(matrix(nrow = 0, ncol = antal_kol)) %>%             # skapa df med 0 rader och lika många kolumner som vi har kolumnnamn ovan
    setNames(kolumn_namn) %>%                                              # döp kolumnnamn efter vektorn vi skapade ovan
    mutate(across(1:(antal_kol-1), as.character),                          # alla kolumner ska vara text, utom sista kolumnen som ska vara en lista med sökord
           sokord = sokord %>% as.list())                                  # som vi initierar här
  
  # här lägger vi till rader (dvs. tabeller) som ska vara hämtbara från geodatabasen med hamta_karta()-funktionen
  karttabell_df <- karttabell_df %>%  
    add_row(namn = "kommun_scb", id_kol = "knkod", lankol = "lanskod_tx", kommunkol = "knkod", sokord = list(c("kommun", "kommuner", "kommunpolygoner"))) %>% 
    add_row(namn = "kommun_lm", id_kol = "kommunkod", lankol = "lankod", kommunkol = "kommunkod", sokord = list(c("kommun_lm", "kommuner_lm", "kommunpolygoner_lm"))) %>% 
    add_row(namn = "lan_scb", id_kol = "lnkod", lankol = "lnkod", kommunkol = NA, sokord = list(c("lan", "lanspolygoner"))) %>% 
    add_row(namn = "lan_lm", id_kol = "lankod", lankol = "lankod", kommunkol = NA, sokord = list(c("lan_lm", "lanspolygoner_lm"))) %>% 
    add_row(namn = "rike_lm", id_kol = "landskod", lankol = NA, kommunkol = NA, sokord = list(c("rike_lm", "rike", "riket", "Sverige"))) %>% 
    add_row(namn = "tatorter", id_kol = "tatortskod", lankol = "lan", kommunkol = "kommun", sokord = list(c("tatort", "tätort", "tatorter", "tätorter", "tatortspolygoner", "tätortspolygoner"))) %>% 
    add_row(namn = "tatortspunkter", id_kol = "tatortskod", lankol = "lan", kommunkol = "kommun", sokord = list(c("tatortspunkter", "tätortspunkter"))) %>% 
    add_row(namn = "regso", id_kol = "regsokod",  lankol = "lanskod", kommunkol = "kommunkod", sokord = list(c("regso", "regsopolygoner"))) %>% 
    add_row(namn = "deso", id_kol = "deso", lankol = "lanskod", kommunkol = "kommunkod", sokord = list(c("deso", "desopolygoner"))) %>% 
    add_row(namn = "distrikt", id_kol = "distriktskod", lankol = "lankod", kommunkol = "kommunkod", sokord = list(c("distrikt"))) %>% 
    add_row(namn = "nuts2", id_kol = "nuts_id", lankol = "cntr_code", kommunkol = "geo", sokord = list(c("nuts2", "nuts2-områden"))) %>% 
    add_row(namn = "nuts3", id_kol = "nuts_id", lankol = "cntr_code", kommunkol = "geo", sokord = list(c("nuts3", "nuts3-områden"))) %>% 
    add_row(namn = "laregion_scb", id_kol = "lakod", lankol = "lan", kommunkol = "kommun", sokord = list(c("la", "laomraden", "la-omraden", "la-områden", "la-omraden"))) %>% 
    add_row(namn = "varlden", id_kol = "Landskod", lankol = NA, kommunkol = NA, sokord = list(c("varlden", "varldskarta", "världen", "världskarta"))) %>% 
    add_row(namn = "varldsdelar", id_kol = "Landskod", lankol = NA, kommunkol = NA, sokord = list(c("varldsdelskarta", "varldsdelar", "världsdelskarta", "världsdelar")))
  
  return(karttabell_df)
}

hamta_karta <- function(karttyp = "kommuner", regionkoder = NA, tabellnamn = NA) {
  
  # här lägger vi till rader (dvs. tabeller) som ska vara hämtbara från geodatabasen med hamta_karta()-funktionen
  tabell_df <- hamta_karttabell()
  
  df_rad <- suppressWarnings(str_which(tabell_df$sokord, karttyp))             # vi letar upp den rad som parametern karrtyp finns på
  
  # om det blir fler träffar, kolla vilken karta där sökordet stämmer helt med karttyp
  if (length(df_rad) > 0) df_rad <- df_rad[map_lgl(df_rad, ~ karttyp %in% tabell_df$sokord[[.x]])]
  
  # om medskickade kartyp inte finns bland sökorden får pg_tabell värdet "finns ej" och då körs inte skriptet nedan
  if (length(df_rad) == 0) pg_tabell <- "finns ej" else pg_tabell <- tabell_df$namn[df_rad] 
  
  # kontrollera om karttypen som skickats med i funktionen finns, om inte så körs inte skriptet nedan utan ett felmeddelande visas istället
  if (pg_tabell != "finns ej"){
    
    # skriv query utifrån medskickade regionkoder, om ingen är medskickad görs en query för att hämta allt
    if (all(!is.na(regionkoder)) & all(regionkoder != "00")) {
      kommunkoder <- regionkoder[nchar(regionkoder) == 4]
      if (karttyp == "nuts2") lanskoder <- regionkoder[nchar(regionkoder) == 2]
      lanskoder <- regionkoder[nchar(regionkoder) == 2 & regionkoder != "00"]
      if (karttyp == "nuts2") kommunkoder <- regionkoder[nchar(regionkoder) == 4]
    } else {
      kommunkoder <- NULL
      lanskoder <- NULL
    }
    # if (is.na(kommunkoder)) kommunkoder <- NULL
    # if (is.na(lanskoder)) lanskoder <- NULL 
    # 
    grundquery <- paste0("SELECT * FROM karta.", pg_tabell) 
    
    #if ((length(kommunkoder) == 0) & (length(lanskoder) == 0)) skickad_query <- paste0(grundquery, ";") else {
    if ((length(kommunkoder) == 0) & (length(lanskoder) == 0)) skickad_query <- NA else {  
      # det finns lan- eller kommunkoder, så vi lägger på ett WHERE på grundqueryn
      #skickad_query <- paste0(grundquery, " WHERE ")
      skickad_query <- " WHERE "
      
      # kolla om det finns länskoder i regionkoder, om så lägger vi på länskoder i queryn
      if (length(lanskoder) != 0 & !is.na(tabell_df$lankol[df_rad])) {
        skickad_query <- paste0(skickad_query, tabell_df$lankol[df_rad], " IN (", paste0("'", lanskoder, "'", collapse = ", "), ")")
      }
      
      # kolla om det finns både läns- och kommunkoder i regionkoder, i så fall lägger vi till ett OR mellan 
      # de båda IN-satserna - då måste tabellen ha både kommun- och länskod
      if ((length(lanskoder) != 0 & !is.na(tabell_df$lankol[df_rad])) & (length(kommunkoder) != 0 & !is.na(tabell_df$kommunkol[df_rad]))) mellanquery <- " OR " else mellanquery <- ""
      
      if (length(kommunkoder) != 0 & !is.na(tabell_df$kommunkol[df_rad])){     # om det finns kommunkoder, lägg på det på tidigare query
        skickad_query <- paste0(skickad_query, mellanquery, tabell_df$kommunkol[df_rad], " IN (", paste0("'", kommunkoder, "'", collapse = ", "), ");")
      } else {                           # om det inte finns kommunkoder, avsluta med ett semikolon
        skickad_query <- paste0(skickad_query, ";") 
      }   
      
    } # slut if-sats för om regionkoder är medskickade, om inte så hämtas alla regioner 
    # query klar, använd inloggningsuppgifter med keyring och skicka med vår serveradress 
    
    retur_sf <- suppress_specific_warning(
      postgis_postgistabell_till_sf(schema = "karta",
                                    tabell = pg_tabell,
                                    query = skickad_query),
      "Invalid time zone 'UTC', falling back to local time.")
    
    return(retur_sf)
    
    
  } else {
    warning(paste0("Karttypen ", karttyp, " finns inte i databasen."))
  } # slut if-sats karttyp
  
} # slut funktion

# ========================================== hantera rutor med GIS ========================================================

sf_fran_df_med_x_y_kol <- function(skickad_df, 
                                   x_kol, 
                                   y_kol,
                                   rutstorlek = NA,              # om man vill ange själv, annars kontrolleras för det automatiskt.
                                   polygonlager = TRUE,          # polygonlager = FALSE -> punktlager
                                   vald_crs = 3006){
  
  if(is.na(rutstorlek)) rutstorlek = rutstorlek_estimera(skickad_df[[x_kol]], skickad_df[[y_kol]])
  
  
  
  retur_sf <- intern_funktion_sf_skapa_fran_df_med_rutkolumner(skickad_df = skickad_df, x_kol = x_kol, 
                                                               y_kol = y_kol, rutstorlek = rutstorlek, vald_crs = vald_crs)
  
  if (polygonlager) retur_sf <- st_buffer(retur_sf,(rutstorlek/2), endCapStyle = "SQUARE")
  
  return(retur_sf)
  
} # slut funktion



intern_funktion_sf_skapa_fran_df_med_rutkolumner <- function(skickad_df, x_kol, y_kol, rutstorlek = NA, vald_crs = 3006){
  
  if (is.na(rutstorlek)) rutstorlek <- rutstorlek_estimera(skickad_df[[x_kol]], skickad_df[[y_kol]])
  
  # skapa en punktgeometri av x- och y-kolumnerna där koordinaten är nedre vänstra hörnet
  retur_sf <- skickad_df %>% 
    mutate(
      across(all_of(c(x_kol, y_kol)), as.character),
      x_temp = if_else(nchar(!!sym(x_kol)) == 7 & nchar(!!sym(y_kol)) == 6, !!sym(y_kol), !!sym(x_kol)),
      y_temp = if_else(nchar(!!sym(x_kol)) == 7 & nchar(!!sym(y_kol)) == 6, !!sym(x_kol), !!sym(y_kol)),
      x_ny = as.numeric(x_temp) + as.numeric(rutstorlek)/2,
      y_ny = as.numeric(y_temp) + as.numeric(rutstorlek)/2) %>%
    select(-c(x_temp, y_temp)) %>% 
    st_as_sf(coords = c("x_ny", "y_ny"), crs = vald_crs) %>% 
    st_cast("POINT")
  
  return(retur_sf)
  
}


rutstorlek_estimera <- function(x, y) {
  
  # Kombinera x- och y-koordinaterna till en enda vektor
  coords <- c(x ,y) %>% as.numeric()
  
  # Kontrollera om det finns något värde som slutar på 100, 200, 300 eller 400
  if (any(coords %% 1000 %in% c(100, 200, 300, 400))) {
    return(100)
  }
  
  # Kontrollera om det finns värden som slutar på 500 och på 1000
  if (any(coords %% 1000 == 500) && any(coords %% 1000 == 0)) {
    return(500)
  }
  
  # Om alla värden slutar på 1000
  if (all(coords %% 1000 == 000)) {
    return(1000)
  }
  
  # Default, if no match is found (this case shouldn't happen given your rules)
  return(NA)
}

berakna_mittpunkter <- function(df, xruta, yruta, rutstorlek, 
                                xkolnamn = "mitt_x", ykolnamn = "mitt_y"){
  
  # Denna funktion beräknar mittpunkter för två kolumner med x- och y-koordinater i 
  # textform, om koordinaterna är i nedre vänstra hörnet (som SCB:s rutor).
  #
  # Funktionen  behöver: 
  # - en dataframe som innehåller kolumner med x- och y-koordinater
  # - namn på x- och y-kolumnerna som text
  # - ett numeriskt värde för rutstorleken
  # - namn för de nya x- och y-kolumnerna med mittpunkter, annars döps de till "mitt_x" och "mitt_y"
  #
  # Retur: en df som är likadan som den som skickades men med 2 nya kolumner som
  #        innehåller mittpunkter för x- och y-koordinaten
  #
  
  # beräkna de nya kolumnerna
  df[xkolnamn] <- df[xruta]+(rutstorlek/2)
  df[ykolnamn] <- df[yruta]+(rutstorlek/2)
  # flytta de nya kolumnerna och lägg dem efter x- och y-kolumnerna
  df <- df %>% 
    relocate(all_of(xkolnamn), .after = all_of(yruta)) %>% 
    relocate(all_of(ykolnamn), .after = all_of(xkolnamn))
  
  return(df)
}

# ============================================ geometriska funktioner =================================================

st_largest_ring <- function(x) {
  
  # Kod hämtad från: https://github.com/r-spatial/sf/issues/1302#issuecomment-606473789
  # Används av funktionen .st_centroid_within_geo nedan
  #
  #' Find largest ring of a multipolygon
  #'
  #' Assumes dealing with polygons
  #'
  #' @param x (sf object of polygons)
  #'
  #' @return (sf object) replaced with centroids
  #'
  #' @seealso sf:::largest_ring()
  #'
  
  if (nrow(x) < 1)
    return(x)
  
  seq(1, nrow(x)) %>%
    lapply(function(i){
      x[i, ] %>%
        sf::st_set_agr("identity") %>%
        sf::st_cast("POLYGON") %>%
        mutate(st_area = sf::st_area(st_geometry(.))) %>%
        top_n(1, st_area) %>%
        select(-st_area)
    }) %>%
    data.table::rbindlist() %>%
    sf::st_as_sf()
}



st_centroid_within_geo <- function(
    x
    , ensure_within = TRUE                   # tvingar centroiden att vara innanför polygonen
    , of_largest_polygon = TRUE              # säkerställer att centroiden är i den största polygonen om det finns fler (om det är en multipolygon)
) {
  
  # Kod hämtad från: https://github.com/r-spatial/sf/issues/1302#issuecomment-606473789
  #' tar ut centroider i polygoner
  #'
  #' reference:
  #' https://stackoverflow.com/questions/52522872/r-sf-package-centroid-within-polygon
  #' https://stackoverflow.com/questions/44327994/calculate-centroid-within-inside-a-spatialpolygon
  #'
  #' @param x (sf) spatial polygon
  #' @param ensure_within (logical) TRUE to ensure point within polygon.
  #' @param of_largest_polygon (logical) TRUE to use largest polygon when
  #'   determining centroid.
  #'
  #' Will return the usual geometric centroid when \code{ensure_within} is FALSE.
  #'
  #' Otherwise the point returned will be the geometric centroid if it lies within
  #' the polygon else a point determined by st_point_on_surface.
  #'
  #' In all cases an attempt is made to observe \code{of_largest_polygon} (see
  #' sf::st_centroid()).
  #'
  #' \code{of_largest_polygon} used when determining geometric centroid.  An
  #' effort to apply this idea to sf::st_point_on_surface() routine.
  #'
  #' @return (sf) spatial object with centroids within spatial feature.
  #'
  
  cx <- sf::st_centroid(
    sf::st_set_agr(x, "identity")
    , of_largest_polygon = of_largest_polygon
  )
  
  if (ensure_within) {
    
    i_within <- sf::st_within(cx, x, sparse = TRUE) %>%
      as.data.frame() %>%
      filter(row.id == col.id) %>%
      dplyr::pull(row.id)
    
    i_nwithin <- setdiff(seq(1, nrow(x)), i_within)
    
    sf::st_geometry(cx[i_nwithin, ]) <- (
      x[i_nwithin, ] %>% {
        if (of_largest_polygon == TRUE)
          st_largest_ring(.)
        else
          .
      } %>%
        sf::st_geometry() %>%
        sf::st_point_on_surface()
    )
  }
  
  return(cx)
}

skapa_linje_langs_med_punkter <- function(skickad_sf,                   # skickad_sf = skickat sf-objekt med punkter
                                          kol_ord,                      # kol_ord = den kolumn som innehåller nummer som rangordnar mellan vilka punkter som linjen ska dras, den dras i samma ordning som i denna kolumn
                                          names,                        # namn på punkterna om man vill ha med det (oklart om vi vill det)
                                          names_bara_startpunkt = TRUE  # om man bara vill ha namn från startpunkten, annars blir namnet "startnamn - slutnamn"
) {
  
  # skicka ett sf-objekt med punkter. En kolumn i objektet innehåller en numerisk kolumn utifrån vilken
  # linjer dras mellan punkterna, från första till andra punkten, från andra till tredje punkten osv.
  # till den sista punkten. Det går också att skicka med korrekt crs som ska vara samma som 
  
  skickad_sf_crs <- st_crs(skickad_sf)
  
  skickad_sf <- skickad_sf %>% arrange(!!as.symbol(kol_ord))
  
  # dataframe med ordningsföljd utifrån en kolumn (sorteras så linjer mellan punkter dras alltid i nummer ordning)
  idx <- data.frame(start = c(1:(nrow(skickad_sf)-1)),
                    end = c(2:nrow(skickad_sf))) 
  
  
  # cycle over the combinations
  for (i in seq_along(idx$start)) {
    
    # line object from two points
    
    start_punkt <- skickad_sf[idx$start[i], ] %>% st_coordinates() %>% sum()
    slut_punkt <- skickad_sf[idx$end[i], ] %>% st_coordinates() %>% sum()
    
    if (start_punkt >= slut_punkt){
      wrk_line  <- skickad_sf[c(idx$start[i], idx$end[i]), ] %>% 
        st_coordinates() %>% 
        st_linestring() %>% 
        st_sfc(crs = skickad_sf_crs)  
    } else {
      wrk_line  <- skickad_sf[c(idx$end[i], idx$start[i]), ] %>% 
        st_coordinates() %>% 
        st_linestring() %>% 
        st_sfc(crs = skickad_sf_crs)
    }
    
    # wrk_line  <- skickad_sf[c(idx$start[i], idx$end[i]), ] %>% 
    #   st_coordinates() %>% 
    #   st_linestring() %>% 
    #   st_sfc(crs = skickad_sf_crs)  
    
    # a single row of results dataframe
    
    # skapa etiketten, från till etikett om names_bara_startpunkt = FALSE, annars bara etikett för startpunkt
    label_varde <- ifelse(names_bara_startpunkt, pull(skickad_sf, names)[idx$start[i]], 
                          paste(pull(skickad_sf, names)[idx$start[i]], "-", pull(skickad_sf, names)[idx$end[i]]))
    
    line_data <- data.frame(
      start = pull(skickad_sf, kol_ord)[idx$start[i]],
      end = pull(skickad_sf, kol_ord)[idx$end[i]],
      label =label_varde,
      geometry = wrk_line
    )
    
    # bind results rows to a single object
    if (i == 1) {
      res <- line_data
      
    } else {
      res <- dplyr::bind_rows(res, line_data)
      
    } # /if - saving results
    
  } # /for
  
  # finalize function result
  res <- sf::st_as_sf(res, crs = skickad_sf_crs)
  
  return(res)
  
} # /function


# ============================================ läs in GIS-filer ==========================================================


las_gisfil_fran_zipfil_via_url <- function(skickad_url){
  
  # läs en gisfil som ligger i en zipfil direkt från url - packar upp 
  cur_tempfile <- tempfile()              # skapa temporär fil som vi laddar ner från url
  download.file(url = skickad_url, destfile = cur_tempfile)      # ladda ner fil från url till tempfil 
  out_directory <- tempfile()             # skapa outputmapp att spara uppackad zipfil till
  unzip(cur_tempfile, exdir = out_directory)        # packa upp fil från url till outputmapp
  
  retur_sf <- st_read(out_directory) #read_sf also works here
  return(retur_sf)
  
}

las_gisfil_fran_zipfil_via_sokvag <- function(skickad_sokvag) {
  
  # läs en gisfil direkt från en zipfil
  out_directory <- tempfile()                  # skapa outputmapp att spara uppackad zipfil till
  unzip(skickad_sokvag, exdir = out_directory)        # packa upp fil från skickad sökväg till outputmapp
  
  retur_sf <- st_read(out_directory) #read_sf also works here
  return(retur_sf)
  
}

unzip_zipfil_med_zipfiler <- function(skickad_url){
  
  # det här är en specialfunktion som används för att ladda ner polygoner för FA- och LA-regioner samt
  # läns- och kommungränser med kustgränser (blir snyggare kartor) de ligger som zipfiler i en zipfil
  # så man måste packa upp dessa i två steg
  cur_tempfile <- tempfile()
  download.file(url = skickad_url, destfile = cur_tempfile)
  out_directory <- tempfile()
  unzip(cur_tempfile, exdir = out_directory)
  
  zipfillista <- list.files(out_directory, full.names = TRUE)
  return(zipfillista)
  
}


# =================================== Supercross-funktioner =====================================


geopackage_skapa_fran_rutor_csv_xlsx_supercross <- function(sokvag_filnamn_vekt,         # vektor med full sökväg, dvs mapp + filnamn, kan vara flatten csv eller xlsx från Superross
                                                            byt_ut_c_mot_varde = 2.5,    # om det finns värden som är "..C" så byts dessa ut mot detta värde, vill man behålla "..C" så kör man NA här
                                                            output_mapp = NA             # om NA, samma som indatamapp
) {
  
  # 
  #
  # Skript för att läsa in csv-filer med rutor från Supercross och exportera en färdig geopackage-fil, där rutan är runt mittpunkten och
  # inte i nedre vänstra hörnet där koordinaten egentligen är. CSV-filen sparas som flattend CSV-fil i Supercross.
  #
  # 
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         readxl,
         sf)
  
  walk2(sokvag_filnamn_vekt, basename(sokvag_filnamn_vekt), function(full_sokvag, filnamn) {
    
    if (str_sub(filnamn, -4) == ".csv") {
      
      # extrahera enhet (dvs. befolkning eller sysselsatta) ur csv-filen från Supercross 
      inlas_typ <- readLines(full_sokvag, n = 5, encoding = "latin1") %>% .[1]
      rut_storlek <- str_extract(inlas_typ, '(?<=\").*?(?=\")') %>% 
        str_remove("SWEREF99") %>% 
        parse_number()
      antal_enhet <- if(str_detect(tolower(filnamn), "syss")) "syss" else "bef"
      ar_txt <- parse_number(filnamn)
      
      rut_txt <- if (rut_storlek == 1000) "km" else paste0(rut_storlek, "m")
      
      # här läser vi in själva csv-filen
      in_df <- read_csv(full_sokvag, col_names = TRUE, show_col_types = FALSE) %>%
        select(-any_of(c("Region", "Dagbefolkning"))) %>% 
        rename(rutid = 1, !!sym(antal_enhet) := 2) %>%
        mutate(rutid = rutid %>% as.character()) %>% 
        mutate(x_koord = as.character(as.numeric(str_sub(rutid, 1,6))+(rut_storlek/2)),                           # flytta x-koordinaten halva rutstorleken för att få en mittpunkt i rutan
               y_koord = as.character(as.numeric(str_sub(rutid, 7,13))+(rut_storlek/2)),                            # flytta y-koordinaten halva rutstorleken för att få en mittpunkt i rutan
               ar = ar_txt) %>% 
        filter(!!sym(antal_enhet) > 0) %>% 
        relocate(ar, .before = 1) %>% 
        relocate(!!sym(antal_enhet), .after = last_col())
    } else if (str_sub(filnamn, -5) == ".xlsx") {
      
      inlas_typ <- read_xlsx(full_sokvag, skip = 2, n_max = 1) %>% dplyr::pull()
      rut_storlek <- inlas_typ %>% 
        str_remove("SWEREF99") %>% 
        parse_number()
      antal_enhet <- if(str_detect(tolower(filnamn), "syss")) "syss" else "bef"
      ar_txt <- parse_number(filnamn %>% str_remove(as.character(rut_storlek)))
      
      rut_txt <- if (rut_storlek == 1000) "km" else paste0(rut_storlek, "m")
      
      in_df <- read_xlsx(full_sokvag) %>% 
        rename(rutid = 1, !!sym(antal_enhet) := 2) %>%
        filter(!is.na(!!sym(antal_enhet))) %>% 
        mutate(rutid = rutid %>% as.character()) %>% 
        mutate(x_koord = as.character(as.numeric(str_sub(rutid, 1,6))+(rut_storlek/2)),                           # flytta x-koordinaten halva rutstorleken för att få en mittpunkt i rutan
               y_koord = as.character(as.numeric(str_sub(rutid, 7,13))+(rut_storlek/2)),                            # flytta y-koordinaten halva rutstorleken för att få en mittpunkt i rutan
               ar = ar_txt) %>% 
        relocate(ar, .before = 1) %>% 
        relocate(!!sym(antal_enhet), .after = last_col())
      
      if (!is.na(byt_ut_c_mot_varde)) {
        in_df <- in_df %>% 
          mutate(!!sym(antal_enhet) := if_else(as.character(!!sym(antal_enhet)) == "..C", 
                                               as.character(byt_ut_c_mot_varde), 
                                               as.character(!!sym(antal_enhet))) %>%
                   as.numeric())
      } 
      
    } else {
      stop("Filen är inte en csv-fil eller xlsx-fil, denna funktion fungerar bara med dessa två format") 
    }
    
    if (is.na(output_mapp)) {
      utmapp <- dirname(full_sokvag)
    } else {
      utmapp <- output_mapp
    }
    
    # kontrollera att utmapp slutar med /, annars lägger vi till det på slutet
    utmapp <- if_else(str_ends(utmapp, "/"), utmapp, str_c(utmapp, "/"))
    
    gis_punkt <- st_as_sf(in_df, coords = c("x_koord", "y_koord"), crs = 3006)
    gis_export <- st_buffer(gis_punkt, (rut_storlek/2) , endCapStyle = "SQUARE")
    st_write(gis_export, paste0(utmapp, rut_txt, "rut_", antal_enhet, "_", ar_txt,  ".gpkg"), delete_dsn = TRUE)
  })
} # slut funktion



skapa_supercross_recode_fran_rutlager <- function(gis_lager,
                                                  rutid_kol = NA,      # finns en funktion för att hitta rutid-kolumnen men man kan skicka med den här 
                                                  recode_kol,
                                                  databastyp,          # "Syss belägenhet", "Bef belägenhet", "Flytt belägenhet", "Syss relationsbelägenhet", "Flytt relationsbelägenhet" 
                                                  outputmapp, 
                                                  outputfilnamn,
                                                  namn_pa_recode,      # vad recoden ska heta i Supercross
                                                  rutstorlek = NA,
                                                  lanskod = "20", 
                                                  header = c("HEADER", "\tVERSION 2", "\tUNICODE", "\tESCAPE_CHAR & ITEM_BY_CODE", "END HEADER", "")) {
  
  # Rutlager till recode-fil
  # Instruktioner
  # Denna algoritm skapar en recode-fil i textformat (.txt) från ett rutlager (polygon). Nya geografiska områden kan skapas av rutorna i ett rutlager och dessa områden måste sparas i en egen kolumn.
  # Således krävs två kolumner i ett polygonlager för att algoritmen ska kunna köras, en med RutID och en med de områden som ska bli recodes i Supercross. Några inställningar behöver göras för att textrecode-filen ska fungera att ladda in i Supercross på Mona.
  # 
  # Inställningar
  # Följande inställningar måste göras för att algoritmen ska kunna köras:
  #   Rutlager (polygon)
  # RutID-kolumn (i rutlagret)
  # Kolumn med områden som ska göras recode på (i rutlagret)
  # Rutstorlek
  # Typ av databas i Supercross
  # Första raderna i recode-filen (ska normalt sett inte ändras)
  # Prefix för RutID som används i recode-filen (normalt sett länskoden)
  # Utdatafil (välj var den skapade recodefilen ska heta och i vilken mapp den ska sparas)
  # 
  # Output
  # Utdata från algoritmen är en fil med en textrecode som kan laddas upp på Mona genom att välja sidan My Files från Monas inloggningsmeny och därefter Upload. När filen har laddats upp kan den hämtas i mappen InBox i Documents på den egna Mona-lagringsytan. Filen kan kopieras därifrån till valfri mapp och därifrån laddas in i Supercross som en textrecode via knappen Load som finns i Fields-dialogrutan.
  
  
  
  # kolla om bara en kolumn heter något med rut, i så fall är det rutid-kolumnen
  if (is.na(rutid_kol[1]) & sum(str_detect(tolower(names(gis_lager)), "rut")) == 1) rutid_kol <- names(gis_lager)[str_which(tolower(names(gis_lager)), "rut")]
  
  # om det inte bara är en kolumn som heter något med "rut", välj den första kolumnen i gis_lager som rutid-kolumn
  if (is.na(rutid_kol[1])) rutid_kol <- names(gis_lager)[1]
  
  # hitta rutstorlek genom att kontrollera tecken 11 i rutid-kolumnen. Om det är km-rutor är det bara "0" där, om det är 500-metersrutor är det bara "0" eller "5" där, och 
  # om det är 100 metersrutor kan alla siffror finnas där, bland annat "1", "2", "3", "4" som inte kan finnas i övriga rutor. Kan bli fel om man har väldigt få rutor men
  # sannolikheten att det ska hända är extremt liten, men då finns möjligheten att skicka med ett värde för rutstorlek
  if (is.na(rutstorlek)) {
    test_vekt <- gis_lager[[rutid_kol]] %>% str_sub(11,11)
    rutstorlek <- case_when(all(test_vekt == "0") ~ 1000,
                            all(test_vekt %in% c("0", "5")) ~ 500,
                            any(test_vekt %in% c("1", "2", "3", "4", "6", "7", "8", "9")) ~ 100)
  }
  
  # skapa databasvektor som fyller recodes utifrån vilken typ av databas vi har
  databas_vekt <- case_when(databastyp == "Syss belägenhet" ~ c("Rutid ", " m SWEREF99", "Personer", "kSaRutid", "_SWEREF99"),
                            databastyp == "Bef belägenhet" ~ c("Rutid ", " m SWEREF99", "fBBefolkning", "kBRutid", "_SWEREF99"),
                            databastyp == "Flytt belägenhet" ~ c("Rutid ", " m SWEREF99", "fFlyttning", "kFRutid", "_SWEREF99"),
                            databastyp == "Syss relationsbelägenhet" ~ c("RelationsRutid ", " m SWEREF99", "Personer", "kSaRutid", "_SWEREF99"),
                            databastyp == "Flytt relationsbelägenhet" ~ c("Rutid ", " m SWEREF99 flyttningsrelation", "fFlyttning", "kFRutid", "_SWEREF99"))
  
  # första recode-raden som anger var variabeln kommer ifrån och vilken tabell
  recode_vekt <- paste0('RECODE "', recode_kol, '" FROM "', databas_vekt[1], rutstorlek, databas_vekt[2], '" FACTTABLE "', databas_vekt[3], '"')
  
  # result-raderna, en rad för varje värde i recoden
  result_vekt <- map_chr(unique(gis_lager[[recode_kol]]), ~ paste0('RESULT "', .x, '"'))
  
  # här kopplas alla rutor till ett av värdena ovan
  mapcode_vekt <- map2_chr(gis_lager[[rutid_kol]], gis_lager[[recode_kol]], ~ 
                             paste0('MAP CODE "', lanskod, .x, '" VALUESET "', databas_vekt[4], rutstorlek, databas_vekt[5], '" TO "', .y, '"'))
  
  # vi sätter ihop alla delar och lägger till END RECODE på slutet
  recodefil_vekt <- c(header, recode_vekt, result_vekt, mapcode_vekt, 'END RECODE')
  
  # sen skriver vi filen med write.table
  write.table(as.data.frame(recodefil_vekt), paste0(outputmapp, outputfilnamn), quote = FALSE, col.names = FALSE, row.names = FALSE, fileEncoding = "utf-8")
  
  # har testat med writeLines() och write_csv, men ingen av dem fungerade. writeLines blev inte i utf-8 och då blev det problem med å, ä och ö i Supercross
  # på Mona. Med write_csv så fick man plötsligt dubletter bland rut-id, vilket inte finns i datasetet, mycket märkligt!
  #write_csv(as.data.frame(recodefil_vekt), paste0(outputmapp, outputfilnamn), escape = "none", quote = "none", col_names = FALSE)
  #writeLines(recodefil_vekt, paste0(outputmapp, outputfilnamn))
  
} # slut funktion


spatial_join_med_ovrkat <- function(gislager_grunddata, 
                                    gislager_omr,
                                    omrade_kol = "omrade",
                                    ovrig_varde = "Övriga områden") {
  # gör en spatial join
  retur_gis <- st_join(gislager_grunddata, gislager_omr)
  
  # koda de som inte är inom gislager_omr med värde från ovrig_varde
  # retur_gis <- retur_gis %>% 
  #   mutate({{omrade_kol}} := ifelse(is.na(!!sym(omrade_kol)), ovrig_varde, !!sym(omrade_kol)))
  # 
  retur_gis <- retur_gis %>% 
    mutate(!!sym(omrade_kol) := ifelse(is.na(!!sym(omrade_kol)), ovrig_varde, !!sym(omrade_kol)))
  
  return(retur_gis)
  
} # slut funktion


skapa_sf_fran_csv_eller_excel_supercross <- function(fil_med_sokvag,               # fil som ska bearbetas, dvs. ett uttag från Supercross 
                                                     rutid_kol = NA,               # finns en funktion för att hitta rutid-kolumnen men man kan skicka med den här
                                                     rutstorlek = NA,              # om man vill ange själv, annars kontrolleras för det automatiskt.
                                                     vald_crs = 3006) {
  
  # funktion för att skapa en gpkg-fil från ett uttag ur supercross där rutid är en kolumn
  
  rut_df <- import(fil_med_sokvag)
  
  # kolla om bara en kolumn heter något med rut, i så fall är det rutid-kolumnen
  if (is.na(rutid_kol[1]) & sum(str_detect(tolower(names(rut_df)), "rut")) == 1) rutid_kol <- names(rut_df)[str_which(tolower(names(rut_df)), "rut")]
  
  # om det inte bara är en kolumn som heter något med "rut", välj den första kolumnen i rut_df som rutid-kolumn
  if (is.na(rutid_kol[1])) rutid_kol <- names(rut_df)[1]
  
  # hitta rutstorlek genom att kontrollera tecken 11 i rutid-kolumnen. Om det är km-rutor är det bara "0" där, om det är 500-metersrutor är det bara "0" eller "5" där, och 
  # om det är 100 metersrutor kan alla siffror finnas där, bland annat "1", "2", "3", "4" som inte kan finnas i övriga rutor. Kan bli fel om man har väldigt få rutor men
  # sannolikheten att det ska hända är extremt liten, men då finns möjligheten att skicka med ett värde för rutstorlek
  if (is.na(rutstorlek)) {
    test_vekt <- rut_df[[rutid_kol]] %>% str_sub(11,11)
    rutstorlek <- case_when(all(test_vekt == "0") ~ 1000,
                            all(test_vekt %in% c("0", "5")) ~ 500,
                            any(test_vekt %in% c("1", "2", "3", "4")) ~ 100)
  }
  
  # skapa x- och y-koordinater från rutid-kolumnen, de läggs i mitten av rutan istället för nedre vänstra hörnet som utgör rutid:t 
  rut_df <- rut_df %>% 
    rename(rutid = !!rutid_kol) %>% 
    mutate(x_koord = paste0(as.character(as.numeric(substr(rutid, 1,6))+(rutstorlek/2))),
           y_koord = paste0(as.character(as.numeric(substr(rutid, 7,13))+(rutstorlek/2))),
           rutid = rutid %>% as.character())
  
  rut_gis <- st_as_sf(rut_df, coords = c("x_koord", "y_koord"), crs = 3006)
  
  rut_gis_cell <- st_buffer(rut_gis,(rutstorlek/2), endCapStyle = "SQUARE")
  
  return(rut_gis_cell)
  
} # slut funktion


# ============================== postgres-funktioner (för att hantera databaser) ============================================


uppkoppling_adm <- function(
    databas = "geodata",
    db_host = "WFALMITVS526.ltdalarna.se",
    db_port = 5432,
    db_options = "-c search_path=public",
    db_user = NA,
    db_password = NA
    ) {
  
  uppkoppling_db(service_name = "databas_adm", 
                 db_name = databas,
                 db_host = db_host,
                 db_port = db_port,
                 db_options = db_options)
}

uppkoppling_db <- function(
    
  # 0. Funktion för att koppla upp mot databasen. Kan användas med defaultvärden enligt nedan eller egna parametrar.
  # Används av andra funktioner som default om inget eget objekt med databasuppkoppling har skickats till dessa funktioner
  # OBS! Ändra default för db_name till "geodata" sen
  
  db_name = "geodata",                  
  service_name = NA,                                 # "rd_geodata"
  db_host = "WFALMITVS526.ltdalarna.se",
  db_port = 5432,
  db_options = "-c search_path=public",
  db_user = NA,
  db_password = NA
) {
  
  # om inte service_name har ett värde så är default-värdet "geodata_las"
  if (!is.na(service_name)) {
    if (is.na(db_user)) db_user <- key_list(service = service_name)$username
    if (is.na(db_password)) db_password <- key_get(service_name, key_list(service = service_name)$username)
  } else {
    if (is.na(db_user)) db_user <- "geodata_las"
    if (is.na(db_password)) db_password <- "geodata_las"
  }
  
  current_hostname <- Sys.info()[["nodename"]] 
  
  if (str_detect(toupper(db_host), toupper(current_hostname))) {
    db_host <- "localhost"
  } else {
    db_host <- if(is.na(db_host)) "WFALMITVS526.ltdalarna.se" else db_host
  }
  
  tryCatch({
    # Etablera anslutningen
    
    con <- suppress_specific_warning(
      dbConnect(          
        RPostgres::Postgres(),
        bigint = "integer",  
        user = db_user,
        password = db_password,
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

oppnadata_hamta <- function(
    schema = NA,
    tabell = NA,
    query = NA
) {
  
  retur_df <- postgres_hamta_oppnadata(
    schema = schema,
    tabell = tabell,
    query = query
  )

  return(retur_df)  
}


postgres_hamta_oppnadata <- function(
    schema = NA,
    tabell = NA,
    query = NA,
    meddelande_info = FALSE,
    meddelande_tid = FALSE
    ) {
  # om inte både schema och tabell skickas med så listas scheman och tabeller för databasen öppna data
  if (is.na(schema) & is.na(tabell)){
    postgres_lista_scheman_tabeller(con = uppkoppling_db(db_name = "oppna_data"))
  } else {
    
    if (str_detect(schema, "\\.")) {
      if (str_count(schema, "\\.") > 1) stop("Det får max vara en punkt, som skiljer schema från tabell.")
      tabell <- str_split(schema, "\\.")[[1]][2]
      schema <- str_split(schema, "\\.")[[1]][1]
    }
    
    scheman_tabeller <- postgres_lista_scheman_tabeller(uppkoppling_db(db_name = "oppna_data"))
    if (!schema %in% names(scheman_tabeller)) stop("Schemat '", schema, "' finns inte i oppna_data-databasen. Kontrollera schemats namn och prova igen.")
    if (!tabell %in% scheman_tabeller[[schema]]) stop("Tabellen '", tabell, "' finns inte i schemat '", schema, "' i oppna_data-databasen. Kontrollera tabellens namn och prova igen.")
    
    retur_df <- postgres_tabell_till_df(
      con = uppkoppling_db(db_name = "oppna_data"),
      schema = schema,
      tabell = tabell,
      query = query,
      meddelande_info = meddelande_info,
      meddelande_tid = meddelande_tid
    )
    return(retur_df)
  }
}

# funktion för att skapa en ny databas på servern som man arbetar i
postgres_databas_skapa <- function(con, databasnamn) {
  
  sql_create <- glue::glue("CREATE DATABASE {DBI::dbQuoteIdentifier(con, databasnamn)};")
  
  tryCatch({
    DBI::dbExecute(con, sql_create)
    message(glue::glue("✅ Databasen '{databasnamn}' har skapats."))
  }, error = function(e) {
    message("❌ Kunde inte skapa databasen: ", e$message)
  })
}

# funktion för att ta bort en hel databas i postgres - använd med största försiktighet!
postgres_databas_ta_bort <- function(con, databasnamn) {
  svar <- readline(prompt = glue::glue("⚠️  Är du säker på att du vill ta bort databasen '{databasnamn}'? Skriv 'ja' för att bekräfta: "))
  
  if (tolower(svar) != "ja") {
    message("❌ Åtgärden avbröts – databasen togs inte bort.")
    return(invisible(FALSE))
  }
  
  sql_drop <- glue::glue("DROP DATABASE IF EXISTS {DBI::dbQuoteIdentifier(con, databasnamn)};")
  
  tryCatch({
    DBI::dbExecute(con, sql_drop)
    message(glue::glue("🗑️ Databasen '{databasnamn}' har tagits bort."))
  }, error = function(e) {
    message("❌ Kunde inte ta bort databasen: ", e$message)
  })
}


logga_event <- function(meddelande, 
                        log_file                # kan tex vara log_file <- "C:/auto_scheduler/loggfiler/logg.txt"
) {
  # funktion som loggar händelser till en loggfil
  
  # Få den aktuella tidstämpeln
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Skapa loggmeddelandet
  log_message <- paste(timestamp, meddelande, sep=" - ")
  
  # Skriv meddelandet till loggfilen
  cat(log_message, "\n", file = log_file, append = TRUE)
}

postgres_lista_databaser <- function(con = "default", 
                                     meddelande_tid = FALSE) {
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  databaser <- dbGetQuery(con, "SELECT datname FROM pg_database WHERE datistemplate = false;") %>% 
    rename(databas = datname)
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(databaser)
}

postgres_lista_scheman_tabeller <- function(con = "default", 
                                            visa_system_tabeller = FALSE,
                                            meddelande_tid = FALSE
) {
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  # Hämta alla scheman i databasen
  scheman <- dbGetQuery(con, "SELECT schema_name FROM information_schema.schemata")
  
  
  if (!visa_system_tabeller) scheman <- scheman %>%
    filter(!str_detect(schema_name, "pg_"),
           !schema_name %in% c("public", "information_schema"))
  
  # Initiera en lista för att spara tabellerna för varje schema
  scheman_tabeller <- list()
  
  # Loopa igenom varje schema och hämta tabeller
  for (schema in scheman$schema_name) {
    tabeller <- dbGetQuery(con, sprintf(
      "SELECT table_name AS tabell_namn FROM information_schema.tables WHERE table_schema = '%s' AND (table_type IN ('BASE TABLE', 'VIEW', 'MATERIALIZED VIEW'))",
      #"SELECT table_name AS tabell_namn FROM information_schema.tables WHERE table_schema = '%s'",
      schema
    ))
    
    # Lägg till tabellerna i listan för respektive schema
    scheman_tabeller[[schema]] <- tabeller$tabell_namn
    
    # Hämta materialiserade vyer för respektive schema
    matviews <- dbGetQuery(con, sprintf(
      "SELECT matviewname AS tabell_namn FROM pg_matviews WHERE schemaname = '%s'",
      schema
    ))
    
    # Lägg till materialiserade vyer till listan för respektive schema
    scheman_tabeller[[schema]] <- c(scheman_tabeller[[schema]], matviews$tabell_namn)
    
  }
  
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(scheman_tabeller)
}


postgres_lista_roller_anvandare <- function(con = "default",
                                            meddelande_tid = FALSE
) {
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  # Lista alla roller och användare
  query <- "
    SELECT rolname, rolsuper, rolinherit, rolcreaterole, rolcreatedb, rolcanlogin
    FROM pg_roles;
  "
  
  roles_and_users <- dbGetQuery(con, query)                      # Exekvera SQL-frågan och spara resultatet
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(roles_and_users)
  
} # slut funktion


postgres_lista_behorighet_till_scheman <- function(con = "default",
                                                   meddelande_tid = FALSE
) {
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  query <- "
  WITH privilege_summary AS (
    SELECT
      grantee AS role_or_user,
      table_schema,
      CASE
        WHEN STRING_AGG(privilege_type, ',') LIKE '%INSERT%' OR
             STRING_AGG(privilege_type, ',') LIKE '%UPDATE%' OR
             STRING_AGG(privilege_type, ',') LIKE '%CREATE%' OR
             STRING_AGG(privilege_type, ',') LIKE '%DELETE%' THEN 'write'
        WHEN STRING_AGG(privilege_type, ',') LIKE '%SELECT%'THEN 'read'
        ELSE 'no access'
      END AS access_type
    FROM
      information_schema.role_table_grants
    GROUP BY
      grantee, table_schema
  )
  SELECT
    role_or_user,
    table_schema,
    MAX(access_type) AS access_level
  FROM
    privilege_summary
  GROUP BY
    role_or_user, table_schema
  ORDER BY
    role_or_user, table_schema;
"
  
  
  
  
  
  
  
  # Exekvera SQL-frågan och spara resultatet
  permissions_per_schema <- dbGetQuery(con, query)
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(permissions_per_schema)
  
}

postgres_lista_kolumnnamn_i_schema <- function(con = "default",
                                               schema = NA,
                                               meddelande_tid = FALSE
) {
  
  if (is.na(schema)) stop("Schema måste anges")
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  kol_namn <- dbGetQuery(con, glue("
  SELECT 
    table_name,
    column_name,
    data_type
  FROM information_schema.columns
  WHERE table_schema = '{schema}'
  ORDER BY table_name, ordinal_position;
"))
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(kol_namn)
  
}

postgres_test <- function(con = "default", 
                          meddelande_tid = FALSE) {
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE  
  
  query <- "
  SELECT 
    member.rolname AS user_or_role,
    role.rolname AS inherited_role
  FROM 
    pg_auth_members m
  JOIN 
    pg_roles member ON m.member = member.oid
  JOIN 
    pg_roles role ON m.roleid = role.oid
  ORDER BY 
    member.rolname, role.rolname;
"
  # Exekvera SQL-frågan och spara resultatet
  test <- dbGetQuery(con, query)
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(test)
}

# postgres_alla_rattigheter <- function(con = "default", 
#                                       meddelande_tid = FALSE
# ) {
#   
#   starttid <- Sys.time()                                        # Starta tidstagning
#   
#   # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
#   if(is.character(con) && con == "default") {
#     con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
#     default_flagga = TRUE
#   } else  default_flagga = FALSE  
#   
#   
#   query <- "
#   WITH recursive role_inheritance AS (
#     -- Start med att samla alla användare och roller de är medlemmar i
#     SELECT 
#       member.oid AS user_oid,
#       member.rolname AS user_or_role,
#       role.oid AS inherited_role_oid,
#       role.rolname AS inherited_role
#     FROM 
#       pg_auth_members m
#     JOIN 
#       pg_roles member ON m.member = member.oid
#     JOIN 
#       pg_roles role ON m.roleid = role.oid
#     
#     UNION ALL
#     
#     -- Rekursivt hämta ärvda roller längre upp i hierarkin
#     SELECT 
#       ri.user_oid,
#       ri.user_or_role,
#       role.oid AS inherited_role_oid,
#       role.rolname AS inherited_role
#     FROM 
#       role_inheritance ri
#     JOIN 
#       pg_auth_members m ON ri.inherited_role_oid = m.member
#     JOIN 
#       pg_roles role ON m.roleid = role.oid
#   ),
#   all_users AS (
#     SELECT oid AS user_oid, rolname AS role_or_user, rolsuper FROM pg_roles WHERE rolcanlogin = TRUE
#   ),
#   all_schemas AS (
#     SELECT schema_name 
#     FROM information_schema.schemata
#     WHERE schema_name NOT LIKE 'pg_%' AND schema_name != 'information_schema'
#   ),
#   privileges AS (
#     SELECT 
#       grantee AS role_or_user,
#       table_schema,
#       CASE
#         WHEN STRING_AGG(privilege_type, ',') LIKE '%INSERT%' OR
#              STRING_AGG(privilege_type, ',') LIKE '%UPDATE%' OR
#              STRING_AGG(privilege_type, ',') LIKE '%DELETE%' THEN 'write'
#         WHEN STRING_AGG(privilege_type, ',') LIKE '%SELECT%' THEN 'read'
#         ELSE 'no access'
#       END AS access_type
#     FROM 
#       information_schema.role_table_grants
#     GROUP BY 
#       grantee, table_schema
#   ),
#   combined_access AS (
#     SELECT 
#       u.role_or_user,
#       s.schema_name,
#       CASE
#         -- Om användaren är en superanvändare, ge dem skrivbehörigheter till alla scheman
#         WHEN u.rolsuper THEN 'write'
#         -- Annars, hämta de faktiska behörigheterna
#         ELSE COALESCE(p.access_type, 'no access')
#       END AS access_type
#     FROM 
#       (SELECT role_or_user, rolsuper FROM all_users UNION SELECT inherited_role AS role_or_user, FALSE AS rolsuper FROM role_inheritance) u
#     CROSS JOIN 
#       all_schemas s
#     LEFT JOIN 
#       privileges p ON u.role_or_user = p.role_or_user AND s.schema_name = p.table_schema
#   )
#   -- Eliminera dubbletter och prioritera 'write' över 'read' och 'no access'
#   SELECT role_or_user, schema_name, 
#          MAX(CASE 
#                WHEN access_type = 'write' THEN 'write'
#                WHEN access_type = 'read' THEN 'read'
#                ELSE 'no access'
#              END) AS access_level
#   FROM combined_access
#   GROUP BY role_or_user, schema_name
#   ORDER BY role_or_user, schema_name;
# "
#   
#   # Exekvera SQL-frågan och spara resultatet
#   user_schema_permissions <- dbGetQuery(con, query)
#   
#   if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
#   berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
#   if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
#   
#   return(user_schema_permissions)
#   
# }


postgres_alla_rattigheter <- function(con = "default", 
                                      anvandarnamn = NULL,
                                      meddelande_tid = FALSE) {
  starttid <- Sys.time()
  
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  filter_anvandare <- if (!is.null(anvandarnamn)) glue::glue("WHERE u.role_or_user = '{anvandarnamn}'") else ""
  
  query <- glue::glue("
    WITH recursive role_inheritance AS (
      SELECT 
        member.oid AS user_oid,
        member.rolname AS user_or_role,
        role.oid AS inherited_role_oid,
        role.rolname AS inherited_role
      FROM pg_auth_members m
      JOIN pg_roles member ON m.member = member.oid
      JOIN pg_roles role ON m.roleid = role.oid
      UNION ALL
      SELECT 
        ri.user_oid,
        ri.user_or_role,
        role.oid AS inherited_role_oid,
        role.rolname AS inherited_role
      FROM role_inheritance ri
      JOIN pg_auth_members m ON ri.inherited_role_oid = m.member
      JOIN pg_roles role ON m.roleid = role.oid
    ),
    all_users AS (
      SELECT oid AS user_oid, rolname AS role_or_user, rolsuper FROM pg_roles WHERE rolcanlogin = TRUE
    ),
    all_schemas AS (
      SELECT schema_name 
      FROM information_schema.schemata
      WHERE schema_name NOT LIKE 'pg_%' AND schema_name != 'information_schema'
    ),
    privileges AS (
      SELECT 
        grantee AS role_or_user,
        table_schema,
        STRING_AGG(privilege_type, ',') AS privileges
      FROM information_schema.role_table_grants
      GROUP BY grantee, table_schema
    ),
    schema_create_privileges AS (
      SELECT 
        r.rolname AS role_or_user,
        n.nspname AS table_schema,
        'CREATE' AS privilege_type
      FROM pg_roles r
      CROSS JOIN pg_namespace n
      WHERE has_schema_privilege(r.rolname, n.nspname, 'CREATE')
    ),
    merged_privileges AS (
      SELECT * FROM privileges
      UNION ALL
      SELECT 
        role_or_user,
        table_schema,
        privilege_type AS privileges
      FROM schema_create_privileges
    ),
    access_levels AS (
      SELECT 
        role_or_user,
        table_schema,
        CASE
          WHEN STRING_AGG(privileges, ',') LIKE '%INSERT%' OR
               STRING_AGG(privileges, ',') LIKE '%UPDATE%' OR
               STRING_AGG(privileges, ',') LIKE '%DELETE%' OR
               STRING_AGG(privileges, ',') LIKE '%CREATE%' THEN 'write'
          WHEN STRING_AGG(privileges, ',') LIKE '%SELECT%' THEN 'read'
          ELSE 'no access'
        END AS access_type
      FROM merged_privileges
      GROUP BY role_or_user, table_schema
    ),
    combined_access AS (
      SELECT 
        u.role_or_user,
        s.schema_name,
        CASE
          WHEN u.rolsuper THEN 'write'
          ELSE COALESCE(p.access_type, 'no access')
        END AS access_type
      FROM 
        (SELECT role_or_user, rolsuper FROM all_users 
         UNION 
         SELECT inherited_role AS role_or_user, FALSE AS rolsuper FROM role_inheritance) u
      CROSS JOIN all_schemas s
      LEFT JOIN access_levels p ON u.role_or_user = p.role_or_user AND s.schema_name = p.table_schema
    )
    SELECT role_or_user, schema_name, 
           MAX(CASE 
                 WHEN access_type = 'write' THEN 'write'
                 WHEN access_type = 'read' THEN 'read'
                 ELSE 'no access'
               END) AS access_level
    FROM combined_access
    {filter_anvandare}
    GROUP BY role_or_user, schema_name
    ORDER BY role_or_user, schema_name;
  ")
  
  resultat <- DBI::dbGetQuery(con, query)
  
  if (default_flagga) DBI::dbDisconnect(con)
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)
  if (meddelande_tid) cat(glue::glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(resultat)
}




postgres_alla_rattigheter <- function(con = "default", 
                                      anvandarnamn = NULL,
                                      meddelande_tid = FALSE) {
  starttid <- Sys.time()
  
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  filter_anvandare <- if (!is.null(anvandarnamn)) glue::glue("WHERE u.role_or_user = '{anvandarnamn}'") else ""
  
  query <- glue::glue("
    WITH recursive role_inheritance AS (
      SELECT 
        member.oid AS user_oid,
        member.rolname AS user_or_role,
        role.oid AS inherited_role_oid,
        role.rolname AS inherited_role
      FROM pg_auth_members m
      JOIN pg_roles member ON m.member = member.oid
      JOIN pg_roles role ON m.roleid = role.oid
      UNION ALL
      SELECT 
        ri.user_oid,
        ri.user_or_role,
        role.oid AS inherited_role_oid,
        role.rolname AS inherited_role
      FROM role_inheritance ri
      JOIN pg_auth_members m ON ri.inherited_role_oid = m.member
      JOIN pg_roles role ON m.roleid = role.oid
    ),
    all_users AS (
      SELECT oid AS user_oid, rolname AS role_or_user, rolsuper FROM pg_roles WHERE rolcanlogin = TRUE
    ),
    all_schemas AS (
      SELECT schema_name 
      FROM information_schema.schemata
      WHERE schema_name NOT LIKE 'pg_%' AND schema_name != 'information_schema'
    ),
    privileges AS (
      SELECT 
        grantee AS role_or_user,
        table_schema,
        STRING_AGG(privilege_type, ',') AS privileges
      FROM information_schema.role_table_grants
      GROUP BY grantee, table_schema
    ),
    schema_create_privileges AS (
      SELECT 
        r.rolname AS role_or_user,
        n.nspname AS table_schema,
        'CREATE' AS privilege_type
      FROM pg_roles r
      CROSS JOIN pg_namespace n
      WHERE has_schema_privilege(r.rolname, n.nspname, 'CREATE')
    ),
    merged_privileges AS (
      SELECT * FROM privileges
      UNION ALL
      SELECT 
        role_or_user,
        table_schema,
        privilege_type AS privileges
      FROM schema_create_privileges
    ),
    access_levels AS (
      SELECT 
        role_or_user,
        table_schema,
        CASE
          WHEN STRING_AGG(privileges, ',') LIKE '%INSERT%' OR
               STRING_AGG(privileges, ',') LIKE '%UPDATE%' OR
               STRING_AGG(privileges, ',') LIKE '%DELETE%' OR
               STRING_AGG(privileges, ',') LIKE '%CREATE%' THEN 'write'
          WHEN STRING_AGG(privileges, ',') LIKE '%SELECT%' THEN 'read'
          ELSE 'no access'
        END AS access_type
      FROM merged_privileges
      GROUP BY role_or_user, table_schema
    ),
    combined_access AS (
      SELECT 
        u.role_or_user,
        s.schema_name,
        CASE
          WHEN u.rolsuper THEN 'write'
          ELSE COALESCE(p.access_type, 'no access')
        END AS access_type
      FROM 
        (SELECT role_or_user, rolsuper FROM all_users 
         UNION 
         SELECT inherited_role AS role_or_user, FALSE AS rolsuper FROM role_inheritance) u
      CROSS JOIN all_schemas s
      LEFT JOIN access_levels p ON u.role_or_user = p.role_or_user AND s.schema_name = p.table_schema
    )
    SELECT role_or_user, schema_name, 
           MAX(CASE 
                 WHEN access_type = 'write' THEN 'write'
                 WHEN access_type = 'read' THEN 'read'
                 ELSE 'no access'
               END) AS access_level
    FROM combined_access
    {filter_anvandare}
    GROUP BY role_or_user, schema_name
    ORDER BY role_or_user, schema_name;
  ")
  
  resultat <- DBI::dbGetQuery(con, query)
  
  if (default_flagga) DBI::dbDisconnect(con)
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)
  if (meddelande_tid) cat(glue::glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(resultat)
}






# Funktion för att lägga till en användare och ge rättigheter till flera databaser
postgres_anvandare_lagg_till <- function(con = "default", 
                                         anvandarnamn, 
                                         losenord,
                                         meddelande_tid = FALSE
) {
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  starttid <- Sys.time()                                        # Starta tidstagning
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  # Skapa användaren med specifikt lösenord (om användaren inte redan finns)
  skapa_anvandare_query <- paste0("CREATE USER ", anvandarnamn, " WITH PASSWORD '", losenord, "';")
  tryCatch({
    dbExecute(con, skapa_anvandare_query)
  }, error = function(e) {
    message("Användaren finns redan eller något annat fel uppstod: ", e$message)
  })
  
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}

# Funktion för att ta bort en användare helt från PostgreSQL-servern
postgres_anvandare_ta_bort <- function(con = "default", 
                                       anvandarnamn,
                                       meddelande_tid = FALSE
) {
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  starttid <- Sys.time()                                        # Starta tidstagning
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  # Kontrollera först om användaren har några beroenden (ägare till objekt etc.)
  kontroll_query <- paste0("SELECT COUNT(*) AS antal FROM pg_authid WHERE rolname = '", anvandarnamn, "';")
  antal <- dbGetQuery(con, kontroll_query)$antal
  
  if (antal == 0) {
    message(paste("Anvandaren", anvandarnamn, "finns inte på servern."))
    return()
  }
  
  # Om användaren finns, försök ta bort den
  tryCatch({
    drop_user_query <- paste0("DROP ROLE ", anvandarnamn, ";")
    dbExecute(con, drop_user_query)
    message(paste("Anvandaren", anvandarnamn, "har tagits bort från servern."))
  }, error = function(e) {
    message(paste("Ett fel uppstod vid försök att ta bort anvandaren:", anvandarnamn))
    message("Felmeddelande:", e$message)
  })
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}


postgres_rattigheter_anvandare_lagg_till <- function(con = "default", 
                                                     anvandarnamn,
                                                     schema = "alla", 
                                                     rattigheter = c("CONNECT", "SELECT", "USAGE"),
                                                     meddelande_rattigheter = TRUE,
                                                     meddelande_tid = FALSE
){
  starttid <- Sys.time()  # Starta tidtagning
  
  # för läsrättigheter så kan c("CONNECT", "SELECT", "USAGE") användas
  # för skrivrättigheter så kan "alla" användas
  # är typ c("CONNECT", "SELECT", "USAGE", "INSERT", "UPDATE", "DELETE", "CREATE") användas
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # lägg databasnamnet i variabeln db som används nedan
  db <- dbGetInfo(con)$dbname
  
  if (all(rattigheter == "alla")) rattigheter <- postgres_lista_giltiga_rattigheter()$Rattighet
  
  # Lista över system-scheman som ska undantas
  system_scheman <- c("pg_catalog", "information_schema", "pg_toast")
  
  
  # Steg 1: Tilldela anslutningsrättigheter till den specifika databasen (CONNECT på databasnivå)
  if ("CONNECT" %in% rattigheter ) {
    tilldela_atkomst_query <- paste0("GRANT CONNECT ON DATABASE ", db, " TO ", anvandarnamn, ";")
    tryCatch({
      dbExecute(con, tilldela_atkomst_query)
      if (meddelande_rattigheter) message(paste("CONNECT-rättighet har lagts till för användaren", anvandarnamn, "till databasen", db))
    }, error = function(e) {
      message(paste("Kunde inte lägga till CONNECT-rättighet för användaren", anvandarnamn, "i databasen", db, ":", e$message))
    })
  }
  
  scheman_att_bearbeta <- postgres_lista_scheman_tabeller(con = con) %>% names()
  if (!all(schema == "alla")) scheman_att_bearbeta <- scheman_att_bearbeta[scheman_att_bearbeta %in% schema] 
  if (length(scheman_att_bearbeta) < 1) stop(glue("Scheman {schema %>% list_komma_och()} finns inte i databasen. Kontrollera uppgifterna och försök igen."))
  
  # Steg 2: Tilldela rättigheter på schemanivå
  for (schema_namn in scheman_att_bearbeta) {
    
    # Tilldela USAGE rättighet till schemat
    if ("USAGE" %in% rattigheter) {
      tilldela_usage_query <- paste0("GRANT USAGE ON SCHEMA ", schema_namn, " TO ", anvandarnamn, ";")
      tryCatch({
        dbExecute(con, tilldela_usage_query)
        if (meddelande_rattigheter) message(paste("USAGE-rättighet har lagts till för schemat", schema_namn, "för användaren", anvandarnamn))
      }, error = function(e) {
        message(paste("Kunde inte lägga till USAGE-rättighet för schemat", schema_namn, "för användaren", anvandarnamn, ":", e$message))
      })
    }
    # Tilldela CREATE rättighet till schemat
    if ("CREATE" %in% rattigheter) {
      tilldela_create_query <- paste0("GRANT CREATE ON SCHEMA ", schema_namn, " TO ", anvandarnamn, ";")
      tryCatch({
        dbExecute(con, tilldela_create_query)
        if (meddelande_rattigheter) message(paste("CREATE-rättighet har lagts till för schemat", schema_namn, "för användaren", anvandarnamn))
      }, error = function(e) {
        message(paste("Kunde inte lägga till CREATE-rättighet för schemat", schema_namn, "för användaren", anvandarnamn, ":", e$message))
      })
    }
    
    # hämta alla materialiserade vyer i det schema som hanteras
    matviews <- dbGetQuery(con, glue::glue(
      "SELECT quote_ident(nspname) || '.' || quote_ident(relname) AS full_name
   FROM pg_class
   JOIN pg_namespace ON pg_namespace.oid = pg_class.relnamespace
   WHERE relkind = 'm' AND nspname = '{schema_namn}'"
    ))
    
    # Tilldela rättigheter på tabellnivå
    for (rattighet in setdiff(rattigheter, c("CONNECT", "USAGE", "CREATE"))) {
      if (rattighet %in% postgres_lista_giltiga_rattigheter()$Rattighet) {
        tilldela_rattigheter_query <- paste0("GRANT ", rattighet, " ON ALL TABLES IN SCHEMA ", schema_namn, " TO ", anvandarnamn, ";")
        # Rättigheter för tabeller
        tryCatch({
          dbExecute(con, tilldela_rattigheter_query)
          if (meddelande_rattigheter) message(paste("Rättigheten", rattighet, "har lagts till för användaren", anvandarnamn, "i schemat", schema_namn))
          
          # om det finns materialiserade vyer i schemat, lägg till rättigheten på dem också
          if (nrow(matviews) > 0) {
            for (mv in matviews$full_name) {
              query_mv <- glue::glue("GRANT {rattighet} ON {mv} TO {anvandarnamn};")
              tryCatch({
                dbExecute(con, query_mv)
                if (meddelande_rattigheter) message(glue::glue("Rättigheten {rattighet} har lagts till för användaren {anvandarnamn} på materialiserad vy {mv}"))
              }, error = function(e) {
                message(glue::glue("Kunde inte lägga till rättigheten {rattighet} på materialiserad vy {mv}: {e$message}"))
              })
            }
          }
          
          
        }, error = function(e) {
          message(paste("Kunde inte lägga till rättigheten", rattighet, "för användaren", anvandarnamn, "i schemat", schema_namn, ":", e$message))
        })
        
      } else {
        message(paste("Ogiltig rättighet:", rattighet, "- denna rättighet har inte lagts till."))
      }
    }
  }
  
  if (default_flagga) dbDisconnect(con)  # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)  # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
} # slut funktion


# Funktion för att ta bort rättigheter från användare
postgres_rattigheter_anvandare_ta_bort <- function(con = "default", 
                                                   anvandarnamn,
                                                   schema = "alla",
                                                   rattigheter = "alla",
                                                   meddelande_rattigheter = TRUE,
                                                   meddelande_tid = FALSE
) {
  
  # för skrivrättigheter så kan c("INSERT", "UPDATE", "DELETE", "CREATE") användas
  # för läsrättigheter så räcker det att ta bort rättigheter för "CONNECT"
  starttid <- Sys.time()  # Starta tidtagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # hämta giltiga rättigheter och fördela per kategori
  giltiga_rattigheter <- postgres_lista_giltiga_rattigheter()$Rattighet
  rattigheter_databas <- c("CONNECT", "TEMP")
  rattigheter_tabell <- c("SELECT", "INSERT", "UPDATE", "DELETE", "TRUNCATE", "REFERENCES")     # "TRIGGER"
  rattigheter_schema <- c("USAGE", "CREATE")
  
  if (all(rattigheter == "alla")) rattigheter <- postgres_lista_giltiga_rattigheter()$Rattighet
  db <- dbGetInfo(con)$dbname
  
  
  # hämta scheman för vald databas (som styrs med con)
  scheman_att_bearbeta <- postgres_lista_scheman_tabeller(con = con) %>% names()
  if (!all(schema == "alla")) scheman_att_bearbeta <- scheman_att_bearbeta[scheman_att_bearbeta %in% schema] 
  if (length(scheman_att_bearbeta) < 1) stop(glue("Scheman {schema %>% list_komma_och()} finns inte i databasen. Kontrollera uppgifterna och försök igen."))
  
  # Ta bort rättigheter för databasen
  databas_rattigheter <- intersect(rattigheter, rattigheter_databas)
  if (length(databas_rattigheter) > 0) {
    for (rattighet in databas_rattigheter) {
      ta_bort_atkomst_query <- glue("REVOKE {rattighet} ON DATABASE {db} FROM {anvandarnamn};")
      dbExecute(con, ta_bort_atkomst_query)
      if (meddelande_rattigheter) message(glue("Rättigheten {rattighet} har tagits bort från användaren {anvandarnamn} i databasen {dbGetInfo(con)$dbname}."))
    }
  }
  
  # Ta bort andra rättigheter för scheman
  schema_rattigheter <- intersect(rattigheter, rattigheter_schema)
  if (length(schema_rattigheter) > 0) {
    for (schema_loop in scheman_att_bearbeta) {
      for (rattighet in schema_rattigheter) {
        ta_bort_schema_query <- glue("REVOKE {rattighet} ON SCHEMA {schema_loop} FROM {anvandarnamn};")
        dbExecute(con, ta_bort_schema_query)
        if (meddelande_rattigheter) message(glue("Rättigheten {rattighet} har tagits bort från användaren {anvandarnamn} på schemat {schema_loop}."))
      }
    }
  }
  
  # Ta bort rättigheter för tabeller
  tabell_rattigheter <- intersect(rattigheter, rattigheter_tabell)
  if (length(tabell_rattigheter) > 0) {
    for (schema_loop in scheman_att_bearbeta) {
      for (rattighet in tabell_rattigheter) {
        ta_bort_rattigheter_query <- glue("REVOKE {rattighet} ON ALL TABLES IN SCHEMA {schema_loop} FROM {anvandarnamn};")
        dbExecute(con, ta_bort_rattigheter_query)
        if (meddelande_rattigheter) message(glue("Rättigheten {rattighet} har tagits bort från användaren {anvandarnamn} i schemat {schema_loop}."))
      }
    }
  }

  if (default_flagga) dbDisconnect(con)  # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)  # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
}

# Funktion som listar rättigheterna i PostgreSQL
postgres_lista_giltiga_rattigheter <- function() {
  
  postgres_giltiga_rattigheter <- c(
    "CONNECT", "SELECT", "INSERT", "UPDATE", "DELETE",
    "TRUNCATE", "REFERENCES", "USAGE", "EXECUTE",
    "CREATE", "TEMP"
  )
  
  # Skapa en dataframe med rättighetsnamn och deras beskrivningar
  rattigheter_df <- data.frame(
    Rattighet = postgres_giltiga_rattigheter,
    Beskrivning = c(
      "Ger användaren rättigheten att ansluta till en specifik databas.",
      "Ger användaren rättigheten att använda SELECT-förfrågningar för att läsa från tabeller och vyer.",
      "Ger användaren rättigheten att lägga till nya rader i en specifik tabell.",
      "Ger användaren rättigheten att uppdatera befintliga data i en tabell eller specifika kolumner.",
      "Ger användaren rättigheten att ta bort rader från en specifik tabell.",
      "Ger användaren rättigheten att tömma en tabell helt utan att utlösa triggers.",
      "Ger användaren rättigheten att skapa foreign keys som refererar till kolumner i en annan tabell.",
      "Ger användaren rättigheten att använda objekt som sekvenser, scheman eller typer.",
      "Ger användaren rättigheten att köra lagrade procedurer eller funktioner i databasen.",
      "Ger användaren rättigheten att skapa nya objekt, såsom tabeller, i ett schema.",
      "Ger användaren rättigheten att skapa temporära tabeller i databasen."
    ),
    stringsAsFactors = FALSE
  )
  
  return(rattigheter_df)
}

postgres_losenord_byt_for_anvandare <- function(con = "default", 
                                                anvandarnamn, 
                                                nytt_losenord) {
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # SQL-fråga för att ändra lösenordet
  satt_losenord_query <- paste0("ALTER ROLE ", anvandarnamn, " WITH PASSWORD '", nytt_losenord, "';")
  
  # Kör SQL-frågan och hantera fel
  tryCatch({
    dbExecute(con, satt_losenord_query)
    message(paste("Lösenordet har ändrats för användaren", anvandarnamn))
  }, error = function(e) {
    message(paste("Kunde inte ändra lösenordet för användaren", anvandarnamn, ":", e$message))
  })
  
  # Koppla ner anslutningen om den skapades som default
  if (default_flagga) dbDisconnect(con)
}

postgres_tabell_till_df <- function(con = "default", 
                                    schema, 
                                    tabell,
                                    query = NA,
                                    meddelande_info = FALSE,
                                    meddelande_tid = FALSE
) {
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # SQL-fråga för att läsa in tabellen från ett specifikt schema
  if (is.na(query)) {
    sql_query <- paste0("SELECT * FROM ", schema, ".", tabell)
  } else {
    sql_query <- paste0("SELECT * FROM ", schema, ".", tabell, " ", query)
  }
  
  # Kör SQL-frågan och hämta tabellen som en dataframe
  tryCatch({
    retur_df <- dbGetQuery(con, sql_query)
    if (meddelande_info) message(paste("Tabellen", tabell, "från schemat", schema, "har lästs in."))
  }, error = function(e) {
    message(paste("Kunde inte läsa tabellen", tabell, "från schemat", schema, ":", e$message))
    retur_df <- NULL
  })
  
  # Koppla ner anslutningen om den skapades som default
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(retur_df)
}

postgres_df_till_postgrestabell <- function(con = "default",
                                            inlas_df,
                                            schema,                            # det schema i postgis-databasen som målpunktstabellen ska ligga under
                                            tabell,                                      # det tabellnamn den nya filen ska få i postgis
                                            id_kol = NA,                   # den kolumn som innehåller ett unikt ID och görs till primärnyckelkolumn
                                            addera_data = FALSE,                         # om TRUE så läggs rader till i en tabell, annars skrivs den över (om den finns, annars skrivs en ny tabell)                                            
                                            meddelande_tid = FALSE
  ) {

    starttid <- Sys.time()                                        # Starta tidstagning
    
    # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
    if(is.character(con) && con == "default") {
      con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
      default_flagga = TRUE
    } else  default_flagga = FALSE  
    
    
    # säkerställ att alla kolumnnamn är i gemener, ställer inte till problem i postgis då
    names(inlas_df) <- tolower(names(inlas_df))
    tabell <- tabell %>% tolower()
    
    # kör sql-kod för att skapa ett nytt schema med namn definierat ovan om det inte redan finns
    schema_finns <- postgres_schema_finns(con, schema)
    
    if (!schema_finns) dbExecute(con, paste0("create schema if not exists ", schema, ";"))
    
    # Kontrollera om tabellen redan finns
    tabell_finns <- DBI::dbExistsTable(con, DBI::Id(schema = schema, table = tabell))
    
    system.time({
      if (!tabell_finns) {
        # Om tabellen inte finns
        append_mode <- FALSE
        
      } else if (addera_data) {
        # Om rader ska adderas till befintlig tabell
        append_mode <- TRUE
        
      } else {
        # Om tabellen finns, töm tabellen men behåll struktur och behörigheter
        dbExecute(con, sprintf("TRUNCATE TABLE %s.%s;", schema, tabell))
        append_mode <- FALSE
        
      }
      
      if (append_mode) {
        DBI::dbWriteTable(con, Id(schema = schema, table = tabell), inlas_df, append = TRUE) 
      } else {
        DBI::dbWriteTable(con, Id(schema = schema, table = tabell), inlas_df, overwrite = TRUE)
      }
    })
    
    # gör id_kol till id-kolumn i tabellen
    if (!is.na(id_kol)) {
      dbExecute(con, paste0("ALTER TABLE ", schema, ".", tabell, " ADD PRIMARY KEY (", id_kol ,");"))
    }
    if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
        berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
    if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
    
} # slut funktion

postgres_meta <- function(con = "default",
                          tabell = "aktuell_version",
                          schema = "metadata",
                          query = NA,
                          meddelande_info = FALSE,
                          meddelande_tid = FALSE) {
  
  starttid <- Sys.time()  # Starta tidtagning
  
  # Skapa standarduppkoppling om con är "default"
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Funktion för standarduppkoppling till databasen
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  sql_query <- paste0("SELECT * FROM ", schema, ".", tabell)
  # Konstruera SQL-frågan
  if (!is.na(query)) sql_query <- paste0(sql_query, " ", query)
  
  
  # Försök läsa in data från databasen
  tryCatch({
    retur_df <- dbGetQuery(con, sql_query)
    if (meddelande_info) {
      message(paste("Tabellen", tabell, "från schemat", schema, "har lästs in."))
    }
  }, error = function(e) {
    message(paste("Kunde inte läsa tabellen", tabell, "från schemat", schema, ":", e$message))
    retur_df <- NULL
  })
  
  # Koppla ner om standardanslutningen användes
  if (default_flagga) {
    dbDisconnect(con)
  }
  
  # Beräkna tid för processen och visa om begärt
  berakningstid <- round(as.numeric(difftime(Sys.time(), starttid, units = "secs")), 1)
  if (meddelande_tid) {
    cat(glue::glue("Processen tog {berakningstid} sekunder att köra.\n"))
  }
  
  return(retur_df)
}

postgres_meta_skapa_vy_aktuell_version <- function(
    con                 # uppkoppling
) {
  
  # om vyn aktuell version redan finns i schemat metadata så görs ingenting
  if ("aktuell_version" %in% postgres_lista_scheman_tabeller(con = con)$metadata){
    stop("Vyn aktuell_version i schemat metadata finns redan. Funktionen stoppas.")
  }
  
  # SQL-skript för att skapa vyn
  create_view_sql <- "
  CREATE OR REPLACE VIEW metadata.aktuell_version AS
  SELECT *
  FROM (
      SELECT DISTINCT ON (schema, tabell)
          id,
          schema,
          tabell,
          version_datum,
          version_tid,
          uppdaterad_datum,
          uppdaterad_tid,
          lyckad_uppdatering,
          kommentar
      FROM metadata.uppdateringar
      ORDER BY schema, tabell, uppdaterad_datum DESC, uppdaterad_tid DESC
  ) subquery
  ORDER BY uppdaterad_datum ASC, uppdaterad_tid ASC;
  "
  
  # Kör SQL-kommandot för att skapa vyn - i geodatabasen
  tryCatch({
    dbExecute(con, create_view_sql)
    message("Vyn 'metadata.aktuell_version' skapades framgångsrikt.")
  }, error = function(e) {
    message("Ett fel uppstod: ", e$message)
  })
  
} # slut funktion

postgres_tabell_ta_bort <- function(con = "default", 
                                    schema, 
                                    tabell,
                                    drop_cascade = FALSE,
                                    meddelande_tid = FALSE) {
  
  starttid <- Sys.time()  # Starta tidstagning
  
  # Kombinera schema och tabellnamn
  schema_tabell <- paste0(schema, ".", tabell)
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # Kontrollera om objektet är en vy eller tabell
  # Kontrollera om objektet är en vy eller tabell
  ar_vy <- nrow(DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT 1 FROM information_schema.views 
     WHERE table_schema = {schema} AND table_name = {tabell}",
    .con = con
  ))) > 0
  
  ar_mat_vy <- nrow(DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT 1 FROM pg_matviews 
     WHERE schemaname = {schema} AND matviewname = {tabell}",
    .con = con
  ))) > 0
  
  # Kontrollera om objektet existerar
  if (ar_vy) {
    finns <- TRUE
    drop_typ <- "VIEW"
    typ_text <- "Vyn"
  } else if (ar_mat_vy) {
    finns <- TRUE
    drop_typ <- "MATERIALIZED VIEW"
    typ_text <- "Den materialiserade vyn"
  } else {
    finns <- DBI::dbExistsTable(con, DBI::Id(schema = schema, table = tabell))
    drop_typ <- "TABLE"
    typ_text <- "Tabellen"
  }
  
  if (!finns) {
    message(typ_text, " '", schema_tabell, "' existerar inte. Ingen åtgärd vidtogs.")
  } else {
    # Bygg SQL-frågan
    sql <- paste0(
      "DROP ", drop_typ, " ",
      DBI::dbQuoteIdentifier(con, schema), ".", 
      DBI::dbQuoteIdentifier(con, tabell), 
      if (drop_cascade) " CASCADE;" else ";"
    )
    
    DBI::dbExecute(con, sql)
    
    if (drop_cascade) {
      message(typ_text, " '", schema_tabell, "' har tagits bort med CASCADE.")
    } else {
      message(typ_text, " '", schema_tabell, "' har tagits bort.")
    }
  }
  
  # Koppla ner anslutningen om den skapades som default
  if (default_flagga) dbDisconnect(con)  # Koppla ner om defaultuppkopplingen har använts
  
  # Beräkna och skriv ut tidsåtgång
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)
  if (meddelande_tid) cat(glue::glue("Processen tog {berakningstid} sekunder att köra"))
}

postgres_tabell_finns <- function(
    con = "default", 
    schema, 
    tabell,
    meddelande_tid = FALSE
    ) {
  
  starttid <- Sys.time()  # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  tabell_finns <- dbExistsTable(con, Id(schema = schema, table = tabell))
  
  # Koppla ner anslutningen om den skapades som default
  if (default_flagga) dbDisconnect(con)  # Koppla ner om defaultuppkopplingen har använts
  
  # Beräkna och skriv ut tidsåtgång
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)
  if (meddelande_tid) cat(glue::glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(tabell_finns)
  
  
}



postgres_schema_finns <- function(con, 
                                  schema_namn) {
  query <- sprintf("
    SELECT EXISTS (
      SELECT 1
      FROM information_schema.schemata
      WHERE schema_name = '%s'
    ) AS schema_exists;
  ", schema_namn)
  
  result <- dbGetQuery(con, query)
  return(result$schema_exists[1])
}

postgres_schema_skapa_om_inte_finns <- function(schema_namn, 
                                                meddelande_tid = FALSE,
                                                con = "default"){
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  # kör sql-kod för att skapa ett nytt schema med namn definierat ovan
  dbExecute(con, paste0("create schema if not exists ", schema_namn, ";"))
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}

postgres_schema_ta_bort <- function(con, 
                                    schema,
                                    meddelande_tid = FALSE
) {
  
  starttid <- Sys.time()  # Starta tidstagning
  
  
  # Kontrollera om schemat existerar
  schema_finns <- postgres_schema_finns(con, schema)
  
  if (!schema_finns) {
    message("Schemat '", schema, "' existerar inte. Ingen åtgärd vidtogs.")
  } else {
    # Hämta alla tabeller i schemat
    tabeller <- dbGetQuery(con, paste0("
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = '", schema, "';"))
    
    if (nrow(tabeller) > 0) {
      # Om schemat innehåller tabeller, skriv ut deras namn och avbryt borttagning
      message("Schemat '", schema, "' innehåller följande tabeller och kan inte tas bort:")
      print(tabeller)
      message("Ta bort dessa tabeller innan du försöker ta bort schemat.")
    } else {
      # Om inga tabeller finns, ta bort schemat
      sql <- paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, schema), ";")
      dbExecute(con, sql)
      message("Schemat '", schema, "' har tagits bort.")
    }
  }
  
  # Koppla ner anslutningen om den skapades som default
  #if (default_flagga) dbDisconnect(con)
  
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)  # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}

postgres_metadata_uppdatera <- function(con, schema, tabell, version_datum = NA, version_tid = NA,
                                        uppdaterad_datum = Sys.Date(), uppdaterad_tid = format(Sys.time(), "%H:%M:%S"),
                                        lyckad_uppdatering, kommentar = NA) {
  
  # Funktion för att uppdatera metadata-tabellen varje gång en tabell i geodatabasen
  # uppdateras
  
  # Kontrollera att schema och tabell är angivna
  if (missing(schema) || missing(tabell)) {
    stop("Parametrarna 'schema' och 'tabell' är obligatoriska. Ge dessa parametrar ett värde och försök igen.")
  }
  
  # om version_datum och version_tid = NA så tar de samma värde som uppdaterad_datum och uppdaterad_tid
  if (is.na(version_datum)) version_datum <- uppdaterad_datum
  if (is.na(version_tid)) version_tid <- uppdaterad_tid
  
  # Kontrollera om schemat och tabellen metadata.uppdateringar finns, skapa om nödvändigt
  query_schema_exists <- paste0(
    "SELECT EXISTS (
      SELECT 1 
      FROM information_schema.schemata 
      WHERE schema_name = 'metadata'
    );"
  )
  
  schema_exists <- dbGetQuery(con, query_schema_exists)$exists
  
  if (!schema_exists) {
    dbExecute(con, "CREATE SCHEMA IF NOT EXISTS metadata;")
  }
  
  query_table_exists <- paste0(
    "SELECT EXISTS (
      SELECT 1 
      FROM information_schema.tables 
      WHERE table_schema = 'metadata' AND table_name = 'uppdateringar'
    );"
  )
  
  table_exists <- dbGetQuery(con, query_table_exists)$exists
  
  if (!table_exists) {
    dbExecute(con, paste0(
      "CREATE TABLE metadata.uppdateringar (
        id SERIAL PRIMARY KEY,
        schema TEXT NOT NULL,
        tabell TEXT NOT NULL,
        version_datum DATE,
        version_tid TIME,
        uppdaterad_datum DATE DEFAULT CURRENT_DATE,
        uppdaterad_tid TIME DEFAULT CURRENT_TIME,
        lyckad_uppdatering BOOLEAN,
        kommentar TEXT
      );"
    ))
  }
  
  # Infoga metadata
  insert_query <- paste0(
    "INSERT INTO metadata.uppdateringar (
      id,
      schema,
      tabell,
      version_datum,
      version_tid,
      uppdaterad_datum,
      uppdaterad_tid,
      lyckad_uppdatering,
      kommentar
   ) VALUES (
      (SELECT COALESCE(MAX(id), 0) + 1 FROM metadata.uppdateringar),
      $1, $2, $3, $4, $5, $6, $7, $8
   );"
  )
  
  
  dbExecute(con, insert_query, params = list(
    schema, tabell, version_datum, version_tid,
    uppdaterad_datum, uppdaterad_tid, lyckad_uppdatering, kommentar
  ))
  
  message("Metadata har lagts till för tabellen: ", schema, ".", tabell)
}

postgres_finns_schema_tabell_kolumner <- function(con = "default",        # uppkoppling, vilken databas ansluter vi till
                                                  schema = NULL,          # vilket schema vill vi kontrollera om det finns
                                                  tabell = NULL,          # vilken tabell vill vi kontrollera om den finns
                                                  kolumner = NULL,        # vilka kolumner vill vi kontrollera om de finns, kan uteslutas
                                                  stoppa_vid_fel = TRUE   # vid TRUE returneras ingenting utan koden stoppas
) {
  
  if (is.null(schema) | is.null(tabell) | is.null(kolumner)) stop("Parametrarna schema, tabell och kolumner måste anges för att funktionen ska kunna köras.")
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # Kontrollera att schema finns
  schema_finns <- DBI::dbGetQuery(con, glue::glue("
    SELECT EXISTS (
      SELECT 1 FROM information_schema.schemata 
      WHERE schema_name = '{schema}'
    ) AS finns;
  "))$finns
  
  if (!schema_finns) {
    fel <- glue::glue("Schemat '{schema}' finns inte.")
    if (stoppa_vid_fel) stop(fel)
    return(list(allt_finns = FALSE, felmeddelande = fel))
  }
  
  # Kontrollera att tabellen finns i schemat
  tabell_finns <- DBI::dbGetQuery(con, glue::glue("
    SELECT EXISTS (
      SELECT 1 FROM information_schema.tables 
      WHERE table_schema = '{schema}' AND table_name = '{tabell}'
    ) AS finns;
  "))$finns
  
  if (!tabell_finns) {
    fel <- glue::glue("Tabellen '{tabell}' i schemat '{schema}' finns inte.")
    if (stoppa_vid_fel) stop(fel)
    return(list(allt_finns = FALSE, felmeddelande = fel))
  }
  
  # Kontrollera om angivna kolumner finns i tabellen
  if (!is.null(kolumner)) {
    kolumner_i_tabell <- DBI::dbGetQuery(con, glue::glue("
      SELECT column_name FROM information_schema.columns
      WHERE table_schema = '{schema}' AND table_name = '{tabell}'
    "))$column_name
    
    saknade_kolumner <- kolumner[!kolumner %in% kolumner_i_tabell]
    
    if (length(saknade_kolumner) > 0) {
      fel <- glue::glue(
        "Kolumn{if(length(saknade_kolumner)==1) '' else 'er'} '{paste(saknade_kolumner, collapse = \"', '\")}' i {schema}.{tabell} finns inte."
      )
      if (stoppa_vid_fel) stop(fel)
      return(list(allt_finns = FALSE, felmeddelande = fel))
    }
  }
  
  # Om allt finns
  if (stoppa_vid_fel) {
    #stop(felmeddelande, call. = FALSE)
    return(invisible(NULL))  # tekniskt onåbar kod men gör inget
  } else {
    return(list(allt_finns = TRUE, felmeddelande = NULL))  
  }
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  
} # slut funktion postgres_finns_schema_tabell_kolumner()


postgres_databas_skriv_med_metadata <- function(
    con,
    inlas_df,
    schema,
    tabell, 
    id_kol = NA,
    postgis_addera_data = FALSE,          # om TRUE töms ej tabellen innan ny data skrivs
    felmeddelande_medskickat = NA,
    kommentar_metadata = NA
) {
  # Det här är egentligen bara en wrapper kring funktionen postgis_databas_skriv_med_metadata() som
  # kom först pga att vi skapade geodatabasen först. Men geokolumnen är NA per default och går inte att 
  # ändra. Även id-kolumnen är NA som default. Bra om man har en id-kolumn men inget större måste (tror jag)
  
  postgis_databas_skriv_med_metadata(
    con = con,
    inlas_sf = inlas_df,
    schema = schema,
    tabell = tabell, 
    postgistabell_geo_kol = NA,
    postgistabell_id_kol = id_kol,
    postgis_addera_data = postgis_addera_data,
    felmeddelande_medskickat = felmeddelande_medskickat,
    kommentar_metadata = kommentar_metadata
  )
} # slut funktion postgres_databas_skriv_med_metadata

postgres_grants_auto_skapa <- function(con, 
                                       remove_old = TRUE, 
                                       rattigheter_pa_befintliga = TRUE,
                                       lasroll_tilldela = NULL,
                                       skrivroll_tilldela = NULL) {
  # Skript för att skapa auto-grants som innebär att lasroll och skrivroll får läs- respektive skrivrättigheter
  # när nya scheman, tabeller, vyer och materialiserade vyer skapas oavsett vem som skapar dem.
  # Körs i regel en gång per databas.
  #
  # remove_old = tar bort gamla triggers
  # rattigheter_pa_befintliga = lägger till dessa rättigheter på redan befintliga scheman, tabeller, vyer och materialiserade vyer
  # lasroll_tilldela = tilldelar användare till lasroll
  # skrivroll_tilldela = tilldelar användare till skrivroll
  
  message("🔍 Kontrollerar befintliga event triggers...")
  
  old_triggers <- DBI::dbGetQuery(con, "
    SELECT evtname, evtevent, evtenabled, evtowner::regrole
    FROM pg_event_trigger
    ORDER BY evtname;
  ")
  
  if (nrow(old_triggers) > 0) {
    message("⚠️  Befintliga triggers hittades:\n")
    print(old_triggers)
    
    if (remove_old) {
      message("🧹 Tar bort gamla triggers...")
      DBI::dbExecute(con, "DROP EVENT TRIGGER IF EXISTS auto_grant_tables;")
      DBI::dbExecute(con, "DROP EVENT TRIGGER IF EXISTS auto_grant_schemas;")
    }
  } else {
    message("✅ Inga gamla triggers hittades.")
  }
  
  message("📦 Installerar ny universal auto_grant-funktion...")
  
  DBI::dbExecute(con, "
    CREATE OR REPLACE FUNCTION public.auto_grant()
    RETURNS event_trigger
    LANGUAGE plpgsql
    AS $$
    DECLARE
        obj record;
        schema_name text;
    BEGIN
        FOR obj IN
            SELECT * FROM pg_event_trigger_ddl_commands()
        LOOP
            BEGIN  -- Inre exception block för varje objekt
                -- Tabeller och vyer
                IF obj.object_type IN ('table', 'view', 'materialized view') THEN
                    RAISE NOTICE 'Ger rättigheter på %: %', obj.object_type, obj.object_identity;
                    EXECUTE format('GRANT SELECT ON %s TO lasroll;', obj.object_identity);
                    EXECUTE format('GRANT SELECT, INSERT, UPDATE, DELETE ON %s TO skrivroll;', obj.object_identity);

                -- Scheman
                ELSIF obj.object_type = 'schema' THEN
                    schema_name := obj.object_identity;
                    RAISE NOTICE 'Ger rättigheter på schema: %', schema_name;
                    
                    EXECUTE format('GRANT USAGE ON SCHEMA %I TO lasroll;', schema_name);
                    EXECUTE format('GRANT USAGE, CREATE ON SCHEMA %I TO skrivroll;', schema_name);
                    EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT ON TABLES TO lasroll;', schema_name);
                    EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO skrivroll;', schema_name);
                END IF;
            EXCEPTION
                WHEN OTHERS THEN
                    RAISE WARNING 'Fel vid grant för % (type: %): %', obj.object_identity, obj.object_type, SQLERRM;
                    -- Fortsätt ändå istället för att krascha hela transaktionen
            END;
        END LOOP;
    END;
    $$;
  ")
  
  message("⚙️ Skapar nya event triggers...")
  
  DBI::dbExecute(con, "DROP EVENT TRIGGER IF EXISTS auto_grant_tables;")
  DBI::dbExecute(con, "
    CREATE EVENT TRIGGER auto_grant_tables
      ON ddl_command_end
      WHEN TAG IN ('CREATE TABLE', 'CREATE VIEW', 'CREATE MATERIALIZED VIEW')
      EXECUTE FUNCTION public.auto_grant();
  ")
  
  DBI::dbExecute(con, "DROP EVENT TRIGGER IF EXISTS auto_grant_schemas;")
  DBI::dbExecute(con, "
    CREATE EVENT TRIGGER auto_grant_schemas
      ON ddl_command_end
      WHEN TAG IN ('CREATE SCHEMA')
      EXECUTE FUNCTION public.auto_grant();
  ")
  
  message("✅ Klart! Nya triggers och funktion är installerade.")
  
  if (rattigheter_pa_befintliga == TRUE) {
    message("🔄 Uppdaterar även rättigheter på befintliga objekt...")
    postgres_grants_pa_befintliga_objekt(con)
  }
  
  # --- Tilldela användare till roller ---
  if (!is.null(lasroll_tilldela) && length(lasroll_tilldela) > 0) {
    purrr::walk(lasroll_tilldela, ~{
      tryCatch({
        DBI::dbExecute(con, glue::glue("GRANT lasroll TO {.x};"))
        message(glue::glue("✅ Tilldelade lasroll till {.x}"))
      },
      error = function(e) {
        message(glue::glue("⚠️  Kunde inte tilldela lasroll till {.x}: {e$message}"))
      })
    })
  }
  
  if (!is.null(skrivroll_tilldela) && length(skrivroll_tilldela) > 0) {
    purrr::walk(skrivroll_tilldela, ~{
      tryCatch({
        DBI::dbExecute(con, glue::glue("GRANT skrivroll TO {.x};"))
        message(glue::glue("✅ Tilldelade skrivroll till {.x}"))
      },
      error = function(e) {
        message(glue::glue("⚠️  Kunde inte tilldela skrivroll till {.x}: {e$message}"))
      })
    })
  }
  
}

postgres_grants_pa_befintliga_objekt <- function(con) {
  # skript för att ge lasroll och skrivroll  läs- respektive skrivrättigheter
  # för befintliga scheman, tabeller, vyer och materialiserade vyer
  
  message("📦 Ger rättigheter till befintliga scheman, tabeller och vyer...")
  
  sql_statements <- c(
    # 1️⃣ Ge rättigheter till alla scheman
    "
    DO $$
    DECLARE
        s RECORD;
    BEGIN
        FOR s IN
            SELECT schema_name
            FROM information_schema.schemata
            WHERE schema_name NOT IN ('pg_catalog', 'information_schema', 'pg_toast')
        LOOP
            EXECUTE format('GRANT USAGE ON SCHEMA %I TO lasroll;', s.schema_name);
            EXECUTE format('GRANT USAGE, CREATE ON SCHEMA %I TO skrivroll;', s.schema_name);
        END LOOP;
    END$$;
    ",
    
    # 2️⃣ Ge rättigheter till alla tabeller och vyer
    "
    DO $$
    DECLARE
        t RECORD;
    BEGIN
        FOR t IN
            SELECT table_schema, table_name
            FROM information_schema.tables
            WHERE table_schema NOT IN ('pg_catalog', 'information_schema', 'pg_toast')
        LOOP
            EXECUTE format('GRANT SELECT ON %I.%I TO lasroll;', t.table_schema, t.table_name);
            EXECUTE format('GRANT SELECT, INSERT, UPDATE, DELETE ON %I.%I TO skrivroll;', t.table_schema, t.table_name);
        END LOOP;
    END$$;
    ",
    
    # 3️⃣ Ge rättigheter till alla materialized views
    "
    DO $$
    DECLARE
        mv RECORD;
    BEGIN
        FOR mv IN
            SELECT schemaname, matviewname
            FROM pg_matviews
            WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast')
        LOOP
            EXECUTE format('GRANT SELECT ON %I.%I TO lasroll;', mv.schemaname, mv.matviewname);
            EXECUTE format('GRANT SELECT, INSERT, UPDATE, DELETE ON %I.%I TO skrivroll;', mv.schemaname, mv.matviewname);
        END LOOP;
    END$$;
    "
  )
  
  purrr::walk(sql_statements, ~DBI::dbExecute(con, .x))
  message("✅ Rättigheter till befintliga objekt har uppdaterats.")
}


postgres_grants_auto_visa <- function(con) {
  message("🔍 Hämtar befintliga event triggers...")
  event_triggers <- DBI::dbGetQuery(con, "
    SELECT 
      evtname AS trigger_name,
      evtevent AS event,
      evtenabled AS enabled,
      evtowner::regrole AS owner
    FROM pg_event_trigger
    ORDER BY evtname;
  ")
  
  if (nrow(event_triggers) == 0) {
    message("⚪ Inga event triggers hittades.")
  } else {
    message("✅ Följande event triggers finns:")
    print(tibble::as_tibble(event_triggers))
  }
  
  message("\n🔍 Hämtar befintliga default privileges...")
  default_privs <- DBI::dbGetQuery(con, "
    SELECT
      defaclrole::regrole AS role_owner,
      defaclnamespace::regnamespace AS schema,
      defaclobjtype AS object_type,
      defaclacl AS privileges
    FROM pg_default_acl
    ORDER BY defaclrole, defaclnamespace;
  ")
  
  if (nrow(default_privs) == 0) {
    message("⚪ Inga default privileges hittades.")
  } else {
    message("✅ Följande default privileges finns:")
    default_parsed <- default_privs %>%
      dplyr::mutate(
        privileges = as.character(privileges)
      ) %>%
      tidyr::separate_rows(privileges, sep = ",") %>%
      dplyr::mutate(
        privileges = stringr::str_remove_all(privileges, "[{}\"]"),
        grantee = stringr::str_extract(privileges, "^[^=]+"),
        rights = stringr::str_extract(privileges, "(?<==)[^/]+"),
        granted_by = stringr::str_extract(privileges, "(?<=/).*"),
        rights_verbose = dplyr::case_when(
          rights == "r" ~ "SELECT only",
          rights == "arwd" ~ "ALL (SELECT, INSERT, UPDATE, DELETE)",
          TRUE ~ rights
        )
      ) %>%
      dplyr::select(role_owner, schema, object_type, grantee, rights_verbose, granted_by)
    
    print(tibble::as_tibble(default_parsed))
  }
  
  message("\n📋 Sammanställning klar.")
  invisible(list(
    event_triggers = event_triggers,
    default_privs = default_privs
  ))
}

postgres_grants_auto_testa <- function(con) {
  # testar om triggers för att skapa rättigheter för lasroll och skrivroll fungerar
  
  results <- tibble(
    kontroll = character(),
    status = character(),
    kommentar = character()
  )
  
  # 1️⃣ Kolla triggers
  triggers <- dbGetQuery(con, "
    SELECT evtname, evtenabled
    FROM pg_event_trigger
    WHERE evtname IN ('auto_grant_tables', 'auto_grant_schemas');
  ")
  
  if (nrow(triggers) == 2 && all(triggers$evtenabled == "O")) {
    results <- add_row(results,
                       kontroll = "Event triggers aktiva",
                       status = "OK",
                       kommentar = "auto_grant_tables och auto_grant_schemas finns och är aktiva"
    )
  } else {
    results <- add_row(results,
                       kontroll = "Event triggers aktiva",
                       status = "FAIL",
                       kommentar = "En eller båda triggers saknas eller är inaktiva"
    )
  }
  
  # 2️⃣ Kolla default privileges
  defaults <- dbGetQuery(con, "
    SELECT defaclrole::regrole AS role_owner,
           defaclnamespace::regnamespace AS schema,
           defaclobjtype AS object_type,
           defaclacl AS privileges
    FROM pg_default_acl;
  ")
  
  has_defaults <- any(grepl("lasroll", defaults$privileges)) &&
    any(grepl("skrivroll", defaults$privileges))
  
  if (has_defaults) {
    results <- add_row(results,
                       kontroll = "Default privileges",
                       status = "OK",
                       kommentar = "ALTER DEFAULT PRIVILEGES inkluderar lasroll och skrivroll"
    )
  } else {
    results <- add_row(results,
                       kontroll = "Default privileges",
                       status = "FAIL",
                       kommentar = "Hittade inga default privileges för lasroll/skrivroll"
    )
  }
  
  # Namn på testschema
  test_schema <- "test_auto_grant"
  test_table <- "tabell_test"
  test_view <- "vy_test"
  test_matview <- "matvy_test"
  
  try({
    dbExecute(con, glue("DROP SCHEMA IF EXISTS {test_schema} CASCADE;"))
    dbExecute(con, glue("CREATE SCHEMA {test_schema};"))
    
    # 3️⃣ Skapa test-tabell
    dbExecute(con, glue("CREATE TABLE {test_schema}.{test_table} (id serial, namn text);"))
    
    # 4️⃣ Skapa test-vy
    dbExecute(con, glue("CREATE VIEW {test_schema}.{test_view} AS SELECT * FROM {test_schema}.{test_table};"))
    
    # 5️⃣ Skapa test-materialized view
    dbExecute(con, glue("CREATE MATERIALIZED VIEW {test_schema}.{test_matview} AS SELECT * FROM {test_schema}.{test_table};"))
    
    # Kontrollera rättigheter för alla tre typer
    grants <- dbGetQuery(con, glue("
      SELECT table_name, grantee, privilege_type
      FROM information_schema.role_table_grants
      WHERE table_schema = '{test_schema}'
        AND table_name IN ('{test_table}', '{test_view}', '{test_matview}')
        AND grantee IN ('lasroll', 'skrivroll')
      ORDER BY table_name, grantee, privilege_type;
    "))
    
    check_grants <- function(obj) {
      subset <- grants %>% filter(table_name == obj)
      las_ok <- "lasroll" %in% subset$grantee && "SELECT" %in% subset$privilege_type[subset$grantee == "lasroll"]
      skriv_ok <- "skrivroll" %in% subset$grantee &&
        all(c("SELECT", "INSERT", "UPDATE", "DELETE") %in% subset$privilege_type[subset$grantee == "skrivroll"])
      list(las_ok = las_ok, skriv_ok = skriv_ok)
    }
    
    for (obj in c(test_table, test_view, test_matview)) {
      res <- check_grants(obj)
      status <- if (res$las_ok && res$skriv_ok) "OK" else "FAIL"
      results <- add_row(
        results,
        kontroll = glue("Trigger-test ({obj})"),
        status = status,
        kommentar = if (status == "OK") "Rättigheter satta automatiskt" else "Rättigheter saknas eller felaktiga"
      )
    }
    
    # 6️⃣ Städar bort testschema
    dbExecute(con, glue("DROP SCHEMA IF EXISTS {test_schema} CASCADE;"))
  }, silent = TRUE)
  
  message("\n📋 Resultat från test:")
  print(results)
  invisible(results)
}



# ================================= postgis-funktioner ================================================

# 
postgis_installera_i_postgres_db <- function(con) {
  
  installera_postgis_query <- "CREATE EXTENSION IF NOT EXISTS postgis;"
  DBI::dbExecute(con, installera_postgis_query)
}

# Skript för att läsa in ett sf-objekt till en postgis-tabell 
postgis_sf_till_postgistabell <- 
  function(con = "default",
           inlas_sf,
           schema = "karta",                            # det schema i postgis-databasen som målpunktstabellen ska ligga under
           tabell,                                      # det tabellnamn den nya filen ska få i postgis
           postgistabell_id_kol = NA,                   # den kolumn som innehåller ett unikt ID och görs till primärnyckelkolumn
           postgistabell_geo_kol = NA,                  # geometry-kolumnens namn
           skapa_spatialt_index = TRUE,
           #nytt_schema_oppet_for_geodata_las = TRUE,    # om TRUE så öppnas läsrättigheter för användaren geodata_las (vilket är det som ska användas om det inte finns mycket goda skäl att låta bli)
           addera_data = FALSE,                         # om TRUE så läggs rader till i en tabell, annars skrivs den över (om den finns, annars skrivs en ny tabell)
           skriv_over_tabell_om_finns = FALSE,            # TRUE så tar den bort hela tabellen och skriver en ny (tex. bra om man behöver lägga till kolumner), annars är FALSE bra som behåller strukturen på tabellen
           #postgistabell_till_crs,
           meddelande_tid = FALSE
  ) {
    #
    # Följande parametrar skickas med funktionen:
    # inlas_mapp = mapp i vilken tabellen finns som innehåller målpunkterna, måste innehålla kolumner
    #              för x- och y- koordinat
    # inlas_filer = en vektor med den eller de filer som ska läsas in, måste finnas i inlas_mapp
    # tabell      = en textsträng eller vektor om det finns flera filer med tabellnamnet som 
    #                    målpunkterna ska ha i postgisdatabasen (bör vara gemener och utan konstiga tecken)
    # schema = det schema i postgisdatabasen som målpunktstabellen ska ligga under
    # postgistabell_id_kol = den kolumn som innehåller ett unikt ID och görs till primärnyckelkolumn, måste finnas!
    # postgistabell_geo_kol = geometry-kolumnen
    
    if (all(is.na(postgistabell_geo_kol)) & skapa_spatialt_index) stop("postgistabell_geo_kol måste skickas med om skapa_spatialt_index är satt till TRUE. Om du vill skapa en tabell utan geografi måste skapa_spatialt_index sättas till FALSE.")
    
    starttid <- Sys.time()                                        # Starta tidstagning
    
    # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
    if(is.character(con) && con == "default") {
      con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
      default_flagga = TRUE
    } else  default_flagga = FALSE  
    
    
    # säkerställ att alla kolumnnamn är i gemener, ställer inte till problem i postgis då
    names(inlas_sf) <- tolower(names(inlas_sf))
    tabell <- tabell %>% tolower()
    
    # kör sql-kod för att skapa ett nytt schema med namn definierat ovan om det inte redan finns
    schema_finns <- postgres_schema_finns(con, schema)
    
    if (!schema_finns) dbExecute(con, paste0("create schema if not exists ", schema, ";"))
  
    # Kontrollera om tabellen redan finns
    tabell_finns <- DBI::dbExistsTable(con, DBI::Id(schema = schema, table = tabell))
    
    system.time({
      if (!tabell_finns) {
        # Om tabellen inte finns
        append_mode <- FALSE
        
      } else if (addera_data) {
        # Om rader ska adderas till befintlig tabell
        append_mode <- TRUE
        
      } else {
        # Om tabellen finns, töm tabellen men behåll struktur och behörigheter
        dbExecute(con, sprintf("TRUNCATE TABLE %s.%s;", schema, tabell))
        append_mode <- !skriv_over_tabell_om_finns               # om append_mode är FALSE så skrivs hela tabellen över som en ny tabell, vid TRUE så  behålls strukturen på tabellen (kolumnerna måste vara samma)
        
      }
      
      # Skriv rader till angiven tabell
      st_write(
        obj   = inlas_sf,
        dsn   = con,
        layer = DBI::Id(schema = schema, table = tabell),
        append = append_mode
      )
    })
    
    if (!all(is.na(postgistabell_geo_kol))) {
      for (geokol in postgistabell_geo_kol) {
        DBI::dbExecute(con, glue::glue(
          "SELECT UpdateGeometrySRID('{schema}', '{tabell}', '{geokol}', {sf::st_crs(inlas_sf)$epsg});"
        ))
      }
    }
    
    # # skriv rut-lagren till postgis 
    # starttid = Sys.time()
    # st_write(obj = inlas_sf,
    #          dsn = con,
    #          Id(schema=schema, table = tabell))
    # print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "sec"),1), " sekunder att läsa in ", tabell, " till postgis."))
    
    # skapa spatialt index, finns det sedan tidigare, ta bort - loopa så att man kan skicka fler geokolumner
    if (skapa_spatialt_index) {
      for (geokol in 1:length(postgistabell_geo_kol)) {
        dbExecute(con, paste0("DROP INDEX IF EXISTS ", schema, ".", postgistabell_geo_kol[geokol], "_idx;")) 
        dbExecute(con, paste0("CREATE INDEX ", postgistabell_geo_kol[geokol], "_idx ON ", schema, ".", tabell, " USING GIST (", postgistabell_geo_kol[geokol], ");"))
      }  
    }
    # gör id_kol till id-kolumn i tabellen
    if (!is.na(postgistabell_id_kol)) {
      dbExecute(con, paste0("ALTER TABLE ", schema, ".", tabell, " ADD PRIMARY KEY (", postgistabell_id_kol ,");"))
    }
    if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
    berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
    if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
    
  } # slut funktion

postgis_postgistabell_till_sf <- function(
    con = "default",
    schema,                 # det schema i vilken tabellen finns som man vill hämta
    tabell,                 # den tabell i postgisdatabasen man vill hämta
    query = NA,     # om man inte skickar med någon query hämtas hela tabellen
    meddelande_tid = FALSE
){
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE 
  
  if (is.na(query)) {
    query <- paste0("SELECT * FROM ", schema, ".", tabell)
  } else {
    query <- paste0("SELECT * FROM ", schema, ".", tabell, " ", query)
  }
  
  retur_sf <- st_read(con, query = query)
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(retur_sf)
} # slut funktion

postgis_kopiera_tabell <- function(con = "default",
                                   schema_fran, 
                                   tabell_fran,
                                   schema_till,
                                   tabell_till,
                                   skriv_over = FALSE,       # skriver över tabell-till om den finns
                                   meddelande_tid = FALSE
) {
  
  # funktion för att kopiera en tabell i en postgisdatabas till en annan tabell
  # i samma schema eller under ett annat schema
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  # kontroll om den tabell som ska kopieras finns, annars stoppas skriptet
  if (!postgres_tabell_finns(con = con, schema = schema_fran, tabell = tabell_fran)) {
    stop(glue("Tabellen {tabell_fran} i schemat {schema_fran} finns inte, kontrollera namnet på schemat och tabellen och försök igen."))
  }
  
  # kollar om tabell-till redan finns, i så fall stannar vi skriptet om inte skriv_over är satt till TRUE
  if (postgres_tabell_finns(con = con, schema = schema_till, tabell = tabell_till)) {
    if (skriv_over) {
      postgres_tabell_ta_bort(con = con, schema = schema_till, tabell = tabell_till)
    } else {
      stop(glue("Tabellen {tabell_till} i schemat {schema_till} finns redan, välj ett annat namn eller sätt skriv_over till TRUE om du vill skriva över befintlig tabell."))
    }
    
  }
  # skapa tabell som har samma struktur som den tabell vi ska kopiera
  dbExecute(con, paste0("CREATE TABLE ", schema_till, ".", tabell_till, " (LIKE ", schema_fran, ".", tabell_fran, " INCLUDING ALL);"))
  
  # fyll på den nya tabellen med data från tabellen vi kopierar från
  dbExecute(con, paste0("INSERT INTO ", schema_till, ".", tabell_till, " SELECT * ",  
                        "FROM ", schema_fran, ".", tabell_fran, ";"))
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}                      

postgis_kopiera_tabell_mellan_databaser <- function(
    con_fran_databas,           # en con-uppkoppling för den databas vi kopierar tabellen från 
    con_till_databas,           # en con-uppkoppling för den databas vi kopierar tabellen till, dvs. en annan än con_fran_databas
    schema_fran,                # schema i den databas vi kopierar tabellen från
    tabell_fran,                # tabell i den databas vi kopierar tabellen från
    schema_till,                # schema i den databas vi kopierar tabellen till
    tabell_till,                # tabell i den databas vi kopierar tabellen till
    lagg_till_metadata_i_till_databas = FALSE,      # om TRUE så läggs metadata till enligt standardförfarande i tilldatabasen
    meddelande_tid = FALSE      # TRUE om vi vill ha meddelande om hur lång tid det tog att köra funktionen, FALSE är default
) {
  
  # funktion för att kopiera en tabell i en postgisdatabas till en annan tabell
  # i samma schema eller under ett annat schema
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con_fran_databas) && con_fran_databas == "default") {
    con_fran_databas <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga_input = TRUE
  } else  default_flagga_input = FALSE
  
  tryCatch({
    # 1. Läs in tabellen (som sf-objekt om den innehåller geometri)
    #input_data <- st_read(con_fran_databas, query = glue("SELECT * FROM {schema_fran}.{tabell_fran}"))
    input_data <- st_read(con_fran_databas, layer = DBI::Id(schema = schema_fran, table = tabell_fran))
    
    # 2. Skriv till den andra databasen
    st_write(dsn = con_till_databas,
             obj = input_data,
             layer = DBI::Id(schema = schema_till, table = tabell_till),
             append = FALSE)
    
    # säkerställ att 
    srid <- st_crs(input_data)$epsg
    geom_kol <- attr(input_data, "sf_column")
    
    if (!is.na(srid) && !is.null(geom_kol)) {
      dbExecute(con_till_databas, glue(
        "SELECT UpdateGeometrySRID('{schema_till}', '{tabell_till}', '{geom_kol}', {srid});"
      ))
    }
    
    meta_frantabell <- postgres_meta(
      con = con_geodb,
      query = glue("WHERE schema = '{schema_fran}' AND tabell = '{tabell_fran}'")
    ) %>% 
      select(version_datum, version_tid, kommentar)
    
    
    if (nrow(meta_frantabell) == 0) {
    # om det inte finns någon rad i metadata-tabellen
      nu <- now()
      dag <- as.integer(format(with_tz(nu, tzone = Sys.timezone()), "%d"))
      manad_ar <- tolower(format(with_tz(nu, tzone = Sys.timezone()), "%b%Y"))
      tid <- format(nu, "%H%M")
      frantabell_ver <- paste0(dag, manad_ar, "_", tid)
      
      frantabell_ver_db <<- glue("{schema_fran}.{tabell_fran} ver: {frantabell_ver}")
      ver_datum <<- format(nu, "%Y-%m-%d")
      ver_tid <<- format(nu, "%H:%M")
    } else {
      # om det finns en rad i metadatatabellen används den
      ver_datum <<- meta_frantabell$version_datum
      ver_tid <<- meta_frantabell$version_tid
      frantabell_ver_db <<- meta_frantabell$kommentar
    }
    lyckad_uppdatering <- TRUE
    
  }, error = function(e) {
    # Skriv ett felmeddelande om något går fel
    frantabell_ver_db <- glue("{e$message}")
    lyckad_uppdatering <- FALSE
    
  }, finally = {
    # kod som körs oavsett om skriptet fungerade att köra eller inte
    # hämta metadata för fran-tabellen
    
    postgres_metadata_uppdatera(
      con = con_till_databas,
      schema = schema_till,
      tabell = tabell_till,
      version_datum = ver_datum,
      version_tid = ver_tid,
      lyckad_uppdatering = lyckad_uppdatering,
      kommentar = frantabell_ver_db
    )
  }) # slut finally och tryCatch()
  
  if(default_flagga_input) dbDisconnect(con_fran_databas)                                        # Koppla ner om defaultuppkopplingen har använts
  #dbDisconnect(con_till_databas)                                        
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}


postgis_flytta_tabell <- function(con = "default",
                                  schema_fran,
                                  tabell_fran,
                                  schema_till,
                                  meddelande_tid = FALSE
) {
  
  # funktion för att flytta en tabell från ett schema till ett annat
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  # byt schema för en tabell
  dbExecute(con_flytt, paste0("ALTER TABLE ", schema_fran, ".", tabell_fran, " SET SCHEMA ", schema_till, ";"))
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}

postgis_aktivera_i_postgres_db <- function(con = "default") {
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # SQL-fråga för att aktivera PostGIS
  aktivera_postgis_query <- "CREATE EXTENSION IF NOT EXISTS postgis;"
  
  # Kör SQL-frågan och hantera fel
  tryCatch({
    dbExecute(con, aktivera_postgis_query)
    message("PostGIS-tillägget har aktiverats i databasen.")
  }, error = function(e) {
    message("Kunde inte aktivera PostGIS-tillägget:", e$message)
  })
  
  # Koppla ner anslutningen om den skapades som default
  if (default_flagga) dbDisconnect(con)
}

postgis_isokroner_bil <- function(
    punkter_sf = NULL,
    schema_punkt = NULL,             # tex "punktlager",
    tabell_punkt = NULL,             # tex "akutmottagningar",
    idkol_punkt = "id",
    nodkolumn_punkt = "nid_nvdb_bil_adresser",    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = "namn",
    intervall_varden = c(15, 30, 45, 60),
    spara_schema = NULL,        # om man vill spara i databasen, ange schema här
    spara_tabell = NULL,        # om man vill spara i databasen, ange tabell här
    returnera_sf = TRUE,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = TRUE   # TRUE så returneras polygoner utan överlappning, annars täcker de större polygonerna även ytan för de mindre polygonerna, med TRUE blir det som "ringar"
    
) {
  retur_sf <- postgis_isokroner_skapa(
    punkter_sf = punkter_sf,
    schema_punkt = schema_punkt,             
    tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt,
    nodkolumn_punkt = nodkolumn_punkt,    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = namnkol_punkt,
    tabell_graf = "nvdb_bil_adresser",
    idkol_graf = "rad_id",
    kostnadskol_graf_f = "kostnad_bil_f_min",
    kostnadskol_graf_b = "kostnad_bil_b_min",
    intervall_varden = intervall_varden,
    kostnad_enhet = "auto",
    restyp = "bil",
    spara_schema = spara_schema,        # om man vill spara i databasen, ange schema här
    spara_tabell = spara_tabell,        # om man vill spara i databasen, ange tabell här
    returnera_sf = returnera_sf,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = dela_upp_polygoner   
  )
  
} # slut funktion isokron_bil

postgis_isokroner_meter <- function(
    punkter_sf = NULL,
    schema_punkt = NULL,             # tex "punktlager",
    tabell_punkt = NULL,             # tex "akutmottagningar",
    idkol_punkt = "id",
    nodkolumn_punkt = "nid_nvdb_alla_adresser",    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = "namn",
    intervall_varden = c(1000, 5000, 10000, 20000),
    spara_schema = NULL,        # om man vill spara i databasen, ange schema här
    spara_tabell = NULL,        # om man vill spara i databasen, ange tabell här
    returnera_sf = TRUE,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = TRUE   # TRUE så returneras polygoner utan överlappning, annars täcker de större polygonerna även ytan för de mindre polygonerna, med TRUE blir det som "ringar"
    
) {
  retur_sf <- postgis_isokroner_skapa(
    punkter_sf = punkter_sf,
    schema_punkt = schema_punkt,             
    tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt,
    nodkolumn_punkt = nodkolumn_punkt,    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = namnkol_punkt,
    tabell_graf = "nvdb_alla_adresser",
    idkol_graf = "rad_id",
    kostnadskol_graf_f = "kostnad_meter",
    kostnadskol_graf_b = NULL,
    intervall_varden = intervall_varden,
    kostnad_enhet = "auto",
    restyp = "meter",
    spara_schema = spara_schema,        # om man vill spara i databasen, ange schema här
    spara_tabell = spara_tabell,        # om man vill spara i databasen, ange tabell här
    returnera_sf = returnera_sf,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = dela_upp_polygoner   
  )
  
} # slut funktion isokron_meter

postgis_isokroner_gang <- function(
    punkter_sf = NULL,
    schema_punkt = NULL,             # tex "punktlager",
    tabell_punkt = NULL,             # tex "akutmottagningar",
    idkol_punkt = "id",
    nodkolumn_punkt = "nid_nvdb_alla_adresser",    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = "namn",
    intervall_varden = c(15, 30, 45, 60),
    spara_schema = NULL,        # om man vill spara i databasen, ange schema här
    spara_tabell = NULL,        # om man vill spara i databasen, ange tabell här
    returnera_sf = TRUE,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = TRUE   # TRUE så returneras polygoner utan överlappning, annars täcker de större polygonerna även ytan för de mindre polygonerna, med TRUE blir det som "ringar"
    
) {
  retur_sf <- postgis_isokroner_skapa(
    punkter_sf = punkter_sf,
    schema_punkt = schema_punkt,             
    tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt,
    nodkolumn_punkt = nodkolumn_punkt,    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = namnkol_punkt,
    tabell_graf = "nvdb_alla_adresser",
    idkol_graf = "rad_id",
    kostnadskol_graf_f = "kostnad_gang_min",
    kostnadskol_graf_b = NULL,
    intervall_varden = intervall_varden,
    kostnad_enhet = "auto",
    restyp = "till fots",
    spara_schema = spara_schema,        # om man vill spara i databasen, ange schema här
    spara_tabell = spara_tabell,        # om man vill spara i databasen, ange tabell här
    returnera_sf = returnera_sf,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = dela_upp_polygoner   
  )
  
} # slut funktion isokron_gang

postgis_isokroner_cykel <- function(
    punkter_sf = NULL,
    schema_punkt = NULL,             # tex "punktlager",
    tabell_punkt = NULL,             # tex "akutmottagningar",
    idkol_punkt = "id",
    nodkolumn_punkt = "nid_nvdb_alla_adresser",    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = "namn",
    intervall_varden = c(15, 30, 45, 60),
    spara_schema = NULL,        # om man vill spara i databasen, ange schema här
    spara_tabell = NULL,        # om man vill spara i databasen, ange tabell här
    returnera_sf = TRUE,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = TRUE   # TRUE så returneras polygoner utan överlappning, annars täcker de större polygonerna även ytan för de mindre polygonerna, med TRUE blir det som "ringar"
    
) {
  retur_sf <- postgis_isokroner_skapa(
    punkter_sf = punkter_sf,
    schema_punkt = schema_punkt,             
    tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt,
    nodkolumn_punkt = nodkolumn_punkt,    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = namnkol_punkt,
    tabell_graf = "nvdb_alla_adresser",
    idkol_graf = "rad_id",
    kostnadskol_graf_f = "kostnad_cykel_min",
    kostnadskol_graf_b = NULL,
    intervall_varden = intervall_varden,
    kostnad_enhet = "auto",
    restyp = "cykel",
    spara_schema = spara_schema,        # om man vill spara i databasen, ange schema här
    spara_tabell = spara_tabell,        # om man vill spara i databasen, ange tabell här
    returnera_sf = returnera_sf,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = dela_upp_polygoner   
  )
  
} # slut funktion isokron_cykel

postgis_isokroner_elcykel <- function(
    punkter_sf = NULL,
    schema_punkt = NULL,             # tex "punktlager",
    tabell_punkt = NULL,             # tex "akutmottagningar",
    idkol_punkt = "id",
    nodkolumn_punkt = "nid_nvdb_alla_adresser",    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = "namn",
    intervall_varden = c(15, 30, 45, 60),
    spara_schema = NULL,        # om man vill spara i databasen, ange schema här
    spara_tabell = NULL,        # om man vill spara i databasen, ange tabell här
    returnera_sf = TRUE,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = TRUE   # TRUE så returneras polygoner utan överlappning, annars täcker de större polygonerna även ytan för de mindre polygonerna, med TRUE blir det som "ringar"
    
) {
  retur_sf <- postgis_isokroner_skapa(
    punkter_sf = punkter_sf,
    schema_punkt = schema_punkt,             
    tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt,
    nodkolumn_punkt = nodkolumn_punkt,    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = namnkol_punkt,
    tabell_graf = "nvdb_alla_adresser",
    idkol_graf = "rad_id",
    kostnadskol_graf_f = "kostnad_elcykel_min",
    kostnadskol_graf_b = NULL,
    intervall_varden = intervall_varden,
    kostnad_enhet = "auto",
    restyp = "elcykel",
    spara_schema = spara_schema,        # om man vill spara i databasen, ange schema här
    spara_tabell = spara_tabell,        # om man vill spara i databasen, ange tabell här
    returnera_sf = returnera_sf,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = dela_upp_polygoner   
  )
  
} # slut funktion isokron_elcykel

postgis_isokroner_skapa <- function(
    con = "default",
    punkter_sf = NULL,               # om man vill skicka med ett sf-objekt istället för en punkttabell i postgis
    schema_punkt = NULL,             # tex "punktlager",
    tabell_punkt = NULL,             # tex "akutmottagningar",
    idkol_punkt,
    nodkolumn_punkt = "nid_nvdb_alla_adresser",    # NULL, eller om man har ett punktlager som redan är kopplat till en graf kan kolumnen för noderna läggas till här
    namnkol_punkt = "namn",
    schema_graf = "grafer",
    tabell_graf = "nvdb_alla_adresser",
    idkol_graf = "rad_id",
    kostnadskol_graf_f = "kostnad_meter",
    kostnadskol_graf_b = NULL,
    intervall_varden = c(5000, 10000, 20000, 30000),
    kostnad_enhet = "auto",     # avgörs av om "meter", "min" etc. ingår i kostnadskol_graf_f
    restyp = "meter",           # skickar med en kolumn med restyp, kan vara "bil", "gång", längd meter", "cykel"
    spara_schema = NULL,        # om man vill spara i databasen, ange schema här
    spara_tabell = NULL,        # om man vill spara i databasen, ange tabell här
    returnera_sf = TRUE,        # om man vill returnera ett sf-objekt
    dela_upp_polygoner = TRUE,  # TRUE så returneras polygoner utan överlappning, annars täcker de större polygonerna även ytan för de mindre polygonerna, med TRUE blir det som "ringar"
    polygon_metod = "concave",  # eller "buffer", "convex"
    buffer_m = 100,             # sätter en buffer på vägar när man vill bygga polygonerna runt vägarna (långsammare än convex hull och concave hull)
    simplify_tol = 100,         # för att förenkla polygoner ju högre desto mer förenkling, nära 0 nästan ingen förenkling, används bara i buffer-metoden
    visa_meddelanden = FALSE,   # om man vill se vad som görs i postgis, oftast vill man inte det
    felsokning_meddelanden = FALSE    # för att se vad som händer i felsökning
) {
  # man måste ha skickat med ett sf-objekt eller en tabell i den databas man jobbar i
  stopifnot(!is.null(punkter_sf) || (!is.null(schema_punkt) && !is.null(tabell_punkt)))
  # man måste returnera ett sf-objekt eller spara i den databas man jobbar i
  stopifnot(returnera_sf || (!is.null(spara_schema) && !is.null(spara_tabell)))
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_adm("ruttanalyser")  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # stäng av meddelanden om visa_meddelanden = FALSE
  if (!visa_meddelanden) DBI::dbExecute(con, "SET client_min_messages TO warning;")
  
  if (is.null(kostnadskol_graf_b)) kostnadskol_graf_b <- kostnadskol_graf_f
  
  # skriv punkter_sf till ett temporär tabell i postgis eller använd den som skickats med
  if (!is.null(punkter_sf)) {
    
    # döp om geometri-kolumn till geom (som är det namn som används nedan)
    geometri_kolumn <- punkter_sf %>% attr("sf_column")
    punkter_sf <- punkter_sf %>% 
      rename(geom = !!geometri_kolumn)
    
    postgres_schema_skapa_om_inte_finns(con = con, schema_namn = "temp")
    sf::st_write(punkter_sf, con, DBI::Id(schema = "temp", table = "franpunkter"), delete_layer = TRUE)
    temptabell <- "temp.franpunkter"
    schema_punkt <- "temp"
    tabell_punkt <- "franpunkter"
  } else {
    temptabell <- paste0(schema_punkt, ".", tabell_punkt)
  }
  
  # kontrollera om namnkolumn finns
  namnkolumn_finns <- postgres_lista_kolumnnamn_i_schema(con = con, schema = schema_punkt) %>% 
    filter(table_name == tabell_punkt,
           column_name == namnkol_punkt) %>% 
    nrow > 0
  
  namnkol_punkt_sql <- if (namnkolumn_finns) paste0(', "', namnkol_punkt, '"') else ""
  
  if (!is.null(nodkolumn_punkt)) {
    nodkolumn_finns <- postgres_lista_kolumnnamn_i_schema(con = con, schema = schema_punkt) %>% 
      filter(table_name == tabell_punkt,
             column_name == nodkolumn_punkt) %>% 
      nrow > 0
  } else nodkolumn_finns <- FALSE
  
  if (!nodkolumn_finns) {
    DBI::dbExecute(con, glue::glue("
    ALTER TABLE {temptabell}
    ADD COLUMN IF NOT EXISTS toponode INTEGER;"))
    
    DBI::dbExecute(con, glue::glue("
    UPDATE {temptabell} p
    SET toponode = g.target
    FROM (
      SELECT p.{idkol_punkt},
             g.target
      FROM {temptabell} p
      JOIN LATERAL (
        SELECT target
        FROM {schema_graf}.{tabell_graf}
        ORDER BY p.geom <-> geom
        LIMIT 1
      ) g ON true
    ) AS g
    WHERE p.{idkol_punkt} = g.{idkol_punkt};
  "))
    nodkolumn_punkt <- "toponode"
  } 
  
  if (felsokning_meddelanden) {
    cat("🧪 Kontroll: Finns kolumnen", nodkolumn_punkt, "i", temptabell, "?\n")
    print(DBI::dbReadTable(con, DBI::Id(schema = "temp", table = "franpunkter")))
  }
  # Ta bort den temporära tabellen om den redan finns
  DBI::dbExecute(con, "DROP TABLE IF EXISTS noder_start")
  DBI::dbExecute(con, glue::glue("
    CREATE TEMP TABLE noder_start AS
    SELECT {idkol_punkt}{namnkol_punkt_sql}, {nodkolumn_punkt} AS node
    FROM {temptabell}
    WHERE {nodkolumn_punkt} IS NOT NULL;
  "))
  
  # stanna om inte noder_start skapas
  stopifnot("noder_start" %in% DBI::dbListTables(con))
  
  # ordna intervallerna
  from_vals <- c(0, head(intervall_varden, -1) + 1)
  to_vals   <- intervall_varden
  max_cost <- max(intervall_varden)
  
  if (tolower(kostnad_enhet) == "auto") {
    kostnad_enhet <- case_when(
      str_detect(kostnadskol_graf_f, "meter") ~ " meter",
      str_detect(kostnadskol_graf_f, "min") ~ " minuter",
      str_detect(kostnadskol_graf_f, "sek") ~ " sekunder",
      str_detect(kostnadskol_graf_f, "tim") ~ " timmar",
      TRUE ~ ""
    ) # slut case_when
  } else kostnad_enhet <- ""
  
  # Skapa CASE WHEN-uttryck som SQL-säker sträng
  case_when_uttryck <- paste0(
    "CASE\n",
    glue::glue_collapse(
      glue::glue("  WHEN d.agg_cost <= {to} THEN '{from}–{to}{kostnad_enhet}'",
                 from = from_vals, to = to_vals),
      sep = "\n"
    ),
    glue::glue("\n  ELSE '> {max(intervall_varden) }'\nEND AS kostnadsintervall")
  )
  
  # Läs in punkternas ID och nod
  noder <- DBI::dbReadTable(con, "noder_start")
  if (felsokning_meddelanden) {
    cat("Noder: \n")
    print(noder)
  } 
  
  sql_per_punkt <- purrr::map_chr(noder[[idkol_punkt]], function(pid) {
    node <- noder$node[noder[[idkol_punkt]] == pid]
    glue::glue("
    SELECT '{pid}' AS punkt_id, d.*, {case_when_uttryck}, e.*
    FROM pgr_drivingDistance(
      'SELECT {idkol_graf} AS id, source, target,
        CASE 
          WHEN {kostnadskol_graf_f} IS NOT NULL THEN {kostnadskol_graf_f} 
          ELSE -1 
          END AS cost,
          CASE 
          WHEN {kostnadskol_graf_b} IS NOT NULL THEN {kostnadskol_graf_b} 
          ELSE -1 
          END AS reverse_cost 
       FROM {schema_graf}.{tabell_graf}',
      {node}, {max_cost}
    ) d
    JOIN {schema_graf}.{tabell_graf} e ON d.edge = e.{idkol_graf}
  ")
  })
  
  # Kombinera till en fullständig SQL-sträng
  sql_union <- glue::glue_collapse(sql_per_punkt, sep = "\nUNION ALL\n")
  
  # Ta bort den temporära tabellen om den redan finns
  DBI::dbExecute(con, "DROP TABLE IF EXISTS isokron_edges")
  # Skapa temporär tabell med alla rutter
  DBI::dbExecute(con, glue::glue("
  CREATE TEMP TABLE isokron_edges AS
  {sql_union}
"))
  if (felsokning_meddelanden) {
    DBI::dbGetQuery(con, "SELECT COUNT(*) FROM isokron_edges")
    DBI::dbGetQuery(con, "SELECT punkt_id, kostnadsintervall, ST_AsText(geom) FROM isokron_edges LIMIT 5")
    cat(sql_union)
  } 
  
  # Ta bort den temporära tabellen om den redan finns
  DBI::dbExecute(con, "DROP TABLE IF EXISTS isokron_polygons")
  if (polygon_metod == "concave") {
    DBI::dbExecute(con, glue::glue("
      CREATE TEMP TABLE isokron_polygons AS
      SELECT punkt_id{namnkol_punkt_sql},
             e.kostnadsintervall,
             ST_ConcaveHull(ST_Collect(geom), 0.5) AS geom
      FROM isokron_edges AS e
      LEFT JOIN noder_start n ON e.punkt_id = n.{idkol_punkt}
      GROUP BY punkt_id{namnkol_punkt_sql}, e.kostnadsintervall;"))
  } else if (polygon_metod == "buffer") {
    DBI::dbExecute(con, glue::glue("
      CREATE TEMP TABLE isokron_polygons AS
      SELECT punkt_id{namnkol_punkt_sql},
             kostnadsintervall,
             ST_Buffer(ST_Union(ST_Simplify(geom, {simplify_tol})), {buffer_m}) AS geom
      FROM isokron_edges AS e
      LEFT JOIN noder_start n ON e.punkt_id = n.{idkol_punkt}
      GROUP BY punkt_id{namnkol_punkt_sql}, kostnadsintervall
    "))
    # gamla: ST_Simplify(ST_Buffer(ST_Union(geom), {buffer_m}), {simplify_tol}) AS geom
  } else if (polygon_metod == "convex") {
    DBI::dbExecute(con, glue::glue("
      CREATE TEMP TABLE isokron_polygons AS
      SELECT punkt_id{namnkol_punkt_sql},
             kostnadsintervall,
             ST_ConvexHull(ST_Collect(geom)) AS geom
      FROM isokron_edges AS e
      LEFT JOIN noder_start n ON e.punkt_id = n.{idkol_punkt}
      GROUP BY punkt_id{namnkol_punkt_sql}, kostnadsintervall;"))
  }
  
  # efterbearbetning när man kör med buffer-metoden
  # 1. Läs in polygonerna
  iso_polys <- st_read(con, query = "SELECT * FROM isokron_polygons") 
  iso_polys <- iso_polys %>% 
    mutate(kostnadsintervall = factor(kostnadsintervall, levels = iso_polys$kostnadsintervall %>%
                                        unique() %>%
                                        stringr::str_extract("^\\d+") %>%     # extrahera startvärdet
                                        as.numeric() %>%
                                        order() %>%
                                        {\(i) unique(iso_polys$kostnadsintervall)[i]}()
    )) %>%
    arrange(punkt_id, desc(kostnadsintervall))
  
  # för testning
  #dark_greens <- colorRampPalette(tail(RColorBrewer::brewer.pal(9, "Greens"), 5))(length(unique(iso_polys$kostnadsintervall)))
  #mapview(iso_polys, zcol = "kostnadsintervall", col.regions = rev(dark_greens))
  
  if (dela_upp_polygoner) {
    iso_polys <- postgis_isokroner_dela_upp_polygoner(
      skickad_sf = iso_polys,
      kategori_kol = "kostnadsintervall",
      isoid_kol = "punkt_id",
      geom_kol = "geom"
    )
  }
  
  if (!is.null(spara_schema) && !is.null(spara_tabell)) {
    sf::st_write(
      iso_polys,
      dsn = con,
      layer = DBI::Id(schema = spara_schema, table = spara_tabell),
      delete_layer = TRUE
    )
  }
  
  # Lägg till en kolumn 
  if (restyp != "meter") {
    geom_kol_namn <- attr(iso_polys, "sf_column")
    iso_polys <- iso_polys %>% 
      mutate(restyp = restyp) %>% 
      relocate(restyp, .before = all_of(geom_kol_namn))
  }
  
  # ta bort temptabell och tempschema om de finns
  if (postgres_schema_finns(con = con, schema_namn = "temp")) {
    suppressMessages(postgres_tabell_ta_bort(con = con, schema = "temp", tabell = "franpunkter"))
    suppressMessages(postgres_schema_ta_bort(con = con, schema = "temp"))
  }
  # återställ så att postgis skickar meddelanden igen
  if (!visa_meddelanden) DBI::dbExecute(con, "SET client_min_messages TO notice;")
  if(default_flagga) dbDisconnect(con)
  
  if (returnera_sf) {
    return(iso_polys)
  } else {
    return(invisible(NULL))
  }
} # slut funktion


postgis_isokroner_dela_upp_polygoner <- function(
    skickad_sf,            # sf-objekt med isokroner som ska delas upp så ingen överlappar
    kategori_kol,          # kolumn med kategorier för isokronerna
    isoid_kol,             # kolumn med id-nummer för isokroner utfall att det finns flera
    geom_kol               # kolumn som innehåller geometri
) {
  
  # Sortera intervallen i rätt ordning (viktigt!)
  nivaa_ordning <- sort(unique(skickad_sf[[kategori_kol]]))
  
  # Unionera per punkt_id och intervall (om inte redan gjort)
  iso_union <- skickad_sf %>%
    group_by(across(everything())) %>% 
    #group_by(across(all_of(c(isoid_kol, kategori_kol)))) %>%
    summarise(!!geom_kol := st_union(.data[[geom_kol]]), .groups = "drop")
  
  # Initiera resultat
  iso_utan_overlapp <- list()
  
  # Loop per nivå
  for (i in seq_along(nivaa_ordning)) {
    aktuell_niva <- nivaa_ordning[i]
    
    aktuella <- iso_union %>% 
      filter(.data[[kategori_kol]] == aktuell_niva)
    
    if (i == 1) {
      iso_utan_overlapp[[i]] <- aktuella
    } else {
      tidigare <- bind_rows(iso_utan_overlapp[1:(i - 1)])
      
      # Klipp bort överlappningar punkt för punkt
      klippt <- map_dfr(unique(aktuella[[isoid_kol]]), function(pid) {
        denna <- aktuella %>% 
          filter(.data[[isoid_kol]] == pid)
        tidigare_pid <- tidigare %>% 
          filter(.data[[isoid_kol]] == pid)
        
        if (nrow(tidigare_pid) == 0) return(denna)
        
        union_tidigare <- st_union(tidigare_pid[[geom_kol]])
        denna[[geom_kol]] <- st_difference(denna[[geom_kol]], union_tidigare)
        denna
      })
      
      iso_utan_overlapp[[i]] <- klippt
    }
  }
  
  # Slå ihop till ett sf-objekt
  iso_unik_sf <- bind_rows(iso_utan_overlapp)
  return(iso_unik_sf)
}

postgis_kopiera_punkttabell_koppla_till_pgr_graf <- function(
    con_fran_databas = uppkoppling_db(),                   # databas från vilket punktlagret kopieras, default är geodatabasen
    con_till_databas = uppkoppling_adm("ruttanalyser"),    # databas till vilket punktlagret kopieras, default är ruttanalyser
    schema_fran,                                        # schema i geodatabasen där adresserna finns
    tabell_fran,                                       # tabell i geodatabasen där adresserna finns
    schema_till,                                      # schema i ruttanalyser-databasen där adresserna ska kopieras till
    tabell_till,                                     # tabell i ruttanalyser-databasen där adresserna ska kopieras till
    geom_kol_punkter = "geom",                      # geometri-kolumnen i punkttabellen som ska kopieras, default är geom
    id_kol_punkter = "id",                         # id-kolumnen i punkttabellen, default är id
    schema_natverk = "grafer",                    # schema där grafen finns, ska normalt inte ändras
    lagg_till_metadata_i_till_databas = TRUE,    # punkttabellen läggs till i 
    stang_db_anslutningar = TRUE
) {
  
  # om man har en punkttabell i en databas som man vill kopiera till ruttanalyser och koppla till befintliga grafer
  # så kan man köra denna funktion
  
  # först kopierar vi tabellen från en databas till en annan
  postgis_kopiera_tabell_mellan_databaser(
    con_fran_databas = con_fran_databas,        # från geodatabasen
    con_till_databas = con_till_databas,        # till ruttanalyser-databasen
    schema_fran = schema_fran,                 # schema i geodatabasen där adresserna finns
    tabell_fran = tabell_fran,                # tabell i geodatabasen där adresserna finns
    schema_till = schema_till,                # schema i ruttanalyser-databasen där adresserna ska kopieras till
    tabell_till = tabell_till,                # tabell i ruttanalyser-databasen där adresserna ska kopieras till
    lagg_till_metadata_i_till_databas = lagg_till_metadata_i_till_databas
  )
  
  # vi kollar i ruttanalyser-db vilka grafer som finns
  natverkstyper <- postgres_lista_scheman_tabeller(con = con_rutt) %>%
    pluck(schema_natverk) %>% 
    .[!str_detect(., "vertices_pgr")]
  
  # därefter kopplar vi punkttabellen till samtliga grafer som finns i ruttanalyser i schemat grafer 
  walk(natverkstyper, ~ {
    pgrouting_punkttabell_koppla_till_pgr_graf(
      con = con_rutt,                        # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
      schema_punkter = schema_till,           # schema där punkterna finns, default är punktlager
      tabell_punkter = tabell_till,            # tabell med punkter som ska kopplas till grafen, default är adresser_dalarna
      geom_kol_punkter = geom_kol_punkter,      # geometri-kolumnen i punkttabellen, default är geom
      id_kol_punkter = id_kol_punkter,         # id-kolumnen i punkttabellen, default är gml_id
      schema_natverk = schema_natverk,        # schema där grafen finns, default är nvdb
      tabell_natverk = .x                    # tabell med grafen som ska användas, default är graf_nvdb_adresser_dalarna
    )
  })
  
  # stänger anslutningar (default)
  if (stang_db_anslutningar) {
    dbDisconnect(con_fran_databas)
    dbDisconnect(con_till_databas)
  }
  
} # slut funktion

postgis_databas_skriv_med_metadata <- function(
    con,
    inlas_sf,
    schema,
    tabell, 
    postgistabell_geo_kol = "geometry",
    postgistabell_id_kol = NA,            # default är att inte skicka med id-kolumn, om det görs så indexeras id-kolumnen
    postgis_addera_data = FALSE,          # om TRUE töms ej tabellen innan ny data skrivs
    skriv_over_tabell_om_finns = TRUE,    # skriver över tabell om den finns, behåller inte struktur (FALSE om man vill behålla struktur)
    felmeddelande_medskickat = NA,
    kommentar_metadata = NA
) {
  # en funktion som används vid uppdatering av data i geodatabasen och andra databaser. Funktionen använder
  # tryCatch, det finns möjlighet att skicka med ett felmeddelande som kan läggas in i metadata om felet uppstått
  # innan data ska sparas i databasen (tex vid nedladdning). Data sparas i databasen och så skrivs information
  # till tabellen metadata
  
  tryCatch({
    
    if (!is.na(felmeddelande_medskickat)) {
      # om hämtningen har gått fel så skickas felmeddelandet med och läggs in i metadata
      geo_db_resultat <<- felmeddelande_medskickat
      lyckad_uppdatering <<- FALSE
    } else {
      suppress_specific_warning(                                               # döljer specifika varningsmeddelande genom parametern warn_text
        postgis_sf_till_postgistabell(con = con,
                                      inlas_sf = inlas_sf,
                                      schema = schema,
                                      tabell = tabell,
                                      postgistabell_geo_kol = postgistabell_geo_kol,
                                      postgistabell_id_kol = postgistabell_id_kol,
                                      skapa_spatialt_index = ifelse(is.na(postgistabell_geo_kol), FALSE, TRUE),
                                      addera_data = postgis_addera_data,
                                      skriv_over_tabell_om_finns = skriv_over_tabell_om_finns
                                      ),
        warn_text = "Invalid time zone 'UTC', falling back to local time.")    # detta specifika varningsmeddelande skrivs inte ut i konsolen
      
      geo_db_resultat <<- if (!is.na(kommentar_metadata)) kommentar_metadata else NA
      lyckad_uppdatering <<- TRUE
    }
    
  }, error = function(e) {
    # Skriv ett felmeddelande om något går fel
    geo_db_resultat <<- glue("{e$message}")
    lyckad_uppdatering <<- FALSE
    
    if (geo_db_resultat == ""){
      message("Data kunde inte läggas till i databasen. Felmeddelande saknas.")
    } else {
      message(glue("Data kunde inte läggas till i databasen. Felmeddelande: {geo_db_resultat}"))
    }
    
  }, finally = {
    # kod som körs oavsett om skriptet fungerade att köra eller inte
    postgres_metadata_uppdatera(
      con = con,
      schema = schema,
      tabell = tabell,
      lyckad_uppdatering = lyckad_uppdatering,
      kommentar = geo_db_resultat
    )
  })
} # slut funktion


postgis_skapa_omradesnyckel_vy <- function(
    con,
    omrades_schema,
    omrades_tabell,
    omrade_urval_kolumner = NULL,
    rut_schema = "rutor",
    rut_tabell = "bo_syss_100m_alla_ar",
    rut_id_kol = "rutid",
    rut_urval_kolumner = NULL,
    utdata_schema = "omradesnycklar",
    utdata_vy = NULL,
    fran_rutid_till_x_y_kol = c("Ruta100swX" = 7, "Ruta100swY" = 6),                  # delar upp döper om rutorna med namn och sedan = längd på del av rutid som är x resp y
                                                                                      # tex så här: fran_rutid_till_x_y_kol = c("Ruta100swX" = 7, "Ruta100swY" = 6), där x är 7 position 1-7 av rutid, och y är position 8-13 av rutid
                                                                                      #, om NULL så behålls rutid-kolumnen och inga x- eller y-kolumner skapas
    materialiserad = TRUE,                # TRUE för materialiserad vy, FALSE för vanlig vy (om data inte är alltför stor kan vanlig vy vara snabbare)
    skriv_over_befintlig_vy = TRUE        # TRUE så tas befintlig vy/materialiserad vy över och så skrivs den nya 
) {
  
  # Generera vynamn om inget anges
  if (is.null(utdata_vy)) {
    utdata_vy <- omrades_tabell
  }
  
  # Kontrollera att rutlagret finns
  if (!DBI::dbExistsTable(con, DBI::Id(schema = rut_schema, table = rut_tabell))) {
    stop("Rutlagret '", rut_schema, ".", rut_tabell, "' hittades inte i databasen.")
  }
  
  # Kontrollera att områdeslagret finns
  if (!DBI::dbExistsTable(con, DBI::Id(schema = omrades_schema, table = omrades_tabell))) {
    stop("Områdeslagret '", omrades_schema, ".", omrades_tabell, "' hittades inte i databasen.")
  }
  
  # Välj vytyp
  vy_typ <- if (materialiserad) "MATERIALIZED VIEW" else "VIEW"
  or_replace <- if (materialiserad) "" else "OR REPLACE "
  
  # kontrollera att utdata_schema finns, annars skapa det
  postgres_schema_skapa_om_inte_finns(con = con, schema_namn = utdata_schema)
  
  # Hämta geometrikolumnens namn för rutlagret
  rut_geom_kol <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT f_geometry_column FROM geometry_columns 
     WHERE f_table_schema = {rut_schema} AND f_table_name = {rut_tabell}",
    .con = con
  ))$f_geometry_column
  
  # Hämta kolumnnamn från rutlagret (exkl. geometri)
  rut_kolumner <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT column_name 
     FROM information_schema.columns 
     WHERE table_schema = {rut_schema}
       AND table_name = {rut_tabell}
       AND column_name NOT IN (
         SELECT f_geometry_column 
         FROM geometry_columns 
         WHERE f_table_schema = {rut_schema}
           AND f_table_name = {rut_tabell}
       )",
    .con = con
  ))$column_name
  
  if (!is.null(rut_urval_kolumner)) rut_kolumner <- rut_kolumner[rut_kolumner %in% rut_urval_kolumner]
  if (length(rut_kolumner) == 0) stop("Inga giltiga kolumner valda för rutlagret.")
  
  # Hämta geometrikolumnens namn för områdeslagret
  omrades_geom_kol <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT f_geometry_column FROM geometry_columns 
     WHERE f_table_schema = {omrades_schema} AND f_table_name = {omrades_tabell}",
    .con = con
  ))$f_geometry_column
  
  # Hämta kolumnnamn från områdesindelningen (exkl. geometri)
  omrades_kolumner <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT column_name 
     FROM information_schema.columns 
     WHERE table_schema = {omrades_schema}
       AND table_name = {omrades_tabell}
       AND column_name != {omrades_geom_kol}",
    .con = con
  ))$column_name
  
  if (!all(is.null(omrade_urval_kolumner))) omrades_kolumner <- omrades_kolumner[omrades_kolumner %in% omrade_urval_kolumner]
  if (length(omrades_kolumner) == 0) stop("Inga giltiga kolumner valda för områdesindelningen.")
  
  # Bygg SELECT-listor med tabellalias
  rut_select <- paste(paste0("r.", rut_kolumner), collapse = ",\n      ")
  omrades_select <- paste(paste0("o.", omrades_kolumner), collapse = ",\n      ")
  
  # Validera fran_rutid_till_x_y_kol
  if (!is.null(fran_rutid_till_x_y_kol)) {
    if (length(fran_rutid_till_x_y_kol) != 2) {
      message("fran_rutid_till_x_y_kol måste innehålla exakt 2 element. Körs utan uppdelning av rutid.")
      fran_rutid_till_x_y_kol <- NULL
    } else if (is.null(names(fran_rutid_till_x_y_kol)) || any(names(fran_rutid_till_x_y_kol) == "")) {
      message("fran_rutid_till_x_y_kol måste ha namngivna element som kolumnnamn. Körs utan uppdelning av rutid.")
      fran_rutid_till_x_y_kol <- NULL
    } else if (!is.numeric(fran_rutid_till_x_y_kol)) {
      message("Värdena i fran_rutid_till_x_y_kol måste vara numeriska (antal tecken). Körs utan uppdelning av rutid.")
      fran_rutid_till_x_y_kol <- NULL
    }
  }
  
  # Bygg eventuell uppdelning av rutid i x- och y-kolumner baserat på positioner i rutid
  rutid_split <- if (!is.null(fran_rutid_till_x_y_kol)) {
    kol_namn <- names(fran_rutid_till_x_y_kol)
    kol_langd <- as.numeric(fran_rutid_till_x_y_kol)
    start_x <- 1
    start_y <- kol_langd[1] + 1
    
    glue::glue(
      ",\n      substring(r.{rut_id_kol}, {start_x}, {kol_langd[1]})::numeric AS {kol_namn[1]},
      substring(r.{rut_id_kol}, {start_y}, {kol_langd[2]})::numeric AS {kol_namn[2]}"
    )
  } else {
    ""
  }
  
  # Skapa vy med DISTINCT ON + störst överlappning via ST_Area(ST_Intersection)
  vy_sql <- glue::glue(
    'CREATE {or_replace}{vy_typ} {utdata_schema}."{utdata_vy}" AS
    SELECT DISTINCT ON (r.{rut_id_kol})
      {rut_select},
      {omrades_select}{rutid_split}
    FROM {rut_schema}."{rut_tabell}" r
    JOIN {omrades_schema}."{omrades_tabell}" o
      ON ST_Intersects(r.{rut_geom_kol}, o.{omrades_geom_kol})
    ORDER BY
      r.{rut_id_kol},
      ST_Area(ST_Intersection(r.{rut_geom_kol}, o.{omrades_geom_kol})) DESC;'
  )
  
  # Droppa befintlig materialiserad vy om den finns
  if (materialiserad) {
    mat_vy_finns_redan <- nrow(DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT 1 FROM pg_matviews 
       WHERE schemaname = {utdata_schema} AND matviewname = {utdata_vy}",
      .con = con
    ))) > 0
    
    if (mat_vy_finns_redan & skriv_over_befintlig_vy) {
      message("Tar bort befintlig materialiserad vy: ", utdata_schema, ".", utdata_vy)
      DBI::dbExecute(con, glue::glue('DROP MATERIALIZED VIEW {utdata_schema}."{utdata_vy}";'))
    }
  }
  
  vy_typ <- if (materialiserad) "materialiserad vy" else "vy"
  
  message(glue("Skapar {vy_typ}: {utdata_schema}.{utdata_vy}"))
  DBI::dbExecute(con, vy_sql)
  message(glue("Klar! {str_to_sentence(vy_typ)} skapad: {utdata_schema}.{utdata_vy}"))
  
  invisible(utdata_vy)
}


postgis_omradesnyckel_exportera_till_mikrodb <- function(
    con,
    omradesnyckel_schema = "omradesnycklar",
    omradesnyckel_tabell,
    max_chunk_stlk_mb = 10,
    filnamn = NULL,
    output_mapp = NULL
) {
  
  if (is.null(output_mapp)) output_mapp <- utskriftsmapp()
  if (is.null(filnamn)) filnamn <- omradesnyckel_tabell
  
  # Hämta data från databasen
  message("Hämtar data från ", omradesnyckel_schema, ".", omradesnyckel_tabell, "...")
  alla_rutor_export <- DBI::dbGetQuery(con, glue::glue(
    "SELECT * FROM {omradesnyckel_schema}.{omradesnyckel_tabell}",
    .con = con
  ))
  message("Mäter storlek på hela datasetet...")
  df_namn <- if (is.null(filnamn))  omradesnyckel_tabell else filnamn
  df_list <- list(alla_rutor_export)
  names(df_list) <- df_namn
  
  # Skriv hela datasetet till en temporär fil för att se storleken totalt
  tmp <- "tempfil.zip"
  spara_som_csv_i_zip(df_list = df_list,
                      zipfilnamn = tmp,
                      output_mapp = output_mapp,
                      meddelande = FALSE)
  total_filstorlek <- file.size(paste0(output_mapp, tmp))
  unlink(paste0(output_mapp, tmp))
  
  totalt_rader    <- nrow(alla_rutor_export)
  bytes_per_rad   <- total_filstorlek / totalt_rader
  rader_per_chunk <- floor((max_chunk_stlk_mb * 1e6) / bytes_per_rad)
  antal_chunks    <- ceiling(totalt_rader / rader_per_chunk)
  
  # meddela användaren om hur stort datasetet är och om det delas upp i fler delar eller om det ryms i en zipfil
  if (antal_chunks == 1) {
    message("Datasetet är ", format(total_filstorlek / 1e6, digits = 2), " MB och kommer att sparas i en fil.")
  } else {
    message("Datasetet är ", format(total_filstorlek / 1e6, digits = 2), " MB och kommer att delas upp i ", antal_chunks, " delar.")
  }
  
  # Om det är för många rader så delas dataset upp i flera delar
  if (antal_chunks < 2) {
    spara_som_csv_i_zip(df_list = df_list,
                        zipfilnamn = df_namn,
                        output_mapp = output_mapp)
  } else {
    df_lista <- alla_rutor_export %>%
      mutate(chunk = ceiling(row_number() / rader_per_chunk)) %>%
      group_split(chunk) %>%
      setNames(paste0(df_namn, "_", str_pad(seq_along(.), 2, pad = "0"))) %>%
      map(~ select(.x, -chunk))
    
    iwalk(df_lista, ~ {
      spara_som_csv_i_zip(df_list = .x,
                          zipfilnamn = .y,
                          output_mapp = output_mapp)
    })
  }
  
  fil_txt <- if (antal_chunks == 1) "fil" else "filer"
  message("Klar! ", antal_chunks, " ", fil_txt, " sparade i ", output_mapp)
}

# ======================================= pgrouting-funktioner ================================================

# funktion för att installera pgrouting i en postgisdatabas

pgrouting_installera_i_postgis_db <- function(con) {
  
  # SQL-fråga för att installera pgrouting
  installera_pgrouting_query <- "CREATE EXTENSION IF NOT EXISTS pgrouting;"
  
  # Kör SQL-frågan och hantera fel
  tryCatch({
    dbExecute(con, installera_pgrouting_query)
    message("pgrouting-tillägget har installerats i databasen.")
  }, error = function(e) {
    message("Kunde inte installera pgrouting-tillägget:", e$message)
  })
  
}

# ------------- Funktioner för att göra rutt analyser
# Funktionerna som finns nedan är ett paket av funktioner som skapar olika typer av rutt-
# analyser.Funktionerna är byggda av vår tidigare GIS-praktikant Mikael Leonidsson år 2024 och har därefter utvecklats vidare av 
# Sebastian Roos Tanse våren 2025 då han var GIS-praktikant på RUF-Samhällsanalys.
# Funktionerna som är från början skrivna för ett specifikt projekt men har efter hand skrivits om 
# för att kunna användas till allehanda olika analyser. I juli 2025 lade Peter Möller en hand över skripten
# för att åtgärda vissa fel och en viss optimering av befintliga skript.


# ----------------- 1. Funktion för att klippa ett nätverk (standard är nvdb) utanför en gräns med 
# buffer_m antal meter. T.ex. att klippa nvdb 3 mil utanför länsgränsen för att få med 
# rutter som går en bit över länsgränsen. Därefter platta till nätverket (Force2D) 
# och göra om Multi till Linestring (för att resten ska funka). Egentligen ingen pgrouting-funktion
# men förberedelse inför att skapa en graf
pgrouting_klipp_natverk_skapa_tabell <- function(
    con,                                   # Uppkoppling, finns ingen default för det behövs en användare med skrivbehörighet för att kunna köra denna funktion
    con_till_databas = "ruttanalyser",     # NULL om man vill lägga resultatet (klippt nätverk, oftast nvdb) i samma databas, annars gör man en uppkoppling till ny databas genom att skicka med ett con-objekt alternativt namnet på den databas som man vill koppla upp sig mot (båda funkar)
    buffer_m = 30000,                      # Om man vill klippa nätverket utanför en gräns (tex. länsgräns), ange hur många meter
    natverk_schema = "nvdb",               # schema i vilket nätverket finns, default är nvdb
    natverk_tabell = "dala_med_grannlan",  # tabell som används att klippa i, default är dala_med_grannlan
    #natverk_id,                           # verkar inte behövas 
    natverk_geokol = "geom",               # geometry-kolumnen i nätverkstabellen, default är geom
    region_schema = "karta",               # schema för region att klippa nätverk med, default är karta
    region_tabell = "lan_lm",              # tabell för region att klippa nätverk med, default är lan_lm, som är länsgränser som är korrekta men inte snygga (som scb:s läns- och kommungränser)
    regionkod_kol = "lankod",              # regionkoderna ligger i denna kolumn, tex. länskoder eller kommunkoder
    regionkoder = "20",                    # man kan ha en eller skicka med en vektor med regionkoder, t.ex. c("20", "21")
    region_geokol = "geom",                # geometry-kolumnen i region-tabellen, default är geom
    output_schema = "grafer",                # schema att spara output-tabellen i, default är nvdb
    output_tabell = "nvdb_alla",           # tabell att spara output i, default är nvdb_alla     (alla för bil + gång och cykelväg, annars tex bil), detta lager är klippt för att koppla bättre mot adresspunkterna
    urval_fran_natverk = ""                # om man vill välja något specifikt från nätverkstabellen, ska då vara i formatet "WHERE vagtrafiknat_nattyp = 1"
) {
  # Starta tidtagning
  starttid <- Sys.time()
  
  # om man skickat med ett con_till_databas (dvs. ska skriva resultatet till annan databas)
  if (!is.null(con_till_databas)) {
    # testar om det är ett con-objekt, i så fall gör vi ingenting utan utgår från att det är korrekt
    if (!inherits(con_till_databas, "DBIConnection")){
      # om det inte är ett con-objekt så behöver vi skapa det men då krävs keyring-paketet och en service som heter rd_geodata
      if (!requireNamespace("keyring", quietly = TRUE)) {
        stop("Funktionen kräver paketet 'keyring' för att hantera databasuppkopplingar. Installera det med install.packages('keyring').")
      }
      # om man har paketet keyring måste också servicen "rd_geodata" finnas, som ska innehålla inloggningsuppgifter till databasen
      if (!"rd_geodata" %in%  keyring::key_list()$service) {
        stop("Keyring-service 'rd_geodata' saknas. Skapa den och lägg in användare och lösenord till databasen där.")
      }
      
      con_till_databas <- uppkoppling_adm(con_till_databas)
      skapad_i_funktionen <- TRUE
      
    } else skapad_i_funktionen <- FALSE # slut test om att det inte är ett con-objekt, om det är ett con-objekt görs inget mer utan vi använder det bara
  } else {
    con_till_databas <- con                  # om con_till_databas är NA så antar det samma värde som con (jobba i samma databas)
    skapad_i_funktionen <- FALSE
  } 
  
  # ====== kontrollera att alla scheman, tabeller och kolumner finns
  postgres_finns_schema_tabell_kolumner(con, natverk_schema, natverk_tabell, natverk_geokol, stoppa_vid_fel = TRUE)     # kontrollera att nätverket finns
  postgres_finns_schema_tabell_kolumner(con, region_schema, region_tabell, c(regionkod_kol, region_geokol), stoppa_vid_fel = TRUE)
  
  # Formatera regionkoder till SQL-sträng
  regionkoder_str <- paste0("'", paste(regionkoder, collapse = "', '"), "'")
  
  tryCatch({
    # Skapa spatialt index på region-tabellen om det inte finns 
    sql_index <- glue("CREATE INDEX IF NOT EXISTS {region_tabell}_geom_idx 
                       ON {region_schema}.{region_tabell} USING GIST({region_geokol});")
    dbExecute(con, sql_index)
    
    # Klipp nätverket med ST_Intersects (behåll hela linjer)
    dbExecute(con_till_databas, glue("DROP TABLE IF EXISTS {output_schema}.{output_tabell};"))
    sql_clip_network <- glue("
      WITH region_buffer AS (
        SELECT ST_Union(ST_Buffer({region_geokol}, {buffer_m})) AS {region_geokol}
        FROM {region_schema}.{region_tabell}
        WHERE {regionkod_kol} IN ({regionkoder_str})
      )
      SELECT l.*
      FROM {natverk_schema}.{natverk_tabell} l
      JOIN region_buffer d
      ON ST_Intersects(l.{natverk_geokol}, d.{region_geokol}){urval_fran_natverk} ;
    ")
    klippt_natverk <- dbGetQuery(con, sql_clip_network)           # kör queryn och spara i r-objektet
    
    if (nrow(klippt_natverk) == 0) {
      stop("Inga rader hittades i det klippta nätverket. Kontrollera regionkoder och nätverkstabell.")
    }
    # skriv det klippta nätverket till den nya databasen
    st_write(obj = klippt_natverk,
             dsn = con_till_databas,
             layer = DBI::Id(schema = output_schema, table = output_tabell),
             append = FALSE)  # skriv över tabellen om den finns, annars skapa ny
    print(glue("✅ Nätverket klippt!"))
    
    
    # Konvertera till 2D
    # Denna raden är utbytt för att det skapar problem för att vi har defingerat att det ska vara en multilinestring 
    # och för att uppdatara detta måste vi göra den uppdaterade skriptet istället. 
    # sql_2d <- glue("UPDATE {output_schema}.{output_tabell} SET {natverk_geokol} = ST_LineMerge(ST_Force2D({natverk_geokol}));")
    
    sql_2d <- glue("ALTER TABLE {output_schema}.{output_tabell} ALTER COLUMN geom TYPE geometry(LineString, 3006) USING ST_LineMerge(ST_Force2D({natverk_geokol}));")
    
    dbExecute(con_till_databas, sql_2d)
    print("Geometrin utplattad (2d) och MultiLinestrings omgjorda till linestrings.")
    
    # Skapa index på geometrin
    index_namn <- glue("{output_schema}_{output_tabell}_geom_idx")
    dbExecute(con_till_databas, glue("DROP INDEX IF EXISTS {index_namn};"))
    dbExecute(con_till_databas, glue("CREATE INDEX {index_namn} ON {output_schema}.{output_tabell} USING GIST({natverk_geokol});"))
    dbExecute(con_till_databas, glue("ANALYZE {output_schema}.{output_tabell};"))
    
    # hämta nvdb-version i metadata-tabell
    meta_nvdb <- postgres_meta(
      con = con,
      query = "WHERE schema = 'nvdb' AND tabell = 'dala_med_grannlan'") %>% 
      select(version_datum, version_tid)
  

    dag <- as.integer(format(with_tz(meta_nvdb$version_datum, tzone = Sys.timezone()), "%d"))
    manad_ar <- tolower(format(with_tz(meta_nvdb$version_datum, tzone = Sys.timezone()), "%b%Y"))
    tid <- paste0(str_sub(meta_nvdb$version_tid, 1, 2), str_sub(meta_nvdb$version_tid, 4, 5))
    nvdb_ver <- paste0(dag, manad_ar, "_", tid)
    
    nvdb_ver_db <- paste0("nvdb ver: ", nvdb_ver)
    lyckad_uppdatering <- TRUE
    
  }, error = function(e) {
    # Skriv ett felmeddelande om något går fel
    nvdb_ver_db <- glue("{e$message}")
    lyckad_uppdatering <- FALSE
    
  }, finally = {
    # kod som körs oavsett om skriptet fungerade att köra eller inte
    postgres_metadata_uppdatera(
      con = con_till_databas,
      schema = output_schema,
      tabell = output_tabell,
      version_datum = meta_nvdb$version_datum,
      version_tid = meta_nvdb$version_tid,
      lyckad_uppdatering = lyckad_uppdatering,
      kommentar = nvdb_ver_db
    )
  })
  
  # Stäng anslutningen om den var temporär
  if (skapad_i_funktionen) dbDisconnect(con_till_databas)
  
  # Tidtagning
  sluttid <- Sys.time()
  message(glue("Resultatet har sparats i {output_schema}.{output_tabell}.\n⏱ Processen tog {round(sluttid - starttid, 2)} minuter att köra"))
}

# ---------- 2. Funktion för att utöka nätverk med nya noder
# denna funktion används för att skapa bättre anslutningar från ett punktlager till ett nätverk (oftast nvdb). I default kör
# vi på adresser som är det vi kommer att använda mest men vill man köra med andra punkter så kan man göra det.
# som det är nu måste punkter flyttas över till databasen "ruttanalyser" där allt körs för att skapa grafer
# då kopplas i en senare funktion också nid till varje punkt så att det går att göra körningar med pgrouting utifrån
# dessa punkter

pgrouting_hitta_narmaste_punkt_pa_natverk <- function(
    con = "ruttanalyser",                           # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
    schema_punkter_fran = "punktlager",             # att köra med adresser är default, men det går att köra med andra tabeller, schema och från annan databas
    tabell_punkter_fran = "adresser",       # tabellen som används, default är "dalarna" som är alla adresser i Dalarna
    geometri_kol_punkter_fran = "geom",             # geometri-kolumnen i punkttabellen från. 
    id_kol_punkter_fran = "gml_id",                 # id-kolumnen i punkttabellen från, default är "gml_id"
    schema_graf = "grafer",                           # schema för grafen, dvs. nätverket som ska användas, default är nvdb
    tabell_graf = "nvdb_alla",                      # tabell för grafen, dvs. nätverket som ska användas, default är graf_nvdb
    geometri_graf = "geom",                         # geometri-kolumnen i grafen, dvs. nätverket som ska användas, default är geom
    id_graf = "rad_id",                             # id-kolumnen i grafen, dvs. nätverket som ska användas, default är rad_id
    kostnadsfalt = "",                              # lite oklart i nuläget vilket syfte detta har, behövs sannolikt inte
    tolerans_avstand = 3                            # för att bygga kluster av punkter och spara körtid, 0 = då skapar vi i praktiken inga kluster
) {
  starttid <- Sys.time()
  
  # om man skickat med ett con (dvs. ska skriva resultatet till annan databas)
  if (!is.null(con)) {
    # testar om det är ett con-objekt, i så fall gör vi ingenting utan utgår från att det är korrekt
    if (!inherits(con, "DBIConnection")){
      # om det inte är ett con-objekt så behöver vi skapa det men då krävs keyring-paketet och en service som heter rd_geodata
      if (!requireNamespace("keyring", quietly = TRUE)) {
        stop("Funktionen kräver paketet 'keyring' för att hantera databasuppkopplingar. Installera det med install.packages('keyring').")
      }
      # om man har paketet keyring måste också servicen "rd_geodata" finnas, som ska innehålla inloggningsuppgifter till databasen
      if (!"rd_geodata" %in%  keyring::key_list()$service) {
        stop("Keyring-service 'rd_geodata' saknas. Skapa den och lägg in användare och lösenord till databasen där.")
      }
      con <- uppkoppling_adm(con)
      skapad_i_funktionen <- TRUE
    } else skapad_i_funktionen <- FALSE # slut test om att det inte är ett con-objekt, om det är ett con-objekt görs inget mer utan vi använder det bara
  } else {
    stop("En anslutning till databasen måste skickas med, som con-objekt eller som namn på databasen man vill koppla upp mot.")
  } 
  
  tabell_ny_natverk <- glue("{tabell_graf}_{tabell_punkter_fran}")
  
  # ge dem värden redan här utfall att fel uppstår innan de får värden i tryCatch()
  ver_datum <- NA
  ver_tid <- NA
  frantabell_ver_db <- NA
  lyckad_uppdatering <- FALSE
  
  # här börjar vi körningen, vi kör med dbBegin() för att kunna rulla 
  dbBegin(con)
  tryCatch({
    # 1. Närmaste punkt på nätverket
    # Ta bort eventuell gammal tabell
    dbExecute(con, glue("DROP TABLE IF EXISTS {schema_graf}.{tabell_ny_natverk}_narmaste_punkt;"))
    sql_narmaste <- glue("
      CREATE TABLE {schema_graf}.{tabell_ny_natverk}_narmaste_punkt AS
      WITH closest_points AS (
        SELECT
          f.{id_kol_punkter_fran} AS adress_id,
          g.{id_graf} AS graf_id,
          ST_ClosestPoint(g.{geometri_graf}, f.{geometri_kol_punkter_fran}) AS punkt_pa_natverk,
          ST_DWithin(ST_ClosestPoint(g.{geometri_graf}, f.{geometri_kol_punkter_fran}), ST_StartPoint(g.{geometri_graf}), {tolerans_avstand}) AS is_near_start_point,
          ST_DWithin(ST_ClosestPoint(g.{geometri_graf}, f.{geometri_kol_punkter_fran}), ST_EndPoint(g.{geometri_graf}), {tolerans_avstand}) AS is_near_end_point
        FROM {schema_punkter_fran}.{tabell_punkter_fran} f,
        LATERAL (
          SELECT {id_graf}, {geometri_graf}
          FROM {schema_graf}.{tabell_graf}
          ORDER BY f.{geometri_kol_punkter_fran} <-> {geometri_graf}
          LIMIT 1
        ) g
      )
      SELECT adress_id, graf_id, punkt_pa_natverk
      FROM closest_points
      WHERE NOT is_near_start_point AND NOT is_near_end_point;
    ")
    
    dbExecute(con, sql_narmaste)
    
    n_narmaste <- dbGetQuery(con, glue("SELECT COUNT(*) FROM {schema_graf}.{tabell_ny_natverk}_narmaste_punkt"))$count
    print(glue("✅ Antal nya närmaste punkter: {n_narmaste}"))
    
    if (n_narmaste == 0) stop("🚫 Inga giltiga punkter hittades på nätverket.")
    
    # 2. Skapa kluster av närmaste punkter
    dbExecute(con, glue("DROP TABLE IF EXISTS {schema_graf}.{tabell_ny_natverk}_klusterpunkt;"))
    sql_kluster <- glue("
      CREATE TABLE {schema_graf}.{tabell_ny_natverk}_klusterpunkt AS
      WITH clusters AS (
        SELECT
          ST_ClusterDBSCAN(punkt_pa_natverk, eps := {tolerans_avstand}, minpoints := 1) OVER(PARTITION BY graf_id) AS cid,
          punkt_pa_natverk,
          graf_id
        FROM {schema_graf}.{tabell_ny_natverk}_narmaste_punkt
      ), cluster_centroids AS (
        SELECT
          cid, graf_id,
          ST_Centroid(ST_Collect(punkt_pa_natverk)) AS centroid
        FROM clusters
        GROUP BY cid, graf_id
      ), representant_punkter AS (
        SELECT DISTINCT ON (c.cid, c.graf_id)
          c.cid,
          c.graf_id,
          c.punkt_pa_natverk AS representant_punkt,
          ST_LineLocatePoint(n.{geometri_graf}, c.punkt_pa_natverk) AS line_location
        FROM clusters c
        JOIN {schema_graf}.{tabell_graf} n ON c.graf_id = n.{id_graf}
        JOIN cluster_centroids cc ON c.cid = cc.cid AND c.graf_id = cc.graf_id
        ORDER BY c.cid, c.graf_id, ST_Distance(c.punkt_pa_natverk, cc.centroid)
      )
      SELECT
        cid,
        graf_id,
        representant_punkt,
        line_location
      FROM representant_punkter
      ORDER BY graf_id, line_location;
    ")
    
    dbExecute(con, sql_kluster)
    n_kluster <- dbGetQuery(con, glue("SELECT COUNT(*) FROM {schema_graf}.{tabell_ny_natverk}_klusterpunkt"))$count
    print(glue("✅ Antal klusterrepresentanter: {n_kluster}"))
    
    if (n_kluster == 0) stop("🚫 Klustring misslyckades – inga punkter valda som representanter.")
    
    # 3. Skapa nytt nätverk
    dbExecute(con, glue("DROP TABLE IF EXISTS {schema_graf}.{tabell_ny_natverk};"))
    print(glue("Tabell {tabell_ny_natverk} droppades om den fanns."))
    
    
    # Plocka ut alla kolumner ur grafen utom geometrin och shape_length, stoppa i en sträng för att använda i SQL:n nedan.
    # OBS! De tre första kolumnnamnen är obligatoriska, resterande (i det här fallet from_ och to_measure) kan fyllas på med fler eller tas bort beroende på hur mycket info man vill ha med i grafen 
    col_query <- glue("
      SELECT column_name
      FROM information_schema.columns
      WHERE table_schema = '{schema_graf}'
        AND table_name = '{tabell_graf}'
        AND column_name NOT IN ('{geometri_graf}', 'shape_length', 'rad_id', 'from_measure', 'to_measure');
    ")
    
    cols <- dbGetQuery(con, col_query)$column_name
    col_string <- paste0('n.\"', cols, '\"', collapse = ", ")
    
    sql_create <- glue("
      CREATE TABLE {schema_graf}.{tabell_ny_natverk} AS 
      WITH locus AS (
        SELECT {id_graf} AS gid, 0 AS l FROM {schema_graf}.{tabell_graf}
        UNION ALL
        SELECT {id_graf} AS gid, 1 AS l FROM {schema_graf}.{tabell_graf}
        UNION ALL
        SELECT graf_id AS gid, line_location AS l FROM {schema_graf}.{tabell_ny_natverk}_klusterpunkt
      ),
      loc_with_idx AS (
        SELECT gid, l, RANK() OVER (PARTITION BY gid ORDER BY l) AS idx
        FROM locus
      ),
      segment AS (
        SELECT
          ROW_NUMBER() OVER (ORDER BY loc1.gid, loc1.idx) AS {id_graf},
          {col_string},
          ST_LineSubstring(n.{geometri_graf}, loc1.l, loc2.l) AS {geometri_graf},
          ST_Length(ST_LineSubstring(n.{geometri_graf}, loc1.l, loc2.l)) AS kostnad_meter
        FROM 
          loc_with_idx loc1
          JOIN loc_with_idx loc2 USING (gid)
          JOIN {schema_graf}.{tabell_graf} n ON loc1.gid = n.{id_graf}
        WHERE loc2.idx = loc1.idx + 1
      )
      SELECT
        * {kostnadsfalt}
      FROM segment;
    ")
    
    
    dbExecute(con, sql_create)
    # Skapa index på geometrin
    index_namn <- glue("{schema_graf}_{tabell_ny_natverk}_geom_idx")
    dbExecute(con, glue("DROP INDEX IF EXISTS {index_namn};"))
    dbExecute(con, glue("CREATE INDEX {index_namn} ON {schema_graf}.{tabell_ny_natverk} USING GIST({geometri_graf});"))
    dbExecute(con, glue("ANALYZE {schema_graf}.{tabell_ny_natverk};"))
    #dbExecute(con, glue("CREATE INDEX ON {schema_graf}.{tabell_ny_natverk} USING GIST({geometri_graf});"))
    print(glue("✅ Nytt nätverk {tabell_ny_natverk} skapat."))
    
    dbCommit(con)
    
    # fyll på variabler som ska vara till metadatabasen 
    meta_graf <- postgres_meta(
      con = con,
      query = glue("WHERE schema = '{schema_graf}' AND tabell = '{tabell_graf}'")) %>% 
      select(version_datum, version_tid, kommentar)
    
    frantabell_ver_db <- meta_graf$kommentar
    lyckad_uppdatering <- TRUE
    
  }, error = function(e) {
    dbRollback(con)
    cat("🚨 Fel i processen:", e$message, "\n")
    
    frantabell_ver_db <- glue("{e$message}")
    lyckad_uppdatering <- FALSE
    
  }, finally = {
    
    # huvudtabellen, tex. nvdb_bil_adresser
    postgres_metadata_uppdatera(
      con = con,
      schema = schema_graf,
      tabell = tabell_ny_natverk,
      version_datum = meta_graf$version_datum,
      version_tid = meta_graf$version_tid,
      lyckad_uppdatering = lyckad_uppdatering,
      kommentar = frantabell_ver_db
    )
    
    # narmaste_punkt-tabellen
    postgres_metadata_uppdatera(
      con = con,
      schema = schema_graf,
      tabell = paste0(tabell_ny_natverk, "_narmaste_punkt"),
      version_datum = meta_graf$version_datum,
      version_tid = meta_graf$version_tid,
      lyckad_uppdatering = lyckad_uppdatering,
      kommentar = frantabell_ver_db
    )
    
    # klusterpunkt-tabellen
    postgres_metadata_uppdatera(
      con = con,
      schema = schema_graf,
      tabell = paste0(tabell_ny_natverk, "_klusterpunkt"),
      version_datum = meta_graf$version_datum,
      version_tid = meta_graf$version_tid,
      lyckad_uppdatering = lyckad_uppdatering,
      kommentar = frantabell_ver_db
    )
    
    # Stäng anslutningen om den var temporär
    if (skapad_i_funktionen) dbDisconnect(con)
    
    sluttid <- Sys.time()
    message(sprintf("⏱ Total tid: %.1f sekunder", as.numeric(difftime(sluttid, starttid, units = "secs"))))
  })
}


# ====== 3. Funktion för att skapa graf av tabell med linjer, ex.vis nvdb
# Här skapar vi själva grafen från en tabell med linjer (ex.vis nvdb) som vi har förberett i funktionerna ovan.
# Det är inte ett krav att funktionerna ovan körs men det kommer att ge en bättre graf

pgrouting_tabell_till_pgrgraf <- function(
    con = "ruttanalyser",                           # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
    schema_graf = "grafer",                           # schema där tabellen som ska bli graf ligger. Default: nvdb
    tabell_graf = "nvdb_alla_adresser",     # Tabellen som ska förberedas för pgrouting. Default: graf_nvdb_adresser_dalarna
    id_kol_graf = "rad_id",                         # Kolumnen med id i den tabell som ska bli graf
    geom_kol_graf = "geom",                     # Namn på kolumnen som innehåller geometrin i tabellen som ska bli graf
    tolerans = 0.001           # Toleransvärdet för hur nära segment måste vara för att ansluta. 
){
  # Starta tidstagning
  starttid <- Sys.time()
  
  # om man skickat med ett con (dvs. ska skriva resultatet till annan databas)
  if (!is.null(con)) {
    # testar om det är ett con-objekt, i så fall gör vi ingenting utan utgår från att det är korrekt
    if (!inherits(con, "DBIConnection")){
      # om det inte är ett con-objekt så behöver vi skapa det men då krävs keyring-paketet och en service som heter rd_geodata
      if (!requireNamespace("keyring", quietly = TRUE)) {
        stop("Funktionen kräver paketet 'keyring' för att hantera databasuppkopplingar. Installera det med install.packages('keyring').")
      }
      # om man har paketet keyring måste också servicen "rd_geodata" finnas, som ska innehålla inloggningsuppgifter till databasen
      if (!"rd_geodata" %in%  keyring::key_list()$service) {
        stop("Keyring-service 'rd_geodata' saknas. Skapa den och lägg in användare och lösenord till databasen där.")
      }
      con <- uppkoppling_adm(con)
      skapad_i_funktionen <- TRUE
    } else skapad_i_funktionen <- FALSE # slut test om att det inte är ett con-objekt, om det är ett con-objekt görs inget mer utan vi använder det bara
  } else {
    stop("En anslutning till databasen måste skickas med, som con-objekt eller som namn på databasen man vill koppla upp mot.")
  } 
  
  # Skapa topologi till pgrouting
  # Använder BEGIN och COMMIT tillsammans med tryCatch för att säkerställa att allt görs eller inget
  tryCatch({
    dbBegin(con)
    # Kolla om topologi-tabellen redan finns, isf ta bort den
    res <- dbGetQuery(con, glue("SELECT table_name FROM information_schema.tables WHERE table_schema = '{schema_graf}' AND table_name = '{tabell_graf}_vertices_pgr';"))
    
    if (nrow(res) == 1) {
      dbExecute(con, glue("DROP TABLE {schema_graf}.{tabell_graf}_vertices_pgr;"))
      print(glue("Tabellen {tabell_graf}_vertices_pgr fanns redan och har tagits bort."))
    }
    
    # se till att id är av typen integer
    dbExecute(con, glue("ALTER TABLE {schema_graf}.{tabell_graf} ALTER COLUMN {id_kol_graf} TYPE integer USING {id_kol_graf}::integer;"))
    # lägg till kolumnen "source"
    dbExecute(con, glue("ALTER TABLE {schema_graf}.{tabell_graf} ADD COLUMN IF NOT EXISTS source integer;"))
    # lägg till kolumnen "target"
    dbExecute(con, glue("ALTER TABLE {schema_graf}.{tabell_graf} ADD COLUMN IF NOT EXISTS target integer;"))
    # skapa topologi i pgrouting
    dbExecute(con, glue("SELECT pgr_createTopology('{schema_graf}.{tabell_graf}', '{tolerans}', the_geom := '{geom_kol_graf}', id := '{id_kol_graf}');"))
    
    
    
    # analysera den nyligen skapade topologin
    dbExecute(con, glue("SELECT pgr_analyzeGraph('{schema_graf}.{tabell_graf}', {tolerans}, the_geom := '{geom_kol_graf}', id := '{id_kol_graf}');"))
    
    
    #Om allt gått bra, committa
    dbCommit(con)
    
    # fyll på variabler som ska vara till metadatabasen 
    meta_graf <- postgres_meta(
      con = con,
      query = glue("WHERE schema = '{schema_graf}' AND tabell = '{tabell_graf}'")) %>% 
      select(version_datum, version_tid, kommentar)
    
    nu <- now()
    dag <- as.integer(format(with_tz(nu, tzone = Sys.timezone()), "%d"))
    manad_ar <- tolower(format(with_tz(nu, tzone = Sys.timezone()), "%b%Y"))
    tid <- format(nu, "%H%M")
    pgr_graf_ver <- paste0(dag, manad_ar, "_", tid)
    
    frantabell_ver_db <- glue("pgr_graf ver: {pgr_graf_ver}, {meta_graf$kommentar}") 
    lyckad_uppdatering <- TRUE
    
    #Om något fel skett i blocket, kör en rollback
  }, error = function(e) {
    dbRollback(con)
    stop("Transaktionen misslyckades: ", e$message)
    
    frantabell_ver_db <- glue("{e$message}")
    lyckad_uppdatering <- FALSE
    
  }, finally = {

    # fyll på metadata-tabellen
    postgres_metadata_uppdatera(
      con = con,
      schema = schema_graf,
      tabell = tabell_graf,
      version_datum = as.Date(nu),
      version_tid = format(nu, "%H:%M"),
      lyckad_uppdatering = lyckad_uppdatering,
      kommentar = frantabell_ver_db
    )
    
    # Koppla ner om uppkopplingen har skapats i funktionen
    if(skapad_i_funktionen){
      dbDisconnect(con)
      print("Uppkopplingen avslutad!")
    }
    
    # Beräkna och skriv ut tidsåtgång
    sluttid <- Sys.time()
    tidstagning <- sluttid - starttid
    message(sprintf("Processen skapa graf tog %s minuter att köra", tidstagning %>% round(., 1)))
  }
  )
}

#====== 4. Funktion för att koppla tabell med punkter till pgRoutinggraf
# Denna ska köras för att få ett nid på våra punkt tabeller. 
# Den ska köras två gånger en på från_tabellen och en på till_tabellen
# 
# Obs!! Om det är så att du vill kolla mellan två adresser behöver du inte köra den två gånger.

pgrouting_punkttabell_koppla_till_pgr_graf <- function(
    con = "ruttanalyser",                      # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
    schema_punkter = "punktlager",             # schema där punkterna finns, default är punktlager
    tabell_punkter = "adresser",               # tabell med punkter som ska kopplas till grafen, default är adresser_dalarna
    geom_kol_punkter = "geom",                 # geometri-kolumnen i punkttabellen, default är geom
    id_kol_punkter = "gml_id",                 # id-kolumnen i punkttabellen, default är gml_id
    schema_natverk = "grafer",                 # schema där grafen finns, default är nvdb
    tabell_natverk = "nvdb_alla_adresser",     # tabell med grafen som ska användas, default är graf_nvdb_adresser_dalarna
    generella_namn_prova = TRUE                # TRUE om man ska testa kolumnnamn "id" respektive "geom"/"geometry" om inte medskickad id-kolumnnamn eller geokolumnnamn finns
){
  
  # Starta tidstagning
  starttid <- Sys.time()
  
  # om man skickat med ett con (dvs. ska skriva resultatet till annan databas)
  if (!is.null(con)) {
    # testar om det är ett con-objekt, i så fall gör vi ingenting utan utgår från att det är korrekt
    if (!inherits(con, "DBIConnection")){
      # om det inte är ett con-objekt så behöver vi skapa det men då krävs keyring-paketet och en service som heter rd_geodata
      if (!requireNamespace("keyring", quietly = TRUE)) {
        stop("Funktionen kräver paketet 'keyring' för att hantera databasuppkopplingar. Installera det med install.packages('keyring').")
      }
      # om man har paketet keyring måste också servicen "rd_geodata" finnas, som ska innehålla inloggningsuppgifter till databasen
      if (!"rd_geodata" %in%  keyring::key_list()$service) {
        stop("Keyring-service 'rd_geodata' saknas. Skapa den och lägg in användare och lösenord till databasen där.")
      }
      con <- uppkoppling_adm(con)
      skapad_i_funktionen <- TRUE
    } else skapad_i_funktionen <- FALSE # slut test om att det inte är ett con-objekt, om det är ett con-objekt görs inget mer utan vi använder det bara
  } else {
    stop("En anslutning till databasen måste skickas med, som con-objekt eller som namn på databasen man vill koppla upp mot.")
  } 
  
  id_kol_finns <- TRUE
  geom_kol_finns <- TRUE
  # kolla om id-kolum samt geometri-kolumn finns, prova andra namn om generella_namn_prova = TRUE 
  if (generella_namn_prova) {
    # prova om id-kolumn för punkter finns, om inte, testa "id" annars blir id_kol_finns = FALSE
    if (postgres_finns_schema_tabell_kolumner(con = con,
                                          schema = schema_punkter,
                                          tabell = tabell_punkter,
                                          kolumner = id_kol_punkter,
                                          stoppa_vid_fel = FALSE)$allt_finns) {
      
    } else if (postgres_finns_schema_tabell_kolumner(con = con,
                                               schema = schema_punkter,
                                               tabell = tabell_punkter,
                                               kolumner = "id",
                                               stoppa_vid_fel = FALSE)$allt_finns) {
      id_kol_punkter <- "id"
      
    } else id_kol_finns <- FALSE
    
    # prova om geo-kolumn för punkter finns, om inte, testa "geom" och "geometry" annars blir geom_kol_finns = FALSE
    
    if (postgres_finns_schema_tabell_kolumner(con = con,
                                               schema = schema_punkter,
                                               tabell = tabell_punkter,
                                               kolumner = geom_kol_punkter,
                                               stoppa_vid_fel = FALSE)$allt_finns) {
      
    } else if (postgres_finns_schema_tabell_kolumner(con = con,
                                                      schema = schema_punkter,
                                                      tabell = tabell_punkter,
                                                      kolumner = "geometry",
                                                      stoppa_vid_fel = FALSE)$allt_finns) {
      geom_kol_punkter <- "geometry"
      
    } else if (postgres_finns_schema_tabell_kolumner(con = con,
                                                      schema = schema_punkter,
                                                      tabell = tabell_punkter,
                                                      kolumner = "geom",
                                                      stoppa_vid_fel = FALSE)$allt_finns) {
      geom_kol_punkter <- "geom"
      
    } else geom_kol_finns <- FALSE
    
  } else {             # om generella_namn_prova = FALSE
    if (postgres_finns_schema_tabell_kolumner(con = con,
                                               schema = schema_punkter,
                                               tabell = tabell_punkter,
                                               kolumner = id_kol_punkter,
                                               stoppa_vid_fel = FALSE)$allt_finns) id_kol_finns <- FALSE
    if (postgres_finns_schema_tabell_kolumner(con = con,
                                                schema = schema_punkter,
                                               tabell = tabell_punkter,
                                               kolumner = geom_kol_punkter,
                                               stoppa_vid_fel = FALSE)$allt_finns) geom_kol_finns <- FALSE
  } # slut kontroll om kolumnnamn finns
  
  if (id_kol_finns & geom_kol_finns) {
    tryCatch({
      dbBegin(con)
      # vi börjar med att skapa en ny kolumn i den nya tabellen
      dbExecute(con, glue("ALTER TABLE {schema_punkter}.{tabell_punkter} ADD COLUMN IF NOT EXISTS nid_{tabell_natverk} bigint;"))
      # Töm kolumnen om där redan finns värden
      dbExecute(con, glue("UPDATE {schema_punkter}.{tabell_punkter} SET nid_{tabell_natverk} = NULL;"))
      # därefter gör vi en spatial join från mittpunkten till noderna i nvdb
      sql_koppla_toponode <- glue("
                      UPDATE {schema_punkter}.{tabell_punkter} AS f
                      SET nid_{tabell_natverk} = n.id
                      FROM (
                        SELECT f2.{id_kol_punkter} AS punkt_id, v.id
                        FROM {schema_punkter}.{tabell_punkter} AS f2
                        JOIN LATERAL (
                          SELECT id
                          FROM {schema_natverk}.{tabell_natverk}_vertices_pgr AS v
                          ORDER BY f2.{geom_kol_punkter} <-> v.the_geom
                          LIMIT 1
                        ) AS v ON TRUE
                      ) AS n
                      WHERE f.{id_kol_punkter} = n.punkt_id;
                    ")
      dbExecute(con, sql_koppla_toponode)        
      
      
      # Om allt gått bra, committa
      dbCommit(con)
      print(glue("Punkterna i {schema_punkter}.{tabell_punkter} har nu nid_{tabell_natverk} kopplat till sig från grafen {schema_natverk}.{tabell_natverk}."))
      
      # skapa variabler för att fylla på metadata-tabellen
      meta_punkter <- postgres_meta(
        con = con,
        query = glue("WHERE schema = '{schema_punkter}' AND tabell = '{tabell_punkter}'")
      ) %>% 
        select(version_datum, version_tid, kommentar)
      
      meta_pgr_graf <- postgres_meta(
        con = con,
        query = glue("WHERE schema = '{schema_natverk}' AND tabell = '{tabell_natverk}'")
      ) %>% 
        select(version_datum, version_tid, kommentar)
      
      tabell_ver_db <- glue("{meta_punkter$kommentar}, {meta_pgr_graf$kommentar}") %>%    # lägg ihop och ta bort dubletter
        str_split(",\\s*") %>%        # Dela upp på kommatecken
        unlist() %>%                  # Gör till vektor
        unique() %>%                  # Ta bort dubbletter
        str_c(collapse = ", ")        # Sätt ihop igen
      
      lyckad_uppdatering <- TRUE
      
    }, error = function(e) {
      # Om något gått fel under processen, återställ databasen till innan denna funktion
      dbRollback(con)
      message(glue("Transaktionen misslyckades: {e$message}"))
      
      tabell_ver_db <- glue("{e$message}")
      lyckad_uppdatering <- FALSE
      
    }, finally = {
      # kod som körs oavsett om skriptet fungerade att köra eller inte
      # hämta metadata för fran-tabellen
      
      postgres_metadata_uppdatera(
        con = con,
        schema = schema_punkter,
        tabell = tabell_punkter,
        version_datum = meta_punkter$version_datum,
        version_tid = meta_punkter$version_tid,
        lyckad_uppdatering = lyckad_uppdatering,
        kommentar = tabell_ver_db
      )
    }) # slut tryCatch() 
  } else {
    # om inte både id-kolumn och geometri-kolumn finns
    if (!id_kol_finns) cat("Angiven id-kolumn saknas, kontrollera namnet och kör igen med korrekt id-kolumnnamn.")
    if (!geom_kol_finns) cat("Angiven geometri-kolumn saknas, kontrollera namnet och kör igen med korrekt geometri-kolumnnamn.")
  }
    
    
  # Koppla ner om uppkopplingen har skapats i funktionen
  if(skapad_i_funktionen){
    dbDisconnect(con)
    #print("Uppkopplingen avslutad!")
  }
  
} # slut funktion

#====== 4,5. Funktion för att skapa kostnadskolumner för alla transportsätt
# Körs för att få kostnad för gång, cykel, elcykel och bil (utöver meter)

pgrouting_kostnadskolumner_transporttyp_graf <- function(
    con = "ruttanalyser",                           # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
    schema_natverk = "grafer",                        # schema där grafen finns, default är nvdb
    tabell_natverk = "nvdb_alla_adresser",   # tabell med grafen som ska användas, default är graf_nvdb_adresser_dalarna
    kostnadskolumn_bil_f = "hastighetsgrans_f",
    kostnadskolumn_bil_b = "hastighetsgrans_b",
    kostnadskolumn_meter = "kostnad_meter",
    berakna_kostnad_bil = TRUE,              # för att kunna stänga av vissa transportsätt, kostnadskolumn måste finnas annars beräknas det ändå inte
    berakna_kostnad_gang = TRUE,
    berakna_kostnad_cykel = TRUE,
    berakna_kostnad_elcykel = TRUE
    ){
  
  # Starta tidstagning
  starttid <- Sys.time()
  
  # om man skickat med ett con (dvs. ska skriva resultatet till annan databas)
  if (!is.null(con)) {
    # testar om det är ett con-objekt, i så fall gör vi ingenting utan utgår från att det är korrekt
    if (!inherits(con, "DBIConnection")){
      # om det inte är ett con-objekt så behöver vi skapa det men då krävs keyring-paketet och en service som heter rd_geodata
      if (!requireNamespace("keyring", quietly = TRUE)) {
        stop("Funktionen kräver paketet 'keyring' för att hantera databasuppkopplingar. Installera det med install.packages('keyring').")
      }
      # om man har paketet keyring måste också servicen "rd_geodata" finnas, som ska innehålla inloggningsuppgifter till databasen
      if (!"rd_geodata" %in%  keyring::key_list()$service) {
        stop("Keyring-service 'rd_geodata' saknas. Skapa den och lägg in användare och lösenord till databasen där.")
      }
      con <- uppkoppling_adm(con)
      skapad_i_funktionen <- TRUE
    } else skapad_i_funktionen <- FALSE # slut test om att det inte är ett con-objekt, om det är ett con-objekt görs inget mer utan vi använder det bara
  } else {
    stop("En anslutning till databasen måste skickas med, som con-objekt eller som namn på databasen man vill koppla upp mot.")
  } 
  
  tryCatch({
    dbBegin(con)
    # vi börjar med att skapa en ny kolumn i den nya tabellen för de
    # transportsätt som vi vill ha med och har underlag för (kostnadskolumn)
    kolumner_att_lagga_till <- c()
    
    if (berakna_kostnad_gang && !is.null(kostnadskolumn_meter)) {
      kolumner_att_lagga_till <- c(kolumner_att_lagga_till,
                                   "ADD COLUMN IF NOT EXISTS kostnad_gang_min double precision"
      )
    }
    
    if (berakna_kostnad_cykel && !is.null(kostnadskolumn_meter)) {
      kolumner_att_lagga_till <- c(kolumner_att_lagga_till,
                                   "ADD COLUMN IF NOT EXISTS kostnad_cykel_min double precision"
      )
    }
    
    if (berakna_kostnad_elcykel && !is.null(kostnadskolumn_meter)) {
      kolumner_att_lagga_till <- c(kolumner_att_lagga_till,
                                   "ADD COLUMN IF NOT EXISTS kostnad_elcykel_min double precision"
      )
    }
    
    if (berakna_kostnad_bil && !is.null(kostnadskolumn_bil_f)) {
      kolumner_att_lagga_till <- c(kolumner_att_lagga_till,
                                   "ADD COLUMN IF NOT EXISTS kostnad_bil_f_min double precision"
      )
    }
    
    if (berakna_kostnad_bil && !is.null(kostnadskolumn_bil_b)) {
      kolumner_att_lagga_till <- c(kolumner_att_lagga_till,
                                   "ADD COLUMN IF NOT EXISTS kostnad_bil_b_min double precision"
      )
    }
    
    # Kör bara om det finns något att lägga till
    if (length(kolumner_att_lagga_till) > 0) {
      alter_sql <- glue::glue("
    ALTER TABLE {schema_natverk}.{tabell_natverk}
    {glue::glue_collapse(kolumner_att_lagga_till, sep = ',\n    ')}
    ;")
      DBI::dbExecute(con, alter_sql)
    }
    
    # gör beräkningarna
    berakningar <- c()
    
    if (berakna_kostnad_gang && !is.null(kostnadskolumn_meter)) {
      berakningar <- c(berakningar,
                       glue("kostnad_gang_min = {kostnadskolumn_meter} / ({pgrouting_hastighet_gang()} / 3.6) / 60")
      )
    }
    
    if (berakna_kostnad_cykel && !is.null(kostnadskolumn_meter)) {
      berakningar <- c(berakningar,
                       glue("kostnad_cykel_min = {kostnadskolumn_meter} / ({pgrouting_hastighet_cykel()} / 3.6) / 60")
      )
    }
    
    if (berakna_kostnad_elcykel && !is.null(kostnadskolumn_meter)) {
      berakningar <- c(berakningar,
                       glue("kostnad_elcykel_min = {kostnadskolumn_meter} / ({pgrouting_hastighet_elcykel()} / 3.6) / 60")
      )
    }
    
    if (berakna_kostnad_bil && !is.null(kostnadskolumn_bil_f)) {
      berakningar <- c(berakningar,
                       glue("kostnad_bil_f_min = {kostnadskolumn_meter} / (NULLIF({kostnadskolumn_bil_f}, 0) / 3.6) / 60")
      )
    }
    
    if (berakna_kostnad_bil && !is.null(kostnadskolumn_bil_b)) {
      berakningar <- c(berakningar,
                       glue("kostnad_bil_b_min = {kostnadskolumn_meter} / (NULLIF({kostnadskolumn_bil_b}, 0) / 3.6) / 60")
      )
    }
    
    # Om det finns något att beräkna
    if (length(berakningar) > 0) {
      update_sql <- glue::glue("
    UPDATE {schema_natverk}.{tabell_natverk}
    SET {glue::glue_collapse(berakningar, sep = ',\n        ')}
  ")
      DBI::dbExecute(con, update_sql)
    }
    
    # Om allt gått bra, committa
    dbCommit(con)
    print(glue("Kostnadskolumner i {schema_natverk}.{tabell_natverk} har beräknats."))
    
    # skapa variabler för att fylla på metadata-tabellen
    meta_kostnader <- postgres_meta(
      con = con,
      query = glue("WHERE schema = '{schema_natverk}' AND tabell = 'nvdb_{natverkstyp}_{punkter_till_tabell}'")
    ) %>% 
      select(version_datum, version_tid, kommentar)
    
    nu <- now()
    dag <- as.integer(format(with_tz(nu, tzone = Sys.timezone()), "%d"))
    manad_ar <- tolower(format(with_tz(nu, tzone = Sys.timezone()), "%b%Y"))
    tid <- format(nu, "%H%M")
    kostnad_ver <- paste0(dag, manad_ar, "_", tid)
    
    
    frantabell_ver_db <- glue("{meta_kostnader$kommentar}, kostnader ver: {kostnad_ver}")
    lyckad_uppdatering <- TRUE
    
    
  }, error = function(e) {
    # Om något gått fel under processen, återställ databasen till innan denna funktion
    dbRollback(con)
    message(glue("Transaktionen misslyckades: {e$message}"))
    
    frantabell_ver_db <- glue("{e$message}")
    lyckad_uppdatering <- FALSE
    
  }, finally = {
  
  # # kod som körs oavsett om skriptet fungerade att köra eller inte
  # # hämta metadata för fran-tabellen
  # postgres_metadata_uppdatera(
  #   con = con,
  #   schema = schema_natverk,
  #   tabell = glue("nvdb_{natverkstyp}_{punkter_till_tabell}"),
  #   version_datum = meta_kostnader$version_datum,
  #   version_tid = meta_kostnader$version_tid,
  #   lyckad_uppdatering = lyckad_uppdatering,
  #   kommentar = frantabell_ver_db
  # )
    
  }) # slut tryCatch()
  
  # Koppla ner om uppkopplingen har skapats i funktionen
  if(skapad_i_funktionen){
    dbDisconnect(con)
    #print("Uppkopplingen avslutad!")
  }
  
} # slut funktion

#====== 5. Beräkna restid/avstånd mellan från punkter och till-punkter

pgrouting_skapa_geotabell_rutt_fran_till <- function(
    con = "ruttanalyser",                           # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)    schema_output,
    schema_fran = "punktlager",
    tabell_fran = "adresser",
    schema_till,
    tabell_till,
    schema_graf = "grafer",
    tabell_graf = "nvdb_alla_adresser",
    schema_output = "resultat",
    tabell_till_namnkol = "mottagnings_namn",
    tabell_till_idkol = "gml_id",
    urval_till_tabell = NULL,        # för att göra ett urval med wheresats glue("where malpunkt_namn != 'Närakut Ludvika' ") eller glue("where malpunkt_namn != 'Akutmottagning Avesta' ")
    urval_till_namn = "",            # för att döpa lagret så att man vet att det är ett urval, tex. "utan_ludvika"
    hastighet_gang = NULL,           # om NULL används pgrouting_hastighet_gang(), dvs. standardvärde för hastighet gång
    hastighet_cykel = NULL,          # om NULL används pgrouting_hastighet_cykel(), dvs. standardvärde för hastighet cykel
    hastighet_elcykel = NULL,        # om NULL används pgrouting_hastighet_elcykel(), dvs. standardvärde för hastighet elcykel
    batch_storlek = 1000,            # Parameter för batch-storlek, lämna som den är om inte väldigt goda skäl finns att ändra det
    antal_batcher_test = NULL        # NULL när man kör skarpt, annars antal batcher om man vill begränsa en körning i ett test, heltal (tex 2 om man vill köra 2 batcher)
){
  # Funktion för att skapa en geotabell för närmaste rutt från alla rader i fråntabell till
  # närmaste punkt i till-tabellen dijkstraNear() med pgrouting, inklusive alla rutter som
  # linjer
  
  # Starta tidstagning
  starttid <- Sys.time()
  
  # om man skickat med ett con (dvs. ska skriva resultatet till annan databas)
  if (!is.null(con)) {
    # testar om det är ett con-objekt, i så fall gör vi ingenting utan utgår från att det är korrekt
    if (!inherits(con, "DBIConnection")){
      # om det inte är ett con-objekt så behöver vi skapa det men då krävs keyring-paketet och en service som heter rd_geodata
      if (!requireNamespace("keyring", quietly = TRUE)) {
        stop("Funktionen kräver paketet 'keyring' för att hantera databasuppkopplingar. Installera det med install.packages('keyring').")
      }
      # om man har paketet keyring måste också servicen "rd_geodata" finnas, som ska innehålla inloggningsuppgifter till databasen
      if (!"rd_geodata" %in%  keyring::key_list()$service) {
        stop("Keyring-service 'rd_geodata' saknas. Skapa den och lägg in användare och lösenord till databasen där.")
      }
      con <- uppkoppling_adm(con)
      skapad_i_funktionen <- TRUE
    } else skapad_i_funktionen <- FALSE # slut test om att det inte är ett con-objekt, om det är ett con-objekt görs inget mer utan vi använder det bara
  } else {
    stop("En anslutning till databasen måste skickas med, som con-objekt eller som namn på databasen man vill koppla upp mot.")
  } 
  
  # hämta värden för hastigheter per transportsätt om de inte man skickat med det
  if (is.null(hastighet_gang)) hastighet_gang <- pgrouting_hastighet_gang() 
  if (is.null(hastighet_cykel)) hastighet_cykel <- pgrouting_hastighet_cykel() 
  if (is.null(hastighet_elcykel)) hastighet_elcykel <- pgrouting_hastighet_elcykel() 
  
  
  tryCatch({
  # Skapa en ny tabell med alla unika nid i tabell_fran om den inte redan finns. Om den finns så töm tabellen först
  # 1. SKapa den nya tabellens namn, en kombination av de två tabellerna fran och till
  
  # först tar vi reda på vilken typ av nätverk det är, tex. "alla" eller "bil"
    natverkstyp <- str_extract(tabell_graf, "alla|bil")
  
  # gammal lösning:  
  # natverkstyp <- str_remove(tabell_graf, fixed(tabell_fran)) %>%
  #   str_remove("_$") %>%
  #   str_extract("[^_]+$")
  
  if (rlang::is_empty(urval_till_tabell)){
    tabell_ny <- glue("{tabell_fran}_till_{tabell_till}_{natverkstyp}")
    urval_till_tabell = ""
  } else {
    tabell_ny <- glue("{tabell_fran}_till_{tabell_till}_{urval_till_namn}_{natverkstyp}")
  }
  
  # här hämtas kolumner från från-tabellen, men tar inte med
  # geom, geometry eller nid, dessa tas med i resultat-tabellerna
  frantabell_kolumner <- dbGetQuery(con, glue("
    SELECT column_name, data_type
    FROM information_schema.columns
    WHERE table_schema = '{schema_fran}'
      AND table_name = '{tabell_fran}'
    ORDER BY ordinal_position;
  ")) %>%
    filter(
      !column_name %in% c("geom", "geometry"),
      !str_detect(column_name, "nid_")
    ) %>% 
    mutate(data_type = ifelse(data_type == "USER-DEFINED", "geometry", data_type),
           data_type = ifelse(data_type == "text", "varchar", data_type),
           definition = glue("{column_name} {data_type}"))
  
  # kolumner som tas med från målpunkterna samt ruttanalysen som genomförs
  # med pgr_dijkstraNear(), dessa läggs efter kolumnerna från "från"-tabellen ovan
  malpunkt_rutt_kolumner <- "malpunkt_id varchar,
                            malpunkt_namn varchar,
                            start_vid int, 
                            end_vid int, 
                            kostnad_meter double precision,
                            kostnad_gang_min double precision,
                            kostnad_cykel_min double precision,
                            kostnad_elcykel_min double precision,
                            kostnad_bil_min double precision,
                            geom geometry" %>% 
    str_split(",\\s*\n?") %>%                      # Dela upp strängen i en lista med ett element per kolumndefinition (separerat på kommatecken, ev. följt av radbrytning)
    pluck(1) %>%                                   # extrahera vektorn (första elementet) ur listan som str_split() returnerar.
    str_trim() %>%                                 # kör en trim (ta bort blanksteg i början eller i slutet av strängen)
    str_match("^([a-zA-Z0-9_]+)\\s+(.+)$") %>%     # Plocka ut kolumnnamn och datatyp med regex. Första gruppen (([a-zA-Z0-9_]+)) är kolumnnamnet, andra ((.+)) är datatypen
    as_tibble(.name_repair = "minimal") %>%        # Gör en tibble av detta
    setNames(c("tabort", "column_name", "data_type")) %>%       # döp kolumnerna i tibble:n
    select(-tabort) %>%                                         # ta bort den första kolumnen som vi inte behöver
    mutate(definition = glue("{column_name} {data_type}"))      # skapa en kolumn av kolumnnamn + mellanslag + datatyp, det använder vi i sql-kod nedan
  
  # 2. Kolla om den redan finns, isf töm den
  res <- dbGetQuery(con, glue("SELECT table_name FROM information_schema.tables WHERE table_schema = '{schema_output}' AND table_name = '{tabell_ny}';"))
  if (nrow(res) == 0) {
    
    # sql_query_drop <- glue("DROP table {schema_output}.{tabell_ny};") # Droppa den nya om det är så att du vill göra en förändring på den nya tabellen
    # dbExecute(con, sql_query_drop)
    
    sql_query_create <- glue("CREATE table IF NOT EXISTS {schema_output}.{tabell_ny} (
                            {frantabell_kolumner$definition %>% paste(collapse = ',\n')},
                            {malpunkt_rutt_kolumner$definition %>% paste(collapse = ',\n')}
                            );")
    
    
    # Skapa den nya tabellen med samma kolumnnamn som djikstra_result
    dbExecute(con, sql_query_create)
    
    print(glue("Tabellen {tabell_ny} har skapats."))
  } else {
    #dbExecute(con, glue("TRUNCATE TABLE {schema_output}.{tabell_ny};"))
    dbExecute(con, glue("TRUNCATE TABLE {schema_output}.{tabell_ny};"))
    print(glue("Tabellen {tabell_ny} fanns redan och har tömts på värden."))
  }
  # Skapa temporära tabeller för nidn från tabell_fran och tabell_till, skall användas i djikstranearcost
  temp_fran <- glue("temp_{tabell_fran}")
  temp_till <- glue("temp_{tabell_till}")
  
  dbExecute(con, glue("DROP TABLE IF EXISTS {temp_fran};"))
  dbExecute(con, glue("DROP TABLE IF EXISTS {temp_till};"))
  
  dbExecute(con, glue("CREATE TEMP TABLE {temp_fran} AS SELECT DISTINCT nid_{tabell_graf} FROM {schema_fran}.{tabell_fran};"))
  dbExecute(con, glue("CREATE TEMP TABLE {temp_till} AS SELECT DISTINCT nid_{tabell_graf} FROM {schema_till}.{tabell_till};"))
  
  # Skapa index till temptabellerna, behöver ej droppas efter de försvinner automagiskt när tabellen droppas
  dbExecute(con, glue("CREATE INDEX temp_fran_nidx ON {temp_fran}(nid_{tabell_graf});"))
  dbExecute(con, glue("CREATE INDEX temp_till_nidx ON {temp_till}(nid_{tabell_graf})"))
  
  # Skapa förutsättningar för batchkörning
  # Räkna antalet rader i tabell_ny
  antal_noder <- dbGetQuery(con, glue("SELECT COUNT(*) as antal FROM {temp_fran}"))$antal
  
  # Beräkna antalet batcher
  antal_batcher <- ceiling(antal_noder / batch_storlek)
  
  
  # ================ om test - kör antal batcher som finns i antal_batcher_test
  if (!is.null(antal_batcher_test)) antal_batcher <- antal_batcher_test
  
  # skapa kostnad utifrån om vi vill mäta kortast sträcka med 
  # restid bil eller avstånd i meter (restid bil är rimligt när man gör
  # ett dedikerat bilnätverk, annars är meter rimligast)
  if (natverkstyp == "bil") {
    # skapa två CASE-satser, en för hastighetsgrans_f och en för hastighetsgrans_b
    # och så kör vi directed := true, för att köra riktat nätverk
    kostnad_berakning <- "CASE 
    WHEN hastighetsgrans_f IS NOT NULL THEN kostnad_meter / (hastighetsgrans_f / 3.6)
    ELSE -1 
  END AS cost,
  CASE 
    WHEN hastighetsgrans_b IS NOT NULL THEN kostnad_meter / (hastighetsgrans_b / 3.6)
    ELSE -1 
  END AS reverse_cost"
    #kostnad_berakning <- "kostnad_meter / (hastighetsgrans_f / 3.6) AS cost, \nkostnad_meter / (hastighetsgrans_b / 3.6) AS reverse_cost"
    #kostnad_berakning_b <- ", kostnad_meter / (hastighetsgrans_b / 3.6) AS reverse_cost \n"
    #kostnad_filter <- " AND hastighetsgrans_f > 0"
    kostnad_filter <- ""
    kostnad_as_sats <- " AS segment_tid_bil"
    kostnad_sum <- "SUM(segment_tid_bil) AS kostnad_tid_bil,"
    vagnat_directed <- "true"
  } else {
    kostnad_berakning <- "kostnad_meter AS cost"
    #kostnad_berakning_b <- ""
    kostnad_filter <- ""
    kostnad_as_sats <- ""
    kostnad_sum <- "NULL AS kostnad_tid_bil,"
    vagnat_directed <- "false"
  }
  
  print(glue("Letar efter kortaste vägen från {antal_noder} noder i {antal_batcher} batcher."))
  total_start <- Sys.time()
  #for(i in 1:antal_batcher){
  walk((1:antal_batcher), function(i){  
    start_tid <- Sys.time()
    tryCatch({
      offset <- (i - 1) * batch_storlek
      
      # Kör pgr_dijkstraNear() med start- och slutvid från temptabellerna med unika noder och stoppa in i tabell_ny
      # Denna kör dijkstraNear() en gång och kopplar sedan på kostnader från nvdb_grafen. 
      
      sql_query <- glue("
         INSERT INTO {schema_output}.{tabell_ny} (
            {frantabell_kolumner$column_name %>% paste(collapse = ',\n')}, 
            {malpunkt_rutt_kolumner$column_name %>% paste(collapse = ',\n')}
          )
      WITH dijkstra_raw AS (
        SELECT *
        FROM pgr_dijkstraNear(
          'SELECT rad_id AS id,
                  source,
                  target,
                  {kostnad_berakning}
           FROM {schema_graf}.{tabell_graf}
           WHERE kostnad_meter IS NOT NULL{kostnad_filter}',
          ARRAY(
            SELECT DISTINCT nid_{tabell_graf} 
            FROM {schema_fran}.{tabell_fran} LIMIT {batch_storlek} OFFSET {offset}
          )::INT[],
          ARRAY(
            SELECT DISTINCT nid_{tabell_graf} 
            FROM {schema_till}.{tabell_till} {urval_till_tabell}
          )::INT[],
          directed := {vagnat_directed},
          cap := 1,
          global := false
        )
      ),
      rutter AS (
        SELECT
          d.path_seq,
          d.start_vid,
          d.end_vid,
          d.edge,
          v.kostnad_meter,
          d.cost{kostnad_as_sats},
          v.geom
        FROM dijkstra_raw d
        JOIN {schema_graf}.{tabell_graf} v ON d.edge = v.rad_id
      ),
      summerad_rutt AS (
        SELECT
          start_vid,
          end_vid,
          SUM(kostnad_meter) AS kostnad_meter,
          {kostnad_sum}
          ST_LineMerge(ST_Union(ARRAY_AGG(geom ORDER BY path_seq))) AS rutt_geom
        FROM rutter
        WHERE edge != -1
        GROUP BY start_vid, end_vid
      )
      SELECT
        {frantabell_kolumner$column_name %>% paste0('fran.', ., collapse = ',\n')},
        till.{tabell_till_idkol} AS malpunkt_id,
        till.{tabell_till_namnkol} AS malpunkt_namn,
        r.start_vid,
        r.end_vid,
        r.kostnad_meter,
        r.kostnad_meter / ({hastighet_gang} / 3.6) / 60 AS kostnad_gang_min,
        r.kostnad_meter / ({hastighet_cykel} / 3.6) / 60 AS kostnad_cykel_min,
        r.kostnad_meter / ({hastighet_elcykel} / 3.6) / 60 AS kostnad_elcykel_min,
        CAST(r.kostnad_tid_bil AS double precision) / 60 AS kostnad_bil_min,
        -- CAST(r.kostnad_tid_bil AS double precision) AS kostnad_bil_min,
        r.rutt_geom AS geom
      FROM summerad_rutt r
      JOIN {schema_fran}.{tabell_fran} fran ON r.start_vid = fran.nid_{tabell_graf}
      JOIN {schema_till}.{tabell_till} till ON r.end_vid = till.nid_{tabell_graf}
      ORDER BY r.start_vid;
    ")
      
      #print(glue("Kör batch {i} av {antal_batcher} med {batch_storlek} noder."))
      #print(sql_query)
      #print(sql_query)     # felsökning
      
      dbExecute(con, sql_query)
      # tid <- difftime(Sys.time(), start_tid, units = "secs") %>% as.numeric() %>% round(.,1)
      # print(glue("Batch {i} av {antal_batcher} bearbetad, denna batch tog {tid} sekunder att köra."))
      #Sys.sleep(90)
    }, warning = function(w) {
      message("Varning upptäckt: ", w)
    }, error = function(e) {
      
      print(glue("Fel under bearbetning av batch {i}: ", e$message))
    }
    )
  }, .progress = TRUE)
  total_tid <- difftime(Sys.time(), total_start, units = "mins") %>% as.numeric() %>% round(.,1)
  print(glue("Samtliga {antal_batcher} batcher är bearbetade, hela körningen tog {total_tid} minuter att köra."))
  
  # Leta noder där start och slutnod är samma och sätt kostnad till 0
  sql_query <- glue("INSERT INTO {schema_output}.{tabell_ny} (start_vid, end_vid, kostnad_meter, kostnad_gang_min, kostnad_cykel_min, kostnad_elcykel_min, kostnad_bil_min)
                        SELECT t.nid_{tabell_graf}, tt.nid_{tabell_graf}, 0, 0, 0, 0, 0
                        FROM {temp_till} tt
                        INNER JOIN {temp_fran} t ON tt.nid_{tabell_graf} = t.nid_{tabell_graf}
                        WHERE NOT EXISTS (
                            SELECT 1
                            FROM {schema_output}.{tabell_ny} ny
                            WHERE ny.start_vid = t.nid_{tabell_graf}
                        );")
  #print(sql_query)     # felsökning
  dbExecute(con, sql_query)
  
  # ===== vi skapar ett punktlager av frånlagret med målpunkts-
  # id respektive namn samt kostnadskolumnerna
  dbExecute(con, glue("DROP TABLE IF EXISTS {schema_output}.{tabell_ny}_punkt;"))
  
  dbExecute(con, glue("CREATE TABLE {schema_output}.{tabell_ny}_punkt AS
  SELECT 
    a.*,
    r.malpunkt_id,
    r.malpunkt_namn,
    r.kostnad_meter,
    r.kostnad_gang_min,
    r.kostnad_cykel_min,
    r.kostnad_elcykel_min,
    r.kostnad_bil_min
  FROM {schema_fran}.{tabell_fran} a
  LEFT JOIN (
      SELECT DISTINCT ON (start_vid)
        *
  FROM {schema_output}.{tabell_ny} ) r
    ON a.nid_{tabell_graf} = r.start_vid;"))
  
  # Skapa spatialt index
  dbExecute(con, glue("
  CREATE INDEX ON {schema_output}.{tabell_ny}_punkt USING GIST (geom);
"))
  
  # Droppa temptabellen
  dbExecute(con, glue("DROP TABLE IF EXISTS {temp_fran};"))
  dbExecute(con, glue("DROP TABLE IF EXISTS {temp_till};"))
  
  #Om allt gått bra, committa
  dbCommit(con)
  
  # Kolla hur många rader som finns i den nya tabellen, bör stämma med antalet unika noder
  antal_rader <- dbGetQuery(con, glue("SELECT COUNT(*) as antal FROM {schema_output}.{tabell_ny}"))$antal
  
  print(glue("Det tog {round(difftime(Sys.time(), starttid, units = \"mins\"),1)} minuter att fylla tabellen {schema_output}.{tabell_ny} med {antal_rader} rader, att jämföra med {antal_noder} unika noder."))
  
  if(antal_rader != antal_noder){
    print("Det finns en differens mellan förväntat och faktiskt resultat vilket indikerar att det finns fler adress rader som är kopplade till unika noder.")
  }
  
  # skapa variabler för att fylla på metadata-tabellen
  meta_punkter_fran <- postgres_meta(
    con = con,
    query = glue("WHERE schema = '{schema_fran}' AND tabell = '{tabell_fran}'")
  ) %>% 
    dplyr::pull(kommentar) %>% 
    str_extract("^[^,]+")
  
  meta_punkter_till <- postgres_meta(
    con = con,
    query = glue("WHERE schema = '{schema_till}' AND tabell = '{tabell_till}'")
  ) %>% 
    dplyr::pull(kommentar) %>% 
    str_extract("^[^,]+")
  
  meta_pgr_graf <- postgres_meta(
    con = con,
    query = glue("WHERE schema = '{schema_graf}' AND tabell = '{tabell_graf}'")
  ) %>% 
    dplyr::pull(kommentar)
  
  ruttreslutat_ver_db <- glue("{meta_pgr_graf}, fran punkter {meta_punkter_fran}, till punkter {meta_punkter_till}")
  
  
  # fyll på variabler som ska vara till metadatabasen 
  meta_ruttresultat <- postgres_meta(
    con = con,
    query = glue("WHERE schema = '{schema_graf}' AND tabell = '{tabell_graf}'")) %>% 
    select(version_datum, version_tid, kommentar)
  
  nu <- now()
  dag <- as.integer(format(with_tz(nu, tzone = Sys.timezone()), "%d"))
  manad_ar <- tolower(format(with_tz(nu, tzone = Sys.timezone()), "%b%Y"))
  tid <- format(nu, "%H%M")
  pgr_graf_ver <- paste0(dag, manad_ar, "_", tid)
  
  ruttreslutat_ver_db <- glue("pgr_graf ver: {pgr_graf_ver}, {meta_ruttresultat$kommentar}") 
  
  
  #Om något fel skett i blocket, kör en rollback
  }, error = function(e) {
    dbRollback(con)
    stop("Transaktionen misslyckades: ", e$message)
    
    ruttreslutat_ver_db <- glue("{e$message}")
    lyckad_uppdatering <- FALSE
    
  }, finally = {
    
    # fyll på metadata-tabellen
    postgres_metadata_uppdatera(
      con = con,
      schema = schema_graf,
      tabell = tabell_graf,
      version_datum = as.Date(nu),
      version_tid = format(nu, "%H:%M"),
      lyckad_uppdatering = lyckad_uppdatering,
      kommentar = ruttreslutat_ver_db
    )
    
    # Beräkna och skriv ut tidsåtgång
    sluttid <- Sys.time()
    tidstagning <- sluttid - starttid
    message(sprintf("Processen skapa graf tog %s minuter att köra", tidstagning %>% round(., 1)))
  }) # slut på trycatch
  
  # Koppla ner om uppkopplingen har skapats i funktionen
  if(skapad_i_funktionen){
    dbDisconnect(con)
    #print("Uppkopplingen avslutad!")
  }
} # slut pgr_dijkstraNear()-funktion


pgrouting_skapa_ny_graf_nvdb_koppla_till_punkter <- function(
    con_geodb = uppkoppling_adm("geodata"),
    con_rutt = uppkoppling_adm("ruttanalyser"),
    natverkstyp = "bil",            # "bil" eller "alla", "alla" kör kortaste rutt på avstånd, "bil" på restid med bil (ofta men inte alltid samma rutter), planer på att skapa nätverk med bara vägar man kan cykla på (inte alla bilvägar)
    punkter_fran_schema = "adresser",
    punkter_fran_tabell = "dalarna",
    punkter_till_tabell = "adresser",
    punkter_till_schema = "punktlager"
) {
  
  # Funktion för att vid ny version av NVDB uppdatera graf för adresser
  # i Dalarna så att vi alltid har färdiga grafer att köra ruttanalyser
  # eller skapa isokroner utifrån.
  
  
  # 1. Kopiera nvdb till en ny tabell och bearbeta lagret så att det fungerar bra med adresser
  
  # om man vill ha urval i nätverket, typ bara ta med bilvägar ("bil"), eller bara vägar som går att cykla på (inte implementerat ännu)
  # eller alla vägar (natverkstyp == "alla")
  natverk_urval <- case_when(natverkstyp == "bil" ~ "WHERE vagtrafiknat_nattyp = 1",
                             TRUE ~ "")
  
  
  pgrouting_klipp_natverk_skapa_tabell(
    con = con_geodb,
    con_till_databas = con_rutt,
    buffer_m = 30000,                      # Om man vill klippa nätverket utanför en gräns (tex. länsgräns), ange hur många meter
    natverk_schema = "nvdb",               # schema i vilket nätverket finns, default är nvdb
    natverk_tabell = "dala_med_grannlan",  # tabell som används att klippa i, default är dala_med_grannlan 
    natverk_geokol = "geom",               # geometry-kolumnen i nätverkstabellen, default är geom
    region_schema = "karta",               # schema för region att klippa nätverk med, default är karta
    region_tabell = "lan_lm",              # tabell för region att klippa nätverk med, default är lan_lm, som är länsgränser som är korrekta men inte snygga (som scb:s läns- och kommungränser)
    regionkod_kol = "lankod",              # regionkoderna ligger i denna kolumn, tex. länskoder eller kommunkoder
    regionkoder = "20",                    # man kan ha en eller skicka med en vektor med regionkoder, t.ex. c("20", "21")
    region_geokol = "geom",                # geometry-kolumnen i region-tabellen, default är geom
    output_schema = "grafer",              # schema att spara output-tabellen i, default är nvdb
    output_tabell = glue("nvdb_{natverkstyp}"),           # tabell att spara output i, default är nvdb_alla     (alla för bil + gång och cykelväg, annars tex bil), detta lager är klippt för att koppla bättre mot adresspunkterna
    urval_fran_natverk = natverk_urval                # om man vill välja något specifikt från nätverkstabellen, ska då vara i formatet "WHERE vagtrafiknat_nattyp = 1", annars ska det vara ""
  )
  
  # 2. Kopiera adresser från geodatabasen till ruttanalyser-databasen                                )
  
  postgis_kopiera_tabell_mellan_databaser(
    con_fran_databas = con_geodb,                # från geodatabasen
    con_till_databas = con_rutt,                 # till ruttanalyser-databasen
    schema_fran = punkter_fran_schema,                    # schema i geodatabasen där adresserna finns
    tabell_fran = punkter_fran_tabell,                     # tabell i geodatabasen där adresserna finns
    schema_till = punkter_till_schema,                  # schema i ruttanalyser-databasen där adresserna ska kopieras till
    tabell_till = punkter_till_tabell,            # tabell i ruttanalyser-databasen där adresserna ska kopieras till
    lagg_till_metadata_i_till_databas = TRUE
  )
  
  # 3. Hitta närmaste punkt på nätverket för varje adress, och skapa klusterpunkter
  
  pgrouting_hitta_narmaste_punkt_pa_natverk(
    con = con_rutt,                           # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
    schema_punkter_fran = "punktlager",             # att köra med adresser är default, men det går att köra med andra tabeller, schema och från annan databas
    tabell_punkter_fran = punkter_till_tabell,       # tabellen som används, default är "adresser_dalarna" som är alla adresser i Dalarna
    geometri_kol_punkter_fran = "geom",             # geometri-kolumnen i punkttabellen från. 
    id_kol_punkter_fran = "gml_id",                 # id-kolumnen i punkttabellen från, default är "gml_id"
    schema_graf = "grafer",                         # schema för grafen, dvs. nätverket som ska användas, default är nvdb
    tabell_graf = glue("nvdb_{natverkstyp}"),                       # tabell för grafen, dvs. nätverket som ska användas, default är graf_nvdb
    geometri_graf = "geom",                         # geometri-kolumnen i grafen, dvs. nätverket som ska användas, default är geom
    id_graf = "rad_id",                             # id-kolumnen i grafen, dvs. nätverket som ska användas, default är rad_id
    tolerans_avstand = 3                            # för att bygga kluster av punkter och spara körtid, 0 = då skapar vi i praktiken inga kluster
  )
  
  # 4. Skapa en graf med det nya nätverket som har klippts med de klusterpunkter som skapades ovan
  
  pgrouting_tabell_till_pgrgraf(
    con = con_rutt,                           # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
    schema_graf = "grafer",                           # schema där tabellen som ska bli graf ligger. Default: nvdb
    tabell_graf = glue("nvdb_{natverkstyp}_{punkter_till_tabell}"),     # Tabellen som ska förberedas för pgrouting. Default: graf_nvdb_adresser_dalarna
    id_kol_graf = "rad_id",                         # Kolumnen med id i den tabell som ska bli graf
    geom_kol_graf = "geom",                     # Namn på kolumnen som innehåller geometrin i tabellen som ska bli graf
    tolerans = 0.001           # Toleransvärdet för hur nära segment måste vara för att ansluta. 
  )
  
  # 5. Beräkna kostnader i graf-tabellen
  pgrouting_kostnadskolumner_transporttyp_graf(
    con = "ruttanalyser",                           # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
    schema_natverk = "grafer",                        # schema där grafen finns, default är nvdb
    tabell_natverk = glue("nvdb_{natverkstyp}_{punkter_till_tabell}"),   # tabell med grafen som ska användas, default är graf_nvdb_adresser_dalarna
    kostnadskolumn_bil_f = "hastighetsgrans_f",
    kostnadskolumn_bil_b = "hastighetsgrans_b",
    kostnadskolumn_meter = "kostnad_meter",
    berakna_kostnad_bil = if (natverkstyp == "bil") TRUE else FALSE,              # för att kunna stänga av vissa transportsätt, kostnadskolumn måste finnas annars beräknas det ändå inte
    berakna_kostnad_gang = TRUE,
    berakna_kostnad_cykel = TRUE,
    berakna_kostnad_elcykel = TRUE
  )
  
  # 6. Flytta de tabeller som inte behövs till schemat underlag_grafer
  
  # lägg de tabeller vi ska flytta i en vektor
  tabeller_flytt <- c(
    glue("nvdb_{natverkstyp}"),
    glue("nvdb_{natverkstyp}_{punkter_till_tabell}_narmaste_punkt"),
    glue("nvdb_{natverkstyp}_{punkter_till_tabell}_klusterpunkt")
  )
  
  # vi kör samma kod på alla tre tabeller som ska flyttas
  walk(tabeller_flytt, function(tabell_namn) {
    # kopiera tabeller till schemat underlag_grafer, skriv över om de redan finns
    postgis_kopiera_tabell(
      con = con_rutt,
      schema_fran = "grafer",
      tabell_fran = tabell_namn,
      schema_till = "underlag_grafer",
      tabell_till = tabell_namn,
      skriv_over = TRUE
    )
    
    # ta bort dem i schemat grafer
    postgres_tabell_ta_bort(
      con = con_rutt,
      schema = "grafer",
      tabell = tabell_namn
    )
    
    # och sedan ändrar vi deras schema i metadata-tabellen
    DBI::dbExecute(con_rutt, glue::glue("
    WITH senaste AS (
      SELECT id
      FROM metadata.uppdateringar
      WHERE tabell = '{tabell_namn}' AND schema = 'grafer'
      ORDER BY version_datum DESC, version_tid DESC
      LIMIT 1
      )
      UPDATE metadata.uppdateringar
      SET schema = 'underlag_grafer'
      WHERE id IN (SELECT id FROM senaste);
    "))
    
  }) # slut walk-funktion
  
  # 7. Koppla punkter till nätverket, vi kör samtliga punktlager till samtliga grafer
  
  # hämta grafer från ruttanalyser-db i schemat "grafer"
  grafer <- postgres_lista_scheman_tabeller(con = con_rutt) %>%
    pluck("grafer") %>% 
    .[!str_detect(., "vertices_pgr")]
  
  # hämta punktlager från ruttanalyser-db i schemat "punktlager"
  punktlager <- postgres_lista_scheman_tabeller(con = con_rutt) %>%
    pluck("punktlager")
  
  kombinationer <- expand_grid(punkttabell = punktlager, graf = grafer)
  
  pwalk(kombinationer, function(punkttabell, graf) {
    pgrouting_punkttabell_koppla_till_pgr_graf(
      con = con_rutt,                           # databas där punkterna finns, default är ruttanalyser (dvs. Region Dalarnas databas för ruttanalyser)
      schema_punkter = "punktlager",                  # schema där punkterna finns, default är punktlager
      tabell_punkter = punkttabell,            # tabell med punkter som ska kopplas till grafen, default är adresser_dalarna
      geom_kol_punkter = "geom",                      # geometri-kolumnen i punkttabellen, default är geom
      id_kol_punkter = "gml_id",                      # id-kolumnen i punkttabellen, default är gml_id
      schema_natverk = "grafer",                        # schema där grafen finns, default är nvdb
      tabell_natverk = graf   # tabell med grafen som ska användas, default är graf_nvdb_adresser_dalarna
    )
  })
  
  dbDisconnect(con_geodb)
  dbDisconnect(con_rutt)
} # slut funktion


# definiera hastigheter för gång, cykel och elcykel
pgrouting_hastighet_gang <- function() 5
pgrouting_hastighet_cykel <- function() 16
pgrouting_hastighet_elcykel <- function() 22


# äldre pgrouting-funktioner, avnänds inte längre
las_in_rutor_xlsx_till_postgis_skapa_pgr_graf <- 
  function(inlas_mapp = "G:/Samhällsanalys/GIS/rutor/",
           inlas_filer,        # enbart filnamnen på filerna som ska läsas in
           inlas_tabellnamn,   # de tabellnamn de nya filerna ska få i postgis
           rutstorlek = 100,
           schema_rut = "rutor",
           schema_natverk = "nvdb",
           tabell_natverk = "nvdb20buff30") {
    
    #  Läs in och bearbeta excelfiler med rutdata 
    
    df_list <- map(inlas_filer, ~read.xlsx(paste0(inlas_mapp, .x)))
    gis_list <- list()
    
    # loopa igenom listan med df:s som ska läsas in till postgis
    for (df_item in 1:length(df_list)){
      
      # om inte rutid finns som kolumnnamn, skapa ett rutid från x- och y-koordinaterna som sätts ihop
      if (!"rutid" %in% names(df_list[[df_item]])) df_list[[df_item]]$rutid <- paste0(df_list[[df_item]]$ruta_x, df_list[[df_item]]$ruta_y) 
      # flytta rutid så att den ligger först av kolumnerna
      df_list[[df_item]] <- df_list[[df_item]] %>% relocate(rutid, .before = ruta_x)
      
      # beräkna mittpunkt för varje ruta (halva rutstorlekten adderas x- och y-koordinaten, som är nedre vänstra hörnet i SCB-rutor)
      df_list[[df_item]] <- berakna_mittpunkter(df_list[[df_item]], "ruta_x", "ruta_y", rutstorlek)
      
      # skapa geometrikolumn för (mitt)punkter
      gis_list[[df_item]] <- st_as_sf(df_list[[df_item]], coords = c("mitt_y", "mitt_x"), crs = 3006)
      names(gis_list[[df_item]])[names(gis_list[[df_item]]) == "geometry"] <- "geom_point"        # döp om geom-kolumnen
      st_geometry(gis_list[[df_item]]) <- "geom_point"         # man måste tala om igen att den omdöpta kolumnen är geom-kolumn
      
      # skapa en andra geometrikolumn där vi skapar polygoner runt mittpunkten som utgör själva rutan
      gis_list[[df_item]]$geom_polygon <- st_buffer(gis_list[[df_item]]$geom_point,(rutstorlek/2), endCapStyle = "SQUARE")
      
      # säkerställ att alla kolumnnamn är i gemener, ställer inte till problem i postigis då
      names(df_list[[df_item]]) <- tolower(names(df_list[[df_item]]))
      
      # lägg över till postgis
      
      con <- dbConnect(          # use in other settings
        RPostgres::Postgres(),
        # without the previous and next lines, some functions fail with bigint data 
        #   so change int64 to integer
        bigint = "integer",  
        user = Sys.getenv("userid_postgres_adm"),
        password = Sys.getenv("pwd_postgres_adm"),
        host = "WFALTSTVS427.ltdalarna.se",
        port = 5432,
        dbname = "postgis_31_db",
        options="-c search_path=public")
      
      # kör sql-kod för att skapa ett nytt schema med namn definierat ovan
      dbExecute(con, paste0("create schema if not exists ", schema_rut, ";"))
      
      # skriv rut-lagren till postgis 
      starttid = Sys.time()
      st_write(obj = gis_list[[df_item]],
               dsn = con,
               Id(schema=schema_rut, table = inlas_tabellnamn[df_item]))
      print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "sec"),1), " sekunder att läsa in ", inlas_tabellnamn[df_item], " till postgis."))
      
      # skapa spatialt index, finns det sedan tidigare, ta bort
      dbExecute(con, paste0("DROP INDEX IF EXISTS ", schema_rut, ".geom_point_idx;")) 
      dbExecute(con, paste0("CREATE INDEX geom_point_idx ON ", schema_rut, ".", inlas_tabellnamn[df_item], " USING GIST (geom_point);"))
      dbExecute(con, paste0("DROP INDEX IF EXISTS ", schema_rut, ".geom_polygon_idx;")) 
      dbExecute(con, paste0("CREATE INDEX geom_polygon_idx ON ", schema_rut, ".", inlas_tabellnamn[df_item], " USING GIST (geom_polygon);"))
      
      # gör rutid till id-kolumn i tabellen
      dbExecute(con, paste0("ALTER TABLE ", schema_rut, ".", inlas_tabellnamn[df_item], " ADD PRIMARY KEY (rutid);"))
      
      # gör en spatial join för mittpunkten i rutorna till nvdb 
      
      # vi börjar med att skapa en ny kolumn i den nya tabellen
      dbExecute(con, paste0("ALTER TABLE ", schema_rut, ".", inlas_tabellnamn[df_item],
                            " ADD COLUMN IF NOT EXISTS toponode_id bigint;"))
      
      # därefter gör vi en spatial join från mittpunkten till noderna i nvdb
      dbExecute(con, paste0("UPDATE ", schema_rut, ".", inlas_tabellnamn[df_item],
                            " SET toponode_id = (",
                            "SELECT ", schema_natverk, ".", tabell_natverk, "_vertices_pgr.id ",
                            "FROM ", schema_natverk, ".", tabell_natverk, "_vertices_pgr ",
                            "ORDER BY ", schema_rut, ".", inlas_tabellnamn[df_item], ".geom_point <-> ",
                            schema_natverk, ".", tabell_natverk, "_vertices_pgr.the_geom ASC NULLS LAST ",
                            "LIMIT 1);"))
      
      dbDisconnect(con)           # stäng postgis-anslutningen igen
    } # slut for-loop som loopar igenom inläsningsfiler
  } # slut funktion


las_in_fil_skapa_punkter_till_postgis_skapa_pgr_graf <- 
  function(inlas_mapp,
           inlas_filer,        # enbart filnamnen på filerna som ska läsas in
           inlas_tabellnamn,   # de tabellnamn de nya filerna ska få i postgis
           schema_malpunkter,
           malpunkter_id_kol,
           malpunkter_x_koord_kol,
           malpunkter_y_koord_kol,
           malpunkter_crs = 3006,
           malpunkter_till_crs = 3006,
           schema_natverk = "nvdb",
           tabell_natverk = "nvdb20buff30",
           pg_db_user = Sys.getenv("userid_postgres_adm"),
           pg_db_pwd = Sys.getenv("pwd_postgres_adm"),
           pg_db_host = "WFALTSTVS427.ltdalarna.se",
           pg_db_port = 5432,
           pg_db_name_db = "postgis_31_db") {
    
    # Skript för att läsa in en tabell som innehåller målpunkter som man vill göra beräkningar mot 
    # företrädelsevis från boenderutor på avstånd eller restid med kortaste eller snabbaste resväg
    # från rutor (eller annan geografi som punkter) till närmaste målpunkt i denna tabell.
    # Det kan vara restid till närmaste skola, vårdcentral eller busshållplats från samtliga 
    # boenderutor i en geografi
    #
    # Följande parametrar skickas med funktionen:
    # inlas_mapp = mapp i vilken tabellen finns som innehåller målpunkterna, måste innehålla kolumner
    #              för x- och y- koordinat
    # inlas_filer = en vektor med den eller de filer som ska läsas in, måste finnas i inlas_mapp
    # inlas_tabellnamn = en textsträng eller vektor om det finns flera filer med tabellnamnet som 
    #                    målpunkterna ska ha i postgisdatabasen (bör vara gemener och utan konstiga tecken)
    # schema_malpunkter = det schema i postgisdatabasen som målpunktstabellen ska ligga under
    # malpunkter_id_kol = den kolumn som innehåller ett unikt ID och görs till primärnyckelkolumn, måste finnas!
    # malpunkter_id
    # schema_natverk = det schema där nätverket mot vilket vi ska koppla målpunkterna finns 
    # tabell_natverk = den tabell där nätverket mot vilket vi ska koppla målpunkterna finns, måste 
    #                  finnas under schemat ovan
    # pg_db_user = användare för den postgisdatabas man ansluter till
    # pg_db_pwd = lösenord för användaren ovan, OBS! Aldrig i klartext!
    # pg_db_host = adress till den server där postgis-databasen finns
    # pg_db_port = den port som databasen ansluts via
    # pg_db_name_db = den databas i postgis som man ansluter till
    
    # Läs in och bearbeta excelfiler med punktkoordinater
    
    df_list <- map(inlas_filer, ~read.xlsx(paste0(inlas_mapp, .x)))
    gis_list <- list()
    
    # loopa igenom listan med df:s som ska läsas in till postgis
    for (df_item in 1:length(df_list)){
      
      # skapa geometrikolumn för punkter utifrån koordinat-kolumner
      gis_list[[df_item]] <- st_as_sf(df_list[[df_item]], coords = c(malpunkter_x_koord_kol, malpunkter_y_koord_kol), crs = malpunkter_crs)
      names(gis_list[[df_item]])[names(gis_list[[df_item]]) == "geometry"] <- "geom"        # döp om geom-kolumnen
      st_geometry(gis_list[[df_item]]) <- "geom"         # man måste tala om igen att den omdöpta kolumnen är geom-kolumn
      
      # säkerställ att alla kolumnnamn är i gemener, ställer inte till problem i postigis då
      names(gis_list[[df_item]]) <- tolower(names(gis_list[[df_item]]))
      
      # om data inte är i rätt projektionssystem, konvertera till rätt
      if (malpunkter_crs != malpunkter_till_crs) gis_list[[df_item]] <- st_transform(gis_list[[df_item]], crs = malpunkter_till_crs)
      
      # lägg över till postgis
      
      con <- dbConnect(          # use in other settings
        RPostgres::Postgres(),
        # without the previous and next lines, some functions fail with bigint data 
        #   so change int64 to integer
        bigint = "integer",  
        user = pg_db_user,
        password = pg_db_pwd,
        host = pg_db_host,
        port = pg_db_port,
        dbname = pg_db_name_db,
        options="-c search_path=public")
      
      # kör sql-kod för att skapa ett nytt schema med namn definierat ovan
      dbExecute(con, paste0("create schema if not exists ", schema_malpunkter, ";"))
      
      
      # skriv målpunkts-lagren till postgis 
      starttid = Sys.time()
      st_write(obj = gis_list[[df_item]],
               dsn = con,
               Id(schema=schema_malpunkter, table = inlas_tabellnamn[df_item]))
      print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "sec"),1), " sekunder att läsa in ", inlas_tabellnamn[df_item], " till postgis."))
      
      # skapa spatialt index, finns det sedan tidigare, ta bort
      dbExecute(con, paste0("DROP INDEX IF EXISTS ", schema_malpunkter, ".geom_idx;")) 
      dbExecute(con, paste0("CREATE INDEX geom_idx ON ", schema_malpunkter, ".", inlas_tabellnamn[df_item], " USING GIST (geom);"))
      
      # gör skickad id-kolumn till primärnyckelkolumn i tabellen
      dbExecute(con, paste0("ALTER TABLE ", schema_malpunkter, ".", inlas_tabellnamn[df_item], " ADD PRIMARY KEY (", malpunkter_id_kol, ");"))
      
      # vi börjar med att skapa en ny kolumn i den nya tabellen
      dbExecute(con, paste0("ALTER TABLE ", schema_malpunkter, ".", inlas_tabellnamn[df_item],
                            " ADD COLUMN IF NOT EXISTS toponode_id bigint;"))
      
      # därefter gör vi en spatial join från punkterna till noderna i nvdb
      dbExecute(con, paste0("UPDATE ", schema_malpunkter, ".", inlas_tabellnamn[df_item],
                            " SET toponode_id = (",
                            "SELECT ", schema_natverk, ".", tabell_natverk, "_vertices_pgr.id ",
                            "FROM ", schema_natverk, ".", tabell_natverk, "_vertices_pgr ",
                            "ORDER BY ", schema_malpunkter, ".", inlas_tabellnamn[df_item], ".geom <-> ",
                            schema_natverk, ".", tabell_natverk, "_vertices_pgr.the_geom ASC NULLS LAST ",
                            "LIMIT 1);"))
      
      dbDisconnect(con)           # stäng postgis-anslutningen igen
    } # slut for-loop som loopar igenom inläsningsfiler
  } # slut funktion


las_in_geosf_skapa_punkter_till_postgis_skapa_pgr_graf <- 
  function(inlas_df,
           inlas_tabellnamn,   # de tabellnamn de nya filerna ska få i postgis
           schema_malpunkter = "malpunkter",
           malpunkter_id_kol,
           malpunkter_geo_kol,
           malpunkter_till_crs,
           schema_natverk = "nvdb",
           tabell_natverk = "nvdb20buff30",
           pg_db_user = Sys.getenv("userid_postgres_adm"),
           pg_db_pwd = Sys.getenv("pwd_postgres_adm"),
           pg_db_host = "WFALTSTVS427.ltdalarna.se",
           pg_db_port = 5432,
           pg_db_name_db = "postgis_31_db") {
    
    # Skript för att läsa in en tabell som innehåller målpunkter som man vill göra beräkningar mot 
    # företrädelsevis från boenderutor på avstånd eller restid med kortaste eller snabbaste resväg
    # från rutor (eller annan geografi som punkter) till närmaste målpunkt i denna tabell.
    # Det kan vara restid till närmaste skola, vårdcentral eller busshållplats från samtliga 
    # boenderutor i en geografi
    #
    # Följande parametrar skickas med funktionen:
    # inlas_mapp = mapp i vilken tabellen finns som innehåller målpunkterna, måste innehålla kolumner
    #              för x- och y- koordinat
    # inlas_filer = en vektor med den eller de filer som ska läsas in, måste finnas i inlas_mapp
    # inlas_tabellnamn = en textsträng eller vektor om det finns flera filer med tabellnamnet som 
    #                    målpunkterna ska ha i postgisdatabasen (bör vara gemener och utan konstiga tecken)
    # schema_malpunkter = det schema i postgisdatabasen som målpunktstabellen ska ligga under
    # malpunkter_id_kol = den kolumn som innehåller ett unikt ID och görs till primärnyckelkolumn, måste finnas!
    # malpunkter_geo_kol = geometry-kolumnen
    # schema_natverk = det schema där nätverket mot vilket vi ska koppla målpunkterna finns 
    # tabell_natverk = den tabell där nätverket mot vilket vi ska koppla målpunkterna finns, måste 
    #                  finnas under schemat ovan
    # pg_db_user = användare för den postgisdatabas man ansluter till
    # pg_db_pwd = lösenord för användaren ovan, OBS! Aldrig i klartext!
    # pg_db_host = adress till den server där postgis-databasen finns
    # pg_db_port = den port som databasen ansluts via
    # pg_db_name_db = den databas i postgis som man ansluter till
    
    # Läs in och bearbeta excelfiler med rutdata
    
    
    # säkerställ att alla kolumnnamn är i gemener, ställer inte till problem i postigis då
    names(inlas_df) <- tolower(names(inlas_df))
    
    # lägg över till postgis
    
    con <- dbConnect(          # use in other settings
      RPostgres::Postgres(),
      # without the previous and next lines, some functions fail with bigint data 
      #   so change int64 to integer
      bigint = "integer",  
      user = pg_db_user,
      password = pg_db_pwd,
      host = pg_db_host,
      port = pg_db_port,
      dbname = pg_db_name_db,
      options="-c search_path=public")
    
    # kör sql-kod för att skapa ett nytt schema med namn definierat ovan
    dbExecute(con, paste0("create schema if not exists ", schema_malpunkter, ";"))
    
    #  skriv rut-lagren till postgis 
    starttid = Sys.time()
    st_write(obj = inlas_df,
             dsn = con,
             Id(schema=schema_malpunkter, table = inlas_tabellnamn))
    print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "sec"),1), " sekunder att läsa in ", inlas_tabellnamn, " till postgis."))
    
    # skapa spatialt index, finns det sedan tidigare, ta bort
    dbExecute(con, paste0("DROP INDEX IF EXISTS ", schema_malpunkter, ".", malpunkter_geo_kol, "_idx;")) 
    dbExecute(con, paste0("CREATE INDEX ", malpunkter_geo_kol, "_idx ON ", schema_malpunkter, ".", inlas_tabellnamn, " USING GIST (", malpunkter_geo_kol, ");"))
    
    # gör rutid till id-kolumn i tabellen
    dbExecute(con, paste0("ALTER TABLE ", schema_malpunkter, ".", inlas_tabellnamn, " ADD PRIMARY KEY (", malpunkter_id_kol ,");"))
    
    # gör en spatial join för mittpunkten i rutorna till nvdb
    
    # vi börjar med att skapa en ny kolumn i den nya tabellen
    dbExecute(con, paste0("ALTER TABLE ", schema_malpunkter, ".", inlas_tabellnamn,
                          " ADD COLUMN IF NOT EXISTS toponode_id bigint;"))
    
    # därefter gör vi en spatial join från mittpunkten till noderna i nvdb
    dbExecute(con, paste0("UPDATE ", schema_malpunkter, ".", inlas_tabellnamn,
                          " SET toponode_id = (",
                          "SELECT ", schema_natverk, ".", tabell_natverk, "_vertices_pgr.id ",
                          "FROM ", schema_natverk, ".", tabell_natverk, "_vertices_pgr ",
                          "ORDER BY ", schema_malpunkter, ".", inlas_tabellnamn, ".", malpunkter_geo_kol, " <-> ",
                          schema_natverk, ".", tabell_natverk, "_vertices_pgr.the_geom ASC NULLS LAST ",
                          "LIMIT 1);"))
    
    dbDisconnect(con)           # stäng postgis-anslutningen igen
    
  } # slut funktion


koppla_punkter_postgis_tabell_till_pgr_graf <- 
  function(punkter_schema,
           punkter_tabellnamn,   
           punkter_geo_kol,
           schema_natverk = "nvdb",
           tabell_natverk = "nvdb20buff30",
           pg_db_user,
           pg_db_pwd ,
           pg_db_host,
           pg_db_port = 5432,
           pg_db_name_db) {
    
    # Skript för att läsa in en postgis-tabell som innehåller målpunkter som man vill göra beräkningar mot 
    # företrädelsevis från boenderutor på avstånd eller restid med kortaste eller snabbaste resväg
    # från rutor (eller annan geografi som punkter) till närmaste målpunkt i denna tabell.
    # Det kan vara restid till närmaste skola, vårdcentral eller busshållplats från samtliga 
    # boenderutor i en geografi
    #
    # Följande parametrar skickas med funktionen:
    # malpunkter_tabellnamn = en textsträng eller vektor om det finns flera filer med tabellnamnet som 
    #                    målpunkterna ska ha i postgisdatabasen (bör vara gemener och utan konstiga tecken)
    # schema_malpunkter = det schema i postgisdatabasen som målpunktstabellen ska ligga under
    # malpunkter_id_kol = den kolumn som innehåller ett unikt ID och görs till primärnyckelkolumn, måste finnas!
    # malpunkter_geo_kol = geometry-kolumnen
    # schema_natverk = det schema där nätverket mot vilket vi ska koppla målpunkterna finns 
    # tabell_natverk = den tabell där nätverket mot vilket vi ska koppla målpunkterna finns, måste 
    #                  finnas under schemat ovan
    # pg_db_user = användare för den postgisdatabas man ansluter till
    # pg_db_pwd = lösenord för användaren ovan, OBS! Aldrig i klartext!
    # pg_db_host = adress till den server där postgis-databasen finns
    # pg_db_port = den port som databasen ansluts via
    # pg_db_name_db = den databas i postgis som man ansluter till
    
    # Läs in och bearbeta excelfiler med rutdata
    
    
    con <- dbConnect(          # use in other settings
      RPostgres::Postgres(),
      # without the previous and next lines, some functions fail with bigint data 
      #   so change int64 to integer
      bigint = "integer",  
      user = pg_db_user,
      password = pg_db_pwd,
      host = pg_db_host,
      port = pg_db_port,
      dbname = pg_db_name_db,
      options="-c search_path=public")
    
    # gör en spatial join för mittpunkten i rutorna till nvdb
    
    # vi börjar med att skapa en ny kolumn i den nya tabellen
    dbExecute(con, paste0("ALTER TABLE ", punkter_schema, ".", punkter_tabellnamn,
                          " ADD COLUMN IF NOT EXISTS toponode_id bigint;"))
    
    # därefter gör vi en spatial join från mittpunkten till noderna i nvdb
    dbExecute(con, paste0("UPDATE ", punkter_schema, ".", punkter_tabellnamn,
                          " SET toponode_id = (",
                          "SELECT ", schema_natverk, ".", tabell_natverk, "_vertices_pgr.id ",
                          "FROM ", schema_natverk, ".", tabell_natverk, "_vertices_pgr ",
                          "ORDER BY ", punkter_schema, ".", punkter_tabellnamn, ".", punkter_geo_kol, " <-> ",
                          schema_natverk, ".", tabell_natverk, "_vertices_pgr.the_geom ASC NULLS LAST ",
                          "LIMIT 1);"))
    
    dbDisconnect(con)           # stäng postgis-anslutningen igen
    
  } # slut funktion


skapa_n_narmaste_malpunkter_tabell <- function(
    n_narmaste = 10,                                  # hur många målpunkter ska beräkningen göras på
    malpunkt_schema,                                  # schema där målpunkterna finns och där rutorna finns  
    malpunkt_tabell,                                  # tabell som innehåller de målpunkter som ska beräknas
    malpunkt_toponode_id = "toponode_id",             # id-kolumn för toponode (för målpunkt från rutt-grafen)
    malpunkt_id_kol = "id",                           # id-kolumn för målpunktstabell
    malpunkt_ovr_kolumner = NA,                       # fylls på med vektor med kolumner som också ska med från målpunktslagret
    mal_tab_n_narmaste,                               # ny tabell med n narmaste malpunkter för varje ruta
    startpunkt_schema,
    startpunkt_tabell,                                       # tabell som innehåller de rutor där slutresultaten ska läggas i
    startpunkt_tabell_id_kol,                      # id-kolumn för ruttabell
    startpunkt_geom_kol, 
    startpunkt_toponode_id = "toponode_id",                  # id-kolumn för toponode (för rutlager från rutt-grafen)
    startpunkt_tabell_malschema,
    pg_db_user,
    pg_db_pwd,
    pg_db_host,
    pg_db_port,
    pg_db_name_db
){
  
  # beräkna n närmaste malpunkter till varje ruta 
  starttid <- Sys.time()
  
  # skapa textvariabel av medskickade målpunktskolumner
  if (!is.na(malpunkt_ovr_kolumner[1])) {
    
    # för användning i första delen av sql-skriptet
    malp_ovr_kol <- malpunkt_ovr_kolumner
    malp_ovr_kol <- paste0("malp.", malp_ovr_kol, collapse = ", ")
    malp_ovr_kol <- paste0(malp_ovr_kol, ", ")
    
    # för användning i första delen av sql-skriptet
    m_ovr_kol <- malpunkt_ovr_kolumner
    m_ovr_kol <- paste0("m.", m_ovr_kol, collapse = ", ")
    m_ovr_kol <- paste0(m_ovr_kol, ", ")
    
  } else {
    malp_ovr_kol <- ""
    m_ovr_kol <- ""
  }
  
  con_n_narmaste <- dbConnect(          # use in other settings
    RPostgres::Postgres(),
    # without the previous and next lines, some functions fail with bigint data 
    #   so change int64 to integer
    bigint = "integer",  
    user = pg_db_user,
    password = pg_db_pwd,
    host = pg_db_host,
    port = pg_db_port,
    dbname = pg_db_name_db,
    options="-c search_path=public")  
  
  # ta bort tabell om den redan finns
  dbExecute(con_n_narmaste, paste0("DROP TABLE IF EXISTS ", 
                                   malpunkt_schema, ".", mal_tab_n_narmaste, ";"))
  
  # skapa en tabell med alla startpunkter och de n närmaste målpunkterna fågelvägen
  # denna tabell används sedan för att beräkna närmaste målpunkt i grafen/nätverket
  dbExecute(con_n_narmaste, paste0("CREATE TABLE ",
                                   malpunkt_schema, ".", mal_tab_n_narmaste, " AS ",
                                   "(SELECT start.", startpunkt_tabell_id_kol, ", ",
                                   "start.", startpunkt_toponode_id, " AS start_toponode_id, ",
                                   #"rut.geom_polygon AS rut_geom_polygon, ", 
                                   "start.", startpunkt_geom_kol, " AS start_geom_point, ",
                                   "malp.", malpunkt_id_kol, ", ",
                                   "malp.", malpunkt_toponode_id, " AS mal_toponode_id, ",
                                   malp_ovr_kol,
                                   "malp.geom AS mal_geom, ", 
                                   "malp.dist ",
                                   "FROM ", malpunkt_schema, ".", startpunkt_tabell_malschema, " AS start ",
                                   "CROSS JOIN LATERAL (",
                                   "SELECT m.", malpunkt_id_kol, ", ",
                                   "m.", malpunkt_toponode_id, ", ",
                                   "m.geom, ",
                                   m_ovr_kol, 
                                   "m.geom <-> start.", startpunkt_geom_kol, " AS dist ",
                                   "FROM ", malpunkt_schema, ".", malpunkt_tabell, " AS m ",
                                   "ORDER BY dist ", 
                                   "LIMIT ", n_narmaste, " ",
                                   ") AS malp );"))
  
  dbDisconnect(con_n_narmaste)           # stäng postgis-anslutningen igen
  print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "min"),2) , 
               " minuter att beräkna fågelavstånd till de ", n_narmaste , 
               " närmaste målpunkterna från varje startpunkt i körningen."))
  
}

berakna_pgr_dijkstracost_n_narmaste_tab <- function(
    natverk_schema,
    natverk_tabell,
    start_toponode_id,
    mal_toponode_id,
    malpunkt_schema,
    mal_tab_n_narmaste,
    cost_col_natverk,
    cost_col_ny,
    pg_db_user,
    pg_db_pwd,
    pg_db_host,
    pg_db_port,
    pg_db_name_db){
  
  starttid <- Sys.time()
  
  con_dijkstra <- dbConnect(          # use in other settings
    RPostgres::Postgres(),
    # without the previous and next lines, some functions fail with bigint data 
    #   so change int64 to integer
    bigint = "integer",  
    user = pg_db_user,
    password = pg_db_pwd,
    host = pg_db_host,
    port = pg_db_port,
    dbname = pg_db_name_db,
    options="-c search_path=public")  
  
  # beräkna avstånd meter för alla målpunkter 
  # Skapa kolumn för avstånd i meter mellan ruta och målpunkt
  dbExecute(con_dijkstra, paste0("ALTER TABLE ", 
                                 malpunkt_schema, ".", mal_tab_n_narmaste, " ",
                                 "ADD COLUMN IF NOT EXISTS ", cost_col_ny, " double precision;"))
  
  # därefter uppdaterar vi kolumnen med värdet från pgRoutring - dijkstraCost
  dbExecute(con_dijkstra, paste0("UPDATE ", malpunkt_schema, ".", mal_tab_n_narmaste, " ",
                                 "SET ", cost_col_ny, " = a.agg_cost ", 
                                 "FROM (",
                                 "SELECT * FROM pgr_dijkstraCost(",
                                 "'SELECT id, source, target, ", cost_col_natverk, " as cost FROM ",
                                 natverk_schema, ".", natverk_tabell, "',",
                                 "'SELECT ", rut_toponode_id, " as source, ", 
                                 mal_toponode_id, " as target FROM ", malpunkt_schema, ".", 
                                 mal_tab_n_narmaste, "', ",
                                 "FALSE)) as a ",
                                 "WHERE (", malpunkt_schema, ".", mal_tab_n_narmaste, ".", 
                                 rut_toponode_id, " = a.start_vid AND ", 
                                 malpunkt_schema, ".", mal_tab_n_narmaste, ".", mal_toponode_id, " = a.end_vid);"))
  
  # byt ut NULL mot 0 om startnoden (ruta) är samma som slutnoden (hållplats)
  dbExecute(con_dijkstra, paste0("UPDATE ", malpunkt_schema, ".", mal_tab_n_narmaste, " ",
                                 "SET ", cost_col_ny, " = 0 ",
                                 "WHERE ", rut_toponode_id, " = ", mal_toponode_id, ";"))
  
  dbDisconnect(con_dijkstra)           # stäng postgis-anslutningen igen
  print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "min"),2) , " minuter att beräkna pgr_Dijkstra"))
  
  
}

join_narmaste_malpunkt_fran_n_narmaste <- function(
    malpunkt_schema,
    mal_start_tabell,
    mal_n_narmaste_tab,
    cost_col_n_narmaste_tab,
    cost_mal_start_tab_ny,
    namn_malpunkt_kol = "",
    n_narm_tab_startid_kol, 
    mal_start_tab_startid_kol,
    pg_db_user,
    pg_db_pwd,
    pg_db_host,
    pg_db_port,
    pg_db_name_db){
  
  starttid <- Sys.time()
  
  con_join <- dbConnect(          # use in other settings
    RPostgres::Postgres(),
    # without the previous and next lines, some functions fail with bigint data 
    #   so change int64 to integer
    bigint = "integer",  
    user = pg_db_user,
    password = pg_db_pwd,
    host = pg_db_host,
    port = pg_db_port,
    dbname = pg_db_name_db,
    options="-c search_path=public") 
  
  # skapa en textsträng om namn_malpunkt_kol har ett värde (som inte är "") för att skapa kolumn och för att koda värdet
  if (namn_malpunkt_kol == ""){ 
    lagg_till_namn_malpunkt_kol <- ""
    set_namn <- " "
    from_namn <- ""
    groupby_namn <- ""
  } else {
    lagg_till_namn_malpunkt_kol <- paste0(", ADD COLUMN IF NOT EXISTS ", cost_mal_start_tab_ny, "_", namn_malpunkt_kol, " text")
    set_namn <- paste0(", ", cost_mal_start_tab_ny, "_", namn_malpunkt_kol, " = B.", namn_malpunkt_kol, " ")
    from_namn <- paste0(", ", namn_malpunkt_kol)
    groupby_namn <- paste0(", ", namn_malpunkt_kol)
  }
  
  # skapa kolumn om den inte redan finns
  dbExecute(con_join, paste0("ALTER TABLE ", malpunkt_schema, ".", mal_start_tabell, " ",
                             "ADD COLUMN IF NOT EXISTS ", cost_mal_start_tab_ny, " double precision", 
                             lagg_till_namn_malpunkt_kol, ";"))
  
  # och så joinar vi på kolumnen från aktuell tabell
  dbExecute(con_join, paste0("UPDATE ", malpunkt_schema, ".", mal_start_tabell, " AS A ",
                             "SET ", cost_mal_start_tab_ny, " = B.minavst", set_namn,
                             "FROM (SELECT DISTINCT ON(", n_narm_tab_startid_kol, ") ",
                             n_narm_tab_startid_kol, from_namn, ", ", cost_col_n_narmaste_tab, " AS minavst ",
                             " FROM ", malpunkt_schema, ".", mal_n_narmaste_tab, " ",
                             "ORDER BY ", n_narm_tab_startid_kol, ", ", cost_col_n_narmaste_tab, ") ",
                             "AS B ",
                             "WHERE A.", mal_start_tab_startid_kol, " = B.", n_narm_tab_startid_kol, ";"))
  
  
  # MIN(", cost_col_n_narmaste_tab, ") as minavst, ", n_narm_tab_startid_kol,
  # from_namn, " FROM ", malpunkt_schema, ".", mal_n_narmaste_tab, " ", 
  # "GROUP BY ", n_narm_tab_startid_kol, groupby_namn, ") ",
  # "AS B ",
  # "WHERE A.", mal_rut_tab_rutid_kol, " = B.", n_narm_tab_startid_kol, ";"))
  
  dbDisconnect(con_join)           # stäng postgis-anslutningen igen
  
  print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "min"),2) , " minuter att köra funktionen"))
  
}


koppla_kommun_till_geokol_i_tabell <- function(
    schema,                              # det schema i vilken tabellen finns som man vill hämta
    tabell,                              # den tabell i postgisdatabasen man vill hämta
    geo_kol,                             # geometrikolumn som ska användas (bör helst vara punktgeometrier)
    filter_lan = "",                     # länskod att filtrera på om man vill, vid "" så görs ingen filtrering
    kommunnamn_ny_kol = "kommun",        # vad kommunnamnskolumnen döps till
    kommunkod_ny_kol = "kommunkod",      # vad kommunkodskolumnen döps till
    kommun_schema = "karta",             # schema där kommunpolygonerna finns som vi använder att spatial-joina från
    kommun_tabell = "kommun_polygon",    # tabell där kommunpolygonerna finns som vi använder att spatial-joina från
    kommun_kommunkod = "kommunkod",        # kolumn där kommunkoden finns i tabell som vi använder för att joina från
    kommun_lanskod = "lankod",          # kolumn där länskoden finns i tabell som vi använder för att joina från
    kommun_kommunnamn = "kommun",    # kolumn där kommunnamnet finns i tabell som vi använder för att joina från
    kommun_geokol = "geom",              # geometrikolumn i tabell för kommunpolygoner
    pg_db_user,
    pg_db_pwd,
    pg_db_host,
    pg_db_port,
    pg_db_name_db) {
  
  starttid <- Sys.time()
  
  con_join_kommun <- dbConnect(          # use in other settings
    RPostgres::Postgres(),
    # without the previous and next lines, some functions fail with bigint data 
    #   so change int64 to integer
    bigint = "integer",  
    user = pg_db_user,
    password = pg_db_pwd,
    host = pg_db_host,
    port = pg_db_port,
    dbname = pg_db_name_db,
    options="-c search_path=public") 
  
  if (filter_lan == "") {
    # om filter_lan är tomt, dvs. vi ska inte göra någon filtrering
    filter_lan <- paste0(")")
  } else {
    # om länskod är medskickad
    filter_lan <- paste0(") AND k.", kommun_lanskod, " = '", filter_lan, "'")
  }
  
  # vi börjar med att skapa kolumner för kommunkod och kommunnamn
  dbExecute(con_join_kommun, paste0("ALTER TABLE ", schema, ".", tabell, " ",
                                    "ADD COLUMN IF NOT EXISTS ", kommunkod_ny_kol, " character varying, ",
                                    "ADD COLUMN IF NOT EXISTS ", kommunnamn_ny_kol, " character varying;"))
  
  # därefter kopplar vi kommunkod till varje punkt i tabellen
  dbExecute(con_join_kommun, paste0("UPDATE ", schema, ".", tabell, " ",
                                    "SET ", kommunkod_ny_kol, " = (SELECT ", kommun_kommunkod, " ",
                                    "FROM ", kommun_schema, ".", kommun_tabell, " AS k ",
                                    "WHERE st_contains(k.", kommun_geokol, ", ", schema, ".", tabell, ".",
                                    geo_kol, filter_lan, ");"))
  
  # och så kopplar vi även kommunnamn till varje punkt i tabellen
  dbExecute(con_join_kommun, paste0("UPDATE ", schema, ".", tabell, " ",
                                    "SET ", kommunnamn_ny_kol, " = (SELECT ", kommun_kommunnamn, " ",
                                    "FROM ", kommun_schema, ".", kommun_tabell, " AS k ",
                                    "WHERE st_contains(k.", kommun_geokol, ", ", schema, ".", tabell, ".",
                                    geo_kol, filter_lan, ");"))
  
  dbDisconnect(con_join_kommun)           # stäng postgis-anslutningen igen
  
  print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "sec"),2) , " sekunder att koppla kommuner till tabellen."))
  
}

# ================================= Qgis funktioner ================================================


# ------------------- kraftfält -------------------------


# Skapad av Henrik Aldén från SWECOS skript G:/skript/gis/sweco_dec_2022/orginalskript/ del1_kraftfält_QGIS_plugin(_uppdaterad).r

# För att funktionen ska fungera, kör först funktionen skapa_vagnatverk_tatort() som finns i func_GIS. Denna funktion skapar nödvändiga tabeller i PostGIS.
# en uppdatering kommer att göras för att skapa dessa tabeller direkt i funktionen eller att det ska gå at läsa in sf_objekt istället för att använda PostGIS.

# source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
# skapa_vagnatverk_tatort()

# LOGIKEN FÖR ANALYSEN FÖLJER
# 
# enskilt_troskelvarde=15%
# totalt_troskelvarde=40%
# 
# LA: 
#   ingen enskild utpendlingsrelation över 15% och total utpendling under 40%
#   
#   Solitär: 
#   ingen enskild utpendlingsrelation över 15% och total utpendling över 40%
#   
#   Satellit:
#   minst en enskild utpendlingsrelation över 15% (oberoende av total utpendling)
# 
# CommonLA:
#   två orter som ej är LA enskilt (utan satelliter) men har störst pendling till varandra som är över 15%
# 
# PrimärtLA/SekundärtLA:
#   en satellit kan tillhöra två LA och dom rankas då baserat på utpendlingsandel.
# tätorten som störst andel av utpendling sker till klassas som primärtLA och näst störst blir sekundärtLA.
# - lägre nivåer (som tredje största) tas ej med.
# 

#   metod är att:
#   1. klassificera tätorter som LA/Solitär/Satelliter/CommonLA
#   2. beräkna vilka LA som satelliter tillhör samt primärt/sekundärt LA
#   3. köra ruttning mellan satelliter och LA (kör även ruttning mellan satelliter och solitärer och andra satelliter)
#   4. skapa kraftfältspolygoner genom buffer av resväg mellan satelliter för LA tätorter.
# 

# hämta data från sekretessdatabasen
# tabell med pendlingsrelationer
# tabell_pend_relation <- postgis_postgistabell_till_sf(
#   schema = "rutor",
#   tabell = "pend_relation_tatorter",
#   con = uppkoppling_db(service = "rd_geodata", db_name = "sekretess")
# )

pendling_kraftfalt <- function(
    tabell_pend_relation, # Dataframe med pendlingsrelationer hämtas förslagsvis på MONA
    ut_mapp = NA, # data ut ur funktionen
    gpkg_namn = NA, # namn på geopackage i utmappen
    enskilt_troskelvarde = 20, # enskilt troskelvärde
    totalt_troskelvarde = 35, # totalt troskelvärde
    primar_la_buffer = 2000, # buffer för primärt LA
    sekundar_la_buffer = 1000, # buffer för sekundärt LA
    gemensam_la_buffer = 5000, # har ej sett denna i resultat! 
    con = NA, # uppkoppling till PostGIS, krävs en connection, om inte anges finns et default värde till Region Dalarnas databas
    dist = 2000, # max avstånd till vägnätet
    skriv_till_gpkg = FALSE # TRUE om resultatet ska skrivas till geopackage, annars returneras som en lista
) { 
  library(readxl)
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  library(purrr)
  
  #source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # fyll i som ett argument till funktionen annars används default värden
  if (is.na(ut_mapp)) {
    ut_mapp <- utskriftsmapp()
  }
  # det namn du ger din geopackage annars används default värden
  if (is.na(gpkg_namn)) {
    gpkg_namn <- "kraftfält.gpkg"
  }
  # uppkoppling till din Postgres databas annars används default värden till Region Dalarnas databas med keyring()
  if (is.na(con)) {
    con <- uppkoppling_db(service = "rd_geodata")
  }
  
  dbExecute(con, "SET search_path TO grafer, public;") # denna är lite mystisk, men det funkar...
  
  # nedan 4 objekt kommer från funktionen skapa_vagnatverk_tatort() och ligger i schemat grafer
  kant_tabell <- "nvdb_noded" # en graf av vägnätet
  nod_tabell <- "grafer.nvdb_noded_vertices_pgr"# en graf av vägnätet
  
  kost_kol = "dist_cost" # kolumn i nvdb_noded med kostnad
  omvand_kost_kol = "dist_reverse_cost" # kolumn i nvdb_noded med omvänd kostnad
  
  # simpel funktion för att skriva postgis tabell till gpkg om skriv_till_gpkg=TRUE
  skriv_pg_tab_gpkg <- function(lyrname, ut_mapp, gpkg_namn, append=FALSE, delete_dsn=FALSE) {
    lyr <- st_read(con, lyrname)
    st_write(lyr, 
             file.path(ut_mapp, gpkg_namn),
             lyrname, 
             append=append,
             delete_dsn=delete_dsn
    )  
  }  
  
  # # läs in pendlingsdata
  # tabell_pend_relation <- read_csv(tabell_pend_relation, locale = locale(encoding = "ISO-8859-1"))%>%
  #   mutate(
  #     from_id = substr(from_id, 3, 11), # tar bort länskoden och tätortsnamnet från t.ex. 202084TC101 Avesta
  #     to_id = substr(to_id, 3, 11)      # tar bort länskoden och tätortsnamnet från t.ex. 202080TC108 Falun
  #   ) %>% 
  #   select(from_id, to_id, n) # väljer ut kolumner från_id, to_id och antal pendlare (n)
  # 
  if (!all(c("from_id", "to_id", "n") %in% colnames(tabell_pend_relation))) { # tabell_pend_relation_ruta måste innehålla dessa kolumner
    stop("`tabell_pend_relation` must contain the columns: 'from_id', 'to_id', 'n'.") # felmeddelande om inte dessa kolumner finns i tabell_pend_relation_ruta
  }
  
  # dbWriteTable(con, 'grafer.data', data, overwrite=TRUE, temporary=FALSE) # denna skriv till grafer.grafer.data !!
  dbWriteTable(con, 'data', tabell_pend_relation, overwrite=TRUE, temporary=FALSE) # skriver till grafer.data således schema grafer
  
  
  # hitta närmaste vertex i vägnätet till varje tätort
  query <- str_glue("CREATE TEMP TABLE tatort_vertex AS
                      SELECT t.tokod, e.id, e.dist
                      FROM tatort t
                      JOIN lateral(
                        SELECT id, e.geom <-> t.geom as dist
                          FROM {nod_tabell} e
                        ORDER BY t.geom <-> e.geom
                        LIMIT 1
                      ) AS e
                      ON true
                      WHERE dist < {dist};")
  dbExecute(con, "DROP TABLE IF EXISTS tatort_vertex;")
  dbExecute(con, query)
  
  query <- str_glue("CREATE TABLE commute_combinations AS
                      with temp_pendlings_data as (
                      	SELECT
                      	      from_id,
                      	      to_id,
                      	      n,
                      	      sum(n) OVER w AS totalworkers, /* totalt antal arbetare i from_id */
                      	      sum(n) FILTER (WHERE from_id=to_id) OVER w AS localworkers /* totalt antal lokala arbetare i from_id */
                        from grafer.data
                        WHERE from_id IN (SELECT tokod FROM tatort_vertex)
                        WINDOW w AS (PARTITION BY from_id)
                      )
                      SELECT
                            from_id,
                            to_id,
                            n,
                            totalworkers,
                            localworkers,
                            100*n::DECIMAL/totalworkers AS perc, /* %-andel arbetare från from_id till to_id*/
                            100-100*localworkers/totalworkers as perc_total_commuters, /* %-andel totala pendlare från from_id*/
                            case 
                            	when from_id=to_id then 0 /* relationen from_id till from_id får ranking 0 */
                            	else count(*) FILTER (WHERE from_id <> to_id) OVER wd /* ranking av relationer från mest pendlare till minst */
                            end AS ranking 
                      FROM temp_pendlings_data
                      WHERE to_id IN (SELECT tokod FROM tatort_vertex)
                      window wd AS (PARTITION BY from_id ORDER BY n DESC)
                      order by from_id, n desc;")
  dbExecute(con, "DROP TABLE IF EXISTS commute_combinations;")
  dbExecute(con, query)
  
  
  # la
  query <- str_glue("CREATE TEMP TABLE la AS
                  SELECT 
                      c.from_id AS id, c.totalworkers, 
                      c.localworkers, c.perc_total_commuters,
                      t.tobeteckn, t.lan, t.kommun, 
                      t.kommunnamn, t.geom 
                  FROM commute_combinations c
                  JOIN grafer.tatort t ON t.tokod = c.from_id
                  WHERE ranking = 1 
                    AND perc <= {enskilt_troskelvarde} 
                    AND perc_total_commuters <= {totalt_troskelvarde};")
  dbExecute(con, "DROP TABLE IF EXISTS la;")
  dbExecute(con, query)
  
  # skriv till geopackage
  # skriv_pg_tab_gpkg(str_glue("la"), ut_mapp, gpkg_namn)
  
  # solitär
  query <- str_glue("CREATE TEMP TABLE solitary AS
                  select 
                    	c.from_id as id, c.totalworkers, 
                    	c.localworkers, c.perc_total_commuters,
                    	t.tobeteckn, t.lan,t.kommun, 
                    	t.kommunnamn,t.geom 
                    from commute_combinations c
                    join grafer.tatort t on t.tokod=c.from_id
                  where ranking=1 and perc <= {enskilt_troskelvarde}
                  and perc_total_commuters > {totalt_troskelvarde}
                  ;")
  dbExecute(con, "DROP TABLE IF EXISTS solitary;")
  dbExecute(con, query)
  # skriv till geopackage
  # skriv_pg_tab_gpkg(str_glue("solitary"), ut_mapp, gpkg_namn)
  
  
  # common la
  query <- str_glue("CREATE TEMP TABLE common_la AS
                  WITH sats AS (
                     SELECT *
                     FROM commute_combinations
                     WHERE ranking > 1 AND perc > {enskilt_troskelvarde}
                  ), common_la_cte AS (
                     SELECT a.*
                     FROM sats a
                     JOIN sats b ON a.from_id = b.to_id AND a.to_id = b.from_id
                  )
                  SELECT
                     c.from_id AS id, c.to_id AS common_la, 
                     c.totalworkers, c.localworkers, c.perc_total_commuters,
                     t.tobeteckn, t.lan, t.kommun, t.kommunnamn, t.geom
                  FROM common_la_cte c
                  JOIN grafer.tatort t ON t.tokod = c.from_id;")
  
  dbExecute(con, "DROP TABLE IF EXISTS common_la;")
  dbExecute(con, query)
  # skriv till geopackage
  # skriv_pg_tab_gpkg(str_glue("common_la"), ut_mapp, gpkg_namn)
  
  # satelit
  query <- str_glue("CREATE TEMP TABLE satellites AS
                  WITH sats AS (
                      SELECT *
                      FROM commute_combinations
                      WHERE ranking IN (1, 2) 
                        AND perc > {enskilt_troskelvarde}
                        AND from_id NOT IN (SELECT id FROM common_la)
                  ), la_id AS (
                      SELECT id FROM la 
                      UNION 
                      SELECT id FROM common_la
                  ), agg AS (
                      SELECT 
                          from_id, 
                          array_agg(to_id ORDER BY ranking) AS id_array_all,
                          array_agg(to_id ORDER BY ranking) FILTER (WHERE to_id IN (SELECT id FROM la_id)) AS id_array
                      FROM sats
                      GROUP BY from_id
                  )
                  SELECT 
                      a.from_id AS id,
                      id_array[1] AS primary_la, 
                      id_array[2] AS secondary_la,
                      id_array_all[1] AS primary_destination,
                      id_array_all[2] AS secondary_destination,
                      s.totalworkers, s.localworkers, s.perc_total_commuters,
                      t.tobeteckn, t.lan, t.kommun, t.kommunnamn, t.geom
                  FROM agg a
                  JOIN sats s ON a.from_id = s.from_id
                  JOIN grafer.tatort t ON t.tokod = s.from_id;")
  dbExecute(con, "DROP TABLE IF EXISTS satellites;")
  dbExecute(con, query)
  
  # skriv till geopackage
  # skriv_pg_tab_gpkg(str_glue("satellites"), ut_mapp, gpkg_namn)
  
  # ruttningsanalys
  
  # tabell med allar rutter som ska köras (satelliter+commonLA)
  query <- str_glue("CREATE TEMP TABLE route_combinations AS
                    with sats as (
                    	select from_id, to_id, ranking
                    	from commute_combinations
                    	where ranking in (1,2) 
                    		and perc > {enskilt_troskelvarde}
                    )
                    select s.*, f.id as start_vid, t.id as end_vid
                    from sats s
                    JOIN tatort_vertex f ON f.tokod=s.from_id
                    JOIN tatort_vertex t ON t.tokod=s.to_id
                  ;")
  dbExecute(con, "DROP TABLE IF EXISTS route_combinations;")
  dbExecute(con, query)
  
  
  query <- str_glue("CREATE TEMP TABLE routes AS
                  WITH astar AS( 
                    SELECT * FROM pgr_aStar(
                      'SELECT id, source, target,
                              {kost_kol} AS cost,
                              {omvand_kost_kol} AS reverse_cost,
                              x1, y1, x2, y2
                  	   FROM grafer.{kant_tabell}',
                      'SELECT 
                        start_vid AS source, 
                        end_vid AS target
                      FROM route_combinations'
                    )
                  ), paths AS(
                    SELECT 
                      a.start_vid, 
                      a.end_vid,
                      sum(a.cost) AS cost,
                      max(a.agg_cost) AS agg_cost,
                      st_LineMerge(st_union(r.geom)) AS geom
                    FROM astar a JOIN grafer.{kant_tabell} r ON a.edge=r.id
                    GROUP BY a.start_vid, a.end_vid
                  )
                  SELECT c.*, p.agg_cost, p.geom
                  FROM route_combinations c 
                  JOIN paths p ON c.start_vid=p.start_vid 
                    and c.end_vid=p.end_vid")
  dbExecute(con, "DROP TABLE IF EXISTS routes;")
  dbExecute(con, query)
  # skriv_pg_tab_gpkg(str_glue("routes"), ut_mapp, gpkg_namn)
  
  
  # rutter till secondary LA
  query <- str_glue("CREATE TEMP TABLE secla_areas AS
          select
            to_id, MAX(ranking) AS max_ranking,
            st_buffer(st_collect(r.geom), {sekundar_la_buffer}) as geom
          from satellites s
          join routes r on s.id=r.from_id and s.secondary_la=r.to_id
          group by to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS secla_areas;")
  dbExecute(con, query)
  # skriv_pg_tab_gpkg(str_glue("secla_areas"), ut_mapp, gpkg_namn)
  
  # rutter till primary LA
  query <- str_glue("CREATE TEMP TABLE primla_areas AS
          select
            to_id, max(ranking),
            st_buffer(st_collect(r.geom), {primar_la_buffer}) as geom
          from satellites s
          join routes r on s.id=r.from_id and s.primary_la=r.to_id
          group by to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS primla_areas;")
  dbExecute(con, query)
  # skriv_pg_tab_gpkg(str_glue("primla_areas"), ut_mapp, gpkg_namn)
  
  # rutter mellan CommonLA
  query <- str_glue("CREATE TEMP TABLE commonla_areas AS
          select
            from_id, to_id, ranking,
            st_buffer(r.geom, {gemensam_la_buffer}) as geom
          from common_la s
          join routes r on s.id=r.from_id and s.common_la=r.to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS commonla_areas;")
  dbExecute(con, query)
  # skriv_pg_tab_gpkg(str_glue("commonla_areas"), ut_mapp, gpkg_namn)
  
  # Define layers to process
  layers <- c(
    "la", 
    "solitary", 
    "common_la", 
    "satellites", 
    "routes", 
    "secla_areas", 
    "primla_areas", 
    "commonla_areas"
  )
  
  # Write layers to GeoPackage or return as a list
  if (skriv_till_gpkg) {
    # Write only non-empty layers to the GeoPackage
    purrr::walk(
      layers,
      ~ {
        tryCatch({
          # Check if the layer exists in the database
          query <- str_glue("SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_name = '{str_remove(.x, 'grafer.')}')")
          layer_exists <- dbGetQuery(con, query)[1, 1]
          
          if (layer_exists) {
            lyr <- st_read(con, query = str_glue("SELECT * FROM {.x}"))
            if (nrow(lyr) > 0) { # Only write if the layer is non-empty
              skriv_pg_tab_gpkg(
                lyrname = str_remove(.x, "grafer."), # Remove schema prefix for GPKG
                ut_mapp = ut_mapp,
                gpkg_namn = gpkg_namn,
                append = TRUE # Append layers to the same GeoPackage
              )
            } else {
              message(glue("Layer `{.x}` is empty, skipping..."))
            }
          } else {
            message(glue("Layer `{.x}` does not exist, skipping..."))
          }
        }, error = function(e) {
          message(glue("Error processing layer `{.x}`: {e$message}"))
        })
      }
    )
    message(glue("Finished writing non-empty layers to {file.path(ut_mapp, gpkg_namn)}"))
  } else {
    # Create a named list with all layers, include NULL for missing or empty layers
    results <- purrr::map(
      layers,
      ~ {
        tryCatch({
          query <- str_glue("SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_name = '{str_remove(.x, 'grafer.')}')")
          layer_exists <- dbGetQuery(con, query)[1, 1]
          
          if (layer_exists) {
            lyr <- st_read(con, query = str_glue("SELECT * FROM {.x}"))
            if (nrow(lyr) > 0) {
              return(lyr) # Return non-empty layer
            } else {
              message(glue("Layer `{.x}` is empty, skipping..."))
              return(NULL)
            }
          } else {
            message(glue("Layer `{.x}` does not exist, skipping..."))
            return(NULL)
          }
        }, error = function(e) {
          message(glue("Error processing layer `{.x}`: {e$message}"))
          return(NULL)
        })
      }
    )
    # Name results list with the layer names (sans schema)
    names(results) <- str_remove(layers, "grafer.")
    return(results)
  }
}

# # # # Exempel på hur funktionen kan användas
# # # 
# kraftfalt_15_35 <- pendling_kraftfalt(tabell_pend_relation = tabell_pend_relation, 
#                                       enskilt_troskelvarde = 15, 
#                                       totalt_troskelvarde = 35)
# #
# # # Define custom colors for better readability
# road_color <- "#4D4D4D" # Dark gray for roads
# area_color <- "green" # Green for areas
# point_color_primary <- "red" # Red for primary points
# point_color_secondary <- "orange" # Orange for secondary points
# #
# # # Update mapview layers
# mapview::mapview(kraftfalt_15_35$la, col.regions = area_color, alpha.regions = 0.5, cex = 6) +
#   mapview::mapview(kraftfalt_15_35$solitary, col.regions = point_color_secondary, cex = 4) +
#   mapview::mapview(kraftfalt_15_35$routes, color = road_color, lwd = 2)  +
#   mapview::mapview(kraftfalt_15_35$satellites, col.regions = point_color_primary, cex = 3)+
#   mapview::mapview(kraftfalt_15_35$primla_areas, col.regions = "blue", alpha.regions = 0.3)+
#   mapview::mapview(kraftfalt_15_35$secla_areas, col.regions = "orange", alpha.regions = 0.4)

# ------------------- pendlingsnätverk -------------------------

# Funktionen skapar ett pendlingsnätverk och beräknar det sammanlagda antalet pendlare per vägsträcka. 
# Resultatet kan sparas som ett GeoPackage eller returneras som ett R-objekt. 
# Skriptet kräver en tabell med pendlingsrelationer och vägnät hämtas från en PostGIS-databas.

# För att funktionen ska fungera, kör först funktionen skapa_vagnatverk_tatort() som finns i func_GIS. Denna funktion skapar nödvändiga tabeller i PostGIS.
# en uppdatering kommer att göras för att skapa dessa tabeller direkt i funktionen eller att det ska gå at läsa in sf_objekt istället för att använda PostGIS.

# source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
# skapa_vagnatverk_tatort()

# tabell med pendlingsrelationer
# tabell_pend_relation <- postgis_postgistabell_till_sf(
#   schema = "rutor",
#   tabell = "pend_relation_tatorter",
#   con = uppkoppling_db(service = "rd_geodata", db_name = "sekretess")
# )

pendling_natverk <- function(
    tabell_pend_relation, # Dataframe med pendlingsrelationer hämtas förslagsvis på MONA
    con = NA, # uppkoppling till PostGIS, krävs en connection, om inte anges finns et default värde till Region Dalarnas databas
    dist = 2000, # max avstånd till vägnätet
    skriv_till_gpkg = FALSE, # flagga för att skriva till geopackage
    ut_mapp = NA, # mapp att skriva till
    gpkg_namn = NA # namn på geopackage
) { 
  
  library(readxl)
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  
  #source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # Hantera NA i parametrar
  # där du eventuellt sparar data lokalt
  if (is.na(ut_mapp)) {
    ut_mapp <- utskriftsmapp()
  }
  # det namn du ger din geopackage
  if (is.na(gpkg_namn)) {
    gpkg_namn <- "pendling_natverk.gpkg"
  }
  # uppkoppling till din Postgres databas
  if (is.na(con)) {
    con <- uppkoppling_db(service = "rd_geodata")
  }
  
  dbExecute(con, "SET search_path TO grafer, public;") # denna är lite mystisk, men det funkar...
  
  # vägnätet skapat med skapa_vagnatverk_tatort() ligger i schema grafer
  kant_tabell <- "nvdb_noded" 
  nod_tabell <- "grafer.nvdb_noded_vertices_pgr" 
  
  kost_kol = "dist_cost" # kolumn i nvdb_noded med kostnad
  omvand_kost_kol = "dist_reverse_cost" # kolumn i nvdb_noded med omvänd kostnad
  
  # # läs in pendlingsdata
  # tabell_pend_relation <- read_csv(tabell_pend_relation, locale = locale(encoding = "ISO-8859-1"))%>%
  #   mutate(
  #     from_id = substr(from_id, 3, 11), # tar bort länskoden och tätortsnamnet från t.ex. 202084TC101 Avesta
  #     to_id = substr(to_id, 3, 11)      # tar bort länskoden och tätortsnamnet från t.ex. 202080TC108 Falun
  #   ) %>% 
  #   select(from_id, to_id, n)
  # 
  if (!all(c("from_id", "to_id", "n") %in% colnames(tabell_pend_relation))) { # tabell_pend_relation_ruta måste innehålla dessa kolumner
    stop("`tabell_pend_relation` must contain the columns: 'from_id', 'to_id', 'n'.") # felmeddelande om inte dessa kolumner finns i tabell_pend_relation_ruta
  }
  
  # dbWriteTable(con, 'grafer.data', data, overwrite=TRUE, temporary=FALSE) # denna skriv till grafer.grafer.data !!
  dbWriteTable(con, 'data', tabell_pend_relation, overwrite=TRUE, temporary=FALSE) # skriver till grafer.data således schema grafer ändra till TEMP
  
  
  # hitta närmaste vertex i vägnätet till varje tätort
  query <- str_glue("CREATE TEMP TABLE tatort_vertex AS
                      SELECT t.tokod, e.id, e.dist
                      FROM tatort t
                      JOIN lateral(
                        SELECT id, e.geom <-> t.geom as dist
                          FROM {nod_tabell} e
                        ORDER BY t.geom <-> e.geom
                        LIMIT 1
                      ) AS e
                      ON true
                      WHERE dist < {dist};")
  dbExecute(con, "DROP TABLE IF EXISTS tatort_vertex;")
  dbExecute(con, query)
  
  # skapa tabell med relationer för routing
  query <- ("CREATE TEMP TABLE combinations AS
            SELECT c.*, t.id AS start_vid, f.id AS end_vid 
            FROM data c
            JOIN tatort_vertex t ON c.from_id = t.tokod
            JOIN tatort_vertex f ON c.to_id = f.tokod
            WHERE c.from_id <> c.to_id;")
  dbExecute(con, "DROP TABLE IF EXISTS combinations;")
  dbExecute(con, query)
  
  # kör routing och samla ihop resultatet
  query <- str_glue("WITH astar AS( 
                      SELECT * FROM pgr_aStar(
                        'SELECT id, source, target, 
                                {kost_kol} AS cost, 
                                {omvand_kost_kol} AS reverse_cost,
                                x1, y1, x2, y2
                    	  FROM {kant_tabell}',
                        'SELECT 
                          start_vid AS source,
                          end_vid AS target
                        FROM combinations'
                      )
                    ), edges AS(
                      SELECT edge as edge_id, sum(n) AS antal_pend
                      FROM astar a 
                      JOIN combinations d ON a.start_vid = d.start_vid
                                  AND a.end_vid = d.end_vid
                      WHERE edge <> -1
                      GROUP BY edge
                    )
                    SELECT * 
                    FROM edges e
                      JOIN {kant_tabell} r ON e.edge_id = r.id;")
  
  natverk <- st_read(con, query = query)
  
  # Check if output is to a geopackage or as R object
  if (skriv_till_gpkg) {
    # Write to geopackage
    output_path <- file.path(ut_mapp, gpkg_namn)
    st_write(natverk, output_path, delete_dsn = TRUE)
    message(glue("Results written to {output_path}"))
  } else {
    # Return the natverk as an R object
    return(natverk)
  }
}


# Exempel på användning:
# natverk <- pendling_natverk(tabell_pend_relation = tabell_pend_relation, skriv_till_gpkg = FALSE)
# #
# # # Load necessary library
# # library(mapview)
# 
# # Define a custom color palette
# custom_colors <- colorRampPalette(c("lightblue", "blue", "lightgreen", "green", "yellow", "orange", "red", "darkred"))
# 
# # Apply the mapview with custom colors
# mapview::mapview(
#   natverk,
#   zcol = "antal_pend",           # Column to control the color gradient
#   lwd = "antal_pend",            # Column to control line width
#   alpha = 0.5,                   # Transparency
#   color = custom_colors    # Apply the custom color palette
# )



# ------------------- in och utpendling på ruta -------------------------

# funktionen är skapad av Henrik Aldén från SWECOS skript G:/skript/gis/sweco_dec_2022/orginalskript/del3_rut_pendling.r

# två versioner av skriptet finns som argument. Postgis versionen är snabbare vid stora dataset

# testa med system.time() för att jämföra exekveringstid på r och pg

# skriptet funkar med vector-data, alltså punkter, linjer och (således inte endast med) polygoner

# exempel på polygoner
# polygon <- hamta_karta(karttyp = "deso", regionkoder = 2085) %>%
#   group_by(kommun) %>%
#   summarise(geometry = st_union(geometry))

# polygon <- st_read("G:/skript/gis/sweco_dec_2022/godtycklig_polygon_test.gpkg")
# 
# polygon <- st_read("G:/skript/gis/sweco_dec_2022/mora_lassarett_test.gpkg")
# 

# hämta pendlingsdata från sekretessdatabasen
# tabell_pend_relation_ruta <- postgis_postgistabell_till_sf(
#   schema = "rutor",
#   tabell = "pend_relation_rutor_1000m",
#   con = uppkoppling_db(service = "rd_geodata", db_name = "sekretess")
# )

# rutor <- postgis_postgistabell_till_sf(
#   schema = "rutor",
#   tabell = "dag_natt_bef_rutor_1000m",
#   con = uppkoppling_db(service = "rd_geodata", db_name = "sekretess")
# )

pendling_ruta <- function(version = c("PostGIS", "R"), # Måste välja mellan PostGIS och R
                          con = NA, # Om PostGIS, krävs en connection
                          tabell_pend_relation_ruta, # Dataframe med pendlingsrelationer
                          rutor, # Grid
                          polygon,  # lägg till valfritt sf_objekt med geometri; punkt, linje eller polygon
                          ut_mapp = NA, # Optional output folder
                          grid_epsg = 3006, # Optional EPSG code for the grid
                          skriv_till_gpkg = FALSE, # Optional flag to write to GeoPackage
                          gpkg_namn = NA) # Optional name for GeoPackage file
{
  
  # Load necessary libraries
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  
  #source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # Validate arguments
  version <- match.arg(version, choices = c("R", "PostGIS")) # verkar vara antingen eller!!
  
  
  
  # Error handling: check required inputs
  if (!inherits(rutor, "sf")) stop("`rutor` must be an sf object.") # rutor måste vara sf object
  if (!inherits(polygon, "sf")) stop("`polygon` must be an sf object.") # polygon måste vara sf object
  if (!all(c("boruta", "arbruta", "antalpend") %in% colnames(tabell_pend_relation_ruta))) { # tabell_pend_relation_ruta måste innehålla dessa kolumner
    stop("`tabell_pend_relation_ruta` must contain the columns: 'boruta', 'arbruta', 'antalpend'.") # felmeddelande om inte dessa kolumner finns i tabell_pend_relation_ruta
  }
  
  if (is.na(con)) {
    if (exists("uppkoppling_db")) {
      con <- uppkoppling_db(service = "rd_geodata")
    } else {
      stop("No connection provided and `uppkoppling_db` is not available. Please specify a valid connection.")
    }
  }
  
  if (version == "PostGIS" && (is.null(con) || !inherits(con, "PqConnection"))) {
    stop("For PostGIS version, a valid `con` database connection must be provided.")
  }
  
  if (!dbIsValid(con)) {
    stop("Database connection is not valid. Please check your connection settings.")
  }
  
  
  # Set default values for optional parameters
  if (is.na(ut_mapp)) {
    ut_mapp <- utskriftsmapp()
  }
  if (is.na(gpkg_namn)) {
    gpkg_namn <- "pendling_ruta.gpkg"
  }
  
  # Ensure CRS consistency
  if (st_crs(polygon) != st_crs(rutor)) {
    polygon <- st_transform(polygon, st_crs(rutor))
  }
  
  # Intersect polygonen med rutor
  vald_polygon <- st_intersection(rutor, polygon)
  
  if (nrow(vald_polygon) == 0) stop("Ingen intersection mellan polygon och rutor.")
  
  # Branch based on the utvalda_rutor version
  if (version == "PostGIS") {
    dbWriteTable(con, "ruta", rutor, overwrite = TRUE, temporary = TRUE)
    dbWriteTable(con, "rutpendling", tabell_pend_relation_ruta, overwrite = TRUE, temporary = TRUE)
    
    utvalda_rutor <- st_read(con, layer = "ruta") %>% st_filter(vald_polygon)
    utvalda_rutor_ids <- paste(utvalda_rutor$rut_id, collapse = ", ")
    
    # In-commuting
    query <- str_glue("WITH commuters_in AS (
                      SELECT
                        boruta, 
                        sum(antalpend) AS total
                      FROM rutpendling
                      WHERE arbruta::BIGINT IN ({utvalda_rutor_ids}) 
                        AND boruta::BIGINT NOT IN ({utvalda_rutor_ids}) 
                      GROUP BY boruta
                      ) 
                        SELECT c.*, r.geom 
                        FROM commuters_in c 
                          JOIN ruta r ON c.boruta = r.rut_id")
    in_pendling <- st_read(con, query = query)
    
    # Out-commuting
    query <- str_glue("WITH commuters_out AS (
                      SELECT
                        arbruta,
                        sum(antalpend) AS total
                      FROM rutpendling
                      WHERE boruta::BIGINT IN ({utvalda_rutor_ids})
                        AND arbruta::BIGINT NOT IN ({utvalda_rutor_ids}) 
                      GROUP BY arbruta
                      )
                      SELECT c.*, r.geom 
                      FROM commuters_out c 
                        JOIN ruta r ON c.arbruta = r.rut_id")
    ut_pendling <- st_read(con, query = query)
    
    if (skriv_till_gpkg) {
      st_write(utvalda_rutor, file.path(ut_mapp, "rut_pendling_pg.gpkg"), "område", delete_dsn = TRUE, append = FALSE)
      st_write(in_pendling, file.path(ut_mapp, "rut_pendling_pg.gpkg"), "in_pend", append = FALSE)
      st_write(ut_pendling, file.path(ut_mapp, "rut_pendling_pg.gpkg"), "ut_pend", append = FALSE)
    }
    
    return(list("utvalda_rutor" = utvalda_rutor, "in_pendling" = in_pendling, "ut_pendling" = ut_pendling))
    
  } else if (version == "R") {
    utvalda_rutor <- st_filter(rutor, vald_polygon)
    
    # Define helper functions
    pendlare_fran_utvalda_rutor <- function(pendlings_data) {
      pendlings_data %>%
        group_by(boruta) %>%
        summarise(pendlare_fran = sum(antalpend))
    }
    pendlare_till_utvalda_rutor <- function(pendlings_data) {
      pendlings_data %>%
        group_by(arbruta) %>%
        summarise(pendlare_till = sum(antalpend))
    }
    filter_med_utvalda_rutor <- function(pendlings_data, utvalda_rutor) {
      fran_utvalda_rutor <- filter(pendlings_data,
                                   (boruta %in% utvalda_rutor$rut_id) &
                                     !(arbruta %in% utvalda_rutor$rut_id))
      till_utvalda_rutor <- filter(pendlings_data,
                                   (arbruta %in% utvalda_rutor$rut_id) &
                                     !(boruta %in% utvalda_rutor$rut_id))
      list("fran_utvalda_rutor" = fran_utvalda_rutor, "till_utvalda_rutor" = till_utvalda_rutor)
    }
    
    data <- filter_med_utvalda_rutor(tabell_pend_relation_ruta, utvalda_rutor)
    from_c <- pendlare_fran_utvalda_rutor(data$till_utvalda_rutor)
    to_c <- pendlare_till_utvalda_rutor(data$fran_utvalda_rutor)
    
    full_table <- full_join(from_c, to_c, by = c("boruta" = "arbruta")) %>%
      rename("rut_id" = "boruta")
    result <- merge(rutor, full_table, by = "rut_id")
    
    in_pendling <- filter(result, !is.na(pendlare_fran)) %>% dplyr::select(-pendlare_till)
    ut_pendling <- filter(result, !is.na(pendlare_till)) %>% dplyr::select(-pendlare_fran)
    
    if (skriv_till_gpkg) {
      st_write(utvalda_rutor, file.path(ut_mapp, "rut_pendlingR.gpkg"), "område", delete_dsn = TRUE, append = FALSE)
      st_write(in_pendling, file.path(ut_mapp, "rut_pendlingR.gpkg"), "in_pend", append = FALSE)
      st_write(ut_pendling, file.path(ut_mapp, "rut_pendlingR.gpkg"), "ut_pend", append = FALSE)
    }
    
    return(list(
      "utvalda_rutor" = if (exists("utvalda_rutor") && nrow(utvalda_rutor) > 0) utvalda_rutor else NULL,
      "in_pendling" = if (exists("in_pendling") && nrow(in_pendling) > 0) in_pendling else NULL,
      "ut_pendling" = if (exists("ut_pendling") && nrow(ut_pendling) > 0) ut_pendling else NULL
    ))
  }
}

# ================================= Övriga funktioner ================================================


get_password_tk <- function(title = "Inloggning - lösenord", prompt = "Skriv lösenord:") {
  tt <- tktoplevel()
  tkwm.title(tt, title)
  
  # Fönsterstorlek
  width <- 320
  height <- 130
  
  # Beräkna position för centrering
  screen_width  <- as.integer(tkwinfo("screenwidth", tt))
  screen_height <- as.integer(tkwinfo("screenheight", tt))
  xpos <- as.integer((screen_width - width) / 2)
  ypos <- as.integer((screen_height - height) / 2)
  tkwm.geometry(tt, sprintf("%dx%d+%d+%d", width, height, xpos, ypos))
  
  # Ram med marginaler
  frame <- tkframe(tt, padx = 15, pady = 10)
  tkpack(frame, expand = TRUE)
  
  # Etikett
  label <- tklabel(frame, text = prompt)
  tkpack(label, pady = 5)
  
  # Maskerad inmatning
  pass_var <- tclVar("")
  entry <- tkentry(frame, textvariable = pass_var, show = "*", width = 30)
  tkpack(entry, pady = 5)
  
  # OK-knapp
  ok_clicked <- FALSE
  ok_button <- tkbutton(frame, text = "OK", command = function() {
    ok_clicked <<- TRUE
    tkdestroy(tt)
  })
  tkpack(ok_button, pady = 5)
  
  tkbind(entry, "<Return>", function() {
    ok_clicked <<- TRUE
    tkdestroy(tt)
  })
  
  # Sätt fokus och markör i inmatningsfältet direkt
  tkfocus(entry)
  
  # Vänta tills fönstret stängs
  tkwait.window(tt)
  
  # Om användaren klickade på krysset – returnera tom sträng
  if (!ok_clicked) {
    return("")
  }
  
  tclvalue(pass_var)
}


keyring_lagg_till_inloggning <- function(keyring_service) {
  
  # ============================================================================================
  #
  #  Skript för att lägga in inloggningsuppgifter i keyring. 
  #  Lösenord som sparas i operativsystemets "credential store", vilket betyder att inga 
  #  lösenord sparas som text på datorn. OBS! Skicka med lösenord med funktioner
  #  men skriv inte ut dem i konsolen eller spara dem i variabler.
  #
  #  OBS! Funktionen get_password_tk() ovan krävs för att köra funktionen.
  #
  #  Författare: Peter Möller, Region Dalarna
  #
  # ============================================================================================
  
  
  # --- Kontrollera att keyring är installerat ---
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("Paketet 'keyring' måste installeras innan funktionen kan köras.")
  }
  if (!requireNamespace("svDialogs", quietly = TRUE)) {
    stop("Paketet 'svDialogs' måste installeras innan funktionen kan köras.")
  }
  
  library(keyring)
  library(svDialogs)
  
  keyring_lista <- key_list()
  avsluta <- FALSE
  svar <- "tomt"
  
    # Be om användarnamn (förifyllt)
    username <- dlg_input(
      "Skriv användarnamn:",
      default = Sys.info()[["user"]],
      title = "Inloggning"  #paste0("Inloggning keyring service: ", keyring_service)
    )$res
    
    if (is.null(username) || username == "" || length(username) == 0) {
      message("Ingen inloggning skapad (användarnamn saknas).")
      return(invisible(NULL))
    }
    
    # --- Kontrollera om inloggning redan finns ---
    if(
      tryCatch({
        keyring::key_get(keyring_service, username)
        TRUE
      }, error = function(e) FALSE)
    ) {
      svar <- dlg_message(
        paste0("Det finns redan en inloggning som heter '", keyring_service,
               "' med användarnamn '", username, "'. Vill du ta bort den och lägga in den på nytt?"),
        type = "yesno"
      )$res
      
      # om användar klickar på no så avslutas funktionen utan att göra något
      if (tolower(svar) == "no") avsluta <- TRUE
    }
    
    
    # --- Om användaren vill fortsätta ---
    if (!avsluta) {
    
    # Be om lösenord (maskerat)
    
    # Exempel
    password <- get_password_tk()               # vi använder ett tcltk-fönster för att kunna skriva lösenord med "*"
    # password <- dlg_input(
    #   "Skriv lösenord:",
    #   hide = TRUE,
    #   title = "Inloggning geodatabasen"
    # )$res
    
    if (is.null(password) || password == "" || length(password) == 0) {
      message("Ingen inloggning skapad (lösenord saknas).")
      return(invisible(NULL))
    }
    
    if (tolower(svar) == "yes") {
      # ta bort de befintliga uppgifterna om det finns några
      key_delete(
        service = keyring_service,
        username = key_list(service = keyring_service)$username[1]
      )
    }
    
    # Spara de nya till keyring
    key_set_with_value(
      service = keyring_service,
      username = username,
      password = password
    )
    
    dlg_message(
      paste0("Inloggning för '", keyring_service, "' och användarnamn '",
             username, "' har sparats i keyring."),
      type = "ok"
    )
  }
}


adresser_inv_reg_folke_bearbeta <- function(skickad_df) {
  
  #Lagt till "," i vektorn för tecken att ta bort /leomik 20240314
  ta_bort_vekt <- c(" nb", " bv", " uv", " 1tr", " 1 tr", ",1tr", ", 1 tr", " 2 tr", " 2tr", " vån1", " vån 1", 
                    " vån2", " vån 2", " vån3", " vån 3", " 3tr", " 3 tr", " 4tr", " 4 tr", " vån0", " vån 0",
                    " 6tr", " 6 tr", " 1/2tr", " 1/2 tr", " 2:a", " lgh", " läg", ",")
  
  ta_bort_vekt_or = paste0(ta_bort_vekt, collapse="|")                   # vi lägger hela ta_bort_vekt som en or-sats med samtliga element
  
  # här städar vi lite men ändrar inte originalkolumnerna utan lägger till adress_join som är den adress vi joinar på samt även postnr_join som vi också joinar på
  #inv_adresser_df <- if (str_sub(filsokvag, nchar(filsokvag)-2, nchar(filsokvag)) == "csv") fread(filsokvag) else read_xlsx(filsokvag)
  inv_adresser_df <- skickad_df %>% 
    mutate(adress_join = adress %>% tolower(),                              # gör om alla till gemener, så slipper vi fel för att man kör med olika gemener och versaler
           adress_join = adress_join %>% str_replace("(?<=[a-z])(?=\\d)", " "),                      # lägg till ett mellanslag mellan bokstäver och siffror som sitter ihop
           adress_join = adress_join %>% str_replace("(?<=\\d)(?=[a-z])", " "),                     # lägg till ett mellanslag mellan siffror och bokstäver som sitter ihop
           adress_join = adress_join %>% str_squish(),
           ta_bort_pos = str_locate(adress_join, ta_bort_vekt_or)[,1] - 1,                            # först hittar vi postition för en träff med någon av elementen i ta_bort_vekt, och tar minus 1 för att hitta slutet på det vi vill behålla (alltså allt från och med ta bort-mönstret och till höger ska bort)
           adress_join = ifelse(!is.na(ta_bort_pos), str_sub(adress_join, 1, ta_bort_pos), adress_join)
           # ,
           # uttags_datum = uttags_datum
    ) %>%      # därefter tar vi bort allt från söksträngen och till höger med str_sub och med ta_bort_pos som vi skapade ovan
    select(-ta_bort_pos)
  
  return(inv_adresser_df)
  
} # slut funktion

library(sf)
library(dplyr)

# Funktion som skapar eller fyller på ett sf-objekt
skapa_punkt_sf_av_koordinatpar <- function(koordinater, malpunktsnamn, sf_obj = NULL, vald_crs = 4326) {
  # Kontrollera att antalet koordinater matchar antalet målpunktsnamn
  if (length(koordinater) != length(malpunktsnamn)) {
    stop("Antalet koordinater måste vara samma som antalet målpunktsnamn.")
  }
  
  # Skapa en dataram från koordinater och namn
  malpunkter_df <- data.frame(
    id = seq_len(length(koordinater)),
    malpunkt_namn = malpunktsnamn,
    stringsAsFactors = FALSE
  )
  
  # Extrahera latitud och longitud från koordinater
  koordinat_matrix <- purrr::map(koordinater, ~ as.numeric(strsplit(.x, ",\\s*")[[1]])) %>% 
    do.call(rbind, .)
  
  # Lägg till latitud och longitud i dataramen
  malpunkter_df$lat <- koordinat_matrix[, 1]
  malpunkter_df$lon <- koordinat_matrix[, 2]
  
  # Konvertera till sf-objekt med CRS WGS84 (EPSG:4326)
  malpunkter_sf <- st_as_sf(malpunkter_df, coords = c("lon", "lat"), crs = vald_crs)
  
  # Om ett sf-objekt skickas med, fyll på det, annars returnera det nya
  if (!is.null(sf_obj)) {
    if (!inherits(sf_obj, "sf")) {
      stop("Det medskickade objektet måste vara ett sf-objekt.")
    }
    return(bind_rows(sf_obj, malpunkter_sf))
  } else {
    return(malpunkter_sf)
  }
}


gdb_extrahera_kolumnnamn_per_gislager <- function(gdb_sokvag,
                                                  tabort_paranteser = TRUE,
                                                  byt_ut_slash_mot_understreck = TRUE,
                                                  byt_ut_mellanslag_mot_understreck = TRUE,
                                                  enbart_gemener = TRUE,
                                                  byt_ut_svenska_tecken = TRUE,
                                                  nytt_namn_id_kol = "nvdb_id",         # NA = behåll gamla, annat värde blir namnet på kolumnen
                                                  nytt_namn_geo_kol = "geom"
) {
  
  # En funktion för att extrahera kolumnnamn ur de gis-lager som finns i en .gdb-fil, 
  # dvs. en ESRI Geodatabase vilket är det format som Trafikverket levererar sina 
  # homogeniserade lager i. Och eftersom st_read() trunkerar kolumnnamn så kan man med
  # denna funktion extrahera vettiga kolumnnamn i en namnsatt vektor som kan användas till 
  # att döpa om ett sf-objekt som lästs in från ESRI Geodatabase.
  
  # definiera en funktion som används nedan
  hitta_ogrinfo <- function() {
    program_files <- Sys.getenv("ProgramFiles")
    
    # Lista kataloger under Program Files och filtrera på de som börjar med QGIS
    qgis_dirs <- list.dirs(program_files, recursive = FALSE) %>%
      keep(~ str_starts(basename(.x), "QGIS"))
    
    # Kontrollera varje QGIS-katalog om ogrinfo.exe finns i bin-mappen
    ogrinfo_path <- qgis_dirs %>%
      map(~ file.path(.x, "bin", "ogrinfo.exe")) %>%
      #keep(file.exists()) %>%
      first()
    
    # Returnera sökväg eller NULL om ingen fil hittades
    ogrinfo_path %||% NULL
  } # slut funktion
  
  # Vi kollar om ogrinfo.exe finns tillgängligt och om inte tittar vi efter
  # QGIS och om ogrinfo.exe finns där, hittas den inte där stoppas funktionen
  if (Sys.which("ogrinfo") == "") {
    
    ogr_sokvag <- hitta_ogrinfo()
    if (is.null(hitta_ogrinfo())) stop("GDAL-programvaran ogrinfo.exe krävs för att köra denna funktion. Installera programvaran om du vill använda funktionen.")  
  } else ogr_sokvag <- "ogrinfo"
  
  
  # Steg 1: Hämta namnet på alla gis-lager som finns i geodatabasen
  alla_lager_i_gdb <- st_layers(gdb_sokvag)$name
  
  #  Steg 2: Extrahera kolumnnamn för alla lager och lägg i en lista
  lager_kolumnnamn_lista <- map(alla_lager_i_gdb, function(lager_namn) {
    kolumner_namn <- system(glue('"{ogr_sokvag}" {gdb_sokvag} {lager_namn} -so'), intern = TRUE)
    id_kol <- str_extract(str_subset(kolumner_namn, "FID Column"), "(?<= = ).*") # extrahera namn på id_kolumn
    geo_kol <- str_extract(str_subset(kolumner_namn, "Geometry Column"), "(?<= = ).*") # extrahera namn på geo-kolumn
    
    # hitta startelement för där kolumnnamnen finns
    kolumner_start <- str_which(kolumner_namn, "Geometry Column = ") + 1
    kolumner_namn <- kolumner_namn[kolumner_start:length(kolumner_namn)]
    kolumner_namn <- c(paste0(id_kol, ":"), kolumner_namn, paste0(geo_kol, ":"))
    
    kolumnnamn_ny <- set_names(
      map_chr(kolumner_namn, ~ {
        old_name <- str_extract(.x, "^[^:]+")
        new_name <- str_extract(.x, 'alternative name=\\"([^"]+)\\"') %>%
          str_replace_all('\\"', "") %>%
          str_remove("alternative name=") %>%
          str_trim()
        
        retur_vekt <- ifelse(is.na(new_name), old_name, new_name)
        
        # korrigera utifrån parametrar i funktionen
        retur_vekt <- retur_vekt %>%
          { if (tabort_paranteser) str_remove_all(., "\\(|\\)") %>% str_trim() else . } %>%
          { if (byt_ut_slash_mot_understreck) str_replace_all(., "/", "_") else . } %>%
          { if (byt_ut_mellanslag_mot_understreck) str_replace_all(., " ", "_") else . } %>%
          { if (enbart_gemener) tolower(.) else . } %>%
          { if (byt_ut_svenska_tecken) svenska_tecken_byt_ut(.) else . }
        retur_vekt
      }),
      map_chr(kolumner_namn, ~ str_extract(.x, "^[^:]+"))
    )
    
    # Ändra kolumnnamn för id-kolumn om användaren valt det
    if (!is.null(id_kol) && !is.na(id_kol) && !is.na(nytt_namn_id_kol)) {
      kolumnnamn_ny[id_kol] <- nytt_namn_id_kol
    }
    
    # Ändra kolumnnamn för geo-kolumn om användaren valt det
    if (!is.null(geo_kol) && !is.na(geo_kol) && !is.na(nytt_namn_geo_kol)) {
      kolumnnamn_ny[geo_kol] <- nytt_namn_geo_kol
    }
    
    return(kolumnnamn_ny)
  })
  
  # döp alla element efter vad dess gis-lager heter
  names(lager_kolumnnamn_lista) <- alla_lager_i_gdb
  
  # Returnera resultatet
  return(lager_kolumnnamn_lista)
}

raster_till_vektor <- function(
    rasterlager,
    vardekolumn,                           # här anges namnet på kolumnen som innehåller värdet
    filtrera_bort_na = TRUE,
    behall_varden_over = 0                # större än detta tal behålls, NA = ingen filtrering
) {
  
  rasterlager <- rasterlager %>% 
    NAflag()
  
  rutor_rast_mask <- mask(rutor_rast, rutor_rast, maskvalues=NA)
  
  rutor_sf <- as.polygons(rutor_rast, dissolve=FALSE, na.rm=filtrera_bort_na) %>% 
    st_as_sf() %>% 
    rename(varde = 1) 
  
  
  if (!is.na(behall_varden_over)) {
    rutor_sf <- rutor_sf %>% 
      filter(varde > behall_varden_over)
  }
  
  return(rutor_sf)
  
}
