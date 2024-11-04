
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

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")

# ===================================== hantera GIS i R ===============================================

kartifiera <- function(skickad_df, geom_nyckel){
  
  kartifiera_regionkoder <- unique(skickad_df[[geom_nyckel]])
  geom_nyckel_langd <- nchar(kartifiera_regionkoder) %>% unique()
  
  if (length(geom_nyckel_langd) > 1) {
    print("Skickad df:s geom_nyckel kan bara innehålla värden av samma typ. Kontrollera att så är fallet och försök igen.") 
  } else {
    
    # här bestäms vilken karttyp vi har att göra med
    if (geom_nyckel_langd == 2) kartifiera_karttyp <- "lan"
    if (geom_nyckel_langd == 4 & all(str_detect(kartifiera_regionkoder, "^[:digit:]+$"))) kartifiera_karttyp <- "kommun" 
    if (geom_nyckel_langd == 4 & !all(str_detect(kartifiera_regionkoder, "^[:digit:]+$"))) kartifiera_karttyp <- "nuts2"
    if (geom_nyckel_langd == 9 & !all(str_detect(kartifiera_regionkoder, "^[:digit:]+$"))) kartifiera_karttyp <- "deso"
    if (geom_nyckel_langd == 8 & !all(str_detect(kartifiera_regionkoder, "^[:digit:]+$"))) kartifiera_karttyp <- "regso"
    
    # vi hämtar gislagret för aktuell karttyp, för de geom_nyckelkoder som skickats med
    gis_lager <- hamta_karta(karttyp = kartifiera_karttyp, regionkoder = kartifiera_regionkoder)
    
    # här lägger vi till rader (dvs. tabeller) som ska vara hämtbara från geodatabasen med hamta_karta()-funktionen
    tabell_df <- hamta_karttabell()
    
    df_rad <- suppressWarnings(str_which(tabell_df$sokord, kartifiera_karttyp))             # vi letar upp den rad som parametern karrtyp finns på
    
    # om medskickade kartyp inte finns bland sökorden får pg_tabell värdet "finns ej" och då körs inte skriptet nedan
    if (length(df_rad) == 0) pg_tab_idkol <- "finns ej" else pg_tab_idkol <- tabell_df$id_kol[df_rad] 
    
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
    add_row(namn = "tatorter", id_kol = "tatortskod", lankol = "lan", kommunkol = "kommun", sokord = list(c("tatort", "tätort", "tatorter", "tätorter", "tatortspolygoner", "tätortspolygoner"))) %>% 
    add_row(namn = "tatortspunkter", id_kol = "tatortskod", lankol = "lan", kommunkol = "kommun", sokord = list(c("tatortspunkter", "tätortspunkter"))) %>% 
    add_row(namn = "regso", id_kol = "regsokod",  lankol = "lan", kommunkol = "kommun", sokord = list(c("regso", "regsopolygoner"))) %>% 
    add_row(namn = "deso", id_kol = "deso", lankol = "lan", kommunkol = "kommun", sokord = list(c("deso", "desopolygoner"))) %>% 
    add_row(namn = "distrikt", id_kol = "distriktskod", lankol = "lankod", kommunkol = "kommunkod", sokord = list(c("distrikt"))) %>% 
    add_row(namn = "nuts2", id_kol = "id", lankol = "id", kommunkol = "cntr_code", sokord = list(c("nuts2", "nuts2-områden"))) %>% 
    add_row(namn = "laregion_scb", id_kol = "lakod", lankol = "lan", kommunkol = "kommun", sokord = list(c("la", "laomraden", "la-omraden", "la-områden", "la-omraden")))
  
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
      if (karttyp == "nuts2") kommunkoder <- regionkoder[nchar(regionkoder) == 2]
      lanskoder <- regionkoder[nchar(regionkoder) == 2 & regionkoder != "00"]
      if (karttyp == "nuts2") lanskoder <- regionkoder[nchar(regionkoder) == 4]
    } else {
      kommunkoder <- NULL
      lanskoder <- NULL
    }
    # if (is.na(kommunkoder)) kommunkoder <- NULL
    # if (is.na(lanskoder)) lanskoder <- NULL 
    # 
    grundquery <- paste0("SELECT * FROM karta.", pg_tabell) 
    
    if ((length(kommunkoder) == 0) & (length(lanskoder) == 0)) skickad_query <- paste0(grundquery, ";") else {
      
      # det finns lan- eller kommunkoder, så vi lägger på ett WHERE på grundqueryn
      skickad_query <- paste0(grundquery, " WHERE ")
      
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
  
  retur_sf <- sf_skapa_fran_df_med_rutkolumner(skickad_df = skickad_df, x_kol = x_kol, 
                                               y_kol = y_kol, rutstorlek = rutstorlek, vald_crs = vald_crs)
  
  if (polygonlager) retur_sf <- st_buffer(retur_sf,(rutstorlek/2), endCapStyle = "SQUARE")
  
  return(retur_sf)
  
} # slut funktion



sf_skapa_fran_df_med_rutkolumner <- function(skickad_df, x_kol, y_kol, rutstorlek = NA, vald_crs = 3006){
  
  if (is.na(rutstorlek)) rutstorlek <- rutstorlek_estimera(skickad_df[[x_kol]], skickad_df[[y_kol]])
  
  # skapa en punktgeometri av x- och y-kolumnerna där koordinaten är nedre vänstra hörnet
  retur_sf <- skickad_df %>% 
    mutate(x_ny = !!sym(x_kol)+(rutstorlek/2),
           y_ny = !!sym(y_kol)+(rutstorlek/2)) %>%
    st_as_sf(coords = c("x_ny", "y_ny"), crs = vald_crs) %>% 
    st_cast("POINT")
  
  return(retur_sf)
  
}


rutstorlek_estimera <- function(x, y) {
  
  # Kombinera x- och y-koordinaterna till en enda vektor
  coords <- c(x ,y)
  
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


uppkoppling_db <- function(
    
  # 0. Funktion för att koppla upp mot databasen. Kan användas med defaultvärden enligt nedan eller egna parametrar.
  # Används av andra funktioner som default om inget eget objekt med databasuppkoppling har skickats till dessa funktioner
  # OBS! Ändra default för db_name till "geodata" sen
  
  service_name = NA,                                 # "rd_geodata"
  db_host = "WFALMITVS526.ltdalarna.se",
  db_port = 5432,
  db_name = "geodata",                  
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

postgres_alla_rattigheter <- function(con = "default", 
                                      meddelande_tid = FALSE
                                      ) {
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE  


  query <- "
  WITH recursive role_inheritance AS (
    -- Start med att samla alla användare och roller de är medlemmar i
    SELECT 
      member.oid AS user_oid,
      member.rolname AS user_or_role,
      role.oid AS inherited_role_oid,
      role.rolname AS inherited_role
    FROM 
      pg_auth_members m
    JOIN 
      pg_roles member ON m.member = member.oid
    JOIN 
      pg_roles role ON m.roleid = role.oid
    
    UNION ALL
    
    -- Rekursivt hämta ärvda roller längre upp i hierarkin
    SELECT 
      ri.user_oid,
      ri.user_or_role,
      role.oid AS inherited_role_oid,
      role.rolname AS inherited_role
    FROM 
      role_inheritance ri
    JOIN 
      pg_auth_members m ON ri.inherited_role_oid = m.member
    JOIN 
      pg_roles role ON m.roleid = role.oid
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
      CASE
        WHEN STRING_AGG(privilege_type, ',') LIKE '%INSERT%' OR
             STRING_AGG(privilege_type, ',') LIKE '%UPDATE%' OR
             STRING_AGG(privilege_type, ',') LIKE '%DELETE%' THEN 'write'
        WHEN STRING_AGG(privilege_type, ',') LIKE '%SELECT%' THEN 'read'
        ELSE 'no access'
      END AS access_type
    FROM 
      information_schema.role_table_grants
    GROUP BY 
      grantee, table_schema
  ),
  combined_access AS (
    SELECT 
      u.role_or_user,
      s.schema_name,
      CASE
        -- Om användaren är en superanvändare, ge dem skrivbehörigheter till alla scheman
        WHEN u.rolsuper THEN 'write'
        -- Annars, hämta de faktiska behörigheterna
        ELSE COALESCE(p.access_type, 'no access')
      END AS access_type
    FROM 
      (SELECT role_or_user, rolsuper FROM all_users UNION SELECT inherited_role AS role_or_user, FALSE AS rolsuper FROM role_inheritance) u
    CROSS JOIN 
      all_schemas s
    LEFT JOIN 
      privileges p ON u.role_or_user = p.role_or_user AND s.schema_name = p.table_schema
  )
  -- Eliminera dubbletter och prioritera 'write' över 'read' och 'no access'
  SELECT role_or_user, schema_name, 
         MAX(CASE 
               WHEN access_type = 'write' THEN 'write'
               WHEN access_type = 'read' THEN 'read'
               ELSE 'no access'
             END) AS access_level
  FROM combined_access
  GROUP BY role_or_user, schema_name
  ORDER BY role_or_user, schema_name;
"
  
  # Exekvera SQL-frågan och spara resultatet
  user_schema_permissions <- dbGetQuery(con, query)
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(user_schema_permissions)

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
                                                     databas = "geodata", 
                                                     schema = "alla", 
                                                     rattigheter = c("CONNECT", "SELECT", "USAGE"),
                                                     meddelande_tid = FALSE
                                                     ){
  starttid <- Sys.time()  # Starta tidtagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  if (all(rattigheter == "alla")) rattigheter <- postgres_lista_giltiga_rattigheter()$Rattighet
  
  # Lista över system-scheman som ska undantas
  system_scheman <- c("pg_catalog", "information_schema", "pg_toast")
  
  # Iterera över varje databas i vektorn och tilldela rättigheter
  for (db in databas) {
    # Steg 1: Tilldela anslutningsrättigheter till den specifika databasen (CONNECT på databasnivå)
    if ("CONNECT" %in% rattigheter) {
      tilldela_atkomst_query <- paste0("GRANT CONNECT ON DATABASE ", db, " TO ", anvandarnamn, ";")
      tryCatch({
        dbExecute(con, tilldela_atkomst_query)
        message(paste("CONNECT-rättighet har lagts till för användaren", anvandarnamn, "till databasen", db))
      }, error = function(e) {
        message(paste("Kunde inte lägga till CONNECT-rättighet för användaren", anvandarnamn, "i databasen", db, ":", e$message))
      })
    }
    
    # Kontrollera om alla scheman ska behandlas
    if (all(schema == "alla")) {
      # Hämta alla scheman i databasen utan att använda catalog_name
      alla_scheman_query <- "SELECT schema_name FROM information_schema.schemata;"
      alla_scheman <- dbGetQuery(con, alla_scheman_query)$schema_name
    } else {
      kontroll_schema_query <- paste0("SELECT schema_name FROM information_schema.schemata WHERE schema_name IN (", paste(sprintf("'%s'", schema), collapse = ", "), ");")
      alla_scheman <- dbGetQuery(con, kontroll_schema_query)$schema_name
    }
    
    # Filtrera bort system-scheman från listan över scheman att bearbeta
    scheman_att_bearbeta <- setdiff(alla_scheman, system_scheman)
    # Filtrera också bort alla scheman som börjar med "pg_"
    scheman_att_bearbeta <- scheman_att_bearbeta[!grepl("^pg_", scheman_att_bearbeta)]
    
    # Iterera över varje schema som existerar och tilldela rättigheter
    for (schema_namn in scheman_att_bearbeta) {
      # Steg 2: Tilldela USAGE rättighet till schemat
      if ("USAGE" %in% rattigheter) {
        tilldela_usage_query <- paste0("GRANT USAGE ON SCHEMA ", schema_namn, " TO ", anvandarnamn, ";")
        tryCatch({
          dbExecute(con, tilldela_usage_query)
          message(paste("USAGE-rättighet har lagts till för schemat", schema_namn, "för användaren", anvandarnamn))
        }, error = function(e) {
          message(paste("Kunde inte lägga till USAGE-rättighet för schemat", schema_namn, "för användaren", anvandarnamn, ":", e$message))
        })
      }
      
      # Tilldela rättigheter på tabellnivå
      for (rattighet in setdiff(rattigheter, c("CONNECT", "USAGE"))) {
        if (rattighet %in% postgres_lista_giltiga_rattigheter()$Rattighet) {
          tilldela_rattigheter_query <- paste0("GRANT ", rattighet, " ON ALL TABLES IN SCHEMA ", schema_namn, " TO ", anvandarnamn, ";")
          tryCatch({
            dbExecute(con, tilldela_rattigheter_query)
            message(paste("Rättigheten", rattighet, "har lagts till för användaren", anvandarnamn, "i schemat", schema_namn))
          }, error = function(e) {
            message(paste("Kunde inte lägga till rättigheten", rattighet, "för användaren", anvandarnamn, "i schemat", schema_namn, ":", e$message))
          })
        } else {
          message(paste("Ogiltig rättighet:", rattighet, "- denna rättighet har inte lagts till."))
        }
      }
    }
  }
  
  if (default_flagga) dbDisconnect(con)  # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)  # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
}


# Funktion för att ta bort rättigheter från användare
postgres_rattigheter_anvandare_ta_bort <- function(con = "default", 
                                                   anvandarnamn, 
                                                   databas, 
                                                   rattigheter = "alla",
                                                   meddelande_tid = FALSE
                                                   ) {
  
  # för skrivrättigheter så kan c("INSERT", "UPDATE", "DELETE", "CREATE") användas
  # för läsrättigheter så räcker det att ta bort rättigheter för "CONNECT"
  starttid <- Sys.time()  # Starta tidtagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  if (all(rattigheter == "alla")) rattigheter <- postgres_lista_giltiga_rattigheter()$Rattighet
  
  # Iterera över varje databas i vektorn och ta bort rättigheter
  for (db in databas) {
    # Ta bort anslutningsrättigheter till databasen
    ta_bort_atkomst_query <- paste0("REVOKE CONNECT ON DATABASE ", db, " FROM ", anvandarnamn, ";")
    dbExecute(con, ta_bort_atkomst_query)
    
    # Om rättigheter är "ALL", ta bort alla rättigheter från användaren för alla tabeller
    if (all(rattigheter == "alla")) {
      ta_bort_rattigheter_query <- paste0("REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM ", anvandarnamn, ";")
      dbExecute(con, ta_bort_rattigheter_query)
      message(paste("Alla rättigheter har tagits bort för användaren", anvandarnamn, "i databasen", db))
    } else {
      # Annars, iterera över varje rättighet och validera om den är giltig
      for (rattighet in rattigheter) {
        if (rattighet %in% postgres_lista_giltiga_rattigheter()$Rattighet) {
          ta_bort_rattigheter_query <- paste0("REVOKE ", rattighet, " ON ALL TABLES IN SCHEMA public FROM ", anvandarnamn, ";")
          dbExecute(con, ta_bort_rattigheter_query)
          message(paste("Rättigheten", rattighet, "har tagits bort från användaren", anvandarnamn, "i databasen", db))
        } else {
          message(paste("Ogiltig rättighet:", rattighet, "- denna rättighet har inte tagits bort."))
        }
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
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
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
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # SQL-fråga för att läsa in tabellen från ett specifikt schema
  if (is.na(query)) sql_query <- paste0("SELECT * FROM ", schema, ".", tabell)
  
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


postgres_tabell_ta_bort <- function(con = "default", 
                                    schema, 
                                    tabell,
                                    meddelande_tid = FALSE
                                    ) {
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  schema_tabell <- paste0(schema, ".", tabell)
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # Kontrollera om tabellen existerar
  full_tabell_namn <- paste0(schema, ".", tabell)
  tabell_finns <- dbExistsTable(con, Id(schema = schema, table = tabell))
  
  if (!tabell_finns) {
    message("Tabellen '", full_tabell_namn, "' existerar inte. Ingen åtgärd vidtogs.")
  } else {
    # Ta bort tabellen om den existerar
    sql <- paste0("DROP TABLE ", DBI::dbQuoteIdentifier(con, schema), ".", DBI::dbQuoteIdentifier(con, tabell), ";")
    dbExecute(con, sql)
    message("Tabellen '", full_tabell_namn, "' har tagits bort.")
  }
  
  # Koppla ner anslutningen om den skapades som default
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}
 
postgres_schema_finns <- function(schema, con = "default"
                                  ) {
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  query <- sprintf("
    SELECT EXISTS (
      SELECT 1
      FROM information_schema.schemata
      WHERE schema_name = '%s'
    ) AS schema_exists;
  ", schema)
  
  result <- dbGetQuery(con, query)
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  
  return(result$schema_exists[1])
}

postgres_schema_skapa <- function(con = "default", 
                                  schema, 
                                  meddelande_tid = FALSE
) {
  
  starttid <- Sys.time()  # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  }
  
  # Skapa schema om det inte redan finns
  skapa_schema_query <- paste0("CREATE SCHEMA IF NOT EXISTS ", schema, ";")
  tryCatch({
    dbExecute(con, skapa_schema_query)
    schema_skapad <- TRUE  # Markera att schemat skapades om inget fel uppstod
  }, error = function(e) {
    message("Ett fel uppstod när schemat skulle skapas: ", e$message)
  })
  
  # Kontrollslinga för att säkerställa att schemat finns innan rättigheter tilldelas
  if (schema_skapad && dbGetInfo(con)$dbname == "geodata") {
        tryCatch({
         suppressMessages(postgres_rattigheter_anvandare_lagg_till(con = con, anvandarnamn = "geodata_las", rattigheter = c("CONNECT", "SELECT", "USAGE"), schema = schema))
        }, error = function(e) {
          message("Ett fel uppstod när rättigheterna skulle tilldelas: ", e$message)
        })
    }
  
  # Koppla ner om defaultuppkopplingen har använts
  if (default_flagga) dbDisconnect(con)
  
  # Beräkna och skriv ut tidsåtgång om meddelande_tid är TRUE
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
}


postgres_schema_ta_bort <- function(con = "default", 
                                    schema,
                                    meddelande_tid = FALSE
                                    ) {
  
  starttid <- Sys.time()  # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
    default_flagga <- TRUE
  } else {
    default_flagga <- FALSE
  } 
  
  # Kontrollera om schemat existerar
  schema_finns <- dbGetQuery(con, paste0("
    SELECT schema_name
    FROM information_schema.schemata
    WHERE schema_name = '", schema, "';"))
  
  if (nrow(schema_finns) == 0) {
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
  if (default_flagga) dbDisconnect(con)
  
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)  # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}


# ================================= postgis-funktioner ================================================


postgis_sf_till_postgistabell <- 
  function(inlas_sf,
           schema = "karta",
           tabell,   # de tabellnamn de nya filerna ska få i postgis
           postgistabell_id_kol,
           postgistabell_geo_kol = NA,
           skapa_spatialt_index = TRUE,
           nytt_schema_oppet_for_geodata_las = TRUE,             # om TRUE så öppnas läsrättigheter för användaren geodata_las (vilket är det som ska användas om det inte finns mycket goda skäl att låta bli)
           #postgistabell_till_crs,
           meddelande_tid = FALSE,
           con = "default"
  ) {
    # Skript för att läsa in ett sf-objekt till en postgistabell 
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
    
    if (!schema_finns) {
      dbExecute(con, paste0("create schema if not exists ", schema, ";"))
    
      if (nytt_schema_oppet_for_geodata_las) {
      sql_command <- sprintf("                                          # skapa sql-kommando för att öppna schemat och framtida tabeller för geodata_las
        ALTER DEFAULT PRIVILEGES IN SCHEMA %s
        GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO %s;
        ", schema, "geodata_las")
      dbExecute(con, sql_command)                               # kör sql-kommandot som skapats ovan
      } 
    }
      
    # Kontrollera om tabellen redan finns
    tabell_finns <- DBI::dbExistsTable(con, DBI::Id(schema = schema, table = tabell))
    
    if (tabell_finns) { 
      # Töm tabellen men behåll struktur och behörigheter
      dbExecute(con, paste0("TRUNCATE TABLE ", schema, ".", tabell, ";"))
      
      # Infoga ny data
      system.time({
      st_write(
        obj = inlas_sf,
        dsn = con,
        layer = DBI::Id(schema = schema, table = tabell),
        append = FALSE)                  # här har jag ändrat till FALSE pga uppstod problem då vi ju vill skriva över och inte bara lägga till rader
      }) # slut system.time
      
    } else { 
      
      # Skriv data och skapa ny tabell
      system.time({
      st_write(
        obj = inlas_sf,
        dsn = con,
        layer = DBI::Id(schema = schema, table = tabell),
        append = FALSE)
      }) # slut system.time
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
    dbExecute(con, paste0("ALTER TABLE ", schema, ".", tabell, " ADD PRIMARY KEY (", postgistabell_id_kol ,");"))
    
    if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
    berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
    if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
    
  } # slut funktion

postgis_postgistabell_till_sf <- function(
    schema,                 # det schema i vilken tabellen finns som man vill hämta
    tabell,                 # den tabell i postgisdatabasen man vill hämta
    query = NA,     # om man inte skickar med någon query hämtas hela tabellen
    meddelande_tid = FALSE,
    con = "default"){
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE 
  
  
  if (is.na(query)) query <- paste0("SELECT * FROM ", schema, ".", tabell)
  retur_sf <- st_read(con, query = query)
  
  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
  return(retur_sf)
} # slut funktion

postgis_kopiera_tabell <- function(schema_fran, 
                                   tabell_fran,
                                   schema_till,
                                   tabell_till,
                                   meddelande_tid = FALSE,
                                   con = "default"
) {
  
  # funktion för att kopiera en tabell i en postgisdatabas till en annan tabell
  # i samma schema eller under ett annat schema
  
  starttid <- Sys.time()                                        # Starta tidstagning
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if(is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga = TRUE
  } else  default_flagga = FALSE
  
  
  # skapa tabell som har samma struktur som den tabell vi ska kopiera
  dbExecute(con_kop, paste0("CREATE TABLE ", schema_till, ".", tabell_till, " (LIKE ", schema_fran, ".", tabell_fran, " INCLUDING ALL);"))
  
  # fyll på den nya tabellen med data från tabellen vi kopierar från
  dbExecute(con_kop, paste0("INSERT INTO ", schema_till, ".", tabell_till, " SELECT * ",  
                            "FROM ", schema_fran, ".", tabell_fran, ";"))

  if(default_flagga) dbDisconnect(con)                                                    # Koppla ner om defaultuppkopplingen har använts
  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(glue("Processen tog {berakningstid} sekunder att köra"))
  
}                      

postgis_flytta_tabell <- function(schema_fran,
                                  tabell_fran,
                                  schema_till,
                                  meddelande_tid = FALSE,
                                  con = "default"
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

postgis_skapa_schema_om_inte_finns <- function(schema_namn, 
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
 
postgis_aktivera_i_postgres_db <- function(con = "default") {
  
  # Kontrollera om anslutningen är en teckensträng och skapa uppkoppling om så är fallet
  if (is.character(con) && con == "default") {
    con <- uppkoppling_db()  # Anropa funktionen för att koppla upp mot db med defaultvärden
    default_flagga <- TRUE
  } else if (is.character(con) && con == "adm") {
    con <- uppkoppling_db(service_name = "rd_geodata")
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

# ======================================= pgrouting-funktioner ================================================

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

# ================================= Övriga funktioner ================================================


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
  
  
  # Steg 1: Hämta namnet på alla gis-lager som finns i geodatabasen
  alla_lager_i_gdb <- st_layers(gdb_sokvag)$name
  
  #  Steg 2: Extrahera kolumnnamn för alla lager och lägg i en lista
  lager_kolumnnamn_lista <- map(alla_lager_i_gdb, function(lager_namn) {
    
    kolumner_namn <- system(glue("ogrinfo {gdb_sokvag} {lager_namn} -so"), intern = TRUE)
    id_kol <- str_extract(str_subset(kolumner_namn, "FID Column"), "(?<= = ).*")        # extrahera namn på id_kolumn
    geo_kol <- str_extract(str_subset(kolumner_namn, "Geometry Column"), "(?<= = ).*")  # extrahera namn på geo-kolumn
    
    # hitta startelement för där kolumnnamnen finns
    kolumner_start <- str_which(kolumner_namn, "Geometry Column = ") + 1 
    kolumner_namn <- kolumner_namn[kolumner_start:length(kolumner_namn)]
    kolumner_namn <- c(paste0(id_kol, ":"), kolumner_namn, paste0(geo_kol, ":"))
    
    kolumnnamn_ny <- set_names(map_chr(kolumner_namn, ~ {
      old_name <- str_extract(.x, "^[^:]+")
      new_name <- str_extract(.x, 'alternative name=\\"([^"]+)\\"') %>% 
        str_replace_all('\\"', "") %>% 
        str_remove("alternative name=") %>% 
        str_trim()
      
      retur_vekt <- ifelse(is.na(new_name), old_name, new_name)
      
      # korrigera utifrån parametrar i funktionen
      if (tabort_paranteser) retur_vekt <- retur_vekt %>% str_remove_all("\\(") %>% str_remove_all("\\)") %>% str_trim()
      if (byt_ut_slash_mot_understreck) retur_vekt <- retur_vekt %>% str_replace_all("/", "_")
      if (byt_ut_mellanslag_mot_understreck) retur_vekt <- retur_vekt %>% str_replace_all(" ", "_")
      if (enbart_gemener) retur_vekt <- retur_vekt %>% tolower()
      if (byt_ut_svenska_tecken) retur_vekt <- retur_vekt %>% svenska_tecken_byt_ut()
      
    }), map_chr(kolumner_namn, ~ str_extract(.x, "^[^:]+"))) 
    
    # ändra kolumnnamn för id- respektive geokolumn om användaren valt det
    kolumnnamn_ny <- kolumnnamn_ny %>%
      modify_at(id_kol, ~ if (!is.na(nytt_namn_id_kol)) nytt_namn_id_kol else .x) %>%
      modify_at(geo_kol, ~ if (!is.na(nytt_namn_geo_kol)) nytt_namn_geo_kol else .x)
    
  })
  
  # döp alla element efter vad dess gis-lager heter
  names(lager_kolumnnamn_lista) <- alla_lager_i_gdb
  
  # Returnera resultatet
  return(lager_kolumnnamn_lista)
}
