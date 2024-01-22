
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       sf,
       data.table,
       rio)

# =================== här ställer vi in mappar och filer som vi hämtar GIS-lager från =======================
mapp_scbadmgranser <- "G:/Samhällsanalys/GIS/Grundkartor/Adm gränser med kustgränser/"
filnamn_scbadmgranser_kommuner <- "Kommungränser_SCB_07.shp"
filnamn_scbadmgranser_lan <- "Länsgränser_SCB_07.shp"

# ===================== skapa variabler som används i funktionerna ========================
sokvag_kommuner_sv <- paste0(mapp_scbadmgranser, filnamn_scbadmgranser_kommuner)
sokvag_lan_sv <- paste0(mapp_scbadmgranser, filnamn_scbadmgranser_lan)

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
berakna_mittpunkter <- function(df, xruta, yruta, rutstorlek, 
                                xkolnamn = "mitt_x", ykolnamn = "mitt_y"){
  # beräkna de nya kolumnerna
  df[xkolnamn] <- df[xruta]+(rutstorlek/2)
  df[ykolnamn] <- df[yruta]+(rutstorlek/2)
  # flytta de nya kolumnerna och lägg dem efter x- och y-kolumnerna
  df <- df %>% 
    relocate(all_of(xkolnamn), .after = all_of(yruta)) %>% 
    relocate(all_of(ykolnamn), .after = all_of(xkolnamn))
  
  return(df)
}

# hämta ett sf-objekt med kommunpolygoner enligt SCB:s gränser, dvs. snygga gränser anpassade
# efter kustlinjer, sjöar etc. och inte lantmäteriets lite fulare. Varje kommun är enbart en polygon
hamta_kommuner_gis <- function(){
  retur_gis <- st_read(sokvag_kommuner_sv, crs = 3006) %>% 
    select(c(kommunkod = KNKOD, kommun = KNNAMN, geometry))
  return(retur_gis)
}

# hämta ett sf-objekt med kommunpolygoner enligt SCB:s gränser, dvs. snygga gränser anpassade
# efter kustlinjer, sjöar etc. och inte lantmäteriets lite fulare. Varje kommun är enbart en polygon
hamta_lan_gis <- function(){
  retur_gis <- st_read(sokvag_lan_sv, crs = 3006) %>% 
    select(c(lanskod = LNKOD, lan = FullName, geometry))
  return(retur_gis)
}

# skicka ett sf-objekt med punkter. En kolumn i objektet innehåller en numerisk kolumn utifrån vilken
# linjer dras mellan punkterna, från första till andra punkten, från andra till tredje punkten osv.
# till den sista punkten. Det går också att skicka med korrekt crs som ska vara samma som 
skapa_linje_langs_med_punkter <- function(skickad_sf,                   # skickad_sf = skickat sf-objekt med punkter
                                          kol_ord,                      # kol_ord = den kolumn som innehåller nummer som rangordnar mellan vilka punkter som linjen ska dras, den dras i samma ordning som i denna kolumn
                                          names,                        # namn på punkterna om man vill ha med det (oklart om vi vill det)
                                          names_bara_startpunkt = TRUE  # om man bara vill ha namn från startpunkten, annars blir namnet "startnamn - slutnamn"
                                          ) {
  
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

# läs en gisfil som ligger i en zipfil direkt från url - packar upp 
las_gisfil_fran_zipfil_via_url <- function(skickad_url){
  cur_tempfile <- tempfile()              # skapa temporär fil som vi laddar ner från url
  download.file(url = skickad_url, destfile = cur_tempfile)      # ladda ner fil från url till tempfil 
  out_directory <- tempfile()             # skapa outputmapp att spara uppackad zipfil till
  unzip(cur_tempfile, exdir = out_directory)        # packa upp fil från url till outputmapp
  
  retur_sf <- st_read(out_directory) #read_sf also works here
  return(retur_sf)
  
}


# läs en gisfil direkt från en zipfil
las_gisfil_fran_zipfil_via_sokvag <- function(skickad_sokvag) {
  
  out_directory <- tempfile()                  # skapa outputmapp att spara uppackad zipfil till
  unzip(skickad_sokvag, exdir = out_directory)        # packa upp fil från skickad sökväg till outputmapp
  
  retur_sf <- st_read(out_directory) #read_sf also works here
  return(retur_sf)
  
}

# det här är en specialfunktion som används för att ladda ner polygoner för FA- och LA-regioner samt
# läns- och kommungränser med kustgränser (blir snyggare kartor) de ligger som zipfiler i en zipfil
# så man måste packa upp dessa i två steg
unzip_zipfil_med_zipfiler <- function(skickad_url){
  
  cur_tempfile <- tempfile()
  download.file(url = skickad_url, destfile = cur_tempfile)
  out_directory <- tempfile()
  unzip(cur_tempfile, exdir = out_directory)
  
  zipfillista <- list.files(out_directory, full.names = TRUE)
  return(zipfillista)
  
}

# funktion för att skapa en gpkg-fil från ett uttag ur supercross där rutid är en kolumn

skapa_sf_fran_csv_eller_excel_supercross <- function(fil_med_sokvag,               # fil som ska bearbetas, dvs. ett uttag från Supercross 
                                                       rutid_kol = NA,               # finns en funktion för att hitta rutid-kolumnen men man kan skicka med den här
                                                       rutstorlek = NA,              # om man vill ange själv, annars kontrolleras för det automatiskt.
                                                       vald_crs = 3006) {
  
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
 st_largest_ring <- function(x) {
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
      pull(row.id)
    
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

