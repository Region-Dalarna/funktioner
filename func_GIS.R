
if (!require("pacman")) install.packages("pacman")
p_load(sf,
       data.table,
       rio,
       openxlsx,
       tidyverse, 
       mapview,
       RPostgres,
       keyring)

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

# =======================================================================================================
# Här börjar postgis funktionen
 
 las_in_rutor_xlsx_till_postgis_skapa_pgr_graf <- 
   function(inlas_mapp = "G:/Samhällsanalys/GIS/rutor/",
            inlas_filer,        # enbart filnamnen på filerna som ska läsas in
            inlas_tabellnamn,   # de tabellnamn de nya filerna ska få i postgis
            rutstorlek = 100,
            schema_rut = "rutor",
            schema_natverk = "nvdb",
            tabell_natverk = "nvdb20buff30") {
     
     # =================== Läs in och bearbeta excelfiler med rutdata ======================
     
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
       
       # =================== lägg över till postgis =======================
       
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
       
       # ================== skriv rut-lagren till postgis 
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
       
       # gör en spatial join för mittpunkten i rutorna till nvdb ====================
       
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
     
     # =================== Läs in och bearbeta excelfiler med punktkoordinater ======================
     
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
       
       # =================== lägg över till postgis =======================
       
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
       
       
       # ================== skriv målpunkts-lagren till postgis 
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
     
     # =================== Läs in och bearbeta excelfiler med rutdata ======================
     
     
     # säkerställ att alla kolumnnamn är i gemener, ställer inte till problem i postigis då
     names(inlas_df) <- tolower(names(inlas_df))
     
     # =================== lägg över till postgis =======================
     
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
     
     # ================== skriv rut-lagren till postgis 
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
     
     # gör en spatial join för mittpunkten i rutorna till nvdb ====================
     
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
     
     # =================== Läs in och bearbeta excelfiler med rutdata ======================
     
     
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
     
     # gör en spatial join för mittpunkten i rutorna till nvdb ====================
     
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
 
 
 skriv_geosf_till_postgis_skapa_spatialt_index <- 
   function(inlas_sf,
            inlas_tabellnamn,   # de tabellnamn de nya filerna ska få i postgis
            schema_karta = "karta",
            postgistabell_id_kol,
            postgistabell_geo_kol,
            postgistabell_till_crs,
            pg_db_user = key_list(service = "rd_geodata")$username,
            pg_db_pwd = key_get("rd_geodata", key_list(service = "rd_geodata")$username),
            pg_db_host = "WFALMITVS526.ltdalarna.se",
            pg_db_port = 5432,
            pg_db_name_db = "geodata") {
     
     # Skript för att läsa in ett sf-objekt till en postgistabell 
     #
     # Följande parametrar skickas med funktionen:
     # inlas_mapp = mapp i vilken tabellen finns som innehåller målpunkterna, måste innehålla kolumner
     #              för x- och y- koordinat
     # inlas_filer = en vektor med den eller de filer som ska läsas in, måste finnas i inlas_mapp
     # inlas_tabellnamn = en textsträng eller vektor om det finns flera filer med tabellnamnet som 
     #                    målpunkterna ska ha i postgisdatabasen (bör vara gemener och utan konstiga tecken)
     # schema_karta = det schema i postgisdatabasen som målpunktstabellen ska ligga under
     # postgistabell_id_kol = den kolumn som innehåller ett unikt ID och görs till primärnyckelkolumn, måste finnas!
     # postgistabell_geo_kol = geometry-kolumnen
     # pg_db_user = användare för den postgisdatabas man ansluter till
     # pg_db_pwd = lösenord för användaren ovan, OBS! Aldrig i klartext!
     # pg_db_host = adress till den server där postgis-databasen finns
     # pg_db_port = den port som databasen ansluts via
     # pg_db_name_db = den databas i postgis som man ansluter till
     
     # =================== Läs in och bearbeta excelfiler med rutdata ======================
     
     
     # säkerställ att alla kolumnnamn är i gemener, ställer inte till problem i postgis då
     names(inlas_sf) <- tolower(names(inlas_sf))
     
     # =================== lägg över till postgis =======================
     
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
       timezone = "UTC",
       options="-c search_path=public")
     
     # kör sql-kod för att skapa ett nytt schema med namn definierat ovan
     dbExecute(con, paste0("create schema if not exists ", schema_karta, ";"))
     
     # ================== skriv rut-lagren till postgis 
     starttid = Sys.time()
     st_write(obj = inlas_sf,
              dsn = con,
              Id(schema=schema_karta, table = inlas_tabellnamn))
     print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "sec"),1), " sekunder att läsa in ", inlas_tabellnamn, " till postgis."))
     
     # skapa spatialt index, finns det sedan tidigare, ta bort - loopa så att man kan skicka fler geokolumner
     for (geokol in 1:length(postgistabell_geo_kol)) {
       dbExecute(con, paste0("DROP INDEX IF EXISTS ", schema_karta, ".", postgistabell_geo_kol[geokol], "_idx;")) 
       dbExecute(con, paste0("CREATE INDEX ", postgistabell_geo_kol[geokol], "_idx ON ", schema_karta, ".", inlas_tabellnamn, " USING GIST (", postgistabell_geo_kol[geokol], ");"))
     }  
     # gör rutid till id-kolumn i tabellen
     dbExecute(con, paste0("ALTER TABLE ", schema_karta, ".", inlas_tabellnamn, " ADD PRIMARY KEY (", postgistabell_id_kol ,");"))
     
     dbDisconnect(con)           # stäng postgis-anslutningen igen
     
   } # slut funktion
 
 
 
 kopiera_tabell_postgis <- function(schema_fran, 
                                    tabell_fran,
                                    schema_till,
                                    tabell_till,
                                    pg_db_user,
                                    pg_db_pwd,
                                    pg_db_host,
                                    pg_db_port,
                                    pg_db_name_db){
   
   # funktion för att kopiera en tabell i en postgisdatabas till en annan tabell
   # i samma schema eller under ett annat schema
   
   con_kop <- dbConnect(          # use in other settings
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
   
   
   # skapa tabell som har samma struktur som den tabell vi ska kopiera
   dbExecute(con_kop, paste0("CREATE TABLE ", schema_till, ".", tabell_till, " (LIKE ", schema_fran, ".", tabell_fran, " INCLUDING ALL);"))
   
   # fyll på den nya tabellen med data från tabellen vi kopierar från
   dbExecute(con_kop, paste0("INSERT INTO ", schema_till, ".", tabell_till, " SELECT * ",  
                             "FROM ", schema_fran, ".", tabell_fran, ";"))
   
   dbDisconnect(con_kop)           # stäng postgis-anslutningen igen
 }                      
 
 byt_schema_for_tabell_postgis <- function(schema_fran, 
                                           tabell_fran,
                                           schema_till,
                                           pg_db_user,
                                           pg_db_pwd,
                                           pg_db_host,
                                           pg_db_port,
                                           pg_db_name_db){
   
   # funktion för att flytta en tabell från ett schema till ett annat
   
   con_flytt <- dbConnect(          # use in other settings
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
   
   # byt schema för en tabell
   dbExecute(con_flytt, paste0("ALTER TABLE ", schema_fran, ".", tabell_fran, " SET SCHEMA ", schema_till, ";"))
   
   dbDisconnect(con_flytt)           # stäng postgis-anslutningen igen
   
 }
 
 # beräkna n närmaste malpunkter till varje ruta 
 
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
     add_row(namn = "lan_scb", id_kol = "lnkod", lankol = "lnkod", kommunkol = NA, sokord = list(c("lan", "lanspolygoner"))) %>% 
     add_row(namn = "tatorter_2020", id_kol = "tatortskod", lankol = "lnkod", kommunkol = "kommunkod", sokord = list(c("tatort", "tätort", "tatorter", "tätorter", "tatortspolygoner", "tätortspolygoner"))) %>% 
     add_row(namn = "regso", id_kol = "regsokod",  lankol = "lan", kommunkol = "kommun", sokord = list(c("regso", "regsopolygoner"))) %>% 
     add_row(namn = "deso", id_kol = "deso", lankol = "lan", kommunkol = "kommun", sokord = list(c("deso", "desopolygoner"))) %>% 
     add_row(namn = "nuts2", id_kol = "id", lankol = "id", kommunkol = "cntr_code", sokord = list(c("nuts2", "nuts2-områden")))
   
   return(karttabell_df)
 }
 
 
 hamta_karta <- function(karttyp = "kommuner", regionkoder = NA, tabellnamn = NA) {
   
   # här lägger vi till rader (dvs. tabeller) som ska vara hämtbara från geodatabasen med hamta_karta()-funktionen
   tabell_df <- hamta_karttabell()
   
   df_rad <- suppressWarnings(str_which(tabell_df$sokord, karttyp))             # vi letar upp den rad som parametern karrtyp finns på
   
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
     
     retur_sf <- suppressWarnings(las_in_postgis_tabell_till_sf_objekt(schema = "karta",
                                                                       tabell = pg_tabell,
                                                                       skickad_query = skickad_query,
                                                                       pg_db_user = key_list(service = "rd_geodata")$username,
                                                                       pg_db_pwd = key_get("rd_geodata", key_list(service = "rd_geodata")$username),
                                                                       pg_db_host = "WFALMITVS526.ltdalarna.se",
                                                                       pg_db_port = 5432,
                                                                       pg_db_name_db = "geodata"))
     
     return(retur_sf)
     
     
   } else {
     warning(paste0("Karttypen ", karttyp, " finns inte i databasen."))
   } # slut if-sats karttyp
   
 } # slut funktion
 
 
 
 las_in_postgis_tabell_till_sf_objekt <- function(
    schema,                 # det schema i vilken tabellen finns som man vill hämta
    tabell,                 # den tabell i postgisdatabasen man vill hämta
    skickad_query = NA,     # om man inte skickar med någon query hämtas hela tabellen
    pg_db_user,
    pg_db_pwd,
    pg_db_host,
    pg_db_port,
    pg_db_name_db){
   
   starttid <- Sys.time()
   
   con_hamta <- dbConnect(          # use in other settings
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
   
   
   if (is.na(skickad_query)) skickad_query <- paste0("SELECT * FROM ", schema, ".", tabell)
   retur_sf <- st_read(con_hamta, query = skickad_query)
   
   dbDisconnect(con_hamta)           # stäng postgis-anslutningen igen
   
   print(paste0("Det tog ", round(difftime(Sys.time(), starttid, units = "sec"),2) , " sekunder att läsa in tabellen."))
   
   return(retur_sf)
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
 
 postgis_skapa_schema_om_inte_finns <- function(schema_namn){
   # kör sql-kod för att skapa ett nytt schema med namn definierat ovan
   dbExecute(con, paste0("create schema if not exists ", schema_namn, ";"))
 }
 
 
 