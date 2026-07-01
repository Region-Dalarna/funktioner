# funktioner för att hantera api-anrop via px_web samt möjligen i framtiden även 
# andra paket 

if (!require("pacman")) install.packages("pacman")
p_load(xml2,
       git2r,
       gert,
       gh,
       pxweb,
       rKolada,
       httr,
       farver,
       DBI,
       keyring,
       rvest,
       curl,
       usethis,
       glue,
       tidyverse)

# ================================================= pxweb-funktioner ========================================================

hamtaregtab <- function(){
  
  # Hämta tabell med regioner
  url_adress <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  
  hamtad_regionkod <- hamta_giltiga_varden_fran_tabell(url_adress, "region")
  hamtad_region <- hamta_giltiga_varden_fran_tabell(url_adress, "region", klartext = TRUE)
  
  regdf <- data.frame(regionkod = hamtad_regionkod, region = hamtad_region)
  
  return(regdf)
}

hamtakommuner <- function(lan = "20", tamedlan = TRUE, tamedriket = TRUE, allakommuner = FALSE){
  regdf <- hamtaregtab()
  #lan <- as.character(lan)
  lan <- substr(lan,1,2)
  nydf <- regdf
  if (!allakommuner) nydf <- nydf %>%  filter(regionkod == "00" | substr(regionkod,1,2) %in% lan)
  if (!tamedlan) nydf <- nydf %>% filter(!regionkod %in% lan) 
  if (!tamedriket) nydf <- nydf %>% filter(regionkod != "00")
  if (allakommuner) {
    if (tamedlan) {
      nydf <- nydf[nchar(nydf$regionkod) == 4 | nchar(nydf$regionkod) == 2,]
      if (!tamedriket) nydf <- nydf[nydf$regionkod != "00",]
    } else {
      nydf <- nydf[nchar(nydf$regionkod) == 4,]
    }
    
  }
  vektor <- as.vector(nydf$regionkod)
  vektor
}

hamtaAllaLan <- function(tamedriket = TRUE){
  
  # Url till befolkningstabellen
  url_adress <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  
  hamtad_regionkod <- hamta_giltiga_varden_fran_tabell(url_adress, "region")    # hämta alla regionkoder
  retur_vektor <- hamtad_regionkod[nchar(hamtad_regionkod) == 2]               # filtrera ut län och riket
  if (!tamedriket) retur_vektor <- retur_vektor[retur_vektor != "00"]          # filtrera ut riket om vi valt bort det
  
  return(retur_vektor)
}

hamtaregion_kod_namn <- function(regionkod, kolada = FALSE){
  regdf <- hamtaregtab()
  retur_df <- regdf[regdf$regionkod %in% regionkod,]
  if (nrow(retur_df) == 0) retur_df <- data.frame(regionkod = regionkod, region = regionkod)
  if (kolada) retur_df <- retur_df %>% mutate(regionkod = str_pad(regionkod, width = 4, side = "left", pad = "0"))
  return(retur_df)
}

hamta_kommunkoder <- function(lanskod = "20", kolada = FALSE) {
  # smidig funktion för att hämta kommunkoder för ett län
  retur_df <- hamtaregion_kod_namn(regionkod = hamtakommuner(lanskod, tamedlan = FALSE, tamedriket = FALSE), kolada = kolada)
  return(retur_df)
}

skapa_kortnamn_lan <- function(lansnamn, byt_ut_riket_mot_sverige = FALSE){
  nyttnamn <- NA
  for (elem in 1:length(lansnamn)){
    if (is.na(lansnamn[elem])) nyttnamn[elem] <- NA else {
      if (substr(lansnamn[elem], nchar(lansnamn[elem])-3, nchar(lansnamn[elem]))==" län") nyttnamn[elem] <- substr(lansnamn[elem],1, nchar(lansnamn[elem])-4) else nyttnamn[elem] <- lansnamn[elem]
      if (substr(nyttnamn[elem], nchar(nyttnamn[elem]),nchar(nyttnamn[elem]))=="s" & grepl("län", lansnamn[elem])) nyttnamn[elem] <- substr(nyttnamn[elem],1,nchar(nyttnamn[elem])-1)
      if (byt_ut_riket_mot_sverige) if (lansnamn[elem] == "Riket") nyttnamn[elem] <- "Sverige"
    } # slut if-sats om det är ett NA-värde
  }
  return(nyttnamn)
}

#################### OBS! Gammal funktion, använd inte denna utan hamta_giltiga_varden_fran_tabell() istället!!!
#################### Använd med max, typ såhär: max(hamta_giltiga_varden_fran_tabell(api_url, "tid"))

hamta_senaste_tid_i_tabell <- function(skickad_url, tidkol = "år", tabort_var = NA, region_varde = "20", query_list = NULL){
  # fyll query-lista till px_web-uttaget
  if (is.null(query_list)) query_list <- list(Region = region_varde, ContentsCode = "*", Tid = "*")
  # om tabellen inte innehåller en variabel, ta bort den variabeln ur query_list
  if (!is.na(tabort_var)) query_list <- query_list[!names(query_list) %in% tabort_var]
  
  px_small <- pxweb_get(url = skickad_url, query = query_list) 
  px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
  senaste_tid <- as.numeric(max(px_df_small[[tidkol]]))     # startår är alltid ett år innan första året i prognosen
  message(paste0('Denna funktion är gammal och kommer att tas bort så småningom. Använd\n   max(hamta_giltiga_varden_fran_tabell(api_url, "tid")) \nistället!'))
  return(as.character(senaste_tid))
}

#################### OBS! Gammal funktion, använd inte denna utan hamta_giltiga_varden_fran_tabell() istället!!!
#################### Använd med min, typ såhär: min(hamta_giltiga_varden_fran_tabell(api_url, "tid"))

hamta_tidigaste_tid_i_tabell <- function(skickad_url, tidkol = "år", tabort_var = NA, region_varde = "20", query_list = NULL){
  # fyll query-lista till px_web-uttaget
  if (is.null(query_list)) query_list <- list(Region = region_varde, ContentsCode = "*", Tid = "*")
  # om tabellen inte innehåller en variabel, ta bort den variabeln ur query_list
  if (!is.na(tabort_var)) query_list <- query_list[!names(query_list) %in% tabort_var]
  
  px_small <- pxweb_get(url = skickad_url,query = query_list) 
  px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
  tidigaste_tid <- as.numeric(min(px_df_small[[tidkol]]))     # startår är alltid ett år innan första året i prognosen
  message(paste0('Denna funktion är gammal och kommer att tas bort så småningom. Använd\n   min(hamta_giltiga_varden_fran_tabell(api_url, "tid")) \nistället!'))
  return(as.character(tidigaste_tid))
} 

#################### OBS! Gammal funktion, använd inte denna utan hamta_giltiga_varden_fran_tabell() istället!!!

hamta_giltig_tid_tabell <- function(skickad_url, tidkol = "år", tabort_var = NA, region_varde = "20", query_list = NULL){
  # fyll query-lista till px_web-uttaget
  if (is.null(query_list)) query_list <- list(Region = region_varde, ContentsCode = "*", Tid = "*")
  # om tabellen inte innehåller en variabel, ta bort den variabeln ur query_list
  if (!is.na(tabort_var)) query_list <- query_list[names(query_list) %in% tabort_var]
  
  px_small <- pxweb_get(url = skickad_url, query = query_list) 
  px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
  senaste_tid <- as.numeric(max(px_df_small[[tidkol]]))     # startår är alltid ett år innan första året i prognosen
  tidigaste_tid <- as.numeric(min(px_df_small[[tidkol]]))     # startår är alltid ett år innan första året i prognosen
  giltig_vekt <- as.character(tidigaste_tid:senaste_tid)
  message(paste0("Denna funktion är gammal och kommer att tas bort så småningom. Använd funktionen\n   hamta_giltiga_varden_fran_tabell()\nistället!"))
  return(giltig_vekt)  
}

pxvarlist <- function(api_url){
  
  # om vi skickar med en metadata-lista (som vi får genom att köra "px_meta <- pxweb_get(api_url)") så använder vi den direkt
  # om det är en url så hämtar vi data med den
  metadata <- if(is.list(api_url)) api_url else pxweb_get(api_url)
  # skapa en dataframe med både koder och klartext som vi returnerar
  retur_df <- data.frame(
    klartext = map_chr(metadata$variables, 2),
    koder = map_chr(metadata$variables, 1)
  )
  return(retur_df)
}

pxvardelist <- function(api_url, variabel, skriv_vektlista_till_clipboard = FALSE){
  retur_varden <- hamta_giltiga_varden_fran_tabell(api_url = api_url, variabel = variabel, kod_OCH_klartext = TRUE)
  #if (skriv_vektlista_till_clipboard) writeClipboard(retur_varden$klartext %>% paste0('"', ., '"', collapse = ", "))
  if (skriv_vektlista_till_clipboard) {
    clipboard_txt <- retur_varden$klartext %>% paste0('"', ., '"', collapse = ", ")
    writeLines(text = clipboard_txt, con = "clipboard", sep = "")
  }
  
  
  return(retur_varden)
}

hamta_giltiga_varden_fran_tabell <- function(api_url, variabel, klartext = FALSE, kod_OCH_klartext = FALSE){
  
  # om vi skickar med en metadata-lista (som vi får genom att köra "px_meta <- pxweb_get(api_url)") så använder vi den direkt
  # om det är en url så hämtar vi data med den
  metadata <- if(is.list(api_url)) api_url else pxweb_get(api_url)
  
  ind <- which(tolower(sapply(metadata$variables, "[[", 1)) == tolower(variabel))  # vi kollar om skickad variabel finns som kod
  if (length(ind) < 1) ind <- which(tolower(sapply(metadata$variables, "[[", 2)) == tolower(variabel)) # om inte variabeln finns som kod så kollar vi om den finns som text
  
  if (length(ind) > 0){
    # välj om vi ska hämta koder eller klartext
    if (klartext) retur_varden <- metadata$variables[[ind]]$valueTexts else retur_varden <- metadata$variables[[ind]]$values
    if (kod_OCH_klartext) retur_varden <- data.frame(kod = metadata$variables[[ind]]$values, klartext = metadata$variables[[ind]]$valueTexts)
  } else {
    warning("Variabeln ", variabel, " hittades inte i tabellen med url: ", api_url, ". Kontrollera stavningen eller att variabeln finns i aktuell tabell.")
    retur_varden <- NULL
  }
  return(retur_varden)
}

hamta_kod_med_klartext <- function(api_url, klartext_varde, skickad_fran_variabel = NA, returnera_alltid_snoflinga = TRUE){
  
  if (returnera_alltid_snoflinga & all(klartext_varde == "*")) {
    return("*")
  } else {
    retur_kod <- sla_upp_varde_klartext_kod(api_url = api_url, 
                                            fran_varde = klartext_varde, 
                                            klartext = TRUE,
                                            fran_variabel = skickad_fran_variabel)
    return(retur_kod)
  }
}

hamta_klartext_med_kod <- function(api_url, kod_varde, skickad_fran_variabel = NA){
  retur_klartext <- sla_upp_varde_klartext_kod(api_url = api_url, 
                                               fran_varde = kod_varde, 
                                               klartext = FALSE,
                                               fran_variabel = skickad_fran_variabel)
  return(retur_klartext)
}

sla_upp_varde_klartext_kod <- function(api_url, fran_varde, klartext = TRUE, fran_variabel = NA) {
  
  if (is.list(api_url)) {                   # om vi vill hämta värde från en medskickad lista och inte en url
    if (is.na(fran_variabel)) stop("fran_variabel måste anges när klartext eller kod ska hämtas i en lista.")
    
    varde <- hamta_kod_eller_klartext_fran_lista(lista = api_url, 
                                                 klartext_eller_kod_varde = fran_varde,
                                                 skickad_fran_variabel = fran_variabel,
                                                 hamta_kod = klartext)
    return(varde)
    
  } else {                                 # om värde ska hämtas via API från SCB-pxweb, dvs. en url
    
    metadata <- pxweb_get(api_url)          # hämta metadata för aktuell tabell
    
    if (klartext) {                        # tilldela variabler fran_kol och till_kol beroende
      fran_kol <- "valueTexts"             # på om vi ska översätta från klartext till kod eller
      till_kol <- "values"                 # eller tvärtom
    } else {
      fran_kol <- "values"
      till_kol <- "valueTexts"
    }
    varde <- NULL
    
    if (is.na(fran_variabel)){         # om man inte skickat med vilken variabel som värdet tillhör
      for (var_nr in 1:length(fran_varde)) {
        # ta reda på var i listan som klartext-variabeln alternativt koden finns och hämta variabelns kod alternativt klartext
        #ind_var <- which(str_detect(tolower(metadata$variables), tolower(paste0("\\b", fran_varde[var_nr], "\\b"))))
        sok_varde <- fran_varde[var_nr] %>% str_replace("\\(", "\\\\(") %>% str_replace("\\)", "\\\\)")
        sok_varde_utan_escape <- fran_varde[var_nr]
        ind_var <- which(str_detect(tolower(metadata$variables), tolower(paste0(sok_varde))))
        
        # om söksträngen finns i två variabler, kolla om söksträngen är precis likadan som de värdena i de variabler som hittats, annars ta den första variabeln
        if (length(ind_var) > 1) {
          # bläddra igenom alla dubletter som söksträngen finns i
          vald_dublett <- ind_var[1]            # vi väljer det första värdet i dublett-vektorn, det kommer att gälla om ingen dublett är identisk med söksträngen 
          hittat_identisk <- FALSE              # variabel som anger om vi hittat någon sökträff-dublett som är identisk med skickat värde
          for (dublett in ind_var){
            # om fran_varde är exakt lika med söksträngen som kontrolleras så blir det 
            sok_traffar <- str_which(sok_varde, metadata$variables[[dublett]][[fran_kol]])
            if (length(sok_traffar) == 0) sok_traffar <- str_which(sok_varde_utan_escape, metadata$variables[[dublett]][[fran_kol]])
            if (length(sok_traffar) > 0) { 
              vald_dublett <- dublett            # tilldela index för variablen där vi får exakt sökträff 
              hittat_identisk <- TRUE
            }
          } # slut for-loop där vi loopar igenom sökträffarna för att kontrollera om någon av dem är identiskt med det skickade värdet
          if (!hittat_identisk) print(paste0("Flera värden matchar det skickade värdet men ingen av träffarna är identisk så första träffen returneras. Om det inte är den som önskas, kontrollera att det skickade värdet är korrekt."))
          ind_var <- vald_dublett          # vi väljer index för första variabeln
        } # slut if-sats som kontrollerar om vi har fler variabler med träff på skickat klartext-/kod-värde
        
        # tilldela index för själva klartext-/kodvärdet
        ind_kod <- which(str_detect(tolower(metadata$variables[[ind_var]][[fran_kol]]), tolower(paste0(sok_varde))))
        if (length(ind_kod) < 1) stop(paste0('Värdet "', sok_varde, '" finns inte i tabellen. Korrigera värdet och försök igen.'))      
        # om söksträngen finns två gånger, kolla om söksträngen är precis likadan som de variabler som hittats, annars ta den första variabeln
        if (length(ind_kod) > 1) {
          # bläddra igenom alla dubletter som söksträngen finns i
          vald_dublett <- ind_kod[1]            # vi väljer det första värdet i dublett-vektorn, det kommer att gälla om ingen dublett är identisk med söksträngen 
          hittat_identisk <- FALSE              # variabel som anger om vi hittat någon sökträff-dublett som är identisk med skickat värde
          for (dublett in 1:length(ind_kod)){
            # om fran_varde är exakt lika med söksträngen som kontrolleras så blir det 
            if (sok_varde == metadata$variables[[ind_var]][[fran_kol]][[ind_kod[dublett]]] |
                sok_varde_utan_escape == metadata$variables[[ind_var]][[fran_kol]][[ind_kod[dublett]]]) {
              vald_dublett <- ind_kod[dublett]
              hittat_identisk <- TRUE
            }
          } # slut for-loop där vi loopar igenom sökträffarna för att kontrollera om någon av dem är identiskt med det skickade värdet
          if (!hittat_identisk) print(paste0("Flera värden matchar det skickade värdet men ingen av träffarna är identisk så första träffen returneras. Om det inte är den som önskas, kontrollera att det skickade värdet är korrekt."))
          ind_kod <- vald_dublett
        } # slut if-sats som kontrollerar om vi har flera sökträffar
        
        varde <- c(varde, metadata$variables[[ind_var]][[till_kol]][[ind_kod]])
      }
    } else  {
      for (var_nr in 1:length(fran_varde)) {
        
        # vi kollar vilket element i metadata-variabellistan fran-variablen finns, dvs. vilken variabel det är
        ind <- which(tolower(sapply(metadata$variables, "[[", 1)) == tolower(fran_variabel))
        
        # om inte variabeln finns som kod så kollar vi om den finns som text
        if (length(ind) < 1) ind <- which(tolower(sapply(metadata$variables, "[[", 2)) == tolower(fran_variabel))
        
        # ta reda på var i listan över variabelns värden som klartext- alternativt kod-värdet finns                 # gammal kod, används ej längre: ind_var <- which(str_detect(tolower(metadata$variables[ind]), tolower(paste0("\\b", fran_varde[var_nr], "\\b"))))
        #ind_kod <- which(str_detect(tolower(metadata$variables[[ind]][[fran_kol]]), tolower(paste0("\\b", fran_varde[var_nr], "\\b"))))
        # test, raden ovan var från början men vad gör \\b egentligen
        sok_varde <- fran_varde[var_nr] %>% str_replace("\\(", "\\\\(") %>% str_replace("\\)", "\\\\)")
        sok_varde_utan_escape <- fran_varde[var_nr]
        ind_kod <- which(str_detect(tolower(metadata$variables[[ind]][[fran_kol]]), tolower(paste0(sok_varde))))
        if (length(ind_kod) < 1) stop(paste0('Värdet "', sok_varde, '" finns inte i tabellen. Korrigera värdet och försök igen.'))
        
        # om söksträngen finns två gånger, kolla om söksträngen är precis likadan som de variabler som hittats, annars ta den första variabeln
        if (length(ind_kod) > 1) {
          # bläddra igenom alla dubletter som söksträngen finns i
          vald_dublett <- ind_kod[1]            # vi väljer det första värdet i dublett-vektorn, det kommer att gälla om ingen dublett är identisk med söksträngen 
          hittat_identisk <- FALSE              # variabel som anger om vi hittat någon sökträff-dublett som är identisk med skickat värde
          for (dublett in ind_kod){
            # om fran_varde är exakt lika med söksträngen som kontrolleras så blir det 
            if (sok_varde == metadata$variables[[ind]][[fran_kol]][[dublett]] |
                sok_varde_utan_escape == metadata$variables[[ind]][[fran_kol]][[dublett]]) {
              vald_dublett <- dublett
              hittat_identisk <- TRUE
            }
          } # slut for-loop där vi loopar igenom sökträffarna för att kontrollera om någon av dem är identiskt med det skickade värdet
          if (!hittat_identisk) print(paste0("Flera värden matchar det skickade värdet men ingen av träffarna är identisk så första träffen returneras. Om det inte är den som önskas, kontrollera att det skickade värdet är korrekt."))
          ind_kod <- vald_dublett
        } # slut if-sats som kontrollerar om vi har flera sökträffar
        
        # När vi har variabelns plats i metadatalistan över variabler (ind), och även värdets plats
        # i variablens lista över värden (ind_kod) så kan vi hämta variabelns kod alternativt 
        # klartext (vilket det blir styrs av till_kol)
        varde <- c(varde, metadata$variables[[ind]][[till_kol]][[ind_kod]])
        
      } # slut for-loop för att gå igenom alla medskickad koder eller klartext-värden
    } # slut if-sats för att avgöra om man skickat med vilken variabel det gäller, eller inte (fran_variabel)
    
    return(varde)       # returnera koder eller klartext-värden
  } # slut if-sats där det testas om vi skickat en lista eller en url i en vektor
} # slut funktion

# används för att kunna skicka en pxweb-metadatalista, ett klartext
hamta_kod_eller_klartext_fran_lista <- function(lista, klartext_eller_kod_varde, skickad_fran_variabel, hamta_kod = TRUE) {
  
  if (hamta_kod) {
    retur_val <- "values"
    hamta_val <- "valueTexts"
  } else {
    retur_val <- "valueTexts"
    hamta_val <- "values"
  }
  
  # Kontrollera om listan har två element och de heter "title" och "variables", om så extraheras bara "variables"-listan och används
  if(length(lista) == 2 && all(c("title", "variables") %in% names(lista))) {
    lista <- lista$variables
  }
  
  # Hitta det element i listan som har den angivna koden
  list_element <- lista %>% 
    keep(~ tolower(.x$code) %in% tolower(skickad_fran_variabel)) %>% 
    first()
  
  # Matcha 'valueTexts' med det angivna klartextvärdet och hämta motsvarande 'values'
  if (!is.null(list_element)) {
    if (all(klartext_eller_kod_varde == "*")) {
      return(list_element[[retur_val]])
    } else {
      matchande_index <- which(tolower(list_element[[hamta_val]]) %in% tolower(klartext_eller_kod_varde))
      return(list_element[[retur_val]][matchande_index])
    } # slut if-sats om klartext_eller_kod_varde == *
  } else {       # nedan är om ingen matchning hittas
    warning("skickad_fran_variabel hittades inte som variabel i aktuell tabell.")
    return(NULL) # Ingen matchning hittades
  }
} # slut funktion hamta_kod_eller_klartext_fran_lista


# används för att konvertera dataset som returneras i pxweb där innehållsvariabler ligger i 
# wideformat, vilket sker om man laddar hem fler än en innehållsvariabel. Denna funktion kör en
# pivot_longer på alla innehållsvariabler
konvertera_till_long_for_contentscode_variabler <- function(skickad_df, 
                                                            api_url, 
                                                            content_var = "variabel", 
                                                            varde_var = "varde",           # kolumnen
                                                            content_kolumner = NA          # används om man har varit tvungen att döpa om innehållsvariablerna och vill skicka med dem som text istället för att de hämtas ur meta (ibland fallet med bas men oftast inte)
) {
  pivot_kol <- if (all(is.na(content_kolumner))) {
    hamta_giltiga_varden_fran_tabell(api_url, "contentscode", klartext = TRUE)
  } else {
    content_kolumner
  }
  
  if (content_var %in% names(skickad_df)) content_var == "vardekategori"               # om det redan finns en kolumn som heter "variabel" (det förekommer i vissa tabeller på SCB)
  if (varde_var %in% names(skickad_df)) varde_var == "vardevariabel"                   # om det redan finns en kolumn som heter "varde" 
  
  pivot_kol <- pivot_kol[pivot_kol %in% names(skickad_df)]                              # ta bort eventuella variabler som inte finns i dataframen
  if (length(pivot_kol) == 0 ) stop("Medskickad content_kolumner finns inte i skickad_df. Kontrollera content_kolumner och försök igen.")
  
  retur_df <- skickad_df %>% 
    pivot_longer(cols = any_of(pivot_kol), names_to = content_var, values_to = varde_var)
  return(retur_df)
}

kontrollera_pxweb_variabelvarden <- function(api_url,                                  # url till aktuell tabell - alternativt en metadata-lista
                                             var_lista,                                # skicka med lista med variabelnamn och värden som ska göras uttag för (query_list)
                                             visa_alla_ogiltiga_varden = TRUE,        # visa alla ogilitiga värden även om det finns giltiga värden för variabeln
                                             stoppa_om_ogiltiga_varden_finns = FALSE) { # TRUE om körningen ska stoppas om det är någon variabel som saknar giltiga värden
  
  alla_giltiga <- map2(var_lista, names(var_lista), ~ hamta_giltiga_varden_fran_tabell(api_url, .y))    # alla giltiga värden för aktuell tabell hämtas
  retur_list <- map2(var_lista, alla_giltiga, ~ .x[.x %in% .y])           # skapa lista med alla giltiga värden som skickats med i var_lista, denna returneras
  antal_rader <- map(retur_list, ~ length(.)) %>% as.character()          # ta ut antal rader för varje variabel som character, "0" om det saknas giltiga värden
  noll_index <- str_which(antal_rader, "0")                               # index för variabler som saknar giltiga värden
  var_saknar_giltiga_varden <- list_komma_och(names(retur_list)[noll_index]) # en sträng med variabelnamn som saknar giltiga värden
  
  # kontrollera om det finns variabler som saknar giltiga värden, i så fall stoppas skriptet
  if (length(noll_index) > 0) stop(paste0("Variablerna ", var_saknar_giltiga_varden, " saknar giltiga värden. Skriptet stoppas."))
  
  # här hanteras ogiltiga värden för variabler som också innehåller giltiga värden, om man vill visa alla ogiltiga värden
  ej_giltiga <- map2(var_lista, alla_giltiga, ~ .x[!.x %in% .y])          # skapa lista med alla ogilitiga värden som skickats med var_lista 
  ej_gilt_rader <-  map(ej_giltiga, ~ length(.)) %>% as.character()          # ta ut antal rader för varje variabel som character, "0" om det saknas giltiga värden
  ej_gilt_rader[ej_gilt_rader != "0"] <- 1                                # sätt alla variabler till "1" som har ogilitiga värden
  ej_gilt_index <- str_which(ej_gilt_rader, "1")                          # index för variabler som innehåller något ogiltigt värde
  var_har_ogiltiga_varden <- list_komma_och(names(ej_giltiga)[ej_gilt_index])    # en sträng med variabelnamn som har ogiltiga värden
  
  # om det finns ogiltiga värden för variabler som också har giltiga värden
  if (any(ej_gilt_rader != "0")){
    if (stoppa_om_ogiltiga_varden_finns) {               # om man ställt in stoppa_om_ogiltiga_varden_finns så stoppas skriptet om ogiltiga värden finns (även om det också finns giltiga värden)
      print(paste0("Variablerna ", var_har_ogiltiga_varden, " innehåller följande ogiltiga värden:"))
      print(ej_giltiga[ej_gilt_index])
      stop("Parametern stoppa_om_ogiltiga_varden_finns är inställd på TRUE och skriptet stoppas.")
    } else {  # det finns ogiltiga värden för variabler som också har giltiga värden, skriptet fortsätter då stoppa_om_ogiltiga_varden_finns är satt till FALSE
      print(paste0("Variablerna ", var_har_ogiltiga_varden, " innehåller följande ogiltiga värden:"))
      print(ej_giltiga[ej_gilt_index])
      print("Parametern stoppa_om_ogiltiga_varden_finns är inställd på FALSE och skriptet fortsätter därför köras men utan dessa ogiltiga värden.")
    }
  }
  return(retur_list)
}

region_kolumn_splitta_kod_klartext <- function(skickad_df, regionkolumn, tabortgammalkolumn = TRUE, separator = " "){
  # används på en regionkolumnn (län, kommun eller annat) där kod och klartext
  # ligger i samma kolumn. Funktionen splittar kolumnen i två kolumner, en för 
  # kod och en för klartext på första förekomsten av separator (default är mellanslag)
  
  retur_df <- skickad_df %>% 
    separate(!!sym(regionkolumn), into = c("regionkod", "region"), sep = separator, extra = "merge", remove = tabortgammalkolumn)
  
}

svenska_tecken_byt_ut <- function(textstrang){
  # funktion för att ta bort prickar över svenska tecken
  suppress_specific_warning(
    if (!require("stringi")) install.packages("stringi")
    ,"built under R version")
  retur_strang <- stri_trans_general(textstrang, "Latin-ASCII")
  return(retur_strang)
}

hamta_regionkod_med_knas_regionkod <- function(api_url, skickade_regionkoder, skickad_fran_variabel, dubbelkolumn = "klartext", 
                                               returnera_nyckeltabell = FALSE                  # om man vill returnera både felaktiga och korrekta koder
){
  # används för de myndigheter som hittar på egna läns- och kommunkoder
  # men lägger de riktiga tillsammans med klartext i klartextkolumnen
  # det funkar att skicka en api-url men också en lista med pxmeta-data
  
  pxmeta <- if (is.list(api_url)) api_url else pxweb_get(api_url)  
  
  regionkodtabell <- pxvardelist(pxmeta, skickad_fran_variabel) %>% 
    region_kolumn_splitta_kod_klartext(dubbelkolumn)
  
  retur_regionkoder <- regionkodtabell
  # sortera ut skickade regionkoder om inte "*" skickats
  if (!all(skickade_regionkoder == "*")) {
    retur_regionkoder <- retur_regionkoder %>%
      filter(regionkod %in% skickade_regionkoder)
  }
  
  if (!returnera_nyckeltabell) { 
    retur_regionkoder <- retur_regionkoder %>% 
      dplyr::pull(kod)
  } else {
    retur_regionkoder <- retur_regionkoder %>% 
      rename(felaktig_kod = kod)
  }
  
  return(retur_regionkoder)
  
}

regsokoder_bearbeta <- function(api_url, regsokoder, var_namn = "region", behall_bara_regsokoder = TRUE) {
  # används för extrahera regsokoder när man skickar med läns- och kommunkoder
  # så man skickar deso-, läns- elle kommunkoder och får regsokoder i retur
  # alla regsokoder för de kommuner eller län man skickar koder för
  # anropar tatortskoder_bearbeta så se beskrivning av den funktionen nedan
  # går att skicka api_url eller en lista med pxmeta-data
  #
  # i vissa tabeller kan man ta ut både regsokoder och kommuner, om behall_bara_regsokoder sätts
  # till TRUE så behåller man bara regsokoderna för dessa kommuner men inte kommunkoden för hela kommunen
  
  regsokoder_retur <- tatortskoder_bearbeta(api_url, regsokoder, var_namn)
  
  if (behall_bara_regsokoder) {
    regsokoder_retur <- regsokoder_retur[str_length(regsokoder_retur) > 4]
  }
  
  return_(regsokoder_retur)
}

desokoder_bearbeta <- function(api_url, desokoder, var_namn = "region", behall_bara_desokoder = TRUE) {
  # används för extrahera desokoder när man skickar med läns- och kommunkoder
  # så man skickar deso-, läns- elle kommunkoder och får desokoder i retur
  # alla desokoder för de kommuner eller län man skickar koder för
  # anropar tatortskoder_bearbeta så se beskrivning av den funktionen nedan
  # går att skicka api_url eller en lista med pxmeta-data
  #
  # i vissa tabeller kan man ta ut både desokoder och kommuner, om behall_bara_desokoder sätts
  # till TRUE så behåller man bara desokoderna för dessa kommuner men inte kommunkoden för hela kommunen
  
  desokoder_retur <-  tatortskoder_bearbeta(api_url, desokoder, var_namn)
  
  if (behall_bara_desokoder) {
    desokoder_retur <- desokoder_retur[str_length(desokoder_retur) > 4]
  }
  
  return(desokoder_retur)
}

tatortskoder_bearbeta <- function(api_url, tatortskoder, var_namn = "region") {
  # används för extrahera tätortskoder när man skickar med läns- och kommunkoder
  # med denna funktion fungerar det att skicka tätortskoder men också
  # läns, eller kommunkoder där man i dessa fall får samtliga tätortskoder för 
  # de kommuner eller län som man skickat koder för
  # det funkar att skicka en api-url men också en lista med pxmeta-data
  
  if (all(tatortskoder == "*")) return(tatortskoder) else {
    pxmeta <- if (is.list(api_url)) api_url else pxweb_get(api_url)
    
    # hämtar alla giltiga tätortskoder
    giltiga_tatortskoder <- hamta_giltiga_varden_fran_tabell(
      api_url = pxmeta,
      variabel = var_namn
    )
    
    # sorterar ut läns-, kommun- och tätortskoder för sig
    kommun_koder <- tatortskoder[str_length(tatortskoder) == 4]
    lan_koder <- tatortskoder[str_length(tatortskoder) == 2]
    tatort_koder <- tatortskoder[str_length(tatortskoder) > 4]
    
    # extraherar giltiga tätortskoder för läns- och kommun-koder samt behåll bara giltiga tätortskoder
    tatortskoder_kommun <- giltiga_tatortskoder[str_sub(giltiga_tatortskoder, 1, 4) %in% kommun_koder]
    tatortskoder_lan <- giltiga_tatortskoder[str_sub(giltiga_tatortskoder, 1, 2) %in% lan_koder]
    tatort_koder <- giltiga_tatortskoder[giltiga_tatortskoder %in% tatort_koder]
    
    # slå ihop de tre vektorerna ovan till en vektor
    tatortskoder_retur <- c(tatortskoder_kommun, tatortskoder_lan, tatort_koder)
    
    return(tatortskoder_retur)
    
  } # slut if-sats att det inte är "*" medskickat 
} # slut funktion

# ================================================= kolada-funktioner ========================================================

#' 
#' 
#' # Globala inställningar för rate limiting
#' .kolada_env <- new.env()
#' .kolada_env$senaste_anrop <- 0
#' .kolada_env$min_intervall <- 0.2  # 5 requests/sekund
#' 
#' # ============================================
#' # INTERNA HJÄLPFUNKTIONER
#' # ============================================
#' 
#' #' Intern: Rate limiting
#' #' @keywords internal
#' intern_kolada_throttle <- function() {
#'   nuvarande_tid <- as.numeric(Sys.time())
#'   forlöpt_tid <- nuvarande_tid - .kolada_env$senaste_anrop
#'   
#'   if (forlöpt_tid < .kolada_env$min_intervall) {
#'     vantetid <- .kolada_env$min_intervall - forlöpt_tid
#'     Sys.sleep(vantetid)
#'   }
#'   
#'   .kolada_env$senaste_anrop <- as.numeric(Sys.time())
#' }
#' 
#' #' Intern: Gör ett enskilt API-anrop
#' #' @keywords internal
#' intern_kolada_request <- function(endpoint, params = NULL, bas_url = "https://api.kolada.se/v3/") {
#'   intern_kolada_throttle()
#'   
#'   url <- paste0(bas_url, endpoint)
#'   response <- GET(url, query = params, timeout(30))
#'   
#'   # Hantera rate limiting från API:et
#'   if (status_code(response) == 429) {
#'     retry_after <- as.numeric(headers(response)$`retry-after` %||% 60)
#'     warning(sprintf("API rate limit nådd. Väntar %d sekunder...", retry_after))
#'     Sys.sleep(retry_after)
#'     return(intern_kolada_request(endpoint, params, bas_url))
#'   }
#'   
#'   # Hantera fel
#'   if (status_code(response) != 200) {
#'     stop(sprintf("API-fel %d: %s", status_code(response), content(response, "text")))
#'   }
#'   
#'   content(response, "text", encoding = "UTF-8") %>%
#'     jsonlite::fromJSON(simplifyVector = FALSE)
#' }
#' 
#' #' Intern: Hantera paginering
#' #' @keywords internal
#' intern_kolada_paginera <- function(endpoint, params = NULL, visa_progress = FALSE) {
#'   params <- params %||% list()
#'   params$page <- 1
#'   params$per_page <- 5000  # Max som API:et tillåter
#'   
#'   alla_objekt <- list()
#'   totalt_antal <- NULL
#'   
#'   if (visa_progress) message(sprintf("Hämtar %s...", endpoint))
#'   
#'   repeat {
#'     resultat <- intern_kolada_request(endpoint, params)
#'     
#'     objekt <- resultat$values %||% list()
#'     alla_objekt <- c(alla_objekt, objekt)
#'     
#'     # Visa progress om vi vet totalt antal
#'     if (is.null(totalt_antal) && !is.null(resultat$count)) {
#'       totalt_antal <- resultat$count
#'       if (visa_progress) message(sprintf("Totalt antal: %d", totalt_antal))
#'     }
#'     
#'     if (visa_progress && !is.null(totalt_antal)) {
#'       message(sprintf("Progress: %d/%d", length(alla_objekt), totalt_antal))
#'     }
#'     
#'     # Kolla om det finns fler sidor
#'     if (is.null(resultat$next_url) || resultat$next_url == "") break
#'     
#'     params$page <- params$page + 1
#'   }
#'   
#'   if (visa_progress) message(sprintf("Hämtade %d objekt totalt", length(alla_objekt)))
#'   
#'   alla_objekt
#' }
#' 
#' #' Intern: Dela upp i batches och kombinera resultat
#' #' @keywords internal
#' intern_kolada_batcha <- function(endpoint, params, batch_params, 
#'                                  max_batch_storlek = 25, visa_progress = TRUE) {
#'   
#'   # Extrahera parametrar som behöver batching
#'   batch_param_varden <- list()
#'   for (param in batch_params) {
#'     if (!is.null(params[[param]])) {
#'       batch_param_varden[[param]] <- params[[param]]
#'       params[[param]] <- NULL
#'     }
#'   }
#'   
#'   # Om inga parametrar behöver batching, gör vanlig paginerad request
#'   if (length(batch_param_varden) == 0) {
#'     return(intern_kolada_paginera(endpoint, params, visa_progress))
#'   }
#'   
#'   # Skapa batches för varje parameter
#'   param_batches <- list()
#'   for (param_namn in names(batch_param_varden)) {
#'     varden <- batch_param_varden[[param_namn]]
#'     # Dela upp i chunks
#'     antal_batches <- ceiling(length(varden) / max_batch_storlek)
#'     batches <- split(varden, ceiling(seq_along(varden) / max_batch_storlek))
#'     param_batches[[param_namn]] <- batches
#'   }
#'   
#'   # Skapa alla kombinationer av batches (för multipla batchade parametrar)
#'   param_namn <- names(param_batches)
#'   batch_index_lista <- lapply(param_batches, function(x) seq_along(x))
#'   batch_kombinationer <- expand.grid(batch_index_lista, stringsAsFactors = FALSE)
#'   
#'   alla_objekt <- list()
#'   totalt_batches <- nrow(batch_kombinationer)
#'   
#'   if (visa_progress) {
#'     message(sprintf("Bearbetar %d batch(ar) för %s...", totalt_batches, endpoint))
#'   }
#'   
#'   for (i in seq_len(totalt_batches)) {
#'     # Skapa params för denna batch
#'     batch_params_aktuell <- params
#'     
#'     # Lägg till batch-värden för varje parameter
#'     for (j in seq_along(param_namn)) {
#'       param_namn_aktuell <- param_namn[j]
#'       batch_idx <- batch_kombinationer[i, j]
#'       batch_params_aktuell[[param_namn_aktuell]] <- param_batches[[param_namn_aktuell]][[batch_idx]]
#'     }
#'     
#'     tryCatch({
#'       # Gör paginerad request för denna batch
#'       batch_objekt <- intern_kolada_paginera(endpoint, batch_params_aktuell, visa_progress = FALSE)
#'       alla_objekt <- c(alla_objekt, batch_objekt)
#'       
#'       if (visa_progress && (i %% 5 == 0 || i == totalt_batches)) {
#'         message(sprintf("  Slutförde batch %d/%d", i, totalt_batches))
#'       }
#'     }, error = function(e) {
#'       warning(sprintf("Fel i batch %d: %s", i, e$message))
#'     })
#'   }
#'   
#'   if (visa_progress) {
#'     message(sprintf("Totalt antal objekt från alla batches: %d", length(alla_objekt)))
#'   }
#'   
#'   alla_objekt
#' }
#' 
#' # ============================================
#' # ANVÄNDARFUNKTIONER (PUBLIKA API:ET)
#' # ============================================
#' 
#' #' Hämta rådata från Kolada API med automatisk batching
#' #'
#' #' @param kpi KPI-ID eller vektor av KPI-ID:n
#' #' @param kommun Kommun-ID eller vektor av kommun-ID:n
#' #' @param ar År eller vektor av år
#' #' @param ou Organisationsenhet-ID eller vektor av OU-ID:n
#' #' @param uppdaterad_sedan Filtrera data uppdaterad sedan detta datum (format: YYYY-MM-DD)
#' #' @param max_batch_storlek Maximalt antal ID:n per batch (standard: 25)
#' #' @param visa_progress Visa progress-meddelanden
#' #'
#' #' @return Lista med rådata från API:et
#' #'
#' #' @examples
#' #' # Hämta data för alla kommuner (ange inte kommun-parameter)
#' #' data <- kolada_hamta_radata(kpi = "N00945", ar = 2023)
#' #'
#' #' # Hämta data för specifika kommuner (automatisk batching om >25)
#' #' kommuner <- c("0180", "1480", "1280") # 290 kommuner
#' #' data <- kolada_hamta_radata(
#' #'   kpi = "N00945",
#' #'   kommun = kommuner,
#' #'   ar = c(2023, 2022, 2021)
#' #' )
#' #' @export
#' kolada_hamta_radata <- function(kpi = NULL,
#'                                 kommun = NULL,
#'                                 ar = NULL,
#'                                 ou = NULL,
#'                                 uppdaterad_sedan = NULL,
#'                                 max_batch_storlek = 25,
#'                                 visa_progress = TRUE) {
#'   
#'   # Bestäm endpoint
#'   if (!is.null(ou)) {
#'     endpoint <- "oudata/"
#'     params <- list()
#'     params$ou_id <- if (!is.list(ou)) as.list(ou) else ou
#'   } else {
#'     endpoint <- "data/"
#'     params <- list()
#'   }
#'   
#'   # Lägg till parametrar
#'   if (!is.null(kpi)) {
#'     params$kpi_id <- if (!is.list(kpi)) as.list(kpi) else kpi
#'   }
#'   
#'   if (!is.null(kommun)) {
#'     params$municipality_id <- if (!is.list(kommun)) as.list(kommun) else kommun
#'   }
#'   
#'   if (!is.null(ar)) {
#'     params$year <- if (!is.list(ar)) as.list(ar) else ar
#'   }
#'   
#'   if (!is.null(uppdaterad_sedan)) {
#'     params$from_date <- uppdaterad_sedan
#'   }
#'   
#'   # Bestäm vilka parametrar som behöver batching
#'   batch_params <- character(0)
#'   
#'   if (!is.null(params$kpi_id) && length(params$kpi_id) > max_batch_storlek) {
#'     batch_params <- c(batch_params, "kpi_id")
#'   }
#'   
#'   if (!is.null(params$municipality_id) && length(params$municipality_id) > max_batch_storlek) {
#'     batch_params <- c(batch_params, "municipality_id")
#'   }
#'   
#'   if (!is.null(params$ou_id) && length(params$ou_id) > max_batch_storlek) {
#'     batch_params <- c(batch_params, "ou_id")
#'   }
#'   
#'   if (!is.null(params$year) && length(params$year) > max_batch_storlek) {
#'     batch_params <- c(batch_params, "year")
#'   }
#'   
#'   # Hämta data med batching om nödvändigt
#'   if (length(batch_params) > 0) {
#'     data <- intern_kolada_batcha(endpoint, params, batch_params, max_batch_storlek, visa_progress)
#'   } else {
#'     data <- intern_kolada_paginera(endpoint, params, visa_progress)
#'   }
#'   
#'   data
#' }
#' 
#' #' Hämta data från Kolada API som en tidy data frame
#' #'
#' #' @inheritParams kolada_hamta_radata
#' #'
#' #' @return Tidy data frame med kolumner: kpi, kommun, ou, period, varde, kon, status, antal
#' #'
#' #' @examples
#' #' # Hämta data för alla kommuner
#' #' df <- kolada_hamta_df(kpi = "N00945", ar = 2023)
#' #'
#' #' # Hämta data för många kommuner (automatisk batching)
#' #' alla_kommuner <- kolada_hamta_kommuner()
#' #' df <- kolada_hamta_df(
#' #'   kpi = "N00945",
#' #'   kommun = alla_kommuner$id,
#' #'   ar = c(2023, 2022)
#' #' )
#' #' @export
#' kolada_hamta_df <- function(kpi = NULL,
#'                             kommun = NULL,
#'                             ar = NULL,
#'                             ou = NULL,
#'                             inkluderа_alla_geografier = FALSE,
#'                             uppdaterad_sedan = NULL,
#'                             max_batch_storlek = 25,
#'                             visa_progress = TRUE) {
#'   
#'   # API:et kräver minst två av tre parametrar: kpi_id, municipality_id, year
#'   # Om varken kommun eller år anges, hämta alla giltiga år från API:et
#'   if (is.null(kommun) && is.null(ar) && !is.null(kpi)) {
#'     ar <- kolada_hamta_giltiga_varden("data", "year", kpi_id = kpi)
#'     if (length(ar) == 0) {
#'       warning("Inga giltiga år hittades för angiven KPI. Försöker utan år...")
#'       ar <- NULL
#'     }
#'   }
#'   
#'   # Hämta rådata med automatisk batching
#'   data <- kolada_hamta_radata(
#'     kpi = kpi,
#'     kommun = kommun,
#'     ar = ar,
#'     ou = ou,
#'     uppdaterad_sedan = uppdaterad_sedan,
#'     max_batch_storlek = max_batch_storlek,
#'     visa_progress = visa_progress
#'   )
#'   
#'   if (length(data) == 0) {
#'     return(data.frame())
#'   }
#'   
#'   # Flatten nested structure till tidy data frame
#'   df <- map_dfr(data, function(objekt) {
#'     varden <- objekt$values
#'     if (is.null(varden) || length(varden) == 0) return(NULL)
#'     
#'     map_dfr(varden, function(val) {
#'       tibble(
#'         kpi = objekt$kpi %||% NA_character_,
#'         kommun = objekt$municipality %||% NA_character_,
#'         ou = objekt$ou %||% NA_character_,
#'         period = objekt$period %||% NA_character_,
#'         varde = val$value %||% NA_real_,
#'         kon = val$gender %||% NA_character_,
#'         status = val$status %||% NA_character_,
#'         antal = val$count %||% NA_integer_
#'       )
#'     })
#'   })
#'   
#'   df
#' }
#' 
#' #' Hämta alla kommuner och regioner från Kolada
#' #'
#' #' @param sokning Valfri sökterm för att filtrera på namn
#' #' @param typ Filtrera på typ ('K' för kommun, 'L' för landsting/region)
#' #'
#' #' @return Data frame med kommun/region-information
#' #'
#' #' @examples
#' #' # Hämta alla kommuner och regioner
#' #' alla <- kolada_hamta_kommuner()
#' #'
#' #' # Hämta endast kommuner
#' #' kommuner <- kolada_hamta_kommuner(typ = "K")
#' #'
#' #' # Hämta endast regioner/landsting
#' #' regioner <- kolada_hamta_kommuner(typ = "L")
#' #'
#' #' # Sök efter specifik kommun
#' #' stockholm <- kolada_hamta_kommuner(sokning = "Stockholm")
#' #' @export
#' kolada_hamta_kommuner <- function(sokning = NULL, typ = NULL) {
#'   params <- list()
#'   
#'   if (!is.null(sokning)) params$title <- sokning
#'   if (!is.null(typ)) params$type <- typ
#'   
#'   kommuner <- intern_kolada_paginera("municipality", params, visa_progress = FALSE)
#'   
#'   bind_rows(kommuner)
#' }
#' 
#' #' Sök efter KPI:er i Kolada
#' #'
#' #' @param sokning Sökterm för att filtrera KPI:er på titel
#' #' @param publiceringsdatum Filtrera på publiceringsdatum (YYYY-MM-DD)
#' #' @param verksamhetsomrade Filtrera på verksamhetsområde
#' #'
#' #' @return Data frame med KPI-information
#' #'
#' #' @examples
#' #' # Sök efter KPI:er relaterade till skola
#' #' skol_kpier <- kolada_kpi_sok("skola")
#' #'
#' #' # Sök efter KPI:er inom ett specifikt verksamhetsområde
#' #' halsa_kpier <- kolada_kpi_sok(verksamhetsomrade = "Hälso- och sjukvård")
#' #' @export
#' kolada_kpi_sok <- function(sokning = NULL, 
#'                            publiceringsdatum = NULL,
#'                            verksamhetsomrade = NULL) {
#'   params <- list()
#'   
#'   if (!is.null(sokning)) params$title <- sokning
#'   
#'   kpier <- intern_kolada_paginera("kpi", params, visa_progress = FALSE)
#'   
#'   # Filtrera på publiceringsdatum om angivet
#'   if (!is.null(publiceringsdatum)) {
#'     kpier <- Filter(function(k) !is.null(k$publication_date) && k$publication_date == publiceringsdatum, kpier)
#'   }
#'   
#'   # Filtrera på verksamhetsområde om angivet
#'   if (!is.null(verksamhetsomrade)) {
#'     kpier <- Filter(function(k) !is.null(k$operating_area) && k$operating_area == verksamhetsomrade, kpier)
#'   }
#'   
#'   bind_rows(kpier)
#' }
#' 
#' #' Hämta specifik KPI baserat på ID
#' #'
#' #' @param kpi_id KPI-ID att hämta
#' #'
#' #' @return Lista med KPI-information
#' #'
#' #' @examples
#' #' # Hämta en specifik KPI
#' #' kpi <- kolada_kpi_hamta("N00945")
#' #' print(kpi$title)
#' #' @export
#' kolada_kpi_hamta <- function(kpi_id) {
#'   resultat <- intern_kolada_request(paste0("kpi/", kpi_id))
#'   kpier <- resultat$values %||% list()
#'   
#'   if (length(kpier) == 0) {
#'     stop(sprintf("KPI med ID %s hittades inte", kpi_id))
#'   }
#'   
#'   kpier[[1]]
#' }
#' 
#' #' Hämta organisationsenheter från Kolada
#' #'
#' #' @param sokning Valfri sökterm för att filtrera på namn
#' #' @param kommun Filtrera på kommun-ID
#' #' @param ou_typ Filtrera på organisationsenhet-typ prefix (t.ex. 'V11' för förskolor)
#' #'
#' #' @return Data frame med organisationsenhet-information
#' #'
#' #' @examples
#' #' # Hämta alla organisationsenheter
#' #' alla_ou <- kolada_hamta_ou()
#' #'
#' #' # Hämta organisationsenheter för en specifik kommun
#' #' ou_stockholm <- kolada_hamta_ou(kommun = "0180")
#' #'
#' #' # Hämta endast förskolor (V11)
#' #' forskolor <- kolada_hamta_ou(ou_typ = "V11")
#' #' @export
#' kolada_hamta_ou <- function(sokning = NULL, kommun = NULL, ou_typ = NULL) {
#'   params <- list()
#'   
#'   if (!is.null(sokning)) params$title <- sokning
#'   if (!is.null(kommun)) params$municipality <- kommun
#'   
#'   enheter <- intern_kolada_paginera("ou", params, visa_progress = FALSE)
#'   
#'   # Filtrera på OU-typ om angivet
#'   if (!is.null(ou_typ)) {
#'     enheter <- Filter(function(u) !is.null(u$id) && startsWith(u$id, ou_typ), enheter)
#'   }
#'   
#'   bind_rows(enheter)
#' }
#' 
#' #' Ställ in anpassad rate limiting
#' #'
#' #' @param max_anrop_per_sekund Maximalt antal anrop per sekund (standard: 5)
#' #'
#' #' @examples
#' #' # Standard är 5 anrop/sekund
#' #' # Minska om du får problem:
#' #' kolada_satt_rate_limit(max_anrop_per_sekund = 2.0)
#' #' @export
#' kolada_satt_rate_limit <- function(max_anrop_per_sekund = 5.0) {
#'   .kolada_env$min_intervall <- 1.0 / max_anrop_per_sekund
#'   message(sprintf("Rate limit satt till %.1f anrop/sekund (%.3f sekunder mellan anrop)", 
#'                   max_anrop_per_sekund, .kolada_env$min_intervall))
#' }
#' 
#' 





hamta_kolada_giltiga_ar <- function(kpi_id, vald_region = "2080"){
  
  vald_region <- vald_region %>% str_pad(4, pad = "0")
  
  hamtade_varden <- get_values(
    kpi = kpi_id,
    municipality = vald_region,
    period = 1900:2060
  )
  
  alla_ar <- hamtade_varden$year %>% as.character() %>% unique()
  return(alla_ar)
}

# hamta_kolada_giltiga_ar <- function(kpi_id, vald_region = "2080") {
#   # Bygg API URL för v3
#   url <- paste0("https://api.kolada.se/v3/data/kpi/", kpi_id, 
#                 "/municipality/", vald_region)
#   
#   # Hämta data
#   response <- httr::GET(url)
#   
#   # Kontrollera att anropet lyckades
#   if (httr::status_code(response) != 200) {
#     stop("API-anrop misslyckades: ", httr::status_code(response))
#   }
#   
#   # Parsa JSON
#   data <- httr::content(response, as = "text", encoding = "UTF-8") %>%
#     jsonlite::fromJSON()
#   
#   # Extrahera alla unika år från values-arrayen
#   if (nrow(data$values) > 0) {
#     alla_ar <- data$values$period %>%
#       unique() %>%
#       sort()
#     
#     return(alla_ar)
#   } else {
#     warning("Inga data hittades för KPI ", kpi_id, " och kommun ", vald_region)
#     return(character(0))
#   }
# }
# 
# hamta_kolada_df <- function(kpi_id, 
#                             valda_kommuner, 
#                             valda_ar = NA, 
#                             konsuppdelat = TRUE,
#                             konsuppdelat_total_ta_bort = FALSE,
#                             dop_om_kolumner = TRUE){
#   
#   # Kontrollera att paketet är installerat
#   if (!requireNamespace("jsonlite", quietly = TRUE)) {
#     stop("Paketet 'jsonlite' krävs men är inte installerat. Installera det med: install.packages('jsonlite')")
#   }
#   
#   kolnamn_vektor <- c(ar = "period", regionkod = "municipality", region = "municipality_name",
#                       kon = "gender", variabelkod = "kpi",  
#                       variabel = "kpi_title", varde = "value") 
#   
#   valda_kommuner <- str_pad(valda_kommuner, 4, pad = "0")
#   
#   retur_alla <- map(kpi_id, ~ {
#     # Hämta giltiga år
#     alla_ar <- hamta_kolada_giltiga_ar(.x, valda_kommuner[1])
#     
#     # Kontrollera om det finns giltiga år
#     if (length(alla_ar) == 0) {
#       message("Inga giltiga år hittades för KPI ", .x, " och kommun ", valda_kommuner[1], ". Returnerar tom tibble.")
#       return(tibble::tibble())
#     }
#     
#     # Bestäm vilka år som ska hämtas
#     if (is.na(valda_ar[1])) {
#       hamta_ar <- alla_ar
#     } else if (all(valda_ar == "9999")) {
#       hamta_ar <- max(alla_ar)
#     } else {
#       hamta_ar <- valda_ar[valda_ar %in% alla_ar]
#     }
#     
#     # Bygg API URL för v3
#     kommuner_str <- paste(valda_kommuner, collapse = ",")
#     ar_str <- paste(hamta_ar, collapse = ",")
#     
#     url <- paste0("https://api.kolada.se/v3/data/kpi/", .x, 
#                   "/municipality/", kommuner_str,
#                   "/year/", ar_str)
#     
#     # Hämta data från API
#     response <- GET(url)
#     
#     if (status_code(response) != 200) {
#       stop("API-anrop misslyckades: ", status_code(response))
#     }
#     
#     # Parsa JSON
#     data <- content(response, as = "text", encoding = "UTF-8") %>%
#       jsonlite::fromJSON()
#     
#     # Hämta KPI-metadata för att få titel
#     kpi_url <- paste0("https://api.kolada.se/v3/kpi/", .x)
#     kpi_response <- GET(kpi_url)
#     kpi_data <- content(kpi_response, as = "text", encoding = "UTF-8") %>%
#       jsonlite::fromJSON()
#     kpi_title <- kpi_data$values$title
#     
#     # Transformera data till tidy format
#     retur_df <- data$values %>%
#       tidyr::unnest(values) %>%
#       dplyr::left_join(kolada_kommuntabell_hamta(), by = c("municipality" = "municipality")) %>%
#       dplyr::mutate(kpi_title = kpi_title) %>%
#       dplyr::select(kpi, kpi_title, municipality, municipality_name, period, gender, value)
#     
#     # Hantera könsuppdelning
#     if ("gender" %in% names(retur_df)) {
#       if (konsuppdelat && any(retur_df$gender %in% c("K", "M"))) {
#         if (konsuppdelat_total_ta_bort) retur_df <- retur_df %>% filter(gender != "T")
#       } else {
#         retur_df <- retur_df %>% filter(gender == "T")
#       }
#       
#       retur_df <- retur_df %>% 
#         mutate(gender = recode(gender, 
#                                "T" = "Båda könen",
#                                "K" = "Kvinnor",
#                                "M" = "Män"))
#     }
#     
#     # Döp om kolumner om önskat
#     if (dop_om_kolumner) {
#       retur_df <- retur_df %>% 
#         rename(
#           ar = period,
#           regionkod = municipality,
#           region = municipality_name,
#           kon = gender,
#           variabelkod = kpi,
#           variabel = kpi_title,
#           varde = value
#         ) %>%
#         mutate(
#           regionkod = if_else(regionkod == "0", "00", as.character(as.numeric(regionkod))),
#           region = str_remove(region, "Region ")
#         )
#     }
#     
#     return(retur_df)
#   }) %>% list_rbind()
#   return(retur_alla)
# }
# 
# # Skapa en global lookup-tabell (körs en gång)
# .kolada_kommun_lookup <- NULL
# 
# kolada_kommuntabell_hamta <- function() {
#   if (is.null(.kolada_kommun_lookup)) {
#     kommun_url <- "https://api.kolada.se/v3/municipality"
#     kommun_response <- httr::GET(kommun_url)
#     kommun_data <- httr::content(kommun_response, as = "text", encoding = "UTF-8") %>%
#       jsonlite::fromJSON()
#     
#     .kolada_kommun_lookup <<- kommun_data$values %>%
#       dplyr::select(id, title, type) %>%
#       dplyr::rename(municipality = id, municipality_name = title)
#   }
#   return(.kolada_kommun_lookup)
# }
# 
# kolada_kpi_grupper_hamta <- function(title = NULL) {
#   
#   # Kontrollera paket
# 
#   if (!requireNamespace("jsonlite", quietly = TRUE)) {
#     stop("Paketet 'jsonlite' krävs. Installera med: install.packages('jsonlite')")
#   }
#   
#   # Bygg API URL
#   url <- "https://api.kolada.se/v3/kpi_groups"
#   
#   # Lägg till title-filter om angivet
#   if (!is.null(title)) {
#     url <- paste0(url, "?title=", utils::URLencode(title))
#   }
#   
#   # Hämta data
#   response <- httr::GET(url)
#   
#   if (httr::status_code(response) != 200) {
#     stop("API-anrop misslyckades: ", httr::status_code(response))
#   }
#   
#   # Parsa JSON
#   data <- httr::content(response, as = "text", encoding = "UTF-8") %>%
#     jsonlite::fromJSON()
#   
#   # Returnera som dataframe
#   return(data$values)
# }

# Användning:
# kpi_grupper <- hamta_kolada_kpigrupper_v3()


hamta_kolada_df <- function(kpi_id, 
                            valda_kommuner, 
                            valda_ar = NA, 
                            konsuppdelat = TRUE, 
                            konsuppdelat_total_ta_bort = FALSE,
                            dop_om_kolumner = TRUE
){
  
  kolnamn_vektor <- c(ar = "year", regionkod = "municipality_id", region = "municipality",
                      #regiontyp = "municipality_type",
                      kon = "gender", variabelkod = "kpi",
                      variabel = "fraga", varde = "value")
  
  valda_kommuner <- valda_kommuner %>% str_pad(4, pad = "0")
  
  alla_ar <- hamta_kolada_giltiga_ar(kpi_id, valda_kommuner[1])
  senaste_ar <- max(alla_ar)
  start_ar <- min(alla_ar)
  
  #alla_giltiga_ar <- if(valda_ar == "9999") senaste_ar else valda_ar[valda_ar %in% alla_ar]
  hamta_ar <- if (all(is.na(valda_ar))) {
    alla_ar
  } else if (all(valda_ar == "9999", na.rm = TRUE)) {
    senaste_ar
  } else {
    valda_ar[valda_ar %in% alla_ar]
  }
  
  # alla_giltiga_ar <- if(all(valda_ar == "9999", na.rm = TRUE)) senaste_ar else valda_ar[valda_ar %in% alla_ar]
  # 
  # hamta_ar <- if (is.na(valda_ar[1])) alla_ar else alla_giltiga_ar
  
  #### Dra hem variablerna från Kolada
  hamtade_varden <- get_values(
    kpi = kpi_id,
    municipality = valda_kommuner,
    period = hamta_ar
  )
  
  # hämta frågenamnen från Kolada
  kpi_df <- get_kpi(kpi_id) %>% select(id, title)
  
  # Koppla på frågenamn som kolumnnamn samt beräkna om värde är över rikets
  retur_df <- hamtade_varden %>%
    left_join(kpi_df, by = c("kpi" = "id")) %>%
    rename(fraga = title)
  
  if ("gender" %in% names(retur_df)) {
    if (konsuppdelat & nrow(retur_df[retur_df$gender %in% c("K", "M"),])>0) {       # om man valt könsuppdelat och det finns värden för kvinnor eller män
      retur_df <- retur_df %>%
        filter(gender != "T")
      
    } else {
      if (konsuppdelat_total_ta_bort) retur_df <- retur_df %>% filter(gender == "T")
    } # slut if-sats om könsuppdelat är valt och det finns kvinnor och män i datasetet
    retur_df <- retur_df %>%
      mutate(gender = case_when(gender == "T" ~ "Båda könen",
                                gender == "K" ~ "Kvinnor",
                                gender == "M" ~ "Män"))
    
    if (!konsuppdelat) {
      finns_total_rader <- nrow(retur_df[retur_df$gender %in% c("Båda könen"),])>0
      if (finns_total_rader) retur_df <- retur_df %>% filter(gender == "Båda könen")
    }
    
  } # slut if-sats om kön finns med som variabel
  
  # gör om år till character
  retur_df <- retur_df %>%
    mutate(year = year %>% as.character())
  
  if (dop_om_kolumner) {
    retur_df <- retur_df %>%
      select(any_of(kolnamn_vektor)) %>%
      mutate(regionkod = regionkod %>% as.numeric() %>% as.character() %>% str_replace("^0$", "00"),
             region = region %>% str_remove("Region "))
  }
  return(retur_df)
}

# =========================================== Skolvkerket-funktioner =========================================================

skolverket_generera_kolumnnamn <- function(df, namnrad, separator = " ", konv_kolnamn_gemener = TRUE) {
  namnmatris <- df[1:namnrad, ] %>%
    #set_names(~ paste0("X", seq_along(.))) %>% 
    setNames(paste0("X", seq_len(ncol(.)))) %>%
    mutate(across(everything(), as.character)) %>%
    as.matrix()
  
  basnamn <- namnmatris[namnrad, ]
  dubbletter <- duplicated(basnamn) | duplicated(basnamn, fromLast = TRUE)
  
  hitta_första_icke_tomma_vänster <- function(rad, kolindex) {
    while (kolindex > 0) {
      värde <- namnmatris[rad, kolindex]
      if (!is.na(värde) && värde != "") return(värde)
      kolindex <- kolindex - 1
    }
    return("")  # Om inget hittas
  }
  
  skapa_namn <- function(kolindex) {
    namn <- character()
    for (rad in seq(namnrad, 1)) {
      värde <- namnmatris[rad, kolindex]
      
      # Om tomt, sök åt vänster
      if (is.na(värde) || värde == "") {
        värde <- hitta_första_icke_tomma_vänster(rad, kolindex - 1)
      }
      
      if (is.na(värde) || värde == "") break
      namn <- c(if (rad == namnrad && konv_kolnamn_gemener) tolower(värde) else värde, namn)
    }
    
    fullständigt_namn <- paste(namn, collapse = separator)
  }
  
  nya_namn <- seq_along(basnamn) %>%
    map_chr(~ if (dubbletter[.x]) skapa_namn(.x) else {
      namn <- basnamn[.x]
    })
  
  return(nya_namn)
}

skolverket_hitta_startrad <- function(df, kolumn = 1, min_längd = 5) {
  # Extrahera vektorn och konvertera till character
  vektor <- as.character(df[[kolumn]])
  
  # Skapa logisk vektor: TRUE där det finns värde, FALSE annars
  har_värde <- !is.na(vektor) & vektor != ""
  
  # Leta efter första position där minst `min_längd` TRUE i rad
  for (i in seq_len(length(har_värde) - min_längd + 1)) {
    if (all(har_värde[i:(i + min_längd - 1)])) {
      return(i)
    }
  }
  
  return(NA_integer_)  # Om ingen sådan rad hittas
}

# funktion som används nedan för att hämta gymnasieprogram- och inriktningskoder från Skolverket
gymnprg_inr_koder_hamta_api_skolverket <- possibly(function(url = "https://api.skolverket.se/planned-educations/v3/support/programs") {
  resp <- GET(url)
  stop_for_status(resp)
  resp_lista <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)$body
  gy_alla <- resp_lista %>%
    { .[str_detect(names(.), "gy")] } %>%      # behåll bara de vars namn innehåller "gy"
    imap(~ mutate(.x, skolform = .y))  %>%     # lägg till kolumn med skolform
    list_rbind() %>%                           # lägg ihop till en dataframe
    mutate(skolform = case_when(skolform == "gy" ~ "Gymnasieskola",
                                skolform == "gyan" ~ "Anpassad gymnasieskola",
                                TRUE ~ skolform))
  
  gy_df <- bind_rows(
    gy_alla %>% select(code, name, skolform),
    gy_alla %>% select(studyPaths, skolform) %>% unnest(studyPaths, keep_empty = TRUE)
  ) %>% distinct() %>% 
    filter(!is.na(code)) %>% 
    rename(Kod = code, 
           Namn = name)
  
  gy25namn <- gy_df %>% 
    filter(str_detect(Kod, "25")) %>% 
    mutate(Kod = Kod %>% str_remove_all("25"))
  
  gy_df <- gy_df %>% 
    bind_rows(gy25namn)
  
  return(gy_df)
}, otherwise = NULL)


# ====================== Läser in all data från pivottabeller i excelfiler utan att behöva stöka med filter etc. ===================

# ── Hjälpfunktion: läs en enskild cache ───────────────────
intern_excel_xml_lasa_pivot_cache <- function(xlsx_fil, cache_nr = 1) {
  
  # Läser pivottabellcachen direkt från en xlsx-fil och returnerar en data.frame.
  # Används av: excel_xml_las_fil()
  
  def_path <- paste0("xl/pivotCache/pivotCacheDefinition", cache_nr, ".xml")
  rec_path <- paste0("xl/pivotCache/pivotCacheRecords", cache_nr, ".xml")
  
  tmp <- tempfile()
  dir.create(tmp)
  unzip(xlsx_fil, files = c(def_path, rec_path), exdir = tmp)
  
  # 1. cacheDefinition: kolumnnamn + sharedItems (oförändrad)
  message("  Läser cacheDefinition...")
  def_doc <- read_xml(file.path(tmp, def_path))
  ns      <- xml_ns(def_doc)
  
  fields    <- xml_find_all(def_doc, ".//d1:cacheField", ns)
  col_namn  <- xml_attr(fields, "name")
  antal_kol <- length(col_namn)
  
  shared_items <- vector("list", antal_kol)
  for (i in seq_along(fields)) {
    items_el <- xml_find_first(fields[[i]], "d1:sharedItems", ns)
    if (!is.na(items_el)) {
      barn   <- xml_children(items_el)
      tags   <- xml_name(barn)
      varden <- ifelse(tags == "m", NA_character_, xml_attr(barn, "v"))
      shared_items[[i]] <- varden
    }
  }
  
  # 2. cacheRecords: helt vektoriserad parsning
  message("  Läser cacheRecords...")
  raw <- readLines(file.path(tmp, rec_path), warn = FALSE, encoding = "UTF-8")
  raw <- paste(raw, collapse = "")
  
  # Räkna antal rader från count-attributet i rotelementet (snabbare än att räkna </r>)
  antal_rader <- as.integer(regmatches(raw, regexpr('(?<=count=")[0-9]+', raw, perl = TRUE)))
  message(paste("  Antal rader:", antal_rader))
  
  # Extrahera alla celler i hela filen på en gång: tagg + värde + radavgränsare
  # Vi lägger till </r> som en sentinel så vi vet var varje rad slutar
  hits    <- gregexpr("<[xnbds] v=\"[^\"]*\"/>|</r>", raw, perl = TRUE)
  träffar <- regmatches(raw, hits)[[1]]
  
  # Identifiera vilka träffar som är radavgränsare
  är_radslut <- träffar == "</r>"
  
  # Tilldela radnummer till varje cell
  rad_nr <- cumsum(är_radslut)
  rad_nr <- rad_nr[!är_radslut] + 1L  # +1 för att rad_nr ökar EFTER </r>
  
  # Extrahera tagg och värde för alla celler
  celler  <- träffar[!är_radslut]
  taggar  <- substr(celler, 2, 2)          # tecknet efter "<"
  varden  <- regmatches(celler, regexpr('(?<=v=")[^"]+', celler, perl = TRUE))
  
  # Räkna ut kolumnposition per cell (position inom sin rad)
  kol_nr <- sequence(tabulate(rad_nr))
  
  # Förbered resultatvektorer
  resultat <- vector("list", antal_kol)
  names(resultat) <- col_namn
  for (i in seq_along(col_namn)) resultat[[i]] <- character(antal_rader)
  
  # Hantera x-celler (index till sharedItems) och direktvärden separat
  är_x <- taggar == "x"
  
  # Direktvärden (n, b, d, s) – helt vektoriserat
  if (any(!är_x)) {
    idx_direkt <- which(!är_x)
    for (k in seq_along(col_namn)) {
      mask <- idx_direkt[kol_nr[idx_direkt] == k]
      if (length(mask) > 0)
        resultat[[k]][rad_nr[mask]] <- varden[mask]
    }
  }
  
  # x-celler (sharedItems-uppslag) – vektoriserat per kolumn
  if (any(är_x)) {
    idx_x <- which(är_x)
    for (k in seq_along(col_namn)) {
      mask <- idx_x[kol_nr[idx_x] == k]
      if (length(mask) > 0) {
        idx <- as.integer(varden[mask]) + 1L
        resultat[[k]][rad_nr[mask]] <- shared_items[[k]][idx]
      }
    }
  }
  
  # 3. Bygg data.frame
  df <- as.data.frame(resultat, stringsAsFactors = FALSE)
  for (i in seq_along(col_namn)) {
    if (is.null(shared_items[[i]])) {
      df[[col_namn[i]]] <- as.numeric(df[[col_namn[i]]])
    }
  }
  
  unlink(tmp, recursive = TRUE)
  df
}



excel_xml_las_fil <- function(xlsx_fil, flikar = NULL) {
  
  # Läser pivottabelldata från en eller flera flikar i en xlsx-fil och returnerar
  # en namngiven lista med en data.frame per flik. Lägger automatiskt till
  # _klartext-kolumner för kolade variabler om mappningar hittas.
  # Använder: intern_excel_xml_lasa_pivot_cache(), intern_excel_xml_hamta_kodmappningar()
  
  # flikar: NULL = läs alla, annars en vektor med fliknamn (character)
  #         eller positioner (integer/numeric), t.ex.:
  #           excel_xml_las_fil(fil, flikar = "SNIAVD 2010-01-")
  #           excel_xml_las_fil(fil, flikar = c(1, 3))
  #           excel_xml_las_fil(fil, flikar = c("1996-01-", "Yrkesområde 201301-"))
  
  tmp <- tempfile()
  dir.create(tmp)
  
  rel_filer  <- c("xl/workbook.xml", "xl/_rels/workbook.xml.rels")
  alla_filer <- unzip(xlsx_fil, list = TRUE)$Name
  sheet_rels <- alla_filer[grepl("xl/worksheets/_rels/sheet.*\\.rels", alla_filer)]
  pivot_rels <- alla_filer[grepl("xl/pivotTables/_rels/pivotTable.*\\.rels", alla_filer)]
  
  unzip(xlsx_fil, files = c(rel_filer, sheet_rels, pivot_rels), exdir = tmp)
  
  # Läs kodmappningar en gång för hela filen
  kodmappningar <- intern_excel_xml_hamta_kodmappningar(xlsx_fil)
  
  # Läs bladnamn från workbook.xml
  wb_doc    <- read_xml(file.path(tmp, "xl/workbook.xml"))
  ns_wb     <- xml_ns(wb_doc)
  sheets    <- xml_find_all(wb_doc, ".//d1:sheet", ns_wb)
  blad_namn <- xml_attr(sheets, "name")
  blad_rid  <- xml_attr(sheets, "id")
  
  # Validera och översätt flikar-argumentet till en indexvektor
  if (is.null(flikar)) {
    urval <- seq_along(blad_namn)
  } else if (is.numeric(flikar)) {
    ogiltiga <- flikar[flikar < 1 | flikar > length(blad_namn)]
    if (length(ogiltiga) > 0)
      stop("Ogiltiga positioner: ", paste(ogiltiga, collapse = ", "),
           ". Filen har ", length(blad_namn), " flikar.")
    urval <- as.integer(flikar)
  } else if (is.character(flikar)) {
    ogiltiga <- flikar[!flikar %in% blad_namn]
    if (length(ogiltiga) > 0)
      stop("Fliknamn hittades inte: ", paste(ogiltiga, collapse = ", "),
           ".\nTillgängliga flikar: ", paste(blad_namn, collapse = ", "))
    urval <- match(flikar, blad_namn)
  } else {
    stop("flikar måste vara NULL, en character-vektor eller en numeric-vektor.")
  }
  
  message(paste0("Läser ", length(urval), " av ", length(blad_namn), " flikar."))
  
  # Bygg karta: rId → målfil
  wb_rels_doc     <- read_xml(file.path(tmp, "xl/_rels/workbook.xml.rels"))
  ns_rels         <- xml_ns(wb_rels_doc)
  rels            <- xml_find_all(wb_rels_doc, ".//d1:Relationship", ns_rels)
  rid_till_target <- setNames(xml_attr(rels, "Target"), xml_attr(rels, "Id"))
  
  resultat_lista <- vector("list", length(urval))
  names(resultat_lista) <- blad_namn[urval]
  
  for (j in seq_along(urval)) {
    i    <- urval[j]
    blad <- blad_namn[i]
    rid  <- blad_rid[i]
    sheet_target <- rid_till_target[rid]
    sheet_nr     <- sub(".*sheet(\\d+)\\.xml", "\\1", sheet_target)
    
    message(paste0("Flik [", j, "/", length(urval), "]: '", blad, "'"))
    
    sheet_rels_path <- file.path(
      tmp, "xl", "worksheets", "_rels", paste0("sheet", sheet_nr, ".xml.rels")
    )
    if (!file.exists(sheet_rels_path)) {
      message("  Ingen relations-fil, hoppar över.")
      next
    }
    
    sheet_rels_doc <- read_xml(sheet_rels_path)
    ns_sr          <- xml_ns(sheet_rels_doc)
    sr_rels        <- xml_find_all(sheet_rels_doc, ".//d1:Relationship", ns_sr)
    sr_types       <- xml_attr(sr_rels, "Type")
    sr_targets     <- xml_attr(sr_rels, "Target")
    
    pivot_idx <- which(grepl("pivotTable", sr_types))
    if (length(pivot_idx) == 0) {
      message("  Ingen pivottabell på detta blad, hoppar över.")
      next
    }
    
    pivot_target <- sr_targets[pivot_idx[1]]
    pivot_nr     <- sub(".*pivotTable(\\d+)\\.xml", "\\1", pivot_target)
    
    pt_rels_path <- file.path(
      tmp, "xl", "pivotTables", "_rels", paste0("pivotTable", pivot_nr, ".xml.rels")
    )
    pt_rels_doc  <- read_xml(pt_rels_path)
    ns_pt        <- xml_ns(pt_rels_doc)
    pt_rels      <- xml_find_all(pt_rels_doc, ".//d1:Relationship", ns_pt)
    cache_target <- xml_attr(pt_rels, "Target")[1]
    cache_nr     <- as.integer(sub(".*pivotCacheDefinition(\\d+)\\.xml", "\\1", cache_target))
    
    message(paste0("  → pivotCacheDefinition", cache_nr, ".xml"))
    
    df <- intern_excel_xml_lasa_pivot_cache(xlsx_fil, cache_nr)
    
    # Lägg till _klartext-kolumner direkt efter respektive kodkolumn
    matchande <- intersect(names(df), names(kodmappningar))
    for (kol in matchande) {
      pos    <- which(names(df) == kol)
      klartext <- kodmappningar[[kol]][df[[kol]]]
      df     <- data.frame(
        df[seq_len(pos)],
        klartext,
        if (pos < ncol(df)) df[seq(pos + 1, ncol(df))] else NULL,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(df)[pos + 1] <- paste0(kol, "_klartext")
      message(paste0("  Lade till kolumn '", kol, "_klartext'"))
    }
    resultat_lista[[blad]] <- df
    message(paste0("  Klar! (", nrow(df), " rader, ", ncol(df), " kolumner)"))
  }
  
  unlink(tmp, recursive = TRUE)
  message("\nKlart!")
  resultat_lista
}


intern_excel_xml_hamta_kodmappningar <- function(xlsx_fil) {
  
  # Letar efter kod-till-klartext-mappningar i en xlsx-fil genom att jämföra
  # sharedStrings med sharedItems i pivotcacherna. Returnerar en namngiven lista
  # med en namngiven vektor per matchande kolumn (t.ex. list(SNI2007_AVD = c(A = "Jordbruk...", ...)))
  # Används av: excel_xml_las_fil()
  
  tmp <- tempfile()
  dir.create(tmp)
  
  # Packa upp sharedStrings och alla cacheDefinitions
  alla_filer  <- unzip(xlsx_fil, list = TRUE)$Name
  cache_defs  <- alla_filer[grepl("pivotCache/pivotCacheDefinition", alla_filer)]
  unzip(xlsx_fil, files = c("xl/sharedStrings.xml", cache_defs), exdir = tmp)
  
  # Läs sharedStrings och extrahera möjliga "kod → beskrivning"-mappningar
  ss_doc   <- read_xml(file.path(tmp, "xl/sharedStrings.xml"))
  ns_ss    <- xml_ns(ss_doc)
  si_noder <- xml_find_all(ss_doc, ".//d1:si", ns_ss)
  strängar <- sapply(si_noder, function(si) {
    paste(xml_text(xml_find_all(si, ".//d1:t", ns_ss)), collapse = "")
  })
  
  # Behåll strängar som matchar "X beskrivning" (en stor bokstav + mellanslag + text)
  träffar <- strängar[grepl("^[A-ZÅÄÖ] {1,2}\\S", strängar)]
  koder   <- substr(träffar, 1, 1)
  beskr   <- trimws(substr(träffar, 2, nchar(träffar)))
  möjliga_mappningar <- setNames(beskr, koder)
  
  # För varje cacheDefinition: kolla vilka kolumner vars sharedItems
  # överlappar starkt med koderna i möjliga_mappningar
  resultat <- list()
  
  for (cache_fil in cache_defs) {
    cache_doc <- read_xml(file.path(tmp, cache_fil))
    ns_c      <- xml_ns(cache_doc)
    fields    <- xml_find_all(cache_doc, ".//d1:cacheField", ns_c)
    
    for (field in fields) {
      kolnamn  <- xml_attr(field, "name")
      items_el <- xml_find_first(field, "d1:sharedItems", ns_c)
      if (is.na(items_el)) next
      
      barn   <- xml_children(items_el)
      varden <- xml_attr(barn[xml_name(barn) == "s"], "v")
      varden <- varden[!is.na(varden)]
      if (length(varden) == 0) next
      
      # Hur stor andel av kolumnens värden finns i möjliga_mappningar?
      overlap <- varden[varden %in% names(möjliga_mappningar)]
      andel   <- length(overlap) / length(varden)
      
      if (andel > 0.3) {
        mappning <- möjliga_mappningar[varden[varden %in% names(möjliga_mappningar)]]
        resultat[[kolnamn]] <- mappning
        message(sprintf("Hittade mappning: kolumn '%s' (%d/%d koder matchade)",
                        kolnamn, length(overlap), length(varden)))
      }
    }
  }
  
  unlink(tmp, recursive = TRUE)
  invisible(resultat)  # Returnera den namngivna vektorn tyst
}



# =========================================== Bra generella funktioner =========================================================

ladda_funk_parametrar <- function(funktion, meddelanden = FALSE) {
  
  # funktion för att ladda in alla parametrars standardvärden i global environment
  # OBS! Den skriver över variabler som heter likadant i global environment
  
  funktionsnamn <- deparse(substitute(funktion)) %>% str_remove("\\(\\)")
  st_var <- formals({{funktionsnamn}})
  
  for (varname in names(st_var)) {
    # Kontrollera om parametern är en symbol, vilket betyder att den saknar ett standardvärde
    if (!is.symbol(st_var[[varname]])) {
      # Försök att utvärdera parametern om den har ett standardvärde
      assign(varname, eval(st_var[[varname]]), envir = .GlobalEnv)
    } else {
      # Parametern saknar standardvärde, hoppa över eller hantera på annat sätt
      if (meddelanden) message(paste("Parametern", varname, "saknar standardvärde och har inte laddats."))
    }
  }
  
} # slut funktion

lista_funktioner_i_skript <- function(filsokvag_eller_github_url) {
  # funktion som extraherar funktionsnamn ur R-filer
  
  # Använder map för att iterera över filvägar och extrahera funktionsnamnen
  retur_vekt <- map(filsokvag_eller_github_url, ~ {
    # avgör om det är en url från github eller en sökväg till en fil lokalt
    if (str_detect(filsokvag_eller_github_url, "^http")) {
      response <- GET(.x)
      text_content <- httr::content(response, "text", encoding = "UTF-8")
      r_kod <- unlist(str_split(text_content, "\n"))
    } else {
      r_kod <- read_lines(.x)  
    }
    
    if (length(r_kod) > 0) {
      funktionsnamn <- str_extract_all(r_kod, "\\b\\w+\\s*(?=\\s*<-\\s*function)") %>% unlist()
      unikaFunktionsnamn <- unique(funktionsnamn[funktionsnamn != ""]) %>% str_trim()
      
    } else {
      unikaFunktionsnamn <- NA
    }
    return(unikaFunktionsnamn)
  }) %>% unlist()
  return(retur_vekt <- retur_vekt[!is.na(retur_vekt)])
} 

ar_alla_kommuner_i_ett_lan <- function(reg_koder, tillat_lanskod = TRUE, tillat_rikskod = TRUE, returnera_text = FALSE, returtext = NA) {
  
  # kontrollerar om kommunkoderna som skickas till funktionen utgör alla kommuner i ett län
  # Man kan tillåta att länets länskod och att rikskoden ("00") ligger med också men inte kommuner
  # från andra län
  #
  # reg_koder         - är de regionkoder vi testar. Om alla kommer från samma län och utgör samtliga kommuner i 
  #                     länet så blir värdet TRUE (eller "<lans> kommuner" om vi valt att returnera_text)
  # tillat_lanskod    - här tillåter vi att vektorn reg_koder innehåller länskoden, om inte blir det FALSE om den är med
  # tillat_rikskod    - här tillåter vi även att vektorn innehåller rikskoden ("00"), om inte blir det FALSE om den är med
  # returnera_text    - istället för att returnera TRUE eller FALSE huruvida reg_koder innehåller alla kommuner i ett län
  #                     så kan vi returnera textsträngen "Dalarnas kommuner" (om det är Dalarna som är länet)
  # returtext         - om man kör TRUE på returnera_text och inte alla reg_koder är från alla kommuner i ett län
  #                     så returneras NA, alternativt returneras returtext om det skickas med. På så sätt kan man
  #                     testa om reg_koder är alla kommuner i ett län och få tillbaka den textsträng man hade från 
  #                     början om det inte är det.
  
  retur_varde <- TRUE                      # vi sätter värdet till TRUE från början, testar nedan och ändrar till FALSE om inte alla kriterier nedan uppfylls
  returtext_na <- if (is.na(returtext)) TRUE else FALSE       # om man skickat med returtext så returneras den om inte regionkoderna är alla kommuner i ett län, annars om man inte skickat med någon returtext  returneras FALSE
  
  if (length(unique(str_sub(reg_koder[reg_koder != "00"], 1, 2))) > 1) retur_varde <- FALSE else {                      # om det finns flera län eller kommuner från flera län med i reg_koder så blir det FALSE                      
    
    # unika_ej_kommunkoder <- unique(c("00", str_sub(reg_koder, 1, 2)))
    # if (length(unika_ej_kommunkoder > 0) & !any(reg_koder %in% unika_ej_kommunkoder)) retur_varde <- FALSE     # om det finns någon annan kod än kommunkod i reg_koder så blir det FALSE)
    if (any(nchar(reg_koder) < 4 & !reg_koder %in% unique(c("00", str_sub(reg_koder, 1, 2))))) retur_varde <- FALSE     # finns kod som inte är kommunkod och inte heller läns- eller rikskod så blir det FALSE
    kommuner_akt_lan <- hamtakommuner(unique(str_sub(reg_koder[reg_koder != "00"], 1, 2)), F, F)         # här hämtar vi alla kommuner i aktuellt län
    reg_koder_bara_komm <- reg_koder[!reg_koder %in% unique(c("00", str_sub(reg_koder, 1, 2)))]
    if (length(reg_koder_bara_komm) < 1) retur_varde <- FALSE               # om det inte finns några kommuner i skickade regionkoder
    if (!all(reg_koder[!reg_koder %in% unique(c("00", str_sub(reg_koder, 1, 2)))] == kommuner_akt_lan)) retur_varde <- FALSE   # alla regionkoder minus länskod och rikskod är lika med det aktuella länets samtliga kommunkoder
    if (any(reg_koder == str_sub(reg_koder, 1, 2)) & !tillat_lanskod) retur_varde <- FALSE
    if (any(reg_koder == "00") & !tillat_rikskod) retur_varde <- FALSE
  }
  
  if (returnera_text) {        # om användaren valt att man ska returnera text (och inte TRUE/FALSE)
    if (retur_varde) {         # om alla kommuner tillhör samma län så skapas texten som ska returneras nedan
      lanskod <- str_sub(reg_koder[reg_koder != "00"], 1, 2) %>% unique()
      retur_text <- hamtaregion_kod_namn(lanskod)$region %>% skapa_kortnamn_lan() %>% paste0(., "s kommuner")
    } else {
      retur_text <- if (returtext_na) FALSE else returtext
    }
    
    return(retur_text)         # här returneras text, nytt värde om alla kommuner kommer från ett län, annars NA
    
  } else {                     # om användaren INTE valt att returnera text returneras TRUE/FALSE
    return(retur_varde)  
  }
  
}

ar_alla_lan_i_sverige <- function(reg_koder, tillat_rikskod = TRUE, returnera_text = FALSE, returtext = NA) {
  
  # kontrollerar om reg_koder innehåller alla län i Sverige
  
  retur_varde <- TRUE                      # vi sätter värdet till TRUE från början, testar nedan och ändrar till FALSE om inte alla kriterier nedan uppfylls
  returtext_na <- if (all(is.na(returtext))) TRUE else FALSE                 # om man skickat med returtext så returneras den om inte regionkoderna är alla län i Sverige , annars om man inte skickat med någon returtext  returneras FALSE
  
  if (any(nchar(reg_koder) > 2)) retur_varde <- FALSE
  reg_koder_utan <- reg_koder[reg_koder != "00"]
  if (!(all(unique(reg_koder_utan) %in% hamtaAllaLan(F)) & all(hamtaAllaLan(F) %in% unique(reg_koder_utan)) & length(reg_koder_utan) == 21)) retur_varde <- FALSE
  if (any(reg_koder == "00") & !tillat_rikskod) retur_varde <- FALSE              # sätt FALSE om man inte tillåter rikskode 
  
  if (returnera_text) {        # om användaren valt att man ska returnera text (och inte TRUE/FALSE)
    if (retur_varde) {         # om alla kommuner tillhör samma län så skapas texten som ska returneras nedan
      retur_text <- "Sveriges län"
    } else {
      retur_text <- if (returtext_na) FALSE else returtext
    }
    
    return(retur_text)         # här returneras text, nytt värde om alla kommuner kommer från ett län, annars NA
    
  } else {                     # om användaren INTE valt att returnera text returneras TRUE/FALSE
    return(retur_varde)  
  } 
  
}

skapa_aldersgrupper <- function(alder, aldergrupp_vekt, konv_fran_txt = TRUE, returnera_faktorvariabel = TRUE) {
  
  # funktion för att enkelt skapa åldersgrupper från ålder som kan användas i en mutate-funktion:
  # mutate(aldersgrupp = skapa_aldersgrupper(alder_var, c(19, 35, 50, 65, 80)))
  #
  # aldergrupp_vekt är en vektor där varje siffra är början på nästa åldersgrupp. 
  # så c(19, 35, 50, 65, 80) ovan blir till åldersgrupperna 0-18 år, 19-34 år, 35-49 år, 50-64 år, 65-79 år samt 80+ år
  
  if (konv_fran_txt && is.character(alder)) alder <- readr::parse_number(alder)
  
  # Bestäm faktisk min/max i datat
  min_alder <- suppressWarnings(min(alder, na.rm = TRUE))
  max_alder <- suppressWarnings(max(alder, na.rm = TRUE))
  
  # Kontrollera och hantera öppna åldersgrupper
  if (!is.infinite(aldergrupp_vekt[[1]])) aldergrupp_vekt <- c(-Inf, aldergrupp_vekt)
  if (!is.infinite(tail(aldergrupp_vekt, n = 1))) aldergrupp_vekt <- c(aldergrupp_vekt, Inf)
  
  # Anpassa till faktisk data
  aldergrupp_vekt[1] <- min(aldergrupp_vekt[1], min_alder)
  aldergrupp_vekt[length(aldergrupp_vekt)] <- max(aldergrupp_vekt[length(aldergrupp_vekt)], max_alder + 1)
  
  
  # Skapa etiketter för grupperna
  labels <- vector("character", length = length(aldergrupp_vekt) - 1)
  for (i in seq_along(labels)) {
    lower <- aldergrupp_vekt[i]
    upper <- aldergrupp_vekt[i + 1] - 1
    
    if (lower < min_alder) lower <- min_alder
    if (upper > max_alder) upper <- max_alder
    
    if (i == length(labels) && max_alder > max(aldergrupp_vekt[is.finite(aldergrupp_vekt)])) {
      labels[i] <- str_c(lower, "+ år")
    } else if (lower == upper) {
      labels[i] <- str_c(lower, " år")
    } else {
      labels[i] <- str_c(lower, "-", upper, " år")
    }
  }
  
  # Dela in åldrarna i grupper
  retur_vekt <- cut(alder, breaks = aldergrupp_vekt, labels = labels, right = FALSE, include.lowest = TRUE)
  if (!returnera_faktorvariabel) retur_vekt <- as.character(retur_vekt) 
  return(retur_vekt)
}

# returnera rätt sökväg till vår utskriftsmapp där vi sparar diagram- och kartfiler som inte har någon särskild
# målmapp
utskriftsmapp <- function(){ return("G:/Samhällsanalys/API/Fran_R/Utskrift/")}

mapp_hamtadata_peter <- function(){ return("C:/gh/hamta_data/")}

mapp_temp_peter <- function(){ return("g:/skript/peter/temp/")}

mapp_leveranser <- function(){"g:/Samhällsanalys/Leveranser/"}

mapp_inlasdata <- function(){"g:/Samhällsanalys/Statistik/data_fran_mikrodb/"}

manader_bearbeta_scbtabeller <- function(skickad_df, kolumn_manad = "månad", kortmanad = FALSE) {
  # funktion för att skapa kolumnerna år, månad, månad_år samt år_månad av kolumnen månad som 
  # ligger i flera scb-tabeller och är strukturerad som år, bokstaven "M" och sedan månads-
  # nummer med två tecken (nolla framför på årets första 9 månader), alltså "2023M11" för 
  # november 2023. kortmanad = TRUE skickar med ytterligare en kolumn i formatet "jan 2026"
  
  retur_df <- skickad_df %>% 
    rename(tid = !!sym(kolumn_manad)) %>% 
    mutate(år = str_sub(tid, 1, 4) %>% as.integer(),
           månad_nr = parse_integer(str_sub(tid, 6,7)),
           månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%B"),
           år_månad = paste0(år, " - ", månad),
           månad_år = paste0(månad, " ", år),
           mån_år = paste0(str_to_lower(str_sub(månad, 1, 3)), " ", år)
    ) 
  
  manad_sort <- retur_df %>% group_by(månad_nr) %>% summarise(antal = n(), månad_sort = max(månad)) %>% select(månad_sort) %>% dplyr::pull()
  
  retur_df <- retur_df %>% 
    mutate(månad_år = factor(månad_år, levels = unique(månad_år[order(år, månad_nr)])),
           mån_år = factor(mån_år, levels = unique(mån_år[order(år, månad_nr)])),
           år_månad = factor(år_månad, levels = unique(år_månad[order(år, månad_nr)])),
           år = factor(år),
           månad = factor(månad, levels = manad_sort)) %>%
    select(-månad_nr) %>%                                                           # ta bort sort-kolumnen när vi använt den för att sortera tids-kolumnerna
    relocate(år, .after = tid) %>%                                              # vi sorterar om kolumnerna så att innehållsvariabeler alltid ligger sist
    relocate(månad, .after = år) %>% 
    relocate(år_månad, .after = månad) %>% 
    relocate(månad_år, .after = år_månad)
  
  if (!kortmanad) retur_df <- retur_df %>% select(-mån_år)
  
  return(retur_df)
}

stop_tyst <- function() {
  # stanna skriptet utan att skriva ut felmeddelande
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

suppress_specific_warning <- function(expr, warn_text = "NAs introduced by coercion") {
  # ta bort specifika felmeddelanden och inte alla
  withCallingHandlers(expr,
                      warning = function(w) {
                        if (grepl(warn_text, conditionMessage(w))) {
                          invokeRestart("muffleWarning")
                        }
                      })
}

korrigera_kolnamn_supercross <- function(skickad_fil, teckenkodstabell = "latin1"){
  kon_korr <- readLines(skickad_fil, encoding = teckenkodstabell)                          # läs in fil
  if (str_detect(kon_korr[1], "/")) {                                                # kör bara om det finns ett "/" på rad 1, annars låt vara
    kon_korr[1] <- kon_korr[1] %>% 
      str_replace("-/", "_")                                                         # ta bort slash som ställer till det vid inläsning av kolumnnamn
    writeLines(kon_korr, paste0(skickad_fil))                                        # skriv över med rätt tecken
  }
}

filhamtning_med_url_och_sokord <- function(
    url_webbsida = "https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik",
    sokord = c("web-platser", ".xlsx"),
    bas_url = "https://arbetsformedlingen.se"
) {
  # Man skickar med en länk och sökord så returnerar funktionen en sökväg till filen som laddats
  # ned till en temporär fil. Filen tas bort när man stänger R igen men kan läsas in med andra
  # funktioner, som readxl::read_xlsx() eller funktionerna ovan för att extrahera data direkt ur pivottabeller
  
  # Funktionen letar bland url-länkar på webbsidan (url_webbsida) och väljer den url (om det finns flera) som
  # innehåller alla sökord i vektorn. Finns flera så lämnas ett felmeddelande istället och man måste skicka med fler sökord
  
  # Använder: webbsida_af_extrahera_url_med_sokord()
  
  url_nedladdning <- webbsida_af_extrahera_url_med_sokord(url_webbsida, sokord, bas_url)
  
  
  if (length(url_nedladdning) == 0) stop("Inga url:er matchar angivna sökord.")
  
  
  td = tempdir()              # skapa temporär mapp
  
  ledigajobb_filer <- map(url_nedladdning, ~ {
    fil <- tempfile(tmpdir=td, fileext = ".xlsx")
    
    httr::GET(.x, httr::write_disk(fil, overwrite = TRUE))
  }) 
  
  if (all(map_int(ledigajobb_filer, "status_code") == "200")) return(map_chr(ledigajobb_filer, "content")) else stop("Något gick fel vid nedladdning av fil. Kontrollera att url:en är korrekt och att sökorden matchar rätt fil.")
}


webbsida_af_extrahera_url_med_sokord <- function(skickad_url, sokord = c("varsel", "lan", "!bransch", ".xlsx"),
                                                 bas_url = "https://arbetsformedlingen.se") {
  
  # hämta webbsidan med tidigare statistik på Arbetsförmedlingen och spara som en vektor
  #webbsida <- suppressWarnings(readLines(skickad_url))
  html_af <- rvest::read_html(skickad_url)
  webbsida <- html_af %>% 
    html_nodes("a") %>%
    html_attr("href")
  
  # Få index för de element på webbsidan där alla sökord är med,
  # tabort-sökord, dvs. sökord som börjar med "!" tas inte med i funktionen
  sokord_filtered <- sokord %>% 
    purrr::discard(~ str_starts(.x, "!"))
  
  sokord_index <- which(map_lgl(webbsida, function(text) {
    all(map_lgl(sokord_filtered, function(s) {
      str_detect(tolower(text), tolower(s))
    }))
  }))
  
  af_urler <- webbsida[sokord_index]                          # skapa sträng med det element där båda är med
  
  # # i den strängen, ta ut startposition för alla "/download/" som hittar i strängen (det är sökvägar)
  # start_sokvagar <- str_locate_all(fil_strang, "/download/")[[1]][,1]  
  # 
  # # funktion för att ta ut fullständig url från de startpositioner vi hittade i raden ovan
  # extrahera_sokvag <- function(strang, startpos) {
  #   
  #   nystrang <- str_sub(strang, startpos, nchar(strang))
  #   slutpos <- str_locate(nystrang, '\"')[[1]]-1
  #   
  #   retur_strang <- str_sub(nystrang, 1, slutpos)
  #   retur_strang <- paste0("https://arbetsformedlingen.se", retur_strang)
  #   return(retur_strang)
  # }       
  # 
  # vi skapar en vektor med fullständiga sökvägar för samtliga excelfiler som finns på webbsidan
  # af_urler <- start_sokvagar %>% map_chr(~ extrahera_sokvag(fil_strang, .x))
  
  # Filtrera de element i 'texts' där alla sökord finns
  sokord_url <- af_urler %>%
    #keep(~ all(map_lgl(sokord, function(s) str_detect(.x, s))))
    keep(function(text) {
      all(map_lgl(sokord, function(s) {
        if (str_starts(s, "!")) {
          # Om sökordet börjar med "!", detektera att det INTE finns i texten
          !str_detect(text, str_remove(s, "^!"))
        } else {
          # Annars, detektera att sökordet finns i texten
          str_detect(tolower(text), tolower(s))
        }
      }))
    })
  
  sokord_url <- paste0(bas_url, af_urler)
  
  return(sokord_url)
} 

konvertera_dataset_filformat <- function(sokvag_fil,
                                         nytt_format_filandelse,
                                         teckenkodtabell = "Latin-1") {
  
  if (!require("rio")) install.packages("rio")
  if (!require("writexl")) install.packages("writexl")
  
  dataset_df <- rio::import(sokvag_fil, encoding = teckenkodtabell)
  
  filandelse <- str_extract(sokvag_fil, "\\.[^.]+$")                               # Extrahera filändelsen inklusive punkt
  filnamn_utan_andelse <- str_remove(sokvag_fil, "\\.[^.]+$")                      # Extrahera filnamnet utan filändelsen
  
  if (str_sub(nytt_format_filandelse, 1, 1) != ".") nytt_format_filandelse <- paste0(".", nytt_format_filandelse)
  nytt_filnamn <- paste0(filnamn_utan_andelse, nytt_format_filandelse)          # Skapa nytt filnamn med ny filändelse
  
  if (nytt_format_filandelse == ".xlsx") {
    writexl::write_xlsx(dataset_df, nytt_filnamn)                       # Exportera datasetet till nytt filformat
  } else {
    rio::export(dataset_df, nytt_filnamn)                                         # Exportera datasetet till nytt filformat
  }
} # slut funktion

funktion_upprepa_forsok_tills_retur_TRUE <- function(funktion, max_forsok = 15, vanta_minuter = 2) {
  
  for (forsok in 1:max_forsok) {
    # Försök att köra funktionen
    resultat <- funktion()
    
    # Kontrollera om funktionen lyckades (returnerar TRUE)
    if (resultat == TRUE) {
      message("Funktionen lyckades på försök ", forsok, ".")
      return(TRUE)  # Avsluta och returnera TRUE om funktionen lyckas
    } else {
      message("Försök ", forsok, " misslyckades. Försöker igen om ", vanta_minuter, " minuter.")
      
      # Om max antal försök har nåtts, ge upp
      if (forsok == max_forsok) {
        message("Max antal försök nått. Funktionen stoppas.")
        return(FALSE)
      }
      
      # Vänta det angivna antalet minuter innan nästa försök
      Sys.sleep(vanta_minuter * 60)
    }
  }
}

funktion_upprepa_forsok_om_fel <- function(funktion, 
                                           max_forsok = 10, 
                                           vanta_sekunder = 1, 
                                           meddelanden = FALSE,
                                           hoppa_over = FALSE,
                                           loggfil = NULL,         # sökväg + filnamn (.txt) om man vill spara felmeddelanden i en fil
                                           returnera_vid_fel = NULL            # valfritt vad som returneras vid fel, kan vara tex NULL, NA eller invisible()
) {
  
  if (!hoppa_over) {
    funktionsnamn <- deparse(substitute(funktion)) %>%   # Hämta namnet på funktionen som skickades in
      str_remove("function\\(\\)") %>% 
      str_extract("^[^(]+") %>%
      .[1] %>%
      paste0(., "()") %>% 
      str_trim()
    
    unika_fel <- character(0)            # vektor för att spara alla unika fel
    
    for (forsok in 1:max_forsok) {
      # Försök att köra funktionen
      resultat <- try(funktion(), silent = TRUE)
      
      # Kontrollera om funktionen lyckades (ingen fel uppstod)
      if (!inherits(resultat, "try-error")) {
        return(resultat)
        
      } else {    # om det blir fel
        
        # Spara unika felmeddelanden
        felmeddelande <- as.character(resultat)
        if (!felmeddelande %in% unika_fel) {
          unika_fel <- c(unika_fel, felmeddelande)
        }
        
        if (meddelanden) cat("Försök ", forsok, " med funktionen ", funktionsnamn, " misslyckades med fel: ", resultat)
        
        # Om max antal försök har nåtts, ge upp
        if (forsok == max_forsok) {
          cat("Max antal försök nått. Funktionen", funktionsnamn, "stoppas.\n\n")
          
          # skriv loggfil om sökväg + filnamn skickats med
          if (!is.null(loggfil)) {
            # Skapa loggtext med tidsstämpel
            loggtext <- paste0(Sys.time(), " | Funktion: ", funktionsnamn, "\nFelmeddelanden:\n", 
                               paste(unique(unika_fel), collapse = "\n"), "\n\n")
            
            # Skriv till loggfil
            write(loggtext, file = loggfil, append = TRUE)
            cat(paste0(unika_fel, "\n"))                   
            #message(paste0(funktionsnamn, "\n"))          # för test
          }
          
          return(returnera_vid_fel)
        }
        
        # Vänta det angivna antalet sekunder innan nästa försök
        if (meddelanden) message("Försöker igen om ", vanta_sekunder, " sekunder.")
        Sys.sleep(vanta_sekunder)
      }
    }
  } else {
    return(funktion())
  }
}

skriptrader_upprepa_om_fel <- function(expr,
                                       max_forsok = 5,
                                       vila_sek = 1,
                                       exportera_till_globalenv = TRUE,
                                       returnera_vid_fel = NULL,
                                       upprepa_vid_felmeddelande_som_innehaller = c("recv failure", "connection was reset", "curl_fetch_memory", "timeout")
) {
  
  # kör funktionen "runt" en eller ett antal skriptrader för att testa igen om felmeddelandet innehåller de ord som listas i parametern
  # upprepa_vid_felmeddelande_som_innehaller. Max antal försök anges med max_forsok och vila_sek anger sekunders vila mellan försöken.
  #
  # för att köra funktionen på en skriptrad: 
  # dataset_df <- skriptrader_upprepa_om_fel(hamta_data())
  #
  # för att köra funktionen på flera skriptrader så läggs dessa mellan måsvingar (enligt nedan):
  # dataset_df <- skriptrader_upprepa_om_fel({
  #                   hamta_data()
  #                   bearbeta_data()
  #                   analysera_data()
  #                 })
  
  expr_sub <- substitute(expr)  # fånga uttrycket INNAN det evalueras
  is_fun_input <- is.symbol(expr_sub) && is.function(get(as.character(expr_sub), envir = parent.frame(), inherits = TRUE))
  
  #is_fun_input <- is.function(eval(expr_sub, parent.frame()))  # kolla typen säkert
  # is_fun_input <- is.function(expr)
  # if (!is_fun_input) expr <- substitute(expr)
  
  for (i in seq_len(max_forsok)) {
    env <- new.env(parent = parent.frame())
    
    out <- tryCatch(
      {
        if (is_fun_input) {
          eval(expr_sub, envir = parent.frame())()                                 # returnerar funktionsvärdet
        } else {
          res <- eval(expr_sub, envir = env)           # kör kod-rader
          obj_namn <- ls(env, all.names = TRUE)
          if (length(obj_namn)) {
            obj_lista <- mget(obj_namn, envir = env)
            if (exportera_till_globalenv) list2env(obj_lista, envir = globalenv())
            obj_lista                              # returnera skapade objekt
          } else {
            res                                    # returnera uttryckets värde
          }
        }
      },
      error = function(e) {
        msg <- tolower(conditionMessage(e))
        message("⚠️ Fel vid försök ", i, ": ", conditionMessage(e))
        
        ska_forsoka_igen <- TRUE
        if (!is.null(upprepa_vid_felmeddelande_som_innehaller)) {
          pats <- tolower(upprepa_vid_felmeddelande_som_innehaller)
          ska_forsoka_igen <- any(vapply(pats, function(p) grepl(p, msg, fixed = TRUE), logical(1)))
        }
        
        if (i < max_forsok && ska_forsoka_igen) {
          Sys.sleep(vila_sek)
          return(NULL)                              # försök igen
        } else {
          if (is.null(returnera_vid_fel)) stop(e) else return(returnera_vid_fel)
        }
      }
    )
    
    if (!is.null(out)) return(out)
  }
}

url_finns_webbsida <- function(skickad_url) {
  response <- HEAD(skickad_url)
  status <- status_code(response)
  return(status == 200)
}

period_jmfr_filter <- function(period_kolumn, vald_period, period_vekt, inkludera_vald_period = TRUE) {
  # en funktion för att extrahera perioder ur dataset med positioner från ett (eller flera) medskickade värden
  # t.ex. om man vill ha samma månad 1, 2, och 3 år tillbaka i ett dataset med månader så skickar man med
  # c(-12, -24, -36) och kanske senaste period tex. 2025M03, kommer då att returnera 2024M03, 2023M03 och 2022M03
  #
  # period_kolumn är hela kolumnen, tex. df$månad
  # vald_period är en eller flera perioder som man gör jämförelser från
  # period_vekt är en vektor med antal enheter bakåt eller framåt i tiden från vald_period
  # inkludera_vald_period är en logisk variabel som anger om vald_period ska inkluderas i retur_vekt
  
  retur_vekt <- map(vald_period, function(period) {
    valda_pos <- period_vekt[period_vekt > 0]
    valda_neg <- period_vekt[period_vekt < 0]
    
    filter_period_pos <- sort(unique(period_kolumn %>% .[. < period]), decreasing = TRUE)[abs(valda_neg)]
    filter_period_neg <- sort(unique(period_kolumn %>% .[. > period]), decreasing = FALSE)[abs(valda_pos)]
    filter_period_tot <- c(filter_period_neg, filter_period_pos)
    return(filter_period_tot)
  }) %>% unlist()
  
  if (inkludera_vald_period) retur_vekt <- c(vald_period, retur_vekt)
  
  return(retur_vekt)
}

# funktionen används för att dela upp värden i en kolumn i ett antal intervaller (default är 5 st)
# den används främst för att skapa intervaller till skriptet som bearbetar dataset för att göra
# bubbeldiagram för branschindelning per kommun eller län

skapa_intervaller <- function(skickad_kolumn, antal_intervaller = 5){
  
  # Hjälpfunktion för att runda till bra tal
  runda_till_bra_tal <- function(tal) {
    10^round(log10(tal))  # Rundar av till närmaste 10, 100, 1000, etc.
  }
  
  # Hitta min och max i kolumnen
  min_varde <- min(skickad_kolumn, na.rm = TRUE)
  max_varde <- max(skickad_kolumn, na.rm = TRUE)
  
  # Beräkna råa intervall
  intervall_steg <- (max_varde - min_varde) / (antal_intervaller - 1)
  intervall_steg <- runda_till_bra_tal(intervall_steg)  # Runda till bra tal
  
  # Justera min-värdet till ett jämnt steg
  min_rundat <- floor(min_varde / intervall_steg) * intervall_steg
  
  if (min_rundat <= 0) {
    min_rundat <- intervall_steg / 2  # Sätt minsta värdet till halva intervallet
  }
  
  # Justera max-värdet till ett jämnt steg
  #max_rundat <- ceiling(max_varde / intervall_steg) * intervall_steg
  
  # Justera min- och maxvärden för att hålla dem nära originalspannet
  min_rundat <- max(floor(min_varde / intervall_steg) * intervall_steg, min_varde)
  max_rundat <- min(ceiling(max_varde / intervall_steg) * intervall_steg, max_varde)
  
  if (max_rundat > max_varde * 1.1) {  # Tillåt max 10 % över faktiska maxvärdet
    max_rundat <- floor(max_varde / intervall_steg) * intervall_steg + intervall_steg / 2
  }
  
  # Kontrollera att intervallet är tillräckligt stort
  if ((max_rundat - min_rundat) < (antal_intervaller - 1) * intervall_steg) {
    max_rundat <- max_rundat + intervall_steg
  }
  
  # Skapa intervallvektorn baserat på justerat spann
  intervaller_obearbetad <- seq(min_rundat, max_rundat, length.out = antal_intervaller)
  
  retur_intervaller <- avrundning_dynamisk(intervaller_obearbetad)
  
  return(retur_intervaller)
  
}

skalcirklar_skapa <- function(min_varde, max_varde, antal_skalcirklar) {
  cirklar <- seq(from = sqrt(min_varde), to = sqrt(max_varde), length.out = antal_skalcirklar) ^ 2
  cirklar <- round(cirklar, -floor(log10(max_varde)) + 1)  # avrundning till "lagom nivå"
  unique(cirklar)
}

varden_jamnt_spridda_valj_ut <- function(skickad_vektor, antal_varden = 4) {
  
  # om man vill välja ut ett antal värden i en vektor, min, max + ett antal till jämnt fördelat
  num_vec <- as.numeric(skickad_vektor)
  n <- length(num_vec)
  
  if (antal_varden >= n) {
    return(as.character(skickad_vektor))  # returnera allt i originalordning
  }
  
  # välj index från den sorterade vektorn
  indices <- round(seq(1, n, length.out = antal_varden))
  valda <- sort(num_vec)[indices]
  
  # Bygg returvektor i originalordning
  resultat <- ifelse(num_vec %in% valda, as.character(num_vec), "")
  
  return(resultat)
}


kontrastfarg_hitta <- function(farg_vektor_hex, cutoff = 0.6, justering = 0.3) {
  # Funktion som väljer mörk eller ljus färg som kontrasterar mot de färger man skickad in som vektor
  
  # Hex → HCL (som numerisk matris)
  col_hcl <- decode_colour(farg_vektor_hex, to = "hcl")
  
  # Luminans (0–100)
  lum <- col_hcl[, "l"] / 100
  
  # Justera ljushet
  col_hcl[, "l"] <- ifelse(lum < cutoff,
                           pmin(col_hcl[, "l"] + justering*100, 100),
                           pmax(col_hcl[, "l"] - justering*100, 0))
  
  # HCL → hex
  encode_colour(col_hcl, from = "hcl")
}


avrundning_dynamisk <- function(x, gräns_stora = 10, gräns_medel = 1, dec_stora = 0, dec_medel = 1, dec_små = 2) {
  avrunda <- function(v) {
    if (abs(v) >= gräns_stora) {
      # Runda till närmaste multipel av en "fin" grund
      multipel <- 10^floor(log10(abs(v)))
      round(v / multipel) * multipel
    } else if (abs(v) >= gräns_medel) {
      round(v, dec_medel)  # En decimal för medelstora tal
    } else {
      round(v, dec_små)    # Två decimaler för små tal
    }
  }
  # Applicera avrundningen på varje element i vektorn
  retur_vekt <- purrr::map_dbl(x, avrunda)
  return(retur_vekt)
}


vektor_till_text <- function(skickad_vektor, 
                             till_urklipp = TRUE                          # om TRUE skrivs source-satserna till urklipp om skriv_source_konsol är TRUE
){
  
  retur_text <- paste0('"', skickad_vektor, '"', collapse = ", ")
  cat(retur_text)
  if (till_urklipp) {
    writeLines(text = retur_text, con = "clipboard", sep = "")
  }
  invisible(retur_text)
}

hamta_logga_path <- function(){
  # sökväg till logga för att kunna lägga in den i diagrammen
  tf <- "https://raw.githubusercontent.com/Region-Dalarna/depot/main/rd_logo_liggande_fri_svart.png"
  return(tf)
}   

anv_anvandarkonto_hamta <- function(){
  Sys.info()["user"] %>% as.character()
}

anv_fornamn_efternamn_hamta <- function(){
  anvandares_namn <- system("powershell -Command \"(Get-WmiObject -Class Win32_UserAccount -Filter \\\"Name='$env:USERNAME'\\\").FullName\"", intern = TRUE)
  
  # Skapa en lista med felaktiga och korrekta tecken
  replacement_list <- tibble(
    felaktig = c("\x94", "\x93", "\x96", "\x95", "\xA4", "\xA5"),  # Felaktiga tecken
    korrekt = c("ö", "Ö", "å", "Å", "ä", "Ä")  # Korrekta tecken
  )
  
  
  anvandares_namn_korrigerad <- reduce(seq_along(replacement_list$felaktig), function(text, i) {
    str_replace_all(text, replacement_list$felaktig[i], replacement_list$korrekt[i])
  }, .init = anvandares_namn)
  
  anvandare_namn <- anvandares_namn_korrigerad %>% 
    str_remove("[^a-zA-ZåäöÅÄÖ ].*") %>% 
    str_remove(" b ") %>% 
    str_squish() %>%
    str_replace("^(\\S+)\\s+(.*)$", "\\2 \\1")
  return(anvandare_namn)
}

anv_epostadress_hamta <- function(){
  if (!require("pacman")) install.packages("pacman")
  p_load(gh)
  
  tryCatch({
    epostlista <- gh::gh("GET /user/emails", .token = key_get("github_token", key_list(service = "github_token")$username))
    retur_epost <- map_chr(epostlista, "email") %>% 
      .[str_detect(., "regiondalarna.se")] %>% 
      .[1]
    return(retur_epost)
  }, error = function(e) {
    # Returnera NULL vid fel
    message("Fel inträffade: ", e$message)
    return(NULL)
  })
  
}

anv_hamta_namn_epost_fran_lista <- function(skickat_namn = NULL){
  
  if (is.null(skickat_namn)) {
    # om skickat_namn är NULL så hämtas för- och efternamn samt epost från 
    # den användare som är inloggad
    retur_lista <- list(namn = anv_fornamn_efternamn_hamta(),
                        epost = anv_epostadress_hamta())
    
  } else {
    
    # om det finns uppgifter  
    retur_lista <- if (tolower(skickat_namn) == "peter") { 
      list(namn = "Peter Möller",
           epost = "peter.moller@regiondalarna.se")
    } else if (tolower(skickat_namn) == "mats") {
      list(namn = "Mats Andersson",
           epost = "mats.b.andersson@regiondalarna.se")
    } else if (tolower(skickat_namn) == "jon") {
      list(namn = "Jon Frank",
           epost = "jon.frank@regiondalarna.se")
    } else NULL
  }  # slut if-sats för att testa om skickat_namn är NULL
  
  return(retur_lista)
  
} # slut funktion


slash_lagg_till <- function(x) {
  if (!str_ends(x, "/")) x <- paste0(x, "/")
  return(x)
}

nummer_till_text <- function(x) {
  ord <- c(
    "ett", "två", "tre", "fyra", "fem",
    "sex", "sju", "åtta", "nio", "tio",
    "elva", "tolv", "tretton", "fjorton", "femton",
    "sexton", "sjutton", "arton", "nitton", "tjugo"
  )
  retur_x <- dplyr::case_when(
    x >= 1 & x <= 20 ~ ord[x],
    TRUE ~ NA_character_
  )
  
  if (any(is.na(retur_x))) warning("Funktionen kan bara hantera talen 1-20, övriga tal tas bort.")
  retur_x <- retur_x %>% .[!is.na(.)]
  
  if (length(retur_x) == 0) stop("Funktionen kan bara hantera tal som är 1-20.") else return(retur_x)
}

sokvag_for_skript_hitta <- function() {
  # funktion för att hämta sökväg för det skript som körs
  
  if (!is.null(sys.frame(1)$ofile)) {
    retur_varde <- dirname(normalizePath(sys.frame(1)$ofile))  # körs via source()
  } else if (interactive()) {
    retur_varde <- dirname(rstudioapi::getActiveDocumentContext()$path)  # interaktivt
  } else {
    retur_varde <- getwd()  # fallback: aktuell arbetskatalog
  }
  # säkerställ att sökvägen slutar med snedstreck
  retur_varde <- if (str_sub(retur_varde, -1) == "/") retur_varde else paste0(retur_varde, "/")
  return(retur_varde)
}

urklipp <- function(x, sep = "\n") {
  # funktion för att lägga saker i urklippshanteraren, så att man bara kan klistra det därefter med ctrl + v
  
  # Om clipr finns, använd det (bäst cross-platform, hanterar även data.frames snyggt)
  if (requireNamespace("clipr", quietly = TRUE)) {
    clipr::write_clip(x)
    return(invisible(x))
  }
  
  # Fallback: gör text
  txt <- if (is.data.frame(x) || inherits(x, "tbl")) {
    tc <- textConnection("..tmp", "w", local = TRUE)
    utils::write.table(x, tc, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    close(tc)
    paste(..tmp, collapse = "\n")
  } else {
    ch <- as.character(x)
    if (length(ch) == 1) ch else paste(ch, collapse = sep)
  }
  
  os <- Sys.info()[["sysname"]]
  if (identical(os, "Windows")) {
    utils::writeClipboard(enc2native(txt))
  } else if (identical(os, "Darwin")) {
    con <- pipe("pbcopy", "w"); writeChar(enc2utf8(txt), con, eos = , useBytes = TRUE); close(con)
  } else {
    if (nzchar(Sys.which("xclip"))) {
      con <- pipe("xclip -selection clipboard", "w"); writeChar(enc2utf8(txt), con, eos = "", useBytes = TRUE); close(con)
    } else if (nzchar(Sys.which("xsel"))) {
      con <- pipe("xsel --clipboard --input", "w"); writeChar(enc2utf8(txt), con, eos = "", useBytes = TRUE); close(con)
    } else {
      stop("Ingen urklippsmetod hittades. Installera paketet 'clipr' eller xclip/xsel.")
    }
  }
  invisible(x)
}


source_utan_cache <- function(url, encoding = "UTF-8", echo = FALSE, pat = NULL) {
  
  #' Source:a en R-fil från GitHub utan risk för gammal CDN-cache
  #'
  #' raw.githubusercontent.com ligger bakom Fastly, som kan servera en gammal
  #' version i upp till några minuter efter en push. Varken `Cache-Control`-headern
  #' eller en unik query-sträng bustar den cachen (testat — ger ändå `x-cache: HIT`).
  #' Det enda pålitliga sättet att få färskt innehåll direkt är att gå via GitHubs
  #' Contents-API med `Accept: raw`, som speglar senaste commit utan Fastly-cache.
  #'
  #' En PAT höjer API:ts rate-limit från 60 till 5000 anrop/timme (och krävs för
  #' privata repon). Token läses i ordningen: argument -> GITHUB_PAT -> keyring.
  #'
  #' Contents-API:t med `Accept: raw` hanterar filer upp till 100 MB. Är filen
  #' större faller funktionen automatiskt tillbaka på vanlig source() och skriver
  #' ut ett meddelande (då kan dock CDN-cache ge en gammal version). I praktiken
  #' slår gränsen aldrig i för vanliga R-funktionsfiler.
  #'
  #' @param url   Vanlig raw-URL, t.ex.
  #'   "https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R"
  #' @param encoding Teckenkodning som skickas till source() (default "UTF-8").
  #' @param echo  Skickas till source() (default FALSE).
  #' @param pat   Valfri GitHub-PAT. Lämna NULL för att läsa från miljö/keyring.
  
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Paketet 'httr' krävs.", call. = FALSE)
  }
  
  # Plocka ut owner / repo / branch / path ur raw-URL:en.
  m <- regmatches(
    url,
    regexec("raw\\.githubusercontent\\.com/([^/]+)/([^/]+)/([^/]+)/(.+)$", url)
  )[[1]]
  
  # Inte en raw.githubusercontent-URL? Fall tillbaka på vanlig source().
  if (length(m) != 5) {
    return(invisible(source(url, encoding = encoding, echo = echo)))
  }
  
  owner <- m[2]; repo <- m[3]; branch <- m[4]; path <- m[5]
  
  # PAT: argument > GITHUB_PAT > keyring (samma mönster som övriga funktioner).
  if (is.null(pat) || !nzchar(pat)) pat <- Sys.getenv("GITHUB_PAT", "")
  if (!nzchar(pat) && requireNamespace("keyring", quietly = TRUE)) {
    kp <- tryCatch(keyring::key_list(service = "github_token"), error = function(e) NULL)
    if (!is.null(kp) && nrow(kp) > 0) {
      pat <- tryCatch(keyring::key_get("github_token", kp$username[1]),
                      error = function(e) "")
    }
  }
  
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
    owner, repo, utils::URLencode(path, reserved = FALSE), branch
  )
  
  hdrs <- c(
    Accept = "application/vnd.github.raw",
    "X-GitHub-Api-Version" = "2022-11-28"
  )
  if (nzchar(pat)) hdrs <- c(hdrs, Authorization = paste("token", pat))
  
  res <- httr::GET(api_url, httr::add_headers(.headers = hdrs))
  
  # Hantera 403: rate-limit (fel) eller för stor fil (fall tillbaka på source()).
  if (httr::status_code(res) == 403) {
    body <- httr::content(res, "text", encoding = "UTF-8")
    if (grepl("rate limit", body, ignore.case = TRUE)) {
      stop("GitHub API rate-limit nådd. Sätt GITHUB_PAT (eller spara token i ",
           "keyring service = 'github_token') för 5000 anrop/timme istället ",
           "för 60.", call. = FALSE)
    }
    if (grepl("too large|larger than", body, ignore.case = TRUE)) {
      message("source_utan_cache(): '", basename(path), "' överskrider ",
              "Contents-API:ts gräns (100 MB). Faller tillbaka på vanlig ",
              "source(). OBS: vanlig source() kan leverera en CDN-cachad ",
              "(eventuellt gammal) version strax efter en push.")
      return(invisible(source(url, encoding = encoding, echo = echo)))
    }
  }
  httr::stop_for_status(res)
  
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(httr::content(res, "raw"), tmp)
  
  invisible(source(tmp, encoding = encoding, echo = echo))
}

source_funktioner <- function(skriptfil, funktioner) {
  # funktion för att source:a in en eller flera funktioner från en fil
  tmp_env <- new.env(parent = .GlobalEnv)
  source(skriptfil, local = tmp_env)
  
  hittade_funktioner <- funktioner[funktioner %in% ls(tmp_env)]
  ej_hittade_funktioner <- funktioner[!funktioner %in% ls(tmp_env)] 
  
  
  walk(hittade_funktioner, ~{
    assign(.x, tmp_env[[.x]], envir = .GlobalEnv)
  })
  
  verb <- if (length(ej_hittade_funktioner) > 1) "Funktionerna " else "Funktionen "
  if (length(ej_hittade_funktioner) > 0) cat(paste0(verb, ej_hittade_funktioner %>% list_komma_och(), " finns inte i skriptfilen '", skriptfil, "'."))
  
  rm(tmp_env)
  
}


excelfil_spara_formaterad <- function(indata,
                                      output_mapp = utskriftsmapp(), 
                                      excelfil_namn = NA,
                                      auto_kolumnbredd = TRUE,          # anpassar bredden på kolumnerna till hur mycket text det är i dem
                                      fetstil_rader = 1,                # vilka rader som ska vara i fetstil, NA = inga rader får fetstil
                                      fetstil_kolumner = NA,            # vilka kolumner som ska vara i fetstil, NA = inga kolumner får fetstil
                                      filnamnstillagg = "xlsx",         # sparas som detta format om man inte anger annat
                                      skriv_over_fil = TRUE             # TRUE = skriver över tidigare version om det finns någon (den kan dock inte vara öppen i Excel, då får man felmeddelande)
) {
  stopifnot(dir.exists(output_mapp))
  
  library(openxlsx, quietly = TRUE)
  
  if (is.na(excelfil_namn)) excelfil_namn <- paste0(deparse(substitute(indata)), ".", filnamnstillagg)             #
  # om det är en dataframe så konverteras den till en lista
  if (all(class(indata) != "list")) {
    indata <- list(indata)
    names(indata) <- c("dataset")
  } else {   # om det är en lista så sätts namn på listan om det inte finns för alla element
    if (any(is.null(names(indata)))) names(indata) <- paste0("dataset_", 1:length(indata))
  }
  
  # Skapa en ny workbook
  wb <- createWorkbook()
  
  # Iterera över listan och lägg till data i separata blad
  walk2(indata, names(indata), ~ {
    addWorksheet(wb, .y)                     # lägg till flik
    writeData(wb, sheet = .y, x = .x)        # lägg till data på fliken
    
    # egen beräkning av autosize
    if (auto_kolumnbredd) {
      kolumnbredd <- pmax(
        map_dbl(.x, ~ max(nchar(as.character(.x)), na.rm = TRUE)),
        nchar(names(.x))) + 2
      
      setColWidths(wb, sheet = .y, cols = 1:ncol(.x), widths = kolumnbredd)        # sätt kolumnbredd till autosize, dvs. så bred som texten i kolumnen är
    }
    
    
    if (!is.na(fetstil_rader)) walk(fetstil_rader, function(rad) addStyle(wb, sheet = .y, rows = rad, cols = 1:ncol(.x),                    # gör fetstil på de rader som användare angett, eller NA för inga rader med fetstil
                                                                          style = createStyle(textDecoration = "bold"), gridExpand = TRUE))
    if (!is.na(fetstil_kolumner)) walk(fetstil_kolumner, function(kolumn) addStyle(wb, sheet = .y, rows = 1:nrow(.x), cols = kolumn,              # gör fetstil på de kolumner som användare angett, eller NA för inga kolumner med fetstil
                                                                                   style = createStyle(textDecoration = "bold"), gridExpand = TRUE))
  })
  
  # Spara workbooken
  saveWorkbook(wb, paste0(output_mapp, excelfil_namn), overwrite = skriv_over_fil)
}


spara_som_csv_i_zip <- function(df_list,
                                output_mapp = NULL,
                                zipfilnamn = NA,
                                pre_namn_csv_fil_utan_namn = "df_",
                                meddelande = TRUE
) {
  
  # Om det är en dataframe och inte en lista så gör vi om den till en lista
  if (is.data.frame(df_list)) {
    namn_df  <- deparse(substitute(df_list))
    df_list  <- list(df_list)
    names(df_list) <- namn_df
  }
  
  if (is.null(output_mapp)) output_mapp <- utskriftsmapp()
  
  # Spara dataseten i varsin csv-fil
  csvfil_lista <- imap(df_list, ~ {
    filnamn_pre <- .y
    
    # Om det inte finns något namn döps den till "df_[siffra]"
    if (str_detect(filnamn_pre, "^\\.[0-9]+")) filnamn_pre <- paste0(pre_namn_csv_fil_utan_namn, .y)
    
    csv_fil <- paste0(output_mapp, paste0(filnamn_pre, ".csv"))
    #if (str_count(csv_fil, "df_")) csv_fil <- csv_fil %>% str_remove("df_")
    write_csv(.x, csv_fil)
    return(csv_fil)
  }) %>% unlist()
  
  # Skapa ett zipfilnamn om det inte finns något redan
  if (is.na(zipfilnamn)) zipfilnamn <- csvfil_lista %>% basename() %>% str_remove("[0-9]") %>% unique() %>% .[1] %>% str_replace(".csv", ".zip")
  if (str_sub(zipfilnamn, nchar(zipfilnamn)-3) != ".zip") zipfilnamn <- paste0(zipfilnamn, ".zip")
  
  zip_full <- paste0(output_mapp, zipfilnamn)
  if (file.exists(zip_full)) invisible(file.remove(zip_full))
  zip(zip_full, csvfil_lista, flags = "-q", extras = '-j')
  invisible(file.remove(csvfil_lista))
  if (meddelande) print(paste0("Filen ", zipfilnamn, " har sparats i mappen ", output_mapp))
}

las_b64 <- function(sokvag_filnamn) {
  if (!file.exists(sokvag_filnamn)) {
    stop("Secret-filen finns inte: ", sokvag_filnamn, call. = FALSE)
  }
  
  x <- readLines(sokvag_filnamn, warn = FALSE, encoding = "UTF-8")
  x <- trimws(x)
  x <- x[nzchar(x)]
  
  if (!length(x)) {
    stop("Secret-filen är tom: ", sokvag_filnamn, call. = FALSE)
  }
  
  raw <- jsonlite::base64_dec(x[[1]])
  rawToChar(raw)
}


# ================================================= Ladda ner data utan API ==============================================

hamta_fk_json_dataset_med_url <- function(url_fk) {
  
  # en funktion för att hämta dataset från Försäkringskassan som är i json-format via url. Url:er kan hittas hos www.dataportal.se
  # där man kan filtrera på Försäkringskassan som organisation. Det går också att ladda ned Excelfiler från dem och då används
  # med fördel funktionen hamta_excel_dataset_med_url()
  
  
  kolumnordning <- c("Period", "period", "tid", "Ar", "ar", "År", "år", "Manad", "Månad", "manad", "månad", "år_månad", "månad_år", "regionkod",
                     "Regionkod", "region", "Region", "Lankod", "Länkod", "lankod", "länkod", 
                     "Län", "Lan", "län", "lan", "Kommunkod", "kommunkod", "Kommun", "kommun")
  
  # skapa en url för att hämta metadata för aktuell tabell - den extraheras med hjälp av url:en till datasetet
  meta_url <- url_fk %>%
    str_replace("/[^/]*$", "/meta.json")
  
  # hämta datasetet
  data_df <- curl_fetch_memory(url_fk) %>% 
    {rawToChar(.$content)} %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    select(-contains(c("rojd"))) %>% 
    rename_with(~ str_remove_all(., "observations\\.|\\.value|dimensions\\.")) %>% 
    select(-row_nr)
  
  # Hämta metadata för datasetet
  meta_df <-  curl_fetch_memory(meta_url) %>% 
    {rawToChar(.$content)} %>% 
    jsonlite::fromJSON(flatten = TRUE)
  
  tabellnamn <- meta_df$key
  
  # Skapa en tabell med nycklar och etiketter för kolumnnamn som vi lägger till övriga kolumnnamn senare i skriptet
  meta_kol <- meta_df$table$columns %>% 
    mutate(element_namn = "meta_dim", under_element = NA) %>% 
    select(-format)
  
  # skapa en lista som vi använder för att extrahera klartext-värden med hjälp av key-label
  meta_dim <- meta_df$filter$dimension
  names(meta_dim$values) <- meta_dim$key       # döp om values från key i meta_dim
  
  # Här hämtar vi alla klartextvärden för samtilga variabler i datasetet
  nyckel_etikett_tabell <- json_extrahera_nyckel_etikett(meta_dim) 
  
  # Vi extaherar klartextvärden för kolumnnamnen och lägger på från meta_kol
  kolumnnamn <- nyckel_etikett_tabell %>% 
    filter(element_namn == "meta_dim") %>% 
    bind_rows(meta_kol) %>% 
    distinct() %>% 
    filter(key %in% colnames(data_df)) 
  
  # ta bort kolumner i nyckel_etikett_tabell som inte finns i datasetet
  nyckel_etikett_tabell <- nyckel_etikett_tabell %>% 
    filter(under_element %in% kolumnnamn$key)
  
  # Uppdatera värdena i kolumnerna
  data_df2 <- json_ersatt_nycklar_med_etiketter(data_df, nyckel_etikett_tabell) %>% 
    rename_with(~ kolumnnamn$label[match(., kolumnnamn$key)], .cols = kolumnnamn$key)
  
  fk_json <- data_df2 %>% 
    select(any_of(kolumnordning), where(~ !is.numeric(.x)), where(is.numeric))
  
  if (sum(c("Kommun", "Län") %in% names(fk_json)) > 1) fk_json <- fk_json %>%
    select(-any_of(c("län", "Län", "lan", "Lan")))
  
  if ("Kommun" %in% names(fk_json)) fk_json <- fk_json %>%
    separate_wider_delim(Kommun, delim = " ", names =  c("Regionkod", "Region"), too_few = "align_end", too_many = "merge")
  
  if ("Län" %in% names(fk_json)) fk_json <- fk_json %>%
    separate_wider_delim(Län, delim = " ", names =  c("Länskod", "Län"), too_few = "align_end", too_many = "merge")
  
  fk_json <- fk_json %>% 
    mutate(Tabellnamn = tabellnamn) %>% 
    relocate(Tabellnamn, .before = 1)
  
  return(fk_json)
  
} # slut funktion

json_extrahera_subdimensions <- function(meta_dim) {
  # Kontrollera om subdimensions.values finns i values
  map_dfr(seq_along(meta_dim$values), ~ {
    value <- meta_dim$values[[.x]]
    if (!is.null(value$subdimension.values) &&
        is.list(value$subdimension.values) &&
        "subdimension.key" %in% colnames(value)) {
      
      # Kontrollera att subdimension.key är av rätt storlek
      sub_key <- value$subdimension.key
      
      # Iterera över subdimension.values
      map2_dfr(value$subdimension.values, sub_key, ~ {
        if (is.data.frame(.x)) {
          .x %>%
            mutate(
              element_namn = "subdimensions.values",
              under_element = .y # Koppla till respektive subdimension.key
            ) %>%
            select(element_namn, under_element, key, label) # Behåll endast relevanta kolumner
        } else {
          # Returnera en tom tibble om det inte är en data frame
          tibble(element_namn = character(), under_element = character(), key = character(), label = character())
        }
      })
    } else {
      # Returnera en tom tibble om subdimensions.values inte existerar
      tibble(element_namn = character(), under_element = character(), key = character(), label = character())
    }
  })
}

json_extrahera_values <- function(meta_dim) {
  # Funktion för att extrahera nyckel/etikett från values - json-data från Försäkringskassan
  
  map_dfr(seq_along(meta_dim$values), ~ {
    value <- meta_dim$values[[.x]]
    if (is.data.frame(value)) {
      value %>%
        mutate(
          element_namn = "values",
          under_element = meta_dim$key[.x] # Koppla till nyckeln från meta_dim
        ) %>%
        select(element_namn, under_element, key, label)
    } else {
      tibble(element_namn = character(), under_element = character(), key = character(), label = character())
    }
  })
}

json_extrahera_nyckel_etikett <- function(meta_dim) {
  # Extrahera huvuddata från meta_dim
  huvuddata <- meta_dim %>%
    mutate(element_namn = "meta_dim", under_element = NA) %>%
    select(element_namn, under_element, key, label)
  
  # Extrahera data från values
  values_data <- json_extrahera_values(meta_dim)
  
  # Extrahera data från subdimension.values
  subdimensions_data <- json_extrahera_subdimensions(meta_dim)
  
  # Extrahera unika subdimension.key och subdimension.label
  subdimension_keys <- json_extrahera_subdimension_keys(meta_dim)
  
  # Kombinera allt
  bind_rows(huvuddata, values_data, subdimensions_data, subdimension_keys) %>%
    distinct() %>%  # Ta bort eventuella dubbletter
    mutate(
      element_namn = factor(
        element_namn,
        levels = c("meta_dim", "values", "subdimensions.values") # Definiera önskad ordning
      )
    ) %>%
    arrange(element_namn, under_element, key, label)
}


json_extrahera_subdimension_keys <- function(meta_dim) {
  # Iterera genom varje values-tabell och extrahera subdimension.key och subdimension.label
  map_dfr(seq_along(meta_dim$values), ~ {
    value <- meta_dim$values[[.x]]
    if (is.data.frame(value) &&
        "subdimension.key" %in% colnames(value) &&
        "subdimension.label" %in% colnames(value)) {
      value %>%
        select(key = subdimension.key, label = subdimension.label) %>%
        mutate(
          element_namn = "meta_dim",
          under_element = NA
        ) %>%
        distinct() # Ta bort eventuella dubbletter
    } else {
      tibble(element_namn = character(), under_element = character(), key = character(), label = character())
    }
  })
}

json_ersatt_nycklar_med_etiketter <- function(data_df, nyckel_etikett) {
  # Iterera endast över kolumner som matchar "under_element"
  data_df %>%
    mutate(across(
      # Filtrera kolumner som finns i under_element
      all_of(unique(nyckel_etikett$under_element[!is.na(nyckel_etikett$under_element)])),
      ~ {
        # Filtrera nyckel-etikett-tabellen för den aktuella kolumnen
        current_map <- nyckel_etikett %>%
          filter(under_element == cur_column()) %>%
          distinct(key, label) %>%
          deframe() # Omvandla till named vector: key -> label
        
        # Byt ut värden baserat på matchning, eller behåll originalvärdet
        current_map[.x] %>% coalesce(.x)
      }
    )) %>% 
    mutate(across(
      everything(),
      ~ ifelse(. == "Riket", "00 Riket", .)
    )) %>% 
    mutate(across(
      where(is.character),  # Använd bara på karaktärskolumner
      ~ as.character(unname(.)) # Ta bort namn och omvandla
    ))
}


hamta_excel_dataset_med_url <- function(url_excel, 
                                        skippa_rader = 0, 
                                        df_om_bara_en_flik = TRUE,
                                        hoppa_over_flikar = NA,
                                        rbind_dataset = FALSE,
                                        mutate_flik = "kolumnnamn") {
  
  # funktion för att ladda ner en Excelfil och få tillbaka en lista med flera dataset eller bara ett
  # skippa_rader = hoppa över rader i excelflikarna
  # df_om_bara_en_flik = returnera df istället för lista om det bara är en flik i Excelfliken
  # hoppa_over_flikar = här kan man ange flikar som inte ska läsas in som dataset (tex. om det ligger information i en flik som inte tillhör själva datasetet)
  # rbind_dataset = binder ihop dataset som kommer från flera flikar till ett dataset, 
  # mutate_flik = om det är samma kolumner och ingen som särskiljer dataseten så kan man skapa en kolumn som tar värdet från fliknamnet som särskiljer detta dataset från övriga flikar
  
  
  if (!require("readxl")) install.packages("readxl")
  
  # läs in dataset
  td = tempdir()                                                                         # skapa temporär mapp
  excel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")                                    # skapa temorär fil
  #GET(url_excel, write_disk(excel_fil, overwrite = TRUE))                                # ladda ner temporär fil
  curl_fetch_disk(url_excel, path = excel_fil)
  
  flikar <- excel_sheets(excel_fil)                                                      # läs in flikar
  if (!is.na(hoppa_over_flikar)) flikar <- flikar[!flikar %in% hoppa_over_flikar]        # ta bort flikar som läses in om det finns värden i hoppa_over_flikar
  
  dataset_lista <- map(flikar, ~ {
    retur_df <- readxl::read_xlsx(excel_fil, sheet = .x, skip = skippa_rader)
    if (!is.na(mutate_flik)) retur_df <- retur_df %>% mutate(!!mutate_flik := .x)
    return(retur_df)
  })
  names(dataset_lista) <- flikar
  
  if (rbind_dataset) dataset_lista <- dataset_lista %>% list_rbind()                      # bind ihop alla dataset till en datafram om rbind_dataset är TRUE
  
  # om det bara finns en flik och df_om_bara_en_flik är TRUE så returneras bara dataframen (om det inte redan är en dataframe)
  if (df_om_bara_en_flik & length(dataset_lista) == 1 & !is.data.frame(dataset_lista)) dataset_lista <- dataset_lista[[1]]  
  
  return(dataset_lista)
}

oppnadata_hamta <- function(
    schema = NA,
    tabell = NA,
    query = NA) {
  # funktion för att hämta öppna data i geodatabasen, eller lista vilka scheman och tabeller som 
  # finns om man inte skickar med några parametrar
  
  # vi source:ar in enbart de fyra funktioner som behövs från func_GIS.R för att köra denna 
  # funktion, så att vi inte kladdar ner global environment för mycket. 
  
  funktioner_nodvandiga <- c("postgres_hamta_oppnadata", "postgres_lista_scheman_tabeller",
                             "uppkoppling_db", "postgres_tabell_till_df")
  
  # kontrollera om alla nödvändiga funktioner redan är laddade
  funktioner_ar_laddade <- funktioner_nodvandiga[funktioner_nodvandiga %in% ls(envir = .GlobalEnv)]
  funktioner_behover_laddas <- funktioner_nodvandiga[!funktioner_nodvandiga %in% ls(envir = .GlobalEnv)]       # kontrollera om alla nödvändiga funktioner redan är laddade
  
  # om inte alla nödvändiga funktioner redan är laddade så laddas de in från rätt fil
  if (length(funktioner_behover_laddas) > 0) {
    source_funktioner("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R",
                      funktioner_behover_laddas)
  }
  
  retur_df <- postgres_hamta_oppnadata(
    schema = schema,
    tabell = tabell,
    query = query
  )
  
  return(retur_df)  
}

# ================================================= github-funktioner ========================================================

github_lista_repos_analytikernatverket <- function(owner = "Analytikernatverket", skriv_ut_reponamn_i_konsol = TRUE){
  # wrapper för att snabbt lista repos i Analytikernätverkets github
  github_lista_repos(
    owner = owner,
    skriv_ut_reponamn_i_konsol = skriv_ut_reponamn_i_konsol
  )
} 

github_lista_repos <- function(owner = "Region-Dalarna", skriv_ut_reponamn_i_konsol = TRUE) {
  # En funktion för att lista alla repositories som finns hos en github-användare
  # Användaren "Region-Dalarna" är standardinställing så körs funktionen utan 
  # parametrar så listas alla repos för Region-Dalarna
  
  url <- paste0("https://api.github.com/users/", owner, "/repos")
  response <- httr::GET(url)
  content <- httr::content(response, "parsed")
  
  if (!http_type(response) %in% "application/json") {
    stop("API-förfrågan misslyckades")
  }
  
  retur_lista <- tibble::tibble(
    namn = map_chr(content, "name"),
    url = map_chr(content, "html_url"),
    url_clone = paste0(url, ".git")
  )
  
  if (skriv_ut_reponamn_i_konsol){ 
    cat("Repositories hos github-användaren", owner, ":\n", retur_lista$namn %>% paste0(., "\n"))
    invisible(retur_lista)
  } else return(retur_lista)
  
}

gh_dia <- function(f = NA) {
  github_lista_repo_filer(filter = f,
                          repo = "diagram")
}

gh_hamta_analytikernatverket <- function(f = NA) {
  github_lista_repo_filer_analytikernatverket(filter = f,
                                              repo = "hamta_data")
}

gh_ppt <- function(f = NA) {
  github_lista_repo_filer(filter = f,
                          repo = "diagram",
                          till_urklipp = TRUE,
                          skriv_source_konsol = FALSE,
                          skriv_ppt_lista = TRUE)
}

ppt_lista_rader <- function(ppt_url = ""){
  retur_txt <- 
    paste0('ppt_lista <- ppt_lista_fyll_pa(\n',
           '\tppt_lista = ppt_lista,\n',
           '\tsource_url = "', ppt_url, '",\n',
           '\tparameter_argument = list(output_mapp = utmapp_bilder),\n',
           '\tregion_vekt = region_vekt,\n',
           '\tutmapp_bilder = utmapp_bilder)\n\n')
  cat(retur_txt)
  writeLines(text = retur_txt %>% purrr::modify_at(length(.), stringr::str_remove, pattern = "\n"), con = "clipboard", sep = "")
}

github_lista_repo_filer_analytikernatverket <- function(
    repo = "hamta_data",
    owner = "Analytikernatverket",
    url_vekt_enbart = TRUE,
    skriv_source_konsol = TRUE,
    till_urklipp = TRUE,
    filter = NA,
    keyring_github_token = "github_token",
    icke_source_repo = FALSE,
    skriv_ppt_lista = FALSE,
    lista_ej_systemfiler = TRUE,
    path = ""
) {
  # wrapper runt github_lista_repo_filer för att slippa ändra uppgifter till analytikernätverkets
  # default-organisation är "Analytikernatverket"
  github_lista_repo_filer(
    owner = owner,
    repo = repo,
    url_vekt_enbart = url_vekt_enbart,
    skriv_source_konsol = skriv_source_konsol,
    till_urklipp = till_urklipp,
    filter = filter,
    keyring_github_token = keyring_github_token,
    icke_source_repo = icke_source_repo,
    skriv_ppt_lista = skriv_ppt_lista,
    lista_ej_systemfiler = lista_ej_systemfiler,
    path = path
  )
}

github_lista_repo_filer <- function(repo = "hamta_data",                          # repot vars filer vi ska lista
                                    owner = "Region-Dalarna",                     # användaren vars repos vi ska lista
                                    url_vekt_enbart = TRUE,                       # om TRUE returneras en vektor med url:er, annars en dataframe med både filnamn och url
                                    skriv_source_konsol = TRUE,                   # om TRUE returneras färdiga source-satser som man kan klistra in i sin kod
                                    till_urklipp = TRUE,                          # om TRUE skrivs source-satserna till urklipp om skriv_source_konsol är TRUE
                                    filter = NA,                                  # om man vill filtrera filer på specifika sökord så gör man det här, kan vara ett eller en vektor med flera (som körs med OR och inte AND)
                                    keyring_github_token = "github_token",         # om man har sparat en github-token i keyring-paketet så anges service_name här (OBS! Det får bara finnas en användare för denna service i keyring om detta ska fungera)
                                    icke_source_repo = FALSE,                      # om TRUE så returneras bara filnamn och url och inte source-satser
                                    skriv_ppt_lista = FALSE,                      # om TRUE så returneras kodrader för att skapa skript för att lägga in i ppt-lista
                                    lista_ej_systemfiler = TRUE,                    # TRUE filtreras LICENSE och filer som börjar med punkt bort ur listan
                                    rekursiv_korning = FALSE,                     # kan sättas till TRUE om vi kör rekursivt
                                    path = "") {                                  # path används för att hantera mappar
  # En funktion för att lista filer i ett repository som finns hos en github-användare
  
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path)
  
  
  if (requireNamespace("keyring", quietly = TRUE) && "github_token" %in% keyring::key_list()[["service"]]) {
    token_finns <- TRUE
  } else {
    token_finns <- FALSE
  }
  
  # Prova utan token
  response <- httr::GET(url)
  
  # Bara använd token om vi får 401/403
  
  if (token_finns & !rekursiv_korning) {
    response <- httr::GET(url, httr::add_headers(Authorization = paste("token", key_get("github_token", key_list(service = "github_token")$username))))
  } else {
    response <- httr::GET(url)
  }
  
  # Kontrollera om förfrågan lyckades
  if (httr::status_code(response) != 200) {
    stop("API-förfrågan misslyckades med statuskod: ", httr::status_code(response), ". Meddelande: ", httr::content(response)$message)
  }
  
  content <- httr::content(response, "parsed")
  
  # Kontrollera att content är en lista
  if (!is.list(content)) {
    stop("Innehållet från API-förfrågan kunde inte tolkas korrekt.")
  }
  
  if (!httr::http_type(response) %in% "application/json") {
    stop("API-förfrågan misslyckades")
  }
  
  # Gå igenom alla poster och hantera mappar och filer
  retur_df <- purrr::map_df(content, function(item) {
    if (item$type == "dir") {
      
      # Skippa dolda mappar (som börjar med .)
      if (grepl("^\\.", item$name)) {
        #cat("Skippar dold mapp:", item$name, "\n")
        return(tibble::tibble())  # Returnera tom tibble
      }
      
      # Om det är en mapp, rekursera genom att kalla funktionen igen
      # Använd tryCatch för att fånga fel från undermappar
      result <- tryCatch({
        github_lista_repo_filer(
          repo = repo,
          owner = owner,
          url_vekt_enbart = FALSE,
          skriv_source_konsol = FALSE,
          till_urklipp = FALSE,
          filter = filter,
          icke_source_repo = FALSE,        # Lägg till denna
          keyring_github_token = keyring_github_token, # Lägg till denna
          lista_ej_systemfiler = FALSE,                # Sätt till FALSE för undermappar
          rekursiv_korning = TRUE,
          path = paste0(path, item$name, "/")
        )
      }, error = function(e) {
        cat("Varning: Kunde inte läsa mapp:", path, item$name, "/", "\n")
        cat("Felmeddelande:", e$message, "\n")
        return(tibble::tibble())  # Returnera tom tibble vid fel
      }) 
      
      return(result)
      
    } else {
      
      ska_skapa_source <- if (rekursiv_korning) FALSE else icke_source_repo
      # Om det är en fil, returnera dess namn och URL
      tibble::tibble(
        namn = paste0(path, item$name),
        url = item$download_url,
        source = ifelse(ska_skapa_source, item$download_url, paste0('source("', item$download_url, '")\n')),
        ppt_url = ifelse(ska_skapa_source, item$download_url, paste0(item$download_url, '\n')), 
        ppt_lista = ifelse(ska_skapa_source, NA, paste0('ppt_lista <- ppt_lista_fyll_pa(\n', 
                                                        'ppt_lista = ppt_lista,\n',
                                                        'source_url = "', item$download_url, '",\n',
                                                        'parameter_argument = list(output_mapp = utmapp_bilder),\n',
                                                        'region_vekt = region_vekt,\n',
                                                        'utmapp_bilder = utmapp_bilder)\n\n'
        )))
    }
  })
  
  if (lista_ej_systemfiler) retur_df <- retur_df %>% filter(namn != "LICENSE", str_sub(namn, 1, 1) != ".")
  
  # Filtrera baserat på sökord om filter inte är NA
  if (!any(is.na(filter))) {
    if (length(filter) > 1) filter <- paste0(filter, collapse = "|")
    
    if (length(filter) > 0 & stringr::str_detect(filter, "\\&")) {
      sok_vekt <- stringr::str_split(filter, "\\&") %>% unlist()
      retur_df <- retur_df %>% 
        dplyr::filter(purrr::reduce(sok_vekt, ~ .x & {
          if (stringr::str_detect(.y, "^!")) {
            !stringr::str_detect(tolower(namn), tolower(stringr::str_remove(.y, "^!")))
          } else {
            stringr::str_detect(tolower(namn), tolower(.y))
          }
        }, .init = TRUE))
    } else {
      if (stringr::str_detect(filter, "^!")) {
        retur_df <- retur_df %>% dplyr::filter(!stringr::str_detect(tolower(namn), tolower(stringr::str_remove(filter, "^!"))))
      } else {
        retur_df <- retur_df %>% dplyr::filter(stringr::str_detect(tolower(namn), tolower(filter)))
      }
    }
    
    if (nrow(retur_df) == 0) stop("Inga filer hittades som matchade sökorden.")
  }
  
  # Kolla om source-kolumnen finns innan vi försöker välja eller ta bort den
  if (skriv_source_konsol && "source" %in% names(retur_df)) {
    cat(retur_df$source)
    if (till_urklipp) {
      writeLines(text = retur_df$source %>% purrr::modify_at(length(.), stringr::str_remove, pattern = "\n"), con = "clipboard", sep = "")
    }
  } else if (skriv_ppt_lista) {
    cat(retur_df$ppt_url)
    if (till_urklipp) {
      writeLines(text = retur_df$ppt_lista %>% purrr::modify_at(length(.), stringr::str_remove, pattern = "\n"), con = "clipboard", sep = "")
    }
  } else if (url_vekt_enbart) {
    return(retur_df$url)
  } else {
    # Kontrollera om 'source'-kolumnen finns innan vi försöker ta bort den
    if ("source" %in% names(retur_df) && !rekursiv_korning) {
      return(retur_df %>% dplyr::select(-source))
    } else {
      return(retur_df)
    }
  }
}

github_status_filer_lokalt_repo_analytikernatverket <- function(
    repo = "hamta_data",
    sokvag_lokal_repo = "c:/gh_an/"
) {
  # wrapper runt github_status_filer_lokalt_repo för att slippa ändra uppgifter till analytikernätverkets
  # default-sökväg lokalt är "c:/gh_an/"
  github_status_filer_lokalt_repo(
    repo = repo,
    sokvag_lokal_repo = sokvag_lokal_repo
  )
}


github_status_filer_lokalt_repo <- function(
    repo = "hamta_data",                          # repot vars filer vi ska lista
    sokvag_lokal_repo = "c:/gh/"
) {
  
  # En funktion för att lista status på filer i ett repository som finns hos en github-användare
  # Användaren "Region-Dalarna" är standardinställing och standardinställning för repo
  # är "hamta_data" så körs funktionen utan parametrar så status för lokala filer i repot
  # "hamta_data" för github-användaren Region-Dalarna, dvs. om det finns ändrade eller nya filer som ännu 
  # inte skickats upp till github-repositoryt
  
  lokal_sokvag_repo <- paste0(sokvag_lokal_repo, repo)
  
  lokalt_repo <- git2r::init(lokal_sokvag_repo)
  repo_status <- git2r::status(lokalt_repo)
  
  retur_meddelande <- NULL
  
  # helt nya filer
  if (length(repo_status$untracked) == 1){
    fil_txt <- "ny fil"
    dessa_txt <- "Det är"
  }  else  {
    fil_txt <- "nya filer"
    dessa_txt <- "Dessa är"
  }
  if (length(repo_status$untracked) > 0) retur_meddelande <- paste0(retur_meddelande, 
                                                                    glue("Det finns {length(repo_status$untracked)} {fil_txt} i {lokal_sokvag_repo}\n{dessa_txt} är:\n{paste0(repo_status$untracked, collapse = '\n')}\n\n"))
  # ändrade filer
  if (length(repo_status$unstaged) == 1){
    fil_txt <- "fil"
    dessa_txt <- "Det är"
  }  else  {
    fil_txt <- "filer"
    dessa_txt <- "Dessa är"
  }
  if (length(repo_status$unstaged) > 0) retur_meddelande <- paste0(retur_meddelande, 
                                                                   glue("{length(repo_status$unstaged)} {fil_txt} i {lokal_sokvag_repo} har ändrats, och {tolower(dessa_txt)}:\n{paste0(repo_status$unstaged, collapse = '\n')}\n\n"))
  # stage:ade filer men ännu inte push:ade till github.com
  if (length(repo_status$staged) == 1){
    fil_txt <- "fil"
    dessa_txt <- "Det är"
    stage_txt <- "stage:ad"
    push_txt <- "push:ad"
  }  else  {
    fil_txt <- "filer"
    dessa_txt <- "Dessa är"
    stage_txt <- "stage:ade"
    push_txt <- "push:ade"
  }
  if (length(repo_status$staged) > 0) retur_meddelande <- paste0(retur_meddelande, 
                                                                 glue("{length(repo_status$staged)} {fil_txt} i {lokal_sokvag_repo} har ändrats och är {stage_txt} men ännu inte {push_txt} till github.com, och {tolower(dessa_txt)}:\n{paste0(repo_status$staged, collapse = '\n')}"))
  if (length(retur_meddelande) == 0) retur_meddelande <- glue("Inga nya filer eller ändringar av befintliga filer har gjorts i {lokal_sokvag_repo}.")
  cat(retur_meddelande)
  
} # slut funktion


github_commit_push_analytikernatverket <- function(
    repo = "hamta_data",
    commit_txt = NA,
    sokvag_lokal_repo = "c:/gh_an/",
    repo_org = "Analytikernatverket",
    fran_rmarkdown = FALSE,
    pull_forst = TRUE
) {
  # wrapper runt github_commit_push för att slippa ändra uppgifter till analytikernätverkets
  # default-sökväg lokalt är "c:/gh_an/"
  github_commit_push(
    repo = repo,
    commit_txt = commit_txt,
    sokvag_lokal_repo = sokvag_lokal_repo,
    repo_org = repo_org,
    fran_rmarkdown = fran_rmarkdown,
    pull_forst = pull_forst
  )
  
}

github_commit_push <- function(
    repo = "hamta_data",
    commit_txt = NA,
    sokvag_lokal_repo = "c:/gh/",
    repo_org = "Region-Dalarna",
    fran_rmarkdown = FALSE,
    pull_forst = TRUE) {
  
  # Testa om git fungerar med nuvarande HOME, annars prova USERPROFILE
  git_test <- suppress_specific_warning(system2("git", args = "--version", stdout = TRUE, stderr = TRUE), warn_text = "status 128")
  if (isTRUE(attr(git_test, "status") == 128)) {
    old_home <- Sys.getenv("HOME")
    Sys.setenv(HOME = Sys.getenv("USERPROFILE"))
    on.exit(Sys.setenv(HOME = old_home), add = TRUE, after = FALSE)
    
    # Verifiera att det faktiskt fungerade
    git_test2 <- system2("git", args = "--version", stdout = TRUE, stderr = TRUE)
    if (isTRUE(attr(git_test2, "status") == 128)) {
      stop("Kan inte nå git, varken med HOME eller USERPROFILE. Kontrollera nätverksuppkopplingen.")
    }
  }
  
  # Säkerställ att github_token finns i keyring innan vi kör vidare - används
  # för både pull och push, och ger annars ett kryptiskt fel längre ner.
  if (nrow(keyring::key_list(service = "github_token")) == 0) {
    stop("Ingen nyckel hittades för service 'github_token' i keyring. ",
         "Kör keyring::key_set('github_token', username = '<ditt-github-anvandarnamn>') ",
         "och ange ett Personal Access Token med scope 'repo' (och 'workflow' om du pushar workflow-filer).")
  }
  
  lokal_sokvag_repo <- paste0(sokvag_lokal_repo, repo)
  
  # Skydd mot parallell körning (låser processen)
  lockfil <- file.path(tempdir(), paste0("github_push_lock_", repo, ".lock"))
  if (file.exists(lockfil)) stop("🛑 En annan push-process verkar redan köra.")
  writeLines(as.character(Sys.time()), lockfil)
  on.exit(unlink(lockfil), add = TRUE)
  
  
  push_repo <- git2r::repository(lokal_sokvag_repo)
  #repo_status <- git2r::status(push_repo)
  repo_status <- system2("git", args = c("-C", lokal_sokvag_repo, "status", "--porcelain"), stdout = TRUE)
  
  # Kategorisera filer
  untracked_files   <- sub("^.. ", "", repo_status[grepl("^\\?\\?", repo_status)])
  unstaged_modified <- sub("^.. ", "", repo_status[grepl("^ M", repo_status)])
  unstaged_deleted  <- sub("^.. ", "", repo_status[grepl("^ D", repo_status)])
  staged_added      <- sub("^.. ", "", repo_status[grepl("^A", repo_status)])
  staged_modified   <- sub("^.. ", "", repo_status[grepl("^M ", repo_status)])
  staged_deleted    <- sub("^.. ", "", repo_status[grepl("^D ", repo_status)])
  
  if (any(lengths(list(untracked_files, unstaged_modified, unstaged_deleted,
                       staged_added, staged_modified, staged_deleted)) > 0)) {
    
    if (!fran_rmarkdown) {
      # Hämta remote repository-filnamn
      github_fillista <- github_lista_repo_filer(owner = repo_org,
                                                 repo = repo,
                                                 url_vekt_enbart = FALSE,
                                                 skriv_source_konsol = FALSE)$namn
      
      # Filklassificering
      filer_tillagda  <- unique(c(unlist(staged_added), unlist(untracked_files)))                 #[!untracked_files %in% github_fillista]))
      filer_andrade   <- unique(c(unlist(staged_modified), unlist(unstaged_modified)))
      filer_borttagna <- unique(c(unlist(staged_deleted),  unlist(unstaged_deleted)))
      
      # Sammanställningsmeddelanden
      konsolmeddelande <- paste0(
        if (length(filer_tillagda) > 0) {
          paste0("Tillagda filer:\n", paste(filer_tillagda, collapse = "\n"), "\n\n")
        } else "",
        if (length(filer_andrade) > 0) {
          paste0("Ändrade filer:\n", paste(filer_andrade, collapse = "\n"), "\n\n")
        } else "",
        if (length(filer_borttagna) > 0) {
          paste0("Borttagna filer:\n", paste(filer_borttagna, collapse = "\n"), "\n\n")
        } else ""
      )
      
      if (is.na(commit_txt)) {
        commit_txt <- str_replace_all(konsolmeddelande, "\n\n", "") %>% str_replace_all("\n", " ")
      }
    } else {
      konsolmeddelande <- commit_txt
    }
    
    # Pull innan push (om aktiverat)
    if (pull_forst) {
      # Kontrollera att current branch har en tracking branch innan pull
      head_branch <- git2r::repository_head(push_repo)
      if (!is.null(git2r::branch_target(head_branch))) {
        git2r::pull(repo = push_repo,
                    credentials = cred_user_pass(username = key_list(service = "github_token")$username,
                                                 password = key_get("github_token", key_list(service = "github_token")$username)))
      } else {
        message("⚠️ Ingen upstream-branch är satt – skippar git pull.")
      }
    }
    
    # Lägg till och comitta alla ändrade filer
    stderr_output <- system2("git", args = c("-C", lokal_sokvag_repo, "add", "."), 
                             stdout = TRUE, stderr = TRUE)
    
    # Filtrera bort LF/CRLF-varningar
    relevanta_fel <- stderr_output[!grepl("LF will be replaced by CRLF|CRLF will be replaced by LF", 
                                          stderr_output)]
    
    if (length(relevanta_fel) > 0) warning(paste(relevanta_fel, collapse = "\n"))
    
    git2r::commit(push_repo, commit_txt)
    
    git2r::push(object = push_repo,
                credentials = cred_user_pass( username = key_list(service = "github_token")$username,
                                              password = key_get("github_token", key_list(service = "github_token")$username)))
    
    cat(paste0("Commit och push till ", repo, " på ", repo_org ,"s Github är klar.\n\n", konsolmeddelande))
    
  } else {
    print("Inga nya eller uppdaterade filer att ladda upp till Github.")
  }
}

github_pull_lokalt_repo_fran_github_analytikernatverket <- function(
    repo = "hamta_data",
    sokvag_lokal_repo = "c:/gh_an/",
    repo_org = "Analytikernatverket"
) {
  # wrapper runt github_pull_lokalt_repo_fran_github för att slippa ändra uppgifter till analytikernätverkets
  # default-sökväg lokalt är "c:/gh_an/"
  github_pull_lokalt_repo_fran_github(
    repo = repo,
    sokvag_lokal_repo = sokvag_lokal_repo,
    repo_org = repo_org
  )
}


github_pull_lokalt_repo_fran_github <- function(
    repo = "hamta_data",
    sokvag_lokal_repo = "c:/gh/",
    repo_org = "Region-Dalarna"
) {
  
  if (all(repo == "*")) repo <- list.files(sokvag_lokal_repo)
  
  walk(repo, ~ {
    
    lokal_sokvag_repo <- paste0(sokvag_lokal_repo, .x)
    
    push_repo <- git2r::repository(lokal_sokvag_repo)
    #repo_status <- git2r::status(push_repo)
    
    # Om repo-initialiseringen misslyckas, hoppa över med ett meddelande
    if (is.null(push_repo)) {
      cat("Kunde inte initiera repositoryt", .x, ". Hoppar över.\n")
      flush.console()
      #return(NULL)
    }
    
    resultat <- tryCatch(
      {
        head_branch <- git2r::repository_head(push_repo)
        
        if (is.null(git2r::branch_target(head_branch))) {
          return("Ingen tracking-branch kopplad – skippar pull.")
        }
        
        git2r::pull(
          repo = push_repo,
          credentials = cred_user_pass(
            username = key_list(service = "github")$username,
            password = key_get("github", key_list(service = "github")$username)
          )
        )
      },
      error = function(e) {
        return(paste("Fel vid git pull:", e$message))
      }
    )
    
    
    # resultat <- git2r::pull( repo = push_repo,                 
    #               credentials = cred_user_pass( username = key_list(service = "github")$username, 
    #                                            password = key_get("github", key_list(service = "github")$username)))
    cat(paste0(.x, ": "))
    flush.console()
    if (is.character(resultat)) {
      cat(resultat, "\n")  # Använd cat() för att skriva ut utan citationstecken och [1]
    } else {
      print(resultat)
    }
    flush.console()
  }) # slut walk-funktion
} # slut funktion

github_lagg_till_repo_fran_github <- function(repo_namn,   # bara själva namnet, inte url:en, tex. "hamta_data" eller "kartor"
                                              repo_org = "Region-Dalarna",
                                              repo_lokalt_mapp = "c:/gh/",
                                              rprojekt_oppna = FALSE) {
  
  # Skript för att lägga till ett repo från Github lokalt på en dator
  # Parametrar:
  # - repo_namn:        Namnet på det repository som ska hämtas (t.ex. "hamta_data").
  # - repo_org:         Organisation på GitHub (default = "Region-Dalarna").
  # - repo_lokalt_mapp: Lokal sökväg där repos lagras (default = "c:/gh/").
  # - rprojekt_oppna:   Om TRUE öppnas det som ett R-projekt efter nedladdning,
  #                     annars fortsätter det som vanligt i R-instansen.
  # ---------------------------------------------------------------------------
  
  # kontrollera att git är installerat och finns i PATH  
  if (Sys.which("git") == "") stop("❌ Git finns inte tillgängligt i PATH. Kontrollera Git-installationen.")
  
  if (!nzchar(repo_namn)) stop("❌ repo_namn måste anges.")
  if (!nzchar(repo_org)) stop("❌ repo_org måste anges.")
  
  # Full sökväg lokalt
  lokal_sokvag <- paste0(repo_lokalt_mapp, repo_namn)
  
  # Stoppa om mappen redan finns
  if (dir.exists(lokal_sokvag)) {
    stop("Katalogen finns redan: ", lokal_sokvag)
  }
  
  # Skapa lokal rotmapp om den inte finns
  if (!dir.exists(repo_lokalt_mapp)) {
    dir.create(repo_lokalt_mapp, recursive = TRUE)
  }
  
  github_url <- glue("https://github.com/{repo_org}/{repo_namn}.git")
  
  # Kontrollera att repo:t finns på GitHub
  repo_url_check <- sub("\\.git$", "", github_url)  # ta bort .git för API-kompatibel URL
  response <- tryCatch(
    {
      httr::HEAD(repo_url_check, httr::user_agent("github_lagg_till_repo"))
    },
    error = function(e) {
      stop("❌ Kunde inte nå GitHub för att kontrollera repo: ", conditionMessage(e))
    }
  )
  
  if (httr::status_code(response) == 404) {
    stop("❌ Repo hittades inte på GitHub: \n", repo_url_check,"\n\nAnvänd funktionen github_lista_repos() för att se vilka repositorys som finns hos en användare på Github.")
  } else if (httr::status_code(response) >= 400) {
    stop("❌ Fel vid kontroll av repo på GitHub (status ", httr::status_code(response), "): ", repo_url_check)
  }
  
  
  # Klona från GitHub
  klon_resultat <- tryCatch({
    out <- system2("git", args = c("clone", github_url, shQuote(lokal_sokvag)), stdout = TRUE, stderr = TRUE)
    cat("🔍 Git clone output:\n", paste(out, collapse = "\n"), "\n")
    out
  }, error = function(e) {
    stop("❌ Fel vid git clone: ", conditionMessage(e))
  })
  
  # 
  # if (!dir.exists(lokal_sokvag)) {
  #   stop("❌ Kloning misslyckades. Kontrollera repo-URL och åtkomst.")
  # }
  
  
  # Skapa .Rproj-fil om den saknas
  rproj_file <- file.path(lokal_sokvag, paste0(repo_namn, ".Rproj"))
  if (!file.exists(rproj_file)) {
    usethis::create_project(lokal_sokvag, open = FALSE, rstudio = TRUE)
  }
  
  # Öppna projektet i RStudio endast om önskat
  if (rprojekt_oppna) {
    usethis::proj_activate(lokal_sokvag)
    message("✅ Projekt öppnat i RStudio: ", lokal_sokvag)
  } else {
    message("✅ Projekt klonat: ", lokal_sokvag)
  }
  
  return(invisible(lokal_sokvag))
}

github_lagg_till_repo_fran_github_analytikernatverket <- function(
    repo_namn,                      # Endast själva namnet, t.ex. "hamta_data" eller "kartor"
    repo_org = "Analytikernatverket",
    repo_lokalt_mapp = "c:/gh_an/",
    rprojekt_oppna = FALSE
) {
  # ---------------------------------------------------------------------------
  # Wrapper runt github_lagg_till_repo_fran_github() med standardinställningar 
  # som pekar mot Analytikernätverkets Github och lokal sökväg "c:/gh_an/".
  
  github_lagg_till_repo_fran_github(
    repo_namn       = repo_namn,
    repo_org        = repo_org,
    repo_lokalt_mapp = repo_lokalt_mapp,
    rprojekt_oppna  = rprojekt_oppna
  )
}


# ================================================= skapa skript-funktioner ========================================================

skapa_hamta_data_skript_pxweb <- function(skickad_url_pxweb = NA, 
                                          tabell_namn = NA, 
                                          output_mapp = NA, 
                                          var_med_koder = NA, 
                                          default_region = "20",
                                          oppna_nya_skriptfilen = TRUE,
                                          skapa_temp_test_fil = TRUE,
                                          skapa_diagram_i_testfil = TRUE
) {
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  
  # funktion för att skapa ett skript för att hämta data från SCB:s pxweb-api
  
  # url = url till api hos scb, t.ex.: "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N", 
  #       men det går bra att skicka med en webb-url också, t.ex.:
  #                                    "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101F/UtlmedbTotNK/"
  # tabell_namn = namn på tabellen, t.ex. "yrke", kommer att ligga först i filnamnet
  # output_mapp = mapp där skriptet ska sparas när det är klart
  # var_med_koder = man kan skicka med variabler som ska få med sin kod i uttaget. Variabeln skrivs med sin kod, t.ex. "yrke2012" för att få ssyk-koder till yrkesvariabeln 
  #                 regionkoder (kommun- eller länskoder) kommer i regel med, men inte bransch- och yrkeskoder tex. Eller andra koder som man kan vilja ha med. 
  #                 Gör så här: 1. Ta reda på vad koden är för din variabel genom att köra funktionen pxvarlist(<url till tabellen>)
  #                             2. Lägg in det som sträng eller vektor (om det är fler) för parametern var_med_koder, alltså tex. var_med_koder = "SNI2007" eller var_med_koder = c("SNI2007", "SSYK4") om det är fler variabler man vill ha med koder för
  
  # säkerställ att det finns värden för dessa parametrar
  if (all(is.na(skickad_url_pxweb))) stop("Parametrarna 'skickad_url_pxweb', 'tabell_namn' och 'output_mapp' måste vara med för att funktionen ska kunna köras.\n'skickad_url_pxweb' är url till den pxweb-tabell som man vill hämta data från.\n'tabell_namn' är ett namn som beskriver tabellen. Det bör vara så kort som möjligt och inte innehålla mellanslag. Det kan t.ex. vara 'rmi' för de regionala matchningsindikatorerna.\n'output_mapp' är en sökväg till den mapp som man vill spara det nya skriptet i.")  
  if (is.na(tabell_namn)) stop("Parametrarna 'skickad_url_pxweb', 'tabell_namn' och 'output_mapp' måste vara med för att funktionen ska kunna köras.\n'skickad_url_pxweb' är url till den pxweb-tabell som man vill hämta data från.\n'tabell_namn' är ett namn som beskriver tabellen. Det bör vara så kort som möjligt och inte innehålla mellanslag. Det kan t.ex. vara 'rmi' för de regionala matchningsindikatorerna.\n'output_mapp' är en sökväg till den mapp som man vill spara det nya skriptet i.")  
  if (is.na(output_mapp)) stop("Parametrarna 'skickad_url_pxweb', 'tabell_namn' och 'output_mapp' måste vara med för att funktionen ska kunna köras.\n'skickad_url_pxweb' är url till den pxweb-tabell som man vill hämta data från.\n'tabell_namn' är ett namn som beskriver tabellen. Det bör vara så kort som möjligt och inte innehålla mellanslag. Det kan t.ex. vara 'rmi' för de regionala matchningsindikatorerna.\n'output_mapp' är en sökväg till den mapp som man vill spara det nya skriptet i.")  
  
  # kontrollera att output_mapp slutar med "/" eller "\", annars lägger vi till det
  if (!str_sub(output_mapp, nchar(output_mapp)) %in% c("/", "\\")) {
    output_mapp <- paste0(output_mapp, "/")
  }
  
  # bearbeta url:en så att vi kan använda den i funktionen
  webb_url <- skickad_url_pxweb %>% paste0(., collapse = "\n  #\t\t\t\t\t\t\t\t\t\t\t\t")
  url_scb <- kontrollera_pxweb_url(skickad_url_pxweb) 
  
  org_namn <- case_when(str_detect(skickad_url_pxweb, "https://www.statistikdatabasen.scb.se") ~ "SCB:s",
                        str_detect(skickad_url_pxweb, "https://api.scb.se") ~ "SCB:s",
                        str_detect(skickad_url_pxweb, "fohm-app.folkhalsomyndigheten.se") ~ "Folkhälsomyndighetens",
                        str_detect(skickad_url_pxweb, "statistik.tillvaxtanalys.se") ~ "Tillväxtanalys",
                        str_detect(skickad_url_pxweb, "statistik.sjv.se") ~ "Jordbruksverkets") %>% 
    unique()
  
  org_kortnamn <- case_when(str_detect(skickad_url_pxweb, "https://www.statistikdatabasen.scb.se") ~ "scb",
                            str_detect(skickad_url_pxweb, "https://api.scb.se") ~ "scb",
                            str_detect(skickad_url_pxweb, "fohm-app.folkhalsomyndigheten.se") ~ "fohm",
                            str_detect(skickad_url_pxweb, "statistik.tillvaxtanalys.se") ~ "tva",
                            str_detect(skickad_url_pxweb, "statistik.sjv.se") ~ "sjv") %>%
    unique()
  
  region_special_org <- c("tva", "sjv")
  
  if (!require("pacman")) install.packages("pacman")
  #source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  px_meta_list <- map(url_scb, ~ pxweb_get(.x))
  
  px_meta_enkel_list <- extrahera_unika_varden_flera_scb_tabeller(px_meta_list)
  tabell_variabler <- pxvarlist(list(title = NULL, variables = px_meta_enkel_list))
  
  # om det finns fler än en tabell så kollar vi om det finns värden som överlappar mellan tabellerna
  px_meta_overlappande_varden <- if (length(url_scb) > 1) overlappande_varden_pxweb_hantera(px_meta_list, url_scb, var_kod = "Tid") else NULL  
  overlapp_txt <- if (is.null(px_meta_overlappande_varden)) "" else "\t"
  
  # om det finns år och månader så sorterar vi dessa i listan så det blir snyggare när de listas som möjliga värden i parameterlistan
  if ("tid" %in% tolower(tabell_variabler$koder)) {
    px_meta_enkel_list <- sortera_px_variabler(px_meta_enkel_list, sorterings_vars = "tid", sortera_pa_kod = TRUE)
  }
  
  if ("år" %in% tolower(tabell_variabler$koder) & any(str_detect(url_scb, "fohm-app.folkhalsomyndigheten.se"))) {
    px_meta_enkel_list <- sortera_px_variabler(px_meta_enkel_list, sorterings_vars = "år", sortera_pa_kod = TRUE)
  }
  
  if ("månad" %in% tolower(tabell_variabler$koder) & any(str_detect(url_scb, "https://statistik.tillvaxtanalys.se"))) {
    px_meta_enkel_list <- sortera_px_variabler(px_meta_enkel_list, sorterings_vars = "månad", sortera_pa_kod = FALSE)
  }
  
  if ("år" %in% tolower(tabell_variabler$koder) & any(str_detect(url_scb, "https://statistik.tillvaxtanalys.se"))) {
    px_meta_enkel_list <- sortera_px_variabler(px_meta_enkel_list, sorterings_vars = "år", sortera_pa_kod = TRUE)
  }
  # vi skapar en lista som heter px_meta som liknar en lista man får med en vanlig pxweb_get()-funktion
  px_meta <- list(title = px_meta_list[[1]]$title, variables = px_meta_enkel_list)
  
  varlist_koder <- tabell_variabler$koder                                                # hämta vektor med variabelkoder
  
  varlist_giltiga_varden <- map(varlist_koder, ~ pxvardelist(px_meta, .x)$klartext) %>% set_names(tolower(varlist_koder) %>% unique())
  varlist_giltiga_varden_koder <- map(varlist_koder, ~ pxvardelist(px_meta, .x)$kod) %>% set_names(tolower(varlist_koder))
  
  regionvariabel_db <- varlist_koder[tolower(varlist_koder) %in% c("region", "lan", "län", "kommun")]
  
  alder_ar_klartext <- FALSE
  
  # kolla om det finns åldrar i tabellen och hur många det är i så fall med eller utan å, dvs. alder eller ålder
  alder_namn <- names(varlist_giltiga_varden)[
    tolower(names(varlist_giltiga_varden)) %in% c("alder", "ålder")
  ][1]
  
  har_alder <- !is.na(alder_namn)
  
  if (har_alder) {
    alder_varden <- varlist_giltiga_varden[[alder_namn]]
    alder_ar_klartext <- length(alder_varden) < 90
    alder_txt <- if (alder_ar_klartext) "_klartext" else "_koder"
  } else {
    alder_ar_klartext <- FALSE
    alder_txt <- ""
  }
  
  # Kombinera allt till en dataframe
  varlista_info <- tibble(kod = map_chr(px_meta$variables, ~ .x$code),
                          namn = map_chr(px_meta$variables, ~ .x$text),
                          elimination = map_lgl(px_meta$variables, ~ .x$elimination))
  
  # kontrollera hur många contentsvariabler som finns i databasen
  antal_contvar <- length(varlist_giltiga_varden$contentscode)
  
  # skapa en korrekt regionkodslista om vi jobbar med en databas med löpnummer som regionkoder
  # här byter vi ut regionkoder för de organisationer som har löpnummer som regionkoder till riktiga regionkoder
  if (org_kortnamn %in% region_special_org & any(c("region", "lan", "län", "kommun") %in% tolower(varlist_koder))) {
    region_koder_bearbetad <- hamta_regionkod_med_knas_regionkod(px_meta, "*", regionvariabel_db, returnera_nyckeltabell = TRUE)$regionkod
  } else {
    if (length(regionvariabel_db) > 0) {
      region_koder_bearbetad <- varlist_giltiga_varden_koder[[tolower(regionvariabel_db)]]
    }
  }
  
  
  # här skapar vi en lista med parametrar som ska skickas med till funktionen, den som är först i hämta data-funktionen
  funktion_parametrar <- pmap_chr(list(varlist_koder, varlist_giltiga_varden, varlist_giltiga_varden_koder), 
                                  function(var_koder, varden_klartext, varden_koder) {
                                    
                                    ar_elimination <- varlista_info$elimination[varlista_info$kod == var_koder]            # hämta information om aktuell variabel kan elimineras ur tabellen
                                    elim_info_txt <- if(ar_elimination) " NA = tas inte med i uttaget, " else ""    # skapa text som används som förklaring vid parametrarna i funktionen
                                    
                                    # korrigera region-koder om det är en databas med löpnummer som regionkoder
                                    if (org_kortnamn %in% region_special_org & any(c("region", "lan", "län", "kommun") %in% tolower(var_koder))){
                                      varden_koder <- region_koder_bearbetad
                                    }
                                    
                                    # hantering av ett stort antal koder, som kan bli för mycket att skriva
                                    # ut i parameterlistan efter "Finns: "
                                    antal_alla_koder <- length(varden_koder)
                                    # om det finns fler värden än 50 st så tas de 2 första och de 2 sista ut för 
                                    # varje unik längd på värdet
                                    varden_koder <- tibble(varden_koder) %>%
                                      mutate(textlangd = nchar(varden_koder)) %>%          # beräkna längden per element
                                      group_by(textlangd) %>%
                                      filter(
                                        n() <= 50 | row_number() <= 2 | row_number() > n() - 2
                                      ) %>%
                                      ungroup() %>%
                                      dplyr::pull(varden_koder)
                                    
                                    # om värdena fortfarande är fler än 50 så behåller vi bara de 25 första
                                    if (length(varden_koder) > 50) varden_koder <- varden_koder[1:25]
                                    
                                    # om vi har tagit ner antalet värden så skickar vi med "t.ex. " innan
                                    # koderna så att användaren förstår att det inte är alla koder
                                    koder_urval_txt <- if (length(varden_koder) != antal_alla_koder) "t.ex. " else ""
                                    
                                    # hantering av ett för stort antal klartextvariabler på motsvarande sätt
                                    # som koderna ovan, är det för många kortar vi ner antalet så det inte blir för många att lista efter "Finns: "
                                    # hantering av ett stort antal koder, som kan bli för mycket att skriva
                                    # ut i parameterlistan efter "Finns: "
                                    antal_alla_klartext <- length(varden_klartext)
                                    
                                    # om värdena fortfarande är fler än 50 så behåller vi bara de 20 första och de 20 sista
                                    if (length(varden_klartext) > 50) varden_klartext <- c(head(varden_klartext, 10), tail(varden_klartext, 10))
                                    
                                    # om vi har tagit ner antalet värden så skickar vi med "t.ex. " innan
                                    # koderna så att användaren förstår att det inte är alla koder
                                    klartext_urval_txt <- if (length(varden_klartext) != antal_alla_klartext) "t.ex. " else ""
                                    
                                    # skapa parameterlistan
                                    retur_txt <- case_when(str_detect(tolower(var_koder), "fodel") ~ paste0(tolower(var_koder) %>% str_replace_all(" ", "_") %>% byt_ut_svenska_tecken(), '_klartext = "*",\t\t\t# ', elim_info_txt, ' Finns: ', klartext_urval_txt, paste0('"', varden_klartext, '"', collapse = ", ")),
                                                           # gammal nedan, testar att alltid döpa variabeln till region_vekt
                                                           #str_detect(tolower(var_koder), "region|lan|län") ~ paste0(tolower(var_koder) %>% byt_ut_svenska_tecken(), '_vekt = "', default_region, '",\t\t\t# Val av region. Finns: ', paste0('"', varden_koder, '"', collapse = ", ")),
                                                           str_detect(tolower(var_koder), "region|lan|län|kommun") ~ paste0('region_vekt = "', default_region, '",\t\t\t   # Val av region. Finns: ', koder_urval_txt, paste0('"', varden_koder, '"', collapse = ", ")),
                                                           # gammal nedan, testar att alltid döpa till tid_koder
                                                           #tolower(var_koder) %in% c("tid") ~ paste0(tolower(var_koder) %>% byt_ut_svenska_tecken(), '_koder = "*",\t\t\t # "*" = alla år eller månader, "9999" = senaste, finns: ', paste0('"', varden_klartext, '"', collapse = ", ")),
                                                           tolower(var_koder) %in% c("tid") ~ paste0('tid_koder = "*",\t\t\t # "*" = alla år eller månader, "9999" = senaste, finns: ', klartext_urval_txt, paste0('"', varden_klartext, '"', collapse = ", ")),
                                                           tolower(var_koder) %in% c("år", "månader") & org_kortnamn == "fohm" ~ paste0('tid_koder = "*",\t\t\t # "*" = alla år eller månader, "9999" = senaste, finns: ', klartext_urval_txt, paste0('"', varden_klartext, '"', collapse = ", ")),
                                                           # Funktion för att ta lägsta och högsta värde i ålder är borttagen genom att jag satt length(varden_klartext) > 0, ska vara typ kanske 90. Större än 0 = alla så därför är den i praktiken avstängd. 
                                                           tolower(var_koder) %in% c("alder", "ålder") ~ paste0(tolower(var_koder), alder_txt,' = "*",\t\t\t # ', elim_info_txt, ' Finns: ', if(alder_ar_klartext) klartext_urval_txt else koder_urval_txt, paste0('"', if(alder_ar_klartext) varden_klartext else varden_koder , '"', collapse = ", ")),                                                 # gammalt: if (length(varden_klartext) < 0) paste0(tolower(var_koder), '_klartext = "*",\t\t\t # ', elim_info_txt, ' Finns: ', paste0('"', varden_klartext, '"', collapse = ", ")) else paste0(tolower(var_koder), '_koder = "*",\t\t\t # Finns: ', min(varden_klartext), " - ", max(varden_klartext)),
                                                           TRUE ~ paste0(tolower(var_koder) %>% str_replace_all(" ", "_") %>% byt_ut_svenska_tecken(), '_klartext = "*",\t\t\t # ', elim_info_txt, ' Finns: ', klartext_urval_txt, paste0('"', varden_klartext %>% unique(), '"', collapse = ", ")) %>% str_replace("contentscode", "cont")) 
                                    
                                  }) %>% 
    c(., if (antal_contvar > 1) 'long_format = TRUE,\t\t\t# TRUE = konvertera innehållsvariablerna i datasetet till long-format \n\t\t\twide_om_en_contvar = TRUE,\t\t\t# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel' else "") %>%
    c(., 'output_mapp = NA,\t\t\t# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till', paste0('excel_filnamn = "', tabell_namn, '.xlsx",\t\t\t# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges'), 'returnera_df = TRUE\t\t\t# TRUE om man vill ha en dataframe i retur från funktionen') %>%                     # lägg på output-mapp och excel-filnamn som kommer sist i funktionsparametrarna
    str_c('\t\t\t', ., collapse = "\n") %>% 
    str_remove("\t\t\t\n")
  
  # om inte inte län finns som region, byt ut "20" mot "00" eller "*" i funktion_parametrar
  if (any(c("region", "lan", "län", "kommun") %in% tolower(varlist_koder))){
    region_variabel <- varlist_koder[str_detect(tolower(varlist_koder), "region|lan|län|kommun") & !str_detect(tolower(varlist_koder), "fodelse")] %>% tolower() %>% byt_ut_svenska_tecken()
    if (!default_region %in% region_koder_bearbetad) {
      funktion_parametrar <- str_replace(funktion_parametrar, glue('{region_variabel}_vekt = "{default_region}"'), glue('{region_variabel}_vekt = \"00\"'))    # för att säkerställa att det byts ut om region-variabeln heter något annat än region_vekt
      funktion_parametrar <- str_replace(funktion_parametrar, glue('region_vekt = "{default_region}"'), glue('region_vekt = \"00\"'))                          # för att säkerställa att det byts ut även om region-variabeln heter region_vekt
      if (!"00" %in% region_koder_bearbetad) {
        funktion_parametrar <- str_replace(funktion_parametrar, glue('{region_variabel}_vekt = \"00\"'), glue('{region_variabel}_vekt = \"*\"'))
        funktion_parametrar <- str_replace(funktion_parametrar, glue('region_vekt = \"00\"'), glue('region_vekt = \"*\"'))
      }
    }
  } # slut test om Dalarna finns med i tabellen, annars byt ut till riket (00), om inte finns så byt till "*"
  
  # skapa variabel-lista för queryn
  varlist_skriptrader <- paste0("list(\n", 
                                paste(map_chr(varlist_koder, ~paste0("  \t\"", .x, "\" = ", str_c(tolower(.x) %>% str_replace_all(" ", "_") %>% byt_ut_svenska_tecken(), "_vekt"))), collapse = ",\n"), 
                                ")") %>% 
    str_replace("contentscode_vekt", "cont_vekt") %>%
    str_replace("manad_vekt", "tid_vekt") %>% 
    str_replace("ar_vekt", "tid_vekt") %>% 
    str_replace("lan_vekt", "region_vekt") %>% 
    str_replace("kommun_vekt", "region_vekt")
  #str_replace("tid_vekt", "tid_koder") 
  
  # skapa skriptrader för klartext-variabler som måste omvandlas till koder till query-listan, dvs. "vekt_" och sedan variabelnamnet
  var_klartext_skriptrader <- map(varlist_koder, function(var_kod) {
    # koda klartext till vekt för variabler som inte innehåller region, tid, contentscode eller som innehåller "fodel"
    if ((!str_detect(tolower(var_kod), "region|lan|län|kommun|contentscode") | str_detect(tolower(var_kod), "fodel|grupp")) & !tolower(var_kod) %in% c("tid", "år", "månad")) {
      if (px_meta$variables %>%
          keep(~ .x$code == var_kod) %>% 
          map_lgl(~ .x$elimination) %>%
          first()) {
        
        # variabler som går att eliminera (dvs. inte ha med i uttaget)
        if (!(str_detect(tolower(var_kod), "alder|ålder") & !alder_ar_klartext) |
            str_detect(tolower(var_kod), "grupp") |
            (str_detect(tolower(var_kod), "alder") & str_detect(tolower(var_kod), "kon") & str_detect(tolower(var_kod), "fodel"))){
          paste0("  ", tolower(var_kod) %>% str_replace_all(" ", "_") %>% byt_ut_svenska_tecken(), '_vekt <- if (!all(is.na(', tolower(var_kod) %>% byt_ut_svenska_tecken(), '_klartext))) hamta_kod_med_klartext(px_meta, ', tolower(var_kod) %>% str_replace_all(" ", "_") %>% byt_ut_svenska_tecken(), '_klartext, skickad_fran_variabel = "', tolower(var_kod), '") else NA\n')
        } else NA
      } else {    # variabler som inte går att eliminera (göra uttag utan) men som är klartext till kod
        #if (!str_detect(tolower(var_kod), "alder|ålder") | str_detect(tolower(var_kod), "grupp")) paste0("  ", tolower(var_kod) %>% str_replace_all(" ", "_"), '_vekt <- hamta_kod_med_klartext(px_meta, ', tolower(var_kod) %>% str_replace_all(" ", "_"), '_klartext, skickad_fran_variabel = "', tolower(var_kod), '")\n')
        if (!(str_detect(tolower(var_kod), "alder|ålder") & !alder_ar_klartext) |
            str_detect(tolower(var_kod), "grupp") |
            (str_detect(tolower(var_kod), "alder") & str_detect(tolower(var_kod), "kon") & str_detect(tolower(var_kod), "fodel"))) paste0("  ", tolower(var_kod) %>% str_replace_all(" ", "_") %>% byt_ut_svenska_tecken(), '_vekt <- hamta_kod_med_klartext(px_meta, ', tolower(var_kod) %>% str_replace_all(" ", "_") %>% byt_ut_svenska_tecken(), '_klartext, skickad_fran_variabel = "', tolower(var_kod), '")\n')
      }
    } else NA           # om det är koder för region eller ålder så ska de inte med på dessa rader
    
  }) %>% 
    list_c() %>% 
    .[!is.na(.)] %>% 
    str_c(collapse = "")
  
  # vi lägger på en radbrytning om det finns värden här, annars inte
  if (var_klartext_skriptrader != "") var_klartext_skriptrader <- paste0('  # Gör om från klartext till kod som databasen förstår\n',
                                                                         var_klartext_skriptrader, "\n") else NULL
  
  # om vi har 1-årsgrupper för åldrar så ändrar vi från alder_klartext till alder_koder
  # i skriptraderna som gör om klartextvariabler till koder
  if ("alder" %in% names(varlist_giltiga_varden)) {
    if (length(varlist_giltiga_varden$alder) > 90) {
      var_klartext_alder_skriptrader <- '  alder_vekt <- if (all(!is.na(alder_koder))) alder_koder %>% as.character() %>% ifelse(. == "100", "-100+", .) %>% ifelse(. == "tot", "totalt ålder", .) else NA\n'
    } else {
      var_klartext_alder_skriptrader <- '  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA\n'
    }
  } else var_klartext_alder_skriptrader <- NULL            # om inte ålder är med i tabellen
  
  
  # skapa skriptrader för klartext-variabler som kan elimineras om de är NA
  var_klartext_tabort_NA_skriptrader <- map(varlist_koder, function(var_kod) {
    if (px_meta$variables %>%
        keep(~ .x$code == var_kod) %>% 
        map_lgl(~ .x$elimination) %>%
        first()) {
      if (!tolower(var_kod) %in% c("region", "lan")) {  
        paste0('  if (all(is.na(', tolower(var_kod) %>% byt_ut_svenska_tecken(), '_klartext))) varlista <- varlista[names(varlista) != "', var_kod, '"]')
      } else NA
    }  else NA 
  }) %>% 
    list_c() %>% 
    .[!is.na(.)] %>% 
    str_c(collapse = "\n")
  
  # om vi har 1-årsgrupper för åldrar så ändrar vi från alder_klartext till alder_koder i elimineringsraderna
  if (any(c("alder", "ålder") %in% names(varlist_giltiga_varden))) {
    if (length(varlist_giltiga_varden$alder) > 90) {
      if (var_klartext_tabort_NA_skriptrader != "") {
        var_klartext_tabort_NA_skriptrader <- str_replace_all(var_klartext_tabort_NA_skriptrader, "alder_klartext", "alder_koder")
      }
    }
  }
  
  # vi lägger på två radbrytningar på ta bort variabler om de är NA efter variabel-listan om det finns värden här, annars inte
  if (var_klartext_tabort_NA_skriptrader != "") var_klartext_tabort_NA_skriptrader <- paste0(var_klartext_tabort_NA_skriptrader, "\n\n")
  
  # extrahera tabell-id från url:en så att vi kan lägga in det i filnamnet
  tabell_id <- url_scb %>% str_remove("/$") %>% str_remove(".px$") %>% str_extract("/[^/]+$") %>% str_sub(2) %>% paste0(collapse = "_")
  if (nchar(tabell_id) > 40) tabell_id <- ""
  
  # skapa filnamn-suffix som vi använder till filnamnet för hämta data-funktionen
  filnamn_suffix <- map_chr(varlist_koder, ~ tolower(.x) %>% str_replace_all(" ", "_")) %>% .[. != "contentscode"] %>% c(tabell_namn, ., tabell_id, org_kortnamn) %>% str_c(collapse = "_") %>% byt_ut_svenska_tecken()
  
  # skapa ett namn för själva funktionen där inte tabell-id är med
  funktion_namn <- filnamn_suffix %>% str_remove(paste0("_", tabell_id))
  
  # hantera region i databaser med felaktiga länsnamn och där de korrekta koderna ligger tillsammans med klartext i samma kolumn
  
  if (org_kortnamn %in% region_special_org) {
    region_variabel <- varlist_koder[str_detect(tolower(varlist_koder), "region|lan|län|kommun") & !str_detect(tolower(varlist_koder), "fodelse")]
    region_special_skriptrader <- paste0(
      '  # Hantera region-koder när regionkoderna ligger tillsammans med klartext i samma kolumn, och det är löpnummer istället för koder för län och kommuner\n',
      '  region_vekt <- hamta_regionkod_med_knas_regionkod(px_meta, region_vekt, "', region_variabel, '")           # konvertera korrekta läns- och kommunkoder till de löpnummer som används i denna databas\n\n'
    )
  } else region_special_skriptrader <- NULL
  
  if (org_kortnamn %in% c("fohm") & "region" %in% tolower(varlist_koder)) {
    region_variabel <- varlist_koder[str_detect(tolower(varlist_koder), "region") & !str_detect(tolower(varlist_koder), "fodelse")]
    region_special_fohm_skriptrader <- paste0(
      '  # speciallösning för Folkhälsomyndigheten där vi tar bort regionkoder som ligger i klartextkolumnen\n',
      '  px_df <- region_kolumn_splitta_kod_klartext(px_df, "', region_variabel, '")\n\n')
  } else region_special_fohm_skriptrader <- NULL
  
  # hantera tid-variabler (heter år i hlv) 
  tid_skriptrader <- NULL             # om ingadera finns så blir det NULL
  
  tid_skriptrader <- if ("år" %in% tolower(names(varlist_giltiga_varden))) {                      # hlv
    paste0('  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "år")\n',
           '  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar\n\n')
  } else tid_skriptrader
  
  if (any(c("månad", "år") %in% tolower(names(varlist_giltiga_varden)))) {                      # hlv
    tid_variabel <- varlist_koder[str_detect(tolower(varlist_koder), "månad|år")]
    tid_klartext <- tid_variabel %>% svenska_tecken_byt_ut() %>% tolower() %>% paste0(., "_klartext")
    if (any(tolower(varlist_koder) %in% c("år", "månader")) & org_kortnamn == "fohm") tid_klartext <- "tid_koder"
    giltig_var <- ifelse(tolower(tid_variabel) == "månad", "giltiga_manader", paste0("giltiga_", tid_variabel %>% tolower %>% svenska_tecken_byt_ut()))
    tid_skriptrader <- paste0('  px_meta$variables <- sortera_px_variabler(px_meta$variables, sorterings_vars = "', tid_variabel, '", sortera_pa_kod = FALSE)        # sortera om månader så att de kommer i kronologisk ordning\n',
                              '  ', tid_klartext, ' <- ', tid_klartext, ' %>%           # ersätt "9999" med senaste ', tolower(tid_variabel), '\n',
                              '     str_replace_all("9999", hamta_giltiga_varden_fran_tabell(px_meta, "', tid_variabel, '", klartext = TRUE) %>% max())\n',
                              '  ', giltig_var, ' <- hamta_giltiga_varden_fran_tabell(px_meta, "', tid_variabel, '")\n\n',
                              '  if (all(', tid_klartext, ' == "*")) {\n',
                              '      tid_vekt <- ', giltig_var, '\n',
                              '  } else {\n',
                              '     tid_vekt <- map(', tid_klartext, ', function(period) {\n',
                              '        if (str_detect(period, ":")){     # kontrollera om det finns ett kolon = intervall\n',
                              '           intervall <- map_chr(str_split(period, ":") %>% unlist(), ~ hamta_kod_med_klartext(px_meta, .x, "', tid_variabel, '"))\n',
                              '           retur_txt <- ', giltig_var, '[which(', giltig_var, ' == intervall[1]):which(', giltig_var, ' == intervall[2])]\n',
                              '        } else retur_txt <- hamta_kod_med_klartext(px_meta, period, "', tid_variabel, '")\n',
                              '     }) %>% unlist()\n',
                              '     index_period <- map_lgl(px_meta$variables, ~ .x$text == "', tid_variabel, '")          # hitta platsen i px_meta$variables där variabeln "', tid_variabel, '" finns\n',
                              '     period_varden <- px_meta$variables[[which(index_period)]]$values         # läs in alla värden för variabeln "', tid_variabel, '"\n',
                              '    tid_vekt <- tid_vekt[match(period_varden[period_varden %in% tid_vekt], tid_vekt)]        # sortera om tid_vekt utifrån ordningen i px_meta (som vi sorterade ovan) \n',
                              '   }\n\n')
  } else tid_skriptrader <- tid_skriptrader
  
  tid_skriptrader <- if ("tid" %in% tolower(names(varlist_giltiga_varden))) {                     # scb
    paste0('  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")\n',
           '  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar\n\n')
  } else tid_skriptrader
  
  # lägg till kommentar innan tid-skriptraderna om de finns och inte är NULL
  if (!is.null(tid_skriptrader)) tid_skriptrader <- paste0('  # Hantera tid-koder\n', tid_skriptrader)
  
  # skapa skriptrader för att hantera region-koder som ligger i samma kolumn som klartext efter uttaget till px_df
  region_special_px_df_skriptrader <- if (is.null(region_special_skriptrader)) {
    NULL
  } else {
    paste0(
      #'  regionvariabel <- '
      '  px_df <- px_df %>% region_kolumn_splitta_kod_klartext("', region_variabel, '")\n')
  }
  # här hakar vi på skript för skript som har flera tabeller och överlappande värden, annars gör vi ingenting
  if (!is.null(px_meta_overlappande_varden)) {
    overlappning_special <- map_chr(px_meta_overlappande_varden, function(item) {
      url <- item$url
      ar <- item$overlappande_varden
      
      # Skapa skriptet för tabeller som har överlapp som gör att vi tar bort
      # överlappande värden i den tabell som har tidigast år
      paste0(
        "\n",
        "\t# special, ta bort överlappande värden mellan tabeller där de förekommer, värden från nyaste tabellerna behålls\n",
        "\tif (url_uttag == '", url, "') {\n",
        "\t\tif (tid_koder == \"*\") tid_vekt <- giltiga_ar\n",
        "\t\ttid_vekt <- tid_vekt[!tid_vekt %in% c('", paste(ar, collapse = "', '"), "')]\n",
        "\t}\n\n",
        "\tif (length(tid_vekt) > 0) {\n"
      ) # slut paste0
    })  # slut map_chr
    
    # lägg ihop med tid_skriptrader så att dessa rader ovan kommer efter de vanliga tid-skriptraderna
    tid_skriptrader <- paste0(tid_skriptrader, "\n",  overlappning_special)
    
    # ge ett värde till avslutning av if-sats utfall att tid_vekt inte innehåller några värden
    overlappning_if_tid_vekt_ar_noll <- "   } # test om det finns giltig(a) tid-kod(er) i aktuell tabell\n"
    
  } else overlappning_if_tid_vekt_ar_noll <- NULL   # slut if-sats om det finns överlappande värden
  
  
  cont_skriptrader <- if ("contentscode" %in% tolower(names(varlist_giltiga_varden))) {
    paste0('  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")\n',
           '  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE\n\n')
  } else NULL
  
  # skapa skript där användaren kan konvertera datasetet till long_format om det finns mer än en innehållsvariabel
  long_format_skriptrader <- if (antal_contvar > 1){
    
    paste0('  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars\n',
           overlapp_txt, '  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel\n',
           overlapp_txt, '  if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)\n\n')
    
  } else NULL # slut if-sats som kontrollera om vi vill ha df i long-format, blir "" om vi inte har fler än en cont_variabler i tabellen
  
  
  variabler_med_kod <- varlist_koder[str_detect(tolower(varlist_koder), "region|lan") & !str_detect(tolower(varlist_koder), "fodel")]
  
  if (!all(is.na(var_med_koder))) {
    #var_med_koder <- var_med_koder %>% paste0('"', ., '"', collapse = ", ")   
    if (any(var_med_koder %in% varlist_koder)) variabler_med_kod <- c(variabler_med_kod, var_med_koder[var_med_koder %in% varlist_koder]) %>% unique()
  } 
  if (length(variabler_med_kod) > 0) {
    names(variabler_med_kod) <- paste0(tolower(variabler_med_kod), "kod")
    variabler_med_klartext <- tabell_variabler$klartext[match(variabler_med_kod, tabell_variabler$koder)]
    var_vektor_skriptdel <- paste0(
      '  var_vektor <- ', capture.output(dput(variabler_med_kod))%>% paste0(collapse = ""), '\n',
      overlapp_txt, '  var_vektor_klartext <- ', capture.output(dput(variabler_med_klartext)) %>% paste0(collapse = ""), '\n'
    )
  } else {                                    # om det inte finns någon kolumn som vi vill ta med koder för
    var_vektor_skriptdel <- paste0(
      '  var_vektor <- NA\n',
      overlapp_txt,  '  var_vektor_klartext <- NA\n'
    )
  }  # slut if-sats om det finns variabler med kod
  
  # 
  # # lägg in möjligheter att få med koder med variabler
  # variabler_med_kod_skriptrader <- paste0(
  #   '  variabler_med_kod <- varlist_koder[str_detect(tolower(varlist_koder), "region")]\n',
  #   '  if (!is.na(c(', var_med_koder, ')) if (any(', var_med_koder, ' %in% varlist_koder)) variabler_med_kod <- c(variabler_med_kod, ', var_med_koder, '[', var_med_koder, ' %in% varlist_koder]) %>% unique()\n',
  #   '  names(variabler_med_kod) <- paste0(tolower(var_med_koder), "koder")\n',
  #   '  if (length(variabler_med_kod) > 0) variabler_med_klartext <- varlist_bada$klartext[match(variabler_med_kod, varlist_bada$koder)]\n\n'
  # )
  
  # lösning för om man skickar med flera url:er, funkar bara om tabellerna innehåller samma variabler
  # skapa en url-sträng utifrån om vi har en eller flera url:er
  if (length(url_scb) > 1) {
    
    # här skapas url_uttag <- eller url_list <- beroende på om vi har en eller flera url:er
    url_list <- paste0('"', url_scb, '"', collapse = (",\n\t\t\t\t\t\t"))
    url_txt <- paste0('  url_list <- c(', url_list, ')')
    
    # om vi har flera url:er måste vi skapa en funktion som hämtar data från varje url och sätter ihop med map
    hamta_funktion_txt <- "\n\n hamta_data <- function(url_uttag) {\n\n"
    hamta_funktion_slut_txt <- paste0(overlapp_txt, '  return(px_df)\n', overlappning_if_tid_vekt_ar_noll,  '  } # slut hämta data-funktion \n\n')
    map_funktion_txt <- '  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()\n\n'
    px_retur_txt <- "px_alla"
    
  } else {
    
    url_txt <- paste0('  url_uttag <- "', url_scb, '"\n')
    hamta_funktion_txt <- NULL
    hamta_funktion_slut_txt <- NULL
    map_funktion_txt <- NULL
    px_retur_txt <- "px_df"
    
  } 
  
  # Skapar själva strängen med skriptet för att hämta data med pxweb
  query_code <- c(
    'hamta_', funktion_namn, ' <- function(\n',
    funktion_parametrar, '\n',
    '){\n\n',
    paste0(c("  # ", rep("=", times = 100)), collapse = ""),
    "\n",
    "  #\n",
    "  # Funktion för att hämta data från ", org_namn, " API med hjälp av pxweb-paketet\n",
    "  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna\n",
    "  #\n",
    "  # Skapad av: ", Sys.info()["user"], " den ", format(Sys.Date(), "%d %B %Y"), "\n",
    "  # Senast uppdaterad: ", format(Sys.Date(), "%d %B %Y"), "\n",
    "  #\n",
    "  # url till tabellens API: ", webb_url, "\n",
    "  #\n",
    paste0(c("  # ", rep("=", times = 100)), collapse = ""),
    '\n\n',
    '  if (!require("pacman")) install.packages("pacman")\n',
    '  p_load(pxweb,\n',
    '    \t\t\ttidyverse,\n',
    '    \t\t\twritexl)\n\n',
    '  # Behändiga funktioner som används i skriptet\n',
    '  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")\n\n',
    '  # Url till databas\n',
    url_txt,
    hamta_funktion_txt,
    '  px_meta <- pxweb_get(url_uttag)\n\n',
    '  varlist_koder <- pxvarlist(px_meta)$koder\n',
    '  varlist_bada <- pxvarlist(px_meta)\n\n',
    var_klartext_skriptrader,
    var_klartext_alder_skriptrader,
    cont_skriptrader,
    region_special_skriptrader,
    tid_skriptrader,
    '  # query-lista till pxweb-uttag\n',
    '  varlista <- ', varlist_skriptrader, '\n\n',
    var_klartext_tabort_NA_skriptrader,
    '  # Hämta data med varlista\n',
    '  px_uttag <- pxweb_get(url = url_uttag, query = varlista)\n\n',
    var_vektor_skriptdel, '\n',
    '  # gör om pxweb-uttaget till en dataframe\n',
    '  px_df <- as.data.frame(px_uttag)\n',
    region_special_px_df_skriptrader,
    '  if (!all(is.na(var_vektor))) {\n',
    '      # om man vill ha med koder också för variabler utöver klartext så läggs de på här (om det finns värden i var_vektor)\n',
    '      px_df <- px_df %>%\n',
    '            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%\n',
    '            select(any_of(var_vektor)))\n\n',
    '      # kolumnerna med koder läggs framför motsvarande kolumner med klartext\n',
    '      for (varflytt_index in 1:length(var_vektor)) {\n',
    '        px_df <- px_df %>%\n',
    '            relocate(any_of(names(var_vektor)[varflytt_index]), .before = any_of(var_vektor_klartext[varflytt_index]))\n',
    '      }\n',
    '  }\n\n',
    region_special_fohm_skriptrader,
    long_format_skriptrader,
    hamta_funktion_slut_txt,
    map_funktion_txt,
    '  # Om användaren vill spara data till en Excel-fil\n',
    '  if (!is.na(output_mapp) & !is.na(excel_filnamn)){\n',
    '    write.xlsx(', px_retur_txt, ', paste0(output_mapp, excel_filnamn))\n',
    '  }\n\n',
    '  # Returnera data som en dataframe om användern valt det\n',
    '  if (returnera_df) return(', px_retur_txt, ')\n',
    '} # slut hämta data-funktion'
  )
  
  # om vi har överlappande år i flera tabeller så vill vi skjuta in
  # koden lite till för att bli mer lättläst, sker nedan i så fall
  if (!is.null(px_meta_overlappande_varden)){
    
    # startrad för den del vi vill lägga till ett tabtecken för
    start_index <- which(str_detect(query_code, "if \\(length\\(tid_vekt\\) > 0\\)"))
    # slutrad för den del vi vill lägga till ett tabtecken för
    end_index <- which(str_detect(query_code, "return\\(px_df\\)"))
    
    # Lägg till ett tab-tecken för elementen mellan start_index och end_index
    query_code[(start_index + 1):(end_index - 1)] <- paste0("\t", query_code[(start_index + 1):(end_index - 1)])
    
    # Konvertera query_code till en enda lång textsträng
    query_code <- query_code %>% paste0(collapse = "")
    
  } else query_code <- query_code %>% paste0(collapse = "")
  
  # Skriv ut den genererade koden - om man vill kolla att skriptet verkar stämma
  #cat(query_code)
  
  # Alternativt, om du vill skapa en skriptfil istället:
  writeLines(query_code, paste0(output_mapp, "hamta_", filnamn_suffix, ".R"))
  
  # Öppna filen i RStudio om användaren inte valt bort det
  if (oppna_nya_skriptfilen) file.edit(paste0(output_mapp, "hamta_", filnamn_suffix, ".R"))
  
  # Skapa en testfil som kan användas för att testa skriptet genom att source:a in det nya
  # hämta data-skriptet och ladda en dataframe med data
  if (skapa_temp_test_fil) {
    temp_dir <- glue("{tempdir()}\\")
    filnamn_testfil <- glue("test_hamta_{tabell_namn}.R")
    
    # ta bort alla tecken i px_meta$title från och med strängen "efter" med str_extract
    auto_diag_titel <- px_meta$title %>% str_remove(" efter[^.]*$")
    
    
    # testa om vissa variabler finns med i datasetet
    region_txt <- if("region" %in% names(varlist_giltiga_varden)) paste0(' i {unique(', tabell_namn, '_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()}') else ""
    regionkod_txt <- if("region" %in% names(varlist_giltiga_varden)) paste0('{unique(', tabell_namn, '_df$regionkod) %>% paste0(collapse = \'_\')}') else ""
    # testar om medskickade regioner är samtliga kommuner i ett län eller samtliga län i Sverige
    
    regionfix_txt <- if("region" %in% names(varlist_giltiga_varden)) paste0('\n\n# om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte\n',
                                                                            'region_start <- unique(', tabell_namn, '_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()\n',
                                                                            'region_txt <- ar_alla_kommuner_i_ett_lan(unique(', tabell_namn, '_df$regionkod), returnera_text = TRUE, returtext = region_start)\n',
                                                                            'region_txt <- ar_alla_lan_i_sverige(unique(', tabell_namn, '_df$regionkod), returnera_text = TRUE, returtext = region_txt)\n',
                                                                            'regionfil_txt <- region_txt\n',
                                                                            'region_txt <- paste0(" i ", region_txt)\n',
                                                                            'regionkod_txt <- if (region_start == region_txt) unique(', tabell_namn, '_df$regionkod) %>% paste0(collapse = "_") else region_txt') else ""
    region_i_glue <- if("region" %in% names(varlist_giltiga_varden)) "{region_txt}" else ""
    regionkod_i_glue <- if("region" %in% names(varlist_giltiga_varden)) "{regionfil_txt}" else "" 
    
    tid_varnamn <- NULL
    tid_txt <- if("tid" %in% names(varlist_giltiga_varden)) {
      tid_varnamn <- varlista_info$namn[tolower(varlista_info$kod) == "tid"]
      paste0(' ', tid_varnamn, ' {min(', tabell_namn, '_df$', tid_varnamn, ')} - {max(', tabell_namn, '_df$', tid_varnamn, ')}')
    } else if ("år" %in% tolower(names(varlist_giltiga_varden))) {
      tid_varnamn <- varlista_info$namn[tolower(varlista_info$kod) == "år"]
      paste0(' ', tid_varnamn, ' {min(', tabell_namn, '_df$', tid_varnamn, ')} - {max(', tabell_namn, '_df$', tid_varnamn, ')}')
    } else ""
    tid_filnamn_txt <- if(any(c("tid", "år") %in% names(varlist_giltiga_varden))) paste0('_ar{min(', tabell_namn, '_df$', tid_varnamn, ')}_{max(', tabell_namn, '_df$', tid_varnamn, ')}') else NULL
    if (is.null(tid_filnamn_txt)) tid_filnamn_txt <- if(any(c("månad", "månader") %in% names(varlist_giltiga_varden))) paste0('_manad{min(', tabell_namn, '_df$', tid_varnamn, ')}_{max(', tabell_namn, '_df$', tid_varnamn, ')}') else NULL
    
    # om skriptet skapas på Regionala utvecklingsförvaltningen på Region Dalarna så 
    #         läggs '\nBearbetning: Samhällsanalys, Region Dalarna"' till i diagram_capt, ananrs inte
    bearbetad_txt <- if (dir.exists(utskriftsmapp())) '\\nBearbetning: Samhällsanalys, Region Dalarna"' else '"'
    #         blir "G:/Samhällsanalys/API/Fran_R/Utskrift/" utskriftsmapp, annars blir det samma mapp som hämta-skriptet skapas i
    utskrift_mapp <- if (dir.exists(utskriftsmapp())) utskriftsmapp() else output_mapp
    
    testfil_skript <- glue('if (!require("pacman")) install.packages("pacman")\n',
                           'p_load(tidyverse,\n',
                           '   \t\t\tglue)\n\n',
                           'source("', paste0(output_mapp, "hamta_", filnamn_suffix, ".R"), '")\n',
                           'source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")\n',
                           'source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")\n\n',
                           'diagram_capt <- "Källa: {org_namn} öppna statistikdatabas{bearbetad_txt}\n',
                           'output_mapp <- "{utskrift_mapp}"\n',
                           'visa_dataetiketter <- FALSE\n',
                           'gg_list <- list()\n\n',
                           '{tabell_namn}_df <- hamta_{funktion_namn}(\n',
                           '{funktion_parametrar}\n\n)')
    
    y_var_txt <- if (length(varlist_giltiga_varden$contentscode) < 1) glue("names({tabell_namn}_df)[length(names({tabell_namn}_df))]") else varlist_giltiga_varden$contentscode[1]        # om det inte finns någon contents-variabel, kör sista variabeln som y-variabel istället
    testfil_diagram <- glue('{regionfix_txt}\n\ndiagramtitel <- glue("', auto_diag_titel, '{region_i_glue}{tid_txt}")\n',
                            'diagramfil <- glue("{tabell_namn}_{regionkod_i_glue}{tid_filnamn_txt}.png") %>% str_replace_all("__", "_")\n\n',
                            'if ("variabel" %in% names({tabell_namn}_df)) {{\n',
                            '   if (length(unique({tabell_namn}_df$variabel)) > 6) chart_df <- {tabell_namn}_df %>% filter(variabel == unique({tabell_namn}_df$variabel)[1]) else chart_df <- {tabell_namn}_df\n',
                            '}} else chart_df <- {tabell_namn}_df\n\n',
                            'gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,\n',
                            '\t\t\t skickad_x_var = "', tid_varnamn, '",\n',
                            '\t\t\t skickad_y_var = if ("varde" %in% names(chart_df)) "varde" else "', y_var_txt, '",\n',
                            '\t\t\t skickad_x_grupp = if ("variabel" %in% names(chart_df) & length(unique(chart_df$variabel)) > 1) "variabel" else NA,\n',
                            '\t\t\t x_axis_sort_value = FALSE,\n',
                            '\t\t\t diagram_titel = diagramtitel,\n',
                            '\t\t\t diagram_capt = diagram_capt,\n',
                            '\t\t\t stodlinjer_avrunda_fem = TRUE,\n',
                            '\t\t\t filnamn_diagram = diagramfil,\n',
                            '\t\t\t dataetiketter = visa_dataetiketter,\n',
                            '\t\t\t manual_y_axis_title = "",\n',
                            '\t\t\t manual_x_axis_text_vjust = 1,\n',
                            '\t\t\t manual_x_axis_text_hjust = 1,\n',
                            '\t\t\t manual_color = if ("variabel" %in% names(chart_df) & length(unique(chart_df$variabel)) > 1) diagramfarger("rus_sex") else diagramfarger("rus_sex")[1],\n',
                            '\t\t\t output_mapp = output_mapp,\n',
                            '\t\t\t diagram_facet = FALSE,\n',
                            '\t\t\t facet_grp = NA,\n',
                            '\t\t\t facet_scale = "free",\n',
                            ')\n\n',
                            'gg_list <- c(gg_list, list(gg_obj))\n',
                            'names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")\n\n')
    
    if (skapa_diagram_i_testfil) testfil_skript <- paste0(testfil_skript, testfil_diagram)
    
    writeLines(testfil_skript, paste0(temp_dir, filnamn_testfil))        
    file.edit(paste0(temp_dir, filnamn_testfil))
    
  } # slut if-sats om man vill skapa en testfil
  
  # returnera sökväg till den skapade filen
  return(paste0(output_mapp, "hamta_", filnamn_suffix, ".R"))
  
} 

kontrollera_pxweb_url <- function(url_scb_lista) {
  # Kontrollera att url:en är en giltig pxweb-url - om det är en webb-url från SCB:s öppna statstikdatabas på webben 
  # så konverterar vi den till en API-url, annars returnerar vi den som den är
  slut_retur_url <- map_chr(url_scb_lista, ~ {
    if (str_detect(.x, "https://www.statistikdatabasen.scb.se/")) {
      
      start_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/"
      # här extraherar vi den del av url:en som är unik för varje tabell och som ska byggas ihop med start_url:en
      retur_url <- .x %>% 
        str_remove("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START") %>%
        str_split("__") %>% unlist() %>% .[. != ""] %>% 
        str_split("/") %>% unlist() %>% .[. != ""] %>% 
        str_c(., collapse = "/") %>% 
        str_c(start_url, .) #%>% str_sub(., 1, nchar(.)-1)
      
      return(retur_url)
    } else if (str_detect(.x, "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb")) {
      
      api_url <- .x %>%
        str_replace("pxweb", "api/v1")                           # byt ut 
      #str_replace("https://", "http://")
      pos_revstart <- str_locate(api_url, "/sv/")[2]+1           # hitta slutet på start-delen av url:en
      start_url <- str_sub(api_url, 1, pos_revstart-2)           # ta ut start-url:en, dvs. den del som är likadan för alla url:er i Folkhälsomyndighetens tabeller
      rev_delar <- str_sub(api_url, pos_revstart) %>% str_split("/") %>% unlist() %>% .[. != ""]         # dela upp den del av url:en som vi ska revidera
      rev_nya <- rev_delar[2] %>% str_remove(rev_delar[1]) %>% str_split("__") %>% unlist() %>% .[. != ""] %>% paste0(collapse = "/")        # här skapar vi mellandelen i den reviderade delen av url:en
      retur_url <- paste(start_url, rev_delar[1], rev_nya, rev_delar[3], sep = "/")             # hela den reviderade url:en
      
      return(retur_url)
    } else if (str_detect(.x, "statistik.tillvaxtanalys.se")) {
      
      retur_url <- .x %>% 
        str_replace("statistik.tillvaxtanalys.se", "statistik.tillvaxtanalys.se:443") %>%
        str_replace("pxweb/sv", "api/v1/sv") %>%
        str_remove("Tillv%C3%A4xtanalys%20statistikdatabas__")  
      
      return(retur_url)
      
    } else if (str_detect(.x, "statistik.sjv.se")) {
      
      retur_url <- .x %>% 
        str_replace("/pxweb/", "/api/v1/") %>%
        str_remove("Jordbruksverkets%20statistikdatabas__") %>% 
        str_replace("__", "/")
      
      return(retur_url)
      
    } else {
      return(.x)
    }
  })
  return(slut_retur_url)
}

scb_tabellid_extrahera_fran_url <- function(url) {
  html <- httr::content(httr::GET(url), "text", encoding = "UTF-8")
  
  id <- str_extract(html, "TAB\\d+")
  unique(id)
}

extrahera_unika_varden_flera_scb_tabeller <- function(px_meta) {
  # En funktion för att skapa en tibble från 'values' och 'valueTexts'
  create_value_pairs <- function(variable) {
    tibble(values = variable$values, valueTexts = variable$valueTexts)
  }
  
  # Slå samman alla variabler från alla tabeller
  all_variables <- lapply(px_meta, function(px) px$variables) %>% purrr::flatten()
  
  # Skapa en tom lista för unika variabler
  unique_vars <- list()
  
  # Iterera över alla variabler och kombinera unika värden och valueTexts
  for (var in all_variables) {
    existing_index <- which(map_chr(unique_vars, "code") == var$code)
    
    if (length(existing_index) > 0) {
      # Kombinera värden om variabeln redan finns
      existing_var <- unique_vars[[existing_index]]
      existing_pairs <- create_value_pairs(existing_var)
      new_pairs <- create_value_pairs(var)
      
      # Kombinera värdeparen och ta bort dubbletter
      combined_pairs <- distinct(bind_rows(existing_pairs, new_pairs))
      
      # Uppdatera den befintliga variabeln med de kombinerade paren
      unique_vars[[existing_index]] <- list(
        code = var$code,
        text = var$text,
        elimination = var$elimination,
        values = combined_pairs$values,
        valueTexts = combined_pairs$valueTexts
      )
    } else {
      # Om variabeln inte finns, lägg till den
      unique_vars <- append(unique_vars, list(var))
    }
  }
  
  return(unique_vars)
}

sortera_px_variabler <- function(lista, sorterings_vars = c("Tid"), sortera_pa_kod = TRUE) {
  
  # denna funktion används till px-web-objekt, närmare bestämt en lista man får när man skriver px_meta <- pxweb_get(url) och sedan 
  # extrahera med lista <- px_meta$variables. 
  
  # Funktionen används för att sortera värdena för de variabler som anges i sorterings_vars
  # sorterings_vars är en vektor med variabel-koder eller namn som sorteras utifrån variabelkoden om sortera_pa_kod är TRUE
  # annars på variabelns klartextvärden (tex. "20" är kod och "Dalarnas län" är klartext för variabeln "region")
  
  # Använd 'map' för att iterera över 'lista' och tillämpa sorteringsfunktionen för varje element
  retur_lista <- map(lista, function(list_element) {
    if (tolower(list_element$code) %in% tolower(sorterings_vars) | tolower(list_element$text) %in% tolower(sorterings_vars)) {
      # Sortera 'values' och 'valueTexts' baserat på 'values' eller 'valueTexts' beroende på sortera_pa_kod är TRUE eller FALSE
      if (sortera_pa_kod) order_index <- order(list_element$values) else order_index <- order(list_element$valueTexts)
      list_element$values <- list_element$values[order_index]
      list_element$valueTexts <- list_element$valueTexts[order_index]
    }
    # Returnera det modifierade eller ursprungliga listelementet
    list_element
  })
  
  return(retur_lista)
}

overlappande_varden_pxweb_hantera <- function(px_meta_lista, url_scb, var_kod = "Tid") {
  
  # returnerar en lista med URL:er och värden för de tabeller som har överlappande värden
  # används primärt för att hantera scb-tabeller med överlappande år, dvs. för att i ett nästa
  # steg kunna ta bort de år i äldre tabeller som finns i nyare tabeller
  #
  # px_meta_list innehåller en lista med flera pxweb-tabeller, 
  # url_scb innehåller en lista med URL:er till pxweb-tabeller, 
  # var_code är variabelkoden för den variabel vi vill kontroller. 
  # Funktionen returnerar en lista med URL:er och de överlappande åren.
  # Det behöver inte nödvändigtvis vara år, utan det kan vara andra variabler man gör detta för.
  
  # En funktion för att extrahera alla värden från en specifik variabel
  extrahera_varden <- function(variabler) {
    tid_variabel <- variabler[map_chr(variabler, "code") == var_kod][[1]]
    return(tid_variabel$values)
  }
  
  # Sortera tabellerna efter högsta värde för den variabel vi vill kontrollera
  sorted_indices <- order(map_chr(px_meta_lista, ~ max(extrahera_varden(.x$variables))), decreasing = TRUE)
  px_meta_lista <- px_meta_lista[sorted_indices]
  url_scb <- url_scb[sorted_indices]
  
  # En lista för att lagra resultat (URL och överlappande värden)
  overlappande_url_varden <- list()
  
  # En vektor som ska lagra giltiga värden för varje tabell
  giltiga_varden_lista <- list()
  
  # Iterera över tabeller och kontrollera överlappande värden
  map2(px_meta_lista, seq_along(px_meta_lista), function(px_meta, i) {
    aktuella_varden <- extrahera_varden(px_meta$variables)
    
    # Kontrollera tidigare värden för att hitta överlapp
    if (i > 1) {
      tidigare_varden <- unlist(giltiga_varden_lista[1:(i - 1)])
      overlappande_varden <- aktuella_varden[aktuella_varden %in% tidigare_varden]
      
      # Om det finns överlappande värden, lagra URL och de överlappande värdena
      if (length(overlappande_varden) > 0) {
        overlappande_url_varden <<- append(overlappande_url_varden, list(
          list(url = url_scb[i], overlappande_varden = overlappande_varden)
        ))
        
        # Ta bort de överlappande värdena från den aktuella tabellen
        aktuella_varden <- aktuella_varden[!aktuella_varden %in% overlappande_varden]
      }
    }
    
    # Lagra de giltiga värdena för den aktuella tabellen
    giltiga_varden_lista[[i]] <<- aktuella_varden
    
    
  })
  
  # Om inga överlappande värden finns, returnera NULL
  if (length(overlappande_url_varden) == 0) {
    return(NULL)
  }
  
  return(overlappande_url_varden)
}

# ======================== skapa demo-diagrambild =====================

demo_diagrambild_skapa <- function(
    diagramskript_filnamn,          # det räcker med filnamn om skriptet ligger i det lokala github-repot för diagramfiler, annars anges mapp i parametern mapp_diagramskript_ej_github
    parameter_lista = list(),       # man kan skicka med parametrar men ska inte skicka med output_mapp, den tilldelas i skriptet nedan för att kunna tas bort
    parameter_output_mapp = NA,     # behövs bara om parametern heter annat än output_mapp eller utmapp
    github_kor_commit_och_push = TRUE,         # kan sättas till FALSE om man vill köra igenom flera skript och commita och pusha efter att man reviderat alla skript
    github_mapp_lokal = "c:/gh/",
    github_diag_demo_repo = "utskrivna_diagram",
    revidera_diagramskript = TRUE,        # skickas med för att revidera själva diagramskriptet med en demo-parameter samt kod i själva skriptet för att visa demo-diagram
    github_diagram_repo = "diagram",    # om man vill skicka ändringar av diagramskriptet till github också
    mapp_diagramskript_ej_github = NA
){
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R")
  
  # vi tar bort mapp från sökvägen för så funkar skriptet nedan
  diagramskript_filnamn <- basename(diagramskript_filnamn)
  
  # Skapa sökväg till diagramskriptet, mapp_diagramskript_ej_github används när man inte ska revidera ett diagramskript
  # som ligger i ett github-repo utan bara lokalt
  if (is.na(mapp_diagramskript_ej_github)){
    diagram_sokvag <- paste0(github_mapp_lokal, github_diagram_repo, "/", diagramskript_filnamn)
  } else {
    if (str_sub(mapp_diagramskript_ej_github, 1, nchar(mapp_diagramskript_ej_github)) != "/") mapp_diagramskript_ej_github <- paste0(mapp_diagramskript_ej_github, "/")
    diagram_sokvag <- paste0(mapp_diagramskript_ej_github, diagramskript_filnamn)
  }
  
  dia_funktion <- hitta_funktioner_i_fil_ej_inuti_andra_funktioner(diagram_sokvag)
  dia_funktion <- dia_funktion[str_sub(dia_funktion,1,4) == "diag"]     # bara första funktionen i skriptet
  
  # Skapa en temporär mapp
  temp_mapp <- file.path(tempdir(), "temp_mapp")
  skapa_mapp_om_den_inte_finns(temp_mapp)
  
  # Sätt att mappen tas bort när skriptet avslutas
  on.exit(unlink(temp_mapp, recursive = TRUE))
  
  source(diagram_sokvag)
  parametrar <- formals(get(dia_funktion))
  
  # Om inte output_mapp-parameternamnet är anget så testas output_mapp eller utmapp
  if (is.na(parameter_output_mapp)){
    if ("output_mapp" %in% names(parametrar)){
      utmappnamn <- "output_mapp"
    } else if ("utmapp" %in% names(parametrar)){
      utmappnamn <- "utmapp"
    } else if ("output_mapp_figur" %in% names(parametrar)){
      utmappnamn <- "output_mapp_figur"
    } else if ("outputmapp_figur" %in% names(parametrar)){
      utmappnamn <- "outputmapp_figur"
    } else {
      stop("Parameternamnet för output_mapp kan inte hittas. Ange det med parametern 'parameter_output_mapp' i denna funktion.")
    }
  } else {
    utmappnamn <- parameter_output_mapp
  }
  
  # Lägg till output_mapp-parametern i parameter_lista, finns den redan skrivs den över
  parameter_lista[[utmappnamn]] <- if (str_sub(temp_mapp, nchar(temp_mapp)) != "/" | str_sub(temp_mapp, nchar(temp_mapp)) != "\\") temp_mapp <- paste0(temp_mapp, "/") else temp_mapp
  
  # Sätt parameter för att skriva diagramfil(er) till TRUE
  match_index <- match(names(parametrar), 
                       c("skriv_diagrambild", "skriv_diagramfil", "skriv_diagram",
                         "spara_figur", "skriv_till_diagramfil", "skapa_fil"), 
                       nomatch = 0)
  parametrar[match_index > 0] <- TRUE          # sätter de parametrar som matchar med vektorn ovan till TRUE (bör vara bara en)
  
  # Uppdatera parametrar med värden från parameter_lista där namnen överensstämmer
  parameter_lista <- imap(parametrar, ~ if(.y %in% names(parameter_lista)) parameter_lista[[.y]] else .x) 
  
  # Skapa diagrammet och skriv ut bildfiler till den temporära mappen vi skapat ovan
  resultat <- do.call(dia_funktion, parameter_lista)
  rm(resultat)             # ta bort ggplot_objektet, vi behöver inte det
  
  # lägg alla skapade filer i en vektor
  demofil_vekt <- list.files(temp_mapp, full.names = TRUE) 
  
  # kopiera alla filer till det lokala github-repot för demo-diagram
  walk(demofil_vekt, ~ file.copy(.x, paste0(github_mapp_lokal, github_diag_demo_repo), overwrite = TRUE))
  
  demofiler_filnamn <- basename(demofil_vekt)
  
  # commita och pusha till github-repot för demo-diagram
  if (github_kor_commit_och_push) {
    commit_meddelande <- paste0("Lagt till följande demo-diagrambilder: ", list_komma_och(demofiler_filnamn))
    github_commit_push(repo = github_diag_demo_repo, commit_txt = commit_meddelande)
  }
  
  # Om diagramskriptet är angivet så revideras det med kod för att titta på 
  # demodiagram för att sedan commita diagramskriptet till github-repot för diagram
  if (revidera_diagramskript){
    reviderat <- FALSE
    
    # Läs in diagramskriptet
    diagram_skript <- readLines(diagram_sokvag)
    
    # kontrollera om det redan finns en demo-parameter
    index_parameter <- str_which(diagram_skript, "demo = ")
    index_demokod <- str_which(diagram_skript, "if \\(demo\\)\\{")
    
    # om det inte gör det läggs det till
    if (length(index_parameter) == 0){
      # först tar vi reda på var i skriptet funktionen startar (med function), 
      # och var parameterlistan slutar (med första } efter function)
      index_parameter_slut <- str_which(diagram_skript, "\\{") %>% min()
      
      mellanslag_txt <- str_extract(diagram_skript[index_parameter_slut-2], "^\\s*")
      ny_rad <- paste0(mellanslag_txt, "demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat")
      # rad att ha efter den nya raden
      ny_rad_tva <- paste0(mellanslag_txt, ") {")                
      
      slut_rad <- diagram_skript[index_parameter_slut] %>% 
        str_remove("\\)") %>% 
        str_remove("\\{") %>% 
        str_trim()
      
      slut_index <- if (nchar(slut_rad) > 0) 0 else 1
      
      # om det finns en slutparantes eller start-måsvinge på raden innan så tar vi bort det (för att lägga till efter demo = FALSE)
      ny_rad_innan <- diagram_skript[index_parameter_slut-slut_index] %>% 
        str_remove("\\)") %>% 
        str_remove("\\{")
      
      # en funktion som kontrollerar om det finns kommatecken i en sträng som ligger utanför
      # citationstecken (enkla eller dubbla)
      finns_kommatecken_utanfor_citat <- function(text) {
        ren_strang <- str_replace_all(text, '["\'].*?,.*?["\']', '')
        finns_kommatecken_utanfor_citat <- str_detect(ren_strang, ",")
        return(finns_kommatecken_utanfor_citat)
      }
      
      # om det inte finns ett kommatecken sist i textsträngen så lägger vi till det, men innan ett eventuellt första #
      if (!finns_kommatecken_utanfor_citat(ny_rad_innan)) {
        # sätt ett komma sist på raden innan demo = FALSE då vi lägger till demo som sista parameter, och då behövs ett komma sist på raden innan
        if (str_detect(ny_rad_innan, "#")) {                       # Kolla om det finns ett # i strängen
          # Sätt ett kommatecken före första # och före eventuella mellanslag innan #
          ny_rad_innan <- str_replace(ny_rad_innan, "(\\s*)#", ",\\1#")
        } else {
          # Om inget # finns, sätt ett kommatecken efter sista tecknet som inte är ett mellanslag
          ny_rad_innan <- str_replace(ny_rad_innan, "(\\S)\\s*$", "\\1,")
        }
      }
      
      # skriv en ny vektor med demo som parameter
      diagram_skript_ny <- c(diagram_skript[1:(index_parameter_slut-1-slut_index)], ny_rad_innan, ny_rad, ny_rad_tva, diagram_skript[(index_parameter_slut+1):length(diagram_skript)])
      reviderat <- TRUE
      if (length(index_demokod) > 0) index_demokod <- index_demokod + 1       # om det finns demokod redan så ökar vi elementet med 1 då vi lägger till en rad, men om den inte finns så låter vi den vara 0
    } # slut test om demo finns som parameter
    
    if (length(index_demokod) == 0){
      # hitta första raden efter att parametrarna definierats i funktionen    
      index_parameter_slut <- str_which(diagram_skript_ny, "\\{") %>% min()+1
      
      # Funktion som kontrollerar om ett element uppfyller kriterierna att hitta första raden efter parametrarna som inte bara består av mellanslag eller börjar med #
      hittat_ratt_rad <- function(x) {
        # Trimma inledande mellanslag och kontrollera om strängen inte börjar med "#"
        trimmed <- str_trim(x)
        trimmed != "" && !str_starts(trimmed, "#")
      }
      
      # Hitta första giltiga elementet från och med första rad efter parametrarna
      demokod_startrad <- index_parameter_slut - 1 + detect_index(diagram_skript_ny[index_parameter_slut:length(diagram_skript_ny)], hittat_ratt_rad)
      
      # fixa till den delen av demokoden som refererar till bildfilerna på github
      demokod_filnamn <- paste0('"', 'https://region-dalarna.github.io/utskrivna_diagram/', demofiler_filnamn, '"', ',')
      demokod_filnamn[1] <- paste0('c(', demokod_filnamn[1])
      demokod_filnamn[length(demokod_filnamn)] <- demokod_filnamn[length(demokod_filnamn)] %>% str_replace(',', ')')
      
      nya_demokodrader <- c(
        "# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen",
        "# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram",
        "if (demo){",
        "  demo_url <- ",
        demokod_filnamn,
        "  walk(demo_url, ~browseURL(.x))",
        '  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))',
        "  stop_tyst()",
        "}",
        "")
      
      # kolla om det finns en tomrad innan startraden för demokoden, annars lägga till det
      # först skapa tomrad om det inte finns innan demokod-startraden, annars NULL (dvs. ingen tomrad)
      tomrad <- if (str_trim(diagram_skript_ny[demokod_startrad-1]) != "") "" else NULL 
      
      # sätt ihop den nya demokoden med tomraden (eller ingen tomrad om det redan finns) och den gamla demokoden
      diagram_skript_ny <- c(diagram_skript_ny[1:(demokod_startrad-1)], tomrad, nya_demokodrader, diagram_skript_ny[demokod_startrad:length(diagram_skript_ny)])
      
      reviderat <- TRUE
      
    } # slut test om demo finns som kod
    
    # Skriv tillbaka diagramskriptet
    if (reviderat) {
      # skriv en nya versionen av skriptet tillbaka till samma fil som vi läste in i början
      writeLines(diagram_skript_ny, diagram_sokvag)
      
      if (github_kor_commit_och_push & is.na(mapp_diagramskript_ej_github)) {
        commit_meddelande <- paste0("Lagt till demo-kod i diagramskriptet ", diagramskript_filnamn)
        github_commit_push(repo = github_diagram_repo, commit_txt = commit_meddelande)
      }
    } # slut test om diagramskriptet är reviderat
  } # slut test om parametern revidera_diagramskript är TRUE
} # slut funktion


# Funktion som hittar alla yttre funktioner i en skriptfil
hitta_funktioner_i_fil_ej_inuti_andra_funktioner <- function(filnamn) {
  # Läser in skriptfilen som en vektor av rader
  if (str_detect(filnamn, "^https?://")) {
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = FALSE)
    con <- curl::curl(filnamn, handle = h)
    rader <- readLines(con)
    close(con)
  } else {
    rader <- readLines(filnamn)
  }
  
  # Identifierar alla rader som innehåller funktionsdefinitioner
  funktionsrader <- str_which(rader, "\\bfunction\\b")
  
  # Initialisera en vektor för att hålla index för yttre funktioner
  outer_functions <- c()
  bracket_balance <- 0
  
  # Loopar igenom funktionsraderna och identifierar yttre funktioner
  for (i in funktionsrader) {
    # Beräkna antalet { och } fram till den aktuella raden
    balance <- sum(str_count(rader[1:i], "\\{")) - sum(str_count(rader[1:i], "\\}"))
    
    # Om balans är 0, är detta en yttre funktion
    if (bracket_balance == 0) {
      outer_functions <- c(outer_functions, i)
    }
    
    # Uppdatera bracket_balance
    bracket_balance <- balance
  }
  
  # Extrahera namnen på yttre funktioner
  funktion_namn <- map_chr(outer_functions, ~ str_extract(rader[.x], "\\b\\w+\\b(?=\\s*<-\\s*function)")) 
  funktion_namn <- funktion_namn[!is.na(funktion_namn)]  
  
  # Returnera namnen på de yttre funktionerna
  return(funktion_namn)
}  


# ======================================== skapa och hantera Rmarkdown-rapporter samt Shiny-appar ======================================================


# ==== Hämta filer från Region-Dalarna/depot ==================================
#
# Delade tillgångar (stilfiler, mallar, typsnitt, loggor m.m.) hämtas via
# GitHub Contents API istället för raw.githubusercontent.com, eftersom rå-URL:er
# cachas hårt av Fastly (samma problem som löstes för source_utan_cache() i
# func_shinyappar.R). Contents API:et missar den cachen och ger alltid senaste
# committade innehållet.

#' Hämta en fil från depot-repot
#'
#' @param rel_sokvag Sökväg till filen inom depot-repot, t.ex.
#'   "regiondalarna_ruf.css" eller "mallar/webbsida_portal/index.qmd.tmpl".
#' @param target_path Om angiven: filen skrivs till denna sökväg på disk.
#'   Om NULL: filinnehållet returneras istället (som text eller raw, se as_text).
#' @param as_text TRUE för textfiler (.css, .qmd, .yml, .R ...), FALSE för
#'   binärfiler (bilder, typsnitt, ico).
#' @param repo Depot-repots namn. Default "depot".
#' @param org  GitHub-org. Default "Region-Dalarna".
#' @param branch Branch att hämta från. Default "main".
#'
#' @return Om target_path är NULL: filinnehållet (character om as_text = TRUE,
#'   annars raw vector). Om target_path anges: target_path osynligt (invisible),
#'   för att kunna kedjas.
depot_hamta_fran <- function(rel_sokvag,
                             target_path = NULL,
                             as_text     = TRUE,
                             repo        = "depot",
                             org         = "Region-Dalarna",
                             branch      = "main") {
  
  api_url <- sprintf("https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
                     org, repo, utils::URLencode(rel_sokvag), branch)
  
  resp <- httr::GET(
    api_url,
    httr::accept("application/vnd.github.raw+json"),  # ber om rått filinnehåll direkt
    httr::add_headers(`Cache-Control` = "no-cache")
  )
  
  if (httr::status_code(resp) == 404) {
    stop("Filen hittades inte i depot: ", rel_sokvag,
         " (repo: ", org, "/", repo, ", branch: ", branch, ")", call. = FALSE)
  }
  httr::stop_for_status(resp, task = paste("hämta", rel_sokvag, "från depot"))
  
  raw_innehall <- httr::content(resp, as = "raw")
  
  if (is.null(target_path)) {
    if (as_text) {
      return(rawToChar(raw_innehall))
    }
    return(raw_innehall)
  }
  
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  
  if (as_text) {
    writeLines(rawToChar(raw_innehall), target_path, useBytes = TRUE)
  } else {
    writeBin(raw_innehall, target_path)
  }
  
  invisible(target_path)
}


#' Lista och ladda ner alla filer i en mapp i depot-repot
#'
#' Motsvarar fonts/-hämtningen i Brottsappen, men generaliserad. Laddar INTE
#' ner undermappar rekursivt (depot har hittills bara platta mappar som
#' fonts/ och mallar/<namn>/) — utöka vid behov om det blir aktuellt.
#'
#' @param rel_mapp Mappens sökväg inom depot-repot, t.ex. "fonts" eller
#'   "mallar/webbsida_portal".
#' @param target_dir Lokal mapp filerna ska sparas i.
#' @param as_text TRUE om filerna i mappen är textfiler, FALSE om binära.
#'   Går inte att blanda i samma anrop — kör depot_hamta_fran() filvis om så behövs.
#' @param repo,org,branch Se depot_hamta_fran().
#'
#' @return Osynligt: character vector med namnen på nedladdade filer.
depot_hamta_mapp_fran <- function(rel_mapp,
                                  target_dir,
                                  as_text = FALSE,
                                  repo    = "depot",
                                  org     = "Region-Dalarna",
                                  branch  = "main") {
  
  api_url <- sprintf("https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
                     org, repo, utils::URLencode(rel_mapp), branch)
  
  resp <- httr::GET(api_url, httr::accept("application/vnd.github+json"))
  
  if (httr::status_code(resp) != 200) {
    warning("Kunde inte lista depot/", rel_mapp, "/ (status ",
            httr::status_code(resp), "). Inga filer hämtades.", call. = FALSE)
    return(invisible(character(0)))
  }
  
  innehall <- httr::content(resp, as = "parsed")
  filer    <- purrr::keep(innehall, ~ identical(.x$type, "file"))
  
  if (length(filer) == 0) {
    warning("Mappen depot/", rel_mapp, "/ är tom eller saknar filer.", call. = FALSE)
    return(invisible(character(0)))
  }
  
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  
  namn <- purrr::map_chr(filer, "name")
  purrr::walk(namn, function(filnamn) {
    depot_hamta_fran(
      rel_sokvag  = paste0(rel_mapp, "/", filnamn),
      target_path = file.path(target_dir, filnamn),
      as_text     = as_text,
      repo        = repo, org = org, branch = branch
    )
  })
  
  message("✅ Hämtade ", length(namn), " fil(er) från depot/", rel_mapp, "/.")
  invisible(namn)
}


#' Hämta en mallfil från depot och fyll i {{variabler}}
#'
#' Tunn wrapper runt depot_hamta_fran() specifikt för .tmpl-filer som ska
#' textsubstitueras innan de skrivs till ett nytt repo, t.ex. vid
#' webbsida_med_portal_skapa_med_github_repo().
#'
#' @param mallfil Filnamn inom mallmappen, t.ex. "_quarto.yml.tmpl".
#' @param malfil Lokal sökväg den ifyllda filen ska skrivas till.
#' @param variabler Named list med värden att fylla i, t.ex.
#'   list(titel = "Min portal", runner_label = "rapport").
#' @param mallmapp Sökväg till mallmappen inom depot-repot.
#' @param repo,org,branch Se depot_hamta_fran().
depot_skriv_mall_fran <- function(mallfil, malfil, variabler,
                                  mallmapp = "mallar/webbsida_portal",
                                  repo     = "depot",
                                  org      = "Region-Dalarna",
                                  branch   = "main") {
  
  raw_mall <- depot_hamta_fran(
    rel_sokvag = paste0(mallmapp, "/", mallfil),
    as_text    = TRUE,
    repo = repo, org = org, branch = branch
  )
  
  ifylld <- glue::glue(raw_mall, .open = "{{", .close = "}}", .envir = list2env(variabler))
  
  dir.create(dirname(malfil), recursive = TRUE, showWarnings = FALSE)
  writeLines(ifylld, malfil, useBytes = TRUE)
  
  invisible(malfil)
}

webbsida_med_portal_skapa_med_github_repo <- function(github_repo,
                                                      server = c("publik", "intern"),
                                                      titel = github_repo,
                                                      beskrivning = titel,
                                                      privat_repo = TRUE,
                                                      github_org = "Region-Dalarna",
                                                      lokal_root = "c:/gh") {
  
  server <- match.arg(server)
  
  malkonfig <- switch(server,
                      publik = list(runner_label = "rapport"),
                      intern = list(runner_label = "rapport-intern")
  )
  
  lokal_path <- file.path(lokal_root, github_repo)
  if (dir.exists(lokal_path)) {
    stop("Mappen finns redan: ", lokal_path, ". Avbryter för säkerhets skull.")
  }
  
  # 1. Skapa repo på GitHub
  gh::gh("POST /orgs/{org}/repos",
         org = github_org,
         name = github_repo,
         private = privat_repo,
         auto_init = FALSE)
  
  # 2. Klona ner lokalt
  repo_url <- sprintf("https://github.com/%s/%s.git", github_org, github_repo)
  gert::git_clone(repo_url, path = lokal_path)
  
  # Säkerställ att branchen heter "main" oavsett lokal git-konfiguration
  current_branch <- gert::git_branch(repo = lokal_path)
  if (current_branch != "main") {
    gert::git_branch_move(current_branch, "main", repo = lokal_path)
  }
  
  # 3. Hämta och skriv ut mallfilerna från depot, ifyllda med repo-specifika värden
  dir.create(file.path(lokal_path, ".github", "workflows"), recursive = TRUE)
  
  variabler <- list(titel        = titel,
                    beskrivning  = beskrivning,
                    runner_label = malkonfig$runner_label,
                    github_repo  = github_repo)
  
  depot_skriv_mall_fran("_quarto.yml.tmpl", file.path(lokal_path, "_quarto.yml"), variabler)
  depot_skriv_mall_fran("index.qmd.tmpl",   file.path(lokal_path, "index.qmd"),   variabler)
  depot_skriv_mall_fran("deploy.yml.tmpl",  file.path(lokal_path, ".github/workflows/deploy.yml"), variabler)
  depot_skriv_mall_fran("README.md.tmpl",   file.path(lokal_path, "README.md"),  variabler)
  
  depot_hamta_fran("regiondalarna_ruf.css",
                   file.path(lokal_path, "regiondalarna_ruf.css"),
                   as_text = TRUE)
  
  depot_hamta_fran("mallar/webbsida_portal/portal_overrides.css",
                   file.path(lokal_path, "portal_overrides.css"),
                   as_text = TRUE)
  
  writeLines(c(".quarto/", "_site/", "*.html", "*_files/"), file.path(lokal_path, ".gitignore"))
  
  # 4. Commit + push grundstrukturen till main
  gert::git_add(".", repo = lokal_path)
  gert::git_commit("Initiera webbsida/portal-struktur", repo = lokal_path)
  gert::git_push(remote = "origin",
                 refspec = "refs/heads/main:refs/heads/main",
                 repo = lokal_path)
  
  # 5. Sätt defaultbranch till "main" på GitHub
  gh::gh("PATCH /repos/{owner}/{repo}",
         owner = github_org,
         repo  = github_repo,
         default_branch = "main")
  
  url_server <- switch(server,
                       publik = "https://samhallsanalys.regiondalarna.se",
                       intern = "https://samhallsanalys.ltdalarna.se"
  )
  
  message("Repo skapat: https://github.com/", github_org, "/", github_repo)
  message("Publiceras vid push till main till: ", url_server, "/", github_repo, "/")
  message("(kräver att runnern '", malkonfig$runner_label, "' är aktiv på rätt server)")
  invisible(lokal_path)
}


skapa_webbrapport_github <- function(githubmapp_lokalt,                 # sökväg till den mapp där du har alla github-repos (ska INTE innehålla själva repositoryt), tex c:/github_repos/
                                     github_repo,                       # namn på själva github-repot, döper mappen och github-repot. Mappen skapas om den inte finns
                                     github_org = "Region-Dalarna",     # ändra till NULL om man vill lägga repo:t i sin privata github
                                     rapport_titel,                     # titel på rapporten i RMarkdown
                                     rapport_undertitel = NA,
                                     anvand_publicera_rapporter = TRUE, # TRUE så publiceras rapporten med Github Pages via repositoryt publicera_rapporter
                                     behorighet_team = "samhallsanalys" # namn på team som ska ges behörighet, NULL om man inte vill ge något team behörighet, teamet måste finnas i organisationen om detta ska fungera
) {         # om man vill ha en undertitel så lägger man in den här
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8")
  
  githubmapp_lokalt <- githubmapp_lokalt %>% str_replace_all(fixed("\\"), "/")
  if (str_sub(githubmapp_lokalt, nchar(githubmapp_lokalt), nchar(githubmapp_lokalt)) != "/") githubmapp_lokalt <- paste0(githubmapp_lokalt, "/")
  
  
  sokvag_proj <- paste0(githubmapp_lokalt, github_repo)
  if (str_sub(sokvag_proj, nchar(sokvag_proj), nchar(sokvag_proj)) != "/") sokvag_proj <- paste0(sokvag_proj, "/")
  
  skapa_mapp_om_den_inte_finns(sokvag_proj)
  
  # # Här skriver vi själva .Rproj-filen
  # str_proj_fil <- paste0(
  #   "Version: 1.0\n\n",
  # 
  #   "RestoreWorkspace: Default\n",
  #   "SaveWorkspace: Default\n",
  #   "AlwaysSaveHistory: Default\n\n",
  # 
  #   "EnableCodeIndexing: Yes\n",
  #   "UseSpacesForTab: Yes\n",
  #   "NumSpacesForTab: 2\n",
  #   "Encoding: UTF-8\n\n",
  # 
  #   "RnwWeave: Sweave\n",
  #   "LaTeX: pdfLaTeX")
  
  # # skriv .Rproj-fil till hårddisken
  # writeLines(str_proj_fil, paste0(sokvag_proj, github_repo, ".Rproj"))
  
  # # skapa r-projekt
  # create_project(sokvag_proj, rstudio = rstudioapi::isAvailable(), open = rlang::is_interactive())
  
  gitprojekt_sokvag <- if (str_sub(sokvag_proj, -1, -1) == "/")  str_sub(sokvag_proj, 1, -2)
  
  usethis::create_project(gitprojekt_sokvag, open = FALSE)
  
  
  # skapa övriga mappar vi brukar ha
  skapa_mapp_om_den_inte_finns(glue("{sokvag_proj}figurer"))
  #skapa_mapp_om_den_inte_finns(glue("{sokvag_proj}docs"))
  skapa_mapp_om_den_inte_finns(glue("{sokvag_proj}skript"))
  
  undertitel_html <- if (!is.na(rapport_undertitel)) glue('<div class="bottom_text">{rapport_undertitel}</div>') else ""
  
  # Här skapar vi filen hero_image.html
  hero_image_html <- glue('
  <div class="hero-image"> 
    <head>
        <link rel="preconnect" href="https://fonts.googleapis.com">
        <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
        <link href="https://fonts.googleapis.com/css2?family=Poppins:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">
    </head>
    <a class="logo" href="https://www.regiondalarna.se/" target="_blank">
        <img class="header-logo" src="logo_liggande_fri_vit.png">
    </a>
    <a class="till_startsida" href="https://www.regiondalarna.se/regionalutveckling/dalastrategin/" target="_blank">
        <img class="header-logo_right" src="dalastrategin_hjul.png"></a>
    <div class="image-text">
        <div class="top-text">{rapport_titel}</div>
        {undertitel_html}
    </div>
</div>
')
  
  
  # Nu kan du använda writelines() för att skriva den här variabeln till en fil
  writeLines(hero_image_html, paste0(sokvag_proj, "hero_image.html"))
  
  # Nu laddar vi ner lite filer som vi behöver för att skapa rapporterna
  list_filer <- list(
    dalastrategin_jpg = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/Dalastrategin.jpg",
    dalastrategin_hjul_png = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/dalastrategin_hjul.png",
    logga_korrekt = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/logga_korrekt.png",
    logo_liggande_fri_vit = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/logo_liggande_fri_vit.png",
    logo_liggande_platta_farg = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/logo_liggande_platta_farg.png",
    logo_liggande_platta_svart = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/logo_liggande_platta_svart.png",
    logo_liggande_fri_svart = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/rd_logo_liggande_fri_svart.png",
    styles_hero_css = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/styles_hero.css",
    favicon_html = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/favicon.html",
    favicon_ico = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/favicon.ico"
  )
  
  filnamn <- map_chr(list_filer, ~ str_extract(.x, "[^/]+$"))
  
  walk(list_filer, ~ {
    filnamn <- str_extract(.x, "[^/]+$")
    download.file(.x, paste0(sokvag_proj, filnamn), mode = "wb")
  })
  
  # nu laddar vi ner filer till mappen "skript"
  list_skript_filer <- list(
    hamta_data_webbrapport = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/1_hamta_data.R",
    knitta_rapport = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/2_knitta_rapport.R",
    kopiera_till_publicera_rapporter = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/3_kopiera_till_publicera_rapporter_docs_for_publicering_pa_webben.R",
    push_till_github = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/4_push_av_hela_repo_till_github.R"
  )
  
  sokvag_skript <- glue("{sokvag_proj}skript/")
  walk(list_skript_filer, ~ {
    filnamn <- str_extract(.x, "[^/]+$")
    download.file(.x, paste0(sokvag_skript, filnamn), mode = "wb")
  })
  
  # vi skapar nu själva .Rmd-filen
  
  # vi börjar med headern i webbrapporten
  rmd_header <- glue('
---
title: {rapport_titel}
author: ""
date: ""
output: 
  html_document:
  self_contained: true
  includes:
      in_header: 
      - favicon.html
      - hero_image.html
    toc: yes
    toc_float: yes
    toc_depth: 6
    css: "styles_hero.css"
    number_sections: true
---
')
  
  # därefter kör vi den del där vi laddar paket etc.
  rmd_init <- glue("
```{{r setup, include=FALSE}}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

# Nödvändiga paket
if (!require('pacman')) install.packages('pacman')
p_load(tidyverse,
       here,
       git2r,
       keyring)

# Funktioner som behövs (hämtas från Git-Hub)
source('https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R')
source('https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R')

# För att information från Tidyverse inte skall visas
options(dplyr.summarise.inform = FALSE)

# Här läggs figurerna
outputmapp = here('Diagram','/')

# Om man vill uppdatera den externa hemsidan sätts denna variabel till TRUE
uppdatera_hemsida = FALSE

# Om man vill uppdatera data sätts denna variabel till TRUE
#uppdatera_data = FALSE

# Om man vill spara figurer sätts denna variabel till TRUE
spara_figur = FALSE

# if(skapa_lista == TRUE){{
# source(here('master_kvinnor_man.R'), encoding = 'utf-8', echo = FALSE)
# lista_figurer=c(lista_figurer,hamta_figurer(skapa_ppt=FALSE))
# }}
# 
# if(uppdatera_data == TRUE){{
# source(here('skript','1_hamta_data.R'), encoding='UTF-8')
# }}

# Läser in data (ett exempel på hur det kan se ut - byt ut detta)
utbildning_df <- read.xlsx('G:/skript/projekt/data/kvinnor_man/utbildningsniva.xlsx')
utbildning_85_df <- read.xlsx('G:/skript/projekt/data/kvinnor_man/utbildningsniva_85.xlsx')
```
")
  
  # därefter kommer inledningen av rapporten
  rmd_text <- glue('
<p style = "font-size:12px">
<i>Rapporten är skapad av Samhällsanalys, Region Dalarna<br>
Senast uppdaterad: `r Sys.Date()`</i>
</p>

# Sammanfattning {{-}}

Här ska vi ha en sammanfattning av rapporten. Det ska vara en punktlista med de viktigaste budskapen och inte ett referat av varje avsnitt.
<br>

<ul>

<li>Första punkten. </li>

<li>Andra punkten.</li>
    
<li>Tredje punkten.</li>

<li>Fjärde punkten.</li>

# Introduktion {{-}}

Här kommer en introduktion till rapporten. Det ska vara en kort text som förklarar vad rapporten handlar om och vad läsaren kan förvänta sig att hitta i rapporten.

# Första avsnittet
Här kommer första avsnittet. 

## Första avsnittets första underrubrik
Här är det första avsnittets första underrubrik

## Första avsnittets andra underrubrik
Här är det första avsnittets andra underrubrik

```{{r, echo=FALSE}}
diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"

  # Skapar en faktorvariabel som styr vilken ordning som utbildningsgrupper visas
  utbildning_df$utbildningsnivå <- factor(utbildning_df$utbildningsnivå, levels = c("eftergymnasial utbildning, 3 år eller mer","eftergymnasial utbildning, mindre än 3 år",
                                                                                    "gymnasial utbildning, 3 år","gymnasial utbildning, högst 2 år","förgymnasial utbildning, 9 (10) år",
                                                                                    "förgymnasial utbildning kortare än 9 år")[6:1])

# Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning
utbildning_85_utskrift<-utbildning_85_df %>%
  filter(utb_niva!="Uppgift saknas") %>%
    group_by(år,region,kön,utb_niva) %>%
      summarize(antal=sum(Befolkning)) %>%
        mutate(andel=(antal/sum(antal))*100)

  diagramtitel <-paste0("Utbildningsnivå (25-64 år) i Dalarna ", unique(utbildning_df$år))
  diagramfilnamn <- paste0("utbildningsniva_Dalarnas län.png")

  utbildning_fig <- SkapaStapelDiagram(skickad_df <- utbildning_df %>%
                                         filter(region == "Dalarna", utbildningsnivå != "uppgift om utbildningsnivå saknas"), 
                                       skickad_x_var = "utbildningsnivå", 
                                       skickad_y_var = "andel", 
                                       skickad_x_grupp = "kön",
                                       x_axis_lutning = 0,
                                       diagram_liggande = TRUE,
                                       x_axis_sort_value=FALSE,
                                       manual_color = diagramfarger("kon"),
                                       manual_y_axis_title = "procent",
                                       diagram_titel = diagramtitel,
                                       diagram_capt =  diagram_capt,
                                       stodlinjer_avrunda_fem = TRUE,
                                       berakna_index = FALSE,
                                       output_mapp = outputmapp,
                                       filnamn_diagram = diagramfilnamn,
                                       skriv_till_diagramfil = spara_figur)

  utb_85_fig <- SkapaStapelDiagram(skickad_df =utbildning_85_utskrift %>%
                              filter(år%in%c("1985","1990","1995","2000","2005","2010","2015",max(utbildning_85_utskrift$år))) %>%
                                filter(utb_niva=="Eftergymnasial utbildning, 3 år eller mer"),
                             skickad_x_var = "år",
                             skickad_y_var = "andel",
                             skickad_x_grupp = "kön",
                             manual_color = diagramfarger("kon"),
                             diagram_titel = diagramtitel,
                             diagram_capt =  diagram_capt,
                             x_axis_lutning = 0,
                             stodlinjer_avrunda_fem = TRUE,
                             manual_y_axis_title="procent",
                             output_mapp = outputmapp,
                             filnamn_diagram = diagramfilnamn,
                             skriv_till_diagramfil = spara_figur)

```

<br>
```{{r, echo = FALSE, message = FALSE, warning = FALSE, fig.height=5, fig.width=8, fig.align="center"}}
utbildning_fig
```

Lite mer text. Och sedan ett till diagram.
<br>
```{{r, echo = FALSE, message = FALSE, warning = FALSE, fig.height=5, fig.width=8, fig.align="center"}}
utb_85_fig
```

## Här kommer en underrubrik till
Lite text

# Och så en huvudrubrik
Här ser vi hur man gör en länk: På nationell nivå har SCB jämfört den standardvägda lönen, dvs. lönen när hänsyn tas 
till bland annat ålder och utbildningsnivå, för att synliggöra om det kan finnas en diskrimineringsaspekt i lönenivån. 
Slutsatsen blev att kvinnors standardvägda lön är ungefär 95 procent av männens standardvägda lön, det vill säga det
finns en oförklarad löneskillnad mellan kvinnor och män [(SCB)](<https://www.scb.se/hitta-statistik/statistik-efter-amne/arbetsmarknad/loner-och-arbetskostnader/lonestrukturstatistik-hela-ekonomin/pong/tabell-och-diagram/kvinnors-lon-i-procent-av-mans-lon-efter-sektor>){target="_blank"}.

## Underrubrik igen
Lite text

# Huvudrubrik
Lite text

## Underrubrik igen
Lite text

## Underrubrik igen
Lite text

')
  
  rmd_slut_skript <- glue('
```{{r, include = FALSE}}

if(uppdatera_hemsida==TRUE){{
 # kopiera html-filen till
 file.copy(from = "{github_repo}.html", to = "docs/index.html", overwrite = TRUE)
 file.copy(from = "{github_repo}.Rmd", to = "docs/index.Rmd", overwrite = TRUE)

 #ska vi ta bort html-filen i projektmappen eller ha dubbelt, både där och i docs-mappen?
#file.remove("{github_repo}.html")

  # ============================================= pusha upp till github ===================================================

  # För att detta ska fungera behöver man använda följande keyring-konton:
  #         1. Service: "git2r", anv: <github användarnamn>, pwd: <mailadress kopplat till github-kontot>
  #         2. Service: "github", anv: <github användarnamn>, pwd: <lösenord på github>
  #         3. Service: "github_token", anv: <github användarnamn>, pwd: <personal access token som man skapar på github>
  # 
  # Man skapar ett personal access token på sin Github-användare på github.com genom att välja:
  #         Settings - Developer settings - Personal access tokens - Tokens (classic)
  #         och där skapar man ett personal token som lägger in i sin keyring enligt ovan.
  #
  # =======================================================================================================================

 repo_lokalt <- repository(here())                     # intiera ett git-objekt i git2r
 # configurera repository och ordna autentisering för Github. Se ovan vad som krävs för att detta ska fungera 
 git2r::config(repo_lokalt, user.name = key_list(service = "git2r")$username, user.email = key_get("git2r", key_list(service = "git2r")$username))
 git2r::commit(all = TRUE, message = "Uppdatera webbsida")                         # commit på alla ändrade filer
 git2r::pull(repo = repo_lokalt,                                                   # pull så att repositoryt är uppdaterat
            credentials = cred_user_pass( username = key_list(service = "github")$username, 
                                          password = key_get("github", key_list(service = "github")$username)))
 git2r::push( object = repo_lokalt,                                                # och så en push så att filerna skickas upp till github
          credentials = cred_user_pass( username = key_list(service = "github_token")$username, 
                                        password = key_get("github_token", key_list(service = "github_token")$username)))


}}

```
')
  
  # nu sätter vi ihop hela Rmd-filen  
  hela_rmd_filen <- paste0(rmd_header, "\n\n",
                           rmd_init, "\n\n",
                           rmd_text, "\n\n",
                           rmd_slut_skript)  
  
  # Vi skriver filen till mappen
  writeLines(hela_rmd_filen, paste0(sokvag_proj, github_repo, ".Rmd"))
  
  #setwd(sokvag_proj)
  
  #usethis::create_project("c:/gh_falupeppe/Test-repo")
  
  # Byt arbetskatalog till det nya projektet
  
  #setwd(sokvag_proj)
  # skapa git först
  
  gert::git_init()
  gert::git_add(".")
  gert::git_commit("Initiera Git")
  
  # därefter github
  if (is.null(github_org)) {
    use_github(
      private = FALSE,
      protocol = "https"
    )
  } else {
    use_github(
      organisation = github_org,
      private = FALSE,
      visibility = "public",
      protocol = "https"
    ) 
  }
  
  # # ställ in att vi ska använda Github pages
  # use_github_pages(branch = git_default_branch(), path = "/docs", cname = NA)
  
  # ställ in behörighet för samhallsanalys om parametern är TRUE
  if (behorighet_samhallsanalys && !is.null(github_org)) {
    
    response <- PUT(
      url = glue("https://api.github.com/orgs/{github_org}/teams/samhallsanalys/repos/{github_org}/{repo_namn}"),
      add_headers(Authorization = paste("token", key_get("github_token", key_list(service = "github_token")$username))),
      body = list(permission = "push"),
      encode = "json"
    )
    
    if (httr::status_code(response) == 204) {
      message("✅ Teamet 'samhallsanalys' har fått push-behörighet.")  # visa svar från GitHub
    }
    
  }
  
} # slut funktion

git_kontrollera_id_uppgifter <- function() {
  
  # 1. Rensa env vars som annars vinner över config
  Sys.unsetenv(c(
    "GIT_AUTHOR_NAME",
    "GIT_AUTHOR_EMAIL",
    "GIT_COMMITTER_NAME",
    "GIT_COMMITTER_EMAIL"
  ))
  
  # 2. Läs global git-config (samma princip som git_value)
  git_value <- function(key) {
    res <- tryCatch(
      system(paste("git config --global", key), intern = TRUE),
      error = function(e) character(0)
    )
    if (length(res) == 0) "" else res[[1]]
  }
  
  name  <- git_value("user.name")
  email <- git_value("user.email")
  
  # 3. Kontroll
  if (!nzchar(name) || !nzchar(email)) {
    stop(
      "Git-identitet saknas eller är tom.\n\n",
      "Åtgärda genom att köra:\n",
      "  git config --global user.name \"Ditt Namn\"\n",
      "  git config --global user.email \"din.epost@example.com\"",
      call. = FALSE
    )
  }
  
  # 4. Sätt lokalt (gäller bara detta repo)
  gert::git_config_set("user.name",  name)
  gert::git_config_set("user.email", email)
  
  invisible(TRUE)
}




shinyapp_skapa_med_github_repo <- function(
    github_repo,                            # Namn på repo OCH Shiny-app (mapp på servern)
    github_org         = "Region-Dalarna",  # Org på GitHub, sätt till NULL för privat konto
    rapport_titel      = github_repo,       # Titel som visas i titlePanel
    rapport_undertitel = NA,                # (används bara i README nu, kan byggas ut)
    githubmapp_lokalt  = "c:/gh/",          # Sökväg till mapp där du har alla github-repon, t.ex. "C:/github_repos"
    behorighet_team    = "samhallsanalys",  # GitHub-team som får push-behörighet, NULL om inget team
    target             = "publik",          # Default-server i _publicering_till_server.yml: "publik" eller "intern"
    test_skapa_ej_repo = FALSE,             # TRUE = hoppa över git-init OCH GitHub-repo (bara lokala filer/mappar)
    oppna_github_sida = TRUE,               # vi öppnar github-sidan när repot är skapat
    github_oppna_fordrojning = 2            # vi fördröjer öppningen av GitHub-sidan med någon sekund för att ge GitHub tid att skapa repot innan vi försöker öppna det (annars kan det bli 404)
) {
  
  # =============================================================================
  #  shinyapp_skapa_med_github_repo()  —  UTPLATTAD STRUKTUR
  #
  #  Ersätt din nuvarande shinyapp_skapa_med_github_repo() i func_API.R med denna.
  #  Övriga funktioner i func_API.R är oförändrade.
  #
  #  Skillnad mot tidigare:
  #   * Appfilerna (global.R, ui.R, server.R, R/, www/) skapas DIREKT I ROTEN,
  #     inte i en app/-undermapp.
  #   * Ingen app.R / runApp('app')-omväg längre — Shiny Server kör appen direkt
  #     från roten, precis som dina fungerande appar (brott, export).
  #   * renv ligger kvar i roten (renv.lock, .Rprofile, renv/) bredvid appfilerna.
  #   * deploy.yml använder oförändrat TEMP_DIR="${GITHUB_WORKSPACE}" (hela roten).
  # =============================================================================
  
  # ==== Beroenden ==============================================================
  
  pkg_needed <- c("usethis", "gert", "glue", "stringr", "purrr", "httr", "keyring", "renv", "callr")
  miss <- pkg_needed[!vapply(pkg_needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0) {
    stop(
      "Följande paket behöver installeras först: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!target %in% c("publik", "intern")) {
    stop("target måste vara 'publik' eller 'intern'.", call. = FALSE)
  }
  
  # Lokal helper: skapa mapp om den inte finns
  skapa_mapp_om_den_inte_finns <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  }
  
  
  # ==== Normalisera sökvägar ===================================================
  
  githubmapp_lokalt <- stringr::str_replace_all(githubmapp_lokalt, stringr::fixed("\\"), "/")
  if (!stringr::str_ends(githubmapp_lokalt, "/")) {
    githubmapp_lokalt <- paste0(githubmapp_lokalt, "/")
  }
  
  sokvag_proj <- paste0(githubmapp_lokalt, github_repo)
  if (!stringr::str_ends(sokvag_proj, "/")) {
    sokvag_proj <- paste0(sokvag_proj, "/")
  }
  
  # Skapa rotmapp om den inte finns
  skapa_mapp_om_den_inte_finns(sokvag_proj)
  
  
  # ==== Skapa R-projekt ========================================================
  
  gitprojekt_sokvag <- if (stringr::str_sub(sokvag_proj, -1, -1) == "/") {
    stringr::str_sub(sokvag_proj, 1, -2)
  } else {
    sokvag_proj
  }
  
  usethis::create_project(gitprojekt_sokvag, open = FALSE)
  unlink(file.path(sokvag_proj, "R"), recursive = TRUE)             # ta bort R-mappen som usethis lägger i roten; vi skapar vår egen nedan
  
  # ==== Skapa struktur i ROTEN: www/, R/, .github/workflows/ ==================
  # Appfilerna ligger direkt i repo-roten (ingen app/-undermapp), så att
  # Shiny Server kör appen utan app.R-omväg.
  
  www_dir       <- file.path(sokvag_proj, "www")
  r_dir         <- file.path(sokvag_proj, "R")
  workflows_dir <- file.path(sokvag_proj, ".github", "workflows")
  fonts_dir     <- file.path(www_dir, "fonts")
  
  purrr::walk(
    c(www_dir, r_dir, workflows_dir, fonts_dir),
    skapa_mapp_om_den_inte_finns
  )
  
  file.create(file.path(r_dir, ".gitkeep"))                        # .gitkeep så att tomma R/ följer med i git
  
  # ==== Hämta stilfiler från Region-Dalarna/depot =============================
  # Filer som hämtas direkt från depot (delade tillgångar):
  #   - favicon.ico               -> www/favicon.ico
  #   - regiondalarna_ruf.css     -> www/regiondalarna_ruf.css
  #   - logo_liggande_fri_vit.png -> www/logo_liggande_fri_vit.png
  #   - fonts/*                   -> www/fonts/  (listas via GitHub API)
  #
  # OBS: app.css genereras lokalt som ett tomt skal nedan, eftersom depot-versionen
  # är specifik för Brottsappen. Lägg appspecifika regler i appens egen app.css.
  
  depot_raw_bas <- "https://raw.githubusercontent.com/Region-Dalarna/depot/main"
  
  # Helper: ladda ner binär eller text-fil från depot via httr::GET
  depot_hamta_fran <- function(rel_sokvag, target_path) {
    url  <- paste0(depot_raw_bas, "/", rel_sokvag)
    resp <- httr::GET(url)
    httr::stop_for_status(resp, task = paste("hämta", rel_sokvag, "från depot"))
    writeBin(httr::content(resp, as = "raw"), target_path)
  }
  
  depot_hamta_fran("favicon.ico",               file.path(www_dir, "favicon.ico"))
  depot_hamta_fran("regiondalarna_ruf.css",     file.path(www_dir, "regiondalarna_ruf.css"))
  depot_hamta_fran("logo_liggande_fri_vit.png", file.path(www_dir, "logo_liggande_fri_vit.png"))
  
  # Lista fonts/-katalogen i depot via GitHub Contents API och ladda ner varje fil
  fonts_api_url <- "https://api.github.com/repos/Region-Dalarna/depot/contents/fonts"
  fonts_resp <- httr::GET(fonts_api_url, httr::accept("application/vnd.github+json"))
  if (httr::status_code(fonts_resp) == 200) {
    fonts_lista <- httr::content(fonts_resp, as = "parsed")
    purrr::walk(fonts_lista, function(f) {
      if (identical(f$type, "file")) {
        depot_hamta_fran(paste0("fonts/", f$name), file.path(fonts_dir, f$name))
      }
    })
    message("✅ Hämtade ", length(fonts_lista), " typsnittsfiler från depot/fonts/.")
  } else {
    warning("Kunde inte lista depot/fonts/ (status ", httr::status_code(fonts_resp),
            "). Lägg till typsnitt manuellt i www/fonts/.", call. = FALSE)
  }
  
  # Generera ett tomt skal för app.css (appspecifika overrides)
  app_css <- glue::glue(
    "/* ============================================================
   app.css
   App-specifik styling för {github_repo}.
   Laddas EFTER regiondalarna_ruf.css så att lokala regler
   kan överstyra den delade identiteten vid behov.
   ============================================================ */

/* Lägg appspecifika regler här. */
"
  )
  writeLines(app_css, file.path(www_dir, "app.css"))
  
  
  # ==== Skapa global.R (i roten) ==============================================
  
  global_R <- glue::glue(
    '## Globala inställningar för Shinyappen: <<github_repo>>

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggiraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# Allmänna options - TRUE = visa inte R-felmeddelanden i appen, FALSE = visa felmeddelanden från R på webben
options(shiny.sanitize.errors = FALSE)
',
    .open = "<<", .close = ">>")
  
  
  writeLines(global_R, file.path(sokvag_proj, "global.R"))
  
  # ==== Skapa ui.R (i roten) ==================================================
  ui_R <- glue::glue("
source('global.R')

shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css')
    ),

    # ---- Header (matchar .rd-header i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-header',
      tags$div(class = 'rd-header__title', '<<rapport_titel>>'),
      tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna'),
        tags$span('Samhällsanalys')
      )
    ),

    # ---- Innehåll ---------------------------------------------------------
    tabsetPanel(
      tabPanel('Tab 1',
        h3('Hej från <<github_repo>>'),
        verbatimTextOutput('example_text')
      ),
      tabPanel('Om', p('Beskriv applikationen här.'))
    ),

    # ---- Footer (matchar .rd-footer i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-footer',
      'Samhällsanalys, Region Dalarna · ',
      tags$a(
        href = 'mailto:samhallsanalys@regiondalarna.se',
        'samhallsanalys@regiondalarna.se'
      )
    )
  )
)
",
  .open = "<<", .close = ">>"
  )

writeLines(ui_R, file.path(sokvag_proj, "ui.R"))

# ==== Skapa server.R (i roten) ==============================================

server_R <-
  "shinyServer(function(input, output, session) {

  output$example_text <- renderText({
    'Byt ut detta mot din egen serverlogik.'
  })

})
"

writeLines(server_R, file.path(sokvag_proj, "server.R"))

# ==== Skapa .gitignore ======================================================

gitignore_content <- "
.Rproj.user
.Rhistory
.RData
.Ruserdata
.Rproj.user/
.Rhistory
.RData
.Ruserdata
.Rhistory
.Rapp.history
"

writeLines(trimws(gitignore_content, which = "left"),
           file.path(sokvag_proj, ".gitignore"))


# ==== Skapa README ==========================================================

readme_content <- glue::glue(
  "# {rapport_titel}

Detta repository innehåller en Shinyapplikation (`{github_repo}`) för Samhällsanalys, Region Dalarna.

## Struktur

Appfilerna ligger **direkt i repo-roten** (så att Shiny Server kör appen utan omväg):

- `ui.R`, `server.R`, `global.R`
- `www/` för favicon, logotyp, CSS (`regiondalarna_ruf.css` + `app.css`) och `fonts/`
- `R/` för hjälpfunktioner

- `_dependencies.R` i root listar alla paket appen använder (läses av `renv::dependencies()`, körs aldrig)
- `_publicering_till_server.yml` i root styr vilken Shiny-server som är default för `shinyapp_publicera()`
- `renv.lock` + `renv/` + `.Rprofile` styr paketversioner. Kör `renv::restore()` efter klon för att få samma paket som senast snapshot:ades. Vid nya paket: `renv::install(...)` följt av `renv::snapshot()`.

- Deployment sker via GitHub Actions:
  - `.github/workflows/deploy.yml` – publicerar vid push till `publicera-publik` eller `publicera-intern`
  - `.github/workflows/avpublicera.yml` – tar bort appen från vald server (manuell trigger)

  Appmapp på servern: `/srv/shiny-server/{github_repo}`.

")

writeLines(readme_content, file.path(sokvag_proj, "README.md"))

# ==== Skapa _dependencies.R (root) ==========================================

dependencies_R <- glue::glue(
  "# _dependencies.R – läses av renv::dependencies(), körs aldrig
# Lägg till alla paket appen använder, även de som laddas via source().
library(DBI)
library(RPostgres)
library(sf)
library(dbplyr)
# ... lägg till fler paket vid behov
"
)

writeLines(dependencies_R, file.path(sokvag_proj, "_dependencies.R"))


# ==== Skapa _publicering_till_server.yml (root) =============================

publicering_yml <- glue::glue(
  "# publicering_till_server.yml
#
# Styr vilken Shiny-server som appen publiceras till när
# shinyapp_publicera() körs utan target-parameter.
#
# Giltiga värden för target:
#   publik   - publicera till den publika Shiny-servern
#              (shiny.regiondalarna.se)
#   intern   - publicera till den interna Shiny-servern
#              (shiny.ltdalarna.se)
#
# För engångs-override utan att andra appens hemvist:
#   shinyapp_publicera(\"appnamn\", target = \"intern\")
#
# For att permanent flytta appen till andra servern, använd:
#   shinyapp_flytta(\"appnamn\", till = \"intern\")

target: {target}
"
)

writeLines(publicering_yml, file.path(sokvag_proj, "_publicering_till_server.yml"))


# ==== Skapa .github/workflows/deploy.yml ====================================

deploy_yml <-
  "name: Deploy Shiny app
run-name: Deploy ${{ github.event.repository.name }} → ${{ github.ref_name == 'publicera-intern' && 'intern' || 'publik' }}

on:
  push:
    branches: [ publicera-publik, publicera-intern ]
  workflow_dispatch:

jobs:
  deploy:
    runs-on:
      - self-hosted
      - ${{ github.ref == 'refs/heads/publicera-intern' && 'shiny-deploy-intern' || 'shiny-deploy-publik' }}

    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Visa vilken server som deploy:as till
        run: |
          if [ \"$GITHUB_REF\" = \"refs/heads/publicera-intern\" ]; then
            echo \"Deploy till INTERN server (${{ github.event.repository.name }})\"
          else
            echo \"Deploy till PUBLIK server (${{ github.event.repository.name }})\"
          fi

      - name: Deploy app using server-side script
        run: |
          TEMP_DIR=\"${GITHUB_WORKSPACE}\"
          /usr/local/bin/shiny_deploy.sh \"${{ github.event.repository.name }}\" \"$TEMP_DIR\"

      - name: Restart Shiny Server if available
        run: |
          echo \"Kontrollerar om shiny-server-tjänsten finns...\"

          if systemctl status shiny-server >/dev/null 2>&1; then
            echo \"shiny-server hittades. Försöker restart...\"

            if sudo systemctl restart shiny-server; then
              echo \"Shiny Server restart lyckades.\"
            else
              echo \"FEL: Kunde inte köra 'sudo systemctl restart shiny-server'\" >&2
              echo \"Försöker hämta senaste loggrader från /var/log/shiny-server.log...\"

              if [ -f /var/log/shiny-server.log ]; then
                echo \"====== SISTA 50 RADERNA UR shiny-server.log ======\"
                sudo tail -n 50 /var/log/shiny-server.log || echo \"Kunde inte läsa /var/log/shiny-server.log\"
                echo \"====================================================\"
              else
                echo \"Ingen loggfil hittades på /var/log/shiny-server.log\"
              fi

              exit 1
            fi
          else
            echo \"Ingen systemd-tjänst med namnet 'shiny-server' hittades. Hoppar över restart.\"
          fi
"

writeLines(deploy_yml, file.path(workflows_dir, "deploy.yml"))


# ==== Skapa .github/workflows/avpublicera.yml ===============================

avpublicera_yml <-
  "name: Avpublicera Shiny app
run-name: Avpublicera ${{ github.event.repository.name }} från ${{ inputs.target }}

on:
  workflow_dispatch:
    inputs:
      target:
        description: 'Vilken server ska appen tas bort från?'
        required: true
        type: choice
        options:
          - publik
          - intern
      bekraftelse:
        description: 'Skriv appnamnet (= repo-namnet) för att bekräfta'
        required: true
        type: string

jobs:
  avpublicera:
    runs-on:
      - self-hosted
      - ${{ inputs.target == 'intern' && 'shiny-deploy-intern' || 'shiny-deploy-publik' }}

    steps:
      - name: Kontrollera bekräftelse
        run: |
          if [ \"${{ inputs.bekraftelse }}\" != \"${{ github.event.repository.name }}\" ]; then
            echo \"FEL: Bekräftelseordet matchar inte repo-namnet.\"
            exit 1
          fi
          echo \"Bekräftelse OK. Tar bort ${{ github.event.repository.name }} från ${{ inputs.target }} server.\"

      - name: Ta bort app från Shiny-server
        run: |
          APP_DIR=\"/srv/shiny-server/${{ github.event.repository.name }}\"

          # säkerhetsräcke: pathen MÅSTE ligga under /srv/shiny-server/
          # och får inte vara just själva foldern
          if [[ \"$APP_DIR\" != /srv/shiny-server/* ]] || [ \"$APP_DIR\" = \"/srv/shiny-server/\" ]; then
            echo \"FEL: Otillåten sökväg: $APP_DIR\"
            exit 1
          fi

          if [ -d \"$APP_DIR\" ]; then
            echo \"Tar bort $APP_DIR\"
            sudo rm -rf \"$APP_DIR\"
            echo \"App borttagen.\"
          else
            echo \"Ingen app hittades i $APP_DIR — inget att ta bort.\"
          fi

      - name: Restart Shiny Server if available
        run: |
          if systemctl status shiny-server >/dev/null 2>&1; then
            sudo systemctl restart shiny-server && echo \"Shiny Server omstartad.\"
          fi
"

writeLines(avpublicera_yml, file.path(workflows_dir, "avpublicera.yml"))


# ==== Initiera renv och installera grundpaket ===============================

paket_app <- c(
  "shiny",
  "shinyjs",
  "shinyWidgets",
  "DT",
  "ggiraph",
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2",
  "DBI",
  "RPostgres",
  "sf"
)

if (!requireNamespace("renv", quietly = TRUE)) {
  stop(
    "Paketet 'renv' är inte installerat. Installera det först med install.packages('renv').",
    call. = FALSE
  )
}

if (!requireNamespace("callr", quietly = TRUE)) {
  stop(
    "Paketet 'callr' är inte installerat. Installera det först med install.packages('callr').",
    call. = FALSE
  )
}

callr::r(
  func = function(project, packages) {
    
    # Tvinga https-CRAN så att renv.lock föds med en URL servern faktiskt når.
    # http://cloud.r-project.org är blockerat utgående på servern, och PPM:s
    # backend (rspm-sync) likaså — https://cloud.r-project.org fungerar.
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    
    renv::init(
      project = project,
      bare    = TRUE,
      restart = FALSE,
      load    = TRUE
    )
    
    # Hämta bara riktiga runtime-beroenden, inte Suggests
    ap <- available.packages()
    
    deps <- tools::package_dependencies(
      packages = packages,
      db       = ap,
      which    = c("Depends", "Imports", "LinkingTo"),
      recursive = TRUE
    )
    
    packages_all <- unique(c(
      packages,
      unlist(deps, use.names = FALSE)
    ))
    
    packages_all <- setdiff(packages_all, c("R", NA))
    
    renv_lib <- renv::paths$library(project = project)
    
    renv::install(
      packages = packages_all,
      project  = project,
      library  = renv_lib,
      prompt   = FALSE
    )
    
    renv::snapshot(
      project = project,
      prompt  = FALSE
    )
    
    renv::restore(
      project = project,
      prompt  = FALSE
    )
    
  },
  args = list(
    project  = gitprojekt_sokvag,
    packages = paket_app
  )
)

message("✅ renv initierat, grundpaket installerade, renv.lock skapad och projektbiblioteket synkat.")

# ==== Initiera Git och GitHub-repo (om test_skapa_ej_repo = FALSE) ==========

if (isTRUE(test_skapa_ej_repo)) {
  message("ℹ️ test_skapa_ej_repo = TRUE — hoppar över git-init och GitHub-repo. ",
          "Projektet finns bara lokalt i ", sokvag_proj)
} else {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(sokvag_proj)
  
  gert::git_init()
  git_kontrollera_id_uppgifter()
  gert::git_add(".")
  gert::git_commit("Initiera Shinyapp-projekt")
  
  
  if (Sys.getenv("GITHUB_PAT") == "") {
    keyring_poster <- keyring::key_list(service = "github_token")
    
    if (nrow(keyring_poster) == 0) {
      stop(
        "Ingen GitHub-token hittades i Sys.getenv('GITHUB_PAT') eller keyring service = 'github_token'.\n",
        "Kör usethis::gh_token_help() eller spara token med gitcreds::gitcreds_set().",
        call. = FALSE
      )
    }
    
    gh_user <- keyring_poster$username[1]
    Sys.setenv(GITHUB_PAT = keyring::key_get("github_token", gh_user))
  }
  
  if (Sys.getenv("GITHUB_PAT") == "") {
    stop(
      "GITHUB_PAT är fortfarande tom efter försök att läsa från keyring.",
      call. = FALSE
    )
  }
  
  # Skapa repo på GitHub
  if (is.null(github_org)) {
    usethis::use_github(
      private   = FALSE,
      protocol  = "https"
    )
  } else {
    
    # Skapa URL till GitHub-repot
    github_repo_url <- paste0("https://github.com/", github_org, "/", github_repo)
    
    # Hindra usethis::use_github() från att öppna webbläsaren direkt
    old_browser <- getOption("browser")
    on.exit(options(browser = old_browser), add = TRUE)
    
    options(browser = function(url) invisible(NULL))
    
    # Skapa repo på GitHub
    usethis::use_github(
      organisation = github_org,
      private      = FALSE,
      visibility   = "public",
      protocol     = "https"
    )
    
    # Återställ browser-option direkt efter use_github()
    options(browser = old_browser)
    
    # Öppna GitHub-sidan själv, efter liten fördröjning
    if (isTRUE(oppna_github_sida)) {
      Sys.sleep(github_oppna_fordrojning)
      utils::browseURL(github_repo_url)
    }
    
  } # if-sats för att avgöra om repo ska skapas under org eller privat konto, detta är slutet på org-delen
  
  # Ge team behörighet om angivet
  if (!is.null(behorighet_team) && !is.null(github_org)) {
    
    gh_token <- Sys.getenv("GITHUB_PAT")
    
    resp <- httr::PUT(
      url = glue::glue(
        "https://api.github.com/orgs/{github_org}/teams/{behorighet_team}/repos/{github_org}/{github_repo}"
      ),
      httr::add_headers(Authorization = paste("token", gh_token)),
      body   = list(permission = "push"),
      encode = "json"
    )
    
    if (httr::status_code(resp) == 204) {
      message("✅ Teamet '", behorighet_team, "' har fått push-behörighet.")
    } else {
      message("⚠️ Kunde inte sätta team-behörighet automatiskt (status ",
              httr::status_code(resp), ").")
    }
  }
  
  # ---- Registrera avpublicera.yml hos GitHub Actions -----------------------
  # En workflow med enbart workflow_dispatch indexeras inte alltid från
  # initial-pushen — bara deploy.yml (som har en push-trigger) plockas upp.
  # Vi rör avpublicera.yml en gång till på master så att GitHub Actions
  # scannar in den. Utan detta failar första avpublicera()/flytta() på en
  # oregistrerad workflow (HTTP 404 vid dispatch).
  cat("\n# (touch för workflow-registrering)\n",
      file = file.path(workflows_dir, "avpublicera.yml"), append = TRUE)
  gert::git_add(".github/workflows/avpublicera.yml", repo = sokvag_proj)
  gert::git_commit("Registrera-avpublicera-workflow", repo = sokvag_proj)
  .gh_push(sokvag_proj, "master")
  message("✅ avpublicera.yml registrerad hos GitHub Actions.")
  
}

invisible(sokvag_proj)
}


shinyapp_skapa_med_github_repo_forka_befintligt <- function(
    github_repo,                            # Namn på NYTT repo OCH Shiny-app (mapp på servern)
    kalla_repo_url,                         # URL till det befintliga repot, t.ex. "https://github.com/nån/coolapp.git"
    kalla_branch       = "main",            # Branch att hämta från i källrepot
    github_org         = "Region-Dalarna",  # Org på GitHub, sätt till NULL för privat konto
    rapport_titel      = github_repo,       # Titel som visas i README
    rapport_undertitel = NA,                # (används bara i README nu, kan byggas ut)
    githubmapp_lokalt  = "c:/gh/",          # Sökväg till mapp där du har alla github-repon
    behorighet_team    = "samhallsanalys",  # GitHub-team som får push-behörighet, NULL om inget team
    target             = "publik",          # Default-server i _publicering_till_server.yml: "publik" eller "intern"
    r_projekt_oppna    = TRUE               # TRUE = öppna .Rproj-filen i RStudio när allt är klart
) {
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8")
  
  # ==== Beroenden ==============================================================
  
  pkg_needed <- c("usethis", "gert", "glue", "stringr", "purrr", "httr", "keyring")
  miss <- pkg_needed[!vapply(pkg_needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0) {
    stop(
      "Följande paket behöver installeras först: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!target %in% c("publik", "intern")) {
    stop("target måste vara 'publik' eller 'intern'.", call. = FALSE)
  }
  
  if (missing(kalla_repo_url) || !nzchar(kalla_repo_url)) {
    stop("Parametern 'kalla_repo_url' måste anges (URL till det befintliga repot).",
         call. = FALSE)
  }
  
  # Lokal helper: skapa mapp om den inte finns
  skapa_mapp_om_den_inte_finns <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  }
  
  
  # ==== Normalisera sökvägar ===================================================
  
  githubmapp_lokalt <- stringr::str_replace_all(githubmapp_lokalt, stringr::fixed("\\"), "/")
  if (!stringr::str_ends(githubmapp_lokalt, "/")) {
    githubmapp_lokalt <- paste0(githubmapp_lokalt, "/")
  }
  
  sokvag_proj <- paste0(githubmapp_lokalt, github_repo)
  if (!stringr::str_ends(sokvag_proj, "/")) {
    sokvag_proj <- paste0(sokvag_proj, "/")
  }
  
  if (dir.exists(sokvag_proj)) {
    stop("Mappen finns redan: ", sokvag_proj,
         "\nVälj ett annat namn eller ta bort mappen först.", call. = FALSE)
  }
  
  # Skapa rotmapp
  skapa_mapp_om_den_inte_finns(sokvag_proj)
  
  
  # ==== Skapa R-projekt ========================================================
  
  gitprojekt_sokvag <- if (stringr::str_sub(sokvag_proj, -1, -1) == "/") {
    stringr::str_sub(sokvag_proj, 1, -2)
  } else {
    sokvag_proj
  }
  
  usethis::create_project(gitprojekt_sokvag, open = FALSE)
  unlink(file.path(sokvag_proj, "R"), recursive = TRUE)  # ta bort mappen R i root
  
  # OBS: skapa INTE app/ — git subtree add kräver att prefix-mappen inte finns
  
  # ==== Skapa .github/workflows-mappen =========================================
  
  workflows_dir <- file.path(sokvag_proj, ".github", "workflows")
  skapa_mapp_om_den_inte_finns(workflows_dir)
  
  
  # ==== Hjälpare: kör git i rätt repo (för subtree-stöd) =======================
  
  kor_git <- function(...) {
    res <- suppressWarnings(
      system2("git", args = c("-C", gitprojekt_sokvag, ...),
              stdout = TRUE, stderr = TRUE)
    )
    status <- attr(res, "status"); if (is.null(status)) status <- 0L
    if (status != 0) {
      stop("git ", paste(c(...), collapse = " "), " misslyckades:\n",
           paste(res, collapse = "\n"), call. = FALSE)
    }
    res
  }
  
  
  # ==== Skapa .gitignore =======================================================
  
  gitignore_content <- "
.Rproj.user
.Rhistory
.RData
.Ruserdata
.Rproj.user/
.Rhistory
.RData
.Ruserdata
.Rhistory
.Rapp.history
"
  
  writeLines(trimws(gitignore_content, which = "left"),
             file.path(sokvag_proj, ".gitignore"))
  
  
  # ==== Skapa README ===========================================================
  
  readme_content <- glue::glue(
    "# {rapport_titel}

Detta repository innehåller en Shinyapplikation (`{github_repo}`) för Samhällsanalys, Region Dalarna.

Appkoden under `app/` är importerad från ett befintligt repo via `git subtree`:

- **Källa:** {kalla_repo_url}
- **Branch vid import:** {kalla_branch}

## Struktur

- All appkod ligger i katalogen `app/` (importerad via git subtree)
- `_publicering_till_server.yml` i root styr vilken Shiny-server som är default för `shinyapp_publicera()`

- Deployment sker via GitHub Actions:
  - `.github/workflows/deploy.yml` – publicerar vid push till `publicera-publik` eller `publicera-intern`
  - `.github/workflows/avpublicera.yml` – tar bort appen från vald server (manuell trigger)

  Appmapp på servern: `/srv/shiny-server/{github_repo}`.

## Hämta uppdateringar från källrepot

Appkoden under `app/` är kopplad till källrepot via `git subtree`. När
källrepot uppdateras kan du dra in ändringarna med ett kommando.

### Så här fungerar det

`git subtree pull` hämtar senaste från källrepot och slår ihop ändringarna
till en enda squashad commit under `app/`. Resten av repot (`.github/`,
README, deploy-workflows) påverkas inte.

Subtree pull kräver att working tree är **clean** — alla ändringar måste
vara committade eller stashade innan du kör. Annars får du felet:

```
fatal: working tree has modifications. Cannot add.
```

### Kommandon (Terminal, från repots mapp)

Stå i `{gitprojekt_sokvag}` när du kör kommandona:

```
cd {gitprojekt_sokvag}
```

**1. Kontrollera att working tree är clean:**

```
git status
```

Om det visar `nothing to commit, working tree clean` — hoppa till steg 3.

**2. Om det finns ändringar — committa eller stasha dem:**

Ett vanligt fall: `.Rproj`-filen ändras när du öppnat projektet i RStudio.
Committa den då:

```
git add .
git commit -m \"Lokala ändringar innan subtree pull\"
```

Eller stasha tillfälligt om du inte vill committa:

```
git stash
```

(återställ sedan efter pull med `git stash pop`)

**3. Hämta uppdateringar från källrepot:**

```
git subtree pull --prefix=app {kalla_repo_url} {kalla_branch} --squash
```

Om en editor öppnas med merge-meddelandet — bara spara och stäng:
- I **vim**: `Esc`, sen `:wq` + Enter
- I **nano**: `Ctrl+O`, Enter, `Ctrl+X`

**4. Pusha till GitHub:**

```
git push
```

### Alternativ: kör från R-konsolen

Om du föredrar att stanna i R kan du köra hela kedjan så här
(fungerar oavsett var i R du står — `-C` byter mapp åt dig):

```r
system2(\"git\", c(\"-C\", \"{gitprojekt_sokvag}\", \"add\", \".\"))
system2(\"git\", c(\"-C\", \"{gitprojekt_sokvag}\", \"commit\",
                 \"-m\", \"Lokala ändringar innan subtree pull\"))
system2(\"git\", c(\"-C\", \"{gitprojekt_sokvag}\",
                 \"subtree\", \"pull\", \"--prefix=app\",
                 \"{kalla_repo_url}\", \"{kalla_branch}\", \"--squash\"))
system2(\"git\", c(\"-C\", \"{gitprojekt_sokvag}\", \"push\"))
```

")
  
  writeLines(readme_content, file.path(sokvag_proj, "README.md"))
  
  
  # ==== Skapa _publicering_till_server.yml (root) =============================
  
  publicering_yml <- glue::glue(
    "# publicering_till_server.yml
#
# Styr vilken Shiny-server som appen publiceras till när
# shinyapp_publicera() körs utan target-parameter.
#
# Giltiga värden för target:
#   publik   - publicera till den publika Shiny-servern
#              (shiny.regiondalarna.se)
#   intern   - publicera till den interna Shiny-servern
#              (shiny.ltdalarna.se)
#
# För engångs-override utan att andra appens hemvist:
#   shinyapp_publicera(\"appnamn\", target = \"intern\")
#
# For att permanent flytta appen till andra servern, använd:
#   shinyapp_flytta(\"appnamn\", till = \"intern\")

target: {target}
"
  )
  
  writeLines(publicering_yml, file.path(sokvag_proj, "_publicering_till_server.yml"))
  
  
  # ==== Skapa .github/workflows/deploy.yml ====================================
  
  deploy_yml <-
    "name: Deploy Shiny app
run-name: Deploy ${{ github.event.repository.name }} → ${{ github.ref_name == 'publicera-intern' && 'intern' || 'publik' }}

on:
  push:
    branches: [ publicera-publik, publicera-intern ]
  workflow_dispatch:

jobs:
  deploy:
    runs-on:
      - self-hosted
      - ${{ github.ref == 'refs/heads/publicera-intern' && 'shiny-deploy-intern' || 'shiny-deploy-publik' }}

    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Visa vilken server som deploy:as till
        run: |
          if [ \"$GITHUB_REF\" = \"refs/heads/publicera-intern\" ]; then
            echo \"Deploy till INTERN server (${{ github.event.repository.name }})\"
          else
            echo \"Deploy till PUBLIK server (${{ github.event.repository.name }})\"
          fi

      - name: Deploy app using server-side script
        run: |
          TEMP_DIR=\"${GITHUB_WORKSPACE}/app\"
          /usr/local/bin/shiny_deploy.sh \"${{ github.event.repository.name }}\" \"$TEMP_DIR\"

      - name: Restart Shiny Server if available
        run: |
          echo \"Kontrollerar om shiny-server-tjänsten finns...\"

          if systemctl status shiny-server >/dev/null 2>&1; then
            echo \"shiny-server hittades. Försöker restart...\"

            if sudo systemctl restart shiny-server; then
              echo \"Shiny Server restart lyckades.\"
            else
              echo \"FEL: Kunde inte köra 'sudo systemctl restart shiny-server'\" >&2
              echo \"Försöker hämta senaste loggrader från /var/log/shiny-server.log...\"

              if [ -f /var/log/shiny-server.log ]; then
                echo \"====== SISTA 50 RADERNA UR shiny-server.log ======\"
                sudo tail -n 50 /var/log/shiny-server.log || echo \"Kunde inte läsa /var/log/shiny-server.log\"
                echo \"====================================================\"
              else
                echo \"Ingen loggfil hittades på /var/log/shiny-server.log\"
              fi

              exit 1
            fi
          else
            echo \"Ingen systemd-tjänst med namnet 'shiny-server' hittades. Hoppar över restart.\"
          fi
"
  
  writeLines(deploy_yml, file.path(workflows_dir, "deploy.yml"))
  
  
  # ==== Skapa .github/workflows/avpublicera.yml ===============================
  
  avpublicera_yml <-
    "name: Avpublicera Shiny app
run-name: Avpublicera ${{ github.event.repository.name }} från ${{ inputs.target }}

on:
  workflow_dispatch:
    inputs:
      target:
        description: 'Vilken server ska appen tas bort från?'
        required: true
        type: choice
        options:
          - publik
          - intern
      bekraftelse:
        description: 'Skriv appnamnet (= repo-namnet) för att bekräfta'
        required: true
        type: string

jobs:
  avpublicera:
    runs-on:
      - self-hosted
      - ${{ inputs.target == 'intern' && 'shiny-deploy-intern' || 'shiny-deploy-publik' }}

    steps:
      - name: Kontrollera bekräftelse
        run: |
          if [ \"${{ inputs.bekraftelse }}\" != \"${{ github.event.repository.name }}\" ]; then
            echo \"FEL: Bekräftelseordet matchar inte repo-namnet.\"
            exit 1
          fi
          echo \"Bekräftelse OK. Tar bort ${{ github.event.repository.name }} från ${{ inputs.target }} server.\"

      - name: Ta bort app från Shiny-server
        run: |
          APP_DIR=\"/srv/shiny-server/${{ github.event.repository.name }}\"

          # säkerhetsräcke: pathen MÅSTE ligga under /srv/shiny-server/
          # och får inte vara just själva foldern
          if [[ \"$APP_DIR\" != /srv/shiny-server/* ]] || [ \"$APP_DIR\" = \"/srv/shiny-server/\" ]; then
            echo \"FEL: Otillåten sökväg: $APP_DIR\"
            exit 1
          fi

          if [ -d \"$APP_DIR\" ]; then
            echo \"Tar bort $APP_DIR\"
            sudo rm -rf \"$APP_DIR\"
            echo \"App borttagen.\"
          else
            echo \"Ingen app hittades i $APP_DIR — inget att ta bort.\"
          fi

      - name: Restart Shiny Server if available
        run: |
          if systemctl status shiny-server >/dev/null 2>&1; then
            sudo systemctl restart shiny-server && echo \"Shiny Server omstartad.\"
          fi
"
  
  writeLines(avpublicera_yml, file.path(workflows_dir, "avpublicera.yml"))
  
  
  # ==== Initiera git + första commit (krävs för subtree add) ==================
  
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(sokvag_proj)
  
  gert::git_init()
  git_kontrollera_id_uppgifter()
  gert::git_add(".")
  gert::git_commit("Initiera Shinyapp-projekt med deploy-workflow")
  
  
  # ==== Hämta källrepots innehåll till app/ via git subtree ===================
  
  cat("Hämtar in '", kalla_repo_url, "' (branch '", kalla_branch,
      "') till app/ ...\n", sep = "")
  kor_git("subtree", "add",
          "--prefix=app",
          kalla_repo_url, kalla_branch, "--squash")
  
  
  # ==== Varna om källrepot inte verkar vara en Shiny-app =======================
  
  app_dir <- file.path(sokvag_proj, "app")
  shiny_filer <- c("ui.R", "server.R", "app.R", "global.R")
  hittade <- shiny_filer[file.exists(file.path(app_dir, shiny_filer))]
  
  if (length(hittade) == 0) {
    rek_traffar <- list.files(app_dir, pattern = "^(ui|server|app|global)\\.R$",
                              recursive = TRUE, ignore.case = TRUE)
    
    msg <- paste0(
      "\n⚠️  VARNING: Hittade inga typiska Shiny-filer ",
      "(ui.R, server.R, app.R, global.R) direkt i app/.\n"
    )
    if (length(rek_traffar) > 0) {
      msg <- paste0(msg,
                    "    Hittade dock följande i undermappar:\n      - ",
                    paste(rek_traffar, collapse = "\n      - "), "\n",
                    "    Deploy-skriptet förväntar sig att filerna ligger direkt i app/.\n"
      )
    } else {
      msg <- paste0(msg,
                    "    Källrepot verkar inte vara en Shiny-app — ",
                    "deployen kommer troligen inte att fungera.\n"
      )
    }
    cat(msg)
    
    svar <- readline(prompt = "Vill du ändå fortsätta och pusha till GitHub? (j/N): ")
    if (!tolower(trimws(svar)) %in% c("j", "ja", "y", "yes")) {
      message("Avbryter. Lokal mapp finns kvar i: ", sokvag_proj)
      return(invisible(NULL))
    }
    cat("Fortsätter på användarens bekräftelse...\n")
  } else {
    cat("✓ Hittade Shiny-filer i app/: ",
        paste(hittade, collapse = ", "), "\n", sep = "")
  }
  
  
  # ==== Initiera GitHub-repo ==================================================
  
  if (Sys.getenv("GITHUB_PAT") == "") {
    gh_user  <- keyring::key_list(service = "github_token")$username
    Sys.setenv(GITHUB_PAT = keyring::key_get("github_token", gh_user))
  }
  
  # Skapa repo på GitHub
  if (is.null(github_org)) {
    usethis::use_github(
      private   = FALSE,
      protocol  = "https"
    )
  } else {
    usethis::use_github(
      organisation = github_org,
      private      = FALSE,
      visibility   = "public",
      protocol     = "https"
    )
  }
  
  # Ge team behörighet om angivet
  if (!is.null(behorighet_team) && !is.null(github_org)) {
    gh_user  <- keyring::key_list(service = "github_token")$username
    gh_token <- keyring::key_get("github_token", gh_user)
    
    resp <- httr::PUT(
      url = glue::glue(
        "https://api.github.com/orgs/{github_org}/teams/{behorighet_team}/repos/{github_org}/{github_repo}"
      ),
      httr::add_headers(Authorization = paste("token", gh_token)),
      body   = list(permission = "push"),
      encode = "json"
    )
    
    if (httr::status_code(resp) == 204) {
      message("✅ Teamet '", behorighet_team, "' har fått push-behörighet.")
    } else {
      message("⚠️ Kunde inte sätta team-behörighet automatiskt (status ",
              httr::status_code(resp), ").")
    }
  }
  
  # ==== Öppna R-projektet (om så valt och RStudio är tillgängligt) ============
  
  if (isTRUE(r_projekt_oppna)) {
    rproj_fil <- list.files(sokvag_proj, pattern = "\\.Rproj$",
                            full.names = TRUE)[1]
    
    if (!is.na(rproj_fil) &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      message("📂 Öppnar R-projektet: ", rproj_fil)
      rstudioapi::openProject(rproj_fil, newSession = FALSE)
    } else {
      message("ℹ️ Kunde inte öppna R-projektet automatiskt ",
              "(kör inte i RStudio eller rstudioapi saknas). ",
              "Öppna manuellt: ", sokvag_proj)
    }
  }
  
  invisible(sokvag_proj)
}



shinyapp_publicera <- function(
    github_repo,
    target            = NULL,        # NULL = läs från _publicering_till_server.yml
    lokal_grundsokvag = "c:/gh/",
    github_org        = "Region-Dalarna"
) {
  
  # --- Validering ---
  if (missing(github_repo) || !nzchar(github_repo)) {
    stop("Parametern 'github_repo' måste anges.")
  }
  repo_sokvag <- file.path(lokal_grundsokvag, github_repo)
  if (!dir.exists(repo_sokvag)) stop("Repot finns inte: ", repo_sokvag)
  if (!dir.exists(file.path(repo_sokvag, ".git"))) {
    stop("Mappen är inte ett git-repository: ", repo_sokvag)
  }
  
  default_branch <- .shinyapp_default_branch(repo_sokvag)                       # hämtar default branch, t.ex. "master" eller "main"
  
  # --- Bestäm target ---
  if (is.null(target)) {
    target <- .shinyapp_las_publicering_config(repo_sokvag)
    cat("Target från config: '", target, "'\n", sep = "")
  } else {
    if (!target %in% c("publik", "intern")) {
      stop("target måste vara 'publik' eller 'intern' (eller NULL).")
    }
    cat("Override: target = '", target,
        "' (config-filen ändras inte).\n", sep = "")
  }
  
  publicera_branch <- switch(target,
                             "publik" = "publicera-publik",
                             "intern" = "publicera-intern")
  
  server_url <- switch(target,
                       "publik" = "shiny.regiondalarna.se",
                       "intern" = "shiny.ltdalarna.se")
  
  # --- Hjälpfunktion: kör git i rätt repo ---
  kor_git <- function(..., stopp_vid_fel = TRUE) {
    git_args <- c("-C", repo_sokvag, ...)
    res <- suppressWarnings(
      system2("git", args = git_args, stdout = TRUE, stderr = TRUE)
    )
    status <- attr(res, "status"); if (is.null(status)) status <- 0L
    if (status != 0 && stopp_vid_fel) {
      stop("git ", paste(c(...), collapse = " "), " misslyckades:\n",
           paste(res, collapse = "\n"))
    }
    res
  }
  
  # --- 1. Working tree clean? ---
  status_output <- kor_git("status", "--porcelain")
  if (length(status_output) > 0 && any(nzchar(status_output))) {
    stop("Repot har ohanterade ändringar:\n",
         paste(status_output, collapse = "\n"),
         "\nCommit:a eller stash:a först.")
  }
  
  # --- 2. Fetch ---
  cat("Fetchar från origin...\n")
  kor_git("fetch", "origin")
  
  # --- 3. Säkerställ att target-branchen finns ---
  remote_branches <- trimws(kor_git("branch", "-r"))
  if (!any(grepl(paste0("^origin/", publicera_branch, "$"), remote_branches))) {
    if (target == "publik") {
      cat("'", publicera_branch, "' saknas — kör konfigurering...\n", sep = "")
      .shinyapp_konfigurera_publicera_branch(github_repo, lokal_grundsokvag)
      # uppdatera fetch efteråt
      kor_git("fetch", "origin")
    } else {
      # publicera-intern skapas inline (vi har ingen migreringsväg där)
      cat("'", publicera_branch, "' saknas — skapar från ", default_branch, "...\n", sep = "")
      kor_git("checkout", default_branch)
      kor_git("pull", "--ff-only", "origin", default_branch)
      # Om branchen redan finns lokalt (t.ex. från en tidigare körning där pushen
      # dog) återanvänder vi den i stället för att krascha på checkout -b.
      status_lokal <- .shinyapp_lokal_branch_redo_for_push(repo_sokvag, publicera_branch, default_branch)
      if (status_lokal == "redo") {
        cat("'", publicera_branch, "' fanns redan lokalt — återanvänder den.\n", sep = "")
        kor_git("checkout", publicera_branch)
      } else {
        kor_git("checkout", "-b", publicera_branch)
      }
      #kor_git("push", "-u", "origin", publicera_branch)
      .gh_push(repo_sokvag, publicera_branch, set_upstream = TRUE)
      kor_git("fetch", "origin")
    }
  }
  
  # --- 4. Checka ut publicera-branchen lokalt ---
  local_branches <- trimws(kor_git("branch"))
  if (!any(grepl(paste0("^\\*?\\s*", publicera_branch, "$"), local_branches))) {
    kor_git("checkout", "-b", publicera_branch,
            paste0("origin/", publicera_branch))
  } else {
    kor_git("checkout", publicera_branch)
    kor_git("pull", "--ff-only", "origin", publicera_branch)
  }
  
  # --- 5. Merge default-branchen in ---
  cat("Merge:ar ", default_branch, " in i ", publicera_branch, "...\n", sep = "")
  kor_git("merge", paste0("origin/", default_branch), "--no-edit")
  
  # --- 5b. No-op-skydd: om merge inte gav någon ny commit, gör en tom commit ---
  # Annars blir push:en en no-op och GitHub Actions triggas inte.
  lokal_sha  <- trimws(kor_git("rev-parse", "HEAD"))[1]
  remote_sha <- trimws(kor_git("rev-parse", paste0("origin/", publicera_branch)))[1]
  if (identical(lokal_sha, remote_sha)) {
    cat("Inga nya commits efter merge — lägger in tom commit för att trigga deploy.\n")
    kor_git("commit", "--allow-empty",
            "-m", paste0("Trigga-deploy-", publicera_branch))
  }
  
  # --- 6. Pusha ---
  cat("Pushar till origin/", publicera_branch, "...\n", sep = "")
  #kor_git("push", "origin", publicera_branch)
  .gh_push(repo_sokvag, publicera_branch)
  
  # --- 7. Tillbaka till default-branchen ---
  kor_git("checkout", default_branch)
  
  cat("\n✓ '", github_repo, "' publicerat till '", target, "'-servern.\n",
      "  GitHub Actions deploy:ar nu till ", server_url, ".\n", sep = "")
  invisible(TRUE)
}


.gh_pat <- function(service = "github_token") {
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(pat)) return(invisible(pat))
  
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("Paketet 'keyring' måste vara installerat.", call. = FALSE)
  }
  
  poster <- keyring::key_list(service = service)
  if (nrow(poster) == 0) {
    stop("Ingen GitHub-token hittades i GITHUB_PAT eller keyring service = '",
         service, "'.\n",
         "Spara en token med keyring::key_set('", service, "') eller ",
         "gitcreds::gitcreds_set().", call. = FALSE)
  }
  
  pat <- keyring::key_get(service, poster$username[1])
  if (!nzchar(pat)) stop("GitHub-token från keyring är tom.", call. = FALSE)
  
  Sys.setenv(GITHUB_PAT = pat)   # så att gert / usethis / gh hittar den automatiskt
  invisible(pat)
}

.gh_push <- function(repo, branch,
                     set_upstream = FALSE,
                     force        = FALSE,
                     ta_bort      = FALSE) {
  
  # Push mot GitHub via gert + paketet 'credentials', som plockar upp
  # GITHUB_PAT ur miljön automatiskt (samma mekanism som webbrapport_publicera).
  # Inga GIT_ASKPASS-skript, inget som stänger av en fungerande credential manager.
  
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("Paketet 'gert' måste vara installerat.", call. = FALSE)
  }
  if (missing(repo) || !nzchar(repo) || !dir.exists(file.path(repo, ".git"))) {
    stop("'repo' måste peka på ett lokalt git-repository: ", repo, call. = FALSE)
  }
  
  .gh_pat()  # ser till att GITHUB_PAT finns i miljön
  
  if (isTRUE(ta_bort)) {
    refspec      <- paste0(":refs/heads/", branch)   # tom källa => radera ref på remote
    set_upstream <- FALSE
  } else {
    refspec <- paste0("refs/heads/", branch)
  }
  
  gert::git_push(
    remote       = "origin",
    refspec      = refspec,
    set_upstream = set_upstream,
    force        = force,
    repo         = repo
  )
  
  invisible(TRUE)
}


.shinyapp_las_publicering_config <- function(repo_sokvag) {
  config_fil <- file.path(repo_sokvag, "_publicering_till_server.yml")
  
  if (!file.exists(config_fil)) {
    warning("Hittar ingen '_publicering_till_server.yml' i ", repo_sokvag,
            " — antar target = 'publik' (default).")
    return("publik")
  }
  
  if (requireNamespace("yaml", quietly = TRUE)) {
    cfg <- yaml::read_yaml(config_fil)
    target <- cfg$target
  } else {
    # Fallback om yaml-paketet saknas: enkel rad-baserad parsning
    lines <- readLines(config_fil, warn = FALSE)
    m <- regmatches(lines, regexec("^\\s*target\\s*:\\s*(\\S+)\\s*$", lines))
    hit <- Filter(function(x) length(x) >= 2, m)
    if (length(hit) == 0) stop("Hittade inte 'target:' i ", config_fil)
    target <- hit[[1]][2]
  }
  
  if (is.null(target) || !target %in% c("publik", "intern")) {
    stop("Felaktigt värde på 'target' i ", config_fil, ": '", target,
         "'\nMåste vara 'publik' eller 'intern'.")
  }
  target
}

shinyapp_avpublicera <- function(
    github_repo,
    target            = NULL,
    lokal_grundsokvag = "c:/gh/",
    github_org        = "Region-Dalarna",
    bekrafta_automatiskt = FALSE                    # TRUE om man kör shinyapp_flytta
) {
  if (missing(github_repo) || !nzchar(github_repo)) {
    stop("github_repo måste anges.")
  }
  
  repo_sokvag <- file.path(lokal_grundsokvag, github_repo)
  if (!dir.exists(repo_sokvag)) {
    stop("Hittar inte repot lokalt: ", repo_sokvag)
  }
  
  if (is.null(target)) {
    target <- .shinyapp_las_publicering_config(repo_sokvag)
  } else if (!target %in% c("publik", "intern")) {
    stop("target måste vara 'publik' eller 'intern', inte '", target, "'.")
  }
  
  server_url <- switch(target,
                       "publik" = "shiny.regiondalarna.se",
                       "intern" = "shiny.ltdalarna.se")
  app_url <- paste0("https://", server_url, "/", github_repo, "/")
  
  # tydlig varning
  # --- Bekräftelse (hoppas över om bekrafta_automatiskt = TRUE) ---
  if (!bekrafta_automatiskt) {
    cat("\n")
    cat("========================================================================\n")
    cat("  VARNING - AVPUBLICERING AV SHINY-APP\n")
    cat("========================================================================\n\n")
    cat("Du är på väg att AVPUBLICERA följande app:\n\n")
    cat("  App:    ", github_repo, "\n", sep = "")
    cat("  Server: ", server_url, " (", target, ")\n", sep = "")
    cat("  URL:    ", app_url, "\n\n", sep = "")
    cat("Detta innebär att:\n")
    cat("  - App-katalogen tas bort från servern\n")
    cat("  - Appen blir otillgänglig på URL:en ovan\n")
    cat("  - Shiny Server startas om\n\n")
    cat("Repot på GitHub PÅVERKAS INTE. All källkod finns kvar och du kan\n")
    cat("publicera appen igen när du vill med:\n\n")
    cat("  shinyapp_publicera(\"", github_repo, "\")\n\n", sep = "")
    cat("========================================================================\n\n")
    cat("Skriv appnamnet (", github_repo, ") för att bekräfta, eller\n", sep = "")
    cat("tryck ENTER för att avbryta:\n")
    
    svar <- readline("> ")
    
    if (trimws(svar) != github_repo) {
      message("Avpublicering avbruten.")
      return(invisible(FALSE))
    }
  }
  
  default_branch <- .shinyapp_default_branch(repo_sokvag)               # kolla om huvud-branchen heter master eller main
  
  message("Triggar avpublicera-workflow på GitHub...")
  .shinyapp_trigga_workflow(
    github_repo      = github_repo,
    workflow_filnamn = "avpublicera.yml",
    inputs           = list(target = target, bekraftelse = github_repo),
    github_org       = github_org,
    ref              = default_branch
  )
  
  message("Workflow triggad. Väntar på att den ska slutföras")
  .shinyapp_vanta_pa_workflow(
    github_repo      = github_repo,
    workflow_filnamn = "avpublicera.yml",
    github_org       = github_org
  )
  
  message("Klart. Appen '", github_repo, "' är avpublicerad från ", server_url, ".")
  message("För att publicera igen: shinyapp_publicera(\"", github_repo, "\")")
  
  invisible(TRUE)
}


# funktion för att flytta en app från intern server till publik, eller tvärtom. 
# Appen publicerar på den server den inte ligger på nu, och tar därefter bort appen från den server den ligger på 
shinyapp_flytta <- function(
    github_repo,
    lokal_grundsokvag = "c:/gh/",
    max_vantetid_s = 600
) {
  if (missing(github_repo) || !nzchar(github_repo)) {
    stop("Parametern 'github_repo' måste anges.")
  }
  
  repo_sokvag <- file.path(lokal_grundsokvag, github_repo)
  if (!dir.exists(repo_sokvag)) stop("Hittar inte repot: ", repo_sokvag)
  if (!dir.exists(file.path(repo_sokvag, ".git"))) {
    stop("Mappen är inte ett git-repository: ", repo_sokvag)
  }
  
  kor_git <- function(..., stopp_vid_fel = TRUE) {
    res <- suppressWarnings(
      system2("git", args = c("-C", repo_sokvag, ...),
              stdout = TRUE, stderr = TRUE)
    )
    status <- attr(res, "status"); if (is.null(status)) status <- 0L
    if (status != 0 && stopp_vid_fel) {
      stop("git ", paste(c(...), collapse = " "),
           " misslyckades:\n", paste(res, collapse = "\n"))
    }
    res
  }
  
  # --- 1. Working tree rent + uppdaterad default-branch ---
  status_output <- kor_git("status", "--porcelain")
  if (length(status_output) > 0 && any(nzchar(status_output))) {
    stop("Repot har ohanterade ändringar:\n",
         paste(status_output, collapse = "\n"),
         "\nCommit:a eller stash:a först innan du kör funktionen.")
  }
  
  default_branch <- .shinyapp_default_branch(repo_sokvag)
  kor_git("checkout", default_branch)
  kor_git("pull", "origin", default_branch)
  
  # --- 2. Bestäm nuvarande target ---
  nuvarande_target <- .shinyapp_las_target(repo_sokvag)
  
  if (is.na(nuvarande_target)) {
    cat("_publicering_till_server.yml saknas eller är ogiltig.\n")
    cat("Försöker detektera vilken server appen ligger på...\n")
    
    cat("  - Pingar publika servern (", 
        .shinyapp_app_url(github_repo, "publik"), ")... ", sep = "")
    pa_publik <- .shinyapp_ping_app(github_repo, "publik")
    cat(if (pa_publik) "✓ svarar\n" else "saknas\n")
    
    cat("  - Pingar interna servern (", 
        .shinyapp_app_url(github_repo, "intern"), ")... ", sep = "")
    pa_intern <- .shinyapp_ping_app(github_repo, "intern")
    cat(if (pa_intern) "✓ svarar\n" else "saknas\n")
    
    if (pa_publik && pa_intern) {
      stop("Appen svarar på BÅDA servrarna. Lös manuellt — kan inte avgöra ",
           "vilken som ska räknas som 'nuvarande'.")
    }
    if (!pa_publik && !pa_intern) {
      stop("Appen verkar inte ligga uppe på någon av servrarna.\n",
           "Använd shinyapp_publicera() för att publicera den först.")
    }
    
    nuvarande_target <- if (pa_publik) "publik" else "intern"
    cat("→ Detekterat: appen ligger på '", nuvarande_target, "'.\n", sep = "")
    cat("Skapar _publicering_till_server.yml och committar till ",
        default_branch, "...\n", sep = "")
    .shinyapp_skriv_target(repo_sokvag, nuvarande_target)
    gert::git_add("_publicering_till_server.yml", repo = repo_sokvag)
    gert::git_commit(
      paste0("Lägg till _publicering_till_server.yml (detekterat: ",
             nuvarande_target, ")"),
      repo = repo_sokvag
    )
    .gh_push(repo_sokvag, default_branch)
  }
  
  nytt_target <- if (nuvarande_target == "publik") "intern" else "publik"
  
  # --- 3. Bekräftelseruta ---
  cat("\n",
      "⚠️  FLYTT AV SHINY-APP\n",
      "─────────────────────────────────────────\n",
      "App:   ", github_repo, "\n",
      "FRÅN:  ", nuvarande_target, " server  (", 
      .shinyapp_app_url(github_repo, nuvarande_target), ")\n",
      "TILL:  ", nytt_target, " server  (", 
      .shinyapp_app_url(github_repo, nytt_target), ")\n",
      "\n",
      "Detta innebär:\n",
      "  1. Appen publiceras på ", nytt_target, " servern\n",
      "  2. Verifiering att den svarar med HTTP 200\n",
      "  3. _publicering_till_server.yml uppdateras till '",
      nytt_target, "' på ", default_branch, "\n",
      "  4. Appen tas bort från den ", nuvarande_target, "a servern\n",
      "─────────────────────────────────────────\n",
      sep = "")
  
  svar <- readline(paste0("Skriv appnamnet ('", github_repo, 
                          "') för att bekräfta: "))
  if (trimws(svar) != github_repo) {
    cat("Bekräftelse matchar inte. Avbryter.\n")
    return(invisible(FALSE))
  }
  
  # --- 4. Publicera till nya servern (explicit target — ignorerar YAML) ---
  cat("\n[1/4] Publicerar till ", nytt_target, " servern...\n", sep = "")
  shinyapp_publicera(github_repo, target = nytt_target,
                     lokal_grundsokvag = lokal_grundsokvag)
  
  # --- 5. Vänta in deploy.yml ---
  cat("\n[2/4] Väntar in deploy.yml på ", nytt_target, " servern...\n", sep = "")
  .shinyapp_vanta_pa_workflow(github_repo, "deploy.yml",
                              max_vantetid_s = max_vantetid_s)
  
  # --- 6. HTTP-ping nya URL:en ---
  cat("\n[3/4] Verifierar att appen svarar på ", nytt_target, " servern...\n",
      sep = "")
  Sys.sleep(3)  # liten paus så Shiny Server hinner ladda om
  if (!.shinyapp_ping_app(github_repo, nytt_target)) {
    stop("Appen svarade INTE med HTTP 200 på ", nytt_target, " servern.\n",
         "URL: ", .shinyapp_app_url(github_repo, nytt_target), "\n\n",
         "Avbryter flytten. Appen ligger nu på BÅDA servrarna men ",
         "_publicering_till_server.yml pekar fortfarande på '",
         nuvarande_target, "'.\n",
         "Felsök manuellt. Om nya servern inte ska användas:\n",
         "  shinyapp_avpublicera('", github_repo, "', target = '",
         nytt_target, "')")
  }
  cat("✓ Appen svarar.\n")
  
  # --- 7. Uppdatera YAML på default_branch ---
  cat("\n[4/4] Uppdaterar _publicering_till_server.yml och tar bort från ",
      nuvarande_target, " servern...\n", sep = "")
  .shinyapp_skriv_target(repo_sokvag, nytt_target)
  gert::git_add("_publicering_till_server.yml", repo = repo_sokvag)
  gert::git_commit(
    paste0("Flytta target: ", nuvarande_target, " -> ", nytt_target),
    repo = repo_sokvag
  )
  .gh_push(repo_sokvag, default_branch)
  
  # --- 8. Avpublicera från gamla servern (utan andra bekräftelse) ---
  shinyapp_avpublicera(github_repo, target = nuvarande_target,
                       lokal_grundsokvag = lokal_grundsokvag,
                       bekrafta_automatiskt = TRUE)
  
  cat("\n✓ Klart! '", github_repo, "' är flyttad från ",
      nuvarande_target, " till ", nytt_target, ".\n", sep = "")
  cat("Ny URL: ", .shinyapp_app_url(github_repo, nytt_target), "\n", sep = "")
  invisible(TRUE)
}


# ---------------------------------------------------------------------------
# Trigga workflow_dispatch via GitHub REST API
# ---------------------------------------------------------------------------
.shinyapp_trigga_workflow <- function(
    github_repo,
    workflow_filnamn,
    inputs       = list(),
    github_org   = "Region-Dalarna",
    ref          = "master"
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Paketet 'httr' måste vara installerat.")
  }
  pat <- .gh_pat()
  
  url <- paste0("https://api.github.com/repos/", github_org, "/", github_repo,
                "/actions/workflows/", workflow_filnamn, "/dispatches")
  
  resp <- httr::POST(
    url,
    httr::add_headers(
      Accept                 = "application/vnd.github+json",
      Authorization          = paste("Bearer", pat),
      `X-GitHub-Api-Version` = "2022-11-28"
    ),
    body   = list(ref = ref, inputs = inputs),
    encode = "json"
  )
  
  if (httr::status_code(resp) != 204) {
    stop("Misslyckades att trigga workflow ", workflow_filnamn,
         ". HTTP ", httr::status_code(resp), ": ",
         httr::content(resp, as = "text", encoding = "UTF-8"))
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Vänta tills senaste körning av ett workflow har slutförts
# ---------------------------------------------------------------------------
.shinyapp_vanta_pa_workflow <- function(
    github_repo,
    workflow_filnamn,
    github_org       = "Region-Dalarna",
    max_vantetid_s   = 600,
    poll_intervall_s = 5
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Paketet 'httr' måste vara installerat.")
  }
  pat <- .gh_pat()
  
  url <- paste0("https://api.github.com/repos/", github_org, "/", github_repo,
                "/actions/workflows/", workflow_filnamn, "/runs?per_page=5")
  
  startad <- Sys.time()
  Sys.sleep(3)   # ge GitHub en kort stund att registrera körningen
  
  repeat {
    resp <- httr::GET(
      url,
      httr::add_headers(
        Accept                 = "application/vnd.github+json",
        Authorization          = paste("Bearer", pat),
        `X-GitHub-Api-Version` = "2022-11-28"
      )
    )
    if (httr::status_code(resp) != 200) {
      stop("Kunde inte läsa workflow-status. HTTP ", httr::status_code(resp))
    }
    data <- httr::content(resp, as = "parsed")
    if (length(data$workflow_runs) == 0) {
      stop("Hittade inga körningar av ", workflow_filnamn)
    }
    senaste <- data$workflow_runs[[1]]
    
    if (isTRUE(senaste$status == "completed")) {
      if (isTRUE(senaste$conclusion == "success")) {
        message("\nWorkflow ", workflow_filnamn, " slutfördes (success).")
        return(invisible(TRUE))
      } else {
        stop("Workflow ", workflow_filnamn, " slutfördes med status '",
             senaste$conclusion, "'. Se: ", senaste$html_url)
      }
    }
    
    if (as.numeric(difftime(Sys.time(), startad, units = "secs")) > max_vantetid_s) {
      stop("Timeout: workflow inte klar efter ", max_vantetid_s, " s.")
    }
    
    cat(".")
    Sys.sleep(poll_intervall_s)
  }
}

.shinyapp_default_branch <- function(repo_sokvag) {
  # Försök 1: git symbolic-ref (snabbast, fungerar om origin/HEAD är satt)
  res <- suppressWarnings(
    system2("git",
            args = c("-C", repo_sokvag,
                     "symbolic-ref", "refs/remotes/origin/HEAD"),
            stdout = TRUE, stderr = TRUE)
  )
  status <- attr(res, "status")
  if (is.null(status) || status == 0) {
    return(sub("^refs/remotes/origin/", "", res[1]))
  }
  
  # Försök 2: fallback — sök efter origin/main eller origin/master
  branches <- suppressWarnings(
    system2("git",
            args = c("-C", repo_sokvag, "branch", "-r"),
            stdout = TRUE, stderr = TRUE)
  )
  branches <- trimws(branches)
  if (any(grepl("^origin/main$",   branches))) return("main")
  if (any(grepl("^origin/master$", branches))) return("master")
  
  stop("Kunde inte hitta default-branch för ", repo_sokvag, ". ",
       "Kör 'git remote set-head origin -a' i repot och försök igen.")
}

.shinyapp_lokal_branch_redo_for_push <- function(repo, branch, bas_branch) {
  # Hjälpare för en lokal branch som finns lokalt men inte på origin (typiskt
  # efter en tidigare körning där pushen dog, t.ex. på autentisering).
  # Returnerar "saknas" om branchen inte finns lokalt, "redo" om den finns och
  # saknar egna commits utöver bas_branch (ofarlig att pusha), och stop():ar om
  # den har egna commits (då vågar vi inte gissa om de ska publiceras).
  g <- function(...) {
    suppressWarnings(
      system2("git", c("-C", repo, ...), stdout = TRUE, stderr = TRUE)
    )
  }
  
  lokala <- trimws(g("branch"))
  finns  <- any(grepl(paste0("^\\*?\\s*", branch, "$"), lokala))
  if (!finns) return("saknas")
  
  egna <- g("log", "--oneline", paste0(bas_branch, "..", branch))
  egna <- egna[nzchar(trimws(egna))]
  if (length(egna) == 0) return("redo")
  
  stop("Branchen '", branch, "' finns lokalt (men inte på origin) och har egna ",
       "commits utöver ", bas_branch, ":\n  ",
       paste(egna, collapse = "\n  "),
       "\nKontrollera manuellt och pusha själv om den är rätt, eller ta bort den:\n",
       "  git branch -D ", branch, call. = FALSE)
}


.shinyapp_konfigurera_publicera_branch <- function(
    github_repo,
    lokal_grundsokvag = "c:/gh/",
    bas_branch        = NULL                   # NULL = auto-detektera
) {
  
  # --- Validera parameter ---
  if (missing(github_repo) || !nzchar(github_repo)) {
    stop("Parametern 'github_repo' måste anges.")
  }
  
  repo_sokvag <- file.path(lokal_grundsokvag, github_repo)
  
  if (!dir.exists(repo_sokvag)) {
    stop("Hittar inte repot: ", repo_sokvag)
  }
  if (!dir.exists(file.path(repo_sokvag, ".git"))) {
    stop("Mappen är inte ett git-repository: ", repo_sokvag)
  }
  
  if (is.null(bas_branch)) {
    bas_branch <- .shinyapp_default_branch(repo_sokvag)
  }
  
  # --- Hjälpfunktion: kör git i rätt repo med felhantering ---
  kor_git <- function(..., stopp_vid_fel = TRUE) {
    git_args <- c("-C", repo_sokvag, ...)
    res <- suppressWarnings(
      system2("git", args = git_args, stdout = TRUE, stderr = TRUE)
    )
    status <- attr(res, "status")
    if (is.null(status)) status <- 0L
    if (status != 0 && stopp_vid_fel) {
      stop("git ", paste(c(...), collapse = " "),
           " misslyckades:\n", paste(res, collapse = "\n"))
    }
    res
  }
  
  cat("\n--- Säkerställer publicera-publik i '",
      github_repo, "' ---\n", sep = "")
  
  # --- 1. Working tree clean ---
  status_output <- kor_git("status", "--porcelain")
  if (length(status_output) > 0 && any(nzchar(status_output))) {
    stop("Repot har ohanterade ändringar:\n",
         paste(status_output, collapse = "\n"),
         "\nCommit:a eller stash:a först innan du kör funktionen.")
  }
  
  # --- 2. Fetch ---
  cat("1. Fetchar från origin...\n")
  kor_git("fetch", "origin")
  
  # --- 3. Vad finns på origin? ---
  remote_branches <- trimws(kor_git("branch", "-r"))
  finns_publicera        <- any(grepl("^origin/publicera$",        remote_branches))
  finns_publicera_publik <- any(grepl("^origin/publicera-publik$", remote_branches))
  finns_bas              <- any(grepl(paste0("^origin/", bas_branch, "$"), remote_branches))
  
  # --- 4. Beslutsträd ---
  
  # 4a. Redan migrerat
  if (finns_publicera_publik && !finns_publicera) {
    message("'publicera-publik' finns redan — ingen åtgärd.")
    return(invisible(FALSE))
  }
  
  # 4b. Konflikt — båda finns
  if (finns_publicera && finns_publicera_publik) {
    stop("Både 'publicera' och 'publicera-publik' finns på origin. ",
         "Lös konflikten manuellt — funktionen vågar inte gissa vilken som är aktuell.")
  }
  
  # 4c. Befintlig rename-väg
  if (finns_publicera) {
    cat("Hittade 'publicera' — döper om till 'publicera-publik'.\n")
    
    cat("2. Checkar ut publicera lokalt...\n")
    local_branches <- trimws(kor_git("branch"))
    finns_lokalt <- any(grepl("^\\*?\\s*publicera$", local_branches))
    if (finns_lokalt) {
      kor_git("checkout", "publicera")
    } else {
      kor_git("checkout", "-b", "publicera", "origin/publicera")
    }
    
    cat("3. Döper om lokalt...\n")
    kor_git("branch", "-m", "publicera", "publicera-publik")
    
    cat("4. Pushar publicera-publik till GitHub...\n")
    #kor_git("push", "-u", "origin", "publicera-publik")
    .gh_push(repo_sokvag, "publicera-publik", set_upstream = TRUE)
    
    cat("5. Tar bort gamla publicera från GitHub...\n")
    #kor_git("push", "origin", "--delete", "publicera")
    .gh_push(repo_sokvag, "publicera", ta_bort = TRUE)
    
    cat("6. Återgår till ", bas_branch, " och städar...\n", sep = "")
    kor_git("checkout", bas_branch)
    kor_git("fetch", "--prune")
    
    cat("\n✓ Klart! '", github_repo,
        "' har nu publicera-publik istället för publicera.\n", sep = "")
    return(invisible(TRUE))
  }
  
  # 4d. NY: Ingen branch finns — skapa från bas_branch
  cat("Ingen 'publicera'- eller 'publicera-publik'-branch finns.\n")
  cat("Skapar 'publicera-publik' från '", bas_branch, "'.\n", sep = "")
  
  if (!finns_bas) {
    stop("Hittar inte basbranchen 'origin/", bas_branch, "'. ",
         "Repot kan vara tomt eller använda annat namn på huvudbranchen ",
         "(t.ex. 'main'). Använd parametern 'bas_branch' för att ange rätt namn.")
  }
  
  cat("2. Checkar ut ", bas_branch, "...\n", sep = "")
  kor_git("checkout", bas_branch)
  kor_git("pull", "--ff-only", "origin", bas_branch)
  
  cat("3. Skapar publicera-publik från ", bas_branch, "...\n", sep = "")
  
  # Om publicera-publik redan finns lokalt (t.ex. från en tidigare körning där
  # pushen dog) återanvänder vi den om den är ofarlig — annars stannar vi.
  status_lokal <- .shinyapp_lokal_branch_redo_for_push(repo_sokvag, "publicera-publik", bas_branch)
  if (status_lokal == "redo") {
    cat("publicera-publik fanns redan lokalt utan egna commits — pushar den befintliga branchen.\n")
    kor_git("checkout", "publicera-publik")
  } else {
    kor_git("checkout", "-b", "publicera-publik")
  }
  
  cat("4. Pushar publicera-publik till GitHub...\n")
  #kor_git("push", "-u", "origin", "publicera-publik")
  .gh_push(repo_sokvag, "publicera-publik", set_upstream = TRUE)
  
  cat("5. Återgår till ", bas_branch, "...\n", sep = "")
  kor_git("checkout", bas_branch)
  kor_git("fetch", "--prune")
  
  cat("\n✓ Klart! '", github_repo,
      "' har nu en ny publicera-publik-branch (skapad från ",
      bas_branch, ").\n", sep = "")
  invisible(TRUE)
}

# === Server-URL och ping ===

.shinyapp_app_url <- function(appnamn, target, anvand_http = FALSE) {
  bas <- switch(target,
                publik = "shiny.regiondalarna.se",
                intern = "shiny.ltdalarna.se",
                stop("Okänt target: ", target)
  )
  protokoll <- if (anvand_http) "http" else "https"
  paste0(protokoll, "://", bas, "/", appnamn, "/")
}

.shinyapp_ping_app <- function(appnamn, target, timeout_s = 10) {
  # Returnerar TRUE om appen svarar med 200, annars FALSE.
  # För 'intern': prova https först, sen http (tills https är konfigurerat).
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Paketet 'httr' krävs.")
  }
  
  prova_http_lista <- if (target == "intern") c(FALSE, TRUE) else c(FALSE)
  
  for (anvand_http in prova_http_lista) {
    url <- .shinyapp_app_url(appnamn, target, anvand_http = anvand_http)
    resp <- tryCatch(
      httr::GET(url, httr::timeout(timeout_s)),
      error = function(e) NULL
    )
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      return(TRUE)
    }
  }
  FALSE
}

# === _publicering_till_server.yml ===

.shinyapp_las_target <- function(repo_sokvag) {
  # Returnerar "publik", "intern" eller NA om filen saknas/är ogiltig.
  yml_path <- file.path(repo_sokvag, "_publicering_till_server.yml")
  if (!file.exists(yml_path)) return(NA_character_)
  
  target <- if (requireNamespace("yaml", quietly = TRUE)) {
    konfig <- tryCatch(yaml::read_yaml(yml_path), error = function(e) NULL)
    konfig$target
  } else {
    rader <- readLines(yml_path, warn = FALSE)
    target_rad <- grep("^\\s*target\\s*:", rader, value = TRUE)
    if (length(target_rad) > 0) {
      trimws(sub("^[^:]*:\\s*", "", target_rad[1]))
    } else NA_character_
  }
  
  if (is.null(target) || is.na(target) || !target %in% c("publik", "intern")) {
    return(NA_character_)
  }
  target
}

.shinyapp_skriv_target <- function(repo_sokvag, target) {
  yml_path <- file.path(repo_sokvag, "_publicering_till_server.yml")
  writeLines(paste0("target: ", target), yml_path)
}

webbrapport_publicera <- function(
    rapport_repo,
    rapport_html_fil   = NULL,
    repo               = "samhallsanalys-rapporter",
    github_org         = "Region-Dalarna",
    from_branch        = "master",
    to_branch          = "publicera",
    remote             = "origin",
    sokvag_lokalt_repo = "c:/gh",
    public_dir         = "public",
    publicera_aven_om_html_fil_aldre_an_rmd_qmd_fil = FALSE
) {
  stopifnot(
    is.character(rapport_repo), length(rapport_repo) == 1, nzchar(rapport_repo),
    is.character(github_org),   length(github_org) == 1,   nzchar(github_org)
  )
  
  # Default: <rapport_repo>.html i rotkatalogen för rapport_repo
  if (is.null(rapport_html_fil) || !nzchar(rapport_html_fil)) {
    rapport_html_fil <- paste0(rapport_repo, ".html")
  }
  
  kalla_repo_dir <- normalizePath(
    file.path(sokvag_lokalt_repo, rapport_repo),
    winslash = "/", mustWork = TRUE
  )
  mal_repo_dir <- normalizePath(
    file.path(sokvag_lokalt_repo, repo),
    winslash = "/", mustWork = TRUE
  )
  
  if (!grepl("\\.html?$", rapport_html_fil, ignore.case = TRUE)) {
    stop("rapport_html_fil måste sluta på .html eller .htm: '", rapport_html_fil, "'")
  }
  
  kalla_html <- normalizePath(
    file.path(kalla_repo_dir, rapport_html_fil),
    winslash = "/", mustWork = FALSE
  )
  
  if (!file.exists(kalla_html)) {
    stop(
      "Hittar inte HTML-filen: ", kalla_html,
      "\nKontrollera att den är renderad och att rapport_html_fil är rätt."
    )
  }
  
  if (!startsWith(paste0(kalla_html, "/"), paste0(kalla_repo_dir, "/"))) {
    stop(
      "rapport_html_fil pekar utanför rapport_repo '", rapport_repo, "'.\n",
      "  Filen: ", kalla_html, "\n",
      "  Repo:  ", kalla_repo_dir, "\n",
      "Filer utanför rapport_repo är inte tillåtna."
    )
  }
  
  # Kontroll: HTML-filen får inte vara äldre än ev. .qmd/.Rmd-källa
  html_stam <- sub("\\.html?$", "", basename(kalla_html), ignore.case = TRUE)
  html_dir  <- dirname(kalla_html)
  kallfil_kandidater <- c(
    file.path(html_dir, paste0(html_stam, ".qmd")),
    file.path(html_dir, paste0(html_stam, ".Rmd")),
    file.path(html_dir, paste0(html_stam, ".rmd"))
  )
  kallfil <- kallfil_kandidater[file.exists(kallfil_kandidater)]
  
  if (length(kallfil) > 0) {
    kallfil_mtime <- max(file.info(kallfil)$mtime)
    html_mtime    <- file.info(kalla_html)$mtime
    if (html_mtime < kallfil_mtime) {
      msg <- paste0(
        "HTML-filen är äldre än källfilen — rendera om innan publicering.\n",
        "  HTML:  ", kalla_html, " (", format(html_mtime), ")\n",
        "  Källa: ", paste(kallfil, collapse = ", "), " (", format(kallfil_mtime), ")"
      )
      if (!isTRUE(publicera_aven_om_html_fil_aldre_an_rmd_qmd_fil)) {
        stop(msg)
      }
      warning(msg, "\nFortsätter eftersom publicera_aven_om_html_fil_aldre_an_rmd_qmd_fil = TRUE.")
    }
  }
  
  mapp_namn <- sub("\\.html?$", "", basename(rapport_html_fil), ignore.case = TRUE)
  
  if (!grepl("^[A-Za-z0-9._-]+$", mapp_namn)) {
    stop(
      "Ogiltigt mappnamn '", mapp_namn, "' — får bara innehålla bokstäver, ",
      "siffror, punkt, understreck och bindestreck."
    )
  }
  
  message("Källa: ", kalla_html)
  message("Mål:   ", repo, "/", public_dir, "/", mapp_namn, "/index.html")
  
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(mal_repo_dir)
  
  # Verifiera att remote pekar mot rätt org/repo
  remotes <- gert::git_remote_list()
  remote_rad <- remotes[remotes$name == remote, , drop = FALSE]
  if (nrow(remote_rad) == 0) {
    stop("Hittar ingen remote med namnet '", remote, "' i ", mal_repo_dir)
  }
  remote_url <- remote_rad$url[1]
  forvantad <- paste0(github_org, "/", repo)
  if (!grepl(forvantad, remote_url, fixed = TRUE)) {
    stop(
      "Remote '", remote, "' pekar inte mot '", forvantad, "'.\n",
      "  Faktisk url: ", remote_url, "\n",
      "Kontrollera github_org/repo eller byt remote."
    )
  }
  
  gert::git_fetch(remote = remote)
  if (gert::git_branch() != from_branch) {
    gert::git_branch_checkout(from_branch)
  }
  suppressMessages(gert::git_pull(remote = remote))
  
  # Skapa mappen om den inte finns, och kopiera HTML-filen som index.html
  mal_dir <- file.path(public_dir, mapp_namn)
  if (!dir.exists(mal_dir)) {
    dir.create(mal_dir, recursive = TRUE)
    message("Skapade ny mapp: ", mal_dir)
  }
  mal_html <- file.path(mal_dir, "index.html")
  file.copy(kalla_html, mal_html, overwrite = TRUE)
  
  # Commit + push till from_branch
  gert::git_add(mal_dir)
  commit_ok <- tryCatch({
    gert::git_commit(paste0("Uppdatera rapport: ", mapp_namn))
    TRUE
  }, error = function(e) {
    if (grepl("No staged files", e$message, fixed = TRUE)) {
      message("Inga ändringar att commita — rapporten är redan up-to-date på '", from_branch, "'.")
      FALSE
    } else {
      stop(e)
    }
  })
  if (isTRUE(commit_ok)) {
    gert::git_push(remote = remote)
    message("Pushade till '", from_branch, "'.")
  }
  
  # Selektiv merge till to_branch
  gert::git_branch_checkout(to_branch)
  suppressMessages(gert::git_pull(remote = remote))
  
  system2("git", c("checkout", from_branch, "--", mal_dir))
  
  gert::git_add(mal_dir)
  publicera_ok <- tryCatch({
    gert::git_commit(paste0("Publicera rapport: ", mapp_namn))
    TRUE
  }, error = function(e) {
    if (grepl("No staged files", e$message, fixed = TRUE)) {
      message("Inga ändringar att publicera — '", to_branch, "' har redan denna version.")
      FALSE
    } else {
      stop(e)
    }
  })
  if (isTRUE(publicera_ok)) {
    gert::git_push(remote = remote)
    message("Pushade till '", to_branch, "' — deploy triggas via GitHub Actions.")
  }
  
  # Tillbaka till from_branch
  gert::git_branch_checkout(from_branch)
  
  invisible(list(
    rapport_repo = rapport_repo,
    rapport_html_fil = rapport_html_fil,
    mapp_namn = mapp_namn,
    kalla = kalla_html,
    mal = mal_html
  ))
}