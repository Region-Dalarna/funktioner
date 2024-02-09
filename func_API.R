# funktioner för att hantera api-anrop via px_web samt möjligen i framtiden även 
# andra paket

if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       rKolada,
       httr)


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


# funktionen används för att dela upp värden i en kolumn i ett antal intervaller (default är 5 st)
# den används främst för att skapa intervaller till skriptet som bearbetar dataset för att göra
# bubbeldiagram för branschindelning per kommun eller län

skapa_intervaller <- function(skickad_kolumn, antal_intervaller = 5){
  
  intervall <- NA              # skapa variabel
  
  max_kol <- max(skickad_kolumn) 
  min_kol <- min(skickad_kolumn)
  range_kol <- max_kol - min_kol
  
  intervall_range <- round(range_kol / (antal_intervaller-1))
  
  # ta fram en siffra som vi avrundar till, ska helst vara 5000 om siffran ligger nära där, 500 om siffran är nära där osv.
  avrundning_num <- round(intervall_range, nchar(intervall_range)*-1)/2
  # om avrundning blir 0 så måste vi öka på den något
  if (avrundning_num == 0) avrundning_num <- round(intervall_range, (nchar(intervall_range)-1)*-1)/2
  
  intervall[1] <- min_kol
  intervall[antal_intervaller] <- max_kol
  
  for (x in 2:(antal_intervaller-1)){
    intervall[x] <- min_kol + (intervall_range * (x-1))
  }
  
  for (x in 1:length(intervall)){
    tal <- round(intervall[x])
    
    intervall[x] <- plyr::round_any(tal, avrundning_num)
    if (intervall[x] == 0) intervall[x] <- avrundning_num/10    # speciallösning, kan kanske fungera för flera fall, om 0 använd avrundning_num / 10
    #intervall[x] <- round(tal, (nchar(tal)-ifelse(nchar(tal)<5,1,2))*-1)
    # lägg till om siffran blir samma som tidigare siffra i vektorn
    if (x > 1){
      if (intervall[x] == intervall[x-1]) intervall[x] <- intervall[x] + round(intervall_range, -2)  
    }
    
    # fixa till om talet är under 1000 och nästa tal är över 1000, sätt till 500 i så fall
    if (x < length(intervall)){
      if (intervall[x] < 1000 & intervall[x+1] > 999) intervall[x] <- 500
    }
  }
  return(intervall)
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

hamta_kod_med_klartext <- function(api_url, klartext_varde, skickad_fran_variabel = NA){
  retur_kod <- sla_upp_varde_klartext_kod(api_url = api_url, 
                                          fran_varde = klartext_varde, 
                                          klartext = TRUE,
                                          fran_variabel = skickad_fran_variabel)
  return(retur_kod)
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
konvertera_till_long_for_contentscode_variabler <- function(skickad_df, api_url, content_var = "variabel", 
                                                            varde_var = "varde") {
  pivot_kol <- hamta_giltiga_varden_fran_tabell(api_url, "contentscode", klartext = TRUE)
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

# en funktion för att sätta ett komma mellan varje element i en vektor
# samt ett och mellan näst sista och sista elementet
list_komma_och <- function(skickad_vektor){
  if (length(skickad_vektor)>1){
    nystrang <- skickad_vektor[1]
    for (elem in 2:length(skickad_vektor)){
      if (elem == length(skickad_vektor)){
        nystrang <- paste0(nystrang, " och ", skickad_vektor[elem])
      } else {
        nystrang <- paste0(nystrang, ", ", skickad_vektor[elem])
      }
    }
  } else nystrang <- skickad_vektor[1]
  return(nystrang)
}

korrigera_kolnamn_supercross <- function(skickad_fil, teckenkodstabell = "latin1"){
  kon_korr <- readLines(skickad_fil, encoding = teckenkodstabell)                          # läs in fil
  if (str_detect(kon_korr[1], "/")) {                                                # kör bara om det finns ett "/" på rad 1, annars låt vara
    kon_korr[1] <- kon_korr[1] %>% 
      str_replace("-/", "_")                                                         # ta bort slash som ställer till det vid inläsning av kolumnnamn
    writeLines(kon_korr, paste0(skickad_fil))                                        # skriv över med rätt tecken
  }
}


ar_alla_kommuner_i_ett_lan <- function(regionkoder, acceptera_lanets_kod = TRUE, acceptera_riket_kod = TRUE) {
  retur_resp <- TRUE
  
  lanskoder <- regionkoder %>% str_sub(1,2) %>% unique() %>% .[!str_detect(., "00")]
  if (length(unique(lanskoder)) > 1) retur_resp <- FALSE else {                  # testa om det finns fler län i skickade regionkoder
  
    kommuner_i_lan <- hamtakommuner(lanskoder, F, F)                             # hämta alla kommuner i aktuellt län
    if (!all(kommuner_i_lan %in% regionkoder)) retur_resp <- FALSE               # testa om alla kommuner i länet finns i regionkoder
    if (lanskoder %in% regionkoder & !acceptera_lanets_kod) retur_resp <- FALSE  # om man inte accepterar länets kod och den finns i regionkoder så returneras FALSE
    if ("00" %in% regionkoder & !acceptera_riket_kod) retur_resp <- FALSE        # om man inte accepterar riket-kod och den finns i regionkoder så returneras FALSE
  
  } # slut if-sats som testar om det finns flera län = returnerar FALSE
  
  return(retur_resp)
  
} # slut funktion

# ================================================= kolada-funktioner ========================================================

hamta_kolada_giltiga_ar <- function(kpi_id, vald_region = "2080"){
  
  hamtade_varden <- get_values(
    kpi = kpi_id,
    municipality = vald_region,
    period = 1900:2060
  )
  
  alla_ar <- hamtade_varden$year %>% as.character()
  return(alla_ar)
}


hamta_kolada_df <- function(kpi_id, valda_kommuner, valda_ar = NA, konsuppdelat = TRUE){
  
  alla_ar <- hamta_kolada_giltiga_ar(kpi_id, valda_kommuner[1])
  senaste_ar <- max(alla_ar)
  start_ar <- min(alla_ar)
  
  alla_giltiga_ar <- valda_ar[valda_ar %in% alla_ar]
  
  hamta_ar <- if (is.na(valda_ar[1])) alla_ar else alla_giltiga_ar
  
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
      retur_df <- retur_df %>% 
        filter(gender == "T")
    } # slut if-sats om könsuppdelat är valt och det finns kvinnor och män i datasetet
    retur_df <- retur_df %>% 
      mutate(gender = case_when(gender == "T" ~ "Båda könen",
                                gender == "K" ~ "Kvinnor",
                                gender == "M" ~ "Män"))
  
  } # slut if-sats om kön finns med som variabel
  
  # gör om år till character
  retur_df <- retur_df %>% 
    mutate(year = year %>% as.character())
  
  return(retur_df)
}

# ====================================================================================================

ladda_funk_parametrar <- function(funktion) {
  
  # funktion för att ladda in alla parametrars standardvärden i global environment
  # OBS! Den skriver över variabler som heter likadant i global environment
  
  st_var <- formals({{funktion}})
  
  for (varname in names(st_var)) {
    assign(varname, st_var[[varname]], envir = .GlobalEnv)
  }
  
} # slut funktion


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
  # returtext      - om man kör TRUE på returnera_text och inte alla reg_koder är från alla kommuner i ett län
  #                     så returneras NA, alternativt returneras returtext om det skickas med. På så sätt kan man
  #                     testa om reg_koder är alla kommuner i ett län och få tillbaka den textsträng man hade från 
  #                     början om det inte är det.
  
  retur_varde <- TRUE                      # vi sätter värdet till TRUE från början, testar nedan och ändrar till FALSE om inte alla kriterier nedan uppfylls
  
  if (length(unique(str_sub(reg_koder[reg_koder != "00"], 1, 2))) > 1) retur_varde <- FALSE else {                      # om det finns flera län eller kommuner från flera län med i reg_koder så blir det FALSE                      
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
      retur_text <- returtext
    }
    
    return(retur_text)         # här returneras text, nytt värde om alla kommuner kommer från ett län, annars NA
    
  } else {                     # om användaren INTE valt att returnera text returneras TRUE/FALSE
    return(retur_varde)  
  }
  
}


skapa_aldersgrupper <- function(alder, aldergrupp_vekt, konv_fran_txt = TRUE) {
  
  # funktion för att enkelt skapa åldersgrupper från ålder som kan användas i en mutate-funktion:
  # mutate(aldersgrupp = skapa_aldersgrupper(alder_var, c(19, 35, 50, 65, 80)))
  #
  # aldergrupp_vekt är en vektor där varje siffra är början på nästa åldersgrupp. 
  # så c(19, 35, 50, 65, 80) ovan blir till åldersgrupperna 0-18 år, 19-34 år, 35-49 år, 50-64 år, 65-79 år samt 80+ år
  
  # Kontrollera och hantera öppna åldersgrupper
  if (!is.infinite(aldergrupp_vekt[[1]])) {
    aldergrupp_vekt <- c(-Inf, aldergrupp_vekt)
  }
  if (!is.infinite(tail(aldergrupp_vekt, n = 1))) {
    aldergrupp_vekt <- c(aldergrupp_vekt, Inf)
  }
  
  # Skapa etiketter för grupperna
  labels <- vector("character", length = length(aldergrupp_vekt) - 1)
  for (i in 1:length(labels)) {
    lower <- aldergrupp_vekt[i]
    upper <- aldergrupp_vekt[i + 1] - 1
    
    if (is.infinite(lower)) {
      labels[i] <- str_c("-", upper, " år")
    } else if (is.infinite(upper + 1)) {
      labels[i] <- str_c(lower, "+ år")
    } else {
      labels[i] <- str_c(lower, "-", upper, " år")
    }
  }
  if (konv_fran_txt & is.character(alder)) alder <- alder %>% readr::parse_number()
  # Dela in åldrarna i grupper
  cut(alder, breaks = aldergrupp_vekt, labels = labels, right = FALSE, include.lowest = TRUE)
}


github_lista_repos <- function(owner = "Region-Dalarna") {
  # En funktion för att lista alla repositories som finns hos en github-användare
  # Användaren "Region-Dalarna" är standardinställing så körs funktionen utan 
  # parametrar så listas alla repos för Region-Dalarna
  
  url <- paste0("https://api.github.com/users/", owner, "/repos")
  response <- httr::GET(url)
  content <- httr::content(response, "parsed")
  
  if (!http_type(response) %in% "application/json") {
    stop("API-förfrågan misslyckades")
  }
  
  tibble::tibble(
    namn = map_chr(content, "name"),
    url = map_chr(content, "html_url")
  )
}

github_lista_repo_filer <- function(owner = "Region-Dalarna", repo = "hamta_data") {
  # En funktion för att lista filer i ett repository som finns hos en github-användare
  # Användaren "Region-Dalarna" är standardinställing och standardinställning för repo
  # är "hamta_data" så körs funktionen utan parametrar så listas alla filer i repot
  # "hamta_data" för github-användaren Region-Dalarna
  
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents")
  response <- httr::GET(url)
  content <- httr::content(response, "parsed")
  
  if (!http_type(response) %in% "application/json") {
    stop("API-förfrågan misslyckades")
  }
  
  tibble::tibble(
    namn = map_chr(content, "name"),
    url = map_chr(content, "download_url")
  ) %>% .[.$namn != ".gitignore",]
}

utskriftsmapp <- function(){ return("G:/Samhällsanalys/API/Fran_R/Utskrift/")}
