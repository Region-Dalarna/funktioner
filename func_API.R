# funktioner för att hantera api-anrop via px_web samt möjligen i framtiden även 
# andra paket 

if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       rKolada,
       httr, 
       glue)


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
  returtext_na <- if (is.na(returtext)) TRUE else FALSE       # om man skickat med returtext så returneras den om inte regionkoderna är alla kommuner i ett län, annars om man inte skickat med någon returtext  returneras FALSE
  
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
  returtext_na <- if (is.na(returtext)) TRUE else FALSE                 # om man skickat med returtext så returneras den om inte regionkoderna är alla län i Sverige , annars om man inte skickat med någon returtext  returneras FALSE
  
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
    url = map_chr(content, "html_url"),
    url_clone = paste0(url, ".git")
  )
}

github_lista_repo_filer <- function(owner = "Region-Dalarna", repo = "hamta_data", url_vekt_enbart = TRUE) {
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
  
  retur_df <- tibble::tibble(
    namn = map_chr(content, "name"),
    url = map_chr(content, "download_url")
  ) %>% .[.$namn != ".gitignore",]
  
  if (url_vekt_enbart) return(retur_df$url) else return(retur_df)
}

# returnera rätt sökväg till vår utskriftsmapp där vi sparar diagram- och kartfiler som inte har någon särskild
# målmapp
utskriftsmapp <- function(){ return("G:/Samhällsanalys/API/Fran_R/Utskrift/")}

mapp_hamtadata_peter <- function(){ return("C:/gh/hamta_data/")}

manader_bearbeta_scbtabeller <- function(skickad_df) {
  # funktion för att skapa kolumnerna år, månad, månad_år samt år_månad av kolumnen månad som 
  # ligger i flera scb-tabeller och är strukturerad som år, bokstaven "M" och sedan månads-
  # nummer med två tecken (nolla framför på årets första 9 månader), alltså "2023M11" för 
  # november 2023.
  
  retur_df <- skickad_df %>% 
    rename(tid = månad) %>% 
    mutate(år = str_sub(tid, 1, 4) %>% as.integer(),
           månad_nr = parse_integer(str_sub(tid, 6,7)),
           månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%B"),
           år_månad = paste0(år, " - ", månad),
           månad_år = paste0(månad, " ", år)) 
  
  manad_sort <- retur_df %>% group_by(månad_nr) %>% summarise(antal = n(), månad_sort = max(månad)) %>% select(månad_sort) %>% dplyr::pull()
  
  retur_df <- retur_df %>% 
    mutate(månad_år = factor(månad_år, levels = unique(månad_år[order(år, månad_nr)])),
           år_månad = factor(år_månad, levels = unique(år_månad[order(år, månad_nr)])),
           år = factor(år),
           månad = factor(månad, levels = manad_sort)) %>%
    select(-månad_nr) %>%                                                           # ta bort sort-kolumnen när vi använt den för att sortera tids-kolumnerna
    relocate(år, .after = tid) %>%                                              # vi sorterar om kolumnerna så att innehållsvariabeler alltid ligger sist
    relocate(månad, .after = år) %>% 
    relocate(år_månad, .after = månad) %>% 
    relocate(månad_år, .after = år_månad)
  return(retur_df)
}


skapa_hamta_data_skript_pxweb_scb <- function(url_scb, 
                                              tabell_namn, 
                                              output_mapp, 
                                              var_med_koder = NA, 
                                              oppna_nya_skriptfilen = TRUE,
                                              skapa_temp_test_fil = TRUE,
                                              skapa_diagram_i_testfil = TRUE
                                              ) {
  
  # funktion för att skapa ett skript för att hämta data från SCB:s pxweb-api
  
  # url = url till api hos scb, t.ex.: "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N", 
  #       men det går bra att skicka med en webb-url också, t.ex.:
  #                                    "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101F/UtlmedbTotNK/"
  # tabell_namn = namn på tabellen, t.ex. "yrke", kommer att ligga först i filnamnet
  # output_mapp = mapp där skriptet ska sparas när det är klart
  # var_med_koder = man kan skicka med variabler som ska få med sin kod i uttaget. Variabeln skrivs med sin kod, t.ex. "yrke2012" för att få ssyk-koder till yrkesvariabeln 
  
  webb_url <- url_scb
  url_scb <- kontrollera_scb_pxweb_url(url_scb)
  
  if (!require("pacman")) install.packages("pacman")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  
  px_meta <- pxweb_get(url_scb)
  
  # Använd en befintlig funktion för att hämta variabellistan från SCB
  tabell_variabler <- pxvarlist(px_meta)
  
  # Skapa en liststruktur för varlistan
  # hämta vektor med variabelkoder
  varlist_koder <- tabell_variabler$koder
  
  # hämta lista med giltiga värden som vektorer för varje variabel
  varlist_giltiga_varden <- map(varlist_koder, ~ pxvardelist(px_meta, .x)$klartext) %>% set_names(tolower(varlist_koder))
  varlist_giltiga_varden_koder <- map(varlist_koder, ~ pxvardelist(px_meta, .x)$kod) %>% set_names(tolower(varlist_koder))
  
  # kolla om det finns åldrar i tabellen och hur många det är i så fall
  if ("alder" %in% names(varlist_giltiga_varden)) alder_txt <- if (length(varlist_giltiga_varden$alder) > 90) "_koder" else "_klartext" else alder_txt <- ""
  
  # Kombinera allt till en dataframe
  varlista_info <- tibble(kod = map_chr(px_meta$variables, ~ .x$code), 
                          namn = map_chr(px_meta$variables, ~ .x$text), 
                          elimination = map_lgl(px_meta$variables, ~ .x$elimination))
  
  # kontrollera hur många contentsvariabler som finns i databasen
  antal_contvar <- length(varlist_giltiga_varden$contentscode)
  
  funktion_parametrar <- map2_chr(varlist_koder, varlist_giltiga_varden, ~ {
    
    ar_elimination <- varlista_info$elimination[varlista_info$kod == .x]            # hämta information om aktuell variabel kan elimineras ur tabellen
    elim_info_txt <- if(ar_elimination) " NA = tas inte med i uttaget, " else ""    # skapa text som används som förklaring vid parametrarna i funktionen
    
    retur_txt <- case_when(str_detect(tolower(.x), "fodel") ~ paste0(tolower(.x), '_klartext = "*",\t\t\t# ', elim_info_txt, ' Finns: ', paste0('"', .y, '"', collapse = ", ")),
                           str_detect(tolower(.x), "region") ~ paste0(tolower(.x), '_vekt = "20",\t\t\t# Val av region.'),
                           tolower(.x) %in% c("tid") ~ paste0(tolower(.x), '_koder = "*",\t\t\t # "*" = alla år eller månader, "9999" = senaste, finns: ', paste0('"', .y, '"', collapse = ", ")),
                           # Funktion för att ta lägsta och högsta värde i ålder är borttagen genom att jag satt length(.y) > 0, ska vara typ kanske 90. Större än 0 = alla så därför är den i praktiken avstängd. 
                           tolower(.x) %in% c("alder") ~ paste0(tolower(.x), alder_txt,' = "*",\t\t\t # ', elim_info_txt, ' Finns: ', paste0('"', .y, '"', collapse = ", ")),                                                 # gammalt: if (length(.y) < 0) paste0(tolower(.x), '_klartext = "*",\t\t\t # ', elim_info_txt, ' Finns: ', paste0('"', .y, '"', collapse = ", ")) else paste0(tolower(.x), '_koder = "*",\t\t\t # Finns: ', min(.y), " - ", max(.y)),
                           TRUE ~ paste0(tolower(.x), '_klartext = "*",\t\t\t # ', elim_info_txt, ' Finns: ', paste0('"', .y, '"', collapse = ", ")) %>% str_replace("contentscode", "cont")) 
    
  }) %>% 
    c(., if (antal_contvar > 1) 'long_format = TRUE,\t\t\t# TRUE = konvertera innehållsvariablerna i datasetet till long-format \n\t\t\twide_om_en_contvar = TRUE,\t\t\t# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel' else "") %>%
    c(., 'output_mapp = NA,\t\t\t# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till', paste0('excel_filnamn = "', tabell_namn, '.xlsx",\t\t\t# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges'), 'returnera_df = TRUE\t\t\t# TRUE om man vill ha en dataframe i retur från funktionen') %>%                     # lägg på output-mapp och excel-filnamn som kommer sist i funktionsparametrarna
    str_c('\t\t\t', ., collapse = "\n") %>% 
    str_remove("\t\t\t\n")
  
  # om inte inte län finns som region, byt ut "20" mot "00" eller "*" i funktion_parametrar
  if ("region" %in% tolower(varlist_koder)){
    if (!"20" %in% varlist_giltiga_varden_koder$region) {
      funktion_parametrar <- str_replace(funktion_parametrar, "region_vekt = \"20\"", "region_vekt = \"00\"")
      if (!"00" %in% varlist_giltiga_varden_koder$region) {
        funktion_parametrar <- str_replace(funktion_parametrar, "region_vekt = \"00\"", "region_vekt = \"*\"")
      }
    }
  } # slut test om Dalarna finns med i tabellen, annars byt ut till riket (00), om inte finns så byt till "*"
  
  # skapa variabel-lista för queryn
  varlist_skriptrader <- paste0("list(\n", 
                                paste(map_chr(varlist_koder, ~paste0("  \"", .x, "\" = ", str_c(tolower(.x), "_vekt"))), collapse = ",\n"), 
                                ")") %>% 
    str_replace("contentscode_vekt", "cont_vekt") %>% 
    str_replace("tid_vekt", "tid_koder")
  
  # skapa skriptrader för klartext-variabler som måste omvandlas till koder till query-listan, dvs. "vekt_" och sedan variabelnamnet
  var_klartext_skriptrader <- map(varlist_koder, function(var_kod) {
    # koda klartext till vekt för variabler som inte innehåller region, alder, tid, contentscode eller som innehåller "fodel"
    if (!str_detect(tolower(var_kod), "region|alder|tid|contentscode") | str_detect(tolower(var_kod), "fodel")) {
      if (px_meta$variables %>%
          keep(~ .x$code == var_kod) %>% 
          map_lgl(~ .x$elimination) %>%
          first()) {
        
          # variabler som går att eliminera (dvs. inte ha med i uttaget)
          paste0("  ", tolower(var_kod), '_vekt <- if (!all(is.na(', tolower(var_kod), '_klartext))) hamta_kod_med_klartext(px_meta, ', tolower(var_kod), '_klartext, skickad_fran_variabel = "', tolower(var_kod), '") else NA\n')
          
      } else {    # variabler som inte går att eliminera (göra uttag utan) men som är klartext till kod
        if (!str_detect(tolower(var_kod), "alder")) paste0("  ", tolower(var_kod), '_vekt <- hamta_kod_med_klartext(px_meta, ', tolower(var_kod), '_klartext, skickad_fran_variabel = "', tolower(var_kod), '")\n')
      }
    } else NA           # om det är koder för region eller ålder så ska de inte med på dessa rader
       
  }) %>% 
    list_c() %>% 
    .[!is.na(.)] %>% 
    str_c(collapse = "")
  
  # om vi har 1-årsgrupper för åldrar så ändrar vi från alder_klartext till alder_koder
  # i skriptraderna som gör om klartextvariabler till koder
  if ("alder" %in% names(varlist_giltiga_varden)) {
    if (length(varlist_giltiga_varden$alder) > 90) {
      var_klartext_alder_skriptrader <- '  alder_vekt <- if (all(!is.na(alder_koder))) alder_koder %>% as.character() %>% ifelse(. == "100", "-100+", .) %>% ifelse(. == "tot", "totalt ålder", .) else NA'
    } else {
      var_klartext_alder_skriptrader <- '  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA'
    }
  } else var_klartext_alder_skriptrader <- ""            # om inte ålder är med i tabellen

  
  # skapa skriptrader för klartext-variabler som kan elimineras om de är NA
  var_klartext_tabort_NA_skriptrader <- map(varlist_koder, function(var_kod) {
    if (px_meta$variables %>%
        keep(~ .x$code == var_kod) %>% 
        map_lgl(~ .x$elimination) %>%
        first()) {
      if (tolower(var_kod) != "region") {  
        paste0('  if (all(is.na(', tolower(var_kod), '_klartext))) varlista <- varlista[names(varlista) != "', var_kod, '"]')
      } else NA
    }  else NA 
  }) %>% 
    list_c() %>% 
    .[!is.na(.)] %>% 
    str_c(collapse = "\n")
  
  # om vi har 1-årsgrupper för åldrar så ändrar vi från alder_klartext till alder_koder i elimineringsraderna
  if ("alder" %in% names(varlist_giltiga_varden)) {
    if (length(varlist_giltiga_varden$alder) > 90) {
      var_klartext_tabort_NA_skriptrader <- str_replace_all(var_klartext_tabort_NA_skriptrader, "alder_klartext", "alder_koder")
    }
  }
  
  filnamn_suffix <- map_chr(varlist_koder, ~ tolower(.x)) %>% .[. != "contentscode"] %>% c(tabell_namn, ., "scb") %>% str_c(collapse = "_")
  
  tid_skriptrader <- paste0('  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")\n',
                            '  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()\n')
  
  cont_skriptrader <- paste0('  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")\n',
                             '  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE\n')
  
  # skapa skript där användaren kan konvertera datasetet till long_format om det finns mer än en innehållsvariabel
  long_format_skriptrader <- if (antal_contvar > 1){
    
    paste0('  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars\n',
           '  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel\n',
           '  if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)\n\n')
      
    } else "" # slut if-sats som kontrollera om vi vill ha df i long-format, blir "" om vi inte har fler än en cont_variabler i tabellen
  
  
  variabler_med_kod <- varlist_koder[str_detect(tolower(varlist_koder), "region") & !str_detect(tolower(varlist_koder), "fodel")]
  
  if (!all(is.na(var_med_koder))) {
    #var_med_koder <- var_med_koder %>% paste0('"', ., '"', collapse = ", ")   
    if (any(var_med_koder %in% varlist_koder)) variabler_med_kod <- c(variabler_med_kod, var_med_koder[var_med_koder %in% varlist_koder]) %>% unique()
  } 
  if (length(variabler_med_kod) > 0) {
    names(variabler_med_kod) <- paste0(tolower(variabler_med_kod), "koder")
    variabler_med_klartext <- tabell_variabler$klartext[match(variabler_med_kod, tabell_variabler$koder)]
    var_vektor_skriptdel <- glue(
    '  var_vektor <- ', capture.output(dput(variabler_med_kod))%>% paste0(collapse = ""), '\n',
    '  var_vektor_klartext <- ', capture.output(dput(variabler_med_klartext)) %>% paste0(collapse = ""), '\n',
    )
  } else {                                    # om det inte finns någon kolumn som vi vill ta med koder för
    var_vektor_skriptdel <- glue(
      '  var_vektor <- NA\n',
      '  var_vektor_klartext <- NA\n',
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
  
  # Skapa en sträng med skriptet för att hämta data med pxweb
  query_code <- paste0(
    'hamta_', filnamn_suffix, ' <- function(\n',
    funktion_parametrar, '\n',
    '){\n\n',
    paste0(c("  # ", rep("=", times = 100)), collapse = ""),
    "\n",
    "  #\n",
    "  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet\n",
    "  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna\n",
    "  #\n",
    "  # Skapad av: ", Sys.info()["user"], " den ", format(Sys.Date(), "%d %B %Y"), "\n",
    "  # Senast uppdaterad: ", format(Sys.Date(), "%d %B %Y"), "\n",
    "  #\n",
    "  # url till tabellens API: ", webb_url, "\n",
    "  #\n",
    paste0(c("  # ", rep("=", times = 100)), collapse = ""),
    '\n\n',
    'if (!require("pacman")) install.packages("pacman")\n',
    '  p_load(pxweb,\n',
    '    \t\t\ttidyverse,\n',
    '    \t\t\twritexl)\n\n',
    '  # Behändiga funktioner som används i skriptet\n',
    '  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")\n\n',
    '  # Url till SCB:s databas\n',
    '  url_uttag <- "', url_scb, '"\n',
    '  px_meta <- pxweb_get(url_uttag)\n\n',
    '  varlist_koder <- pxvarlist(px_meta)$koder\n',
    '  varlist_bada <- pxvarlist(px_meta)\n\n',
    '  # Gör om från klartext till kod som databasen förstår\n',
    var_klartext_skriptrader, '\n',
    var_klartext_alder_skriptrader, '\n',
    cont_skriptrader, '\n',
    '  # Hantera tid-koder\n',
    tid_skriptrader, '\n',
    #variabler_med_kod_skriptrader, '\n',
    '# query-lista till pxweb-uttag\n',
    '  varlista <- ', varlist_skriptrader, '\n\n',
    var_klartext_tabort_NA_skriptrader, '\n\n',
    #variabler_med_kod_skriptrader, '\n\n',
    '  px_uttag <- pxweb_get(url = url_uttag, query = varlista)\n\n',
    var_vektor_skriptdel, '\n',
    '  px_df <- as.data.frame(px_uttag)\n',
    '  if (!is.na(var_vektor)) {',
    '      px_df <- px_df %>%\n',
    '            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%\n',
    '            select(any_of(var_vektor)))\n\n',
    '      px_df <- map2(names(var_vektor), var_vektor_klartext, ~ px_df %>% relocate(all_of(.x), .before = all_of(.y))) %>% list_rbind()\n',
    '  }\n\n',
    long_format_skriptrader, '\n',
    '  # Om användaren vill spara data till en Excel-fil\n',
    '  if (!is.na(output_mapp) & !is.na(excel_filnamn)){\n',
    '    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))\n',
    '  }\n\n',
    '  # Returnera data som en dataframe om användern valt det\n',
    '  if (returnera_df) return(px_df)\n\n',
    '}'
  )
  
  # Skriv ut den genererade koden - om man vill kolla att skriptet verkar stämma
  #cat(query_code)
  
  tabell_id <- url_scb %>% str_extract("/[^/]+$") %>% str_sub(2)
  
  # Alternativt, om du vill skapa en skriptfil istället:
  writeLines(query_code, paste0(output_mapp, "hamta_", filnamn_suffix, "_", tabell_id, ".R"))
  
  # Öppna filen i RStudio om användaren inte valt bort det
  if (oppna_nya_skriptfilen)file.edit(paste0(output_mapp, "hamta_", filnamn_suffix, ".R"))
  
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
    tid_txt <- if("tid" %in% names(varlist_giltiga_varden)) {
      tid_varnamn <- varlista_info$namn[tolower(varlista_info$kod) == "tid"]
      paste0(' ', tid_varnamn, ' {min(', tabell_namn, '_df$', tid_varnamn, ')} - {max(', tabell_namn, '_df$', tid_varnamn, ')}')
    } else ""
    tid_filnamn_txt <- if("tid" %in% names(varlist_giltiga_varden)) paste0('_ar{min(', tabell_namn, '_df$', tid_varnamn, ')}_{max(', tabell_namn, '_df$', tid_varnamn, ')}')
      
    testfil_skript <- glue('if (!require("pacman")) install.packages("pacman")\n',
                           'p_load(tidyverse,\n',
                           '   \t\t\tglue)\n\n',
                           'source("', paste0(output_mapp, "hamta_", filnamn_suffix, ".R"), '")\n',
                           'source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")\n\n',
                           'diagram_capt <- "Källa: SCB:s öppna statistikdatabas\\nBearbetning: Samhällsanalys, Region Dalarna"\n',
                           'output_mapp <- utskriftsmapp()\n',
                           'visa_dataetiketter <- FALSE\n',
                           'gg_list <- list()\n\n',
                           '{tabell_namn}_df <- hamta_{filnamn_suffix}(\n',
                           '{funktion_parametrar}\n\n)')
    
    testfil_diagram <- glue('\n\ndiagramtitel <- glue("', auto_diag_titel, '{region_txt}{tid_txt}")\n',
                           'diagramfil <- glue("{tabell_namn}_{regionkod_txt}{tid_filnamn_txt}.png")\n\n',
                           'if ("variabel" %in% names({tabell_namn}_df)) {{\n',
                           '   if (length(unique({tabell_namn}_df$variabel)) > 6) chart_df <- {tabell_namn}_df %>% filter(variabel == unique({tabell_namn}_df$variabel)[1]) else chart_df <- {tabell_namn}_df\n',
                           '}} else chart_df <- {tabell_namn}_df\n\n',
                           'gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,\n',
                           '\t\t\t skickad_x_var = "', tid_varnamn, '",\n',
                           '\t\t\t skickad_y_var = if ("varde" %in% names(chart_df)) "varde" else "', varlist_giltiga_varden$contentscode[1], '",\n',
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

kontrollera_scb_pxweb_url <- function(url_scb) {
  # Kontrollera att url:en är en giltig pxweb-url - om det är en webb-url från SCB:s öppna statstikdatabas på webben 
  # så konverterar vi den till en API-url, annars returnerar vi den som den är
  if (str_detect(url_scb, "https://www.statistikdatabasen.scb.se/")) {
    
    start_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/"
    # här extraherar vi den del av url:en som är unik för varje tabell och som ska byggas ihop med start_url:en
    retur_url <- url_scb %>% 
      str_remove("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START") %>%
      str_split("__") %>% unlist() %>% .[. != ""] %>% 
      str_split("/") %>% unlist() %>% .[. != ""] %>% 
      str_c(., collapse = "/") %>% 
      str_c(start_url, .) #%>% str_sub(., 1, nchar(.)-1)
    
    return(retur_url)
  } else {
    return(url_scb)
  }
}

