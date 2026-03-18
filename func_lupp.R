if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       data.table,
       readxl,
       glue,
       stringi,
       svDialogs
       )

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)


lupp_skapa_svarssort_fraga <- function(frage_svarsvektor, 
                                       meddelande = TRUE,
                                       lupp_svarssortering_df,
                                       ej_besvarat_fragan_position_sist = FALSE
                                       ){
  
  # vi hanterar om det finns en kategori som är "Har inte besvarat frågan
  if (any(str_detect(frage_svarsvektor, "Har inte besvarat frågan"))) {
    frage_svarsvektor_justerad <- frage_svarsvektor %>% .[. != "Har inte besvarat frågan"]
    ej_besvarat_fragan_finns <- TRUE
  } else {
    frage_svarsvektor_justerad <- frage_svarsvektor
    ej_besvarat_fragan_finns <- FALSE
  }
  
  sorterad_vektor <- lupp_svarssortering_df %>%
    filter(map_lgl(values, ~ all(frage_svarsvektor_justerad %in% .x))) %>%
    slice(1) %>% # Ta den första matchande raden
    dplyr::pull(values) %>% 
    unlist()
  
  # om det finns en kategori för ej besvarat frågan, lägg till den i svarsvektorn
  if (ej_besvarat_fragan_finns) {
    if (ej_besvarat_fragan_position_sist) {
      sorterad_vektor <- c(sorterad_vektor, "Har inte besvarat frågan")
    } else {
      sorterad_vektor <- c("Har inte besvarat frågan", sorterad_vektor)
    } # slut if-sats som avgör utifrån parametern ej_besvarat_fragan_position_sist om "Har inte besvarat frågan" läggs sist eller först i vektorn
  } # slut if-sats om det finns en ej besvarat frågan-kategori
  
  retur_vektor <- frage_svarsvektor[order(match(frage_svarsvektor, sorterad_vektor))]
  
  if (all(is.null(sorterad_vektor))) { 
    if (meddelande) message("Skickade svarsalternativ finns inte i svarssorteringsvektorn.")
    return(frage_svarsvektor)
  } else {
    return(retur_vektor)
  }
}

lupp_skapa_svarsgrupp_text <- function(svarsvektor) {
  
  # Dela varje element i vektorn i enskilda ord
  splittade_ord <- str_split(svarsvektor, "\\s+")
  
  # Hitta ord som förekommer i alla element
  gemensamma_ord <- Reduce(intersect, splittade_ord)
  
  # Rensa gemensamma ord från alla element utom det sista
  rensad_vektor <- map_chr(seq_along(svarsvektor), ~ {
    if (.x < length(svarsvektor)) {
      # Ta bort gemensamma ord
      str_remove_all(svarsvektor[.x], paste0("\\b(", paste(gemensamma_ord, collapse = "|"), ")\\b"))
    } else {
      # Behåll sista elementet oförändrat
      svarsvektor[.x]
    }
  }) %>%
    str_squish() # Ta bort extra mellanslag
  
  retur_vektor <- rensad_vektor %>% 
    list_komma_eller() %>%      # sätt ihop till en textsträng, med komma mellan varje element utom mellan de två sista där vi sätter in ett "eller"
    str_to_sentence()           # stor bokstav i första ordets första bokstav enbart
  
  return(retur_vektor)
}


lupp_skapa_sortering_faktorvariabel <- function(variabel_1, variabel_2 = NULL, lupp_dataset) {
  

  if (variabel_1 == "Kön_alla") variabel_1 <- "Kön"
  if (!is.null(variabel_2)) {
    if (variabel_2 == "Kön_alla") variabel_2 <- "Kön"
  }
  if (is.null(variabel_2)) {
    
    sortering_levels <- expand.grid(
      var_1 = levels(lupp_dataset[[variabel_1]])
    ) %>%
      arrange(var_1) %>%
      mutate(Grupp = str_c(var_1, sep = " ")) %>%
      dplyr::pull(Grupp)  
    } else {
  
    sortering_levels <- expand.grid(
      var_1 = levels(lupp_dataset[[variabel_1]]),
      var_2 = levels(lupp_dataset[[variabel_2]])
    ) %>%
      arrange(var_1) %>%
      mutate(Grupp = str_c(var_1, var_2, sep = " ")) %>%
      dplyr::pull(Grupp)
    }
  return(sortering_levels)
} # slut funktion

lupp_valj_fragor_dialogruta <- function(
    lupp_dataset,
    lupp_fragenyckel_df
  ) {
  
  #if(!exists("lupp_dataset") || is.null(lupp_dataset)) lupp_dataset <- lupp_ladda_dataset()
  #if(!exists("lupp_fragenyckel_df") || is.null(lupp_fragenyckel_df)) lupp_fragenyckel_df <- lupp_ladda_fragenyckel()
  
  vald_fraga_nyckel <- dlg_list(lupp_dataset$Fraga %>% unique(), "Välj fråga", multiple = TRUE)$res
  vald_fraga_id <- lupp_dataset %>% filter(Fraga %in% vald_fraga_nyckel) %>% distinct(ID) %>% dplyr::pull()
  vald_fraga <- lupp_fragenyckel_df %>% filter(ID %in% vald_fraga_id)
  return(vald_fraga)
  
}


lupp_ladda_fargvektor <- function(kategori = "kon",
                                  lupp_dataset) {
  

  # Funktion för att skapa färgvektor dynamiskt baserat på unika värden och färgintervall
  returnera_kategorivektor <- function(kategori_namn, 
                                       kategorier_farger, 
                                       befintlig_vektor = fargvektor_retur) {
    
    variabelnamn <- kategori_namn %>% str_remove("_alla")        # för att möjliggöra att lägga på "_alla" på variabler om man vill ha olika versioner (tex. med kön)
    valda_variabler <- names(kategorier_farger)
    
    vektor_retur <- c(befintlig_vektor, 
                          setNames(
                            list(
                              valda_variabler %>% 
                                map(~ setNames(
                                  colorRampPalette(kategorier_farger[[.x]])(length(unique(lupp_dataset$År))) %>% rev(),
                                  paste0(.x, ".", unique(lupp_dataset$År))
                                )) %>%
                                flatten_dfc() %>%
                                unlist()
                            ),
                            kategori_namn
                          ))
  }
  
  fargvektor_retur <- list()

  # ============================== kön ==============================================
  
  # definiera färger per unikt värde i kön
  Kön <- list(
    Tjej = c("#E2A855", "#FFF5CC"),
    Kille = c("#459079", "#D5EAE6")
  )
  
  fargvektor_retur <- returnera_kategorivektor(kategori_namn = "Kön",
                                               kategorier_farger = Kön)
  
  # ============================== kön - alla ==============================================
  
  Kön_alla <- list(
    Tjej = c("#E2A855", "#FFF5CC"),
    Kille = c("#459079", "#D5EAE6"),
    "Annan könstillhörighet" = c("#0074A2", "#B6F0FD")
  )
  
  fargvektor_retur <- returnera_kategorivektor(kategori_namn = "Kön_alla", 
                                               kategorier_farger = Kön_alla)
  
  # ============================== socioekonomi ============================================
  
  Socioekonomi <- list(
    "Resurssvaga hushåll" = c("#AE2D3A", "#FFDDE2"),
    "Övriga hushåll" = c("#0074A2", "#B6F0FD")
  )
  
  fargvektor_retur <- returnera_kategorivektor(kategori_namn = "Socioekonomi", 
                                               kategorier_farger = Socioekonomi)
  
  # ============================== födelseland ============================================
  
  Födelseland <- list(
    "Utrikes född" = c("#0e5a4c", "#93cec1"),
    "Inrikes född" = c("#0074A2", "#B6F0FD")
  )
  
  fargvektor_retur <- returnera_kategorivektor(kategori_namn = "Födelseland", 
                                               kategorier_farger = Födelseland)
  
  
  # ================================= alla =================================================
  
  Alla <- list(
    "Alla" = diagramfarger("rus_sex")
  )
  
  fargvektor_retur <- returnera_kategorivektor(kategori_namn = "Alla",
                                               kategorier_farger = Alla)
  
  # =================== avslut med att lägga ihop alla kategorier i en lista ===============
  # här skapar vi en returlista som innehåller både färgvektorn och färgvariablerna
  fargvektor_retur_lista <- map2(fargvektor_retur, names(fargvektor_retur), ~ {
    retur_lista <- list(
      fargvektor = .x,
      fargkategorier = names(get(.y))
    )
    retur_lista  # Returnera resultatet
  })
  
  return(fargvektor_retur_lista)
  #} # slut if-sats med test om fargvektor-listan redan finns
  
}

hamta_lupp_frageurval <- function(valt_urval = "alla_diagram",
                                  lupp_fragenyckel_df) {
 
  # alla_diagram
  alla_diagram <- lupp_fragenyckel_df %>% filter(Fragetyp %in% c(1, 2, 3)) %>% dplyr::pull(ID) %>% unique()
  
  # fråge-id  
  tor_avesta_dalarna <- c("E0102", "E0103", "E0100", "E0101", "E0200", "E0201", "E0206", "E0202", "E0205", "E0203", "E0204", "E0207", "E0208", "F0602", "F0603", "F0600", "F0601", "F0800", "F1000", "E0104", "E0105", "E0106", "E0107")
  rus_uppfoljning <- c("D0600")
  radda_barnen <- c("A110", "A046", "A045", "D0600", "D0700", "D1100", "D1200", "E0300", "E0401", "E0403", "E0402", "E0410", "E0404", "E0408", "E0409", "E0407", "E0405", "E0406", "E0400", "E0800", "F0202", "F0201", "H1400", "A040", "E0411")
  
  return(get(valt_urval))
}


lupp_skriv_ut_diagram <- function(frage_id = NA, 
                                  bakgrundsvariabel = "Kön",                # NA om man vill att alla är med,  bakgrundsvariabel <- "Kön_alla"    # tar med även "Annan könsidentitet
                                  undersokning = list(c("Högstadiet", "Gymnasiet")),           # NA om man vill att alla är med, varje element (som kan innehålla flera undersökningar), skrivs ut i ett eget diagram
                                  kommun = "Dalarna",                  # kan vara "Dalarna", "Falun", "Avesta" etc. En eller flera. Ett diagram per kommun
                                  kommuntyp = NA,                              # "Skolkommun" eller "Boendekommun" - om inte angivet så hämtas det från frågenyckeln
                                  output_mapp = NA,
                                  bortfall_per_fraga_ta_bort = TRUE,           # beräkna andelar enbart på de som har besvarat en fråga
                                  diagramtitel_egen = NA,                      # om NA så blir det automatisk titel från frågenyckeln, annars den som skickas med
                                  undertitel_diagram_tabort = FALSE,           # TRUE så blir det ingen undertitel där det står om det är boend- eller skolkommun, och vilken
                                  svarsalternativ_alla_skriv_ut = FALSE,       # TRUE om man vill skriva ut alla svaraslaternativ för de frågor som ska skrivas ut
                                  grans_for_0_till_100_diagram_typ2 = 60,      # har datasetet värden över detta så blir det ett 0-100 procents-diagram
                                  diagram_capt = "Källa: Lupp-undersökningarna i Dalarna\nBearbetning: Samhällsanalys, Region Dalarna",
                                  diagram_capt_skickad = NA,                   # special för att skicka med en egen caption
                                  etiketter_x_axel_langd = NA,                 # om NA används värdet i frågenyckeln, annars kan man styra det med denna parameter (antal tecken) 
                                  logga_i_diagram = TRUE,                      # TRUE ha med logga, FALSE ta inte med logga
                                  diagram_titel_storlek = 20,
                                  x_axis_storlek = 10.5,
                                  y_axis_storlek = 12,
                                  diagram_caption_storlek = 11,
                                  legend_storlek = 12, 
                                  etikett_inst = FALSE,
                                  skriv_diagram = TRUE,
                                  skriv_excelfil = FALSE,
                                  spara_ggplot_objekt = FALSE,
                                  felsok = FALSE
) {
  
  if (!skriv_diagram && !skriv_excelfil && !spara_ggplot_objekt) {
    cat('Någon av parametrarna "skriv_diagram", "skriv_excelfil" eller "spara_ggplot_objekt" måste vara TRUE för att funktionen ska köras, för annars returneras ingenting.')
    stop_tyst()
  } 
  
  if (all(is.na(output_mapp))) {
    if (dir.exists(utskriftsmapp())) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  if (all(is.na(frage_id))) frage_id <- lupp_valj_fragor_dialogruta(lupp_dataset_df, lupp_fragenyckel_df)$ID          # välj diagram
  
  # =========================== bearbetningar ====================================
  
  
  valda_fragor <- lupp_fragenyckel_df %>%
    filter(ID %in% frage_id,
           !str_detect(ID, "9999"))
  
  if (!svarsalternativ_alla_skriv_ut) {
    valda_fragor <- valda_fragor %>% 
      mutate(ID = if_else(Fragetyp == 2, str_sub(ID, 1, 3), ID)) %>%      # frågetyp 2 behåller vi bara första fyra id-tecknen
      distinct(ID, .keep_all = TRUE)  # och tar bort dubletter (för att skriva ut ett diagram men alla alternativ i samma grupp kommer med)
  }
  
  if (nrow(valda_fragor) == 0) {
    cat("Valda ID finns inte i frågenyckeln. Kontrollera uppgifterna och försök igen.")
    stop_tyst()
  }
  
  gg_list <- list()
  
  skapa_lupp_diagram <- function(valda_diagramfragor,
                                 vald_fraga_id,
                                 vald_bakgrundsvariabel, 
                                 vald_undersokning,
                                 vald_kommun,
                                 vald_kommuntyp
  ) {
    
    if (felsok) cat(glue("Fråge-id {vald_fraga_id} hanteras."), "\n") # om man vill felsöka
    
    vald_fraga <- lupp_fragenyckel_df %>% 
      filter(str_sub(ID, 1, nchar(as.character(vald_fraga_id))) %in% vald_fraga_id) 
    
    if (svarsalternativ_alla_skriv_ut) {
      fraga_fragetyp <- 3
    } else {
      fraga_fragetyp <- vald_fraga %>% distinct(Fragetyp) %>% dplyr::pull()  
    }
    
    
    # ================ frågetyp 1 - sammanslagna svarsalternativ ===============
    if (fraga_fragetyp == 1) {
      #browser()
      vald_bakgrund_varden <- lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargkategorier
      filter_vald_bakgrundsvariabel <- vald_bakgrundsvariabel %>% str_remove("_alla")
      
      # välj boendekommun eller skolkommun - om det inte är medskickat så hämtas det från frågenyckeln 
      diagram_kommuntyp <- if (is.na(vald_kommuntyp)) vald_fraga %>% dplyr::pull(Kommun_typ) else vald_kommuntyp 
      
      # hämta svarsalternativ som används för denna fråga
      fraga_svargrp <- vald_fraga %>% 
        dplyr::pull(gruppering_vektor) %>% 
        unlist() %>% 
        lupp_skapa_svarssort_fraga(meddelande = FALSE, lupp_svarssortering_df = lupp_svarssortering_df)
      
      # Titel på diagrammet hämtas här
      fraga_svargrp_txt <- lupp_skapa_svarsgrupp_text(fraga_svargrp)
      
      # Sortering av Bakgrundsvariabler
      if (is.na(vald_bakgrundsvariabel)) {
        factor_var_sort <- lupp_skapa_sortering_faktorvariabel("Undersökning", lupp_dataset_df = lupp_dataset_df)
      } else {
        factor_var_sort <- lupp_skapa_sortering_faktorvariabel("Undersökning", filter_vald_bakgrundsvariabel, lupp_dataset_df)
      }
      
      diagram_df <- lupp_dataset_df %>% 
        filter(str_sub(ID, 1, nchar(vald_fraga$ID)) %in% vald_fraga$ID) %>% 
        filter(Svar != "_bortfall_" | is.na(Svar))
      
      
      # sortera ut rätt kommun
      if (vald_kommun != "Dalarna") {
        if (!"Boendekommun" %in% names(diagram_df)) {
          diagram_df <- diagram_df %>% 
            filter(Kommun %in% vald_kommun)
        } else {
          diagram_df <- diagram_df %>% 
            filter(!!sym(diagram_kommuntyp) %in% vald_kommun)
        }
      }
      
      # anpassa utifrån om och i så fall vilken bakgrundsvariabel som är vald
      if (!all(is.na(vald_bakgrundsvariabel))){
        # filtrera ut enbart de värden som vi vill ta med i redovisningen
        diagram_df <- diagram_df %>% 
          filter(!!sym(filter_vald_bakgrundsvariabel) %in% vald_bakgrund_varden) %>%
          mutate(Grupp = str_c(Undersökning, !!sym(filter_vald_bakgrundsvariabel), sep = " ") %>% str_to_sentence())
        # ta bara med värden som vi använder för vald bakgrundsvariabel i factor_var_sort
        factor_var_sort <- factor_var_sort[str_detect(factor_var_sort, regex(paste(vald_bakgrund_varden, collapse = "|"), ignore_case = TRUE))]
        bakgrund_txt <- vald_bakgrundsvariabel
      } else {
        diagram_df <- diagram_df %>%
          mutate(Grupp = Undersökning %>% str_to_sentence())
        bakgrund_txt <- ""
      } 
      
      # anpassa utifrån om och i så fall vilken undersökning som är vald
      if (!all(is.na(vald_undersokning))) {
        # ta bara med de undersökningar som är valda
        diagram_df <- diagram_df %>% 
          filter(Undersökning %in% vald_undersokning)
        # ta bort undersökningar ur sorteringsvektorn som inte kommer vara med i diagrammen
        factor_var_sort <- factor_var_sort[str_detect(factor_var_sort, regex(paste(vald_undersokning, collapse = "|"), ignore_case = TRUE))]
        undersokning_txt <- vald_undersokning %>% str_c(collapse = "_") %>% svenska_tecken_byt_ut()
      } else undersokning_txt <- ""
      
      # gör en faktorvariabel av kolumnen Grupp
      diagram_df <- diagram_df %>% 
        mutate(Grupp = factor(Grupp, levels = factor_var_sort %>% str_to_sentence()),
               Svargrupp = ifelse(Svar %in% fraga_svargrp, fraga_svargrp_txt, "Övriga")) 
      
      # här tar vi bort NA-värden ur datasetet så att de inte kommer med i andelarna (om man valt TRUE på bortfall_per_fraga_ta_bort)
      if (bortfall_per_fraga_ta_bort) {
        diagram_df <- diagram_df %>%
          filter(!is.na(Svar) & Svar != "")
      }
      
      # gruppera och räkna ut andel per Svargrupp
      diagram_df <- diagram_df %>%
        group_by(År, ID, Fraga, Grupp, Svargrupp) %>% 
        summarise(antal = sum(antal, na.rm = TRUE)) %>% 
        mutate(andel = antal / sum(antal) * 100) %>% 
        ungroup() %>% 
        filter(Svargrupp %in% fraga_svargrp_txt)
      
      # ibland finns inga uppgifter för valt urval (ex. vissa frågor finns bara för anpassad, högstadiet och gymnasiet)
      if (nrow(diagram_df) > 0) {
        
        fraga_txt <- lupp_fragenyckel_df %>% 
          filter(ID %in% diagram_df$ID) %>% 
          dplyr::pull(Titel)
        
        fraga_filnamn <- lupp_fragenyckel_df %>% 
          filter(ID %in% diagram_df$ID) %>% 
          dplyr::pull(fragefilnamn)
        
        # diagramundertitel beroende av om man bor eller går i skolan i vald kommun
        if (diagram_kommuntyp == "Boendekommun") {
          undertitel_txt <- glue("- boende i {vald_kommun}")
          kommuntyp_txt <- "bo"
        } else {
          undertitel_txt <- glue("- går i skolan i {vald_kommun}")
          kommuntyp_txt <- "skola"
        } 
        
        # hantera undertitlar som läggs till för att visa att frågan rör en delpopulation av de som svarat vissa specifika
        # svar på annan fråga (t.ex. alkohol, narkotika etc.)
        undertitel_tillagg_txt <- lupp_fragenyckel_df %>%
          filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id) %>% 
          dplyr::pull(Undertitel) %>%
          unique()
        
        if (!is.na(undertitel_tillagg_txt)) undertitel_txt <- paste(undertitel_txt, undertitel_tillagg_txt)
        
        # anpassa färgvektor utifrån om det finns bakgrundsvariabel eller inte  
        if(is.na(vald_bakgrundsvariabel)) {
          skickad_fargvektor <- NA
          skickade_fargvariabler <- NA
        } else {
          skickad_fargvektor <- lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargvektor 
          skickade_fargvariabler <- setNames(list(lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargkategorier),vald_bakgrundsvariabel) 
        }
        
        diagram_titel <- fraga_txt
        diagramfil <- paste0(fraga_filnamn, "_", bakgrund_txt, "_", undersokning_txt, "_", kommuntyp_txt, "_", vald_kommun, ".png")
        
        gg_obj <- SkapaStapelDiagram(skickad_df = diagram_df,  
                                     skickad_x_var = "Grupp", #if(is.na(vald_bakgrundsvariabel)) "Grupp" else "År",
                                     skickad_y_var = "andel",
                                     skickad_x_grupp = "År", # if(is.na(vald_bakgrundsvariabel)) "År" else "Grupp",
                                     skickad_namngiven_fargvektor = skickad_fargvektor,
                                     farg_variabler = skickade_fargvariabler,
                                     legend_kategorier_tabort_xgrupp = TRUE,
                                     legend_rader = if(is.na(vald_bakgrundsvariabel)) NULL else length(vald_bakgrund_varden),
                                     diagram_titel = if(is.na(diagramtitel_egen)) diagram_titel else diagramtitel_egen,
                                     diagram_undertitel = if (undertitel_diagram_tabort) NULL else undertitel_txt,
                                     diagram_capt = if (is.na(diagram_capt_skickad)) diagram_capt else diagram_capt_skickad,
                                     stodlinjer_avrunda_fem = TRUE,
                                     dataetiketter = etikett_inst,
                                     lagg_pa_logga = logga_i_diagram,
                                     diagram_titel_storlek = diagram_titel_storlek,
                                     x_axis_storlek = x_axis_storlek,
                                     y_axis_storlek = y_axis_storlek,
                                     diagram_caption_storlek = diagram_caption_storlek,
                                     legend_storlek = legend_storlek,
                                     x_axis_lutning = 0,
                                     manual_y_axis_title = "procent",
                                     facet_legend_bottom = TRUE,
                                     procent_0_100_10intervaller = TRUE,
                                     manual_color = diagramfarger("rus_sex"),
                                     skriv_till_diagramfil = skriv_diagram,
                                     output_mapp = output_mapp,
                                     filnamn_diagram = diagramfil)
        
        # om man valt att spara ggplot-objekt
        if (spara_ggplot_objekt) {
          gg_list <- c(gg_list, list(gg_obj))
          names(gg_list)[length(gg_list)] <- diagramfil %>% str_remove(".png")
        }
        
        # om man valt att spara excelfil
        if (skriv_excelfil) {
          diagram_df <- diagram_df %>% 
            mutate(Kommunurval = diagram_kommuntyp,
                   Kommun = vald_kommun) %>% 
            select(År, Grupp, Kommun, Kommunurval, ID, Fraga, Svarsalternativ = Svargrupp, antal, andel)
          
          gg_list <- c(gg_list, list(diagram_df))
          names(gg_list)[length(gg_list)] <- "xlsx" #diagramfil %>% str_remove(".png") %>% paste0(., "_xlsx")
          
        } # slut test skriv_excelfil
      } # slut test om det finns några rader i aktuellt urval
    } # slut fragetyp 1
    
    # ================ frågetyp 2 - ihopslagna frågor svarsalternativ ===============
    
    if (fraga_fragetyp == 2) {
      
      vald_bakgrund_varden <- lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargkategorier
      filter_vald_bakgrundsvariabel <- vald_bakgrundsvariabel %>% str_remove("_alla")
      
      # välj boendekommun eller skolkommun - om det inte är medskickat så hämtas det från frågenyckeln 
      diagram_kommuntyp <- if (is.na(vald_kommuntyp)) {
        vald_fraga %>% 
          dplyr::pull(Kommun_typ) %>% 
          unique() %>% 
          .[!is.na(.)]
      }  else vald_kommuntyp 
      
      # hämta svarsalternativ som används för denna fråga
      fraga_svargrp <- vald_fraga %>% 
        dplyr::pull(gruppering_vektor) %>% 
        unlist() %>%
        unique() %>% 
        lupp_skapa_svarssort_fraga(meddelande = FALSE, lupp_svarssortering_df = lupp_svarssortering_df)
      
      # Titel på diagrammet hämtas här
      fraga_svargrp_txt <- lupp_skapa_svarsgrupp_text(fraga_svargrp)
      
      # hämta vektor med grupper som ska tas bort när det summeras
      fraga_minusgrp <- vald_fraga %>% 
        dplyr::pull(gruppering_minus_vektor) %>% 
        unlist() %>% 
        unique()
      
      
      # Sortering av Bakgrundsvariabler
      if (is.na(vald_bakgrundsvariabel)) {
        factor_var_sort <- lupp_skapa_sortering_faktorvariabel("Undersökning", lupp_dataset_df = lupp_dataset_df)
      } else {
        factor_var_sort <- lupp_skapa_sortering_faktorvariabel("Undersökning", filter_vald_bakgrundsvariabel, lupp_dataset_df)
      }
      
      # hämta alla rader med de frågor med svarsalternativ som är valda
      diagram_df <- lupp_dataset_df %>% 
        filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id)
      
      # sortera ut rätt kommun
      if (vald_kommun != "Dalarna") {
        if (!"Boendekommun" %in% names(diagram_df)) {
          diagram_df <- diagram_df %>% 
            filter(Kommun %in% vald_kommun)
        } else {
          diagram_df <- diagram_df %>% 
            filter(!!sym(diagram_kommuntyp) %in% vald_kommun)
        }
      }
      
      ihopfragetyp_join <- lupp_fragenyckel_df %>%
        filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id) %>% 
        select(ID, Titel, Titel_slaihopfragor)
      
      # anpassa utifrån om och i så fall vilken bakgrundsvariabel som är vald
      if (!all(is.na(vald_bakgrundsvariabel))){
        # filtrera ut enbart de värden som vi vill ta med i redovisningen
        diagram_df <- diagram_df %>% 
          filter(!!sym(filter_vald_bakgrundsvariabel) %in% vald_bakgrund_varden) %>%
          mutate(Grupp = str_c(Undersökning, !!sym(filter_vald_bakgrundsvariabel), sep = " ") %>% str_to_sentence())
        # ta bara med värden som vi använder för vald bakgrundsvariabel i factor_var_sort
        factor_var_sort <- factor_var_sort[str_detect(factor_var_sort, regex(paste(vald_bakgrund_varden, collapse = "|"), ignore_case = TRUE))]
        bakgrund_txt <- vald_bakgrundsvariabel
      } else {
        diagram_df <- diagram_df %>%
          mutate(Grupp = Undersökning %>% str_to_sentence())
        bakgrund_txt <- ""
      } 
      
      if (!all(is.na(vald_undersokning))) {
        # ta bara med de undersökningar som är valda
        diagram_df <- diagram_df %>% 
          filter(Undersökning %in% vald_undersokning)
        # ta bort undersökningar ur sorteringsvektorn som inte kommer vara med i diagrammen
        factor_var_sort <- factor_var_sort[str_detect(factor_var_sort, regex(paste(vald_undersokning, collapse = "|"), ignore_case = TRUE))]
        undersokning_txt <- vald_undersokning %>% str_c(collapse = "_") %>% svenska_tecken_byt_ut()
      } else undersokning_txt <- ""
      
      diagram_df <- diagram_df %>% 
        left_join(ihopfragetyp_join, by = "ID") %>% 
        mutate(Grupp = factor(Grupp, levels = factor_var_sort %>% str_to_sentence()),
               Svargrupp = ifelse(Svar %in% fraga_svargrp, fraga_svargrp_txt, "Övriga"),
               antal_ejminus = ifelse(!Svar %in% fraga_minusgrp, antal, 0)) %>%
        group_by(År, ID, Fraga = Titel_slaihopfragor, Grupp, Svarsalternativ = Titel, Svargrupp) %>% 
        summarise(antal = sum(antal, na.rm = TRUE),
                  antal_sum = sum(antal_ejminus, na.rm = TRUE)) %>% 
        mutate(andel = antal / sum(antal_sum) * 100) %>% 
        ungroup() %>% 
        filter(Svargrupp %in% fraga_svargrp_txt)
      
      # byt ut tjej mot tjej men med kyrilliskt "j" så att inte staplar får gul färg för att svarsalternativet 
      # innehåller ordet "tjej"
      if (!is.na(vald_bakgrundsvariabel) & vald_bakgrundsvariabel == "Kön" ) {
        alla_svarsalternativ <- unique(diagram_df$Svarsalternativ)
        #utbytes_text_gemen <- vald_bakgrund_varden[map_lgl(vald_bakgrund_varden, ~ any(str_detect(alla_svarsalternativ, .x)))]
        #utbytes_text_versal <- vald_bakgrund_varden[map_lgl(vald_bakgrund_varden, ~ any(str_detect(alla_svarsalternativ, tolower(.x))))] %>% tolower()
        #utbytes_text <- c(utbytes_text_gemen, utbytes_text_versal)
        utbytes_text <- vald_bakgrund_varden[map_lgl(vald_bakgrund_varden, ~ any(str_detect(alla_svarsalternativ, tolower(.x))))] %>% tolower()
        
        if (length(utbytes_text) > 0) {
          # funktion för att byta tecken 
          ersatt_homoglyph <- function(ord) {
            ord |>
              stringr::str_replace_all(c(
                "i" = "\u0456",   # i → kyrilliska і
                "j" = "\u0458"    # j → kyrilliska ј
              ))
          }
          for (byt_txt in utbytes_text) {
            ersattnings_text <- ersatt_homoglyph(byt_txt)
            
            diagram_df <- diagram_df %>% mutate(Svarsalternativ = ifelse(str_detect(Svarsalternativ, byt_txt), str_replace_all(Svarsalternativ, byt_txt, ersattnings_text), Svarsalternativ))
          }
        } # slut test om det finns ord som måste bytas ut
      } # slut if-sats där vi kollar om "tjej" eller "kille" finns som svarsalternativ
      
      # ibland finns inga uppgifter för valt urval (ex. vissa frågor finns bara för anpassad, högstadiet och gymnasiet)
      if (nrow(diagram_df) > 0) {
        
        fraga_txt <- lupp_fragenyckel_df %>%
          filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id) %>% 
          dplyr::pull(Titel_slaihopfragor) %>%
          unique()
        
        fraga_filnamn <- lupp_fragenyckel_df %>% 
          filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id) %>% 
          dplyr::pull(fragefilnamn_grp) %>% 
          unique()
        
        # om man skickat med ett värde för hur långa etiketterna för denna fråga ska vara så används det,
        # annars används värdet som ligger i frågenyckeln
        if (is.na(etiketter_x_axel_langd)) {
          etiketter_x_axel_langd <- lupp_fragenyckel_df %>% 
            filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id) %>% 
            dplyr::pull(etikettlangd_x_axel) %>% 
            unique() %>% 
            max()                  # om det finns flera olika värden tas det högsta värdet
        } else etiketter_x_axel_langd
        
        diagram_df <- diagram_df %>% 
          mutate(Svarsalternativ = Svarsalternativ %>% str_wrap(etiketter_x_axel_langd)) %>% 
          complete(Grupp, År, Svarsalternativ, fill = list(antal = NA, andel = NA))
        
        # diagramundertitel beroende av om man bor eller går i skolan i vald kommun
        if (diagram_kommuntyp == "Boendekommun") {
          undertitel_txt <- glue("- boende i {vald_kommun}")
          kommuntyp_txt <- "bo"
        } else {
          undertitel_txt <- glue("- går i skolan i {vald_kommun}")
          kommuntyp_txt <- "skola"
        }
        
        # hantera undertitlar som läggs till för att visa att frågan rör en delpopulation av de som svarat vissa specifika
        # svar på annan fråga (t.ex. alkohol, narkotika etc.)
        undertitel_tillagg_txt <- lupp_fragenyckel_df %>%
          filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id) %>% 
          dplyr::pull(Undertitel) %>%
          unique()
        
        if (!is.na(undertitel_tillagg_txt)) undertitel_txt <- paste(undertitel_txt, undertitel_tillagg_txt)
        
        # anpassa färgvektor utifrån om det finns bakgrundsvariabel eller inte  
        if(is.na(vald_bakgrundsvariabel)) {
          skickad_fargvektor <- NA
          skickade_fargvariabler <- NA
        } else {
          skickad_fargvektor <- lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargvektor 
          skickade_fargvariabler <- setNames(list(lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargkategorier),vald_bakgrundsvariabel) 
        }
        
        
        diagram_titel <- fraga_txt
        diagramfil <- paste0(fraga_filnamn, "_", bakgrund_txt, "_", undersokning_txt, "_", kommuntyp_txt, "_", vald_kommun, ".png")
        
        gg_obj <- SkapaStapelDiagram(skickad_df = diagram_df,  
                                     skickad_x_var = "Svarsalternativ",
                                     skickad_y_var = "andel",
                                     skickad_x_grupp = "År",
                                     skickad_namngiven_fargvektor = skickad_fargvektor,
                                     farg_variabler = skickade_fargvariabler,
                                     legend_kategorier_tabort_xgrupp = TRUE,
                                     legend_rader = if(is.na(vald_bakgrundsvariabel)) NULL else length(vald_bakgrund_varden),
                                     diagram_titel = if(is.na(diagramtitel_egen)) diagram_titel else diagramtitel_egen,
                                     diagram_undertitel = undertitel_txt,
                                     diagram_capt = if (is.na(diagram_capt_skickad)) diagram_capt else diagram_capt_skickad,
                                     #diagram_liggande = TRUE,
                                     stodlinjer_avrunda_fem = TRUE,
                                     dataetiketter = etikett_inst,
                                     lagg_pa_logga = logga_i_diagram,
                                     diagram_titel_storlek = diagram_titel_storlek,
                                     x_axis_storlek = x_axis_storlek,
                                     y_axis_storlek = y_axis_storlek,
                                     diagram_caption_storlek = diagram_caption_storlek,
                                     legend_storlek = legend_storlek,
                                     x_axis_lutning = 45,
                                     manual_x_axis_text_hjust = 1,
                                     manual_x_axis_text_vjust = 1,
                                     manual_y_axis_title = "procent",
                                     facet_x_axis_storlek = 6,
                                     facet_legend_bottom = TRUE,
                                     procent_0_100_10intervaller = if (max(diagram_df$andel, na.rm = TRUE) >= grans_for_0_till_100_diagram_typ2) TRUE else FALSE,
                                     manual_color = diagramfarger("rus_sex"),
                                     diagram_facet = TRUE,
                                     facet_grp = "Grupp",
                                     facet_scale = "free_x",
                                     #facet_sort = TRUE,
                                     skriv_till_diagramfil = skriv_diagram,
                                     output_mapp = output_mapp,
                                     filnamn_diagram = diagramfil)
        
        # om man valt att spara ggplot-objekt
        if (spara_ggplot_objekt) {
          gg_list <- c(gg_list, list(gg_obj))
          names(gg_list)[length(gg_list)] <- diagramfil %>% str_remove(".png") # %>% paste0(., "_ggobj")
        }
        
        # om man valt att spara Excelfil
        if (skriv_excelfil) {
          
          diagram_df <- diagram_df %>% 
            mutate(Kommunurval = diagram_kommuntyp,
                   Kommun = vald_kommun) %>% 
            select(År, Grupp, Kommun, Kommunurval, ID, Fraga, Svarsalternativ, antal, andel)
          
          gg_list <- c(gg_list, list(diagram_df))
          names(gg_list)[length(gg_list)] <- "xlsx" #diagramfil %>% str_remove(".png") %>% paste0(., "_xlsx")
          
        } # slut test skriv_excelfil
      } # slut test om det finns några rader i aktuellt urval
    } # slut fragetyp 2
    
    
    # ================ frågetyp 3 - samtliga svarsalternativ ===============
    if (fraga_fragetyp == 3) {
      
      vald_bakgrund_varden <- lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargkategorier
      filter_vald_bakgrundsvariabel <- vald_bakgrundsvariabel %>% str_remove("_alla")
      
      # välj boendekommun eller skolkommun - om det inte är medskickat så hämtas det från frågenyckeln 
      diagram_kommuntyp <- if (is.na(vald_kommuntyp)) vald_fraga %>% dplyr::pull(Kommun_typ) else vald_kommuntyp 
      
      # # # hämta svarsalternativ som används för denna fråga
      fraga_svargrp <- vald_fraga %>%
        dplyr::pull(Titel_allaSvar) %>%
        unlist() %>%
        lupp_skapa_svarssort_fraga(meddelande = FALSE, lupp_svarssortering_df = lupp_svarssortering_df)
      
      # # Titel på diagrammet hämtas här
      fraga_svargrp_txt <- lupp_skapa_svarsgrupp_text(fraga_svargrp)
      
      # Sortering av Bakgrundsvariabler
      if (is.na(vald_bakgrundsvariabel)) {
        factor_var_sort <- lupp_skapa_sortering_faktorvariabel("Undersökning", lupp_dataset_df = lupp_dataset_df)
      } else {
        factor_var_sort <- lupp_skapa_sortering_faktorvariabel("Undersökning", vald_bakgrundsvariabel, lupp_dataset_df)
      }
      
      diagram_df <- lupp_dataset_df %>% 
        filter(str_sub(ID, 1, nchar(vald_fraga$ID)) %in% vald_fraga$ID) %>% 
        mutate(Svar = if_else(Svar == "", "Har inte besvarat frågan", Svar)) %>% 
        filter(Svar != "_bortfall_" | is.na(Svar))
      
      # # hämta svarsalternativ som används för denna fråga
      fraga_svargrp <- unique(diagram_df$Svar) %>% 
        lupp_skapa_svarssort_fraga(meddelande = FALSE, lupp_svarssortering_df = lupp_svarssortering_df)
      
      # sortera ut rätt kommun
      if (vald_kommun != "Dalarna") {
        if (!"Boendekommun" %in% names(diagram_df)) {
          diagram_df <- diagram_df %>% 
            filter(Kommun %in% vald_kommun)
        } else {
          diagram_df <- diagram_df %>% 
            filter(!!sym(diagram_kommuntyp) %in% vald_kommun)
        }
      }
      
      # if (!all(is.na(vald_bakgrundsvariabel))){
      #   # filtrera ut enbart de värden som vi vill ta med i redovisningen
      #   diagram_df <- diagram_df %>% 
      #     filter(!!sym(filter_vald_bakgrundsvariabel) %in% vald_bakgrund_varden)
      #   # ta bara med värden som vi använder för vald bakgrundsvariabel i factor_var_sort
      #   factor_var_sort <- factor_var_sort[str_detect(factor_var_sort, regex(paste(vald_bakgrund_varden, collapse = "|"), ignore_case = TRUE))]
      #   bakgrund_txt <- vald_bakgrundsvariabel
      # } else bakgrund_txt <- ""
      
      # anpassa utifrån om och i så fall vilken bakgrundsvariabel som är vald
      if (!all(is.na(vald_bakgrundsvariabel))){
        # filtrera ut enbart de värden som vi vill ta med i redovisningen
        diagram_df <- diagram_df %>% 
          filter(!!sym(filter_vald_bakgrundsvariabel) %in% vald_bakgrund_varden) %>%
          mutate(Grupp = str_c(Undersökning, !!sym(vald_bakgrundsvariabel), sep = " ") %>% str_to_sentence())
        # ta bara med värden som vi använder för vald bakgrundsvariabel i factor_var_sort
        factor_var_sort <- factor_var_sort[str_detect(factor_var_sort, regex(paste(vald_bakgrund_varden, collapse = "|"), ignore_case = TRUE))]
        bakgrund_txt <- vald_bakgrundsvariabel
      } else {
        diagram_df <- diagram_df %>%
          mutate(Grupp = Undersökning %>% str_to_sentence())
        bakgrund_txt <- ""
      } 
      
      if (!all(is.na(vald_undersokning))) {
        # ta bara med de undersökningar som är valda
        diagram_df <- diagram_df %>% 
          filter(Undersökning %in% vald_undersokning)
        # ta bort undersökningar ur sorteringsvektorn som inte kommer vara med i diagrammen
        factor_var_sort <- factor_var_sort[str_detect(factor_var_sort, regex(paste(vald_undersokning, collapse = "|"), ignore_case = TRUE))]
        undersokning_txt <- vald_undersokning %>% str_c(collapse = "_") %>% svenska_tecken_byt_ut()
      } else undersokning_txt <- ""
      
      diagram_df <- diagram_df %>% 
        mutate(Grupp = factor(Grupp, levels = factor_var_sort %>% str_to_sentence()),
               Svargrupp = Svar) %>%
        group_by(År, ID, Fraga, Grupp, Svargrupp) %>% 
        summarise(antal = sum(antal, na.rm = TRUE)) %>% 
        mutate(andel = antal / sum(antal) * 100) %>% 
        filter(!(Svargrupp == "" & andel == 100)) %>% 
        ungroup()
      
      # om man skickat med ett värde för hur långa etiketterna för denna fråga ska vara så används det,
      # annars används värdet som ligger i frågenyckeln
      if (is.na(etiketter_x_axel_langd)) {
        etiketter_x_axel_langd <- lupp_fragenyckel_df %>% 
          filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id) %>% 
          dplyr::pull(etikettlangd_x_axel) %>% 
          unique()
      }
      
      diagram_df <- diagram_df %>% 
        mutate(Svargrupp = factor(Svargrupp, levels = fraga_svargrp),
               #Svargrupp = Svargrupp %>% str_wrap(etiketter_x_axel_langd)
        ) %>% 
        complete(Grupp, År, Svargrupp, fill = list(antal = NA, andel = NA)) %>% # lägg till rader som inte finns pga saknade värden så att alla staplar blir lika breda
        group_by(Grupp) %>%                     # Gruppera efter Grupp
        filter(!all(is.na(andel))) %>%          # Behåll grupper där inte alla andel är NA
        ungroup()   
      
      # ibland finns inga uppgifter för valt urval (ex. vissa frågor finns bara för anpassad, högstadiet och gymnasiet)
      if (nrow(diagram_df) > 0) {
        
        fraga_txt <- lupp_fragenyckel_df %>% 
          filter(ID %in% diagram_df$ID) %>% 
          dplyr::pull(Titel_allaSvar)
        
        fraga_filnamn <- lupp_fragenyckel_df %>% 
          filter(ID %in% diagram_df$ID) %>% 
          dplyr::pull(fragefilnamn)
        
        # diagramundertitel beroende av om man bor eller går i skolan i vald kommun
        if (diagram_kommuntyp == "Boendekommun") {
          undertitel_txt <- glue("- boende i {vald_kommun}")
          kommuntyp_txt <- "bo"
        } else {
          undertitel_txt <- glue("- går i skolan i {vald_kommun}")
          kommuntyp_txt <- "skola"
        } 
        
        # hantera undertitlar som läggs till för att visa att frågan rör en delpopulation av de som svarat vissa specifika
        # svar på annan fråga (t.ex. alkohol, narkotika etc.)
        undertitel_tillagg_txt <- lupp_fragenyckel_df %>%
          filter(str_sub(ID, 1, nchar(vald_fraga_id)) %in% vald_fraga_id) %>% 
          dplyr::pull(Undertitel) %>%
          unique()
        
        if (!is.na(undertitel_tillagg_txt)) undertitel_txt <- paste(undertitel_txt, undertitel_tillagg_txt)
        
        # anpassa färgvektor utifrån om det finns bakgrundsvariabel eller inte  
        if(is.na(vald_bakgrundsvariabel)) {
          skickad_fargvektor <- NA
          skickade_fargvariabler <- NA
        } else {
          skickad_fargvektor <- lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargvektor 
          skickade_fargvariabler <- setNames(list(lupp_fargvektorer_list[[vald_bakgrundsvariabel]]$fargkategorier),vald_bakgrundsvariabel) 
        }
        
        diagram_titel <- fraga_txt
        diagramfil <- paste0(fraga_filnamn, "_", bakgrund_txt, "_", undersokning_txt, "_", kommuntyp_txt, "_", vald_kommun, "_alla.png")
        
        gg_obj <- SkapaStapelDiagram(skickad_df = diagram_df,  
                                     skickad_x_var = "Svargrupp",
                                     skickad_y_var = "andel",
                                     skickad_x_grupp = "År",
                                     skickad_namngiven_fargvektor = skickad_fargvektor,
                                     farg_variabler = skickade_fargvariabler,
                                     diagram_titel = if(is.na(diagramtitel_egen)) diagram_titel else diagramtitel_egen,
                                     diagram_undertitel = undertitel_txt,
                                     diagram_capt = if (is.na(diagram_capt_skickad)) diagram_capt else diagram_capt_skickad,
                                     stodlinjer_avrunda_fem = TRUE,
                                     dataetiketter = etikett_inst,
                                     lagg_pa_logga = logga_i_diagram,
                                     diagram_titel_storlek = diagram_titel_storlek,
                                     x_axis_storlek = x_axis_storlek,
                                     y_axis_storlek = y_axis_storlek,
                                     diagram_caption_storlek = diagram_caption_storlek,
                                     legend_storlek = legend_storlek,
                                     x_axis_lutning = 45,
                                     manual_x_axis_text_hjust = 1,
                                     manual_x_axis_text_vjust = 1,
                                     manual_y_axis_title = "procent",
                                     diagram_facet = TRUE,
                                     facet_grp = "Grupp",
                                     facet_legend_bottom = TRUE,
                                     legend_rader = if (length(unique(diagram_df$År)) == 1) 1 else 2,
                                     legend_kategorier_tabort_xgrupp = TRUE,
                                     procent_0_100_10intervaller = TRUE,
                                     manual_color = diagramfarger("rus_sex"),
                                     skriv_till_diagramfil = skriv_diagram,
                                     output_mapp = output_mapp,
                                     filnamn_diagram = diagramfil)
        
        # om man valt att spara ggplot-objekt
        if (spara_ggplot_objekt) {
          gg_list <- c(gg_list, list(gg_obj))
          names(gg_list)[length(gg_list)] <- diagramfil %>% str_remove(".png")
        }
        
        # om man valt att spara excelfil
        if (skriv_excelfil) {
          
          diagram_df <- diagram_df %>% 
            mutate(Kommunurval = diagram_kommuntyp,
                   Kommun = vald_kommun,
                   Svargrupp = Svargrupp %>% str_replace_all("\n", " ") %>% str_squish()) %>% 
            select(År, Grupp, Kommun, Kommunurval, ID, Fraga, Svarsalternativ = Svargrupp, antal, andel)
          
          gg_list <- c(gg_list, list(diagram_df))
          names(gg_list)[length(gg_list)] <- "xlsx" #diagramfil %>% str_remove(".png") %>% paste0(., "_xlsx")
          
        } # slut test skriv_excelfil
      } # slut test om det finns några rader i aktuellt urval
    } # slut fragetyp 3
    
    
    if (length(gg_list) > 0) return(gg_list)
  } # slut skapa_lupp_diagram
  
  arglist <- list(fraga_ID = unique(valda_fragor$ID), 
                  fraga_bakgrundsvariabel = unique(bakgrundsvariabel), 
                  fraga_undersokning = unique(undersokning),
                  fraga_kommun = unique(kommun),
                  fraga_kommuntyp = unique(kommuntyp))                               # skapa lista med de två variabler vi vill göra diagram med
  crossarg <- expand_grid(!!!arglist)
  
  dia_lista <- pmap(crossarg, ~ skapa_lupp_diagram(valda_diagramfragor = valda_fragor, 
                                                   vald_fraga_id = ..1,
                                                   vald_bakgrundsvariabel = ..2, 
                                                   vald_undersokning = ..3, 
                                                   vald_kommun = ..4,
                                                   vald_kommuntyp = ..5)) %>% purrr::flatten()
  
  if (spara_ggplot_objekt | skriv_excelfil) return(dia_lista)
  
} # slut funktion

