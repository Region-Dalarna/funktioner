if (!require("RColorBrewer")) install.packages("RColorBrewer")


# ================================== funktion för att hämta sökväg till logga som läggs in i diagram =============================
hamta_logga_path <- function(){
  # sökväg till logga för att kunna lägga in den i diagrammen
  tf <- "https://raw.githubusercontent.com/Region-Dalarna/depot/main/rd_logo_liggande_fri_svart.png"
  return(tf)
}           

nDigits <- function(x) nchar( trunc( abs(x) ) )    # funktion för att räkna antal siffror i heltal

avrunda_till_multipel <- function(n, multipel_in = 5) {

# Denna funktion ser till att avrunda det tal som skickas in till närmaste tal som slutar på multipel (vi kör fem i diagrammen)
# Alltså, 73 avrundas till 75, 88743 avrundas till 90000 osv. För att få snyggare y-axel
  
  # här beräknar vi hur stor siffra vi ska multiplicera med
  multiple_diff <- nDigits(n) - nDigits(multipel_in)-1
  multipel <- (10 ^ multiple_diff) * multipel_in
  
  # här gör vi själva avrundningen
  resultat <- floor((n + multipel/2) / multipel) * multipel
  resultat <- as.numeric(format(resultat, scientific = FALSE))
  
  kor_igen <- TRUE
  
  # här ser vi till att maxvärdet på axeln alltid är större (för positiva tal)
  # än maxvärdet i skickad df, tvärtom för negativa tal
  while(kor_igen == TRUE){
    if (n>0) {    # om det är ett positivt tal
      if (resultat>n) kor_igen <- FALSE else resultat <- resultat + multipel
    } else {      # om det är ett negativt tal
      if (resultat<n) kor_igen <- FALSE else resultat <- resultat - multipel
    } 
  }

  # ny metod:
  retur_div <- hitta_div_for_jamn_intervall(max_varde = resultat, multipel = multipel)
  resultat <- retur_div$resultat        # om nytt max-värde från funktionen hitta_div..., hämtar vi det här från retur-listan
  slut_div <- retur_div$slut_div        # här får vi värdet att dividera med från retur-listan
  maj_by_var <- resultat / slut_div     # beräkna intervall mellan tjocka stödlinjer
  min_by_yvar <- maj_by_var / 5         # beräkna intervall mellan tunna stödlinjer
  
  # test_div <- c(6, 5, 4, 7)    # testa hur många maj_by_yvar som funkar med maxvärdet (=blir ett heltal om man divider maxvärdet med antalen i vektorn, i den ordning de står prövas de)
  # for (i in 1:length(test_div)){
  #   if (resultat %% test_div[i] == 0) {
  #     maj_by_var <- resultat / test_div[i]
  #     min_by_yvar <- maj_by_var / 5
  #     slut_div <- test_div[i]
  #     break
  #   }
  # }
  
  # om vi har för mycket y-axel ovanför maxvärdet så kan vi ta ned den ett steg
  if (n>0) {    # om det är ett positivt tal
    ny_resultat <- if(maj_by_var*(slut_div-1)>n) resultat-maj_by_var else resultat
  } else {      # om det är ett negativt tal
    ny_resultat <- if(maj_by_var*(slut_div-1)<n) resultat+maj_by_var else resultat
  } 
  
  retur_list <- list(max_varde = ny_resultat,
                     maj_by_var = maj_by_var,
                     min_by_var = maj_by_var/multipel_in)
  
  return(retur_list)
}
# ny version - vi testar. Funkar den inte bra så finns den gamla kvar nedan, bortkommenterad
hitta_div_for_jamn_intervall <- function(
    max_varde,
    multipel,
    div_varden = c(4:12)
) {
  
  multipel_vekt <- c(multipel, (multipel*2), (multipel*multipel))     # skapa tillåtna steg per tjock stödlinje (1,2 eller 5 med multipel 5)
  
  har_godkant_heltal <- FALSE                                         # för kontroll om vi behöver utöka maxvärdet för att få vettigt antal steg av tjocka stödlinjer     
  nytt_max_varde <- max_varde                                         # nytt_max_varde, utfall att vi behöver utöka det så returnerar vi det nya värdet
  
  while(!har_godkant_heltal){                                         # testa om vi har ett godkänt heltal, om inte kör vi ett varv till
    antal_maj <- nytt_max_varde/multipel_vekt                         # antal tjocka stödlinjer
    godkant_heltal <- div_varden[div_varden %in% antal_maj]           # kolla om antalet steg ovan är ett tillåtet antal (anges i div_varden)
    # om det finns något godkänt heltal i antal_maj så sätter vi har_godkant_heltal till TRUE och använder det minsta antalet steg som är tillåtet
    if (length(godkant_heltal)>0){
      godkant_heltal <- min(godkant_heltal)
      har_godkant_heltal <- TRUE
    } else nytt_max_varde <- nytt_max_varde + multipel
  } # slut while-loop
  # lägg antalet tilltåtna tjocka stödlinjer i slut_div och maxvärdet i resultat (oavsett om det är nytt eller inte)
  retur_varde <- list(slut_div = godkant_heltal,
                      resultat = nytt_max_varde)
  
  return(retur_varde)
}

### gammal version av beräkning av antalet tjocka stödlinjer

# hitta_div_for_jamn_intervall <- function(
#     max_varde,
#     multipel,
#     #div_varden = c(5,6,7,8,9),
#     div_varden = c(5:15)
#     ) {
#   
#   blir_inf <- TRUE
#   
#   while (blir_inf) {
#     x <- 0            # vi börjar med 0
#     
#     res1 <- ((abs(max_varde) / div_varden)/5)/5
#     #res_div <- ifelse(nchar(trunc(res1)) < 2, 1, nchar(trunc((res1)))-1)
#     
#     while (all(res1< 1)){
#       x <- x+1
#       nytt_max_varde <- max_varde * (10 ^ x)
#       res1 <- ((abs(nytt_max_varde) / div_varden)/5)/5
#     }
#     
#     # ny test
#     res_div <- nchar(trunc((res1)))-1
#     
#     # här beräknar vi vilken av talen i div_varden som ger ett jämnt delbart tal med 5 så att vi får fina intervaller
#     div_res <- res1 / (10 ^ res_div)
#     
#     # returnera det (första) div-värde som i beräkningen ovan blir 2 
#     #retur_varde <- div_varden[which(div_res == 2 | which(div_res == 1))][1]
#     div_minsta_heltal <- suppressWarnings(min(div_res[(div_res - round(div_res)) == 0]))
#   
#     if (!is.infinite(div_minsta_heltal)) blir_inf <- FALSE else max_varde <- max_varde + multipel
#   } # slut på test om div_minsta_heltal blir inf
#   
#   retur_varde <- list(slut_div = div_varden[which(div_res == div_minsta_heltal)],
#                       resultat = max_varde)
#   return(retur_varde)
# }

Berakna_varden_stodlinjer <- function(min_varde, max_varde, y_borjar_pa_noll = TRUE, 
                                      procent_0_100_10intervaller = FALSE, 
                                      avrunda_fem = FALSE,
                                      minus_plus_samma = FALSE) {
  # om både min- och maxvärde är mindre än noll (dvs. bara negativa tal)
  if (min_varde < 0 & max_varde < 0) min_och_max_mindre_an_noll <- TRUE else min_och_max_mindre_an_noll <- FALSE
  if (min_och_max_mindre_an_noll) {
    min_varde <- abs(min_varde)
    max_varde <- abs(max_varde)
    if (min_varde > max_varde) {     # om min_varde är större än max_varde så byter vi plats på dem
      temp <- min_varde
      min_varde <- max_varde
      max_varde <- temp
    }
  }
  

  if (procent_0_100_10intervaller) {
    stodlinjer <- list("min_yvar" = 0, "max_yvar" = 100, "min_by_yvar" = 2, "maj_by_yvar" = 10)
  } else {
    
    modifierat_varde <- FALSE
    
    if (avrunda_fem) {
      avrund_list <- avrunda_till_multipel(max_varde)

      min_yvar <- ifelse(min_varde > 0 & y_borjar_pa_noll, 0, round(min_varde,(nchar(trunc(min_varde))-2)*-1))
      max_yvar <- avrund_list$max_varde
      maj_by_yvar <- avrund_list$maj_by_var
      min_by_yvar <- avrund_list$min_by_var
      
      # anpassa maxvärdet för stödlinjen om den inte är jämn med noll och
      # värdena sträcker sig över 0-linjen (både pos och neg tal)
      if (min_varde < 0 & max_varde > 0){
        
        # vi börjar med positiva maxvärdet
        test_varde <- 0
        while(test_varde < max_varde) test_varde <- test_varde + maj_by_yvar
        max_yvar <- test_varde
        
        # och så kör vi negativa minvärdet också
        test_varde <- 0
        while(test_varde > min_varde) test_varde <- test_varde - maj_by_yvar
        min_yvar <- test_varde
      }
      
      if (min_varde < 0 & max_varde < 0) max_yvar <- 0
      
      
    } else {
      if (max_varde < 1) {
        max_varde <- max_varde * 100
        min_varde <- min_varde * 100
        modifierat_varde <- TRUE
      }
      
      # tilldela variabler för att beräkna max och min i diagrammet
      min_yvar <- ifelse(min_varde > 0 & y_borjar_pa_noll, 0, round(min_varde,(nchar(trunc(min_varde))-2)*-1))
      if (min_yvar > min_varde) min_yvar <- floor(min_varde)
      max_yvar <- round(max_varde,(nchar(trunc(max_varde))-2)*-1)
      # om max_yvar är mindre än maxvärdet eller ligger mindre än 5 % över maxvärdet
      # så lägger vi till ett värde så att vi istället avrundar uppåt
      antal_siff_avrundn <- ifelse(nchar(trunc(max_yvar)) < 2, -1,-2)
      max_yvar <- ifelse(max_yvar < max_varde,
                         plyr::round_any(max_yvar, (10 ^ (nchar(trunc(max_yvar))+antal_siff_avrundn)), f = ceiling),
                         max_yvar)
      maj_by_yvar <- round((max_yvar-min_yvar) / 6, (nchar(trunc((max_yvar-min_yvar)/6))-1)*-1)
      maj_by_yvar <- 2 * ifelse(floor(maj_by_yvar/2)==0, 1, ceiling(maj_by_yvar/2))
      test_div <- c(5, 6, 4, 7)
      max_yvar <- (round(max_yvar / maj_by_yvar)) * maj_by_yvar     # För att maxvärdet alltid ska vara en major break
      if (max_yvar < max_varde) max_yvar <- (round(max_yvar / maj_by_yvar)+1) * maj_by_yvar
      min_by_yvar <- NA
      for (i in 1:length(test_div)){
        if (maj_by_yvar %% test_div[i] == 0) {
          min_by_yvar <- maj_by_yvar / test_div[i]
          break
        }
      }
      if (is.na(min_by_yvar)) min_by_yvar <- maj_by_yvar / 5
      
      # anpassa maxvärdet för stödlinjen om den inte är jämn med noll och
      # värdena sträcker sig över 0-linjen (både pos och neg tal)
      if (min_varde < 0 & max_varde > 0){
    
        # vi börjar med positiva maxvärdet
        test_varde <- 0
        while(test_varde < max_varde) test_varde <- test_varde + maj_by_yvar
        max_yvar <- test_varde
        
        # och så kör vi negativa minvärdet också
        test_varde <- 0
        while(test_varde > min_varde) test_varde <- test_varde - maj_by_yvar
        min_yvar <- test_varde
      }
      
      if (min_varde < 0 & max_varde < 0) max_yvar <- 0
    } # slut if-sats om det inte är avrunda_fem
    
    # om båda talen har varit negativa så omvandlas de till negativa tal här
    if (min_och_max_mindre_an_noll) {
      min_yvar <- min_yvar * -1
      max_yvar <- max_yvar * -1
      if (min_yvar > max_yvar) {     # om min_varde är större än max_varde så byter vi plats på dem
        temp <- min_yvar
        min_yvar <- max_yvar
        max_yvar <- temp
      }
    }
    
    # om vi vill att minus- och plusskalan ska vara lika stor (= största absolutvärdet) när vi har värden större och mindre än noll
    if ((min_yvar < 0 & max_yvar > 0) & minus_plus_samma) {
      if (abs(min_yvar) > max_yvar) {
        max_yvar <- abs(min_yvar)            # för att ge maxvärdet ett positivt värde lika stort som minvärdet
      } else {
        min_yvar <- max_yvar * -1            # för att ge minvärdet ett negativt värde lika stort som maxvärdet
      }
    }
    
    stodlinjer <- list("min_yvar" = min_yvar, "max_yvar" = max_yvar, "min_by_yvar" = min_by_yvar, "maj_by_yvar" = maj_by_yvar)
    # kontrollera om vi har modifierat max och min-värdena, i så fall, modifiera tillbaka till ursprungsvärdena
    if (modifierat_varde) {
      stodlinjer <- list("min_yvar" = min_yvar / 100, "max_yvar" = max_yvar / 100, 
                         "min_by_yvar" = min_by_yvar / 100, "maj_by_yvar" = maj_by_yvar / 100)
    }

  } # slut if-sats procent_0_100_10intervaller
  return(stodlinjer)
}

SkapaProcForandrTvaAr <- function(df, ar_kol, gruppering_vect, summ_var, startar = NA, slutar = NA){
  
  if (is.na(startar)) startar <- min(df[,ar_kol])            # om inte startår skickas med, använd första året i årskolumnen
  if (is.na(slutar)) slutar <- max(df[,ar_kol])            # om inte slutår skickas med, använd sista året i årskolumnen
  gruppering_strang <- c(ar_kol , gruppering_vect)
  
  retur_df <- df %>%
    filter(!!as.name(ar_kol) == startar | !!as.name(ar_kol) == slutar) %>%
    group_by_at(vars(one_of(gruppering_strang))) %>% 
    summarize(syss = sum(!!as.name(summ_var))) %>% 
    ungroup() %>% 
    pivot_wider(names_from = !!as.name(ar_kol), names_prefix = "ar_", values_from = syss)
  
  retur_df <- retur_df %>% mutate(proc = ((!!as.name(names(retur_df)[ncol(retur_df)])-
                                           !!as.name(names(retur_df)[ncol(retur_df)-1]))/
                                           !!as.name(names(retur_df)[ncol(retur_df)-1]))*100)
  retur_df$proc[retur_df$proc == "Inf"] <- NA
  names(retur_df)[ncol(retur_df)] <- paste0("Förändring ", tolower(summ_var), " ", startar, "-", slutar, " (procent)")
  return(retur_df)
}

diagramfarger <- function(farg = "gron_sex"){

  # ===================================== RUS-färgskalor ===========================================
  
  #rus_sex <- c("#E2EFDA", "#C6E0B4", "#A9D08E", "#70AD47", "#548235", "#375623")
  
  #rus_gradient <- c("#459079", "#4C957F", "#539B86", "#5AA08C", "#61A693", "#68AC99", "#6FB1A0", "#76B7A6", "#7DBDAD", "#84C2B3", "#8BC8BA", "#93CEC1")
  
  # blå variant av gradient-skalan
  rus_gradient <- c("#93CEC1", "#87C7B9", "#7CC0B2", "#71BAAB", "#65B3A3", "#5AAC9C", "#4FA695", "#449F8E", "#389886", "#2D927F", "#228B78", "#178571")
  
  #rus_tva_fokus <- c("#54B798", "#459079")
  # blå variant av rus_tva_fokus
  rus_tva_fokus <- c("#93cec1", "#178571")
  rus_tva_gra <- c("#178571", "#e6e6e6")
  
  #rus_tre_fokus <- c("#54B798", "#459079", "#000000")
  # blå variant av rus_tre_fokus
  rus_tre_fokus <- c("#93cec1", "#178571", "#000000")
  
  #rus_sex <- c("#0074A2", "#00B4E4", "#E2A855", "#FFD378", "#459079", "#54B798")
  
  # ny version baserad på utvecklingsarbete för Power BI
  rus_sex <- c("#178571", "#93cec1", "#0e5a4c", "#8edded", "#158daf", "#00577b")
  
  # ===================================== orange färgskalor ===========================================
  
  orange_en <- c("#ED7D31")
  
  orange_tva <- c("#833C0C", "#ED7D31")
  
  orange_tre_fokus <- c("#ED7D31", "#833C0C", "#000000")
  
  orange_fyra <- c("#F5C2B1", "#DE752D", "#BA6124", "#833C0C")
  
  orange_sex <- c("#F5BEAF", "#EB9A93", "#DE752D", "#BA6124", "#975020", "#673513")
  
  # ==================================== grön färgskalor ================================================
  
  gron_tva_fokus_morkgron <- c("#70AD47", "#375623")
  
  gron_tva_fokus <- c("#70AD47", "#000000")
  
  gron_tre_fokus <- c("#70AD47", "#375623", "#000000")
  
  gron_fyra <- c("#A9D08E", "#70AD47", "#548235", "#375623")
  
  gron_sex <- c("#E2EFDA", "#C6E0B4", "#A9D08E", "#70AD47", "#548235", "#375623")
  
  gron_export <- c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C")
  
  gron_export_en <- c("#41AB5D")
  
  # ==================================== blå färgskalor ================================================
  
  bla_sex <- c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")
  
  # ======================================== kön =========================================================
  
  kon <- c("#e2a855", "#459079")
  kon_fokus <- c("#e2a855", "#459079", "#ED7D31", "#0e5a4c")
  
  # ======================================== Lupp ========================================================
  
  # färgskala för att sätta ihop undersökning och kön
  grp_und_kon <- c("#FFEC9F", "#FFD378", "#E2A855", "#93CEC1", "#54B798", "#459079")
  
  Kön <- c("#e2a855", "#459079")
  
  Fodelseland_kat <- gron_sex
  
  Vistelsetid_kat <- gron_sex
  
  socek_kat <- orange_sex
  
  funk_kat <- c("#FDE0DD", "#FA9FB5", "#DD3497", "#AE017E", "#7A0177","#49006A")
  
  # grupperna men färger per år - använd samma färgskala som ovan
  
  Kön_år <- c("#9ECAE1", "#4292C6", "#08519C")
  
  Fodelseland_kat_år <- gron_sex
  
  Vistelsetid_kat_år <- gron_sex                                           #c(brewer.pal(9, "Greens"), brewer.pal(9, "Purples")[c(4, 6, 8)])
  
  socek_kat_år <- orange_sex
  
  funk_kat_år <- orange_sex
  
  # grupperna men färger för alla svarsalternativ
  
  Kön_alla <- c("#FFEC9F", "#93CEC1", "#FFD378", "#54B798", "#E2A855", "#459079")
  
  Fodelseland_kat_alla <- c("#70AD47", "#E2EFDA", "#548235", "#C6E0B4", "#375623", "#A9D08E")
  
  Vistelsetid_kat_alla <- c("#A9D08E", "#70AD47", "#548235", "#375623", "#A9D08E", "#70AD47", "#548235", "#375623", "#A9D08E", "#70AD47", "#548235", "#375623")
  
  socek_kat_alla <- c("#BA6124", "#F5BEAF", "#975020", "#EB9A93", "#673513", "#DE752D")
  
  funk_kat_alla <- funk_kat
  
  # kommunjämförelser
  kommun_jmfr <- c("#8C6BB1", "#88419D", "#810F7C")
  
  utan_grp_alla <- c("#EC7014", "#CC4C02", "#993404", "#662506")
  
  # ========================================= rd-profilfärger =======================================
  
  rd_alla_primar <- c("#F15060", "#FFD378", "#00B4E4", "#54B798", "#969696")
  
  rd_bla <- c("#00B4E4", "#8EDDED", "#B6F0FD", "#0074A2")
  
  rd_gron <- c("#54B798", "#93CEC1", "#D5EAE6", "#459079")
  
  rd_gul <- c("#FFD378", "#FFEC9F", "#FFF5CC", "#E2A855")
  
  rd_rod <- c("#F15060", "#F8AAB6", "#FFDDE2", "#AE2D3A")
  
  rd_gra <- c("#969696", "#e6e6e6", "#f1f1f1", "#424242")
  
  rd_karta_gron <- c("#459079", "#54B798", "#93CEC1", "#D5EAE6")
  
  rd_primar_atta <- c("#F15060", "#FFD378", "#00B4E4", "#54B798", "#969696", "#F8AAB6", "#0074A2", "#459079")
  
  rd_primar_nio <- c("#F15060", "#00B4E4", "#54B798", "#AE2D3A", "#0074A2", "#459079", "#E2A855", "#F8AAB6", "#969696")
  
  rd_handelsbalans <- c("#54B798", "#969696", "#459079")
  
  rd_export_gron <- c("#93CEC1", "#459079", "#54B798", "#93CEC1", "#459079", "#54B798", "#93CEC1", "#459079", "#54B798", "#93CEC1", "#459079")
  
  rd_gron_tva_fokus <- c("#54B798", "#000000")
  
  rd_gron_tre_fokus <- c("#54B798", "#459079", "#000000")
  
  #rd_export_gron <- c("#54B798", "#93CEC1", "#8EDDED", "#00B4E4", "#0074A2")
  
  # ========================================= övriga =====================================================
  
  bla_gra_tre <- c("#5B9BD5", "#BFBFBF", "#1F4E78")
  
  gron_gul_tvagrp_fyra <- c("#375623", "#548235", "#70AD47", "#C6E0B4", "#806000", "#BF8F00", "#FFC000", "#FFE699")
  
  gron_yrke_4_fokus <- c("#000000", "#548235", "#70AD47", "#375623")
  
  utb_floden <- c("#70AD47", "#548235", "#375623", "#F4A460", "#D2691E", "#8B4513")
  
  etabl_fokus <- c("#4472C4", "#203864")
  
  konsbalans_fem <- c("#E2A855", "#FFD378", "#969696", "#459079", "#54B798")
  
  kon_bakgrund <- c("#e2a855", "#459079", "#00B4E4", "#0074A2")
  
  rsp_enkat <- c("#178571", "#0e5a4c", "#158daf", "#00577b", "#BFBFBF")            # "#158daf"
  
  return(get(farg))
}
