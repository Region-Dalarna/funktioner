if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       RColorBrewer,
       magick,
       scales,        # för att använda format_format-funktionen och fixa till format på etiketter
       httr,
       openxlsx,
       ggtext,
       tidytext)     # för att sortera facet-diagram med funktionen reorder_within() och scale_x_reordered()
#library(png)
options(dplyr.summarise.inform = FALSE)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_logga_i_diagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_diagramfunktioner.R", encoding = "utf-8", echo = FALSE)

# ================================== Inställningar för alla diagram som skapas ===========================================================

SkapaStapelDiagram <- function(skickad_df, 
                               skickad_x_var, 
                               skickad_y_var, 
                               skickad_x_grupp = NA, 
                               skickad_filter_OR_vect = NA, 
                               skickad_filter_OR_var = NA,
                               diagram_titel = NULL,          # textsträng, blir diagramtitel, om NULL så skrivs diagrammet utan titel
                               diagram_undertitel = NULL,     # textsträng, blir undertitel, om NULL så skrivs diagrammet utan undertitel
                               undertitel_hjust = 0.5,        # styr vart undertiteln hamnar, 0 = vänster, 0.5 = mitten, 1 = höger
                               undertitel_storlek = 11,       # styr storleken på undertiteln
                               output_mapp,                   # textsträng, en giltig sökväg till den mapp som diagrammet ska sparas i 
                               diagram_capt = NULL,           # skicka med en textsträng som hamnar i nedre vänstra hörnet på diagram, som kan beskriva källa, vem som gjort diagrammet etc.
                               #diagram_capt_size, 
                               berakna_index = FALSE,
                               diagram_facet = FALSE,             # TRUE om vi vill skapa ett facet-diagram, annars FALSE
                               facet_grp = NA,                    # om vi kör ett facetdiagram så skickas facet-variabeln med här, ex. om man vill skriva ut ett diagram för varje region så skickas "region" med här 
                               facet_scale = "free",              # här finns möjlighet att styra skalan på facet_diagram, default är "free", alla får olika skalor, men kan sättas till "fixed" för att ha samma skala på alla facet-diagram
                               facet_legend_bottom = FALSE,       # TRUE tvingar in en teckenförklaring längst ned i diagrammet
                               facet_sort = FALSE,                # TRUE sorterar varje facet för sig. Kräver att man skapar en variabel på följande sätt: mutate(facetgruppen = as.factor(facetgruppen), x_axelvar = reorder_within(x_axelvar, y_axelvar, facetgruppen)), detta kräver paketet tidytext
                               facet_kolumner = NULL,             # hur många diagram i bredd vill man ha i sin facet-bild, NA = låter ggplot bestämma
                               facet_rader = NULL,                # hur många diagram i höjd vill man ha i sin facet-bild, NA = låter ggplot bestämma
                               facet_x_axis_storlek = 8,          # styr storleken på x-axeln i facetdiagram
                               facet_y_axis_storlek = 8,          # styr storleken på x-axeln i facetdiagram
                               facet_rubrik_storlek = 12,         # styr rubriken som skrivs ut ovanför varje facet
                               facet_space_diag_horisont = 5.5,   # styr det horisontella avståndet mellan facet-diagram
                               facet_oka_avstand_vid_visa_sista_vardet = 1.5,   # ökar avståndet mellan facet-diagram om man visar sista värdet med x_axis_visa_var_xe_etikett
                               manual_color = NA,                 # man kan lägga till en färgpalett som skickas som vektor med hexkoder, går före brew_palett
                               brew_palett = "Greens",            # defaultpalett är Greens i Rcolorbrewer men byts ut om det finns manual color
                               utan_diagramtitel = FALSE,         # visar ingen diagramtitel vid TRUE, om vi vill stänga av diagramtitel
                               manual_y_axis_title = NA,          # man kan skicka med en text som blir y-axelns titel, annars används variabelnamnet, om den sätts till "procent" så läggs % på efter värdet
                               manual_x_axis_title = NA,          # man kan skicka med en text som blir x-axelns titel
                               manual_x_axis_text_vjust = 0,      # justering av x-axelns etiketter, är lite snårig men kan sättas till 1 här och för hjust, brukar kunna funka hyfsat
                               manual_x_axis_text_hjust = 0.5,    # justering av x-axelns etiketter, är lite snårig men kan sättas till 1 här och för vjust, brukar kunna funka hyfsat
                               x_axis_lutning = 45,               # styr om etiketterna på x-axeln ska luta åt något håll, default är 45 graders lutning, men 0 brukar kunna vara bra också
                               x_axis_storlek = 10.5,             # styr storleken på x-axelns etiketter
                               y_axis_storlek = 12,               # styr storleken på y-axelns etiketter
                               y_axis_lutning = 0,                # styr om etiketterna på y-axeln ska luta åt något håll, default är 0 graders lutning
                               x_axis_sort_value = FALSE,         # för att sortera x-axelns etiketter efter värdet i y-variablen
                               x_axis_sort_grp = NA,              # måste vara ett heltal som är index för den grupp man vill sortera på, om man vill sortera på en grupp och inte på totalen (bra när man har stacked diagram med 100 % och vill sortera efter någon av grupperna) 
                               vand_sortering = FALSE,           # möjlighet att vända sortering, funkar bara om inte x_axis_sort_value används
                               x_var_fokus = NA,                  # lägg fokus på någon kategori i den variabel som skickas med denna parameter (alltså kolumnnamnet på den variabel som skickas med %>% %>% %>% %>% )
                               y_axis_100proc = FALSE,            # om man har ett diagram med procentvärden och vill visa hela skalan upp till 100 %, och inte sluta på en lägre procentsats kör man TRUE
                               y_axis_borjar_pa_noll = TRUE,      # man kan stänga av att diagrammet börjar på noll om man sätter detta till FALSE, då blir minvärdet högre än 0, annars börjar det alltid på 0
                               y_axis_minus_plus_samma_axel = FALSE, # om man vill ha lika stort avstånd från 0 till min och maxvärde på y-axeln - gäller endast när man har min-värde < 0 och max-värde > 0
                               x_axis_visa_var_xe_etikett = NA,   # möjlighet att bara visa var x:e etikett. X bestäms av det värde man skickar med (kan vara ex. 3 för att visa var tredje etikett på x-axeln)
                               inkludera_sista_vardet_var_xe_etikett = TRUE,              # om man vill ha med sista värdet när man kör x_axis_visa_var_xe_etikett
                               x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = FALSE, # tar bort näst sista värdet i visa_var_xe_etikett
                               procent_0_100_10intervaller = FALSE,  # om TRUE, så går y-axeln mellan 0 och 100, med tjocka stödlinjer med 10 enheters mellanrum, passar bra med procent 
                               legend_titel = NA,                 # om man vill ha en titel på teckenförklaringen kan man skicka med det som text här
                               legend_tabort = FALSE,             # om man vill tvinga bort legenden så kan man göra det här med TRUE
                               legend_vand_ordning = FALSE,       # om man vill vända ordningen på kategorierna i legenden (men inte i själva diagrammet)
                               legend_rader = NULL,               # ange hur många rader man vill ha i legenden
                               legend_kolumner = NULL,            # ange hur många kolumner man vill ha
                               legend_byrow = FALSE,              # om man vill byta ordning på objekten i legenden till radvis istället för kolumnvis
                               diagram_liggande = FALSE,          # sätt till TRUE om man vill vrida diagrammet och få det liggande
                               geom_position_stack = FALSE,       # om man vill ha ett stacked bar chart och inte dodge (=grupper i samma stapel och inte bredvid varandra)
                               AF_special = FALSE,                # speciallösning för diagram för AF-data, inget att bry sig om
                               lagg_pa_logga = TRUE,              # TRUE om man vill lägga på en logga, kräver en logga_path
                               logga_path = NA,                   # sökväg till var det finns en logga att lägga till i diagrammet
                               logga_scaling = 20,                # ett sätt att bestämma storlek på loggan i relation till diagrammet, lägre tal = större logga
                               dataetiketter = FALSE,             # rita ut dataetiketter 
                               dataetikett_storlek = 2.3,         # storlek på dataetiketterna
                               dataetiketter_antal_dec = 1,      # antal decimaler på dataetiketterna
                               dataetikett_noll_visa_ej = FALSE,  # om TRUE, skrivs inga dataetiketter för nollvärden
                               dataetiketter_justering_hojdled=0, # möjlighet att skriva en siffra för att skjuta dataetiketterna något uppåt (positivit tal) eller nedåt (negativt tal) från eller mot stapeln
                               dataetiketter_farg = "#464d48",    # möjlighet att byta färg på dataetiketterna
                               fokusera_varden = NA,              # för att rita ut ex. en rektangel som fokuserar på vissa specifika värden. fokusera_varden måste vara en lista som innehåller följande form = "rect", xmin, xmax, ymin, ymax, alpha samt fill. Form är alltså formen på fokusområdet, alpha är genomskinlighet (värde mellan 0 helt genomskinligt och 1 inte alls genomskinligt) och fill är färgen på området som ska fokuseras. Ex: fokusera_varden = list(list(geom = "rect", ymin=40, ymax=60, xmin=0, xmax=Inf, alpha=0.2, fill="grey20"))
                               stodlinjer_avrunda_fem = FALSE,     # för att alltid ha y-axlar som blir jämna tal, FALSE under testperioden, TRUE när vi vet att det funkar
                               skickad_namngiven_fargvektor = NA,  # om man vill sätta färger kopplat till kategorinamn så används denna
                               farg_variabler = NA,                # används tillsammans med skickad_namngiven_fargvektor, de två variabler som används för att bestämma färger
                               legend_kategorier_tabort_xgrupp = FALSE,   # om man vill tabort x-gruppen ur kategorierna som de skrivs ut i legenden
                               skriv_till_diagramfil = TRUE,      # om TRUE så skrivs diagrammet till en fil                                                                                                                                                                                                                                                                                                                                                               list(list(geom = "text", x = 0.3, y = 50, size = 2.5, fontface = "bold", angle = 0,label = "Jämställda branscher", color ="grey60"))
                               skriv_till_excelfil = FALSE,       # om TRUE så skrivs df:n ut till en excelfil som heter samma som diagrammet men med .xlsx istället för .png på slutet
                               diagramfil_hojd = 7,               # om ett diagram skrivs till fil kan proportionerna ändras här i filens upplösning, OBS! påverkar även textstorlekar etc.                                                                                                                                                                                                                                                                                                                   fontface finns "plain", "bold", "italic", "bold.italic"
                               diagramfil_bredd = 12,             # man kan påverka både storlek på diagrammet och proportionen mellan bredd och höjd
                               diagram_bildformat = "png",   # ange filändelsen för det format som du vill spara diagrammet i (svg, eps, jpg m.fl, alla som funkar i ggsave)
                               filnamn_diagram){                  # filnamn som diagrammet sparas som, bara filnamn med filändlse, sökvägen skickas med i output_mapp ovan
  
  # Här skapas variabler som används av de som skickats till funktionen
  x_var <- as.name(skickad_x_var)
  if (!is.na(skickad_x_grupp)) x_grupp <- as.name(skickad_x_grupp) else x_grupp <- NA
  if (!is.na(x_var_fokus)) x_var_fokus_asname <- as.name(x_var_fokus)
  if (is.na(skickad_x_grupp)) grupp_var <- x_var else grupp_var <- as.character(c(x_var, x_grupp))       # Skapa group_by-sträng utifrån om det finns grupperingsvariabel
  if (!is.na(x_var_fokus)) grupp_var <- c(grupp_var, x_var_fokus)
  if (is.na(facet_grp)) {
    facet_grp <- x_grupp 
  } else {
    facet_grp <- as.name(facet_grp)
    grupp_var <- c(grupp_var, facet_grp)
  }
  # eventuellt är det så att `-symbolen ställer till det i variabelnamn med mellanslag i just grupp_var, 
  # jag testar därför att ta bort ` ur grupp_var
  grupp_var <- gsub("`", "", grupp_var)
  
  y_var <- as.name(skickad_y_var)
  filter_or <- skickad_filter_OR_vect
  filter_or_var <- skickad_filter_OR_var
  
  # om vi vill ha en titel på teckenförklaringen
  if (is.na(legend_titel)) legend_titel <- NULL
  
  # Här skapas ett filter av 
  if (!is.na(filter_or)){
    filter_or <- paste0("'", filter_or, "'") #%>%             # Lägg till ' runt värdena som ska filtreras ut
    filter_or <- paste0(filter_or_var, " == ", filter_or)     # Sätt ihop variabeln som de tillhör, likamedtecken samt variabelvärdena (från raden ovan) 
    filter_or <- paste(filter_or, collapse = " | ")           # Sätt ihop alla värden till en OR-sats
  }
  # legendfix
  #if (legend_rader > 1) legend_radbryt = TRUE else legend_radbryt = FALSE
  
  # kontrollera att output_mapp har "/" eller "\\" som sista tecken
  if (!str_sub(output_mapp, nchar(output_mapp)) %in% c("/", "\\")) {
    output_mapp <- paste0(output_mapp, "/")
  }
  
  # speciallösning, om vi vill visa sista värdet med x_var_visa_var_xe_etikett så ökar vi avståndet mellan facet-diagram
  if (inkludera_sista_vardet_var_xe_etikett) facet_space_diag_horisont <- facet_space_diag_horisont + facet_oka_avstand_vid_visa_sista_vardet
  
  # gruppera variabler inför diagramskapande ===========================================================
  # testa om filter-variabeln är tom
  if (!is.na(filter_or)){
    # om filter_or har ett värde
    plot_df <- skickad_df %>% 
      filter(!! rlang::parse_expr(filter_or)) %>% 
      group_by(across(all_of(grupp_var))) %>% 
      summarize(total = sum(!!y_var, na.rm = TRUE)) %>% 
      ungroup()
  } else {
    # om filter_or är NA
    plot_df <- skickad_df %>% 
      group_by(across(all_of(grupp_var))) %>% 
      summarize(total = sum(!!y_var, na.rm = TRUE)) %>% 
      ungroup()
  }
  
  # beräkna index om det är valt =======================================================
  if (berakna_index){
    plot_index <- plot_df %>% 
      group_by(region) %>% 
      mutate(!!paste0("index_", min(plot_df$år)) := round((total/total[år == min(år)] * 100),0)) %>% 
      ungroup()
  }
  
  # ======================================== specialanpassningar av titlar och kategorinamn ===========================
  
  if (!is.na(manual_y_axis_title)) y_titel <- manual_y_axis_title else y_titel <- skickad_y_var
  
  # =========================================== skapa stödlinje-variabler =============================================
  
  if (y_axis_100proc) max_varde_plot_df <- 100 else max_varde_plot_df <- max(plot_df["total"])    # om vi skickat med att vi vill ha låsa y-axelns maxvärde till 100 så fixar vi det här - slice(1) utfall att det finns flera grupper som uppnår maxvärde (då tar vi bara en av dem)
  if (geom_position_stack) {
    if (max(plot_df$total) > 0 & min(plot_df$total) < 0){
      # om det finns både positiva och negativa värden, beräkna max-värden bara på postiva värden och min-värden bara på negativa värden
      max_varde_plot_df <- plot_df %>% filter(total > 0) %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == max(summ)) %>% slice(1) %>% dplyr::pull()
      min_varde_plot_df <- plot_df %>% filter(total < 0) %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == min(summ)) %>% slice(1) %>% dplyr::pull()
    } else {
      if (diagram_facet) {
        # skapa en vektor med de variabler som kan vara med, ta bort om något är NA
        variabel_vekt <- c(as.character(skickad_x_var), as.character(facet_grp), as.character(skickad_x_grupp)) %>% .[!is.na(.)]
        
        max_varde_plot_df <- plot_df %>% group_by(across(any_of(variabel_vekt))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == max(summ)) %>% slice(1) %>% dplyr::pull()
        min_varde_plot_df <- plot_df %>% group_by(across(any_of(variabel_vekt))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == min(summ)) %>% slice(1) %>% dplyr::pull()
      } else {
        # om det bara finns positiva eller negativa värden beräknas max- och min-värden som vanligt
        max_varde_plot_df <- plot_df %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == max(summ)) %>% slice(1) %>% dplyr::pull()
        min_varde_plot_df <- plot_df %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == min(summ)) %>% slice(1) %>% dplyr::pull()
      }  
    } # slut if-sats för att kolla om värdena i stacked bar sträcker sig över 0
    
  } else {
    min_varde_plot_df <- min(plot_df["total"])
  }
  if (min_varde_plot_df < 0 & max_varde_plot_df < 0) min_och_max_negativa <- TRUE else min_och_max_negativa <- FALSE
  stodlinjer_list <- Berakna_varden_stodlinjer(min_varde =  min_varde_plot_df, max_varde = max_varde_plot_df, y_borjar_pa_noll = y_axis_borjar_pa_noll, procent_0_100_10intervaller = procent_0_100_10intervaller, avrunda_fem = stodlinjer_avrunda_fem, minus_plus_samma = y_axis_minus_plus_samma_axel)
  
  min_yvar <- stodlinjer_list$min_yvar
  max_yvar <- stodlinjer_list$max_yvar
  min_by_yvar <- stodlinjer_list$min_by_yvar
  maj_by_yvar <- stodlinjer_list$maj_by_yvar
  
  # =================================================================================================
  
  antal_grupper <- ifelse(is.na(skickad_x_grupp), 0, nrow(unique(plot_df[x_grupp])))
  
  #etikettformat <- format_format(big.mark = " ", decimal.mark = ".",
  #                               scientific = FALSE)
  
  # används för att skapa etikettformat
  etikett_format <- function(x){
    x <- format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)
    if (!is.na(manual_y_axis_title)){
      if (manual_y_axis_title == "procent") x <- paste0(x, " %")
    }
    return(x)
  }
  
  # diagramfärger
  if (!is.na(manual_color)[1]) {
    # om man skickat med manual_color så används den, ananrs kollar man övrigt
    #if (!is.na(skickad_x_grupp) | !is.na(x_var_fokus)){             # om man har skickat med en x-grupp
    if (length(grupp_var) > 1){
      
        # om det bara finns en 
      #chart_col <- ifelse(nrow(unique(plot_df[x_grupp])) > 1 | !is.na(x_var_fokus), manual_color, manual_color[1])
      chart_col <- manual_color
      
      
    } else {
      chart_col <- manual_color[1]
    } # slut if-sats om det finns skickad x-grupp
    
  } else {
    if (is.na(brew_palett)) brew_palett <- "Greens"
    if (!is.na(skickad_x_grupp)) {
      chart_col <- case_when(
        nrow(unique(plot_df[x_grupp])) == 1 ~ "#4f6228",
        nrow(unique(plot_df[x_grupp])) == 2 ~ c("#9bbb59", "#4f6228")
      )
      if (nrow(unique(plot_df[x_grupp])) > 2) chart_col <- brewer.pal(nrow(unique(plot_df[x_grupp])), brew_palett)
    } else {
      chart_col <- "#4f6228"
    }
  }
  # ge legend_pos ett standardvärde
  legend_pos <- "none"
  
  # ge stapelbredd ett standardvärde
  stapel_bredd <- 0.9
  
  # ändra y-axelns storlek om det är ett facet-diagram
  if (diagram_facet) y_axis_storlek <- facet_y_axis_storlek
  
  # ge position ett standardvärde
  if (geom_position_stack) geom_bar_position <- "stack" else geom_bar_position <- "dodge"
  
  # här styr vi huruvida vi vill ha titlar på x- och y-axlarna. 
  if (is.na(y_titel)) y_titel <- NULL
  if (manual_y_axis_title == "procent" & !is.na(manual_y_axis_title)) y_titel <- NULL            # om vi skickat med "procent" manuellt för y-axeln så läggs % på enheten och vi kan stänga av "procent" som y-axeletikett
  if (is.na(manual_x_axis_title)) manual_x_axis_title <- NULL
  
  # test med sortering inom facet-diagram
  if (diagram_facet & facet_sort) {
    plot_df[facet_grp] <- factor(plot_df[[facet_grp]])
    plot_df[x_var] <- reorder_within(plot_df[[x_var]], plot_df$total, plot_df[[facet_grp]])
  }
  
  # vill vi sortera x-axeln utifrån värdet på y 
  if (x_axis_sort_value == TRUE) {
    plot_df[x_var] <- factor(plot_df[[x_var]])
    if (diagram_liggande){
      # kolla om användaren vill sortera på en av grupperna och inte på totalen
      if (is.na(x_axis_sort_grp)){
        # detta är om man sorterar som vanligt, dvs. på totalen (sortera omvänt om vi valt vand_sortering)
        if (!vand_sortering) plot_df[x_var] <- reorder(plot_df[[x_var]], plot_df$total) else plot_df[x_var] <- reorder(plot_df[[x_var]], desc(plot_df$total)) # om vi kör liggande diagram sorterar vi inte med desc() så blir största värde högst upp istället, blir bättre vid liggande diagram
      } else {
        # sortera om df:n och lägg till en sorteringskolumn som vi använder för att sortera rätt
        plot_df <- plot_df %>% 
          filter(!! rlang::sym(x_grupp) == unique(plot_df[[skickad_x_grupp]][x_axis_sort_grp])) %>% 
          arrange(!! rlang::sym(x_grupp), desc(total)) %>% 
          ungroup() %>% 
          select(all_of(skickad_x_var)) %>% 
          mutate(sort = row_number()) %>% 
          right_join(plot_df, by = skickad_x_var) %>% 
          arrange(sort, !! rlang::sym(x_grupp)) %>% 
          mutate(sort2 = row_number())
        # här sorterar vi om den skickade x-variabeln som faktor-variabel (sortera omvänt om vi valt vand_sortering)   
        if (!vand_sortering) plot_df[x_var] <- reorder(plot_df[[x_var]], plot_df$sort2) else plot_df[x_var] <- reorder(plot_df[[x_var]], desc(plot_df$sort2))
      }
      
    } else {  # samma övning men för icke liggande diagram
      if (is.na(x_axis_sort_grp)){
        # detta är om man sorterar som vanligt, dvs. på totalen (sortera omvänt om vi valt vand_sortering)
      if (!vand_sortering) plot_df[x_var] <- reorder(plot_df[[x_var]], desc(plot_df$total)) else plot_df[x_var] <- reorder(plot_df[[x_var]], plot_df$total)
      
      } else {
        # sortera om df:n och lägg till en sorteringskolumn som vi använder för att sortera rätt
        plot_df <- plot_df %>% 
          filter(!! rlang::sym(x_grupp) == unique(plot_df[[skickad_x_grupp]][x_axis_sort_grp])) %>% 
          arrange(!! rlang::sym(x_grupp), desc(total)) %>% 
          ungroup() %>% 
          select(all_of(skickad_x_var)) %>% 
          mutate(sort = row_number()) %>% 
          right_join(plot_df, by = skickad_x_var) %>% 
          arrange(sort, !! rlang::sym(x_grupp)) %>% 
          mutate(sort2 = row_number())
        # här sorterar vi om den skickade x-variabeln som faktor-variabel (sortera omvänt om vi valt vand_sortering)    
        if (!vand_sortering) plot_df[x_var] <- reorder(plot_df[[x_var]], desc(plot_df$sort2)) else plot_df[x_var] <- reorder(plot_df[[x_var]], plot_df$sort2)
      }
      
    }
  } else {
    # om vi INTE sorterar på värden utan på x-axeln så vänder vi den om det är ett liggande diagram
    if (diagram_liggande) {
      if (vand_sortering) plot_df[x_var] <- reorder(plot_df[[x_var]], desc(plot_df[[x_var]])) else plot_df[x_var] <- reorder(plot_df[[x_var]], plot_df[[x_var]])
    }  
  }
  
    if (!all(is.na(farg_variabler))) {
      ny_variabel <- names(farg_variabler)
      plot_df <- plot_df %>% 
        mutate(
          !!ny_variabel := map_chr(as.character(plot_df[[x_var]]), ~ {
            # Iterera över alla värden i farg_variabler[[1]] och kontrollera matchning
            match <- farg_variabler[[1]][str_detect(.x, regex(farg_variabler[[1]], ignore_case = TRUE))]
            if (length(match) > 0) {
              match[1]  # Returnera första matchande värdet
            } else {
              NA_character_  # Om inget matchar
            }
          }),
          !!ny_variabel := factor(!!sym(ny_variabel), levels = farg_variabler[[1]])
        )
      legend_kategorier <- levels(interaction(plot_df[[names(farg_variabler)]], plot_df[[x_grupp]])) %>% str_replace_all("\\.", " ")
      if (legend_kategorier_tabort_xgrupp) {
        legend_kategorier <- legend_kategorier %>% 
          str_remove(str_c("\\b", unique(plot_df[[names(farg_variabler)]]), "\\b", collapse = "|")) %>% 
          str_squish()
      } 
    }
  
  # Här börjar vi rita ut diagrammet
  if (!is.na(x_var_fokus)) {
    plot_df[x_var_fokus] <- factor(plot_df[[x_var_fokus]])
    #plot_df[x_var_fokus] <- reorder(plot_df[[x_var_fokus]], desc(plot_df$total))
    # Detta om vi bara ska ha en färg i diagrammet men fokusera på ett par olika staplar
    p <- plot_df %>% 
      ggplot(aes(x=!!x_var, y=total)) + 
      geom_bar(position = geom_bar_position, stat="identity", aes(fill = !!x_var_fokus_asname))
  # om vi skickat med en namngiven färgvektor 
  } else if (!all(is.na(skickad_namngiven_fargvektor)) & !all(is.na(farg_variabler))) {
    p <- plot_df %>% 
      ggplot(aes(x=!!x_var, y=total, fill = interaction(!!sym(names(farg_variabler)), !!x_grupp)))
    if (!diagram_facet | facet_legend_bottom) legend_pos <- "bottom" else legend_pos <- "none"        # lite oklart här om det ska vara or eller and i första if-satsen
    if (legend_tabort) legend_pos <- "none"
    
  } else if (is.na(skickad_x_grupp)) {
    p <- plot_df %>% 
      ggplot(aes(x=!!x_var, y=total, fill = chart_col))
  } else {
    p <- plot_df %>% ggplot(aes(x=!!x_var, y=total, fill = as.factor(!!x_grupp)))
    if (!diagram_facet | facet_legend_bottom) legend_pos <- "bottom" else legend_pos <- "none"        # lite oklart här om det ska vara or eller and i första if-satsen
    if (legend_tabort) legend_pos <- "none"
  }
  # en möjlighet att ta bort legenden oavsett andra val
  if (legend_tabort) legend_pos <- "none"
  
  p <- p +
    {if (antal_grupper > 3) {
      stapel_bredd <- 0.6                   # vi använder samma för etiketterna nedan (om vi har dataetiketter)
      geom_bar(stat="identity",width = stapel_bredd, position = geom_bar_position)
      #scale_x_discrete()
    } else if (!is.na(x_var_fokus)){
      #geom_bar(position = "dodge", stat="identity", fill = plot_df$fokus)
    } else {
      geom_bar(position = geom_bar_position, stat="identity")
    }} +
    #geom_bar(stat="identity", position = "dodge") +      #position = "dodge", width = 0.25,
    
    {if(dataetiketter){
      
      if (dataetikett_noll_visa_ej){
        # om vi valt parametern dataetikett_noll_visa_ej == TRUE, dvs. att ta bort dataetiketter som visar noll
        # och så ytterligare ett test om det är stacked eller dodge-diagram
        if (geom_position_stack) {
          geom_text(aes(y=total+sign(total), x=!!x_var, label=ifelse(total == 0, "", round(total,dataetiketter_antal_dec)),
                                                           vjust = ifelse(total >= 0, -0.5-dataetiketter_justering_hojdled, 1+dataetiketter_justering_hojdled)),
                  color = dataetiketter_farg,
                  size=dataetikett_storlek,
                  position = position_stack(vjust = 0.5))
        } else {
          geom_text(aes(y=total+sign(total), x=!!x_var, label=ifelse(total == 0, "", round(total,dataetiketter_antal_dec)),
                                                             vjust = ifelse(total >= 0, -0.5-dataetiketter_justering_hojdled, 1+dataetiketter_justering_hojdled)),
                    color = dataetiketter_farg,
                    size=dataetikett_storlek,
                    position = position_dodge(width = stapel_bredd))
        }
      } else {
        # om vi kör på som vanligt, dvs. visar nollvärden som dataetiketter  
        # och så ytterligare ett test om det är stacked eller dodge-diagram
        if (geom_position_stack) {
          geom_text(aes(y=total+sign(total), x=!!x_var, label=round(total,dataetiketter_antal_dec)),
                        vjust = ifelse(plot_df$total >= 0, -0.5-dataetiketter_justering_hojdled, 1+dataetiketter_justering_hojdled),
                    color = dataetiketter_farg,
                    size=dataetikett_storlek,
                    position = position_stack(vjust = 0.5))
        } else {
          geom_text(aes(y=total+sign(total), x=!!x_var, label=round(total,dataetiketter_antal_dec)),
                     vjust = ifelse(plot_df$total >= 0, -0.5-dataetiketter_justering_hojdled, 1+dataetiketter_justering_hojdled),
                  color = dataetiketter_farg,
                  size=dataetikett_storlek,
                  position = position_dodge(width = stapel_bredd))
        }
      }
    }} +
    
    {if (diagram_liggande) coord_flip()} +
    theme(axis.text.x = element_text(size = x_axis_storlek, angle = x_axis_lutning, 
                                     hjust = manual_x_axis_text_hjust, vjust = manual_x_axis_text_vjust),
          axis.text.y = element_text(size = y_axis_storlek, angle = y_axis_lutning),
          axis.ticks = element_blank(),
          legend.position = legend_pos,
          legend.margin = margin(0,0,0,0),
          legend.title = element_text(),
          legend.text = element_text(size = 12),
          #plot.title = element_text(hjust = 0.5, size = 20),
          plot.title = element_textbox_simple(
            size = 20,
            width = unit(0.9, "npc"),  # Bredd som proportion av plottens område
            halign = 0.5,  # Centrera texten
            margin = margin(7, 0, 7, 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(hjust = undertitel_hjust, size = undertitel_storlek),
          plot.caption = element_text(face = "italic",
                                      hjust = 0, vjust = 0),
          plot.caption.position = "plot",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(linewidth=0.8, colour = "lightgrey"),
          panel.grid.minor.y = element_line(linewidth=0.4, colour = "lightgrey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing.x = unit(facet_space_diag_horisont, "mm")) +
    {if (diagram_liggande) { 
      theme(panel.grid.major.x = element_line(linewidth=0.8, colour = "lightgrey"),
            panel.grid.minor.x = element_line(linewidth=0.4, colour = "lightgrey"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.spacing.x = unit(facet_space_diag_horisont, "mm"),
            plot.margin=unit(c(5.5, 25, 5.5, 5.5), 'pt'))
    }
    } +
    {if (utan_diagramtitel) theme(plot.title = element_blank())} +
    #{if (diagram_liggande) scale_x_discrete(limits = rev(levels(plot_df[x_var])))} + 
    labs(title = diagram_titel,
         subtitle = diagram_undertitel,
         caption = diagram_capt,
         x = manual_x_axis_title,
         y = y_titel,
         fill = legend_titel) +
      guides(fill = guide_legend(title.position = "top",
                                 title.hjust = 0.5, 
                                 reverse = legend_vand_ordning,
                                 ncol = legend_kolumner,
                                 nrow = legend_rader,
                                 byrow = legend_byrow)) +
    { if (!all(is.na(skickad_namngiven_fargvektor)) & !all(is.na(farg_variabler))){
      scale_fill_manual(values = skickad_namngiven_fargvektor,
                        labels = legend_kategorier
                          # names(farg_karta) %>% 
                          # str_replace_all("\\.", " ") %>%           #unique(plot_df[[x_grupp]])
                          # factor(levels = levels(interaction(plot_df$Kön, plot_df$År)) %>% str_replace_all("\\.", " "))
                        
      )
    } else {
      scale_fill_manual(values = chart_col)  
    }} +
    #scale_fill_manual(values = c("red", "blue")) +  
    { if (AF_special) {  
      scale_x_continuous(expand = c(0,.3), breaks = seq(1,max(plot_df[x_var]), by = 1))
    }} +
    # funktion för att bara visa var x:e etikett på x-axeln
    { if (!is.na(x_axis_visa_var_xe_etikett)) {  
      scale_x_discrete(expand = c(0,0), breaks = every_nth(n = x_axis_visa_var_xe_etikett, sista_vardet = inkludera_sista_vardet_var_xe_etikett, ta_bort_nast_sista = x_axis_var_xe_etikett_ta_bort_nast_sista_vardet))
    }} +
    
    # en lösning för att ta bort fasta stödlinjer om man kör facets med facet scales = "free"
    {if (!diagram_facet | (facet_scale == "fixed" & diagram_facet)){
      scale_y_continuous(breaks = seq(min_yvar, max_yvar, 
                                      by = maj_by_yvar),
                         minor_breaks = seq(min_yvar, max_yvar, by = min_by_yvar),
                         labels = etikett_format,
                         limits = c(min_yvar, max_yvar),
                         expand = c(0,0)) 
    } else {                        # om det fuckar med facet-diagram, ta bort denna else-sats då. Den gör att procentformatet funkar även på free-scale facetdiagram men jag är inte säker att det funkar för övriga diagram
      scale_y_continuous(labels = etikett_format,
                         expand = c(0,0))
      }} +
    
    {if (diagram_facet) facet_wrap(as.formula(paste("~",facet_grp)), scales = facet_scale,
                                   ncol = facet_kolumner,
                                   nrow = facet_rader) } +
    {if (diagram_facet & facet_sort) scale_x_reordered()} +                 # sorterar varje facetgrupp för sig om man kör facet_sort
    {if(diagram_facet){
      theme(strip.text = element_text(color = "black", size = facet_rubrik_storlek),
            strip.background = element_blank(),
            axis.text.x = element_text(size = facet_x_axis_storlek))
        
    } else {  
      theme(strip.text = element_blank())
    }}
  
  #p
  # här kan man lägga in en rektangel som markerar ett område, ex. de värden som är jämställda (40-60 %)
  if (!is.na(fokusera_varden[1])){
    
    if (is.null(names(fokusera_varden)[1])){          # ett sätt att göra skriptet bakåtkompatibelt. Om första elementet i listan är en lista, då kör vi på nya sättet, annars på det gamla (där det bara går att ha en lista)
      # här kör vi på det nya sättet där vi kan skicka med flera annoteringar
      for (ann_nr in 1:length(fokusera_varden)){
          if (fokusera_varden[[ann_nr]]$geom == "rect"){    # om geom är rect kör denna kod
            p <- p +
               annotate(
                 geom = fokusera_varden[[ann_nr]]$geom,          # måste vara "rect" här
                 ymin = fokusera_varden[[ann_nr]]$ymin,          # var rektangeln börjar på y-axeln
                 ymax = fokusera_varden[[ann_nr]]$ymax,          # var rektangeln slutar på y-axeln
                 xmin = fokusera_varden[[ann_nr]]$xmin,          # var rektangeln börjar på x-axeln
                 xmax = fokusera_varden[[ann_nr]]$xmax,          # var rektangeln slutar på x-axeln
                 alpha = fokusera_varden[[ann_nr]]$alpha,        # genomskinlighet, 1 är inte genomskinligt, 0 är helt genomskinligt
                 fill = fokusera_varden[[ann_nr]]$fill)          # färg på fyllningen i rektangeln
          } else if (fokusera_varden[[ann_nr]]$geom == "text") { 
            p <- p +
                annotate(
                  geom = fokusera_varden[[ann_nr]]$geom,          # måste vara "text" här.
                  x = fokusera_varden[[ann_nr]]$x,                # kan anges i siffror eller kateogri
                  y = fokusera_varden[[ann_nr]]$y,                # siffror, var på y-axeln läggs texten
                  label = fokusera_varden[[ann_nr]]$label,        # självaste texten
                  color = fokusera_varden[[ann_nr]]$color,        # färg på texten
                  size = fokusera_varden[[ann_nr]]$size,          # storlek på texten, runt 2-2.5 brukar vara lagom
                  fontface = fokusera_varden[[ann_nr]]$fontface,  # det finns "plain" som är vanlig text, "bold" och "italic" samt "bold.italic"
                  angle = fokusera_varden[[ann_nr]]$angle)        # vinkel på texten
          } # slut if-sats som pröver vilken typ av annotering det rör sig om
            
      } # slut på for-loop där vi loopar igenom listan med listor som innehåller annoteringar
    } else {
      # här kör vi på det gamla sättet då man bara kunde ha en annotering per diagram
      p <- p +
        annotate(
          geom = fokusera_varden$geom,
          ymin = fokusera_varden$ymin,
          ymax = fokusera_varden$ymax,
          xmin = fokusera_varden$xmin,
          xmax = fokusera_varden$xmax,
          alpha = fokusera_varden$alpha,
          fill = fokusera_varden$fill)
    } # slut på if-sats där vi testar 
  } # slut på if-sats där vi kontrollerar om vi har någon fokusera_varden-lista överhuvudtaget
  
  if (diagram_bildformat != "png") filnamn_diagram <- filnamn_diagram %>% str_replace(".png", paste0(".", diagram_bildformat))
  
  fullpath <- paste0(output_mapp, filnamn_diagram)  # används i skriv_till_diagramfil OCH i skriv_till_excelfil
  
  if (skriv_till_diagramfil){
    # Ändra höjd och bredd på den sparade png-filen, + ange mapp och filnamn
    bredd <- diagramfil_bredd
    hojd <- diagramfil_hojd
    
    if (diagram_bildformat == "eps") {
      
      cairo_ps(filename = paste0(output_mapp, filnamn_diagram),
               width = bredd, height = hojd, pointsize = 12,
               fallback_resolution = 300)
      print(gg_obj)
      dev.off()
      
    } else {
      ggsave(fullpath, width = bredd, height = hojd)
    }
    # Lägg till logga till diagrammet =======================================
    
    if (lagg_pa_logga){  
      if (is.na(logga_path)) logga_path <- hamta_logga_path()   # hämta sökväg till diagram
      add_logo(
        plot_path = paste0(output_mapp, filnamn_diagram), # url or local file for the plot
        logo_path = logga_path, # url or local file for the logo
        logo_position = "bottom right", # choose a corner
        # 'top left', 'top right', 'bottom left' or 'bottom right'
        logo_scale = logga_scaling,
        
        #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
        replace = TRUE
      )
    }
  }
  if (skriv_till_excelfil) {
    fullpath_excel <- str_replace(fullpath, ".png", ".xlsx")
    write.xlsx(skickad_df, fullpath_excel)
  }
  return(p)
}

SkapaLinjeDiagram <- function(skickad_df, 
                              skickad_x_var, 
                              skickad_y_var, 
                              skickad_x_grupp = NA, 
                              skickad_filter_OR_vect = NA, 
                              skickad_filter_OR_var = NA,
                              diagram_titel = NULL, 
                              diagram_undertitel = NULL, 
                              undertitel_hjust = 0.5,        # styr vart undertiteln hamnar, 0 = vänster, 0.5 = mitten, 1 = höger
                              undertitel_storlek = 11,       # styr storleken på undertiteln
                              output_mapp, 
                              diagram_capt = NULL,             # skicka med en textsträng som hamnar i nedre vänstra hörnet på diagram, som kan beskriva källa, vem som gjort diagrammet etc.
                              diagramfil_hojd = 7,               # om ett diagram skrivs till fil kan proportionerna ändras här i filens upplösning, OBS! påverkar även textstorlekar etc.                                                                                                                                                                                                                                                                                                                   fontface finns "plain", "bold", "italic", "bold.italic"
                              diagramfil_bredd = 12,             # man kan påverka både storlek på diagrammet och proportionen mellan bredd och höjd
                              berakna_index = FALSE,
                              lagga_till_punkter = FALSE,     # Om man vill kombinera ett linjediagram med ett punktdiagran, dvs, med punkter vid varje observation
                              diagram_facet = FALSE,
                              facet_grp = NA,
                              facet_scale = "free",
                              facet_sort = FALSE,                # TRUE sorterar varje facet för sig. Kräver att man skapar en variabel på följande sätt: mutate(facetgruppen = as.factor(facetgruppen), x_axelvar = reorder_within(x_axelvar, y_axelvar, facetgruppen)), detta kräver paketet tidytext
                              facet_kolumner = NULL,             # hur många diagram i bredd vill man ha i sin facet-bild, NA = låter ggplot bestämma
                              facet_rader = NULL,                # hur många diagram i höjd vill man ha i sin facet-bild, NA = låter ggplot bestämma
                              facet_x_axis_storlek = 8,          # styr storleken på x-axeln i facetdiagram
                              facet_y_axis_storlek = 8,          # styr storleken på x-axeln i facetdiagram
                              facet_rubrik_storlek = 12,         # styr rubriken som skrivs ut ovanför varje facet
                              facet_space_diag_horisont = 5.5,   # styr det horisontella avståndet mellan facet-diagram
                              facet_oka_avstand_vid_visa_sista_vardet = 1.5,   # ökar avståndet mellan facet-diagram om man visar sista värdet med x_axis_visa_var_xe_etikett
                              manual_color = NA,
                              brew_palett = "Greens",
                              stodlinjer_avrunda_fem = FALSE,     # för att alltid ha y-axlar som blir jämna tal, FALSE under testperioden, TRUE när vi vet att det funkar
                              manual_y_axis_title = NA,
                              manual_x_axis_title = NA,
                              x_axis_lutning = 45,
                              x_axis_storlek = 10.5,
                              y_axis_minus_plus_samma_axel = FALSE, # om man vill ha lika stort avstånd från 0 till min och maxvärde på y-axeln - gäller endast när man har min-värde < 0 och max-värde > 0
                              facet_legend_bottom = FALSE,
                              x_axis_visa_var_xe_etikett = NA,
                              inkludera_sista_vardet_var_xe_etikett = TRUE,              # om man vill ha med sista värdet när man kör x_axis_visa_var_xe_etikett
                              x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = FALSE, # tar bort näst sista värdet i visa_var_xe_etikett
                              AF_special = FALSE,
                              
                              legend_titel = NA,                 # om man vill ha en titel på teckenförklaringen kan man skicka med det som text här
                              legend_tabort = FALSE,             # om man vill tvinga bort legenden så kan man göra det här med TRUE
                              legend_vand_ordning = FALSE,       # om man vill vända ordningen på kategorierna i legenden (men inte i själva diagrammet)
                              legend_rader = NULL,               # ange hur många rader man vill ha i legenden
                              legend_kolumner = NULL,            # ange hur många kolumner man vill ha
                              legend_byrow = FALSE,              # om man vill byta ordning på objekten i legenden till radvis istället för kolumnvis
                              
                              lagg_pa_logga = TRUE,
                              procent_0_100_10intervaller = FALSE,
                              logga_path = NA,
                              logga_scaling = 15,
                              skriv_till_diagramfil = TRUE,
                              filnamn_diagram,
                              utan_diagramtitel = FALSE,
                              diagram_bildformat = "png",           # ange filändelsen för det format som du vill spara diagrammet i (svg, eps, jpg m.fl, alla som funkar i ggsave)
                              y_axis_borjar_pa_noll = TRUE,              # sätt till FALSE om y-axeln ska börja på annat värde än 0
                              y_axis_100proc = FALSE){                   # sätt till TRUE om y-axeln ska vara mellan 0 och 100
  
  # Här skapas variabler som används av de som skickats till funktionen
  x_var <- as.name(skickad_x_var)
  if (!is.na(skickad_x_grupp)) x_grupp <- as.name(skickad_x_grupp) else x_grupp <- NA
  if (is.na(skickad_x_grupp)) grupp_var <- x_var else grupp_var <- as.character(c(x_var, x_grupp))       # Skapa group_by-sträng utifrån om det finns grupperingsvariabel
  if (is.na(facet_grp)) {
    facet_grp <- x_grupp 
  } else {
    facet_grp <- as.name(facet_grp)
    grupp_var <- as.character(c(grupp_var, facet_grp))
  }
  y_var <- as.name(skickad_y_var)
  filter_or <- skickad_filter_OR_vect
  filter_or_var <- skickad_filter_OR_var
  
  # Här skapas ett filter av 
  filter_or <- paste0("'", filter_or, "'") #%>%             # Lägg till ' runt värdena som ska filtreras ut
  filter_or <- paste0(filter_or_var, " == ", filter_or)     # Sätt ihop variabeln som de tillhör, likamedtecken samt variabelvärdena (från raden ovan) 
  filter_or <- paste(filter_or, collapse = " | ")           # Sätt ihop alla värden till en OR-sats
  
  # kontrollera att output_mapp har "/" eller "\\" som sista tecken
  if (!str_sub(output_mapp, nchar(output_mapp)) %in% c("/", "\\")) {
    output_mapp <- paste0(output_mapp, "/")
  }
  
  # speciallösning, om vi vill visa sista värdet med x_var_visa_var_xe_etikett så ökar vi avståndet mellan facet-diagram
  if (inkludera_sista_vardet_var_xe_etikett) facet_space_diag_horisont <- facet_space_diag_horisont + facet_oka_avstand_vid_visa_sista_vardet
  
  # gruppera variabler inför diagramskapande ===========================================================
  # testa om filter-variabeln är tom
  if (!is.na(skickad_filter_OR_vect)){
    # om filter_or har ett värde
    plot_df <- skickad_df %>% 
      filter(!! rlang::parse_expr(filter_or)) %>% 
      group_by(across(all_of(grupp_var))) %>% 
      summarize(total = sum(!!y_var, na.rm = TRUE)) %>% 
      ungroup()
  } else {
    # om filter_or är NA
    plot_df <- skickad_df %>% 
      group_by(across(all_of(grupp_var))) %>% 
      summarize(total = sum(!!y_var, na.rm = TRUE)) %>% 
      ungroup()
  }
  
  # om vi vill ha en titel på teckenförklaringen
  if (is.na(legend_titel)) legend_titel <- NULL
  
  # beräkna index om det är valt =======================================================
  if (berakna_index){
    plot_df <- plot_df %>% 
      group_by(!!x_grupp) %>% 
      mutate(index := round((total/total[år == min(år)] * 100),0)) %>% 
      ungroup()
    if (is.na(manual_y_axis_title)) y_titel <- paste0(strsplit(skickad_y_var, ",")[[1]][1], ", index 100 = ", min(plot_df$år)) else y_titel <- manual_y_axis_title
    plot_df <- plot_df %>% select(-total) %>% rename(total = index)
  } else {
    
    # ======================================== specialanpassningar av titlar och kategorinamn ===========================
    
    if (!is.na(manual_y_axis_title)) y_titel <- manual_y_axis_title else y_titel <- skickad_y_var
  }
  # =========================================== skapa stödlinje-variabler =============================================
  
  # 
  # if (y_axis_100proc) max_varde_plot_df <- 100 else max_varde_plot_df <- max(plot_df["total"])    # om vi skickat med att vi vill ha låsa y-axelns maxvärde till 100 så fixar vi det här - slice(1) utfall att det finns flera grupper som uppnår maxvärde (då tar vi bara en av dem)
  # 
  #   if (max(plot_df$total) > 0 & min(plot_df$total) < 0){
  #     # om det finns både positiva och negativa värden, beräkna max-värden bara på postiva värden och min-värden bara på negativa värden
  #     max_varde_plot_df <- plot_df %>% filter(total > 0) %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == max(summ)) %>% slice(1) %>% pull()
  #     min_varde_plot_df <- plot_df %>% filter(total < 0) %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == min(summ)) %>% slice(1) %>% pull()
  #   } else {
  #     # om det bara finns positiva eller negativa värden beräknas max- och min-värden som vanligt
  #     max_varde_plot_df <- plot_df %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == max(summ)) %>% slice(1) %>% pull()
  #     min_varde_plot_df <- plot_df %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == min(summ)) %>% slice(1) %>% pull()
  #     
  #   } # slut if-sats för att kolla om värdena i stacked bar sträcker sig över 0
  #   
  # stodlinjer_list <- Berakna_varden_stodlinjer(min_varde =  min_varde_plot_df, max_varde = max_varde_plot_df, y_borjar_pa_noll = y_axis_borjar_pa_noll, procent_0_100_10intervaller = procent_0_100_10intervaller, avrunda_fem = stodlinjer_avrunda_fem)
  # 
  # min_yvar <- stodlinjer_list$min_yvar
  # max_yvar <- stodlinjer_list$max_yvar
  # min_by_yvar <- stodlinjer_list$min_by_yvar
  # maj_by_yvar <- stodlinjer_list$maj_by_yvar

  
  # Ändrat 14 aug 2024:
  # if (y_axis_100proc) max_varde_plot_df <- 100 else max_varde_plot_df <- max(plot_df["total"])    # om vi skickat med att vi vill ha låsa y-axelns maxvärde till 100 så fixar vi det här
  # stodlinjer_list <- Berakna_varden_stodlinjer(min_varde =  min(plot_df["total"]), max_varde = max_varde_plot_df, y_borjar_pa_noll = y_axis_borjar_pa_noll, procent_0_100_10intervaller = procent_0_100_10intervaller, avrunda_fem = stodlinjer_avrunda_fem)
  # min_yvar <- stodlinjer_list$min_yvar
  # max_yvar <- stodlinjer_list$max_yvar
  # min_by_yvar <- stodlinjer_list$min_by_yvar
  # maj_by_yvar <- stodlinjer_list$maj_by_yvar
  
  # =========================================== skapa stödlinje-variabler =============================================
  
  if (y_axis_100proc) max_varde_plot_df <- 100 else max_varde_plot_df <- max(plot_df["total"])    # om vi skickat med att vi vill ha låsa y-axelns maxvärde till 100 så fixar vi det här - slice(1) utfall att det finns flera grupper som uppnår maxvärde (då tar vi bara en av dem)
  if (diagram_facet) {
    variabel_vekt <- c(as.character(skickad_x_var), as.character(facet_grp), as.character(skickad_x_grupp)) %>% .[!is.na(.)]
    
    max_varde_plot_df <- plot_df %>% group_by(across(any_of(variabel_vekt))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == max(summ)) %>% slice(1) %>% dplyr::pull()
    min_varde_plot_df <- plot_df %>% group_by(across(any_of(variabel_vekt))) %>% summarise(summ = sum(total)) %>% ungroup() %>% filter(summ == min(summ)) %>% slice(1) %>% dplyr::pull()
    
  } else { 
    min_varde_plot_df <- min(plot_df["total"])
  }
  if (min_varde_plot_df < 0 & max_varde_plot_df < 0) min_och_max_negativa <- TRUE else min_och_max_negativa <- FALSE
  stodlinjer_list <- Berakna_varden_stodlinjer(min_varde =  min_varde_plot_df, max_varde = max_varde_plot_df, y_borjar_pa_noll = y_axis_borjar_pa_noll, procent_0_100_10intervaller = procent_0_100_10intervaller, avrunda_fem = stodlinjer_avrunda_fem, minus_plus_samma = y_axis_minus_plus_samma_axel)
  
  min_yvar <- stodlinjer_list$min_yvar
  max_yvar <- stodlinjer_list$max_yvar
  min_by_yvar <- stodlinjer_list$min_by_yvar
  maj_by_yvar <- stodlinjer_list$maj_by_yvar
  
  
  
  
  # om vi vill visa var x:e x-axeletikett
  #if (visa_var_x_xlabel > 0) rep_vec <- c(T, rep(F, visa_var_x_xlabel-1))
  
  
  etikett_format <- function(x){
    x <- format(x, big.mark = " ", scientific = FALSE)
    if (!is.na(manual_y_axis_title)){
      if (manual_y_axis_title == "procent") x <- paste0(x, " %")
    }
    return(x)
  }
  
  # diagramfärger
  if (!is.na(skickad_x_grupp)) {
    chart_col <- case_when(
      nrow(unique(plot_df[x_grupp])) == 1 ~ "#4f6228",
      nrow(unique(plot_df[x_grupp])) == 2 ~ c("#9bbb59", "#4f6228")
    )
    if (nrow(unique(plot_df[x_grupp])) > 2) chart_col <- brewer.pal(nrow(unique(plot_df[x_grupp])), brew_palett)
  } else {
    chart_col <- "#4f6228"
  }
  # styr om vi ska köra manuell färgskala som är medskickad samt samma med x- och y-titlar
  if (!is.na(manual_color[1])) chart_col <- manual_color
  if (manual_y_axis_title == "procent" & !is.na(manual_y_axis_title)) y_titel <- NULL
  if (is.na(manual_x_axis_title)) manual_x_axis_title <- NULL
  
  # för att sätta limits i ggplot korrekt
  expand_vekt <- c(0,0)
  limit_min <- ifelse(min_yvar < 0, min_yvar, 0)
  if (!y_axis_borjar_pa_noll) {
    limit_min <- min_yvar
    expand_vekt <- NULL
  }
  
  # ge legend_pos ett standardvärde
  legend_pos <- "none"
  
  # Här börjar vi göra diagrammet ======================================================
  # om x_grupp är tom ta bort den raden, annars kör med den  
  if (is.na(skickad_x_grupp)) {
    p <-plot_df %>% ggplot(aes(x=!!x_var, y=total)) +
      {if (berakna_index) geom_hline(yintercept = 100, color = "grey32", linewidth = 1.2)} +
      geom_line(aes(color = chart_col), linewidth = 1.5)
  } else {
    p<-plot_df %>% ggplot(aes(x=!!x_var, y=total, group = !!x_grupp)) +
      {if (berakna_index) geom_hline(yintercept = 100, color = "grey32", linewidth = 1.2)} +
      geom_line(aes(color = !!x_grupp), linewidth = 1.5)
    if (!diagram_facet | facet_legend_bottom) legend_pos <- "bottom"
  }
  if(legend_tabort) legend_pos <- "none"
  # fortsätt att fylla på objektet p som är diagrammet
  
  if(lagga_till_punkter){
    p <- p + geom_point(aes(color = !!x_grupp), size = 2.5)
  }
  
  p <- p +
    theme(axis.text.x = element_text(size = x_axis_storlek, angle = x_axis_lutning, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.ticks = element_blank(),
          legend.position = legend_pos,
          legend.key = element_rect(fill = "white"),
          legend.margin = margin(0,0,0,0),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = undertitel_hjust, size = undertitel_storlek),
          plot.caption = element_text(face = "italic",
                                      hjust = 0, vjust = 0),
          plot.caption.position = "plot",
          panel.spacing.x = unit(facet_space_diag_horisont, "mm"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(linewidth=0.8, colour = "lightgrey"),
          panel.grid.minor.y = element_line(linewidth=0.4, colour = "lightgrey") ,
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    {if (utan_diagramtitel) theme(plot.title = element_blank())} +
    labs(title = diagram_titel,
         subtitle = diagram_undertitel,
         x = manual_x_axis_title,
         caption = diagram_capt,
         y = y_titel,
         color=legend_titel) +
    guides(color = guide_legend(title.position = "top",
                               title.hjust = 0.5, 
                               reverse = legend_vand_ordning,
                               ncol = legend_kolumner,
                               nrow = legend_rader,
                               byrow = legend_byrow)) +
    scale_color_manual(values = chart_col) +  
    { if (AF_special) {  
      scale_x_continuous(expand = c(0,.3), breaks = seq(1,53, by = 1))
    }} +
    { if (!is.na(x_axis_visa_var_xe_etikett)) {  
      scale_x_discrete(expand = c(0,0), breaks = every_nth(n = x_axis_visa_var_xe_etikett, sista_vardet = inkludera_sista_vardet_var_xe_etikett, ta_bort_nast_sista = x_axis_var_xe_etikett_ta_bort_nast_sista_vardet))
    }} +
    
    # scale_y_continuous(breaks = seq(min_yvar, max_yvar, 
    #                                 by = round(max_yvar / 6, (nchar(trunc(max_yvar/6))-1)*-1)),
    #                    minor_breaks = seq(min_yvar, max_yvar, by = min_by_yvar),
    
    {if (!diagram_facet | (facet_scale == "fixed" & diagram_facet)){
      scale_y_continuous(breaks = seq(min_yvar, max_yvar, 
                                      by = maj_by_yvar),
                         minor_breaks = seq(min_yvar, max_yvar, by = min_by_yvar),
                         labels = etikett_format,
                         limits = c(min_yvar, max_yvar),
                         expand = c(0,0)) 
    } else {                        # om det fuckar med facet-diagram, ta bort denna else-sats då. Den gör att procentformatet funkar även på free-scale facetdiagram men jag är inte säker att det funkar för övriga diagram
      scale_y_continuous(labels = etikett_format,
                         expand = c(0,0))
    }} +
    
    
    # scale_y_continuous(breaks = seq(min_yvar, max_yvar, 
    #                                 by = maj_by_yvar),
    #                    minor_breaks = seq(min_yvar, max_yvar, by = min_by_yvar),
    #                    labels = etikett_format,
    #                    #expand = expand_vekt, 
    #                    limits = c(limit_min,max_yvar),
    #                    expand = c(0,0)) +
    # 
    
    
    #labels = function(x) format(x, big.mark = " ")) +
    #{if (diagram_facet & x_grupp != "NA") facet_wrap(as.formula(paste("~",facet_grp)), scales = facet_scale) } +
    #{if (diagram_facet) facet_wrap(as.formula(paste("~",facet_grp)), scales = facet_scale) } +
  
    {if (diagram_facet) facet_wrap(as.formula(paste("~",facet_grp)), scales = "free",
                                   ncol = facet_kolumner,
                                   nrow = facet_rader) } +
    
    #{if (diagram_facet & x_grupp != "NA"){               # gammal, om det krånglar kan man lägga till x_grupp-delen igen
    {if (diagram_facet & facet_sort) scale_x_reordered()} +                 # sorterar varje facetgrupp för sig om man kör facet_sort
    {if (diagram_facet){
      theme(strip.text = element_text(color = "black", size = facet_rubrik_storlek),
            strip.background = element_blank(),
            axis.text.x = element_text(size = facet_x_axis_storlek),
            axis.text.y = element_text(size = facet_y_axis_storlek)
            )  
    } else {  
      theme(strip.text = element_blank())
    }}
  
  # skriv till diagramfil om sådan är vald
  if (skriv_till_diagramfil){
    # Ändra höjd och bredd på den sparade png-filen, + ange mapp och filnamn
    bredd <- diagramfil_bredd
    hojd <- diagramfil_hojd
    
    if (diagram_bildformat != "png") filnamn_diagram <- filnamn_diagram %>% str_replace(".png", paste0(".", diagram_bildformat))
    
    fullpath <- paste0(output_mapp, filnamn_diagram)
    ggsave(fullpath, width = bredd, height = hojd)
    
    # Lägg till logga till diagrammet =======================================
    if (lagg_pa_logga) {
      if (is.na(logga_path)) logga_path <- hamta_logga_path()       # hämta logga_path i funktion först i denna fil
      if (!is.null(logga_path)){  
        add_logo(
          plot_path = paste0(output_mapp, filnamn_diagram), # url or local file for the plot
          logo_path = logga_path, # url or local file for the logo
          logo_position = "bottom right", # choose a corner
          # 'top left', 'top right', 'bottom left' or 'bottom right'
          logo_scale = logga_scaling,
          #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
          replace = TRUE
        )
      } # if !is.null(logga_path)
    } # if lagg_pa_logga
  } # if skriv_till_diagramfil
  return(p)
}

skriv_till_diagramfil <- function(ggplot_objekt,
                                  diagramfil_bredd = 12,
                                  diagramfil_hojd = 7,
                                  output_mapp,
                                  filnamn_diagram,
                                  lagg_pa_logga = TRUE,
                                  logga_scaling = 15,
                                  logga_path = NA,
                                  diagram_bildformat = "png"
                                  ) {   
  
  g <- ggplot_objekt
  # Ändra höjd och bredd på den sparade png-filen, + ange mapp och filnamn
  bredd <- diagramfil_bredd
  hojd <- diagramfil_hojd
  
  if (diagram_bildformat != "png") filnamn_diagram <- filnamn_diagram %>% str_replace(".png", paste0(".", diagram_bildformat))
  
  fullpath <- paste0(output_mapp, filnamn_diagram)
  ggsave(fullpath, width = bredd, height = hojd)
  
  # Lägg till logga till diagrammet =======================================
  
  if (lagg_pa_logga){  
    if (is.na(logga_path)) logga_path <- hamta_logga_path()   # hämta sökväg till diagram
    add_logo(
      plot_path = paste0(output_mapp, filnamn_diagram), # url or local file for the plot
      logo_path = logga_path, # url or local file for the logo
      logo_position = "bottom right", # choose a corner
      # 'top left', 'top right', 'bottom left' or 'bottom right'
      logo_scale = logga_scaling,
      
      #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
      replace = TRUE)
    }
}

# every_nth <- function(n) {
#   return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
# }

every_nth <- function(n, sista_vardet, ta_bort_nast_sista = FALSE) {
  return(function(x) {
    # Skapar en logisk vektor med var n:te element satt till TRUE
    vec <- c(TRUE, rep(FALSE, n - 1))
    # Upprepar vektorn så att den täcker hela längden på x
    repeated_vec <- rep(vec, length.out = length(x))
    # Säkerställer att det sista värdet alltid är TRUE
    if (sista_vardet) repeated_vec[length(x)] <- TRUE
    # Om ta_bort_nast_sista är TRUE, sätt näst sista elementet till FALSE
    if (ta_bort_nast_sista && length(x) > 1) repeated_vec[length(x) - 1] <- FALSE
    
    return(x[repeated_vec])
  })
}
