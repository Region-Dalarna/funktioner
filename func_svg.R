
# Färdiga funktioner för att hantera SVG-filer ========================================================================

svg_koppla_ihop_grupper <- function(svg_indatafil_sokvag_filnamn) {
  # Funktion för att läsa SVG och koppla ihop <g>-grupper med deras innehåll
  
  # Läs in SVG-filen
  svg <- read_xml(svg_indatafil_sokvag_filnamn)
  
  # Definiera namnrymden för SVG
  ns <- c(svg = "http://www.w3.org/2000/svg")
  
  # Extrahera attribut från <svg>-noden
  svg_attribut <- xml_attrs(svg)
  
  # Hitta alla <g>-element och deras attribut samt barn
  grupper <- xml_find_all(svg, ".//svg:g", ns)
  
  # Extrahera information om varje <g>-element med purrr::map
  grupp_info <- map(grupper, function(g) {
    list(
      attributes = xml_attrs(g),
      children = xml_children(g)
    )
  })
  
  # Skapa en lista för att lagra hela SVG-strukturen
  svg_lista <- list(
    svg_attributes = svg_attribut,
    groups = grupp_info
  )
  
  return(svg_lista)
}
svg_skriv_fil_fran_svg_lista <- function(svg_lista, svgfil_ny) {
  # Funktion för att skapa en ny SVG-fil från svg_lista  
  
  # Skapa en ny <svg>-nod med de sparade attributen
  svg_ny <- xml_new_root("svg", .namespace = "http://www.w3.org/2000/svg")
  
  # Lägg till alla attribut från den ursprungliga <svg>-noden
  xml_set_attrs(svg_ny, svg_lista$svg_attributes)
  
  # Funktion för att rekursivt kopiera grupper och deras innehåll
  grupp_lagg_till <- function(grupp_info, foralder) {
    # Skapa en <g>-nod och sätt dess attribut
    grupp_ny <- xml_add_child(foralder, "g")
    xml_set_attrs(grupp_ny, grupp_info$attributes)
    
    # Lägg till alla barnnoder i gruppen
    walk(grupp_info$children, function(child) {
      if (xml_name(child) == "circle") {
        # Om noden är en cirkel, kopiera attributen
        cirkel_nod <- xml_add_child(grupp_ny, "circle")
        xml_set_attrs(cirkel_nod, xml_attrs(child))
      } else if (xml_name(child) == "text") {
        # Om noden är text, kopiera attributen och textinnehållet
        text_nod <- xml_add_child(grupp_ny, "text", xml_text(child))
        xml_set_attrs(text_nod, xml_attrs(child))
      }
    })
  }
  
  # Lägg till alla <g>-grupper
  walk(svg_lista$groups, grupp_lagg_till, foralder = svg_ny)
  
  # Spara den nya SVG-filen
  write_xml(svg_ny, svgfil_ny)
}

svg_meta_df_skapa <- function(svg_lista) {
  # skapa en dataframe med information om vilka grupper de olika elementen tillhör
  
  # Extrahera textinnehåll från det första elementet i varje text-nod
  texter <- map_chr(svg_lista$groups[[3]]$children, xml_text)
  namn_vektor <- c("root", texter[-1])  # Exkludera första elementet, eftersom det är tomt för root
  
  df <- tibble(
    #radnummer = seq_along(namn_vektor),
    namn = namn_vektor,
    grupp = NA_character_,
    typ = NA_character_
  )
  
  # Iterera över elements i groups[[2]] för att hitta grupperingar
  current_group <- "root"  # Starta med "root" som den första gruppen
  
  for (i in seq_along(svg_lista$groups[[2]]$children)) {
    node <- svg_lista$groups[[2]]$children[[i]]
    class_attr <- xml_attr(node, "class")
    
    if (grepl("node--root|node", class_attr) && !grepl("node--leaf", class_attr)) {
      # Om noden är en "transform" (ej leaf), uppdatera current_group
      current_group <- texter[i]  # Använd texten i motsvarande position som gruppnamn
      df$typ[i] <- "huvud"
    } else df$typ[i] <- "nod"
    
    # Tilldela current_group till respektive rad i df
    df$grupp[i] <- current_group
  }
  
  df <- df %>% mutate(grupp = ifelse(namn == "root", "root", grupp)) 
  
  return(df)
} # slut funktion

svg_namnge_element_i_lista <- function(svg_lista) {
  # Namnge elementen i svg_lista$groups baserat på textinnehåll eller attribut
  
  # Namnge texter baserat på textinnehåll i groups[[3]]
  names(svg_lista$groups[[3]]$children) <- map_chr(seq_along(svg_lista$groups[[3]]$children), function(i) {
    node_typ <- xml_attr(svg_lista$groups[[2]]$children[[i]], "class")
    text_innehall <- xml_text(svg_lista$groups[[3]]$children[[i]])
    #text_content <- xml_text(node)
    if (str_detect(node_typ, "node--root")) {
      "root"
    } else if (nchar(text_innehall) > 0) {
      text_innehall
    } else {
      "tom"
    }
  })
    
    # Kopiera namnen från groups[[3]] till groups[[2]]
    names(svg_lista$groups[[2]]$children) <- names(svg_lista$groups[[3]]$children)
    
    # namnge de övergripande grupperna
    names(svg_lista$groups) <- map_chr(svg_lista$groups, function(group) {
      if (all(map_chr(group$children, xml_name) == "circle")) {
        return("cirklar")
      } else if (all(map_chr(group$children, xml_name) == "text")) {
        return("texter")
      } else {
        return("overgripande")
      }
    })
    
    return(svg_lista)
} # slut funktion

svg_element_ta_bort_med_namn <- function(svg_lista, meta, namn) {
  # Funktion för att ta bort element ur svg_lista baserat på deras namn, går att skicka flera namn på en gång
  
  # Enkel felkontroll
  if (all(!namn %in% meta$namn)) stop("Namnen finns inte i svg-filen")
  
  # Hitta index för de element som ska tas bort, bara de som finns i svg-filen kommer med
  element_att_ta_bort <- which(meta$namn %in% namn)
  
  # Ta bort de valda elementen i första gruppen (groups[[1]])
  circle_nodes_1 <- xml_children(svg_lista$groups[[1]]$children[[1]])
  circle_nodes_2 <- xml_children(svg_lista$groups[[1]]$children[[2]])
  walk(element_att_ta_bort, ~ xml_remove(circle_nodes_1[.x]))
  walk(element_att_ta_bort, ~ xml_remove(circle_nodes_2[.x]))
  
  # Ta bort de valda elementen i andra och tredje gruppen
  svg_lista$groups[[2]]$children <- svg_lista$groups[[2]]$children[-element_att_ta_bort]
  svg_lista$groups[[3]]$children <- svg_lista$groups[[3]]$children[-element_att_ta_bort]
  
  # Ta bprt de valda elementen ur meta-tabellen
  meta <- meta[-element_att_ta_bort, ]
  
  retur_lista <- list(svg_lista = svg_lista,
                      meta = meta)
  return(retur_lista)
} # slut funktion


svg_element_ta_bort_med_grupp <- function(svg_lista, meta, grupp, element = c("cirkel", "text"), element_typ = c("huvud", "nod")) {
  # Funktion för att ta bort element ur svg_lista baserat på deras namn, går att skicka flera namn på en gång
  
  # Enkel felkontroll
  if (all(!grupp %in% meta$grupp)) stop("Namnen finns inte i svg-filen")
  
  # Hitta index för de element som ska tas bort
  element_att_ta_bort <- which(meta$grupp %in% grupp & meta$typ %in% element_typ)
  
  # Ta bort de valda elementen i första gruppen (groups[[1]])
  circle_nodes_1 <- xml_children(svg_lista$groups[[1]]$children[[1]])
  circle_nodes_2 <- xml_children(svg_lista$groups[[1]]$children[[2]])
  if ("cirkel" %in% element) walk(element_att_ta_bort, ~ xml_remove(circle_nodes_1[.x]))
  if ("text" %in% element) walk(element_att_ta_bort, ~ xml_remove(circle_nodes_2[.x]))
  
  # Ta bort de valda elementen i andra och tredje gruppen
  if ("cirkel" %in% element) svg_lista$groups[[2]]$children <- svg_lista$groups[[2]]$children[-element_att_ta_bort]    # cirkel
  if ("text" %in% element) svg_lista$groups[[3]]$children <- svg_lista$groups[[3]]$children[-element_att_ta_bort]    # text
  
  # Ta bprt de valda elementen ur meta-tabellen
  meta <- meta[-element_att_ta_bort, ]
  
  retur_lista <- list(svg_lista = svg_lista,
                      meta = meta)
  
  return(retur_lista)
} # slut funktion


svg_skala_cirklar_ordna <- function(svg_lista, meta, typsnitt_storlek = 20, mellanrum = 3) {
  
  skala_index <- meta %>% filter(grupp == "skala") %>% row_number()
  
  # Hitta gruppen "skala" och extrahera cirklarna i den
  skala_cirklar <- svg_lista$groups[[2]]$children[skala_index]  
  
  # ta reda på höjden på samtliga skalbubblor + mellanrum
  skala_cirklar_hojd <- map_dbl(skala_cirklar, ~ as.numeric(xml_attr(.x, "r"))) %>% 
    sum() %>% 
    {. + (mellanrum * (length(skala_cirklar)-1))}
  
  # Sortera cirklarna i fallande ordning baserat på radie
  sorterade_cirklar <- skala_cirklar[order(map_dbl(skala_cirklar, ~ as.numeric(xml_attr(.x, "r"))), decreasing = TRUE)]
  
  # y-värdet för alla data-cirklar
  mitt_y_data <- svg_grupp_position_hamta("data", svg_inlast, meta_df)$mitt_y
  
  # Hämta x- och y-positionen för den största cirkeln och låt den ligga kvar
  start_x <- as.numeric(xml_attr(sorterade_cirklar[[1]], "transform") %>% sub("translate\\((.*),.*", "\\1", .))
  start_y <- as.numeric(xml_attr(sorterade_cirklar[[1]], "transform") %>% sub(".*,(.*)\\)", "\\1", .))
  start_r <- as.numeric(xml_attr(sorterade_cirklar[[1]], "r"))
  
  # beräkna y_värde för största cirkeln
  #nytt_y_varde <- mitt_y_data + (skala_cirklar_hojd/2) - (start_r / 2)
  # Beräkna det nya y-värdet för den största cirkeln
  nytt_y_varde <- mitt_y_data + (skala_cirklar_hojd / 2)
  
  # Placera varje mindre cirkel ovanför den största, med mellanrum
  for (i in seq_along(sorterade_cirklar)) {
    radie <- as.numeric(xml_attr(sorterade_cirklar[[i]], "r"))
    
    # Beräkna ny y-position för varje cirkel
    if (i == 1) {
      ny_y <- nytt_y_varde           # start_y  # Första (största) cirkeln behåller sin ursprungliga y-position
    } else {
      # Placera nästa cirkel ovanför den tidigare och lägg till mellanrum
      tidigare_radie <- as.numeric(xml_attr(sorterade_cirklar[[i - 1]], "r"))
      ny_y <- ny_y - (tidigare_radie + radie + mellanrum)  # Flytta uppåt med extra mellanrum
    }
    
    # Uppdatera transform-attributet för cirkeln
    ny_transform <- sprintf("translate(%s,%s)", start_x, ny_y)
    xml_set_attr(sorterade_cirklar[[i]], "transform", ny_transform)
  }
  
  # Uppdatera svg_lista_ny med nya positioner för "skala"-cirklarna
  svg_lista$groups[[2]]$children[skala_index] <- sorterade_cirklar
  
  
  # 5. Flytta etiketterna för skalbubblorna ==============================================================================
  
  # Sortera cirklar och motsvarande textetiketter enligt radie i fallande ordning
  #sorterade_index <- skala_index[order(map_dbl(skala_index, ~ as.numeric(xml_attr(svg_lista_ny$groups[[2]]$children[[.]], "r"))), decreasing = FALSE)]
  sorterade_index <- order(map_dbl(sorterade_cirklar, ~ as.numeric(xml_attr(.x, "r"))), decreasing = TRUE)
  
  # Sortera cirklarna och textetiketterna enligt de sorterade indexen
  sorterade_cirklar <- sorterade_cirklar[sorterade_index]
  sorterade_texter <- svg_lista$groups[[3]]$children[sorterade_index] %>% rev()
  
  # Bestäm x-positionen för etiketterna (lite till höger om den största cirkeln)
  text_x_position <- start_x + (start_r / 2) + mellanrum  # Justera detta värde för önskat avstånd till höger om cirklarna
  
  # Uppdatera varje cirkel och motsvarande textetikett baserat på de sorterade indexen
  for (i in seq_along(sorterade_index)) {
    # Hämta cirkelns och textetikettens index från de sorterade indexen
    cirkel_nod <- sorterade_cirklar[[i]]
    text_nod <- sorterade_texter[[i]]
    
    # Hämta y-position för cirkelns mittpunkt
    cirkel_y <- as.numeric(xml_attr(cirkel_nod, "transform") %>% sub(".*,(.*)\\)", "\\1", .))
    
    text_y_position <- cirkel_y + (typsnitt_storlek/2)  # Justera för att centrera textetiketten
    
    # Formatera textinnehållet med tusenseparator
    text_innehall <- xml_text(text_nod)
    text_innehall_formaterat <- format(as.numeric(text_innehall), big.mark = " ", scientific = FALSE)
    xml_text(text_nod) <- text_innehall_formaterat
    
    # Bestäm ny y-position för textetiketten för att matcha cirkelns y-position
    ny_text_transform <- sprintf("translate(%s,%s)", text_x_position, text_y_position)
    
    # Uppdatera transform-attributet för textetiketten och justera textankaret för högerjustering
    xml_set_attr(text_nod, "transform", ny_text_transform)
    xml_set_attr(text_nod, "text-anchor", "end")
    
    # Ändra storlek på typsnittet till tredubbel storlek
    xml_set_attr(text_nod, "style", glue("font-size: {typsnitt_storlek}px; font-family: Arial, Helvetica;"))
  }
  return(svg_lista)
} # slut funktion

svg_grupp_position_hamta <- function(grupp, svg_lista, meta, inkludera_texter = TRUE) {
  # En funktion för att hitta ytterkanterna samt mittvärdena i x- och y-led för en grupp av cirklar i en SVG-fil
  
  # Hitta index för cirklar som tillhör den angivna gruppen
  grupp_index <- which(meta$grupp == grupp)
  
  # Hämta cirklarna i den angivna gruppen
  grupp_cirklar <- svg_lista$groups[[2]]$children[grupp_index]  
  
  # Hämta texterna i den angivna gruppen om inkludera_texter är TRUE
  if (inkludera_texter) {
    grupp_texter <- svg_lista$groups[[3]]$children[grupp_index]
    grupp_element <- c(grupp_cirklar, grupp_texter)
  } else {
    grupp_element <- grupp_cirklar
  }
  
  # Hämta x- och y-positionen för varje cirkel samt deras radier
  positioner <- map(grupp_element, function(element) {
    x <- as.numeric(xml_attr(element, "transform") %>% sub("translate\\((.*),.*", "\\1", .))
    y <- as.numeric(xml_attr(element, "transform") %>% sub(".*,(.*)\\)", "\\1", .))
    r <- as.numeric(xml_attr(element, "r"))
    if (is.na(r)) r <- 0  # Om radieattributet saknas, sätt till 0
    list(x = x, y = y, r = r)
  })
  
  # Beräkna gränserna
  vanster <- min(map_dbl(positioner, ~ .x$x - .x$r))
  hoger <- max(map_dbl(positioner, ~ .x$x + .x$r))
  over <- min(map_dbl(positioner, ~ .x$y - .x$r))
  under <- max(map_dbl(positioner, ~ .x$y + .x$r))
  
  # Beräkna mittpunkten
  mitt_x <- (vanster + hoger) / 2
  mitt_y <- (over + under) / 2
  
  retur_lista <- list(mitt_x = mitt_x, mitt_y = mitt_y, vanster = vanster, hoger = hoger, over = over, under = under)
  
  # Returnera mittpunkten och gränspunkterna
  return(retur_lista)
}


svg_grupp_flytta <- function(svg_lista, meta, grupp, flytta_x = 0, flytta_y = 0) {
  # Hitta index för cirklar som tillhör den angivna gruppen
  grupp_index <- which(meta$grupp == grupp)
  
  # Kontrollera om gruppen finns i meta
  if (length(grupp_index) == 0) {
    stop(paste("Gruppen", grupp, "finns inte i meta."))
  }
  
  # Hämta cirklarna i den angivna gruppen
  grupp_cirklar <- svg_lista$groups[[2]]$children[grupp_index]
  grupp_texter <- svg_lista$groups[[3]]$children[grupp_index]
  
  # Flytta varje cirkel i gruppen
  walk(c(grupp_cirklar, grupp_texter), function(element) {
    # Hämta nuvarande x- och y-koordinater från transform-attributet
    current_transform <- xml_attr(element, "transform")
    current_x <- as.numeric(sub("translate\\((.*),.*", "\\1", current_transform))
    current_y <- as.numeric(sub(".*,(.*)\\)", "\\1", current_transform))
    
    # Beräkna nya positioner
    new_x <- current_x + flytta_x
    new_y <- current_y - flytta_y
    
    # Uppdatera transform-attributet med de nya värdena
    new_transform <- paste0("translate(", new_x, ",", new_y, ")")
    xml_set_attr(element, "transform", new_transform)
  })
  # Returnera den uppdaterade svg_listan
  return(svg_lista)
}


svg_justera_canvas <- function(svg_lista) {
  
  # Hämta nuvarande bredd och höjd från svg_attributes
  svg_width <- as.numeric(svg_lista$svg_attributes["width"])
  svg_height <- as.numeric(svg_lista$svg_attributes["height"])
  
  # Hämta alla element från SVG-listan
  alla_element <- c(svg_lista$groups[[2]]$children, svg_lista$groups[[3]]$children)
  
  # Beräkna min- och max-koordinater för x och y för alla element
  koordinater <- map(alla_element, function(element) {
    x <- as.numeric(sub("translate\\((.*),.*", "\\1", xml_attr(element, "transform")))
    y <- as.numeric(sub(".*,(.*)\\)", "\\1", xml_attr(element, "transform")))
    r <- as.numeric(xml_attr(element, "r"))
    
    if (!is.na(r)) {
      # För cirklar, inkludera radien i beräkningen av koordinaterna
      list(min_x = x - r, max_x = x + r, min_y = y - r, max_y = y + r)
    } else {
      # För texter, behandla dem som punkter (kan justeras om de har en bredd)
      list(min_x = x, max_x = x, min_y = y, max_y = y)
    }
  })
  
  # Extrahera de yttersta koordinaterna
  min_x <- min(map_dbl(koordinater, "min_x"), na.rm = TRUE)
  max_x <- max(map_dbl(koordinater, "max_x"), na.rm = TRUE)
  min_y <- min(map_dbl(koordinater, "min_y"), na.rm = TRUE)
  max_y <- max(map_dbl(koordinater, "max_y"), na.rm = TRUE)
  
  # Flytta allt om det behövs så att min_x och min_y börjar vid 0
  if (min_x < 0 || min_y < 0) {
    flytta_x <- ifelse(min_x < 0, abs(min_x), 0)
    flytta_y <- ifelse(min_y < 0, abs(min_y), 0)
    
    walk(alla_element, function(element) {
      current_transform <- xml_attr(element, "transform")
      current_x <- as.numeric(sub("translate\\((.*),.*", "\\1", current_transform))
      current_y <- as.numeric(sub(".*,(.*)\\)", "\\1", current_transform))
      new_transform <- paste0("translate(", current_x + flytta_x, ",", current_y + flytta_y, ")")
      xml_set_attr(element, "transform", new_transform)
    })
  }
  
  # Uppdatera SVG-bredd och höjd baserat på de maximala koordinaterna
  ny_bredd <- max_x + ifelse(min_x < 0, abs(min_x), 0)
  ny_hojd <- max_y + ifelse(min_y < 0, abs(min_y), 0)
  
  svg_lista$svg_attributes["width"] <- as.character(ny_bredd)
  svg_lista$svg_attributes["height"] <- as.character(ny_hojd)
  
  return(svg_lista)
}
