
library(tidyverse)
library(xml2)


svg_mapp <- "C:/Users/moepet/Downloads/"
svg_indatafil <- paste0(svg_mapp, "Dalarna_kon_aug_2024.svg")


typsnitt_storlek <- 20            # typsnitts-storlek för skalcirkel-etiketterna
mellanrum <- 3                    # Mellanrum mellan skal-cirklarna

# =================== skapa hanterbar lista med xml-objekt (cirklar och text) =======================================

# Funktion för att läsa SVG och koppla ihop <g>-grupper med deras innehåll
svg_koppla_ihop_grupper <- function(svg_indatafil) {
  # Läs in SVG-filen
  svg <- read_xml(svg_indatafil)
  
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

# ============= skriv tillbaka matchade circles till svg-fil =====================================

# Funktion för att skapa en ny SVG-fil från svg_lista
svg_skriv_fil_fran_svg_lista <- function(svg_lista, svgfil_ny) {
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

# # Använd funktionen för att skriva ut svg_list till en ny SVG-fil
# output_file <- paste0(svg_mapp, "Dalarna_kon_aug_2024_ny6.svg")
# svg_skriv_fil_fran_svg_lista(svg_lista_ny , output_file)
# #svg_skriv_fil_fran_svg_lista(svg_lista , output_file)


# 1. Skapa en lista med cirklar och textetiketter från SVG-filen ============================================================

svg_lista <- svg_koppla_ihop_grupper(svg_indatafil)

# 2. Skapa en dataframe med namn så att vi enkelt kan hålla reda på cirklarna och gruppera dem i olika grupper som de tillhör ==============================================================

# Extrahera textinnehåll från det första elementet i varje text-nod
texter <- map_chr(svg_lista$groups[[3]]$children, xml_text)
namn_vektor <- c("root", texter[-1])  # Exkludera första elementet, eftersom det är tomt för root

df <- tibble(
  #radnummer = seq_along(namn_vektor),
  texter = namn_vektor,
  grupp = NA_character_
)

# Iterera över elements i groups[[2]] för att hitta grupperingar
current_group <- "root"  # Starta med "root" som den första gruppen

for (i in seq_along(svg_lista$groups[[2]]$children)) {
  node <- svg_lista$groups[[2]]$children[[i]]
  class_attr <- xml_attr(node, "class")
  
  if (grepl("node--root|node", class_attr) && !grepl("node--leaf", class_attr)) {
    # Om noden är en "transform" (ej leaf), uppdatera current_group
    current_group <- texter[i]  # Använd texten i motsvarande position som gruppnamn
  }
  
  # Tilldela current_group till respektive rad i df
  df$grupp[i] <- current_group
}

# 3. Ta bort de tre största cirklarna som vi inte vill ha kvar ============================================================
svg_lista_ny <- svg_lista

# Ta bort elementen 1, 2 och 8 i varje grupp (groups[[1]], groups[[2]], groups[[3]])
element_att_ta_bort <- which(df$texter %in% c("root", "skala", "data"))


svg_lista_ny$groups[[1]]$children <- svg_lista_ny$groups[[1]]$children[-element_att_ta_bort]
svg_lista_ny$groups[[2]]$children <- svg_lista_ny$groups[[2]]$children[-element_att_ta_bort]
svg_lista_ny$groups[[3]]$children <- svg_lista_ny$groups[[3]]$children[-element_att_ta_bort]
df <- df[-element_att_ta_bort, ]

svg_lista_korrekt <- svg_lista_ny

# 4. Flytta på cirklarna i gruppen "skala" ==============================================================================

svg_lista_ny <- svg_lista_korrekt

skala_index <- df %>% filter(grupp == "skala") %>% row_number()

# Hitta gruppen "skala" och extrahera cirklarna i den
# Anta att "skala"-cirklarna är på positionerna 3 till 7, justera vid behov
skala_cirklar <- svg_lista_ny$groups[[2]]$children[skala_index]  

# Sortera cirklarna i fallande ordning baserat på radie
sorterade_cirklar <- skala_cirklar[order(map_dbl(skala_cirklar, ~ as.numeric(xml_attr(.x, "r"))), decreasing = TRUE)]

# Hämta x- och y-positionen för den största cirkeln och låt den ligga kvar
start_x <- as.numeric(xml_attr(sorterade_cirklar[[1]], "transform") %>% sub("translate\\((.*),.*", "\\1", .))
start_y <- as.numeric(xml_attr(sorterade_cirklar[[1]], "transform") %>% sub(".*,(.*)\\)", "\\1", .))
start_r <- as.numeric(xml_attr(sorterade_cirklar[[1]], "r"))

# Placera varje mindre cirkel ovanför den största, med mellanrum
for (i in seq_along(sorterade_cirklar)) {
  radie <- as.numeric(xml_attr(sorterade_cirklar[[i]], "r"))
  
  # Beräkna ny y-position för varje cirkel
  if (i == 1) {
    ny_y <- start_y  # Första (största) cirkeln behåller sin ursprungliga y-position
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
svg_lista_ny$groups[[2]]$children[skala_index] <- sorterade_cirklar

# 5. Flytta etiketterna för skalbubblorna ==============================================================================

# Sortera cirklar och motsvarande textetiketter enligt radie i fallande ordning
#sorterade_index <- skala_index[order(map_dbl(skala_index, ~ as.numeric(xml_attr(svg_lista_ny$groups[[2]]$children[[.]], "r"))), decreasing = FALSE)]
sorterade_index <- order(map_dbl(sorterade_cirklar, ~ as.numeric(xml_attr(.x, "r"))), decreasing = TRUE)

# Sortera cirklarna och textetiketterna enligt de sorterade indexen
sorterade_cirklar <- sorterade_cirklar[sorterade_index]
sorterade_texter <- svg_lista_ny$groups[[3]]$children[sorterade_index] %>% rev()

# Bestäm x-positionen för etiketterna (lite till höger om den största cirkeln)
text_x_position <- start_x + (start_r * 2) + mellanrum  # Justera detta värde för önskat avstånd till höger om cirklarna

# Uppdatera varje cirkel och motsvarande textetikett baserat på de sorterade indexen
for (i in seq_along(sorterade_index)) {
  # Hämta cirkelns och textetikettens index från de sorterade indexen
  cirkel_nod <- sorterade_cirklar[[i]]
  text_nod <- sorterade_texter[[i]]
  
  # cirkel_nod <- svg_lista_ny$groups[[2]]$children[[sorterade_index[i]]]
  # text_nod <- svg_lista_ny$groups[[3]]$children[[sorterade_index[i]]]
  
  
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

# 6. Skapa en ny SVG-fil med de uppdaterade positionerna ===============================================================


output_file <- paste0(svg_mapp, "Dalarna_kon_aug_2024_ny6.svg")
svg_skriv_fil_fran_svg_lista(svg_lista_ny , output_file)
