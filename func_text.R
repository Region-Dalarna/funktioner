if (!require("stringr")) install.packages("stringr")
# funktioner för att hantera text i olika former, strängar, vecktorer etc.

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

# en funktion för att sätta ett komma mellan varje element i en vektor
# samt ett eller mellan näst sista och sista elementet
list_komma_eller <- function(skickad_vektor){
  if (length(skickad_vektor)>1){
    nystrang <- skickad_vektor[1]
    for (elem in 2:length(skickad_vektor)){
      if (elem == length(skickad_vektor)){
        nystrang <- paste0(nystrang, " eller ", skickad_vektor[elem])
      } else {
        nystrang <- paste0(nystrang, ", ", skickad_vektor[elem])
      }
    }
  } else nystrang <- skickad_vektor[1]
  return(nystrang)
}

# en funktion för att sätta ett komma mellan varje element i en vektor
# samt ett eller mellan näst sista och sista elementet
list_komma_samt <- function(skickad_vektor){
  if (length(skickad_vektor)>1){
    nystrang <- skickad_vektor[1]
    for (elem in 2:length(skickad_vektor)){
      if (elem == length(skickad_vektor)){
        nystrang <- paste0(nystrang, " samt ", skickad_vektor[elem])
      } else {
        nystrang <- paste0(nystrang, ", ", skickad_vektor[elem])
      }
    }
  } else nystrang <- skickad_vektor[1]
  return(nystrang)
}

# funktion för att dela upp en sträng i flera rader. Kan användas för att exempelvis
# få en vettig rubrik i ett diagram i ggplot. Man skickar in en sträng, anger en 
# max-längd (eller kanske snarare bredd) på rubriken så får man tillbaka en sträng
# med radbrytningar där det passar bäst, dessa sker där det är mellanslag (om man inte väljer annan sokstrang)
dela_upp_strang_radbryt <- function(strang, max_langd, sokstrang = " "){
  returstrang <- NULL
  for (vect_strang in 1:length(strang)){
    strang_elem <- strang[vect_strang]
    antal_delar <- ceiling(nchar(strang_elem)/max_langd)         # ta fram hur många rader som vi behöver skapa
    optimal_langd <- round(nchar(strang_elem)/antal_delar)       # ta fram optimal längd om vi delar raderna lika
    strang_elem_pos <- 0                                         # variabel att spara bästa mellanslagsposition i 
    varv <- 1                                               # håll reda på varv för att hitta bästa position för varje rad
    while (nchar(strang_elem)-strang_elem_pos>max_langd) {
      # skapa matris med alla förekomster av mellanslag
      delstrang_elem <- str_locate_all(strang_elem, sokstrang) %>%
        sapply(., function(x) x[,2])
      #delstrang_elem <- as.vector(delstrang_elem)
      
      # först tar vi ut en vektor med alla mellanslag (eller annat tecken) som är maxlängd eller mindre
      str_vekt <- delstrang_elem[delstrang_elem > strang_elem_pos & delstrang_elem <= strang_elem_pos + max_langd]
      # därefter tar vi ut den position som ligger närmast optimal_langd 
      strang_elem_pos <- str_vekt[abs(str_vekt - (optimal_langd*varv)) == min(abs(str_vekt - (optimal_langd*varv)))]
      # vi byter ut det optimala mellanslaget mot en radbrytning
      strang_elem <- paste0(substr(strang_elem,1,strang_elem_pos-1), "\n", substr(strang_elem,strang_elem_pos+1, nchar(strang_elem))) 
      # öka varv med 1
      varv <- varv + 1
    }
  returstrang <- c(returstrang, strang_elem)
  }
  return(returstrang)
}

byt_ut_svenska_tecken <- function(text) {
  # Ersätt små bokstäver
  text <- str_replace_all(text, "å", "a")
  text <- str_replace_all(text, "ä", "a")
  text <- str_replace_all(text, "ö", "o")
  
  # Ersätt stora bokstäver
  text <- str_replace_all(text, "Å", "A")
  text <- str_replace_all(text, "Ä", "A")
  text <- str_replace_all(text, "Ö", "O")
  
  return(text)
}

procent_till_text <- function(procent) {
  # Funktion för att omvandla procenttal till text  (exempel: 50 -> "varannan")
  
  tolerans = 0.5
  
  case_when(
    procent == 0 ~ "ingen",
    procent > 0 & procent < 0.5 - tolerans ~ "nästan ingen",
    procent >= 0.5 & procent < 2.5 - tolerans ~ "väldigt få",
    procent >= 2.5 & procent < 5 - tolerans ~ "knappt var tjugonde",
    procent >= 5 - tolerans & procent <= 5 + tolerans ~ "var tjugonde",
    procent > 5 + tolerans & procent < 6.3 - tolerans ~ "drygt var tjugonde",
    procent >= 6.3 & procent < 6.6 - tolerans ~ "knappt var femtonde",
    procent >= 6.6 - tolerans & procent <= 6.6 + tolerans ~ "var femtonde",
    procent > 6.6 + tolerans & procent < 7.5 - tolerans ~ "drygt var femtonde",
    procent > 7.5 + tolerans & procent < 10 - tolerans ~ "knappt var tionde",
    procent >= 10 - tolerans & procent <= 10 + tolerans ~ "var tionde",
    procent > 10 + tolerans & procent < 12.3 - tolerans ~ "drygt var tionde",
    procent >= 12.3 - tolerans & procent < 14.3 - tolerans ~ "knappt var sjunde",
    procent >= 14.3 - tolerans & procent <= 14.3 + tolerans ~ "var sjunde",
    procent > 14.3 + tolerans & procent < 17.2 + tolerans ~ "drygt var sjunde",
    procent >= 17.2 - tolerans & procent < 20 - tolerans ~ "knappt var femte",
    procent >= 20 - tolerans & procent <= 20 + tolerans ~ "var femte",
    procent > 20 + tolerans & procent < 22.5 - tolerans ~ "drygt var femte",
    procent >= 22.5 - tolerans & procent < 25 - tolerans ~ "knappt var fjärde",
    procent >= 25 - tolerans & procent <= 25 + tolerans ~ "var fjärde",
    procent > 25 + tolerans & procent < 29.2 - tolerans ~ "drygt var fjärde",
    procent >= 29.2 - tolerans & procent < 33.3 - tolerans ~ "knappt var tredje",
    procent >= 33.3 - tolerans & procent <= 33.3 + tolerans ~ "var tredje",
    procent > 33.3 + tolerans & procent < 45 - tolerans ~ "drygt var tredje",
    procent >= 45 - tolerans & procent < 50 - tolerans ~ "knappt varannan",
    procent >= 50 - tolerans & procent <= 50 + tolerans ~ "varannan",
    procent > 50 + tolerans & procent < 55 - tolerans ~ "drygt varannan",
    procent >= 55 - tolerans & procent < 60 - tolerans ~ "knappt tre av fem",
    procent >= 60 - tolerans & procent <= 60 + tolerans ~ "tre av fem",
    procent > 60 + tolerans & procent < 65 - tolerans ~ "drygt tre av fem",
    procent >= 65 - tolerans & procent < 66.6 - tolerans ~ "knappt två av tre",
    procent >= 66.6 - tolerans & procent <= 66.6 + tolerans ~ "två av tre",
    procent > 66.6 + tolerans & procent < 70 - tolerans ~ "drygt två av tre",
    procent >= 70 - tolerans & procent < 75 - tolerans ~ "knappt tre av fyra",
    procent >= 75 - tolerans & procent <= 75 + tolerans ~ "tre av fyra",
    procent > 75 + tolerans & procent < 77.5 - tolerans ~ "drygt tre av fyra",
    procent >= 77.5 - tolerans & procent < 80 - tolerans ~ "knappt fyra av fem",
    procent >= 80 - tolerans & procent <= 80 + tolerans ~ "fyra av fem",
    procent > 80 + tolerans & procent < 85 - tolerans ~ "drygt fyra av fem",
    procent >= 85 - tolerans & procent < 90 - tolerans ~ "knappt nio av tio",
    procent >= 90 - tolerans & procent <= 90 + tolerans ~ "nio av tio",
    procent >= 90 + tolerans & procent < 95 + tolerans ~ "drygt nio av tio",
    procent >= 95 ~ "nästan alla",
    procent == 100 ~ "alla",
    TRUE ~ paste0(procent, "%")
  )
}



