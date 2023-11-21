library(stringr)
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
