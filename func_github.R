
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       git2r,
       keyring,
       glue)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R")

skicka_filer_till_github <- function(lokalt_repo_sokvag,        # sökväg till en mapp som utgör ett lokalt Github-repository
                                     filnamn_gh_push,           # filnamn utan sökväg, utöver undermappar i den lokala repository-mappen
                                     commit_message = NA,       # commit-meddelande, NA om man  vill köra det automatiska "Uppdaterat av r-skript automatiskt: <datum> <tid>
                                     pull_forst = TRUE) {       # gör en pull() innan man gör en push() till github

  # =======================================================================================================================
  #
  # Funktioner för att skripta mot Github. Ett sätt att slippa göra commits, pull och push etc. manuellt med klick i 
  # R-studio. Använder man credentials, vilket vi gör nedan, kan man köra dessa skript utan R-studio och därmed schema-
  # lägga dessa skript om man vill. 
  #
  # För att detta ska fungera behöver man använda följande keyring-konton:
  #         1. Service: "git2r", anv: <github användarnamn>, pwd: <mailadress kopplat till github-kontot>
  #         2. Service: "github", anv: <github användarnamn>, pwd: <lösenord på github>
  #         3. Service: "github_token", anv: <github användarnamn>, pwd: <personal access token som man skapar på github>
  # 
  # Man skapar ett personal access token på sin Github-användare på github.com genom att välja:
  #                                            Settings - Developer settings - Personal access tokens - Tokens (classic)
  # och där skapar man ett personal token som lägger in i sin keyring enligt ovan.
  #
  # =======================================================================================================================
  
  
  # filerna ska redan vara skrivna till mappen som tillhör repositoriet för att detta ska fungera
  if (in_repository(lokalt_repo_sokvag)) {
    
    repo_lokalt <- repository(lokalt_repo_sokvag)                          # initiera repositoriet
    
    git2r::config(repo_lokalt, user.name = key_list(service = "git2r")$username, user.email = key_get("git2r", key_list(service = "git2r")$username))
    
    git2r::add( repo = repo_lokalt,            # först gör vi en stage av filen/filerna
         path = filnamn_gh_push)
    
    if (is.na(commit_message)) commit_message <- paste0("Updaterat av r-skript automatiskt: ", Sys.time())
    
    git2r::commit( repo = repo_lokalt,
            message = commit_message)    # sen gör vi en commit på de filer som har stage:ats
    
    # först en pull
    if (pull_forst){
      git2r::pull( repo = repo_lokalt,                 
            credentials = cred_user_pass( username = key_list(service = "github")$username, 
                                          password = key_get("github", key_list(service = "github")$username)))
    } # slut if-sats där man kan stänga av att man kör en pull först (inte att rekommendera)
    
    # och sedan en push
    git2r::push( object = repo_lokalt,               
          credentials = cred_user_pass( username = key_list(service = "github_token")$username, 
                                        password = key_get("github_token", key_list(service = "github_token")$username)))
    
  } else warning(paste0("Sökvägen '", lokalt_repo_sokvag, "' finns inte som lokalt Github-repository, kontrollera sökvägen och försök igen."))
  
} # slut funktion


skapa_webbrapport_github <- function(githubmapp_lokalt,                 # sökväg till den mapp där du har github-repos (ska INTE innehålla själva repositoryt)
                                     github_repo,                       # namn på själva github-repot, döper mappen och github-repot. Mappen skapas om den inte finns
                                     github_org = "Region-Dalarna",     # ändra till NULL om man vill lägga repo:t i sin privata github
                                     rapport_titel,                     # titel på rapporten i RMarkdown
                                     rapport_undertitel = NA) {         # om man vill ha en undertitel så lägger man in den här
  
  githubmapp_lokalt <- githubmapp_lokalt %>% str_replace_all(fixed("\\"), "/")
  if (str_sub(1, nchar(githubmapp_lokalt)) != "/") githubmapp_lokalt <- paste0(githubmapp_lokalt, "/")
  
  
  sokvag_proj <- paste0(githubmapp_lokalt, github_repo)
  if (str_sub(1, nchar(sokvag_proj)) != "/") sokvag_proj <- paste0(sokvag_proj, "/")
  
  # skapa_mapp_om_den_inte_finns(sokvag_proj)
  # 
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
  # 
  # # skriv .Rproj-fil till hårddisken
  # writeLines(str_proj_fil, paste0(sokvag_proj, github_repo, ".Rproj"))
  
  # skapa r-projekt
  create_project(sokvag_proj, rstudio = rstudioapi::isAvailable(), open = rlang::is_interactive())
  
  
  # skapa övriga mappar vi brukar ha
  skapa_mapp_om_den_inte_finns(glue("{sokvag_proj}Diagram"))
  skapa_mapp_om_den_inte_finns(glue("{sokvag_proj}docs"))
  skapa_mapp_om_den_inte_finns(glue("{sokvag_proj}Skript"))
  
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
      styles_hero_css = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/styles_hero.css"
  )
  
  filnamn <- map_chr(list_filer, ~ str_extract(.x, "[^/]+$"))
  
  walk(list_filer, ~ {
    filnamn <- str_extract(.x, "[^/]+$")
    download.file(.x, paste0(sokvag_proj, filnamn), mode = "wb")
  })
  
  # ============================================== vi skapar nu själva .Rmd-filen ===================================================
  
  # vi börjar med headern i webbrapporten
  rmd_header <- glue('
---
title: {rapport_titel}
author: ""
date: ""
output: 
  html_document:
    includes:
      in_header: hero_image.html
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
p_load(here,
       tidyverse,
       openxlsx,
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
# source(here('Skript','1_hamta_data.R'), encoding='UTF-8')
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
  

# ================================================== skicka upp till github =================================================  


skapa_git_och_github_repo_med_pages <- glue('
if (!require("pacman")) install.packages("pacman")
p_load(usethis)

# skapa git först
use_git(message = "Skapa repository")

# därefter github

use_github(
  organisation = {github_org},
  private = FALSE,
  visibility = "public"
)

use_github_pages(branch = git_default_branch(), path = "/docs", cname = NA)
')

# Vi skriver filen till mappen
writeLines(skapa_git_och_github_repo_med_pages, paste0(sokvag_proj, "skapa_github_repo.R"))

message("Ett nytt R-projekt har skapats och öppnats i en ny session i R-studio. I det nya projektet ligger skriptet 'skapa_github_repo.R'. Kör det skriptet för att skapa ett Github-repository och aktivera Github Pages för det repositoryt.")

  
} # slut funktion

github_commit_push <- function(
    sokvag_lokal_repo = "c:/gh/",
    repo_namn = "hamta_data",
    repo_org = "Region-Dalarna",
    commit_txt = NA,
    pull_forst = TRUE) {
  
  lokal_sokvag_repo <- paste0(sokvag_lokal_repo, repo_namn)
  
  push_repo <- git2r::init(lokal_sokvag_repo)
  repo_status <- git2r::status(push_repo)
  
  if (length(repo_status$untracked) > 0) {
    # hämta ner en lista med filer som finns i remote repot
    github_fillista <- github_lista_repo_filer(owner = repo_org,
                                               repo = repo_namn,
                                               url_vekt_enbart = FALSE,
                                               skriv_source_konsol = FALSE)$namn
    
    filer_uppdatering <- repo_status$untracked[repo_status$untracked %in% github_fillista] 
    filer_nya <- repo_status$untracked[!repo_status$untracked %in% github_fillista]
    
    if (is.na(commit_txt)) {
      commit_txt <- case_when(length(filer_uppdatering) > 0 & length(filer_nya) > 0 ~ 
                                paste0(length(filer_nya), " filer har lagts till och ", length(filer_uppdatering), 
                                       " filer har uppdaterats."),
                              length(filer_uppdatering) > 0 & length(filer_nya) == 0 ~
                                paste0(length(filer_uppdatering), " filer har uppdaterats."),
                              length(filer_uppdatering) == 0 & length(filer_nya) > 0 ~
                                paste0(length(filer_nya), " filer har lagts till."))
    }
    git2r::add(push_repo, path = repo_status$untracked %>% as.character())
    git2r::commit(push_repo, commit_txt)
    
    # först en pull
    if (pull_forst){
      git2r::pull( repo = push_repo,                 
                   credentials = cred_user_pass( username = key_list(service = "github")$username, 
                                                 password = key_get("github", key_list(service = "github")$username)))
    } # slut if-sats där man kan stänga av att man kör en pull först (inte att rekommendera)
    
    # och sedan en push
    git2r::push( object = push_repo,               
                 credentials = cred_user_pass( username = key_list(service = "github_token")$username, 
                                               password = key_get("github_token", key_list(service = "github_token")$username)))
    
  } else {
    print("Inga nya eller uppdaterade filer att ladda upp till Github.")
  } # slut if-sats som testar om det finns filer att committa
} # slut funktion
