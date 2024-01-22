
if (!require("pacman")) install.packages("pacman")
p_load(git2r,
       keyring)

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
