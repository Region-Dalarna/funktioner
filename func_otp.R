# ========================================================================================================
#
# Skript för att starta en OTP-server för att kunna köra Swecos skript för tillgänglighet med kollektiva
# färdmedel. Servern behöver sättas upp innan den kan sättas igång, instruktioner för hur detta görs finns
# på G:\Samhällsanalys\GIS\projekt\SWECO\OTP. 
# 
# OTP-servern körs på en serveryta som nås via fjärrskrivbord. Därefter kan skripten som ligger i samma
# mapp som instruktionerna ovan köras. 
#
# Peter Möller, Region Dalarna, juni 2024
#
# ========================================================================================================
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse, 
       httr)

# kolla process-id (pid) för processer som är igång med programmet som skickas med parametern
# program_fil (default = "java.exe")
hamta_pid_for_processer <- function(program_fil = "java.exe") {    # ska vara en exefil
  
  pid <- system(paste0('tasklist /FI "IMAGENAME eq ', program_fil,  '" /FO CSV'), intern = TRUE)
  
  if (all(pid != "INFO: No tasks are running which match the specified criteria.")) {
    # Extrahera PIDs från output
    pid_list <- read_csv(paste(pid, collapse = "\n"), show_col_types = FALSE)
    
    # Filter out rows that don't have a valid PID (e.g., header rows)
    pid_list <- pid_list[grep("java.exe", pid_list$`Image Name`), ]
    
  } else pid_list <- NULL
  
  return(pid_list)  
}

# Funktion för att hitta och döda en process (default = "java.exe")
stang_otp_server <- function(program_fil = "java.exe", 
                             undanta_pid = NA,                 # vektor med pid-id för processer som INTE ska stängas
                             stang_pid = NA                    # vektor med pid_id som SKA stängas, trumfar undantag ovan
                             ) {

  if (!all(is.na(stang_pid))) {
   stang_lista <- stang_pid 
    
  } else {
    pid_list <- hamta_pid_for_processer()
    
    # om man vill låta några processer vara igång
    if (!all(is.na(undanta_pid))) {
      pid_list <- pid_list[!pid_list$PID %in% undanta_pid,]
    }
    
    stang_lista <- pid_list$PID
  }
  
  # Avsluta alla Java-processer (du kan justera detta om du har flera Java-processer och vill identifiera rätt process)
  if (length(stang_lista) > 0) {
    for (pid in stang_lista) {
      system(paste('taskkill /PID', pid, '/F'))
    }
    message("Servern har stängts.")
  } else message("Inga processer har stängts.")
  
}

# Funktion för att testa om en otp-server är igång
testa_otp_server <- function(otp_url = 'http://localhost:8801'){
  
  # Skicka en GET-förfrågan till OTP-servern
  response <- GET(otp_url)
  
  # Kontrollera statuskoden
  if (status_code(response) == 200) {
    cat("OTP-servern är igång och tillgänglig.\n")
  } else {
    cat("OTP-servern är inte tillgänglig. Statuskod:", status_code(response), "\n")
  }
}

# Funktion för att starta en otp-server (kräver att den är uppsatt på korrekt sätt innan)
starta_otp_server <- function() {
  
  old_http_proxy <- Sys.getenv("http_proxy")
  old_https_proxy <- Sys.getenv("https_proxy")
  # vi behöver stänga av proxy för att otp-lösningen ska fungera
  Sys.setenv(http_proxy = "")
  Sys.setenv(https_proxy = "")

  # för att kolla vilka pid-id:n som eventuellt redan körs för java.exe, så att vi kan stänga av enbart de processer vi startar nedan
  fore_processer <- hamta_pid_for_processer() 
  if(!is.null(fore_processer)) fore_processer <- fore_processer %>% dplyr::pull(PID)
  
  # Starta servern i ett nytt fönster på Windows
  shell('start cmd /c "cd /d C:/otp/ && java -Xmx3G -jar otp-2.3.0-shaded.jar --load --serve --port 8801 --securePort 8802 ./data_otp"')

  Sys.sleep(2)            # för att processerna ska hinna igång och kunna fångas upp av nästa rad i skriptet
  efter_processer <- hamta_pid_for_processer() %>% dplyr::pull(PID)
  
  if(length(fore_processer) > 0) efter_processer <- efter_processer[!efter_processer %in% fore_processer]
  if(length(efter_processer) == 0) efter_processer <- NULL
  #Sys.setenv(http_proxy = old_http_proxy)
  #Sys.setenv(https_proxy = old_https_proxy)
  
  return(efter_processer)
}
