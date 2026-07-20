#' Starta en skrapsession (Edge + chromote + selenider)
#'
#' Startar Microsoft Edge headless med en fjärrfelsökningsport, ansluter
#' chromote till den via 127.0.0.1 (INTE localhost, som kan lösas till ::1
#' och göra att chromote timar ut trots att Edge svarar), och kopplar
#' selenider till samma session.
#'
#' Edge startas med processx::process$new(..., supervise = FALSE) istället
#' för chromotes/shell()s egna processtart, eftersom processx annars
#' spawnar en hjälpprocess (supervisor.exe) som ofta blockeras av
#' AppLocker/gruppolicy på företagsdatorer. supervise = FALSE undviker det
#' helt, samtidigt som vi får ett processobjekt med $get_pid()/$kill() att
#' städa bort sessionen med senare.
#'
#' @param port Fjärrfelsökningsport för Edge. Default NULL, vilket väljer en
#'   ledig port automatiskt (kräver paketet httpuv). Ange ett fast
#'   portnummer bara om du har ett särskilt behov av det.
#' @param headless Kör Edge utan synligt fönster. Default TRUE.
#' @param browser_path Sökväg till msedge.exe. Default hämtas från
#'   miljövariabeln SKRAP_EDGE_PATH om den är satt, annars standardsökvägen
#'   för 64-bitars Edge.
#' @param profil_dir Mapp för Edges temporära användarprofil. Default en
#'   unik temp-mapp per session (så flera sessioner kan köras parallellt).
#' @param timeout Antal sekunder att vänta på att debug-porten svarar.
#' @param view Om chromote ska visa webbläsarfönstret (kräver headless = FALSE
#'   för att synas). Default FALSE.
#' @param bredd Viewportens bredd i pixlar. chromote tvingar fram en fast
#'   virtuell skärmstorlek via Chrome DevTools-protokollet oberoende av det
#'   faktiska OS-fönstrets storlek - höj den här om sidan bara syns i en
#'   smal kolumn trots ett maximerat fönster.
#' @param hojd Viewportens höjd i pixlar.
#' @param user_agent Valfri user agent-sträng. I headless-läge innehåller
#'   standard-UA:n "HeadlessChrome", vilket enkla botskydd känner igen -
#'   ange en vanlig webbläsar-UA här för att undvika det.
#'
#' @return Ett objekt av klass "skrapsession" med fälten session (selenider-
#'   session), chrom (Chromote-objekt), process (processx-processobjekt),
#'   profil_dir och port. Skicka objektet till stang_skrapsession() när du
#'   är klar.
#'
#' @examples
#' \dontrun{
#' skrap <- starta_skrapsession()
#' on.exit(stang_skrapsession(skrap), add = TRUE)
#'
#' selenider::open_url(skrap$session, "https://www.regiondalarna.se")
#' }
starta_skrapsession <- function(port = NULL,
                                headless = TRUE,
                                browser_path = Sys.getenv(
                                  "SKRAP_EDGE_PATH",
                                  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
                                ),
                                profil_dir = tempfile("edge-profil-"),
                                timeout = 15,
                                view = FALSE,
                                bredd = 1600,
                                hojd = 1000,
                                user_agent = NULL) {
  
  if (!file.exists(browser_path)) {
    stop(
      "Hittar inte Edge pa: ", browser_path,
      "\nAnge korrekt sokvag via argumentet browser_path, ",
      "eller satt miljovariabeln SKRAP_EDGE_PATH."
    )
  }
  
  # Valj en ledig port automatiskt om ingen port angetts. Detta undviker att
  # av misstag ansluta till en gammal, kvarglomd Edge-process pa samma port
  # (vilket ger kryptiska fel som "Session and underlying target have been
  # closed"), och gor det ocksa mojligt att kora flera sessioner parallellt.
  if (is.null(port)) {
    rlang::check_installed("httpuv", reason = "for att valja en ledig port automatiskt")
    port <- httpuv::randomPort(min = 9222, max = 9999, host = "127.0.0.1")
  }
  
  dir.create(profil_dir, showWarnings = FALSE, recursive = TRUE)
  
  args <- c(
    if (isTRUE(headless)) "--headless" else "--start-maximized",
    paste0("--remote-debugging-port=", port),
    paste0("--user-data-dir=", profil_dir),
    "--no-first-run",
    "--no-default-browser-check",
    # Headless-laget avslojar sig sjalvt via "HeadlessChrome" i user
    # agent-strangen - den vanligaste signalen enkla botskydd tittar pa.
    # Med user_agent kan en vanlig UA-strang sattas aven i headless-lage.
    if (!is.null(user_agent)) paste0("--user-agent=", user_agent)
  )
  
  # supervise = FALSE ar avgorande: undviker processx supervisor.exe
  proc <- processx::process$new(
    command = browser_path,
    args = args,
    supervise = FALSE
  )
  
  # Vanta tills Edges debug-port svarar (eller processen dor)
  url <- sprintf("http://127.0.0.1:%d/json/version", port)
  ok <- FALSE
  forsok <- max(1L, ceiling(timeout * 2))
  for (i in seq_len(forsok)) {
    if (!proc$is_alive()) {
      stop(
        "Edge avslutades ovantat vid start. Kontrollera sokvag, ",
        "rattigheter och att porten ", port, " inte redan ar upptagen."
      )
    }
    ok <- tryCatch(
      {
        httr::GET(url, httr::timeout(1))
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) break
    Sys.sleep(0.5)
  }
  
  if (!ok) {
    proc$kill()
    unlink(profil_dir, recursive = TRUE, force = TRUE)
    stop("Edge svarade inte pa debug-porten inom ", timeout, " sekunder.")
  }
  
  b <- chromote::ChromeRemote$new(host = "127.0.0.1", port = port)
  chrom <- chromote::Chromote$new(browser = b)
  
  session <- selenider::selenider_session(
    options = selenider::chromote_options(parent = chrom, width = bredd, height = hojd),
    view = view,
    local = FALSE
  )
  
  structure(
    list(
      session = session,
      chrom = chrom,
      process = proc,
      profil_dir = profil_dir,
      port = port
    ),
    class = "skrapsession"
  )
}

#' Stäng en skrapsession och städa bort allt den skapade
#'
#' Stänger selenider-sessionen och chromote-anslutningen, avslutar
#' Edge-processen (bara den specifika process som starta_skrapsession()
#' startade, inte andra Edge-fönster som redan kör på datorn), och tar
#' bort den temporära profilmappen.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#'
#' @export
stang_skrapsession <- function(skrap) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  
  # Stang i ratt ordning: selenider -> chromote -> sjalva Edge-processen
  try(selenider::selenider_session_close(skrap$session), silent = TRUE)
  try(skrap$chrom$close(), silent = TRUE)
  
  if (!is.null(skrap$process) && skrap$process$is_alive()) {
    skrap$process$kill()
  }
  
  if (dir.exists(skrap$profil_dir)) {
    unlink(skrap$profil_dir, recursive = TRUE, force = TRUE)
  }
  
  invisible(TRUE)
}

# --- interna hjälpare för iframe-stöd -------------------------------------

# Bygger JavaScript som satter variabeln `dok` till ratt document:
# huvudsidans document om iframe ar NULL, annars dokumentet inuti angiven
# iframe (eller nastlade iframes om en vektor av selektorer anges).
# Fungerar for same-origin-iframes; cross-origin blockeras av weblasarens
# sakerhetsmodell och ger ett tydligt fel via kontrollera_iframe_svar().
bygg_dokument_js <- function(iframe = NULL) {
  if (is.null(iframe)) {
    return("var dok = document;")
  }
  rlang::check_installed("jsonlite")
  sprintf(
    "var dok = document;
     var framar = %s;
     for (var fi = 0; fi < framar.length; fi++) {
       var fr = dok.querySelector(framar[fi]);
       if (!fr) return 'IFRAME_SAKNAS:' + framar[fi];
       try { dok = fr.contentDocument || fr.contentWindow.document; }
       catch (e) { return 'IFRAME_KORSDOMAN:' + framar[fi]; }
       if (!dok) return 'IFRAME_KORSDOMAN:' + framar[fi];
     }",
    jsonlite::toJSON(as.character(iframe))
  )
}

# Kastar begripliga fel om JavaScript-svaret signalerar iframe-problem.
kontrollera_iframe_svar <- function(resultat) {
  if (is.character(resultat) && length(resultat) == 1) {
    if (startsWith(resultat, "IFRAME_SAKNAS:")) {
      stop("Hittade ingen iframe som matchar: ",
           sub("^IFRAME_SAKNAS:", "", resultat))
    }
    if (startsWith(resultat, "IFRAME_KORSDOMAN:")) {
      stop(
        "Iframen '", sub("^IFRAME_KORSDOMAN:", "", resultat),
        "' laddar innehall fran en annan doman (cross-origin) - da blockerar ",
        "webblasarens sakerhetsmodell atkomst via JavaScript. Enklaste ",
        "losningen ar oftast att navigera direkt till iframens src-URL med ",
        "open_url() istallet."
      )
    }
  }
  invisible(NULL)
}

#' Kör JavaScript på sidan och returnera resultatet
#'
#' Liten hjälpfunktion för felsökning och inspektion.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param js JavaScript-kod (ett uttryck vars värde returneras).
#'
#' @return Uttryckets värde, konverterat till R.
kor_js <- function(skrap, js) {
  skrap$session$driver$Runtime$evaluate(js)$result$value
}

#' Kartlägg alla kontroller på den aktuella sidan
#'
#' Läser av sidans select-listor (med alla options), klickbara element
#' (länkar och knappar) samt inmatningsfält (text, checkbox, radio).
#' Ovärderlig när man bygger ett nytt skrapskript: öppna sidan, kör denna,
#' och du ser exakt vilka id:n, values och texter du ska styra mot.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#'
#' @return En lista med tre data.frames: selects (en rad per option, med
#'   select_id, multiple, value, text, selected), klickbara (id, tag, text)
#'   och inmatning (id, typ, name, value, checked).
#'
#' @examples
#' \dontrun{
#' skrap <- starta_skrapsession(headless = FALSE)
#' selenider::open_url("https://exempel.se", session = skrap$session)
#' kontroller <- inspektera_kontroller(skrap)
#' kontroller$selects     # alla dropdowns och deras alternativ
#' kontroller$klickbara   # alla länkar och knappar
#' }
inspektera_kontroller <- function(skrap) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  rlang::check_installed("jsonlite")
  
  js <- "JSON.stringify({
    selects: [...document.querySelectorAll('select')].flatMap(s =>
      [...s.options].map(o => ({
        select_id: s.id || s.name || '(utan id)',
        multiple: s.multiple,
        value: o.value,
        text: o.text.trim(),
        selected: o.selected
      }))
    ),
    klickbara: [...document.querySelectorAll(
      'a, button, input[type=button], input[type=submit]'
    )].map(e => ({
      id: e.id || '',
      tag: e.tagName.toLowerCase(),
      text: (e.innerText || e.value || '').trim().slice(0, 60)
    })).filter(e => e.id !== '' || e.text !== ''),
    inmatning: [...document.querySelectorAll(
      'input[type=text], input[type=checkbox], input[type=radio], textarea'
    )].map(e => ({
      id: e.id || '',
      typ: e.type,
      name: e.name || '',
      value: e.value,
      checked: e.checked
    }))
  })"
  
  jsonlite::fromJSON(kor_js(skrap, js))
}

#' Skriv ut en kompakt översikt av sidans kontroller
#'
#' Som inspektera_kontroller(), men skriver en lättläst sammanfattning till
#' konsolen istället för att returnera data.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param max_options Max antal options att visa per select-lista.
visa_kontroller <- function(skrap, max_options = 10) {
  k <- inspektera_kontroller(skrap)
  
  cat("=== SELECT-LISTOR ===\n")
  if (length(k$selects) && nrow(k$selects)) {
    for (id in unique(k$selects$select_id)) {
      opts <- k$selects[k$selects$select_id == id, ]
      cat(sprintf(
        "#%s (%s, %d alternativ)\n",
        id, if (opts$multiple[1]) "flerval" else "enval", nrow(opts)
      ))
      visa <- utils::head(opts, max_options)
      cat(sprintf("   value=%-12s %s\n", visa$value, visa$text), sep = "")
      if (nrow(opts) > max_options) {
        cat("   ... och", nrow(opts) - max_options, "till\n")
      }
    }
  }
  
  cat("\n=== KLICKBARA ELEMENT (med id) ===\n")
  if (length(k$klickbara) && nrow(k$klickbara)) {
    med_id <- k$klickbara[k$klickbara$id != "", ]
    cat(sprintf("#%-40s %s\n", med_id$id, med_id$text), sep = "")
  }
  
  invisible(k)
}

#' Klicka på ett element via dess synliga text, direkt via JavaScript
#'
#' Motsvarar i praktiken Playwrights `text=`-selektor, men körs som EN
#' JavaScript-körning via Chrome DevTools-protokollet istället för att låta
#' selenider polla en xpath-fråga upprepade gånger. Använd den här hellre än
#' ett xpath-baserat elem_click() när sidan har många element med liknande
#' text (t.ex. en lång lista med "Alla kommuner" - en per län) - då kan
#' seleniders upprepade väntekontroller bli mycket långsamma (uppemot en
#' minut har setts i praktiken), eftersom varje kontroll måste läsa ihop
#' all text i hela DOM-trädet för att hitta rätt matchning.
#'
#' Klickar på det FÖRSTA elementet (i dokumentordning) vars normaliserade
#' text är exakt lika med `text`. Default-selektorn omfattar bara riktiga
#' klickbara elementtyper (`a`, `button`, `input`, `label`) - INTE generiska
#' omslagstaggar som `div`/`span`/`li`. Det är medvetet: en förälder-`<div>`
#' vars enda innehåll är en `<a>`-länk har samma trimmade text som länken
#' själv, och skulle då hittas FÖRE länken i dokumentordning (föräldrar
#' kommer alltid före sina barn) - men klick på en sådan `<div>` gör oftast
#' ingenting, eftersom klickhanteraren sitter på själva länken. Ange
#' `selector` explicit bara om du vet att målelementet är av annan typ
#' (t.ex. `"td"` för en klickbar tabellcell).
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param text Den synliga text som ska matchas exakt (efter trimning).
#' @param selector CSS-selektor som avgränsar vilka element som
#'   kontrolleras. Default täcker riktiga klickbara elementtyper.
#'
#' @return Inget (osynligt TRUE). Kastar fel om ingen match hittas.
#'
#' @examples
#' \dontrun{
#' klicka_via_text(skrap, "Alla kommuner")
#' }
klicka_via_text <- function(skrap, text, selector = "a, button, input[type=button], input[type=submit], label",
                            vanta = TRUE, grace = 0.3, timeout = 30,
                            iframe = NULL) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  rlang::check_installed("jsonlite")
  
  js <- sprintf(
    "(function(){
       %s
       var els = dok.querySelectorAll(%s);
       for (var i = 0; i < els.length; i++) {
         if (els[i].textContent.trim() === %s) {
           window.__skrapKlickMarkering = true;
           els[i].click();
           return true;
         }
       }
       return false;
     })()",
    bygg_dokument_js(iframe),
    jsonlite::toJSON(selector, auto_unbox = TRUE),
    jsonlite::toJSON(text, auto_unbox = TRUE)
  )
  
  hittad <- kor_js(skrap, js)
  kontrollera_iframe_svar(hittad)
  if (!isTRUE(hittad)) {
    stop("Hittade inget klickbart element med texten: '", text, "'")
  }
  
  if (isTRUE(vanta)) {
    vanta_pa_sidladdning(skrap, grace = grace, timeout = timeout)
  }
  invisible(TRUE)
}

#' Läs ut alla options ur en select-lista, i ett enda JavaScript-anrop
#'
#' Snabbare motsvarighet till att loopa `find_elements("option")` +
#' `elem_attr()` per alternativ (som gör ett separat CDP-anrop per
#' `<option>`, och därmed blir märkbart långsamt för listor med många
#' alternativ). Läser istället ut alla `value`/`text`-par i en enda
#' JavaScript-körning.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param css CSS-selektor för `<select>`-elementet, t.ex. `"#AR"`.
#'
#' @return En data.frame med kolumnerna `value` och `text`, en rad per
#'   `<option>`.
#'
#' @examples
#' \dontrun{
#' hamta_select_options(skrap, "#AR")
#' }
hamta_select_options <- function(skrap, css, iframe = NULL) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  rlang::check_installed("jsonlite")
  
  js <- sprintf(
    "(function(){
       %s
       var sel = dok.querySelector(%s);
       if (!sel) return 'ELEMENT_SAKNAS';
       return JSON.stringify([...sel.options].map(o => ({value: o.value, text: o.text.trim()})));
     })()",
    bygg_dokument_js(iframe),
    jsonlite::toJSON(css, auto_unbox = TRUE)
  )
  
  resultat <- kor_js(skrap, js)
  kontrollera_iframe_svar(resultat)
  
  if (is.null(resultat)) {
    stop(
      "Fick inget svar fran sidan vid avlasning av ", css, ". ",
      "Detta hander typiskt om sidan haller pa att laddas om (t.ex. efter ",
      "en postback) - vanta in ett element med elem_expect(is_visible) ",
      "innan avlasningen."
    )
  }
  if (identical(resultat, "ELEMENT_SAKNAS")) {
    stop("Hittade ingen select-lista som matchar: ", css)
  }
  
  jsonlite::fromJSON(resultat)
}

#' Klicka på ett element via dess CSS-selektor, direkt via JavaScript
#'
#' Som klicka_via_text(), men matchar på en CSS-selektor (t.ex. ett `id`)
#' istället för synlig text, och görs i en enda JavaScript-körning utan
#' seleniders auto-wait. Använd för att snabba upp klick på element du
#' redan vet finns och är synliga - eller som diagnostik för att avgöra om
#' ett långsamt elem_click() beror på seleniders väntelogik eller på att
#' sidan/servern faktiskt är långsam.
#'
#' Med `vanta = TRUE` (default) väntar funktionen automatiskt in en
#' eventuell sidnavigering/postback som klicket utlöser: en markör
#' planteras på sidan i samma ögonblick som klicket, och försvinner den har
#' en ny sida börjat laddas - då väntar funktionen tills den är helt klar.
#' Sker ingen navigering inom respitperioden `grace` släpps koden vidare
#' nästan direkt.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param css CSS-selektor för elementet som ska klickas.
#' @param vanta Om en eventuell sidladdning ska väntas in. Default TRUE.
#' @param grace Respitperiod i sekunder som en navigering får på sig att
#'   börja. Kostnaden när ingen navigering sker. Default 0.3.
#' @param timeout Max antal sekunder att vänta på att en sidladdning blir
#'   klar.
#'
#' @return Inget (osynligt TRUE). Kastar fel om inget element hittas.
klicka_via_id <- function(skrap, css, vanta = TRUE, grace = 0.3, timeout = 30,
                          iframe = NULL) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  rlang::check_installed("jsonlite")
  
  # Markören satts i SAMMA JavaScript-körning som klicket - satts den i ett
  # separat anrop finns en kapplöpning dar navigeringen hinner börja emellan.
  js <- sprintf(
    "(function(){
       %s
       var el = dok.querySelector(%s);
       if (!el) return false;
       window.__skrapKlickMarkering = true;
       el.click();
       return true;
     })()",
    bygg_dokument_js(iframe),
    jsonlite::toJSON(css, auto_unbox = TRUE)
  )
  
  hittad <- kor_js(skrap, js)
  kontrollera_iframe_svar(hittad)
  if (!isTRUE(hittad)) {
    stop("Hittade inget element som matchar: ", css)
  }
  
  if (isTRUE(vanta)) {
    vanta_pa_sidladdning(skrap, grace = grace, timeout = timeout)
  }
  invisible(TRUE)
}

#' Vänta tills ett JavaScript-villkor blir sant
#'
#' Kompletterar elem_expect()/is_visible, som bara kontrollerar DOM-tillstånd
#' (finns elementet, är det synligt). Vissa sidor har element som blir
#' synliga innan sidans egen JavaScript hunnit initiera klart (t.ex. globala
#' variabler som en onchange-hanterare förutsätter finns) - då kan ett klick
#' eller val på ett "synligt" element ändå trigga ett JavaScript-fel. Denna
#' funktion pollar ett godtyckligt JS-uttryck tills det returnerar en sann
#' boolean.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param villkor JavaScript-uttryck som ska utvärderas till en boolean,
#'   t.ex. `"typeof AR !== 'undefined'"`.
#' @param timeout Max antal sekunder att vänta.
#' @param intervall Sekunder mellan varje kontroll.
#'
#' @return Inget (osynligt TRUE). Kastar fel om villkoret aldrig blir sant.
#'
#' @examples
#' \dontrun{
#' vanta_pa_js(skrap, "typeof AR !== 'undefined'")
#' }
vanta_pa_js <- function(skrap, villkor, timeout = 30, intervall = 0.25) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  
  start <- Sys.time()
  repeat {
    if (isTRUE(kor_js(skrap, villkor))) {
      return(invisible(TRUE))
    }
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) {
      stop("Villkoret blev aldrig sant inom ", timeout, " sekunder: ", villkor)
    }
    Sys.sleep(intervall)
  }
}

#' Markera ett alternativ i en select-lista UTAN att trigga change-eventet
#'
#' Sätter markeringen direkt via JavaScript, utan att skicka det
#' change-event som elem_select() (och en riktig användare) utlöser.
#' Användbar när sidans egen onchange-hanterare är trasig och kraschar -
#' vilket kan hindra sidan från att registrera valet innan t.ex. en
#' ASP.NET-postback läser av det. Markeringen ligger i select-elementets
#' DOM-tillstånd och postas ändå med formuläret vid nästa knapptryck.
#'
#' OBS: eftersom inget change-event skickas körs INTE heller eventuell
#' legitim onchange-logik (t.ex. uppdatering av en räknare pa sidan).
#' Använd elem_select() som förstahandsval och den här bara när sidans
#' onchange bevisligen ställer till problem.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param css CSS-selektor för `<select>`-elementet.
#' @param value Värdet (eller vektor av värden för flervalslista) att
#'   markera. Ersätter eventuell befintlig markering.
#'
#' @return Inget (osynligt TRUE). Kastar fel om elementet eller något av
#'   värdena inte hittas.
valj_option_utan_event <- function(skrap, css, value, iframe = NULL) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  rlang::check_installed("jsonlite")
  
  js <- sprintf(
    "(function(){
       %s
       var sel = dok.querySelector(%s);
       if (!sel) return 'ELEMENT_SAKNAS';
       var varden = %s;
       var saknade = [];
       for (var i = 0; i < sel.options.length; i++) {
         sel.options[i].selected = varden.indexOf(sel.options[i].value) !== -1;
       }
       for (var j = 0; j < varden.length; j++) {
         var finns = [...sel.options].some(o => o.value === varden[j]);
         if (!finns) saknade.push(varden[j]);
       }
       return saknade.length ? 'SAKNAS:' + saknade.join(',') : 'OK';
     })()",
    bygg_dokument_js(iframe),
    jsonlite::toJSON(css, auto_unbox = TRUE),
    jsonlite::toJSON(as.character(value))
  )
  
  resultat <- kor_js(skrap, js)
  kontrollera_iframe_svar(resultat)
  if (identical(resultat, "ELEMENT_SAKNAS")) {
    stop("Hittade ingen select-lista som matchar: ", css)
  }
  if (startsWith(resultat, "SAKNAS:")) {
    stop("Foljande value(s) finns inte i ", css, ": ",
         sub("^SAKNAS:", "", resultat))
  }
  invisible(TRUE)
}

#' Vänta in en eventuell sidladdning efter ett klick (intern)
#'
#' Förutsätter att en markör (window.__skrapKlickMarkering) planterades pa
#' sidan i samma JavaScript-körning som klicket. Logiken:
#' - Är markören borta har en navigering skett (den nya sidan har den inte)
#'   - vänta tills document.readyState === 'complete'.
#' - Är markören kvar efter respitperioden `grace` skedde ingen navigering
#'   - släpp vidare direkt.
#' - Svarar sidan inte alls (JS-kontexten riven) pagar navigeringen - vänta.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param grace Respitperiod i sekunder som en eventuell navigering far pa
#'   sig att börja innan vi drar slutsatsen att ingen kommer.
#' @param timeout Max antal sekunder att vänta pa att en pagaende
#'   sidladdning blir klar.
#' @param intervall Sekunder mellan kontrollerna.
vanta_pa_sidladdning <- function(skrap, grace = 0.3, timeout = 30, intervall = 0.1) {
  start <- Sys.time()
  navigering_sedd <- FALSE
  
  js <- "(function(){
    if (window.__skrapKlickMarkering === true) {
      return document.readyState === 'complete' ? 'KVAR_KLAR' : 'KVAR_LADDAR';
    }
    return document.readyState === 'complete' ? 'NY_KLAR' : 'NY_LADDAR';
  })()"
  
  repeat {
    status <- tryCatch(kor_js(skrap, js), error = function(e) NULL)
    tid <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    
    if (is.null(status) || identical(status, "NY_LADDAR")) {
      # JS-kontexten riven eller ny sida under inladdning - navigering pagar
      navigering_sedd <- TRUE
    } else if (identical(status, "NY_KLAR")) {
      # Ny sida fardigladdad
      return(invisible(TRUE))
    } else if (identical(status, "KVAR_KLAR") && !navigering_sedd && tid >= grace) {
      # Markören kvar och ingen navigering setts under respitperioden
      return(invisible(TRUE))
    }
    # KVAR_LADDAR eller inom respitperioden - fortsatt vanta
    
    if (tid > timeout) {
      stop("Sidan blev inte fardigladdad inom ", timeout, " sekunder efter klicket.")
    }
    Sys.sleep(intervall)
  }
}

#' Hitta ett element i en skrapsession (genväg)
#'
#' Tunn wrapper runt selenider::find_element() som tar skrapsession-objektet
#' direkt. Motsvarar seleniders s(), som inte kan användas här eftersom den
#' saknar session-argument (och våra sessioner skapas med local = FALSE).
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param ... Vidare till selenider::find_element() (css, xpath, id, ...).
#'
#' @return Ett selenider-element, redo att pipas till elem_click(),
#'   elem_expect() osv.
#'
#' @examples
#' \dontrun{
#' hitta(skrap, "#AR") |> selenider::elem_expect(is_visible)
#' }
hitta <- function(skrap, ...) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  selenider::find_element(skrap$session, ...)
}

#' Hitta alla matchande element i en skrapsession (genväg)
#'
#' Som hitta(), men motsvarar selenider::find_elements()/ss().
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param ... Vidare till selenider::find_elements().
hitta_alla <- function(skrap, ...) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  selenider::find_elements(skrap$session, ...)
}

#' Hitta ett element via dess synliga text
#'
#' Returnerar det INNERSTA elementet vars hela normaliserade text är exakt
#' lika med `text` (tål blandade text- och elementnoder, t.ex.
#' <label><input>Text</label>). Använd när du behöver ett element-objekt
#' att kedja vidare på (elem_expect, elem_text, ...) - ska elementet bara
#' klickas är klicka_via_text() snabbare, särskilt på sidor med många
#' liknande textmatchningar (se dokumentationen).
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param text Den synliga text som ska matchas exakt.
#'
#' @return Ett selenider-element.
hitta_via_text <- function(skrap, text) {
  hitta(skrap, xpath = paste0(
    "//*[normalize-space(.)='", text, "']",
    "[not(.//*[normalize-space(.)='", text, "'])]"
  ))
}

#' Läs ut enbart value-vektorn ur en select-lista
#'
#' Bekvämlighetswrapper runt hamta_select_options() för det vanliga fallet
#' att man bara behöver alternativens value-attribut, t.ex. för att välja
#' alla utom vissa: setdiff(hamta_select_varden(skrap, "#AR"), c("2014")).
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param css CSS-selektor för select-elementet.
#' @param iframe Valfri iframe-selektor, se hamta_select_options().
#'
#' @return En character-vektor med alla option-values.
hamta_select_varden <- function(skrap, css, iframe = NULL) {
  hamta_select_options(skrap, css, iframe = iframe)$value
}

#' Kör kod med en skrapsession och städa bort den automatiskt efteråt
#'
#' Bekvämlighetsfunktion som garanterar städning även om koden i uttrycket
#' kastar ett fel.
#'
#' @param expr Kod att köra. En variabel med namnet `skrap` finns
#'   tillgänglig i uttrycket och pekar på sessionsobjektet.
#' @param ... Argument som skickas vidare till starta_skrapsession().
#'
#' @examples
#' \dontrun{
#' with_skrapsession({
#'   selenider::open_url(skrap$session, "https://www.regiondalarna.se")
#'   selenider::s(skrap$session, "title") |> selenider::elem_text()
#' })
#' }
with_skrapsession <- function(expr, ...) {
  skrap <- starta_skrapsession(...)
  on.exit(stang_skrapsession(skrap), add = TRUE)
  eval(substitute(expr), envir = list(skrap = skrap), enclos = parent.frame())
}

#' Ställ in var Edge ska spara nedladdade filer
#'
#' Talar om för Chrome DevTools-protokollet att nedladdningar ska tillåtas
#' och sparas i en specifik mapp. Anropas normalt inte direkt, utan via
#' hamta_nedladdning().
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param mapp Mapp dit nedladdningar ska sparas. Skapas om den inte finns.
#'
#' @return Mappens sökväg (osynligt).
stall_in_nedladdningsmapp <- function(skrap, mapp) {
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  
  dir.create(mapp, showWarnings = FALSE, recursive = TRUE)
  
  # Windows-Edge ar sakrast med backslash-sokvagar i downloadPath
  mapp_norm <- normalizePath(mapp, winslash = "\\", mustWork = FALSE)
  
  # Vilken niva kommandot maste skickas till (hela webblasaren eller den
  # enskilda flikens session) varierar mellan Chrome/Edge-versioner - satt
  # pa BADA for att vara saker. try() pa webblasarnivan eftersom vissa
  # versioner avvisar kommandot dar nar det saknar browserContextId.
  try(
    skrap$chrom$Browser$setDownloadBehavior(
      behavior = "allow",
      downloadPath = mapp_norm
    ),
    silent = TRUE
  )
  skrap$session$driver$Browser$setDownloadBehavior(
    behavior = "allow",
    downloadPath = mapp_norm
  )
  
  invisible(mapp)
}

#' Klicka fram en nedladdning och vänta tills filen är klar
#'
#' Motsvarigheten till Playwrights `expect_download()`. Ställer in en
#' nedladdningsmapp, kör den kod som triggar nedladdningen (t.ex. ett klick
#' på en exportknapp), och pollar sedan mappen tills en ny fil dyker upp och
#' slutar växa i storlek (dvs. Chrome/Edge är klar med att skriva den).
#'
#' Chrome/Edge sparar ofdärdiga nedladdningar med ändelsen `.crdownload`
#' (eller `.tmp`) tills de är klara — dessa ignoreras vid sökningen efter
#' den färdiga filen.
#'
#' @param skrap Ett objekt skapat av starta_skrapsession().
#' @param trigger En funktion utan argument som utför klicket/handlingen som
#'   startar nedladdningen, t.ex.
#'   `function() selenider::s(skrap$session, "button.exportera") |> selenider::elem_click()`.
#' @param nedladdningsmapp Mapp dit filen ska sparas. Default en unik
#'   temp-mapp per anrop.
#' @param monster Valfritt reguljärt uttryck för att bara acceptera filer
#'   som matchar (t.ex. `"\\.xlsx$"`), ifall sidan skapar flera filer
#'   samtidigt och bara en är den du vill ha.
#' @param timeout Max antal sekunder att vänta på att nedladdningen dyker
#'   upp och blir klar.
#' @param stabil_tid Antal sekunder filstorleken måste vara oförändrad
#'   innan filen räknas som klar. Höj vid stora filer på långsamma nät.
#'
#' @return Sökvägen till den nedladdade filen.
#'
#' @examples
#' \dontrun{
#' skrap <- starta_skrapsession()
#' selenider::open_url(skrap$session, "https://exempel.se/statistik")
#'
#' fil <- hamta_nedladdning(
#'   skrap,
#'   trigger = function() {
#'     selenider::s(skrap$session, "button:has-text('Ladda ner Excel')") |>
#'       selenider::elem_click()
#'   },
#'   nedladdningsmapp = "C:/temp/nedladdningar",
#'   monster = "\\.xlsx$"
#' )
#'
#' fil
#' stang_skrapsession(skrap)
#' }
hamta_nedladdning <- function(skrap,
                              trigger,
                              nedladdningsmapp = tempfile("nedladdning-"),
                              monster = NULL,
                              timeout = 30,
                              stabil_tid = 1) {
  
  if (!inherits(skrap, "skrapsession")) {
    stop("Objektet ar inte skapat av starta_skrapsession().")
  }
  if (!is.function(trigger)) {
    stop("trigger maste vara en funktion utan argument, t.ex. ",
         "function() selenider::s(skrap$session, 'button') |> selenider::elem_click()")
  }
  
  stall_in_nedladdningsmapp(skrap, nedladdningsmapp)
  
  filer_innan <- list.files(nedladdningsmapp, full.names = TRUE)
  
  trigger()
  
  start <- Sys.time()
  senast_storlek <- -1
  stabil_sedan <- NULL
  
  repeat {
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) {
      stop(
        "Ingen nedladdning blev klar inom ", timeout,
        " sekunder i mappen: ", nedladdningsmapp
      )
    }
    
    alla_filer <- list.files(nedladdningsmapp, full.names = TRUE)
    if (!is.null(monster)) {
      alla_filer <- alla_filer[grepl(monster, alla_filer)]
    }
    nya_filer <- setdiff(alla_filer, filer_innan)
    # Ignorera Edges/Chromes ofardiga nedladdningsfiler
    kandidater <- nya_filer[!grepl("\\.crdownload$|\\.tmp$", nya_filer)]
    
    if (length(kandidater) >= 1) {
      fil <- kandidater[[1]]
      storlek <- suppressWarnings(file.info(fil)$size)
      
      if (!is.na(storlek) && storlek == senast_storlek && storlek > 0) {
        if (is.null(stabil_sedan)) {
          stabil_sedan <- Sys.time()
        }
        if (as.numeric(difftime(Sys.time(), stabil_sedan, units = "secs")) >= stabil_tid) {
          return(fil)
        }
      } else {
        senast_storlek <- storlek
        stabil_sedan <- NULL
      }
    }
    
    Sys.sleep(0.3)
  }
}