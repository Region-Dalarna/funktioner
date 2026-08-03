# =============================================================================
#  Packed circles - sysselsatta per bransch
#  Region Dalarna / Samhallsanalys
#
#  Ritar "circle packing"-diagram per grupp (kommun, kon, eller hela regionen).
#  Funktioner:
#    * area-proportionella cirklar inom varje klunga
#    * valbar sorteringsriktning (storst i mitten ELLER storst i ytterkant)
#    * valbar omslutande ring runt varje grupp
#    * geografisk placering av Dalarnas kommuner (eller grid / kraftbaserad)
#    * valbar dampning av storleksskillnaden mellan kommuner
#    * farger lasta direkt ur datasetets HexCode-kolumn
#
#  Beroenden:
#    packcircles, ggplot2, dplyr, scales
#    ggforce      (endast om visa_ring = TRUE)
#    readxl       (for Excel-inlasning)
# =============================================================================

library(packcircles)
library(ggplot2)
library(dplyr)
library(scales)

# -----------------------------------------------------------------------------
# 0. TYPSNITT (valfritt men rekommenderat)
#    Registrerar Poppins sa att den faktiskt renderas i PNG/SVG-export.
#    Utan detta faller R tillbaka pa standardtypsnitt aven om family = "Poppins"
#    anges. Anropa aktivera_font() EN gang innan du ritar.
#
#    Forsta gangen: kor registrera_poppins_google() om du har internet och vill
#    hamta Poppins fran Google Fonts. Har du redan Poppins installerad i Windows
#    racker det att aktivera_font() hittar den via systemets typsnitt.
# -----------------------------------------------------------------------------
aktivera_font <- function(namn = "Poppins", google = FALSE) {
  if (!requireNamespace("showtext", quietly = TRUE) ||
      !requireNamespace("sysfonts", quietly = TRUE)) {
    message("Paketen 'showtext' och 'sysfonts' saknas - hoppar over fontregistrering. ",
            "Installera med install.packages(c('showtext','sysfonts')).")
    return(invisible(FALSE))
  }
  ok <- TRUE
  if (google) {
    # Hamtar fran Google Fonts (kraver internet)
    tryCatch(sysfonts::font_add_google(namn, namn),
             error = function(e) { message("Kunde inte hamta fran Google: ", e$message); ok <<- FALSE })
  } else {
    # Forsoker hitta en redan installerad systemfont
    paths <- sysfonts::font_files()
    rad <- paths[grepl(namn, paths$family, ignore.case = TRUE) &
                   grepl("regular", paths$face, ignore.case = TRUE), ]
    if (nrow(rad) >= 1) {
      sysfonts::font_add(namn, file.path(rad$path[1], rad$file[1]))
    } else {
      message("Hittade ingen installerad font '", namn,
              "'. Kor aktivera_font('", namn, "', google = TRUE) for att hamta den.")
      ok <- FALSE
    }
  }
  if (ok) showtext::showtext_auto()   # rendera registrerade fonter i all output
  invisible(ok)
}

# -----------------------------------------------------------------------------
# 1. GEOGRAFISK LAYOUT FOR DALARNAS KOMMUNER
#    Ungefarliga relativa lagen (0-100) enligt din Dalarna-bild.
#    x okar at hoger, y okar uppat. Justera fritt.
# -----------------------------------------------------------------------------

dalarna_layout <- data.frame(
  grupp = c("Älvdalen", "Orsa", "Malung-Sälen", "Mora", "Vansbro",
            "Rättvik", "Leksand", "Gagnef", "Borlänge", "Falun",
            "Ludvika", "Smedjebacken", "Säter", "Hedemora", "Avesta"),
  # Koordinater avlasta direkt fran referensbilden (dalarna_kommuner_aug2024).
  # gx/gy i 0-100, y UPPAT. Justera vid behov.
  gx = c(42.4, 67.6, 34.0, 58.3, 32.6,  84.0, 63.2, 41.7, 55.6, 86.1,
         34.0, 56.2, 72.2, 83.3, 97.2),
  gy = c(88.1, 88.4, 76.8, 77.5, 63.7,  63.7, 61.8, 52.4, 39.9, 42.4,
         18.6, 14.9, 24.9, 14.9, 12.4),
  stringsAsFactors = FALSE
)
# OBS: stavningen matchar nu svenska tecken (Älvdalen, Malung-Sälen). Kontrollera
# att din region-kolumn anvander samma. layout = "geo" ger placering som bilden.

# -----------------------------------------------------------------------------
# 1b. HJALPFUNKTION: minsta omslutande cirkel for en mangd cirklar
#     Hittar centrum + radie sa att ringen blir sa LITEN som mojligt (minskar
#     luften jamfort med att mata avstand fran origo). Iterativ 1-center.
# -----------------------------------------------------------------------------
minsta_omslutande <- function(x, y, r, iter = 4000) {
  cx <- mean(x); cy <- mean(y)
  step <- max(r) + diff(range(c(x, y)))
  for (i in seq_len(iter)) {
    d <- sqrt((x - cx)^2 + (y - cy)^2) + r
    k <- which.max(d)
    dx <- x[k] - cx; dy <- y[k] - cy
    nd <- sqrt(dx^2 + dy^2)
    if (nd > 1e-12) { cx <- cx + dx / nd * step; cy <- cy + dy / nd * step }
    step <- step * 0.999
  }
  R <- max(sqrt((x - cx)^2 + (y - cy)^2) + r)
  list(cx = cx, cy = cy, R = R)
}

# -----------------------------------------------------------------------------
# 1c. HJALPFUNKTION: snygga, jamna skalvarden som spanner nara min -> max
#     Returnerar exakt n (eller narmast mojliga) varden med lika steg,
#     dar steget ar "snyggt" (1,2,2.5,5 * 10^k) och forsta vardet nara
#     data_min, sista nara data_max.
# -----------------------------------------------------------------------------
bra_skalvarden <- function(data_min, data_max, n) {
  if (n <= 0) return(numeric(0))
  rng <- data_max - data_min
  if (rng == 0) rng <- max(abs(data_max), 1)
  
  # Avrunda resultatet till heltal nar vardena ar >= 1 (sysselsattning ar
  # alltid heltal). Behall decimaler bara om datan ar genuint subheltal (< 1).
  heltala <- function(v) {
    v <- v[v > 0]
    if (length(v) > 0 && all(v >= 1)) v <- round(v)
    sort(unique(v))
  }
  
  if (n == 1) {
    mag <- 10^floor(log10(max(data_max, 1)))
    return(heltala(round(data_max / mag) * mag))
  }
  ratio <- data_max / max(data_min, 1)
  
  # Avrunda till narmaste "fina" tal i 1-2-5-serien (1,2,5,10,20,50,100,...)
  avrunda_snyggt <- function(v, riktning = "narmast") {
    if (v <= 0) return(1)
    nices <- sort(as.vector(outer(c(1, 2, 5), 10^(-1:7))))   # 0.1 ... 5 000 000
    if (riktning == "upp") {
      kand <- nices[nices >= v]
      if (length(kand) > 0) return(kand[1]) else return(max(nices))
    } else if (riktning == "ner") {
      kand <- nices[nices <= v]
      if (length(kand) > 0) return(kand[length(kand)]) else return(min(nices))
    } else {
      return(nices[which.min(abs(nices - v))])
    }
  }
  
  if (ratio < 10 || data_min < 1) {
    # Linjart: snyggt steg (1-2-5-serien), start nara min
    ideal <- rng / (n - 1)
    mag <- 10^floor(log10(max(ideal, 1)))
    best <- NULL; best_score <- Inf
    for (nice in c(1, 2, 5, 10)) {
      s <- nice * mag
      for (start in unique(c(floor(data_min/s)*s, ceiling(data_min/s)*s, s))) {
        if (start <= 0) next
        v <- seq(start, by = s, length.out = n)
        if (max(v) < data_max * 0.5) next
        p <- abs(min(v) - data_min) / rng * 3 + abs(max(v) - data_max) / rng
        if (p < best_score) { best <- v; best_score <- p }
      }
    }
    if (!is.null(best)) return(heltala(best))
  }
  
  # Stort spann: forsta nara min (uppat), sista nara max (nedat), log-jamt
  lo <- avrunda_snyggt(data_min, "upp")
  hi <- avrunda_snyggt(data_max, "ner")
  if (n == 2) return(heltala(c(lo, hi)))
  log_inner <- exp(seq(log(lo), log(hi), length.out = n))
  snygga <- sapply(log_inner, avrunda_snyggt)
  heltala(snygga)
}

# -----------------------------------------------------------------------------
# 1d. HJALPFUNKTION: orm-/coil-layout for cirklar
#     Varje cirkel placeras TANGENT till den foregaende, och bland mojliga
#     tangentpositioner valjs den narmast klungans tyngdpunkt. Det drar in
#     kedjan sa att den rullar sig utat som ett snackskal/orm - minst i mitten,
#     storst ytterst, och man kan folja slingan i storleksordning.
# -----------------------------------------------------------------------------
spiral_layout <- function(areas) {
  radii <- sqrt(areas / pi)
  n <- length(radii)
  if (n == 0) return(data.frame(x = numeric(0), y = numeric(0), radius = numeric(0)))
  px <- numeric(n); py <- numeric(n); pr <- radii
  px[1] <- 0; py[1] <- 0
  if (n == 1) return(data.frame(x = px, y = py, radius = pr))
  
  # Andra cirkeln tangent till forsta, at hoger
  px[2] <- radii[1] + radii[2]; py[2] <- 0
  
  for (i in seq_len(n)[-(1:2)]) {   # dvs 3:n, men tomt (ingen krasch) om n < 3
    r <- radii[i]
    fx <- px[i-1]; fy <- py[i-1]; fr <- pr[i-1]   # foregaende cirkel
    tang_r <- fr + r                               # tangentavstand
    
    # Klungans tyngdpunkt hittills (att rulla runt)
    cgx <- mean(px[1:(i-1)]); cgy <- mean(py[1:(i-1)])
    
    # Prova alla tangentvinklar runt foregaende, valj den narmast tyngdpunkten
    basta_x <- NA; basta_y <- NA; basta_score <- Inf
    for (deg in seq(0, 358, by = 2)) {
      ang <- deg * pi / 180
      cx <- fx + tang_r * cos(ang)
      cy <- fy + tang_r * sin(ang)
      # Overlapp med tidigare (utom foregaende som ar tangent)?
      ok <- TRUE
      if (i > 2) {
        for (k in 1:(i-2)) {
          if (sqrt((cx-px[k])^2 + (cy-py[k])^2) < (pr[k] + r - 1e-6)) {
            ok <- FALSE; break
          }
        }
      }
      if (!ok) next
      score <- sqrt((cx - cgx)^2 + (cy - cgy)^2)   # nara tyngdpunkt -> coil
      if (score < basta_score) {
        basta_score <- score; basta_x <- cx; basta_y <- cy
      }
    }
    if (is.na(basta_x)) {
      # Fallback: svang utat
      ang <- atan2(fy, fx) + 0.6
      basta_x <- fx + tang_r * cos(ang)
      basta_y <- fy + tang_r * sin(ang)
    }
    px[i] <- basta_x; py[i] <- basta_y
  }
  
  data.frame(x = px, y = py, radius = pr)
}


# -----------------------------------------------------------------------------
# 2. HUVUDFUNKTION
# -----------------------------------------------------------------------------
#  data         data.frame i langt format (en rad per grupp+bransch)
#  grupp_kol    grupperingskolumn, t.ex. "region" eller "kon". NULL [default] = en grupp.
#  bransch_kol  branschnamn (fyll/legend) - MASTE vara KLARTEXT, inte kod,
#               eftersom den anvands direkt som legendtext. NULL [default] =
#               auto-detektera: forst en kolumn som heter exakt "bransch"
#               (skiftlagesokanslig, se bransch_kol_kandidater), annars en
#               kolumn med "SNI" i namnet (se bransch_kol_monster). Ger detta
#               FLERA traffar tas de vars namn tyder pa kod bort (se
#               bransch_kol_kod_monster, default "kod"/"code") - meddelas i
#               konsolen. Kvarstar da exakt EN kolumn anvands den; annars fel
#               med forslag pa att ange bransch_kol explicit. (Overstyrs
#               automatiskt om autokoppla_branschnamn_farg redan hittat en
#               branschkodkolumn - se den parametern.)
#  antal_kol    antal sysselsatta. NULL [default] = auto-detektera: forst en
#               kolumn vars namn EXAKT matchar (skiftlagesokanslig) nagot i
#               antal_kol_kandidater (default "varde"/"antal"), annars en
#               kolumn vars namn innehaller ALLA ord i nagon delmangd i
#               antal_kol_monster (default sysselsatta+belagenhet). Matchar
#               det da precis EN kolumn anvands den. Matchar FLERA kolumner:
#               foredra en som innehaller nagot i antal_kol_prioritet (t.ex.
#               "arbetsstalle"/"dagbefolkning") - meddelas i konsolen. Kvarstar
#               flera, eller hittas ingen: fel med forslag pa att ange
#               antal_kol explicit. Bygg pa listorna for fler namn/monster.
#  farg_kol     hex-farg per rad (NULL = automatisk palett)
#
#  storlek_ut   TRUE  = storsta cirklarna i YTTERKANTEN (som dina bilder)  [default]
#               FALSE = storsta cirklarna i MITTEN
#
#  visa_ring    rita tunn omslutande cirkel runt varje grupp (TRUE/FALSE)
#  ring_farg    farg pa ringen
#  ring_marginal  ringens radie relativt klungans yttersta cirkelkant.
#               1.02 = sa tight som mojligt. Hoj for mer luft.
#  ring_metod   "omslutande" = minsta omslutande cirkel runt branscherna
#                              (minst luft - rekommenderas) [default]
#               "origo"      = gamla metoden (avstand fran spiralens origo)
#
#  layout       "auto"   valj automatiskt [default]: Dalarnas kommuner, dar
#                        minst 60% av lanets kommuner finns med -> "geo";
#                        fatal utvalda kommuner -> "repel" (annars for glest);
#                        uppdelning pa kon/fodelseregion -> "grid". Du kan
#                        alltid tvinga ett specifikt lage genom att ange det nedan.
#               "repel"  tatpackad, klungorna sa nara varandra som mojligt,
#                        ungefarlig geografisk vinkel bevaras
#               "vinkel" placerar grupperna radiellt ut fran en STARTGRUPP,
#                        var och en i sin geografiska riktning. Bevarar
#                        vinklar bast men blir glesare langst ut.
#               "geo"    fast geografisk placering enligt layout_tabell
#               "grid"   enkelt rutnat (bra for kon/fodelseregion sida vid sida)
#               "none"   en enda grupp, centrerad
#  layout_tabell  data.frame med kolumner grupp, gx, gy. Anvands for vinklar
#                 i "repel"/"vinkel" och fasta lagen i "geo".
#  geo_metod    bara for layout = "geo":
#               "skala_bubblor" = behall koordinaterna exakt fran layout_tabell,
#                                 krymp bubblorna sa inget overlappar. Bevarar
#                                 kommunernas inbordes avstand troget. [default]
#               "skala_avstand" = skala upp koordinaterna tills bubblorna far
#                                 plats. Snabbare men forvranger relativa avstand.
#  start_grupp  startgrupp (centrum) for layout = "vinkel". NULL = storsta.
#  ncol           antal kolumner for "grid"
#
#  skala_grupp  TRUE  = gruppens storlek speglar total sysselsattning
#               FALSE = alla klungor lika stora (jamfor branschMIX)
#  skala_styrka 1.0 = full storleksproportion (radie ~ sqrt(total))
#               0.5 = halverad effekt, 0.35 = kraftigt dampad ...
#               Lagre varde -> mindre skillnad mellan stora och sma kommuner.
#               NULL [default] = berakna automatiskt fran spridningen mellan
#               storsta och minsta gruppens totalsumma (mer dampning ju
#               storre spridning). Anvands bara nar skala_grupp = TRUE.
#  tathet       avstandsfaktor mellan grupper i geo-layouten.
#               1.0 = automatiskt kalibrerat sa inget overlappar.
#               <1 tatare, >1 glesare.
#
#  hojd_andel   andel av total hojd som diagrammet upptar. NULL [default] =
#               berakna automatiskt utifran hur mycket plats titel och
#               diagram_caption faktiskt behover (mindre vit luft nar ingen
#               caption ar satt). Ange t.ex. 0.85 for fast schablonandel.
#  namn_utanfor TRUE = gruppnamn UTANFOR ringen (default), FALSE = strax under.
#  skal_bubblor antal svarta skalbubblor (0 = inga). De staplas vertikalt till
#               vanster, minst overst, med "snygga" jamna varden (via pretty())
#               och storlekar som speglar diagrammets cirkelskala.
#  diskreta_labels TRUE = sifferetiketterna far en morkare/ljusare nyans av
#               bubblans egen farg istallet for ren svart/vit.
#  labels_visa_skalvarden  FALSE = visa siffror enligt antal_min (normalt lage).
#               TRUE = visa siffror BARA i de cirklar vars antal ligger narmast
#               skalvardena. Fungerar aven utan skalbubblor (anvander da 5 niva).
#               Kan kombineras med diskreta_labels.
#
#  visa_antal   skriv ut siffror i cirklar (gäller nar labels_visa_skalvarden=0)
#  antal_min    minsta antal for utskriven siffra. NULL [default] = berakna
#               automatiskt fran cirkelns FAKTISKA storlek i den ritade
#               figuren (storlek_labels), sa att bara siffror som far rimlig
#               plats visas - oavsett om datan racknar hundratal eller
#               hundratusental. Ange ett eget tal for att styra manuellt.
#  visa_namn    visa gruppnamn
#  font         typsnitt (din profil: "Poppins") - kor aktivera_font() forst
#  titel        valfri plot-titel
#  diagram_caption  valfri bildtext/kalla, placeras vansterstalld UNDER
#                diagrammet (i linje med klungornas vanstra kant). Tom/NULL =
#                inget ritas. Storlek styrs med storlek_caption (default 3).
#  bakgrund     "white" eller "black" m.m. (svart = som din DagBef-bild)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 1e. HJALPFUNKTION: bestam geografisk omfattning for rubrik/filnamn.
#     - en enda region        -> dess namn (kommun ELLER lan)
#     - alla kommuner i ETT lan, grupperat PER KOMMUN (grupp_kol = region-kol):
#                              -> "<lan>s kommuner" (t.ex. "Dalarnas kommuner")
#     - alla kommuner i ETT lan, SUMMERAT (kon/fodelseregion/total):
#                              -> lanets KORTNAMN (t.ex. "Dalarna")
#     - delmangd av kommuner    -> NULL (ingen geografietikett)
#     Kraver func_API.R (hamtakommuner, hamtaregion_kod_namn) for lans-koll;
#     utan dem faller den tillbaka pa att ALLA kommuner i layout_tabell finns.
# -----------------------------------------------------------------------------
bestam_omfattning <- function(data, layout_tabell = NULL, grupp_kol = NULL) {
  reg_kol <- names(data)[tolower(names(data)) == "region"]
  if (length(reg_kol) == 0) return(NULL)
  namn <- unique(as.character(data[[reg_kol[1]]])); namn <- namn[!is.na(namn)]
  if (length(namn) == 0) return(NULL)
  if (length(namn) == 1) return(namn[1])           # en kommun eller ett lan
  
  # Visas kommunerna var for sig (grupperat per region) eller summeras de ihop?
  per_kommun <- !is.null(grupp_kol) &&
    tolower(grupp_kol) == tolower(reg_kol[1])
  
  # Bygg ratt etikett fran lansnamnet ("Dalarnas län")
  lan_etikett <- function(lannamn) {
    if (per_kommun) {
      # kommuner visas separat -> "<lan utan ' län'> kommuner" = "Dalarnas kommuner"
      paste0(sub("\\s*l\u00e4n$", "", lannamn), " kommuner")
    } else if (exists("skapa_kortnamn_lan") && is.function(get("skapa_kortnamn_lan"))) {
      get("skapa_kortnamn_lan")(lannamn)           # summerat -> "Dalarna"
    } else {
      kort <- sub("\\s*l\u00e4n$", "", lannamn)
      if (grepl("l\u00e4n", lannamn) && grepl("s$", kort)) kort <- sub("s$", "", kort)
      kort
    }
  }
  
  # kommunkoder (for lans-koll)
  regkod_kol <- names(data)[tolower(names(data)) %in% c("regionkod","kommunkod")]
  koder <- character(0)
  if (length(regkod_kol) > 0) {
    koder <- unique(as.character(data[[regkod_kol[1]]]))
    koder <- koder[!is.na(koder)]
    # vanstra-stoppa rena sifferkoder till 4 tecken (t.ex. "114" -> "0114")
    sif <- grepl("^[0-9]+$", koder)
    koder[sif] <- formatC(as.integer(koder[sif]), width = 4, flag = "0")
    koder <- koder[grepl("^[0-9]{4}$", koder)]
  }
  
  if (length(koder) > 0 && exists("hamtakommuner") &&
      is.function(get("hamtakommuner"))) {
    lan <- unique(substr(koder, 1, 2))
    if (length(lan) == 1) {
      alla <- tryCatch(get("hamtakommuner")(lan, FALSE, FALSE),
                       error = function(e) NULL)
      if (!is.null(alla) && length(alla) > 0 && all(alla %in% koder)) {
        lannamn <- tryCatch(get("hamtaregion_kod_namn")(lan)$region,
                            error = function(e) NULL)
        if (!is.null(lannamn) && length(lannamn) > 0)
          return(lan_etikett(lannamn[1]))
      }
    }
    return(NULL)                                   # delmangd eller flera lan
  }
  
  # Fallback utan func_API: krav att ALLA kommuner i layout_tabell finns med
  if (!is.null(layout_tabell) && all(layout_tabell$grupp %in% namn))
    return(if (per_kommun) "Dalarnas kommuner" else "Dalarna")
  NULL
}

# -----------------------------------------------------------------------------
# 1f. CACHE for branschnyckelfilen (Bransch_Gxx_farger.xlsx)
#     Utan detta laddades filen ner och lastes in pa nytt VID VARJE ANROP av
#     skapa_packed_circles() nar autokoppla_branschnamn_farg = TRUE - onodig
#     natverkslatens om funktionen anropas flera ganger, t.ex. i en Shiny-app.
#     Cachas i en egen miljo sa lange R-sessionen lever. Rensa manuellt med:
#     rm(list = ls(.bransch_nyckel_cache), envir = .bransch_nyckel_cache)
# -----------------------------------------------------------------------------
.bransch_nyckel_cache <- new.env(parent = emptyenv())

hamta_bransch_nyckel <- function(url) {
  if (exists(url, envir = .bransch_nyckel_cache, inherits = FALSE)) {
    return(get(url, envir = .bransch_nyckel_cache, inherits = FALSE))
  }
  nyckel <- tryCatch({
    tmpf <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmpf), add = TRUE)
    curl::curl_download(url = url, destfile = tmpf, quiet = TRUE)
    as.data.frame(readxl::read_excel(tmpf))
  }, error = function(e) NULL)
  # cacha bara vid lyckad hamtning - ett misslyckat forsok ska forsoka igen nasta gang
  if (!is.null(nyckel)) assign(url, nyckel, envir = .bransch_nyckel_cache)
  nyckel
}

skapa_packed_circles <- function(data,
                                 # ====================================================================
                                 # VANLIGA PARAMETRAR - de du oftast vill andra
                                 # ====================================================================
                                 grupp_kol     = NULL,                           # kolumnnamn for gruppering, t.ex. "region", "kon" eller "fodelseregion". NULL [default] = en enda grupp.
                                 bransch_kol   = NULL,                           # kolumnnamn for branschnamn (anvands i legend); NULL = auto-detektera (se AVANCERAT for de kolumnlistor autodetekteringen anvander)
                                 antal_kol     = NULL,                           # kolumnnamn for antal sysselsatta; NULL = auto-detektera (se AVANCERAT for de kolumnlistor autodetekteringen anvander)
                                 farg_kol      = "HexCode",                      # kolumnnamn med hex-farg per rad; NULL = auto-palett
                                 diagram_caption = NULL,                         # valfri bildtext/kalla under diagrammet; NULL = ingen
                                 titel         = NULL,                           # plot-titel som strang, eller NULL
                                 bakgrund      = "white",                        # bakgrundsfarg; "black" ger ljus text och svart panel
                                 visa_antal    = FALSE,                          # TRUE/FALSE = skriv ut siffror i cirklarna alls
                                 antal_min     = NULL,                           # minsta antal for utskriven siffra; NULL = autoberakna fran cirkelstorlek (se dokumentation ovan)
                                 visa_namn     = TRUE,                           # TRUE/FALSE = visa gruppnamn vid klungorna
                                 autokoppla_branschnamn_farg = TRUE,             # TRUE = upptack branschkod-kolumn (Gxx m.m.) och koppla pa Bransch + HexCode fran nyckelfilen automatiskt
                                 skal_bubblor  = 0,                              # 0 = inga; >0 = antal svarta skalbubblor (storleksreferens)
                                 diskreta_labels = FALSE,                        # TRUE = sifferfarg = nyans av bubblans egen farg; FALSE = svart/vit
                                 labels_visa_skalvarden = FALSE,                 # TRUE = visa siffror BARA i cirklarna narmast skalvardena (kraver skal_bubblor > 0); FALSE = enligt antal_min
                                 storlek_titel = 5,                              # textstorlek for plottiteln (ggplot2 size-enhet)
                                 storlek_caption = 3,                            # textstorlek for diagram_caption (ggplot2 size-enhet)
                                 storlek_legend = 12,                            # textstorlek for legendtext i pt
                                 storlek_skal_text = 3.2,                        # textstorlek for skalbubblor-etiketter
                                 storlek_labels = 2.6,                           # textstorlek for siffror i cirklarna
                                 storlek_namn  = 3.0,                            # textstorlek for gruppnamn (kommun/kon)
                                 
                                 # --- Spara till bildfil (valfritt) ---------------------------------
                                 spara_bildfil = TRUE,                           # TRUE = spara diagrammet som bildfil
                                 filnamn       = NULL,                            # filnamn; NULL = autogenereras fran titel/grupp + datum
                                 mapp          = NULL,                            # mapp att spara i; NULL = anvand utskriftsmapp() om den finns
                                 bildhojd      = 8,                              # bildhojd i tum; bredden beraknas dynamiskt om bildbredd = NULL
                                 
                                 # ====================================================================
                                 # AVANCERAT - dessa behover du sallan andra. Layout valjs automatiskt:
                                 # Dalarnas kommuner -> "geo"; uppdelning pa kon/fodelseregion -> "grid".
                                 # ====================================================================
                                 layout        = c("auto","geo","repel","vinkel","grid","none"),  # "auto" = valj sjalv (se ovan)
                                 storlek_ut    = TRUE,                           # TRUE = storsta cirklarna i ytterkanten; FALSE = i mitten
                                 packning      = c("spiral", "progressiv"),      # "spiral" = orm-coil i storleksordning; "progressiv" = tatast
                                 visa_ring     = TRUE,                           # TRUE/FALSE = rita tunn ring runt varje klunga
                                 ring_farg     = "grey70",                       # farg pa ringen
                                 ring_marginal = 1.02,                           # ringradie / klungans yttre kant; 1.00-1.10
                                 ring_metod    = c("omslutande", "origo"),       # "omslutande" = tatare ring; "origo" = fran spiralens origo
                                 geo_metod     = c("skala_bubblor", "skala_avstand"),  # layout="geo": "skala_bubblor" = trogen karta
                                 kompaktera    = TRUE,                           # layout="geo": TRUE = dra ihop kommunerna (tatare karta)
                                 layout_tabell = dalarna_layout,                 # data.frame med kolumner grupp, gx, gy (0-100, y uppat)
                                 start_grupp   = NULL,                           # layout="vinkel": namn pa centrum-grupp; NULL = storsta
                                 ncol          = 2,                              # antal kolumner i layout="grid"
                                 skala_grupp   = TRUE,                           # TRUE = klungans storlek beror pa gruppens total
                                 skala_styrka  = NULL,                           # 0-1: 1.0 full proportion, lagre = dampad; NULL [default] = autoberakna fran spridningen mellan grupperna (se dokumentation ovan)
                                 tathet        = 1.0,                            # avstandsfaktor: <1 tatare, >1 glesare
                                 hojd_andel    = NULL,                           # 0-1: andel av panelhojd diagrammet upptar; NULL [default] = berakna exakt utrymme for titel/caption (se dokumentation ovan)
                                 namn_utanfor  = TRUE,                           # TRUE = gruppnamn UTANFOR ringen
                                 font          = "Poppins",                      # typsnittsnamn
                                 autoladda_typsnitt = TRUE,                      # TRUE = registrera 'font' automatiskt, annars standardfont
                                 legend_radbryt = 30,                            # max tecken per rad i legendtext; 0/NULL = ingen brytning
                                 
                                 # --- Bildexport - finjustering --------------------------------------
                                 dpi           = 300,                            # upplosning vid sparande
                                 bildbredd     = NULL,                            # bildbredd i tum; NULL = beraknas fran innehallets proportioner (minskar vit luft)
                                 
                                 # --- Kolumn-autodetektering (bransch_kol/antal_kol = NULL) - sallan behovs ---
                                 bransch_nyckel_url = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/Bransch_Gxx_farger.xlsx",  # nyckelfil for autokoppla_branschnamn_farg
                                 bransch_kol_kandidater = c("bransch"),          # bransch_kol=NULL: exakta kolumnnamn (case-insensitive) att leta efter forst. Lagg till fler har.
                                 bransch_kol_monster = list(c("SNI")),           # bransch_kol=NULL: hittas ingen exakt traff, leta efter kolumn vars namn innehaller ALLA strangar i nagon delmangd (OR mellan delmangder, case-insensitive). Lagg till fler delmangder har.
                                 bransch_kol_kod_monster = c("kod", "code"),     # bransch_kol=NULL: vid FLERA traffar, ta bort de vars namn tyder pa KOD (bransch_kol ska vara klartext - anvands direkt som legendtext). Meddelas alltid i konsolen nar detta avgor valet.
                                 antal_kol_kandidater = c("varde", "antal"),     # antal_kol=NULL: exakta kolumnnamn (case-insensitive) att leta efter forst. Lagg till fler har.
                                 antal_kol_monster = list(c("sysselsatta", "belägenhet")),  # antal_kol=NULL: hittas ingen exakt trakk, leta efter kolumn vars namn innehaller ALLA strangar i nagon delmangd (OR mellan delmangder). Lagg till fler delmangder har, t.ex. list(c("sysselsatta","belägenhet"), c("forvarvsarbetande"))
                                 antal_kol_prioritet = c("arbetsställe", "dagbefolkning")) { # antal_kol=NULL: vid FLERA traffar, foredra en kolumn som innehaller nagot av dessa (case-insensitive). Meddelas alltid i konsolen nar detta avgor valet.
  
  layout <- match.arg(layout)
  ring_metod <- match.arg(ring_metod)
  geo_metod <- match.arg(geo_metod)
  packning <- match.arg(packning)
  
  # --- AUTO-LAYOUT: valj layout sjalv om anvandaren inte angett nagon --------
  if (layout == "auto") {
    if (is.null(grupp_kol)) {
      layout <- "none"                              # en enda grupp
    } else {
      grupp_varden <- unique(as.character(data[[grupp_kol]]))
      # Ar grupperna Dalarnas kommuner (finns i layout_tabell)?
      ar_kommuner <- !is.null(layout_tabell) &&
        mean(grupp_varden %in% layout_tabell$grupp) >= 0.5
      if (ar_kommuner) {
        # Hur STOR ANDEL av HELA lanet (layout_tabell) finns med i datan?
        # Fatal utvalda kommuner ger mycket tomrum med "geo" (som haller fast
        # vid de riktiga koordinaterna oavsett hur fa som visas) - da blir
        # "repel" (tatpackat, ungefarlig geografisk riktning bevarad) battre.
        andel_lanet <- mean(layout_tabell$grupp %in% grupp_varden)
        if (andel_lanet >= 0.6) {
          layout <- "geo"                           # merparten av lanet med -> geografin ar meningsfull
        } else {
          layout <- "repel"                         # fatal utvalda kommuner -> tatpacka istallet
        }
      } else {
        layout <- "grid"                            # kon/fodelseregion -> sida vid sida
      }
    }
  }
  
  # --- AUTO-KOPPLA branschnamn + farg fran nyckelfil -------------------------
  # Upptack om nagon kolumn i datan innehaller branschkoder (G01.., A/B+C..,
  # dA/dB+C..) och koppla i sa fall pa Bransch och HexCode fran nyckelfilen.
  if (isTRUE(autokoppla_branschnamn_farg)) {
    if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("readxl", quietly = TRUE)) {
      message("Obs: paketen 'curl' och 'readxl' kravs for autokoppla_branschnamn_farg - hoppar over.")
    } else {
      nyckel <- hamta_bransch_nyckel(bransch_nyckel_url)   # cachad - laddas bara ner en gang per URL/session
      
      if (is.null(nyckel)) {
        message("Obs: kunde inte hamta branschnyckeln (", bransch_nyckel_url,
                ") - fortsatter utan autokoppling.")
      } else {
        kod_kolumner <- intersect(c("BrKod", "Br15kod", "Br15kod_2"), names(nyckel))
        # hitta vilken kolumn i datan som bast matchar nagon kodkolumn
        basta_datakol <- NULL; basta_kodkol <- NULL; basta_andel <- 0
        for (kol in names(data)) {
          dv <- unique(as.character(data[[kol]]))
          dv <- dv[!is.na(dv)]
          if (length(dv) == 0) next
          for (kk in kod_kolumner) {
            koder <- as.character(nyckel[[kk]])
            andel <- mean(dv %in% koder)
            if (andel > basta_andel) {
              basta_andel <- andel; basta_datakol <- kol; basta_kodkol <- kk
            }
          }
        }
        if (!is.null(basta_datakol) && basta_andel >= 0.5) {
          # uppslag: en rad per kod (deduplicera, t.ex. G99)
          lk <- nyckel[!duplicated(as.character(nyckel[[basta_kodkol]])), ]
          bransch_map <- setNames(lk$Bransch, as.character(lk[[basta_kodkol]]))
          farg_map    <- setNames(lk$HexCode,  as.character(lk[[basta_kodkol]]))
          koder_data  <- as.character(data[[basta_datakol]])
          data$Bransch <- unname(bransch_map[koder_data])
          data$HexCode <- unname(farg_map[koder_data])
          bransch_kol <- "Bransch"
          farg_kol    <- "HexCode"
          message("Auto-kopplade branschnamn + farg via kolumnen '", basta_datakol,
                  "' (", round(basta_andel * 100), "% traff mot ", basta_kodkol, ").")
        }
      }
    }
  }
  
  # --- AUTO-DETEKTERA antal_kol -----------------------------------------------
  # Om antal_kol = NULL (inget angetts): leta forst efter en kolumn vars namn
  # EXAKT matchar (case-insensitive) nagot i antal_kol_kandidater. Hittas
  # ingen sadan, leta efter en kolumn vars namn innehaller ALLA strangar i
  # nagon av delmangderna i antal_kol_monster (ocksa case-insensitive; flera
  # delmangder = OR mellan dem, flera strangar inom en delmangd = AND).
  # Bygg pa listorna nedan for att kanna igen fler kolumnnamn/monster.
  if (is.null(antal_kol)) {
    namn_lower <- tolower(names(data))
    
    # 1) Exakta namn (t.ex. "varde", "antal")
    traff <- names(data)[namn_lower %in% tolower(antal_kol_kandidater)]
    
    # 2) Monster: alla strangar i nagon delmangd maste finnas i kolumnnamnet
    #    (t.ex. c("sysselsatta","belägenhet") matchar
    #    "sysselsatta efter arbetsställets belägenhet")
    if (length(traff) == 0 && length(antal_kol_monster) > 0) {
      ar_match <- vapply(namn_lower, function(kn) {
        any(vapply(antal_kol_monster, function(delmangd) {
          all(vapply(tolower(delmangd), function(s) grepl(s, kn, fixed = TRUE), logical(1)))
        }, logical(1)))
      }, logical(1))
      traff <- names(data)[ar_match]
    }
    
    # 3) Flera traffar? Foredra en kolumn som innehaller nagot i
    #    antal_kol_prioritet (t.ex. "arbetsstalle" eller "dagbefolkning") -
    #    vanligt att BADE "...belagenhet efter arbetsstallet..." och
    #    "...belagenhet efter bostad/dagbefolkning..." finns i samma dataset.
    if (length(traff) > 1 && length(antal_kol_prioritet) > 0) {
      traff_lower <- tolower(traff)
      prioriterade <- traff[vapply(traff_lower, function(kn) {
        any(vapply(tolower(antal_kol_prioritet), function(s) grepl(s, kn, fixed = TRUE), logical(1)))
      }, logical(1))]
      if (length(prioriterade) == 1) {
        message("Flera kolumner matchade mojliga antal_kol (",
                paste(traff, collapse = ", "), "). Valde '", prioriterade,
                "' eftersom den innehaller nagot av: ",
                paste(antal_kol_prioritet, collapse = ", "), ".")
        traff <- prioriterade
      }
    }
    
    if (length(traff) == 1) {
      antal_kol <- traff
      message("Auto-detekterade antal_kol = '", antal_kol, "'.")
    } else if (length(traff) > 1) {
      stop("Flera kolumner matchar mojliga antal_kol (",
           paste(traff, collapse = ", "), "). Ange antal_kol explicit.",
           call. = FALSE)
    } else {
      stop("Kunde inte auto-detektera antal_kol (ingen kolumn matchar ",
           "antal_kol_kandidater eller antal_kol_monster). Ange antal_kol ",
           "explicit, eller utoka listorna. Tillgangliga kolumner: ",
           paste(names(data), collapse = ", "), ".", call. = FALSE)
    }
  }
  
  # --- AUTO-DETEKTERA bransch_kol ---------------------------------------------
  # Om bransch_kol = NULL (inget angetts, och autokoppla_branschnamn_farg har
  # inte redan satt den ovan): leta forst efter en kolumn som heter exakt
  # "bransch" (skiftlagesokanslig), annars en kolumn med "SNI" i namnet.
  if (is.null(bransch_kol)) {
    namn_lower <- tolower(names(data))
    
    traff <- names(data)[namn_lower %in% tolower(bransch_kol_kandidater)]
    
    if (length(traff) == 0 && length(bransch_kol_monster) > 0) {
      ar_match <- vapply(namn_lower, function(kn) {
        any(vapply(bransch_kol_monster, function(delmangd) {
          all(vapply(tolower(delmangd), function(s) grepl(s, kn, fixed = TRUE), logical(1)))
        }, logical(1)))
      }, logical(1))
      traff <- names(data)[ar_match]
    }
    
    # Flera traffar? bransch_kol ANVANDS DIREKT SOM LEGENDTEXT (klartext), sa
    # ta bort ev. kolumner vars namn tyder pa att de innehaller KODER istallet
    # (t.ex. "SNI-kod" bredvid "SNI-benamning") - se bransch_kol_kod_monster.
    if (length(traff) > 1 && length(bransch_kol_kod_monster) > 0) {
      traff_lower <- tolower(traff)
      ar_kod <- vapply(traff_lower, function(kn) {
        any(vapply(tolower(bransch_kol_kod_monster), function(s) grepl(s, kn, fixed = TRUE), logical(1)))
      }, logical(1))
      if (any(ar_kod) && !all(ar_kod)) {
        message("Flera kolumner matchade mojliga bransch_kol (",
                paste(traff, collapse = ", "), "). Tog bort ",
                paste0("'", traff[ar_kod], "'", collapse = ", "),
                " (namnet tyder pa kod, inte klartext); anvander ",
                paste0("'", traff[!ar_kod], "'", collapse = ", "), ".")
        traff <- traff[!ar_kod]
      }
    }
    
    if (length(traff) == 1) {
      bransch_kol <- traff
      message("Auto-detekterade bransch_kol = '", bransch_kol, "'.")
    } else if (length(traff) > 1) {
      stop("Flera kolumner matchar mojliga bransch_kol (",
           paste(traff, collapse = ", "), "). Ange bransch_kol explicit.",
           call. = FALSE)
    } else {
      stop("Kunde inte auto-detektera bransch_kol (ingen kolumn matchar ",
           "bransch_kol_kandidater eller bransch_kol_monster). Ange ",
           "bransch_kol explicit, eller utoka listorna. Tillgangliga kolumner: ",
           paste(names(data), collapse = ", "), ".", call. = FALSE)
    }
  }
  
  # --- Kontrollera att angivna kolumner finns --------------------------------
  saknade <- character(0)
  if (!is.null(grupp_kol) && !(grupp_kol %in% names(data)))
    saknade <- c(saknade, paste0("grupp_kol = '", grupp_kol, "'"))
  if (!(bransch_kol %in% names(data)))
    saknade <- c(saknade, paste0("bransch_kol = '", bransch_kol, "'"))
  if (!(antal_kol %in% names(data)))
    saknade <- c(saknade, paste0("antal_kol = '", antal_kol, "'"))
  if (length(saknade) > 0) {
    stop("Foljande kolumn(er) saknas i datan: ",
         paste(saknade, collapse = ", "), ".\n",
         "Tillgangliga kolumner: ", paste(names(data), collapse = ", "),
         call. = FALSE)
  }
  # farg_kol ar valfri - varna bara (auto-palett anvands om den saknas)
  if (!is.null(farg_kol) && !(farg_kol %in% names(data))) {
    message("Obs: farg_kol = '", farg_kol, "' saknas - anvander automatisk palett.")
  }
  
  # --- Font: forsok registrera automatiskt, annars fall tillbaka tyst --------
  if (autoladda_typsnitt && !is.null(font) && nzchar(font)) {
    ok <- tryCatch(suppressWarnings(suppressMessages(
      aktivera_font(font, google = FALSE))),
      error = function(e) FALSE)
    if (!isTRUE(ok)) {
      # forsok hamta fran Google Fonts
      ok <- tryCatch(suppressWarnings(suppressMessages(
        aktivera_font(font, google = TRUE))),
        error = function(e) FALSE)
    }
    if (!isTRUE(ok)) {
      # kunde inte ladda - anvand standardfont sa ingen varning uppstar
      font <- "sans"
    }
  }
  
  # --- Manad och geografisk omfattning (for auto-rubrik OCH auto-filnamn) ----
  # Manad fran en kolumn i formatet YYYYMmm (t.ex. "2026M03")
  man_txt <- NULL
  for (kol in names(data)) {
    v <- as.character(data[[kol]])
    tr <- v[grepl("^[0-9]{4}M[0-9]{2}$", v)]
    if (length(tr) > 0) {
      mv <- tr[1]; ar <- substr(mv, 1, 4); mn <- as.integer(substr(mv, 6, 7))
      manader <- c("januari","februari","mars","april","maj","juni",
                   "juli","augusti","september","oktober","november","december")
      if (!is.na(mn) && mn >= 1 && mn <= 12) man_txt <- paste0(manader[mn], " ", ar)
      break
    }
  }
  # Geografisk omfattning (en kommun -> namn; alla i ett lan -> lanets kortnamn)
  omfattning <- bestam_omfattning(data, layout_tabell, grupp_kol)
  
  # --- AUTO-RUBRIK: skapa en rimlig titel om ingen angetts -------------------
  if (is.null(titel)) {
    titel <- paste0("Sysselsatta per bransch",
                    if (!is.null(omfattning)) paste0(" i ", omfattning) else "",
                    if (!is.null(man_txt))    paste0(" i ", man_txt)    else "")
  }
  
  # --- Standardisera internt -------------------------------------------------
  df <- data
  df$.antal   <- df[[antal_kol]]
  df$.bransch <- df[[bransch_kol]]
  if (is.null(grupp_kol)) { df$.grupp <- "Alla"; layout <- "none" }
  else                    { df$.grupp <- as.character(df[[grupp_kol]]) }
  df$.farg <- if (!is.null(farg_kol) && farg_kol %in% names(df)) df[[farg_kol]] else NA_character_
  df <- df[!is.na(df$.antal) & df$.antal > 0, ]
  
  # --- Aggregera: summera antal per grupp + bransch --------------------------
  # Datan kan ha flera rader per bransch (t.ex. uppdelat pa kon och
  # fodelseregion). Allt som INTE ar grupp_kol slas ihop till en total sa att
  # varje bransch blir EN cirkel per grupp.
  # (dplyr istallet for aggregate() - snabbare och slipper den tysta
  # factor-konverteringen som aggregate() gor internt.)
  saknar_grupp_bransch <- sum(is.na(df$.grupp) | is.na(df$.bransch))
  if (saknar_grupp_bransch > 0) {
    message("Obs: ", saknar_grupp_bransch, " rad(er) med NA i grupp- eller ",
            "branschkolumnen ignoreras vid aggregering (aggregate() gjorde ",
            "tidigare samma sak, men helt tyst).")
  }
  agg <- df %>%
    dplyr::filter(!is.na(.grupp), !is.na(.bransch)) %>%
    dplyr::group_by(.grupp, .bransch) %>%
    dplyr::summarise(.antal = sum(.antal, na.rm = TRUE), .groups = "drop")
  # behall en farg per bransch
  farg_map <- df$.farg[!duplicated(df$.bransch)]
  names(farg_map) <- df$.bransch[!duplicated(df$.bransch)]
  df <- data.frame(.grupp   = as.character(agg$.grupp),
                   .bransch = as.character(agg$.bransch),
                   .antal   = agg$.antal,
                   .farg    = unname(farg_map[as.character(agg$.bransch)]),
                   stringsAsFactors = FALSE)
  
  # --- Fargpalett: en hex per bransch ---------------------------------------
  if (all(is.na(df$.farg))) {
    bran   <- unique(df$.bransch)
    palett <- setNames(scales::hue_pal()(length(bran)), bran)
  } else {
    pp <- dplyr::distinct(df, .bransch, .farg)
    palett <- setNames(pp$.farg, pp$.bransch)
  }
  legend_ordning <- names(palett)
  
  # Radbryt langa legendetiketter (t.ex. fullstandiga SNI 2007-namn)
  radbryt <- function(txt, bredd) {
    if (is.null(bredd) || bredd <= 0) return(txt)
    vapply(txt, function(s) paste(strwrap(s, width = bredd), collapse = "\n"),
           character(1))
  }
  legend_etiketter <- radbryt(legend_ordning, legend_radbryt)
  names(legend_etiketter) <- legend_ordning
  
  # --- Packa varje grupp till en ENHETSKLUNGA (ring-radie = 1) ---------------
  grupper <- unique(df$.grupp)
  pk <- list()
  for (g in grupper) {
    sub <- df[df$.grupp == g, , drop = FALSE]
    # storlek_ut=TRUE -> minst forst (minst i mitten, storst ut)
    sub <- sub[order(sub$.antal, decreasing = !storlek_ut), ]
    if (packning == "spiral") {
      lay <- spiral_layout(sub$.antal)
    } else {
      lay <- circleProgressiveLayout(sub$.antal, sizetype = "area")
    }
    if (ring_metod == "omslutande") {
      mc <- minsta_omslutande(lay$x, lay$y, lay$radius)
      base <- mc$R
      lay$x <- (lay$x - mc$cx) / mc$R     # centrera pa omslutande cirkelns mitt
      lay$y <- (lay$y - mc$cy) / mc$R
      lay$radius <- lay$radius / mc$R
    } else {
      base <- max(sqrt(lay$x^2 + lay$y^2) + lay$radius)  # avstand fran origo
      lay$x <- lay$x / base; lay$y <- lay$y / base; lay$radius <- lay$radius / base
    }
    sub <- cbind(sub, lay)
    sub$.gid <- g
    pk[[g]] <- list(sub = sub, total = sum(sub$.antal), base = base)
  }
  
  totals  <- sapply(pk, function(z) z$total)
  maxtot  <- max(totals)
  
  # --- Klungstorlek (radie) --------------------------------------------------
  if (skala_grupp) {
    if (is.null(skala_styrka)) {
      # Auto: dampa MER ju storre spridning det ar mellan storsta och minsta
      # gruppens totalsumma - annars forsvinner de sma grupperna till punkter.
      # Liten spridning (ratio nara 1) -> nara full proportion (1.0);
      # ratio ~10x -> ~0.45 (samma niva som det gamla fasta defaultvardet);
      # mycket stor spridning -> golv pa 0.3 sa storleksskillnaden alltid syns.
      ratio <- maxtot / max(min(totals), 1)
      skala_styrka <- max(0.3, min(1, 1 - 0.55 * log10(ratio)))
    }
    klung_r <- (totals / maxtot)^skala_styrka      # 0..1
  } else {
    klung_r <- setNames(rep(1, length(totals)), names(totals))
  }
  klung_r <- klung_r / max(klung_r)                # normalisera till max=1
  
  # --- Positionering ---------------------------------------------------------
  pos <- data.frame(.gid = grupper, stringsAsFactors = FALSE)
  
  if (layout == "geo") {
    lt <- layout_tabell; names(lt)[names(lt) == "grupp"] <- ".gid"
    pos <- dplyr::left_join(pos, lt, by = ".gid")
    if (any(is.na(pos$gx))) {
      saknas <- pos$.gid[is.na(pos$gx)]
      warning("Saknar koordinater for: ", paste(saknas, collapse = ", "),
              " - placeras i (0,0). Kontrollera stavning i layout_tabell.")
      pos$gx[is.na(pos$gx)] <- 0; pos$gy[is.na(pos$gy)] <- 0
    }
    R <- klung_r[pos$.gid]
    n <- nrow(pos)
    if (geo_metod == "skala_bubblor") {
      # KOORDINATER FIXERAS pa layout_tabell-positionerna. Bubblorna krymps sa
      # att inget overlappar. Detta bevarar kommunernas relativa avstand exakt.
      pos$ox <- pos$gx
      pos$oy <- pos$gy
      # Hitta storsta skalfaktor s sa att (R[i]+R[j])*ring_marginal*s <= d_geo for alla par.
      # Vektoriserat med dist()/outer() istallet for dubbel for-loop (snabbare, och
      # undviker R:s "1:(n-1)"-sekvensfalla som kraschade nar n == 1).
      if (n >= 2) {
        d_mat <- as.matrix(dist(cbind(pos$gx, pos$gy)))
        Rsum  <- outer(R, R, "+") * ring_marginal
        tillaten_mat <- d_mat / Rsum
        tillaten_mat[d_mat == 0] <- NA        # ogiltigt: samma koordinat (division med 0)
        diag(tillaten_mat) <- NA              # uteslut diagonalen (cirkel mot sig sjalv)
        s <- min(tillaten_mat, na.rm = TRUE)
        if (!is.finite(s)) s <- 1             # alla par pa samma punkt - inget att skala mot
      } else {
        s <- 1                                # en enda grupp - ingen kollision att undvika
      }
      s <- s * 0.98 * tathet  # 2% sakerhetsmarginal, justerbar via tathet
      klung_r <- klung_r * s  # multiplicera in skalan i klungradierna
    } else {
      # SKALA UPP KOORDINATERNA (gamla beteendet). Bubblorna behaller storleken
      # men kommunernas inbordes avstand tanjs olika mycket. Snabbare men ger
      # mindre trogen geografisk placering.
      behov <- 0
      if (n >= 2) {
        d_mat <- as.matrix(dist(cbind(pos$gx, pos$gy)))
        Rsum  <- outer(R, R, "+")
        kvot_mat <- Rsum / d_mat
        kvot_mat[d_mat == 0] <- 0             # samma koordinat - begransar inte skalan
        diag(kvot_mat) <- 0
        behov <- max(kvot_mat, na.rm = TRUE)
        if (!is.finite(behov)) behov <- 0
      }
      k <- behov * 1.08 / tathet
      pos$ox <- pos$gx * k
      pos$oy <- pos$gy * k
    }
    
    # KOMPAKTERING: dra ihop kommunerna mot tyngdpunkten sa mellanrummen
    # minimeras, men behall ungefar geografisk placering. Varje kommun flyttas
    # i sma steg mot mitten sa lange den inte overlappar nagon annan.
    if (kompaktera) {
      Rkm <- klung_r[pos$.gid] * ring_marginal     # kommunradie (ring)
      cgx <- mean(pos$ox); cgy <- mean(pos$oy)
      for (iter in 1:500) {
        flyttad <- FALSE
        for (i in 1:n) {
          dx <- cgx - pos$ox[i]; dy <- cgy - pos$oy[i]
          d <- sqrt(dx^2 + dy^2)
          if (d < 1e-6) next
          steg <- 0.02 * d                          # 2 % av avstandet mot mitten
          nx <- pos$ox[i] + dx/d * steg
          ny <- pos$oy[i] + dy/d * steg
          ok <- TRUE
          for (j in 1:n) {
            if (j == i) next
            # 5 % marginal sa kommunerna inte ligger helt dikt an
            if (sqrt((nx-pos$ox[j])^2 + (ny-pos$oy[j])^2) < (Rkm[i] + Rkm[j]) * 1.05) {
              ok <- FALSE; break
            }
          }
          if (ok) { pos$ox[i] <- nx; pos$oy[i] <- ny; flyttad <- TRUE }
        }
        if (!flyttad) break
      }
    }
    
  } else if (layout == "repel") {
    # Tatpackad layout: behandla varje KOMMUN som en cirkel (med ringradien)
    # och packa dem sa tatt som mojligt. Startvinklarna tas fran geo-
    # koordinaterna sa att kommuner hamnar ungefar ratt relativt varandra,
    # men de dras ihop tills ringarna precis nuddar.
    if (!is.null(layout_tabell)) {
      lt <- layout_tabell; names(lt)[names(lt) == "grupp"] <- ".gid"
      pos <- dplyr::left_join(pos, lt, by = ".gid")
    }
    if (is.null(pos$gx) || any(is.na(pos$gx))) {
      ang <- seq(0, 2*pi, length.out = nrow(pos) + 1)[-1]
      pos$gx <- 50 + 40 * cos(ang); pos$gy <- 50 + 40 * sin(ang)
    }
    
    # Kollisionsradie = klungradie * ring_marginal (sa ringarna inte korsar)
    R <- klung_r[pos$.gid] * ring_marginal
    
    # Startposition: behall geo-RIKTNING fran tyngdpunkten, men krymp ihop
    cx <- mean(pos$gx); cy <- mean(pos$gy)
    px <- pos$gx - cx;  py <- pos$gy - cy
    span <- max(sqrt(px^2 + py^2)); if (span == 0) span <- 1
    start_r <- sqrt(sum(R^2)) * 0.6          # kompakt startblob
    px <- px / span * start_r
    py <- py / span * start_r
    
    n <- length(px)
    # Hela simuleringen kraver minst tva punkter (n == 1 hade tidigare en
    # "1:(n-1)"-sekvensfalla som kraschade - nu skyddad med if (n >= 2)).
    if (n >= 2) {
      for (iter in 1:3000) {
        moved <- FALSE
        # 1) Knuffa isar overlappande par (sekventiell fysiksimulering - varje
        #    par paverkar naston direkt, darfor kvar som loop och inte vektoriserad)
        for (i in 1:(n-1)) for (j in (i+1):n) {
          dx <- px[j]-px[i]; dy <- py[j]-py[i]
          d  <- sqrt(dx^2 + dy^2); mind <- R[i] + R[j]
          if (d < mind && d > 1e-9) {
            push <- (mind - d) / 2; ux <- dx/d; uy <- dy/d
            px[i] <- px[i]-ux*push; py[i] <- py[i]-uy*push
            px[j] <- px[j]+ux*push; py[j] <- py[j]+uy*push
            moved <- TRUE
          }
        }
        # 2) Mild dragning mot tyngdpunkten for att halla det tatt
        if (!moved) {
          gx_ <- mean(px); gy_ <- mean(py)
          px <- px - (px - gx_) * 0.01
          py <- py - (py - gy_) * 0.01
          # avbryt sa fort dragningen skulle skapa overlapp igen - vektoriserad
          # koll med dist()/outer() istallet for dubbel for-loop
          d_mat <- as.matrix(dist(cbind(px, py)))
          Rsum  <- outer(R, R, "+")
          diag(d_mat) <- Inf                    # uteslut diagonalen
          if (any(d_mat < Rsum - 1e-6)) break
        }
      }
    }
    pos$ox <- px / tathet; pos$oy <- py / tathet
    
  } else if (layout == "vinkel") {
    # Vinkelstyrd: valj en startgrupp som centrum. Varje annan grupp far en
    # onskad vinkel (geografisk riktning fran start enligt layout_tabell) och
    # placeras pa det minsta avstand dar dess ring precis nuddar nagon redan
    # placerad grupp. Storsta grupperna placeras forst for tatare packning.
    lt <- layout_tabell; names(lt)[names(lt) == "grupp"] <- ".gid"
    pos <- dplyr::left_join(pos, lt, by = ".gid")
    if (any(is.na(pos$gx))) {
      # ge saknade grupper jamnt fordelade vinklar
      na_i <- which(is.na(pos$gx))
      ang0 <- seq(0, 2*pi, length.out = length(na_i) + 1)[-1]
      pos$gx[na_i] <- 50 + 40*cos(ang0); pos$gy[na_i] <- 50 + 40*sin(ang0)
    }
    R <- klung_r[pos$.gid] * ring_marginal
    names(R) <- pos$.gid
    
    # Startgrupp: angiven, annars storsta
    if (is.null(start_grupp)) {
      start <- pos$.gid[which.max(totals[pos$.gid])]
    } else {
      start <- start_grupp
    }
    if (!(start %in% pos$.gid)) {
      stop("start_grupp = '", start, "' finns inte bland grupperna. ",
           "Tillgangliga: ", paste(pos$.gid, collapse = ", "), ". ",
           "OBS att layout_tabell anvander svenska tecken (t.ex. 'Borlange' vs 'Borlänge').")
    }
    sx <- pos$gx[pos$.gid == start]; sy <- pos$gy[pos$.gid == start]
    
    # Onskad vinkel for varje ovrig grupp
    ang <- atan2(pos$gy - sy, pos$gx - sx); names(ang) <- pos$.gid
    
    # Placera storst forst (utom start som ligger i origo)
    ovr <- pos$.gid[pos$.gid != start]
    ovr <- ovr[order(-R[ovr])]
    
    OX <- setNames(numeric(nrow(pos)), pos$.gid)
    OY <- setNames(numeric(nrow(pos)), pos$.gid)
    placerade <- start  # OX/OY redan 0 for start
    
    for (g in ovr) {
      a <- ang[g]; ux <- cos(a); uy <- sin(a)
      t <- R[start] + R[g]                 # minst tangent till start
      for (s in 1:500) {
        ok <- TRUE
        for (q in placerade) {
          qx <- OX[q]; qy <- OY[q]
          d <- sqrt((t*ux - qx)^2 + (t*uy - qy)^2)
          need <- R[g] + R[q]
          if (d < need - 1e-9) {
            # los andragradsekv for minsta t langs strale dar avstand = need
            B <- -2*(ux*qx + uy*qy); C <- qx^2 + qy^2 - need^2
            disc <- B^2 - 4*C
            if (disc >= 0) t <- max(t, (-B + sqrt(disc)) / 2) else t <- t + need*0.1
            ok <- FALSE
          }
        }
        if (ok) break
      }
      OX[g] <- t*ux; OY[g] <- t*uy
      placerade <- c(placerade, g)
    }
    pos$ox <- (OX[pos$.gid]) / tathet
    pos$oy <- (OY[pos$.gid]) / tathet
    
  } else if (layout == "grid") {
    ord <- order(-totals[pos$.gid])
    pos <- pos[ord, , drop = FALSE]
    R <- klung_r[pos$.gid]
    cell <- max(R) * 2.3 / tathet
    idx <- seq_len(nrow(pos)) - 1
    pos$ox <-  (idx %% ncol)  * cell
    pos$oy <- -(idx %/% ncol) * cell
    
  } else { # none
    pos$ox <- 0; pos$oy <- 0
  }
  rownames(pos) <- pos$.gid
  
  # --- Bygg slutliga koordinater ---------------------------------------------
  cirklar_lst <- list(); poly_lst <- list(); ring_lst <- list()
  for (g in grupper) {
    sub <- pk[[g]]$sub
    r   <- klung_r[[g]]; ox <- pos[g,"ox"]; oy <- pos[g,"oy"]
    sub$x      <- sub$x * r + ox
    sub$y      <- sub$y * r + oy
    sub$radius <- sub$radius * r
    cirklar_lst[[g]] <- sub
    
    # polygoner for mjuka cirkelkanter
    lay_g <- data.frame(x = sub$x, y = sub$y, radius = sub$radius)
    poly  <- circleLayoutVertices(lay_g, npoints = 60)
    poly$.bransch <- sub$.bransch[poly$id]
    poly$.uid     <- paste(g, poly$id, sep = "_")
    poly_lst[[g]] <- poly
    
    if (visa_ring) {
      ring_lst[[g]] <- data.frame(x0 = ox, y0 = oy, r = r * ring_marginal, .gid = g)
    }
  }
  cirklar   <- dplyr::bind_rows(cirklar_lst)
  polygoner <- dplyr::bind_rows(poly_lst)
  
  # --- Plot ------------------------------------------------------------------
  txt_default <- if (bakgrund %in% c("black","#000000","grey10")) "white" else "black"
  
  # Skalreferens: radie-per-antal i den STORSTA klungans skala.
  # En branschcirkel med "antal" sysselsatta i den storsta klungan far:
  #   r_norm = sqrt(antal/pi) / base_ref   (enhetsklunga)
  #   r_plot = r_norm * klung_r[ref_g]     (efter all skalning inkl. geo_metod)
  ref_g    <- names(which.max(klung_r))
  base_ref <- pk[[ref_g]]$base
  klung_r_ref <- klung_r[[ref_g]]
  radie_av_antal <- function(a) sqrt(a / pi) / base_ref * klung_r_ref
  
  # Plotomradets utstrackning (for hojd_andel och placering av skalbubblor).
  # Ta MED ringarna - de ar storre an branschcirklarna och bestammer kanten.
  if (visa_ring && length(ring_lst) > 0) {
    rdf <- dplyr::bind_rows(ring_lst)
    y_min <- min(rdf$y0 - rdf$r)
    y_max <- max(rdf$y0 + rdf$r)
    x_min <- min(rdf$x0 - rdf$r)
    x_max <- max(rdf$x0 + rdf$r)
  } else {
    y_min <- min(cirklar$y - cirklar$radius)
    y_max <- max(cirklar$y + cirklar$radius)
    x_min <- min(cirklar$x - cirklar$radius)
    x_max <- max(cirklar$x + cirklar$radius)
  }
  # Spara ringarnas extent separat - xlim baseras pa dessa, inte skalbubblor.
  # Skalbubblor ritas med clip = "off" och far ligga utanfor panelen.
  ring_x_min <- x_min; ring_x_max <- x_max
  ring_y_min <- y_min; ring_y_max <- y_max
  h_tot <- y_max - y_min
  w_tot <- x_max - x_min
  
  # Uppskattad texthojd i DATA-enheter for en given ggplot2 'size' (samma
  # kalibreringskonstant, 0.004 * h_tot per storleksenhet, som anvands for
  # gruppnamnen langre ner - approximativt men konsekvent over hela figuren).
  # Anvands for att ge titel/caption EXAKT sa mycket marginal de behover,
  # istallet for en fast schablonandel (se hojd_andel = NULL nedan).
  text_hojd <- function(storlek) storlek * h_tot * 0.004
  txt_luft  <- h_tot * 0.02                                      # luft runt texten
  titel_texthojd   <- if (!is.null(titel))           text_hojd(storlek_titel)   else 0
  caption_texthojd <- if (!is.null(diagram_caption)) text_hojd(storlek_caption) else 0
  marg_top_auto    <- txt_luft + titel_texthojd   + txt_luft
  marg_bottom_auto <- txt_luft + caption_texthojd + txt_luft
  
  p <- ggplot()
  if (visa_ring) {
    if (!requireNamespace("ggforce", quietly = TRUE))
      stop("Paketet 'ggforce' kravs for visa_ring = TRUE.")
    ringar <- dplyr::bind_rows(ring_lst)
    p <- p + ggforce::geom_circle(
      data = ringar, aes(x0 = x0, y0 = y0, r = r),
      colour = ring_farg, fill = NA, linewidth = 0.3, inherit.aes = FALSE)
  }
  p <- p +
    geom_polygon(data = polygoner, aes(x, y, group = .uid, fill = .bransch),
                 colour = NA) +
    scale_fill_manual(values = palett, name = NULL, breaks = legend_ordning,
                      labels = legend_etiketter) +
    theme_void(base_family = font) +
    theme(plot.background  = element_rect(fill = bakgrund, colour = NA),
          panel.background = element_rect(fill = bakgrund, colour = NA),
          legend.text  = element_text(colour = txt_default, size = storlek_legend),
          legend.key.size = unit(storlek_legend * 0.06, "cm"),
          # Legenden utanfor panelen till hoger = klipps aldrig, tar inte plats
          # fran cirklarna. Vertikalt centrerad med plot.
          legend.position    = "right",
          legend.box.just    = "center",
          legend.background  = element_blank(),
          legend.margin      = margin(0,0,0,0),
          # liten luft mellan legendtexten och hogerkanten
          plot.margin        = margin(t = 5, r = 18, b = 5, l = 5))
  # Titel centrerad over cirklarna (inte hela panelen) via annotate.
  # Berakna klungornas horisontella mitt fran ring- eller cirkeldata.
  kx_mitt <- (x_min + x_max) / 2
  if (!is.null(titel)) {
    # Placera titeln ovanfor allt innehall
    titel_y <- y_max + txt_luft
    p <- p + annotate("text", x = kx_mitt, y = titel_y, label = titel,
                      hjust = 0.5, vjust = 0, size = storlek_titel, fontface = "bold",
                      colour = txt_default, family = font)
  }
  
  # diagram_caption vansterstalld, i linje med klungornas vanstra kant.
  if (!is.null(diagram_caption)) {
    caption_y <- y_min - txt_luft
    p <- p + annotate("text", x = x_min, y = caption_y, label = diagram_caption,
                      hjust = 0, vjust = 1, size = storlek_caption,
                      colour = txt_default, family = font)
  }
  
  # --- (3) Skalbubblor: svarta cirklar staplade vertikalt, minst overst ------
  skal_varden <- NULL
  visa_labels_vid_skala <- isTRUE(labels_visa_skalvarden)
  
  # Klungornas vertikala mitt (utan skalbubblor) - anvands for centrering
  klunge_y_mitt <- (y_min + y_max) / 2
  # Skalbubblornas utstrackning (uppdateras om skalbubblor ritas)
  skal_x_min <- ring_x_min; skal_y_min <- ring_y_min; skal_y_max <- ring_y_max
  
  # Berakna skalvarden om vi ska rita skalbubblor ELLER visa labels vid skalvarden.
  # Utan skalbubblor anvands 5 niva for labels_visa_skalvarden.
  if (skal_bubblor > 0 || visa_labels_vid_skala) {
    n_skal <- if (skal_bubblor > 0) skal_bubblor else 5
    rng <- range(cirklar$.antal)
    skal_varden <- bra_skalvarden(rng[1], rng[2], n_skal)
  }
  
  if (skal_bubblor > 0 && !is.null(skal_varden)) {
    sv <- sort(skal_varden)                       # minst forst (overst)
    r_sv <- radie_av_antal(sv)
    bx <- x_min - max(r_sv) * 2.2                # x till vanster om klungorna (mer luft)
    
    # Mellanrum mellan skalbubblor
    gap <- max(min(r_sv) * 0.5, h_tot * 0.01)
    
    # Berakna total hojd av staplade skalbubblor med mellanrum
    total_skal_h <- sum(r_sv) * 2 + gap * (length(sv) - 1)
    # Centrera stapeln kring klunge_y_mitt
    cy <- numeric(length(sv))
    acc <- klunge_y_mitt + total_skal_h / 2        # top-of-stack
    for (i in seq_along(sv)) {
      cy[i] <- acc - r_sv[i]                      # tangerar linjen ovanfor
      acc   <- cy[i] - r_sv[i] - gap              # nasta hamnar under + gap
    }
    
    # Textposition: ALLA texter i samma x-kolumn (hoger om storsta bubblan)
    txt_x <- bx + max(r_sv) + h_tot * 0.015
    
    # Etiketternas y: utga fran bubblans centrum, men tvinga ett MINSTA
    # vertikalt avstand sa sma bubblors siffror inte hamnar ovanpa varandra.
    min_lbl_gap <- h_tot * 0.022 * (storlek_skal_text / 3.2)
    lbl_y <- cy                                    # borja vid bubbelcentrum
    # cy gar fran topp (storst y) till botten; tvinga gap nedat.
    # Skyddad med length >= 2 (annars kraschade "2:length(lbl_y)" nar
    # skal_bubblor == 1, dvs 2:1 = c(2,1) - R:s klassiska sekvensfalla).
    if (length(lbl_y) >= 2) {
      for (i in 2:length(lbl_y)) {
        if (lbl_y[i-1] - lbl_y[i] < min_lbl_gap) {
          lbl_y[i] <- lbl_y[i-1] - min_lbl_gap
        }
      }
    }
    
    skal_df <- data.frame(x = bx, y = cy, r = r_sv, lbl = format(sv, big.mark = " "))
    lbl_df  <- data.frame(x = txt_x, y = lbl_y, lbl = format(sv, big.mark = " "))
    p <- p +
      ggforce::geom_circle(data = skal_df, aes(x0 = x, y0 = y, r = r),
                           fill = "black", colour = "black", inherit.aes = FALSE) +
      geom_text(data = lbl_df,
                aes(x = x, y = y, label = lbl),
                hjust = 0, size = storlek_skal_text, colour = txt_default, family = font)
    
    # Spara skalbubblornas vansterkant for inkludering i xlim
    skal_x_min <- min(skal_df$x - skal_df$r)
    skal_y_min <- min(skal_df$y - skal_df$r)
    skal_y_max <- max(skal_df$y + skal_df$r)
  }
  
  # --- (4)+(5) Etiketter i cirklarna -----------------------------------------
  # antal_min: auto-berakna om NULL, utifran cirkelns FAKTISKA radie i den
  # ritade figuren (samma "storlek -> andel av h_tot"-heuristik som anvands
  # for gruppnamnen nedan). Sifforna visas da bara i cirklar som ar stora nog
  # for att texten ska fa rimlig plats - oavsett om datan racknar hundratal
  # eller hundratusental sysselsatta.
  if (is.null(antal_min)) {
    r_text_min <- storlek_labels * h_tot * 0.0045   # minsta radie for lasbar siffra
    antal_min  <- pi * (r_text_min * base_ref / klung_r_ref)^2
  }
  # Vilka cirklar far text?
  if (visa_labels_vid_skala) {
    # (5) TRUE = visa labels i de cirklar vars antal ligger NARMAST skalvardena
    mal <- sort(skal_varden)
    valda_idx <- sapply(mal, function(m) which.min(abs(cirklar$.antal - m)))
    cirklar$.visa <- FALSE
    cirklar$.visa[unique(valda_idx)] <- TRUE
  } else {
    cirklar$.visa <- cirklar$.antal >= antal_min
  }
  
  if (visa_antal || visa_labels_vid_skala) {
    cirklar$.lbl <- ifelse(cirklar$.visa,
                           format(cirklar$.antal, big.mark = " "), "")
    if (diskreta_labels) {
      # (4) textfarg = morkare/ljusare nyans av bubblans egen farg
      rgbm <- col2rgb(cirklar$.farg)
      lum  <- (0.299*rgbm[1,] + 0.587*rgbm[2,] + 0.114*rgbm[3,]) / 255
      # ljus bubbla -> morka ner; mork bubbla -> ljusa upp.
      # Anvand additiv shift sa att aven (0,0,0) far en synlig nyans.
      shift <- ifelse(lum < 0.5,  90, -90)        # +90 ljusare, -90 morkare
      ny_mat <- matrix(
        pmin(255, pmax(0, rgbm + matrix(shift, nrow = 3, ncol = ncol(rgbm), byrow = TRUE))),
        nrow = 3
      )
      cirklar$.txt <- rgb(ny_mat[1,], ny_mat[2,], ny_mat[3,], maxColorValue = 255)
    } else {
      rgbm <- col2rgb(cirklar$.farg)
      lum  <- (0.299*rgbm[1,] + 0.587*rgbm[2,] + 0.114*rgbm[3,]) / 255
      cirklar$.txt <- ifelse(lum < 0.5, "white", "black")
    }
    p <- p + geom_text(data = cirklar, aes(x, y, label = .lbl),
                       colour = cirklar$.txt, size = storlek_labels, family = font)
  }
  
  # --- (2) Gruppnamn: undvik att hamna inuti/over NAGON kommuns cirkel -------
  # Sveper runt egen cirkel i prioritetsordning (nedanfor forst, rakt upp sist)
  # och pa okande avstand tills etikettens hela ruta ar fri fran alla cirklar.
  if (visa_namn && !is.null(grupp_kol)) {
    ox_v <- pos[grupper, "ox"]; oy_v <- pos[grupper, "oy"]
    rr_v <- klung_r[grupper] * ring_marginal
    names(ox_v) <- grupper; names(oy_v) <- grupper; names(rr_v) <- grupper
    
    off <- h_tot * 0.012
    text_h  <- storlek_namn * h_tot * 0.004          # uppskattad texthojd
    char_w  <- text_h * 0.62                          # bredd per tecken
    
    # Exakt overlapp mellan cirkel och axelriktad ruta (etikettens bounding box)
    cirkel_rect_krock <- function(cx, cy, r, rx, ry, hw, hh) {
      dx <- abs(cx - rx); dy <- abs(cy - ry)
      if (dx > hw + r || dy > hh + r) return(FALSE)
      if (dx <= hw || dy <= hh) return(TRUE)
      ((dx - hw)^2 + (dy - hh)^2) <= r^2
    }
    
    # Vinklar i prioritetsordning: nedanfor -> ner-hoger/vanster -> sidor ->
    # upp-hoger/vanster -> rakt upp (sist). (y uppat, sa -90 grader = nedanfor)
    vinkel_pref <- c(-90, -60, -120, -30, -150, 0, 180, 30, 150, 60, 120, 90)
    
    namn_x <- numeric(length(grupper)); namn_y <- numeric(length(grupper))
    
    for (gi in seq_along(grupper)) {
      g <- grupper[gi]
      cx <- ox_v[g]; cy <- oy_v[g]; rr <- rr_v[g]
      hw <- nchar(g) * char_w / 2                    # etikettens halvbredd
      hh <- text_h / 2                                # etikettens halvhojd
      
      hittad <- FALSE
      bx <- cx; by <- cy - rr - off - hh             # default = rakt under
      # namn_utanfor = TRUE: prova okande avstand fran ringen och svep runt tills
      # etiketten inte krockar med nagon cirkel (som tidigare).
      # namn_utanfor = FALSE: stanna vid default-laget rakt under ringen - ingen
      # kollisionssvepning. (Detta var tidigare en obruken parameter utan effekt.)
      if (isTRUE(namn_utanfor)) {
        for (extra in c(0, 0.5, 1.0, 1.8) * (text_h + off)) {
          for (v in vinkel_pref) {
            ang <- v * pi / 180
            # etikettens MITT placeras utanfor ringen i denna riktning
            d <- rr + off + hh + extra
            lx <- cx + d * cos(ang)
            ly <- cy + d * sin(ang)
            # kolla etikettrutan mot ALLA cirklar (inkl. egen)
            krock <- FALSE
            for (pj in seq_along(grupper)) {
              if (cirkel_rect_krock(ox_v[pj], oy_v[pj], rr_v[pj], lx, ly, hw, hh)) {
                krock <- TRUE; break
              }
            }
            if (!krock) { bx <- lx; by <- ly; hittad <- TRUE; break }
          }
          if (hittad) break
        }
      }
      namn_x[gi] <- bx; namn_y[gi] <- by
    }
    
    namn <- data.frame(.gid = grupper, x = namn_x, y = namn_y,
                       stringsAsFactors = FALSE)
    # centrerad text (hjust/vjust 0.5) eftersom vi placerar etikettens MITT
    p <- p + geom_text(data = namn, aes(x, y, label = .gid),
                       size = storlek_namn, fontface = "bold",
                       colour = txt_default, family = font,
                       hjust = 0.5, vjust = 0.5)
  }
  
  # --- (1) hojd_andel + legend- och titelpositionering -----------------------
  # Berakna slutliga panelgranser FORST, sedan harledd legend- och titelposition.
  if (!is.null(hojd_andel) && hojd_andel > 0 && hojd_andel < 1) {
    # Explicit angivet hojd_andel - fast andel av panelhojden (som tidigare).
    marg <- h_tot * (1 - hojd_andel) / (2 * hojd_andel)
    x_marg <- marg * 0.5
    # xlim inkluderar BADE ringar OCH skalbubblor sa att inget klipps
    xl <- c(min(ring_x_min, skal_x_min) - x_marg, ring_x_max + x_marg)
    yl <- c(min(ring_y_min, skal_y_min) - marg, max(ring_y_max, skal_y_max) + marg)
  } else {
    # hojd_andel = NULL [default]: anvand det EXAKT behovda utrymmet for
    # titel/caption (marg_top_auto/marg_bottom_auto, beraknat ovan vid h_tot)
    # istallet for en fast schablonandel. Ger mindre vit luft nar ingen
    # caption ar satt, och sakert utrymme nar bade titel och caption ar satta.
    x_marg <- max(marg_top_auto, marg_bottom_auto) * 0.5
    xl <- c(min(ring_x_min, skal_x_min) - x_marg, ring_x_max + x_marg)
    yl <- c(min(ring_y_min, skal_y_min) - marg_bottom_auto,
            max(ring_y_max, skal_y_max) + marg_top_auto)
  }
  pan_w <- xl[2] - xl[1]; pan_h <- yl[2] - yl[1]
  
  p <- p + coord_equal(xlim = xl, ylim = yl, clip = "off")
  
  # --- Dynamiska bildmatt: matcha duken mot innehallets proportioner ---------
  # Beraknas alltid (aven utan sparande) sa att de kan bifogas objektet och
  # anvandas for forhandsvisning i ratt storlek.
  panel_prop <- pan_w / pan_h
  rader <- unlist(strsplit(legend_etiketter, "\n"))
  max_tecken <- if (length(rader)) max(nchar(rader)) else 0
  legend_tum <- 0.6 + (storlek_legend / 72) * max_tecken * 0.6
  bh <- if (!is.null(bildhojd))  bildhojd  else 8
  bb <- if (!is.null(bildbredd)) bildbredd else bh * panel_prop + legend_tum
  # Bifoga matten pa objektet (las med forhandsvisa() eller attr(p, "bildmatt"))
  attr(p, "bildmatt") <- c(bredd = bb, hojd = bh)
  
  # --- Spara till bildfil (valfritt) -----------------------------------------
  if (isTRUE(spara_bildfil)) {
    # 1. Mapp: angiven -> annars utskriftsmapp() om den finns -> annars fel
    if (is.null(mapp)) {
      if (exists("utskriftsmapp") && is.function(get("utskriftsmapp"))) {
        mapp <- utskriftsmapp()
      } else {
        stop("Ingen mapp angiven och funktionen utskriftsmapp() (func_api.R) ",
             "kunde inte hittas. Ange 'mapp = ...' for att spara.", call. = FALSE)
      }
    }
    
    # 2. Filnamn: angivet -> annars autogenererat enligt monstret
    #    syss_bransch[_<grupp om kon/fodelseregion>]_<geografi>_<manad>.png
    if (is.null(filnamn)) {
      delar <- "syss_bransch"
      gl <- if (!is.null(grupp_kol)) tolower(grupp_kol) else ""
      if (gl %in% c("k\u00f6n", "kon", "f\u00f6delseregion", "fodelseregion"))
        delar <- c(delar, grupp_kol)               # ta med kon/fodelseregion
      if (!is.null(omfattning)) delar <- c(delar, omfattning)
      if (!is.null(man_txt))    delar <- c(delar, man_txt)
      filnamn <- paste(delar, collapse = "_")
      # translitterera svenska tecken -> ASCII och stada
      filnamn <- chartr("\u00e5\u00e4\u00f6\u00c5\u00c4\u00d6", "aaoAAO", filnamn)
      filnamn <- gsub("[^A-Za-z0-9_-]+", "_", filnamn)
      filnamn <- sub("^_+", "", sub("_+$", "", gsub("_+", "_", filnamn)))
      filnamn <- paste0(filnamn, ".png")
    }
    if (!grepl("\\.(png|jpg|jpeg|tiff|svg|pdf)$", filnamn, ignore.case = TRUE))
      filnamn <- paste0(filnamn, ".png")
    full_sokvag <- file.path(mapp, filnamn)
    
    # 3. Matcha showtext-dpi mot ggsave sa texten far ratt storlek. on.exit
    #    aterstaller skarm-dpi (96) nar funktionen avslutas, sa att det
    #    returnerade objektet visas ratt pa skarmen (inte i 300-dpi-storlek).
    if (requireNamespace("showtext", quietly = TRUE)) {
      suppressWarnings(showtext::showtext_opts(dpi = dpi))
      on.exit(suppressWarnings(showtext::showtext_opts(dpi = 96)), add = TRUE)
    }
    ggsave(full_sokvag, p, width = bb, height = bh, dpi = dpi, bg = bakgrund)
    message("Diagram sparat: ", full_sokvag,
            sprintf(" (%.1f x %.1f tum)", bb, bh))
  }
  
  p
}

# -----------------------------------------------------------------------------
# 2b. FORHANDSVISA ett diagram i samma proportioner som den sparade bildfilen.
#     ggplot-objekt har ingen inbyggd storlek - pa skarmen ritas de i plot-
#     rutans storlek, vilket gor texten relativt storre an i den stora bildfilen.
#     forhandsvisa() oppnar en grafikenhet i ratt matt och visar diagrammet dar.
# -----------------------------------------------------------------------------
forhandsvisa <- function(p, bredd = NULL, hojd = NULL) {
  matt <- attr(p, "bildmatt")
  if (is.null(bredd)) bredd <- if (!is.null(matt)) matt[["bredd"]] else 12
  if (is.null(hojd))  hojd  <- if (!is.null(matt)) matt[["hojd"]]  else 8
  if (requireNamespace("showtext", quietly = TRUE))
    suppressWarnings(showtext::showtext_opts(dpi = 96))
  dev.new(width = bredd, height = hojd, unit = "in", noRStudioGD = TRUE)
  print(p)
  invisible(p)
}