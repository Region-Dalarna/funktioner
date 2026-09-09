# Plan: dela upp `funktioner`-repot i R-paket

Status: levande dokument. Senast uppdaterad 2026-09-09 under session
`session_014sApQqWRFx4ecXVutD9G4e`.

**Spikade beslut:** monorepo heter `rdpaket`. `rd*`-funktioner har svenska namn
(`pxweb2r` är engelskt). `rdgis` = ett paket (postgres/routing redan utbrutna).
Region-/kodlistor hämtas via SCB:s fristående codelist-endpoints, inte ur en
befolkningstabell. Supercross-funktioner: se avsnitt 3.

## Läge 2026-09-09

**7 av 8 paket byggda och pushade** till `Region-Dalarna/rdpaket`, plus
`pxweb2r` i `FaluPeppe/pxweb2r`:

| Paket | Status |
|---|---|
| `pxweb2r` | klart (eget repo, engelskt) |
| `rdverktyg` | klart |
| `rddiagram` | klart |
| `rdshinyappar` | klart |
| `rdpostgres` | klart |
| `rdgis` | klart |
| `rddeploy` | klart |
| `rdadminportal` | klart – **nytt paket**, se nedan |
| `rd` (paraply) | klart (attach:ar analyspaketen) |
| `rdgeorouting` | **kvar att bygga** – postgis/pgRouting/pendling ur `func_GIS.R` |

**Nytt sedan planen skrevs:** `func_landningssida_adminportal.R` och
`func_kor_cron_jobb.R` visade sig vara medvetet fristående filer som
`source()`:as av serverskript. De hamnade därför i ett eget paket
**`rdadminportal`** (adminportal + serverdrift), inte i `rddeploy`. De
SSH-baserade `landningssida_*` i `func_API.R` utgick (DB-varianten ersätter dem).

**Kvar efter rdgeorouting:** migrera `source(".../func_X.R")` → `library(rdX)`
i övriga funktionsfiler och konsumerande skript, med shims kvar under övergången.

---

## 1. Mål och principer

- **Ett paket = en domän.** Inte "en fil = ett paket". `func_API.R` styckas,
  andra filer slås ihop.
- **Nerifrån och upp.** Extrahera lövnoderna (inga interna beroenden) först.
- **Byt `source()` mot `Imports`.** Varje
  `source(".../func_X.R")` blir ett paketberoende. Tunn shim kvar i `funktioner`
  under övergången.
- **Luta dig mot etablerade paket** i stället för att underhålla egen kod där
  ett välkänt paket gör jobbet bättre (se avsnitt 6).
- **Packa inte pipelines.** Filer som körs rakt igenom kan förbli skript eller
  bli moduler i ett större paket.

---

## 2. Paketöversikt

| Paket | Repo | Innehåll | Tunga beroenden |
|---|---|---|---|
| `pxweb2r` | eget (`FaluPeppe/pxweb2r`) | **klart** – PxWeb API v2 | httr, jsonlite |
| `rdverktyg` | monorepo | `func_text.R` + `func_filer.R` + SCB-/region-hämtning + allmänna verktyg + fil-/formatinläsning | dplyr, purrr, stringr, tibble, tidyr, httr/httr2, jsonlite, `pxweb2r` |
| `rddiagram` | monorepo | `func_SkapaDiagram.R` + `func_diagramfunktioner.R` + `func_logga_i_diagram.R` + `func_bubbeldiagram.R` | tidyverse, ggplot2, RColorBrewer, magick, sf, ggrepel, cowplot, ggtext, packcircles |
| `rdgis` | monorepo | ren geometri/sf ur `func_GIS.R` | sf, mapview |
| `rdpostgres` | monorepo | `postgres_*` / `uppkoppling_*` ur `func_GIS.R` | DBI, RPostgres |
| `rdgeorouting` | monorepo | `postgis_*`, `pgrouting_*`, `pendling_*` | sf, pgRouting |
| `rddeploy` | monorepo | github/git/deploy + webbrapport/portal + shinyapp-scaffolding (ur `func_API.R`) | gh, gert, gitcreds, usethis |
| `rdadminportal` | monorepo | landningssidor/ikoner/nedladdningar + `kor_cron_jobb()` (ur `func_landningssida_adminportal.R` + `func_kor_cron_jobb.R`) | DBI, `rdshinyappar` |
| `rdshinyappar` | monorepo | **exakt** innehållet i `func_shinyappar.R` (runtime: DB, lösenord, `df_till_sf`, telemetri) | DBI, RPostgres, sf |
| `rd` (paraply) | monorepo | `Depends:` på analyspaketen – ger `library(rd)` för analysarbete (ej `rddeploy`/`rdadminportal`) | – |

Senare, ej i denna omgång: `rdgtfs` (`func_gtfs.R` + `func_trafiklab_till_postgis.R`),
`rdotp` (`func_otp.R`), `rdlupp` (`func_lupp.R`), `rdqgispendling` (`func_qgis_pendling.R`),
`rdsvg` (`func_svg.R`). Utanför: webbskrapning (eget repo, likt `pxweb2r`).

### Monorepo-upplägg

```
Region-Dalarna/rdpaket/
  packages/
    rdverktyg/     DESCRIPTION, R/, man/, tests/, inst/
    rddiagram/
    rdgis/
    rdpostgres/
    rdgeorouting/
    rddeploy/
    rdshinyappar/
    rd/
  install_all.R    # pak::pkg_install(local subdirs)
```

Installeras per paket:
`remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdgis")`.
Beroende `rd*`-paket installeras automatiskt. `library(rdgis)` räcker i skript;
`library(rd)` drar in allt interaktivt.

---

## 3. `func_API.R` (146 funktioner) – disposition per grupp

### A. SCB/PxWeb – tabeller, tid, giltiga värden → **ersätts av `pxweb2r`**

`hamtaregtab`, `hamta_senaste_tid_i_tabell`, `hamta_tidigaste_tid_i_tabell`,
`hamta_giltig_tid_tabell`, `pxvarlist`, `pxvardelist`,
`hamta_giltiga_varden_fran_tabell`, `hamta_kod_med_klartext`,
`hamta_klartext_med_kod`, `sla_upp_varde_klartext_kod`,
`hamta_kod_eller_klartext_fran_lista`,
`konvertera_till_long_for_contentscode_variabler`,
`kontrollera_pxweb_variabelvarden`, `kontrollera_pxweb_url`,
`scb_tabellid_extrahera_fran_url`, `extrahera_unika_varden_flera_scb_tabeller`,
`sortera_px_variabler`, `overlappande_varden_pxweb_hantera`

- **Behåll:** `manader_bearbeta_scbtabeller()` (månadskolumn-bearbetning, ingen
  motsvarighet i `pxweb2r`) → `rdverktyg`.
- **Byt ut:** resten mot `pxweb2r`-funktioner
  (`pxweb2_get_values()`, `pxweb2_get_metadata()`, `pxweb2_search_tables()`).
  Gå igenom anropsställen i övriga filer och ersätt.
- **Stryk:** `skapa_hamta_data_skript_pxweb()` – behövs inte längre
  (`pxweb2r::pxweb2_data_script_template()` täcker behovet).

### B. Region-/områdeskoder → **`rdverktyg`, tunna wrappers över `pxweb2r`**

`hamtaregtab` (navet), `hamtakommuner`, `hamtaAllaLan`, `hamtaregion_kod_namn`,
`hamta_kommunkoder`, `skapa_kortnamn_lan`, `ar_alla_kommuner_i_ett_lan`,
`ar_alla_lan_i_sverige`, `regsokoder_bearbeta`, `desokoder_bearbeta`,
`tatortskoder_bearbeta`, `region_kolumn_splitta_kod_klartext`,
`hamta_regionkod_med_knas_regionkod`

Byggs på SCB:s **fristående codelist-endpoints** (kräver ingen tabell):

| Endpoint | Innehåll |
|---|---|
| `/api/v2/codelists/vs_RegionKommun07` | 290 kommuner |
| `/api/v2/codelists/vs_RegionLän07` | 21 län |
| `/api/v2/codelists/vs_RegionRiket99` | Riket |

```r
hamtakommuner_alla <- function() pxweb2r::pxweb2_get_codelist("vs_RegionKommun07")
hamtaAllaLan       <- function() pxweb2r::pxweb2_get_codelist("vs_RegionLän07")
hamtaregtab <- function() dplyr::bind_rows(
  pxweb2r::pxweb2_get_codelist("vs_RegionRiket99"),
  pxweb2r::pxweb2_get_codelist("vs_RegionLän07"),
  pxweb2r::pxweb2_get_codelist("vs_RegionKommun07")
) |> dplyr::transmute(regionkod = code, region = label)
```

Övriga funktioner är ren sträng-filtrering på den df:en och ändras inte.
Cacha med `memoise` inom sessionen. DeSO/RegSO har egna valuesets på de
tabeller som har dem (via `pxweb2_list_codelists()`).

### C. Andra datakällor → **`rdverktyg`, granskas var för sig**

| Funktion(er) | Beslut (preliminärt) |
|---|---|
| `hamta_kolada_giltiga_ar`, `hamta_kolada_df` | behåll – Kolada är en distinkt, använd API |
| `skolverket_generera_kolumnnamn`, `skolverket_hitta_startrad` | granska – kanske överflödiga |
| `hamta_fk_json_dataset_med_url` + `json_extrahera_*` (5 st) | granska – generalisera ev. till en json-stat-hjälpare, eller stryk om `rjstat` räcker |
| `hamta_excel_dataset_med_url` | behåll – generisk, använd |
| `oppnadata_hamta` | behåll – generisk öppna-data-hämtning |

### Supercross-funktioner (ur `func_GIS.R` + `func_API.R`)

Verksamheten lämnar Supercross.

| Funktion | Fil | Beslut |
|---|---|---|
| `geopackage_skapa_fran_rutor_csv_xlsx_supercross` + `skapa_sf_fran_csv_eller_excel_supercross` | GIS | **slå ihop** till `sf_fran_rutid_fil()` i `rdgis` – rutid→cellpolygon-logiken är generell, släpp Supercross-inramningen |
| `spatial_join_med_ovrkat` | GIS | **behåll** → `rdgis` (generisk spatial join, ligger bara i Supercross-sektionen) |
| `skapa_supercross_recode_fran_rutlager` | GIS | **stryk** (öppen fråga: används Mona fortfarande?) |
| `korrigera_kolnamn_supercross` | API | **stryk** |

### D. Fil- & formatinläsning → **`rdverktyg`, behåll de flesta**

| Funktion | Beslut / ersätt med |
|---|---|
| `excel_xml_las_fil`, `intern_excel_xml_lasa_pivot_cache`, `intern_excel_xml_hamta_kodmappningar` | behåll, men bygg om på `tidyxl` + `unpivotr` för pivot-Excel |
| `konvertera_dataset_filformat` | behåll som tunn `rio`-wrapper |
| `excelfil_spara_formaterad` | behåll (`openxlsx`/`openxlsx2`) |
| `spara_som_csv_i_zip`, `csv_fran_zipfiler_inlasning` | behåll; överväg `archive` för att läsa direkt ur zip |
| `separator_gissa` | ev. stryk – `readr::read_delim()` / `data.table::fread()` autodetekterar |
| `las_b64` | behåll (liten) |
| `korrigera_kolnamn_supercross` | behåll teckenkodningsfixen; använd `janitor::clean_names()` för namnstädning |

### E. Allmänna R-verktyg → **`rdverktyg`, döp om konsekvent, ersätt där paket finns**

| Funktion | Rekommendation |
|---|---|
| `funktion_upprepa_forsok_tills_retur_TRUE`, `funktion_upprepa_forsok_om_fel`, `skriptrader_upprepa_om_fel` | **ersätt med `purrr::insistently()` / `purrr::possibly()`** + `purrr::rate_backoff()`; för HTTP: `httr2::req_retry()` |
| `skapa_intervaller`, (och `skapa_aldersgrupper`) | **`santoku::chop()`** för namngiven bin-indelning; behåll ev. tunn wrapper för SCB-åldersgrupper |
| `avrundning_dynamisk` | `scales::number()` / `signif()`; behåll om beteendet är svårt att återskapa |
| `sokvag_for_skript_hitta` | **`this.path::this.path()`** (robust) |
| `urklipp` | **`clipr::write_clip()`** (samma som `pxweb2r` redan använder) |
| `slash_lagg_till` | **`fs`** för all sökvägshantering (`fs::path()`), behåll ev. en-radaren |
| `stop_tyst`, `suppress_specific_warning` | `rlang::abort()` / `cli::cli_abort()` för fel; `rlang::cnd_muffle()` / `purrr::quietly()` för varningar |
| `ladda_funk_parametrar`, `lista_funktioner_i_skript`, `hitta_funktioner_i_fil_ej_inuti_andra_funktioner` | behåll (parsning via `utils::getParseData()`), döp om konsekvent |
| `period_jmfr_filter` | behåll – domänspecifik |

Namnkonvention: bestäm ett mönster (t.ex. `verb_objekt`, snake_case, svenska)
och döp om alla `rdverktyg`-funktioner efter det i samma veva.

### F. Kod-/webbhämtning → **`rdverktyg`**

| Funktion | Rekommendation |
|---|---|
| `filhamtning_med_url_och_sokord`, `webbsida_af_extrahera_url_med_sokord` | bygg om på **`rvest`** (HTML) + **`httr2`** (hämtning) |
| `url_finns_webbsida` | `httr2::request() |> httr2::req_perform()` med `req_error()` |
| `source_utan_cache`, `source_funktioner` | behåll minimal version för övergången; på sikt onödig när allt är paket |

### G. Personliga sökvägar → **`rdverktyg`, konfigurerbara + varna, aldrig hårt fel**

`utskriftsmapp`, `mapp_hamtadata_peter`, `mapp_temp_peter`, `mapp_leveranser`,
`mapp_inlasdata`

Lösning:

```r
mapp_leveranser <- function() {
  getOption("rdverktyg.mapp_leveranser",
            default = "G:/Samhallsanalys/Leveranser/")
}
```

- Värden sätts i `.Rprofile` / `.Renviron` per dator, eller via en
  `rdverktyg_config_set()` som lagrar i en lokal fil (paketets `tools::R_user_dir()`).
  `keyring` är overkill för sökvägar (det är för hemligheter) – spar `keyring`
  till PAT och DB-lösenord.
- Döp om utan `_peter`-suffix: `mapp_hamtadata`, `mapp_temp`.
- Funktioner som **använder** en sådan mapp: kontrollera att den finns, annars
  `cli::cli_warn()` (inte `stop()`), och fortsätt/returnera `NULL`.
- Lägg till `rdverktyg_mappar_status()` som listar konfigurerade sökvägar och
  om de finns.

### rddiagram-kandidater ur `func_API.R`

`skalcirklar_skapa`, `varden_jamnt_spridda_valj_ut`, `kontrastfarg_hitta`
→ `rddiagram`. `skapa_intervaller` – se grupp E.
**Stryk:** `demo_diagrambild_skapa()` – behövs inte längre.

### rddeploy – allt som rör github/git/deploy

`github_*` + `_analytikernatverket`-tvillingarna (**behålls som separata
funktioner**), `gh_dia`, `gh_ppt`, `gh_hamta_analytikernatverket`,
`ppt_lista_rader`, `.gh_pat`, `.gh_push`, `anv_*` (4 st),
`git_kontrollera_id_uppgifter`, `depot_hamta_fran`, `depot_hamta_mapp_fran`,
`depot_skriv_mall_fran`, `copilot_konvertera`,
`webbsida_med_portal_skapa_med_github_repo`, `dokumentation_sida_skapa`,
`skapa_webbrapport_github`, `webbrapport_publicera`, `webbrapport_avpublicera`,
alla `shinyapp_*` + `.shinyapp_*` (17 st), alla `landningssida_*` +
`.landningssida_*` (12 st) – **konsolideras med `func_landningssida_adminportal.R`**.

### rdshinyappar

**Endast** det som ligger i `func_shinyappar.R` idag. Inga deploy-funktioner
hit – de går till `rddeploy`.

---

## 3b. `pxweb2r`-tillägg innan `rdverktyg`

SCB API v2 har `GET /codelists/{id}` men **ingen global listning** – codelists
upptäcks per tabell via metadata.

| Ny funktion | Vad |
|---|---|
| `pxweb2_get_codelist(id)` | `GET /codelists/{id}` → `tibble(code, label, valueMap)`. Fristående, id är globalt (`"vs_RegionKommun07"`, `"agg_RegionLA2018"`, …) |
| `pxweb2_list_codelists(table)` | parsar metadata → `tibble(variable, id, type, label)`, `type` = `"Valueset"` / `"Aggregation"`. Återanvänder intern `intern_cl_info`-logik |

## 4. `rddeploy` – refaktoreringsdesign

De tre monoliterna (`shinyapp_skapa_med_github_repo` ~730 rader,
`shinyapp_skapa_med_github_repo_forka_befintligt` ~510,
`skapa_webbrapport_github` ~400) byggs om enligt:

1. **Mallar som filer.** `deploy.yml`, `avpublicera.yml`, `global.R`-skal,
   `ui.R`-skal, `server.R`-skal, `.gitignore`, `README`, `_dependencies.R`,
   `_publicering_till_server.yml` → `inst/templates/` med `{{platshållare}}`
   (`glue` eller `whisker`). Tar bort ~400 rader inbäddad text; mallarna blir
   diff- och testbara. `depot`-mönstret gör redan detta för stilfiler.
2. **Delad kärna create/fork.** `.shinyapp_scaffold_common()` för struktur,
   workflows, renv och git. De publika funktionerna skiljer sig bara i var
   appfilerna kommer ifrån (genereras / kopieras från forkat repo).
3. **Små stegfunktioner:** `.scaffold_struktur()`, `.scaffold_global_r()`,
   `.scaffold_workflows()`, `.scaffold_renv()`, `.init_git_och_github()` – var
   och en testbar mot `tempdir()`.
4. **Skilj disk från GitHub.** "Skriv filer" och "prata med GitHub" separeras så
   torrkörning blir möjlig och GitHub-anropen blir en mockbar enhet.
   **Pre-flight** (viktigt): funktionen
   - skriver ut exakt vilka mappar/filer/repo som skapas och var,
   - kräver att föräldermappen (`githubmapp_lokalt`) finns – annars `stop()`
     med tydligt meddelande,
   - kräver bekräftelse innan något skrivs: interaktivt `utils::askYesNo()` /
     `cli`-prompt, eller `force = TRUE` i skript.
5. **Config-objekt:** `shinyapp_config(...)` → validerad lista som skickas runt,
   i stället för ~12 argument.
6. **GitHub-auth-stack, se avsnitt 5.**
7. **renv-bootstrap** isoleras som `.shinyapp_init_renv()` med tydliga in-/
   utvärden (den fiddliga "tvinga https-CRAN" + "bara runtime-deps"-logiken
   samlas på ett ställe i stället för mitt i den stora funktionen).

`skapa_webbrapport_github` får samma behandling och delar git/GitHub-kärnan.

---

## 5. GitHub-autentisering – rekommenderad stack

Idag: lapptäcke av `.gh_pat`, `git_kontrollera_id_uppgifter`, handrullad
`system("git ...")`, egna `gh_*`-wrappers.

Standardisera på:

| Lager | Paket | Roll |
|---|---|---|
| Token-lagring | **`gitcreds`** | PAT i OS:ets credential store; `gitcreds::gitcreds_get()/_set()` |
| GitHub REST API | **`gh`** | alla API-anrop; `gh::gh_token()` hittar token via `gitcreds`/`GITHUB_PAT` |
| Git-operationer | **`gert`** | init/add/commit/push/pull via libgit2 (ingen `system()`), auth via `credentials`-paketet |
| Orkestrering | **`usethis`** | högnivåflöden: `use_github()`, `create_from_github()`, diagnostik `git_sitrep()` |

Konkret:
- Ersätt alla `system("git ...")` med `gert::git_*`.
- Ersätt egna curl-/httr-anrop mot GitHub med `gh::gh()`.
- En `rddeploy_auth_check()` som wrappar `usethis::git_sitrep()`-stil diagnostik
  (token finns? scope? git-identitet satt? SSH/HTTPS?).
- PAT skapas med `usethis::create_github_token()` och lagras med
  `gitcreds::gitcreds_set()` – dokumentera detta som engångs-setup per dator.

---

## 6. Externa paket att luta sig mot (sammanställning)

| Behov | Paket |
|---|---|
| Retry / felhantering | `purrr` (`insistently`, `possibly`, `safely`), `httr2::req_retry` |
| HTTP | `httr2` (nytt), `httr` (befintligt) |
| HTML-skrapning | `rvest` |
| Bin/intervall-indelning | `santoku` |
| Sökvägar / filsystem | `fs` |
| Hitta körande skript | `this.path` |
| Urklipp | `clipr` |
| Fel/varningar/meddelanden | `cli`, `rlang` |
| Excel (stökig/pivot) | `tidyxl`, `unpivotr`; annars `readxl`/`openxlsx2` |
| Läsa ur arkiv | `archive` |
| Kolumnnamn-städning | `janitor` |
| Cachning inom session | `memoise` |
| GitHub/git | `gh`, `gert`, `gitcreds`, `usethis` |
| Config/hemligheter | `.Renviron`/`options()` för sökvägar; `keyring` för lösenord/PAT |

---

## 7. Extraktionsordning

1. ✅ **Skapa monorepot** `Region-Dalarna/rdpaket`.
2. ✅ **`rdverktyg`** – `func_text.R` + `func_filer.R` + region-/kodgrupp B
   (`pxweb2r`-wrappers) + grupp D–G, dubbletter rensade.
3. ✅ **`rddiagram`** – `SkapaDiagram` + `diagramfunktioner` + `logga` +
   `bubbeldiagram` + färg-/skalhjälparna ur `func_API.R`.
4. ✅ **`rdpostgres`** – postgres-avsnittet ur `func_GIS.R` (~42 funktioner)
   + `get_password_tk`/`keyring_lagg_till_inloggning`. `con="default"`-mönstret
   → `intern_con()`; `meddelande_tid` borttaget; dubbeldefinierad
   `postgres_alla_rattigheter` deduplicerad.
5. ✅ **`rdgis`** – geometri-avsnitten ur `func_GIS.R` (~20 funktioner).
   Ny `sf_fran_rutid_fil()` slår ihop Supercross-inläsarna. `sf` i Suggests
   med vakt. Ej portad: `skapa_supercross_recode_fran_rutlager` (öppen
   Mona-fråga).
6. ✅ **`rdshinyappar`** – `func_shinyappar.R` rakt in.
7. ✅ **`rddeploy`** – github/git-kärnan (gert/gh/gitcreds), webbrapport/portal,
   shinyapp-scaffolding med mallar i `inst/templates/`. Monoliterna uppdelade
   i `intern_scaffold_*`-steg + `shinyapp_config()`.
   ✅ **`rdadminportal`** – landningssida/ikoner/nedladdning/cron (se avsnitt Läge).
8. ⬜ **`rdgeorouting`** – `postgis_*`/`pgrouting_*`/`pendling_*` (rad 3653–7864
   i `func_GIS.R`, ~35 funktioner). Beror på `rdgis` + `rdpostgres`. Kända
   buggar att fixa: `postgis_flytta_tabell` använder odefinierad `con_flytt`;
   `<<-` i `postgis_kopiera_tabell_mellan_databaser`.
9. ✅ **`rd`** – paraplypaketet (analyspaketen; `rdgeorouting` läggs till sen).
10. ⬜ **Migrera nedströms:** `source(".../func_X.R")` → `library(rdX)` i
    övriga funktionsfiler och konsumerande skript, shims kvar under övergången.
11. Senare: `rdgtfs`, `rdotp`, `rdlupp`, `rdqgispendling`, `rdsvg`.

---

## 8. Öppna frågor

- Namnkonvention för `rdverktyg`: svenska, `snake_case`, verb-först vid handling
  (`hamta_kommuner`, `las_excel_fil`), inga förkortningar. Fastställs i steg 4.
- Används Mona fortfarande? → avgör om `skapa_supercross_recode_fran_rutlager`
  stryks helt.
- `func_qgis_pendling.R` – kräver QGIS installerat; paket eller skript?
- FaluPeppe-kontots push-rätt till `Region-Dalarna`-orgen (bekräftas vid första
  push).

---

## 9. Nästa konkreta steg

1. Bygg **`rdgeorouting`** (avsnitt 7 punkt 8). Samma upplägg som `rdpostgres`:
   bevara SQL verbatim, `con="default"` → `intern_con()`, kvalificera
   namespaces, fixa de kända buggarna. `postgis_isokroner_skapa()` (~280 rader
   SQL) och pgRouting-grafbyggena kan inte verifieras utan en riktig
   pgRouting-databas – porta försiktigt och flagga.
2. Lägg `rdgeorouting` i `Depends` för `rd`.
3. Migrera `source()` → `library()` nedströms, med shims.
4. Besluta om `func_pxweb2.R` i `funktioner` ska bli en tunn shim mot `pxweb2r`
   när nedströms-skripten är migrerade.
