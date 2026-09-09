# Kodgranskning: SkapaStapelDiagram() och SkapaLinjeDiagram()

Underlag inför porten till `rddiagram`. Radnummer avser `func_SkapaDiagram.R`.

---

## 1. Dead code – ta bort

### A. Filter-mekanismen (bekräftat oanvänt)

Parametrarna `skickad_filter_OR_vect` / `skickad_filter_OR_var` och variablerna
`filter_or` / `filter_or_var`.

- **Stapel:** rad 26–27, 122–123, 128–133; i grupperingsblocket 147–160 behålls
  bara `else`-grenen.
- **Linje:** rad 606–607, 691–698; grupperingsblocket 709–733 → behåll bara
  `else`-grenen (inkl. `na_varden_behall_i_dataset`-logiken).
- Linje bygger dessutom `filter_or` **ovillkorligt** med `paste0` (695–697) även
  när inget skickas (`paste0("'", NA, "'")` → `"'NA'"`). Fult men ofarligt –
  försvinner med borttagningen.
- Tar bort beroendet av den bräckliga `paste0(...) |> rlang::parse_expr()`.

### B. `berakna_index` i **SkapaStapelDiagram** – beräknas men används aldrig

Rad 37 (param) + 162–168. `plot_index` skapas och slängs direkt. Refererar
hårdkodat till kolumnerna `region` och `år`. **Ren dead code – ta bort helt.**

(I **SkapaLinjeDiagram** används `berakna_index` på riktigt, rad 739–745, 870,
877 – behåll den där. Men den hårdkodar `år` som x-kolumn (`total[år == min(år)]`,
`plot_df$år`) – bör använda den faktiska x-variabeln.)

### C. `skriv_till_excelfil` i **SkapaStapelDiagram** – trasig

Rad 99 + 595–598: `fullpath_excel <- str_replace(fullpath, ".png", ".xlsx")` men
`fullpath` finns inte i funktionens scope (byggs inne i `skriv_till_diagramfil`).
`skriv_till_excelfil = TRUE` ger **`object 'fullpath' not found`**. Fixa (bygg
sökväg av `output_mapp` + `filnamn_diagram`) eller ta bort parametern.

### D. `min_och_max_negativa` – beräknas, används aldrig

Stapel rad 199, Linje rad 796 och 807. Ta bort.

### E. Dubblerad stödlinjeberäkning i **SkapaLinjeDiagram**

Rad 786–802 **och** 804–813 gör exakt samma sak (`Berakna_varden_stodlinjer` +
plocka ut `min_yvar`/`max_yvar`/`min_by_yvar`/`maj_by_yvar`). Andra blocket är ren
dubblett. Plus ~30 rader bortkommenterad historik (754–782). Ta bort dubbletten
och historiken.

### F. `AF_special` – författaren: "inget att bry sig om"

Stapel rad 82 + 486–488. Linje rad 647 + 929–931 (hårdkodat `seq(1, 53)` =
veckor). **Förslag: ta bort.** Bekräfta.

### G. `utan_diagramtitel` – redundant

Stapel rad 52 + 464, Linje rad 665 + 914. Om `diagram_titel = NULL` finns ingen
titel ändå (`labs(title = NULL)`). **Ta bort parametern.**

### H. `%||%` definieras i filen (rad 1447)

Finns i base R sedan 4.5.0. Ta bort (som i `rdverktyg`).

### I. Bortkommenterad kod – behåll inget

Stapel: 36, 135, 211–212, 226, 230, 355, 382, 388, 449, 465, 485, 495, 527–531,
536. Linje: 682, 754–782, 827, 873–874, 903, 936, 946, 961.
`skriv_till_diagramfil`: 1443.

---

## 2. Buggar

| # | Var | Problem |
|---|---|---|
| 1 | Stapel 216 vs Linje 831 | `etikett_format`: Stapel har `decimal.mark = ","`, Linje saknar den (får `"."`). Bör vara `","` i båda. |
| 2 | Stapel 218, 264; Linje 833, 850 | `manual_y_axis_title == "procent"` testas utan `!is.na()`-skydd först. Med `manual_y_axis_title = NA` och andra ledet `TRUE` → `if (NA)` → **error**. Byt till `identical(manual_y_axis_title, "procent")`. |
| 3 | Stapel 263 | `if (is.na(y_titel)) y_titel <- NULL` – `y_titel` är alltid en sträng här, testet blir aldrig sant. Död rad. |
| 4 | Linje 742 | `berakna_index` hårdkodar `år` som x-kolumn. |

---

## 3. Parameterförenkling: TRUE/FALSE + separat värde → `NULL`-default

Användarens fråga. Där en boolean bara finns för att "slå på" en funktion som
ändå styrs av ett värde – slopa booleanen, låt värdet vara `NULL` som default.

| Idag | Förslag | Vinst |
|---|---|---|
| `diagram_facet = FALSE` + `facet_grp = NA` | **`facet_grp = NULL`** – satt ⇒ facet på | störst; ~15 `if (diagram_facet …)` → `if (!is.null(facet_grp) …)` |
| `lagg_pa_logga = TRUE` + `logga_path = NA` | **`logga = TRUE`** – `TRUE` = standardlogga, `FALSE` = ingen, `"sökväg"` = egen | 2 → 1 |
| `manual_color = NA` + `brew_palett = "Greens"` | **`farger = NULL`** – `NULL` = default, hex-vektor = manuell, sträng = brewer-palett | 2 → 1 |
| `x_axis_sort_value = FALSE` + `x_axis_sort_grp = NA` | **`sortera_x = NULL`** – `NULL` = ingen, `TRUE` = på total, heltal = på grupp N | 2 → 1 (+ behåll `vand_sortering`) |
| `skickad_namngiven_fargvektor = NA` + `farg_variabler = NA` | båda `NULL`-default | – |
| `legend_titel = NA`, `x_var_fokus = NA`, `fokusera_varden = NA`, `manual_x_axis_title = NA`, `manual_y_axis_title = NA` | genomgående **`NULL`** i stället för `NA` | `is.null()` är säkert; `is.na()` på vektor/lista ger varning eller fel |

Behåll som rena på/av-flaggor (inget värde att härleda från):
`y_axis_100proc`, `y_axis_borjar_pa_noll`, `procent_0_100_10intervaller`,
`diagram_liggande`, `geom_position_stack`, `dataetiketter`, `stodlinjer_minor_tabort`,
`stodlinjer_avrunda_fem`, `lagga_till_punkter` (Linje), `legend_tabort`,
`legend_vand_ordning`, `legend_byrow`, `facet_legend_bottom`.

---

## 4. Optimering / deduplicering

`SkapaStapelDiagram` och `SkapaLinjeDiagram` delar ~40 % kod. Bryt ut interna
hjälpfunktioner (delas i paketet):

| Hjälpfunktion | Ersätter |
|---|---|
| `intern_forbered_plotdata()` | grupperings-/summeringsblocket (finns i båda) |
| `intern_valj_farger()` | `chart_col`-logiken (Stapel 224–249, Linje 838–849) |
| `intern_rd_diagramtema()` | `theme(...)`-blocket (~30 rader, nästan identiskt i båda) |
| `intern_etikett_format()` | `etikett_format`-closuren (identisk) |
| `intern_stodlinjer()` | anropet till `Berakna_varden_stodlinjer` + uppackning |
| `intern_sortera_x()` | sorteringsblocket Stapel 279–327 – `diagram_liggande` TRUE/FALSE-grenarna är nästan identiska (bara `desc()`-placering skiljer) |
| `intern_stack_extremvarde()` | 6 upprepningar av `group_by \|> summarise(summ = sum(total)) \|> filter(summ == max/min(summ)) \|> slice(1) \|> pull()` (Stapel 177–198) |

Effekt: båda funktionerna krymper rejält och blir underhållbara.

### NSE-modernisering

Genomgående `as.name(skickad_x_var)` + `!!x_var` + `sym()`. Modernt: ta emot
strängar och använd `.data[[skickad_x_var]]` i `aes()` och dplyr. Tar bort behovet
av `!!`/`sym()`/`as.name()` och den farliga `paste0()`+`parse_expr()`-filtern (som
ändå ska bort).

---

## 5. Föreslagen ny signatur (skiss, SkapaStapelDiagram)

```r
SkapaStapelDiagram(
  skickad_df, skickad_x_var, skickad_y_var,
  skickad_x_grupp = NULL,
  output_mapp, filnamn_diagram,
  # titlar
  diagram_titel = NULL, diagram_undertitel = NULL, diagram_capt = NULL,
  manual_x_axis_title = NULL, manual_y_axis_title = NULL,
  # facet (facet_grp satt = på)
  facet_grp = NULL, facet_scale = "free", facet_sort = FALSE, ...,
  # färg (ett param)
  farger = NULL, skickad_namngiven_fargvektor = NULL, farg_variabler = NULL,
  # sortering (ett param)
  sortera_x = NULL, vand_sortering = FALSE,
  # y-axel
  y_axis_borjar_pa_noll = TRUE, y_axis_100proc = FALSE,
  y_axis_minus_plus_samma_axel = FALSE, procent_0_100_10intervaller = FALSE,
  stodlinjer_avrunda_fem = FALSE, stodlinjer_minor_tabort = FALSE,
  # dataetiketter (dataetiketter = på/av, resten styling)
  dataetiketter = FALSE, dataetikett_storlek = 2.3, ...,
  # x-axel
  x_axis_lutning = 45, x_axis_visa_var_xe_etikett = NULL, ...,
  # legend
  legend_titel = NULL, legend_tabort = FALSE, ...,
  # övrigt
  diagram_liggande = FALSE, geom_position_stack = FALSE,
  noll_linje_betona = "grey40", fokusera_varden = NULL,
  # logga (ett param)
  logga = TRUE, logga_storlek = 20,
  # fil
  skriv_till_diagramfil = TRUE, diagramfil_bredd = 12, diagramfil_hojd = 7,
  diagram_bildformat = "png"
)
```

Borttaget: `skickad_filter_OR_vect`, `skickad_filter_OR_var`, `berakna_index`
(Stapel), `AF_special`, `utan_diagramtitel`, `skriv_till_excelfil`,
`diagram_facet`, `lagg_pa_logga`, `logga_path`, `manual_color`, `brew_palett`,
`x_axis_sort_value`, `x_axis_sort_grp`.

Netto: ~90 parametrar → ~55, och ~580 rader → förmodligen ~350 med utbrutna
hjälpfunktioner.

---

# skapa_koropletkarta_ggplot()

Radnummer avser `func_SkapaDiagram.R`.

## Dead code / buggar

| # | Var | Problem |
|---|---|---|
| 1 | 1253 | `labs(fill = if (exists("titel_legend")) titel_legend else NULL)` – `titel_legend` **definieras aldrig**. `exists()` → alltid `FALSE` → `fill = NULL`. Legendtiteln kommer i praktiken via `name = legend_titel` i skalan. **Ta bort hela `labs()`-blocket** – det gör inget. |
| 2 | 1254–1255 | `caption = if (exists("karta_caption")) …`, `title = if (exists("karta_titel")) …` – `karta_caption` och `karta_titel` är **parametrar**, så `exists()` är alltid `TRUE`. Wrappern är meningslös – använd värdena direkt (i `labs()`). |
| 3 | 1080 & 1090 | `antal_unika <- length(unique(na.omit(sf_objekt[[vardekolumn]])))` beräknas **två gånger**. En gång. |
| 4 | 1191 | `if (exists("diagramfarger"))` – i paketet finns `diagramfarger` alltid. Ta bort testet, anropa direkt. |
| 5 | 1300, 1317 | Bortkommenterade positionsrader. |
| 6 | 1049 | `legend_box_margin <- margin(0,0,0,0)` – sätts en gång, ändras aldrig. Inline. |

## Parameterförenkling

| Idag | Förslag |
|---|---|
| `karta_farg_hogst = "mork"` / `"ljus"` / `NA` | byt `NA` → `NULL` (`NULL` = rör inte skalan) |
| `karta_bredd = "auto"` (sträng-eller-tal) | `NULL` = auto |
| `returnera_ggobj = TRUE` | kan tas bort – returnera alltid `p` (osynligt om kartan sparats) |
| `output_mapp` + `filnamn` (redan NULL-gate) | behåll |
| `logga_url` + `logga_storlek` + `logga_position` (redan NULL-gate på `logga_url`) | behåll; `logga_url = "dala"` för standardlogga funkar redan |
| `etiketter_kolumn` (NULL-gate) + styling | behåll |

## Optimering

- `image_read`/`image_info` (1294–95) → `magick::`; `ggsave` (1340) → `ggplot2::ggsave`; `str_ends` (1326) → `endsWith`.
- `glue::glue()` i meddelanden → vanlig `message(paste0(...))` (droppar `glue`-beroendet).
- Beroenden: `sf`, `ggplot2`, `ggtext`, `farver`(via diagramfarger) i Imports; `classInt`, `forcats`, `ggrepel`, `cowplot`, `magick`, `scales` i Suggests med `requireNamespace()`-vakt (klassindelning="natural" kräver classInt, etiketter kräver ggrepel, logga kräver cowplot+magick).

Netto: ~35 parametrar → ~30, städat, dead code borta.

---

# func_bubbeldiagram.R (packed circles)

**Redan i bra skick.** Någon har moderniserat den: `requireNamespace()`-vakter,
`tryCatch`, `match.arg`, paket-env-cache, engelska i identifierare, inga
`%>%`-kedjor att tala om. Inga TODO/FIXME. De 114 "kommenterade kodrader" är i
praktiken parameter-dokumentation, inte dead code.

## Att göra vid porten

| Vad | Var |
|---|---|
| `library(packcircles/ggplot2/dplyr/scales)` (rad 20–23) | ta bort – deps i DESCRIPTION |
| `dalarna_layout` – hårdkodad `data.frame` (15 kommuner + koordinater) | flytta till paketdata: `data/dalarna_layout.rda` (exporterad, dokumenterad) eller `R/sysdata.rda` |
| `.bransch_nyckel_cache` – paket-env | behåll (funkar i paket) |
| Beroenden | Imports: `ggplot2`, `dplyr`, `scales`, `packcircles`. Suggests + vakt: `curl`, `readxl` (branschnyckel), `showtext`, `sysfonts` (font), `ggforce` (`visa_ring`), `ggrepel` (`layout="repel"`) |

## Parameterförenkling (lätt)

| Idag | Förslag |
|---|---|
| `visa_antal = FALSE` + `antal_min = NULL` | `antal_min` satt ⇒ visa antal (`antal_min = NULL` + separat `visa_antal` kan slås ihop) |
| `spara_bildfil = TRUE` + `filnamn`/`mapp` | behåll (mönster som Skapa*Diagram) |
| `skal_bubblor = 0` | redan sentinel (0 = av) – ev. `NULL` för konsekvens |

`skapa_packed_circles()` är **1021 rader** med ~60 parametrar och flera
layout-algoritmer (geo-placering av Dalarnas kommuner, spiral-packning,
force-repel, grid, vinkel). Det är inneboende komplexitet, inte stök – porten
blir mest mekanisk (`library` bort, `dalarna_layout` → paketdata, qualificera
`::`), men kräver rendering mot riktig branschdata för att verifiera.

## Hjälpfunktioner (35–431) – exportera eller ej?

`aktivera_font`, `minsta_omslutande`, `bra_skalvarden`, `spiral_layout`,
`bestam_omfattning`, `hamta_bransch_nyckel` – de flesta är rena interna hjälpare
(`minsta_omslutande`, `spiral_layout`, `bestam_omfattning`,
`hamta_bransch_nyckel`). `aktivera_font` kan vara publik (användbar fristående).
Föreslår: exportera `skapa_packed_circles`, `forhandsvisa`, `aktivera_font`;
resten interna (`intern_`-prefix).
