# Translate everything to swedish
# Översätt allt till svenska, variabler, argument och kommentarer


# ------------------- Funktion skapa kraftfält -------------------------


# Skapad av Henrik Aldén från SWECOS skript G:/skript/gis/sweco_dec_2022/orginalskript/ del1_kraftfält_QGIS_plugin(_uppdaterad).r

# För att funktionen ska fungera, kör först funktionen skapa_vagnatverk_tatort() som finns i func_GIS. Denna funktion skapar nödvändiga tabeller i PostGIS.
# en uppdatering kommer att göras för att skapa dessa tabeller direkt i funktionen eller att det ska gå at läsa in sf_objekt istället för att använda PostGIS.

# source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
# skapa_vagnatverk_tatort()

# LOGIKEN FÖR ANALYSEN FÖLJER
# 
# enskilt_troskelvarde=15%
# totalt_troskelvarde=40%
# 
# LA: 
#   ingen enskild utpendlingsrelation över 15% och total utpendling under 40%
#   
#   Solitär: 
#   ingen enskild utpendlingsrelation över 15% och total utpendling över 40%
#   
#   Satellit:
#   minst en enskild utpendlingsrelation över 15% (oberoende av total utpendling)
# 
# CommonLA:
#   två orter som ej är LA enskilt (utan satelliter) men har störst pendling till varandra som är över 15%
# 
# PrimärtLA/SekundärtLA:
#   en satellit kan tillhöra två LA och dom rankas då baserat på utpendlingsandel.
# tätorten som störst andel av utpendling sker till klassas som primärtLA och näst störst blir sekundärtLA.
# - lägre nivåer (som tredje största) tas ej med.
# 

#   metod är att:
#   1. klassificera tätorter som LA/Solitär/Satelliter/CommonLA
#   2. beräkna vilka LA som satelliter tillhör samt primärt/sekundärt LA
#   3. köra ruttning mellan satelliter och LA (kör även ruttning mellan satelliter och solitärer och andra satelliter)
#   4. skapa kraftfältspolygoner genom buffer av resväg mellan satelliter för LA tätorter.
# 
# kommentera parametrar

# gå igenom och svenskifiera

# tabell_pend_relation <- "G:/Samhällsanalys/GIS/grundkartor/mona/pendlingsrelationer_tatort_nattbef_filtrerad.csv"

pendling_kraftfalt <- function(
    tabell_pend_relation, # Dataframe med pendlingsrelationer hämtas förslagsvis på MONA
    ut_mapp = NA, # data ut ur funktionen
    gpkg_namn = NA, # namn på geopackage i utmappen
    enskilt_troskelvarde = 20, # enskilt troskelvärde
    totalt_troskelvarde = 35, # totalt troskelvärde
    primar_la_buffer = 2000, # buffer för primärt LA
    sekundar_la_buffer = 1000, # buffer för sekundärt LA
    gemensam_la_buffer = 5000, # har ej sett denna i resultat! 
    con = NA, # uppkoppling till PostGIS, krävs en connection, om inte anges finns et default värde till Region Dalarnas databas
    dist = 2000, # max avstånd till vägnätet
    skriv_till_gpkg = FALSE # TRUE om resultatet ska skrivas till geopackage, annars returneras som en lista
) { 
  library(readxl)
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  library(purrr)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # fyll i som ett argument till funktionen annars används default värden
  if (is.na(ut_mapp)) {
    ut_mapp <- "G:/skript/gis/sweco_dec_2022/utdata"
  }
  # det namn du ger din geopackage annars används default värden
  if (is.na(gpkg_namn)) {
    gpkg_namn <- "kraftfält.gpkg"
  }
  # uppkoppling till din Postgres databas annars används default värden till Region Dalarnas databas med keyring()
  if (is.na(con)) {
    con <- uppkoppling_db(service = "rd_geodata")
  }
  
  dbExecute(con, "SET search_path TO grafer, public;") # denna är lite mystisk, men det funkar...
  
  # nedan 4 objekt kommer från funktionen skapa_vagnatverk_tatort() och ligger i schemat grafer
  kant_tabell <- "nvdb_noded" # en graf av vägnätet
  nod_tabell <- "grafer.nvdb_noded_vertices_pgr"# en graf av vägnätet
  
  kost_kol = "dist_cost" # kolumn i nvdb_noded med kostnad
  omvand_kost_kol = "dist_reverse_cost" # kolumn i nvdb_noded med omvänd kostnad
  
  # simpel funktion för att skriva postgis tabell till gpkg om skriv_till_gpkg=TRUE
  skriv_pg_tab_gpkg <- function(lyrname, ut_mapp, gpkg_namn, append=FALSE, delete_dsn=FALSE) {
    lyr <- st_read(con, lyrname)
    st_write(lyr, 
             file.path(ut_mapp, gpkg_namn),
             lyrname, 
             append=append,
             delete_dsn=delete_dsn
    )  
  }  
  
  # läs in pendlingsdata
  tabell_pend_relation <- read_csv(tabell_pend_relation, locale = locale(encoding = "ISO-8859-1"))%>%
    mutate(
      from_id = substr(from_id, 3, 11), # tar bort länskoden och tätortsnamnet från t.ex. 202084TC101 Avesta
      to_id = substr(to_id, 3, 11)      # tar bort länskoden och tätortsnamnet från t.ex. 202080TC108 Falun
    ) %>% 
    select(from_id, to_id, n) # väljer ut kolumner från_id, to_id och antal pendlare (n)
  
  if (!all(c("from_id", "to_id", "n") %in% colnames(tabell_pend_relation))) { # tabell_pend_relation_ruta måste innehålla dessa kolumner
    stop("`tabell_pend_relation` must contain the columns: 'from_id', 'to_id', 'n'.") # felmeddelande om inte dessa kolumner finns i tabell_pend_relation_ruta
  }
  
  # dbWriteTable(con, 'grafer.data', data, overwrite=TRUE, temporary=FALSE) # denna skriv till grafer.grafer.data !!
  dbWriteTable(con, 'data', tabell_pend_relation, overwrite=TRUE, temporary=FALSE) # skriver till grafer.data således schema grafer
  
  
  # hitta närmaste vertex i vägnätet till varje tätort
  query <- str_glue("CREATE TEMP TABLE tatort_vertex AS
                      SELECT t.tokod, e.id, e.dist
                      FROM tatort t
                      JOIN lateral(
                        SELECT id, e.geom <-> t.geom as dist
                          FROM {nod_tabell} e
                        ORDER BY t.geom <-> e.geom
                        LIMIT 1
                      ) AS e
                      ON true
                      WHERE dist < {dist};")
  dbExecute(con, "DROP TABLE IF EXISTS tatort_vertex;")
  dbExecute(con, query)
  
  query <- str_glue("CREATE TABLE commute_combinations AS
                      with temp_pendlings_data as (
                      	SELECT
                      	      from_id,
                      	      to_id,
                      	      n,
                      	      sum(n) OVER w AS totalworkers, /* totalt antal arbetare i from_id */
                      	      sum(n) FILTER (WHERE from_id=to_id) OVER w AS localworkers /* totalt antal lokala arbetare i from_id */
                        from grafer.data
                        WHERE from_id IN (SELECT tokod FROM tatort_vertex)
                        WINDOW w AS (PARTITION BY from_id)
                      )
                      SELECT
                            from_id,
                            to_id,
                            n,
                            totalworkers,
                            localworkers,
                            100*n::DECIMAL/totalworkers AS perc, /* %-andel arbetare från from_id till to_id*/
                            100-100*localworkers/totalworkers as perc_total_commuters, /* %-andel totala pendlare från from_id*/
                            case 
                            	when from_id=to_id then 0 /* relationen from_id till from_id får ranking 0 */
                            	else count(*) FILTER (WHERE from_id <> to_id) OVER wd /* ranking av relationer från mest pendlare till minst */
                            end AS ranking 
                      FROM temp_pendlings_data
                      WHERE to_id IN (SELECT tokod FROM tatort_vertex)
                      window wd AS (PARTITION BY from_id ORDER BY n DESC)
                      order by from_id, n desc;")
  dbExecute(con, "DROP TABLE IF EXISTS commute_combinations;")
  dbExecute(con, query)
  
  
  # la
  query <- str_glue("CREATE TEMP TABLE la AS
                  SELECT 
                      c.from_id AS id, c.totalworkers, 
                      c.localworkers, c.perc_total_commuters,
                      t.tobeteckn, t.lan, t.kommun, 
                      t.kommunnamn, t.geom 
                  FROM commute_combinations c
                  JOIN grafer.tatort t ON t.tokod = c.from_id
                  WHERE ranking = 1 
                    AND perc <= {enskilt_troskelvarde} 
                    AND perc_total_commuters <= {totalt_troskelvarde};")
  dbExecute(con, "DROP TABLE IF EXISTS la;")
  dbExecute(con, query)
  
  # skriv till geopackage
  # skriv_pg_tab_gpkg(str_glue("la"), ut_mapp, gpkg_namn)
  
  # solitär
  query <- str_glue("CREATE TEMP TABLE solitary AS
                  select 
                    	c.from_id as id, c.totalworkers, 
                    	c.localworkers, c.perc_total_commuters,
                    	t.tobeteckn, t.lan,t.kommun, 
                    	t.kommunnamn,t.geom 
                    from commute_combinations c
                    join grafer.tatort t on t.tokod=c.from_id
                  where ranking=1 and perc <= {enskilt_troskelvarde}
                  and perc_total_commuters > {totalt_troskelvarde}
                  ;")
  dbExecute(con, "DROP TABLE IF EXISTS solitary;")
  dbExecute(con, query)
  # skriv till geopackage
  # skriv_pg_tab_gpkg(str_glue("solitary"), ut_mapp, gpkg_namn)
  
  
  # common la
  query <- str_glue("CREATE TEMP TABLE common_la AS
                  WITH sats AS (
                     SELECT *
                     FROM commute_combinations
                     WHERE ranking > 1 AND perc > {enskilt_troskelvarde}
                  ), common_la_cte AS (
                     SELECT a.*
                     FROM sats a
                     JOIN sats b ON a.from_id = b.to_id AND a.to_id = b.from_id
                  )
                  SELECT
                     c.from_id AS id, c.to_id AS common_la, 
                     c.totalworkers, c.localworkers, c.perc_total_commuters,
                     t.tobeteckn, t.lan, t.kommun, t.kommunnamn, t.geom
                  FROM common_la_cte c
                  JOIN grafer.tatort t ON t.tokod = c.from_id;")
  
  dbExecute(con, "DROP TABLE IF EXISTS common_la;")
  dbExecute(con, query)
  # skriv till geopackage
  # skriv_pg_tab_gpkg(str_glue("common_la"), ut_mapp, gpkg_namn)
  
  # satelit
  query <- str_glue("CREATE TEMP TABLE satellites AS
                  WITH sats AS (
                      SELECT *
                      FROM commute_combinations
                      WHERE ranking IN (1, 2) 
                        AND perc > {enskilt_troskelvarde}
                        AND from_id NOT IN (SELECT id FROM common_la)
                  ), la_id AS (
                      SELECT id FROM la 
                      UNION 
                      SELECT id FROM common_la
                  ), agg AS (
                      SELECT 
                          from_id, 
                          array_agg(to_id ORDER BY ranking) AS id_array_all,
                          array_agg(to_id ORDER BY ranking) FILTER (WHERE to_id IN (SELECT id FROM la_id)) AS id_array
                      FROM sats
                      GROUP BY from_id
                  )
                  SELECT 
                      a.from_id AS id,
                      id_array[1] AS primary_la, 
                      id_array[2] AS secondary_la,
                      id_array_all[1] AS primary_destination,
                      id_array_all[2] AS secondary_destination,
                      s.totalworkers, s.localworkers, s.perc_total_commuters,
                      t.tobeteckn, t.lan, t.kommun, t.kommunnamn, t.geom
                  FROM agg a
                  JOIN sats s ON a.from_id = s.from_id
                  JOIN grafer.tatort t ON t.tokod = s.from_id;")
  dbExecute(con, "DROP TABLE IF EXISTS satellites;")
  dbExecute(con, query)
  
  # skriv till geopackage
  # skriv_pg_tab_gpkg(str_glue("satellites"), ut_mapp, gpkg_namn)
  
  # ruttningsanalys
  
  # tabell med allar rutter som ska köras (satelliter+commonLA)
  query <- str_glue("CREATE TEMP TABLE route_combinations AS
                    with sats as (
                    	select from_id, to_id, ranking
                    	from commute_combinations
                    	where ranking in (1,2) 
                    		and perc > {enskilt_troskelvarde}
                    )
                    select s.*, f.id as start_vid, t.id as end_vid
                    from sats s
                    JOIN tatort_vertex f ON f.tokod=s.from_id
                    JOIN tatort_vertex t ON t.tokod=s.to_id
                  ;")
  dbExecute(con, "DROP TABLE IF EXISTS route_combinations;")
  dbExecute(con, query)
  
  
  query <- str_glue("CREATE TEMP TABLE routes AS
                  WITH astar AS( 
                    SELECT * FROM pgr_aStar(
                      'SELECT id, source, target,
                              {kost_kol} AS cost,
                              {omvand_kost_kol} AS reverse_cost,
                              x1, y1, x2, y2
                  	   FROM grafer.{kant_tabell}',
                      'SELECT 
                        start_vid AS source, 
                        end_vid AS target
                      FROM route_combinations'
                    )
                  ), paths AS(
                    SELECT 
                      a.start_vid, 
                      a.end_vid,
                      sum(a.cost) AS cost,
                      max(a.agg_cost) AS agg_cost,
                      st_LineMerge(st_union(r.geom)) AS geom
                    FROM astar a JOIN grafer.{kant_tabell} r ON a.edge=r.id
                    GROUP BY a.start_vid, a.end_vid
                  )
                  SELECT c.*, p.agg_cost, p.geom
                  FROM route_combinations c 
                  JOIN paths p ON c.start_vid=p.start_vid 
                    and c.end_vid=p.end_vid")
  dbExecute(con, "DROP TABLE IF EXISTS routes;")
  dbExecute(con, query)
  # skriv_pg_tab_gpkg(str_glue("routes"), ut_mapp, gpkg_namn)
  
  
  # rutter till secondary LA
  query <- str_glue("CREATE TEMP TABLE secla_areas AS
          select
            to_id, MAX(ranking) AS max_ranking,
            st_buffer(st_collect(r.geom), {sekundar_la_buffer}) as geom
          from satellites s
          join routes r on s.id=r.from_id and s.secondary_la=r.to_id
          group by to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS secla_areas;")
  dbExecute(con, query)
  # skriv_pg_tab_gpkg(str_glue("secla_areas"), ut_mapp, gpkg_namn)
  
  # rutter till primary LA
  query <- str_glue("CREATE TEMP TABLE primla_areas AS
          select
            to_id, max(ranking),
            st_buffer(st_collect(r.geom), {primar_la_buffer}) as geom
          from satellites s
          join routes r on s.id=r.from_id and s.primary_la=r.to_id
          group by to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS primla_areas;")
  dbExecute(con, query)
  # skriv_pg_tab_gpkg(str_glue("primla_areas"), ut_mapp, gpkg_namn)
  
  # rutter mellan CommonLA
  query <- str_glue("CREATE TEMP TABLE commonla_areas AS
          select
            from_id, to_id, ranking,
            st_buffer(r.geom, {gemensam_la_buffer}) as geom
          from common_la s
          join routes r on s.id=r.from_id and s.common_la=r.to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS commonla_areas;")
  dbExecute(con, query)
  # skriv_pg_tab_gpkg(str_glue("commonla_areas"), ut_mapp, gpkg_namn)
  
  # Define layers to process
  layers <- c(
    "la", 
    "solitary", 
    "common_la", 
    "satellites", 
    "routes", 
    "secla_areas", 
    "primla_areas", 
    "commonla_areas"
  )
  
  # Write layers to GeoPackage or return as a list
  if (skriv_till_gpkg) {
    # Write only non-empty layers to the GeoPackage
    purrr::walk(
      layers,
      ~ {
        tryCatch({
          # Check if the layer exists in the database
          query <- str_glue("SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_name = '{str_remove(.x, 'grafer.')}')")
          layer_exists <- dbGetQuery(con, query)[1, 1]
          
          if (layer_exists) {
            lyr <- st_read(con, query = str_glue("SELECT * FROM {.x}"))
            if (nrow(lyr) > 0) { # Only write if the layer is non-empty
              skriv_pg_tab_gpkg(
                lyrname = str_remove(.x, "grafer."), # Remove schema prefix for GPKG
                ut_mapp = ut_mapp,
                gpkg_namn = gpkg_namn,
                append = TRUE # Append layers to the same GeoPackage
              )
            } else {
              message(glue("Layer `{.x}` is empty, skipping..."))
            }
          } else {
            message(glue("Layer `{.x}` does not exist, skipping..."))
          }
        }, error = function(e) {
          message(glue("Error processing layer `{.x}`: {e$message}"))
        })
      }
    )
    message(glue("Finished writing non-empty layers to {file.path(ut_mapp, gpkg_namn)}"))
  } else {
    # Create a named list with all layers, include NULL for missing or empty layers
    results <- purrr::map(
      layers,
      ~ {
        tryCatch({
          query <- str_glue("SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_name = '{str_remove(.x, 'grafer.')}')")
          layer_exists <- dbGetQuery(con, query)[1, 1]
          
          if (layer_exists) {
            lyr <- st_read(con, query = str_glue("SELECT * FROM {.x}"))
            if (nrow(lyr) > 0) {
              return(lyr) # Return non-empty layer
            } else {
              message(glue("Layer `{.x}` is empty, skipping..."))
              return(NULL)
            }
          } else {
            message(glue("Layer `{.x}` does not exist, skipping..."))
            return(NULL)
          }
        }, error = function(e) {
          message(glue("Error processing layer `{.x}`: {e$message}"))
          return(NULL)
        })
      }
    )
    # Name results list with the layer names (sans schema)
    names(results) <- str_remove(layers, "grafer.")
    return(results)
  }
}

# # # Exempel på hur funktionen kan användas
# # 
# kraftfalt_25_40 <- pendling_kraftfalt(tabell_pend_relation = tabell_pend_relation, enskilt_troskelvarde = 25, totalt_troskelvarde = 40)
# # 
# # # Define custom colors for better readability
# road_color <- "#4D4D4D" # Dark gray for roads
# area_color <- "green" # Green for areas
# point_color_primary <- "red" # Red for primary points
# point_color_secondary <- "orange" # Orange for secondary points
# # 
# # # Update mapview layers
# mapview::mapview(kraftfalt_25_40$la, col.regions = area_color, alpha.regions = 0.5, cex = 6) +
#   mapview::mapview(kraftfalt_25_40$solitary, col.regions = point_color_secondary, cex = 4) +
#   mapview::mapview(kraftfalt_25_40$routes, color = road_color, lwd = 2)  +
#   mapview::mapview(kraftfalt_25_40$satellites, col.regions = point_color_primary, cex = 3)+
#   mapview::mapview(kraftfalt_25_40$primla_areas, col.regions = "blue", alpha.regions = 0.3)+
#   mapview::mapview(kraftfalt_25_40$secla_areas, col.regions = "orange", alpha.regions = 0.4)

# ------------------- funktion som skapar pendlingsnätverk -------------------------

# Funktionen skapar ett pendlingsnätverk och beräknar det sammanlagda antalet pendlare per vägsträcka. 
# Resultatet kan sparas som ett GeoPackage eller returneras som ett R-objekt. 
# Skriptet kräver en tabell med pendlingsrelationer och vägnät hämtas från en PostGIS-databas.
# För att funktionen ska fungera, kör först funktionen skapa_vagnatverk_tatort() som finns i func_GIS. Denna funktion skapar nödvändiga tabeller i PostGIS.
# en uppdatering kommer att göras för att skapa dessa tabeller direkt i funktionen eller att det ska gå at läsa in sf_objekt istället för att använda PostGIS.

# source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
# skapa_vagnatverk_tatort()

tabell_pend_relation <- "G:/Samhällsanalys/GIS/grundkartor/mona/pendlingsrelationer_tatort_nattbef_filtrerad.csv"

pendling_natverk <- function(
    tabell_pend_relation, # Dataframe med pendlingsrelationer hämtas förslagsvis på MONA
    con = NA, # uppkopling till PostGIS, krävs en connection, om inte anges finns et default värde till Region Dalarnas databas
    dist = 2000, # max avstånd till vägnätet
    skriv_till_gpkg = FALSE, # Flag to control output
    ut_mapp = NA, # Output folder for geopackage
    gpkg_namn = NA # Name of the geopackage
) { 
  
  library(readxl)
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # Hantera NA i parametrar
  # där du eventuellt sparar data lokalt
  if (is.na(ut_mapp)) {
    ut_mapp <- "G:/skript/gis/sweco_dec_2022/utdata"
  }
  # det namn du ger din geopackage
  if (is.na(gpkg_namn)) {
    gpkg_namn <- "pendling_natverk.gpkg"
  }
  # uppkoppling till din Postgres databas
  if (is.na(con)) {
    con <- uppkoppling_db(service = "rd_geodata")
  }
  
  dbExecute(con, "SET search_path TO grafer, public;") # denna är lite mystisk, men det funkar...
  
  # vägnätet skapat med skapa_vagnatverk_tatort() ligger i schema grafer
  kant_tabell <- "nvdb_noded" 
  nod_tabell <- "grafer.nvdb_noded_vertices_pgr" 
  
  kost_kol = "dist_cost" # kolumn i nvdb_noded med kostnad
  omvand_kost_kol = "dist_reverse_cost" # kolumn i nvdb_noded med omvänd kostnad
  
  # läs in pendlingsdata
  tabell_pend_relation <- read_csv(tabell_pend_relation, locale = locale(encoding = "ISO-8859-1"))%>%
    mutate(
      from_id = substr(from_id, 3, 11), # tar bort länskoden och tätortsnamnet från t.ex. 202084TC101 Avesta
      to_id = substr(to_id, 3, 11)      # tar bort länskoden och tätortsnamnet från t.ex. 202080TC108 Falun
    ) %>% 
    select(from_id, to_id, n)
  
  if (!all(c("from_id", "to_id", "n") %in% colnames(tabell_pend_relation))) { # tabell_pend_relation_ruta måste innehålla dessa kolumner
    stop("`tabell_pend_relation` must contain the columns: 'from_id', 'to_id', 'n'.") # felmeddelande om inte dessa kolumner finns i tabell_pend_relation_ruta
  }
  
  # dbWriteTable(con, 'grafer.data', data, overwrite=TRUE, temporary=FALSE) # denna skriv till grafer.grafer.data !!
  dbWriteTable(con, 'data', tabell_pend_relation, overwrite=TRUE, temporary=FALSE) # skriver till grafer.data således schema grafer
  
  
  # hitta närmaste vertex i vägnätet till varje tätort
  query <- str_glue("CREATE TEMP TABLE tatort_vertex AS
                      SELECT t.tokod, e.id, e.dist
                      FROM tatort t
                      JOIN lateral(
                        SELECT id, e.geom <-> t.geom as dist
                          FROM {nod_tabell} e
                        ORDER BY t.geom <-> e.geom
                        LIMIT 1
                      ) AS e
                      ON true
                      WHERE dist < {dist};")
  dbExecute(con, "DROP TABLE IF EXISTS tatort_vertex;")
  dbExecute(con, query)
  
  # skapa tabell med relationer för routing
  query <- ("CREATE TEMP TABLE combinations AS
            SELECT c.*, t.id AS start_vid, f.id AS end_vid 
            FROM data c
            JOIN tatort_vertex t ON c.from_id = t.tokod
            JOIN tatort_vertex f ON c.to_id = f.tokod
            WHERE c.from_id <> c.to_id;")
  dbExecute(con, "DROP TABLE IF EXISTS combinations;")
  dbExecute(con, query)
  
  # kör routing och samla ihop resultatet
  query <- str_glue("WITH astar AS( 
                      SELECT * FROM pgr_aStar(
                        'SELECT id, source, target, 
                                {kost_kol} AS cost, 
                                {omvand_kost_kol} AS reverse_cost,
                                x1, y1, x2, y2
                    	  FROM {kant_tabell}',
                        'SELECT 
                          start_vid AS source,
                          end_vid AS target
                        FROM combinations'
                      )
                    ), edges AS(
                      SELECT edge as edge_id, sum(n) AS antal_pend
                      FROM astar a 
                      JOIN combinations d ON a.start_vid = d.start_vid
                                  AND a.end_vid = d.end_vid
                      WHERE edge <> -1
                      GROUP BY edge
                    )
                    SELECT * 
                    FROM edges e
                      JOIN {kant_tabell} r ON e.edge_id = r.id;")
  
  natverk <- st_read(con, query = query)
  
  # Check if output is to a geopackage or as R object
  if (skriv_till_gpkg) {
    # Write to geopackage
    output_path <- file.path(ut_mapp, gpkg_namn)
    st_write(natverk, output_path, delete_dsn = TRUE)
    message(glue("Results written to {output_path}"))
  } else {
    # Return the natverk as an R object
    return(natverk)
  }
}

# natverk <- pendling_natverk(tabell_pend_relation = tabell_pend_relation, skriv_till_gpkg = FALSE)
# #
# # Load necessary library
# library(mapview)
# 
# # Define a custom color palette
# custom_colors <- colorRampPalette(c("lightblue", "blue", "lightgreen", "green", "yellow", "orange", "red", "darkred"))
# 
# # Apply the mapview with custom colors
# mapview::mapview(
#   natverk,
#   zcol = "antal_pend",           # Column to control the color gradient
#   lwd = "antal_pend",            # Column to control line width
#   alpha = 0.5,                   # Transparency
#   color = custom_colors    # Apply the custom color palette
# )



# ------------------- funktion in och utpendling på ruta -------------------------

# funktionen är skapad av Henrik Aldén från SWECOS skript G:/skript/gis/sweco_dec_2022/orginalskript/del3_rut_pendling.r

# två versioner av skriptet finns som argument. Postgis versionen är snabbare vid stora dataset

# testa med system.time() för att jämföra exekveringstid på r och pg

# skriptet funkar med vector-data, alltså punkter, linjer och (således inte endast med) polygoner

# exempel på polygoner
# polygon <- hamta_karta(karttyp = "deso", regionkoder = 2085) %>%
#   group_by(kommun) %>%
#   summarise(geometry = st_union(geometry))


# # polygon <- st_read("G:/skript/gis/sweco_dec_2022/godtycklig_polygon_test.gpkg")
# 
# polygon <- st_read("G:/skript/gis/sweco_dec_2022/mora_lassarett_test.gpkg")
# 
# # ändra så tt det blir absolut tydligt vad som är inpendling och utpendling och till vad
# 
# 
# # 1 km rutor
# base_dir <- "G:/Samhällsanalys/GIS/grundkartor/mona/rutor"
# 
# # Define the file name
# file_name <- "dag_natt_bef_ruta1km.gpkg"
# 
# # Combine to create the full file path
# file_path <- file.path(base_dir, file_name)
# 
# rutor <- st_read(file_path, crs = 3006)%>%
#   dplyr::select(rut_id = xy, nattbef, dagbef, geom)%>%
#   st_cast("MULTIPOLYGON")
# 
# tabell_pend_relation_ruta <- read.csv(
#   "G:/Samhällsanalys/GIS/rutor/rutor1km_pendlingsrelationer_2022.csv",
#   header = FALSE,
#   sep = ";",
#   skip = 6,
#   colClasses = c("character", "character", "integer")
# )

# Rename the columns
colnames(tabell_pend_relation_ruta) <- c("boruta", "arbruta", "antalpend")

# ett förslag på förbättring, lägg till beefolkningen så att det går att se hur många som bor i utvalda_rutor, skapa centroider av rutor och kör cex = befolkningen (natt eller dag)

pendling_ruta <- function(version = c("PostGIS", "R"), # Måste välja mellan PostGIS och R
                          con = NA, # Om PostGIS, krävs en connection
                          tabell_pend_relation_ruta, # Dataframe med pendlingsrelationer
                          rutor, # Grid
                          polygon,  # lägg till valfritt sf_objekt med geometri; punkt, linje eller polygon
                          ut_mapp = NA, # Optional output folder
                          grid_epsg = 3006, # Optional EPSG code for the grid
                          skriv_till_gpkg = FALSE, # Optional flag to write to GeoPackage
                          gpkg_namn = NA) # Optional name for GeoPackage file
{
  
  # Load necessary libraries
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # Validate arguments
  version <- match.arg(version, choices = c("R", "PostGIS")) # verkar vara antingen eller!!
  
  
  
  # Error handling: check required inputs
  if (!inherits(rutor, "sf")) stop("`rutor` must be an sf object.") # rutor måste vara sf object
  if (!inherits(polygon, "sf")) stop("`polygon` must be an sf object.") # polygon måste vara sf object
  if (!all(c("boruta", "arbruta", "antalpend") %in% colnames(tabell_pend_relation_ruta))) { # tabell_pend_relation_ruta måste innehålla dessa kolumner
    stop("`tabell_pend_relation_ruta` must contain the columns: 'boruta', 'arbruta', 'antalpend'.") # felmeddelande om inte dessa kolumner finns i tabell_pend_relation_ruta
  }
  
  if (is.na(con)) {
    if (exists("uppkoppling_db")) {
      con <- uppkoppling_db(service = "rd_geodata")
    } else {
      stop("No connection provided and `uppkoppling_db` is not available. Please specify a valid connection.")
    }
  }
  
  if (version == "PostGIS" && (is.null(con) || !inherits(con, "PqConnection"))) {
    stop("For PostGIS version, a valid `con` database connection must be provided.")
  }
  
  if (!dbIsValid(con)) {
    stop("Database connection is not valid. Please check your connection settings.")
  }
  
  
  # Set default values for optional parameters
  if (is.na(ut_mapp)) {
    ut_mapp <- "G:/skript/gis/sweco_dec_2022/utdata"
  }
  if (is.na(gpkg_namn)) {
    gpkg_namn <- "pendling_ruta.gpkg"
  }
  
  # Ensure CRS consistency
  if (st_crs(polygon) != st_crs(rutor)) {
    polygon <- st_transform(polygon, st_crs(rutor))
  }
  
  # Intersect polygonen med rutor
  vald_polygon <- st_intersection(rutor, polygon)
  
  if (nrow(vald_polygon) == 0) stop("Ingen intersection mellan polygon och rutor.")
  
  # Branch based on the utvalda_rutor version
  if (version == "PostGIS") {
    dbWriteTable(con, "ruta", rutor, overwrite = TRUE, temporary = TRUE)
    dbWriteTable(con, "rutpendling", tabell_pend_relation_ruta, overwrite = TRUE, temporary = TRUE)
    
    utvalda_rutor <- st_read(con, layer = "ruta") %>% st_filter(vald_polygon)
    utvalda_rutor_ids <- paste(utvalda_rutor$rut_id, collapse = ", ")
    
    # In-commuting
    query <- str_glue("WITH commuters_in AS (
                      SELECT
                        boruta, 
                        sum(antalpend) AS total
                      FROM rutpendling
                      WHERE arbruta::BIGINT IN ({utvalda_rutor_ids}) 
                        AND boruta::BIGINT NOT IN ({utvalda_rutor_ids}) 
                      GROUP BY boruta
                      ) 
                        SELECT c.*, r.geom 
                        FROM commuters_in c 
                          JOIN ruta r ON c.boruta = r.rut_id")
    in_pendling <- st_read(con, query = query)
    
    # Out-commuting
    query <- str_glue("WITH commuters_out AS (
                      SELECT
                        arbruta,
                        sum(antalpend) AS total
                      FROM rutpendling
                      WHERE boruta::BIGINT IN ({utvalda_rutor_ids})
                        AND arbruta::BIGINT NOT IN ({utvalda_rutor_ids}) 
                      GROUP BY arbruta
                      )
                      SELECT c.*, r.geom 
                      FROM commuters_out c 
                        JOIN ruta r ON c.arbruta = r.rut_id")
    ut_pendling <- st_read(con, query = query)
    
    if (skriv_till_gpkg) {
      st_write(utvalda_rutor, file.path(ut_mapp, "rut_pendling_pg.gpkg"), "område", delete_dsn = TRUE, append = FALSE)
      st_write(in_pendling, file.path(ut_mapp, "rut_pendling_pg.gpkg"), "in_pend", append = FALSE)
      st_write(ut_pendling, file.path(ut_mapp, "rut_pendling_pg.gpkg"), "ut_pend", append = FALSE)
    }
    
    return(list("utvalda_rutor" = utvalda_rutor, "in_pendling" = in_pendling, "ut_pendling" = ut_pendling))
    
  } else if (version == "R") {
    utvalda_rutor <- st_filter(rutor, vald_polygon)
    
    # Define helper functions
    pendlare_fran_utvalda_rutor <- function(pendlings_data) {
      pendlings_data %>%
        group_by(boruta) %>%
        summarise(pendlare_fran = sum(antalpend))
    }
    pendlare_till_utvalda_rutor <- function(pendlings_data) {
      pendlings_data %>%
        group_by(arbruta) %>%
        summarise(pendlare_till = sum(antalpend))
    }
    filter_med_utvalda_rutor <- function(pendlings_data, utvalda_rutor) {
      fran_utvalda_rutor <- filter(pendlings_data,
                              (boruta %in% utvalda_rutor$rut_id) &
                                !(arbruta %in% utvalda_rutor$rut_id))
      till_utvalda_rutor <- filter(pendlings_data,
                            (arbruta %in% utvalda_rutor$rut_id) &
                              !(boruta %in% utvalda_rutor$rut_id))
      list("fran_utvalda_rutor" = fran_utvalda_rutor, "till_utvalda_rutor" = till_utvalda_rutor)
    }
    
    data <- filter_med_utvalda_rutor(tabell_pend_relation_ruta, utvalda_rutor)
    from_c <- pendlare_fran_utvalda_rutor(data$till_utvalda_rutor)
    to_c <- pendlare_till_utvalda_rutor(data$fran_utvalda_rutor)
    
    full_table <- full_join(from_c, to_c, by = c("boruta" = "arbruta")) %>%
      rename("rut_id" = "boruta")
    result <- merge(rutor, full_table, by = "rut_id")
    
    in_pendling <- filter(result, !is.na(pendlare_fran)) %>% dplyr::select(-pendlare_till)
    ut_pendling <- filter(result, !is.na(pendlare_till)) %>% dplyr::select(-pendlare_fran)
    
    if (skriv_till_gpkg) {
      st_write(utvalda_rutor, file.path(ut_mapp, "rut_pendlingR.gpkg"), "område", delete_dsn = TRUE, append = FALSE)
      st_write(in_pendling, file.path(ut_mapp, "rut_pendlingR.gpkg"), "in_pend", append = FALSE)
      st_write(ut_pendling, file.path(ut_mapp, "rut_pendlingR.gpkg"), "ut_pend", append = FALSE)
    }
    
    return(list(
      "utvalda_rutor" = if (exists("utvalda_rutor") && nrow(utvalda_rutor) > 0) utvalda_rutor else NULL,
      "in_pendling" = if (exists("in_pendling") && nrow(in_pendling) > 0) in_pendling else NULL,
      "ut_pendling" = if (exists("ut_pendling") && nrow(ut_pendling) > 0) ut_pendling else NULL
    ))
  }
}


# # R Version
# result <- pendling_ruta(
#   polygon = polygon,
#   version = "R",
#   tabell_pend_relation_ruta = tabell_pend_relation_ruta,
#   rutor = rutor
# )
# 
# mapview::mapview(result$utvalda_rutor)+
#   mapview::mapview(result$in_pendling, zcol = "pendlare_fran", lwd = 0)+
#   mapview::mapview(result$ut_pendling, zcol = "pendlare_till", lwd = 0)+
#   mapview::mapview(polygon)
# 
# 
# # PostGIS Version
# result_pg <- pendling_ruta(
#   polygon = polygon,
#   version = "PostGIS",  # Add database connection details
#   tabell_pend_relation_ruta = tabell_pend_relation_ruta,
#   rutor = rutor
# )
# 
# mapview::mapview(result_pg$utvalda_rutor)+
#   mapview::mapview(result_pg$in_pendling, zcol = "total", lwd = 0)+
#   mapview::mapview(result_pg$ut_pendling, zcol = "total", lwd = 0)+
#   mapview::mapview(result$utvalda_rutor)+
#   mapview::mapview(result$in_pendling, zcol = "pendlare_fran", lwd = 0)+
#   mapview::mapview(result$ut_pendling, zcol = "pendlare_till", lwd = 0)
# 

# polygon <- hamta_karta(karttyp = "tatort", regionkoder = 2084) %>%
#   filter(tatort == "Avesta")
# 
# avesta <- pendling_ruta(polygon = polygon, version = "R", rutor = rutor, tabell_pend_relation_ruta = tabell_pend_relation_ruta)
# 
# mapview::mapview(avesta$utvalda_rutor)+
#   mapview::mapview(avesta$in_pendling, zcol = "pendlare_fran", lwd = 0)+
#   mapview::mapview(avesta$ut_pendling, zcol = "pendlare_till", lwd = 0)+
#   mapview::mapview(polygon)
# 
# 
# vagfil_66<- postgis_postgistabell_till_sf(
#   schema = "nvdb",
#   tabell = "dala_med_grannlan",
#   query = "SELECT * FROM nvdb.dala_med_grannlan WHERE vagnummer_huvudnummer_1 = 66;"
# ) %>% 
#      st_zm()
# 
# # %>% 
# #   st_zm() %>% 
# #   st_buffer(500) 
# # %>% 
# #   st_union(vagnummer_huvudnummer_1 = 66)
# # 
# mapview::mapview(vagfil_66)
# polygon <- vagfil_66
# vag_66 <- pendling_ruta(polygon = polygon, version = "R", rutor = rutor, tabell_pend_relation_ruta = tabell_pend_relation_ruta)
# 
# mapview::mapview(vag_66$utvalda_rutor)+
#   mapview::mapview(vag_66$in_pendling, zcol = "pendlare_fran", lwd = 0)+
#   mapview::mapview(vag_66$ut_pendling, zcol = "pendlare_till", lwd = 0)+
#   mapview::mapview(polygon)


# tatortspunkt_falun <- hamta_karta(karttyp = "tatortspunkter", regionkoder = 2080)%>%
#   filter(tatort == "Svärdsjö")
# 
# mapview::mapview(tatortspunkt_falun)
# 
# polygon <- tatortspunkt_falun
# 
# falun <- pendling_ruta(polygon = polygon, version = "R", rutor = rutor, tabell_pend_relation_ruta = tabell_pend_relation_ruta)
# 
# mapview::mapview(falun$utvalda_rutor)+
#   mapview::mapview(falun$in_pendling, zcol = "pendlare_fran", lwd = 0)+
#   mapview::mapview(falun$ut_pendling, zcol = "pendlare_till", lwd = 0)+
#   mapview::mapview(polygon)
