

# ------------------- funktion skapa kraftfält -------------------------


# funktionen är skapad av Henrik Aldén från SWECOS skript G:/skript/gis/sweco_dec_2022/orginalskript/ del1_kraftfält_QGIS_plugin(_uppdaterad).r

# Först  kör funktionen skapa_vagnatverk_tatort() för att skapa temporära tabeller i postgis
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

pendling_kraftfalt <- function(
    datafile = NA,
    output_folder = NA, 
    gpkg_name = NA,
    enskilt_troskelvarde = 20,
    totalt_troskelvarde = 35,
    primary_la_zone_buffer_length = 2000,
    secondary_la_zone_buffer_length = 1000,
    common_la_zone_buffer_length = 5000,
    con = NA,
    dist = 2000, # max avstånd till vägnätet
    write_to_gpkg = FALSE # Flag to control whether to write to GPKG or return as list
) { 
  library(readxl)
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  library(purrr)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # lägg nedan i ett schema och hämta därifrån
  
  
  
  # Hantera NA i parametrar
  # indata med pendlingsrelationer hämtas förslagsvis på MONA
  if (is.na(datafile)) {
    datafile <- "G:/Samhällsanalys/GIS/grundkartor/mona/pendlingsrelationer_tatort_nattbef_filtrerad.csv" # lägg i geodatabasen
  }
  # där du eventuellt sparar data lokalt
  if (is.na(output_folder)) {
    output_folder <- "G:/skript/gis/sweco_dec_2022/utdata"
  }
  # det namn du ger din geopackage
  if (is.na(gpkg_name)) {
    gpkg_name <- "kraftfält.gpkg"
  }
  # uppkoppling till din Postgres databas
  if (is.na(con)) {
    con <- uppkoppling_db(service = "rd_geodata")
  }
  
  dbExecute(con, "SET search_path TO grafer, public;") # denna är lite mystisk, men det funkar...
  
  # vägnätet skapat med skapat med skapa_vagnatverk_tatort() ligger i schemat grafer
  edges_table <- "nvdb_noded" 
  vertices_table <- "grafer.nvdb_noded_vertices_pgr" 
  
  cost_col = "dist_cost" # kolumn i nvdb_noded med kostnad
  reverse_cost_col = "dist_reverse_cost" # kolumn i nvdb_noded med omvänd kostnad
  
  # simpel funktion för att skriva postgis tabell till gpkg om write_to_gpkg=TRUE
  write_pgtable2gpkg <- function(lyrname, output_folder, gpkg_name, append=FALSE, delete_dsn=FALSE) {
    lyr <- st_read(con, lyrname)
    st_write(lyr, 
             file.path(output_folder, gpkg_name),
             lyrname, 
             append=append,
             delete_dsn=delete_dsn
    )  
  }  
  
  # läs in pendlingsdata
  pendlingsdata <- read_csv(datafile, locale = locale(encoding = "ISO-8859-1"))
  
  data <- pendlingsdata %>%
    mutate(
      from_id = substr(from_id, 3, 11), # tar bort länskoden och tätortsnamnet från t.ex. 202084TC101 Avesta
      to_id = substr(to_id, 3, 11)      # tar bort länskoden och tätortsnamnet från t.ex. 202080TC108 Falun
    ) %>% 
    select(from_id, to_id, n)
  
  # dbWriteTable(con, 'grafer.data', data, overwrite=TRUE, temporary=FALSE) # denna skriv till grafer.grafer.data !!
  dbWriteTable(con, 'data', data, overwrite=TRUE, temporary=FALSE) # skriver till grafer.data således schema grafer
  
  
  # hitta närmaste vertex i vägnätet till varje tätort
  query <- str_glue("CREATE TEMP TABLE tatort_vertex AS
                      SELECT t.tokod, e.id, e.dist
                      FROM tatort t
                      JOIN lateral(
                        SELECT id, e.geom <-> t.geom as dist
                          FROM {vertices_table} e
                        ORDER BY t.geom <-> e.geom
                        LIMIT 1
                      ) AS e
                      ON true
                      WHERE dist < {dist};")
  dbExecute(con, "DROP TABLE IF EXISTS tatort_vertex;")
  dbExecute(con, query)
  
  query <- str_glue("CREATE TABLE commute_combinations AS
                      with temp_commute_data as (
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
                      FROM temp_commute_data
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
  # write_pgtable2gpkg(str_glue("la"), output_folder, gpkg_name)
  
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
  # write_pgtable2gpkg(str_glue("solitary"), output_folder, gpkg_name)
  
  
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
  # write_pgtable2gpkg(str_glue("common_la"), output_folder, gpkg_name)
  
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
  # write_pgtable2gpkg(str_glue("satellites"), output_folder, gpkg_name)
  
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
                              {cost_col} AS cost,
                              {reverse_cost_col} AS reverse_cost,
                              x1, y1, x2, y2
                  	   FROM grafer.{edges_table}',
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
                    FROM astar a JOIN grafer.{edges_table} r ON a.edge=r.id
                    GROUP BY a.start_vid, a.end_vid
                  )
                  SELECT c.*, p.agg_cost, p.geom
                  FROM route_combinations c 
                  JOIN paths p ON c.start_vid=p.start_vid 
                    and c.end_vid=p.end_vid")
  dbExecute(con, "DROP TABLE IF EXISTS routes;")
  dbExecute(con, query)
  # write_pgtable2gpkg(str_glue("routes"), output_folder, gpkg_name)
  
  
  # rutter till secondary LA
  query <- str_glue("CREATE TEMP TABLE secla_areas AS
          select
            to_id, MAX(ranking) AS max_ranking,
            st_buffer(st_collect(r.geom), {secondary_la_zone_buffer_length}) as geom
          from satellites s
          join routes r on s.id=r.from_id and s.secondary_la=r.to_id
          group by to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS secla_areas;")
  dbExecute(con, query)
  # write_pgtable2gpkg(str_glue("secla_areas"), output_folder, gpkg_name)
  
  # rutter till primary LA
  query <- str_glue("CREATE TEMP TABLE primla_areas AS
          select
            to_id, max(ranking),
            st_buffer(st_collect(r.geom), {primary_la_zone_buffer_length}) as geom
          from satellites s
          join routes r on s.id=r.from_id and s.primary_la=r.to_id
          group by to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS primla_areas;")
  dbExecute(con, query)
  # write_pgtable2gpkg(str_glue("primla_areas"), output_folder, gpkg_name)
  
  # rutter mellan CommonLA
  query <- str_glue("CREATE TEMP TABLE commonla_areas AS
          select
            from_id, to_id, ranking,
            st_buffer(r.geom, {common_la_zone_buffer_length}) as geom
          from common_la s
          join routes r on s.id=r.from_id and s.common_la=r.to_id
          ;")
  dbExecute(con, "DROP TABLE IF EXISTS commonla_areas;")
  dbExecute(con, query)
  # write_pgtable2gpkg(str_glue("commonla_areas"), output_folder, gpkg_name)
  
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
  if (write_to_gpkg) {
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
              write_pgtable2gpkg(
                lyrname = str_remove(.x, "grafer."), # Remove schema prefix for GPKG
                output_folder = output_folder,
                gpkg_name = gpkg_name,
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
    message(glue("Finished writing non-empty layers to {file.path(output_folder, gpkg_name)}"))
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

# # Exempel på hur funktionen kan användas
# 
# kraftfalt_20_40 <- pendling_kraftfalt(enskilt_troskelvarde = 20, totalt_troskelvarde = 40)
# 
# # Define custom colors for better readability
# road_color <- "#4D4D4D" # Dark gray for roads
# area_color <- "green" # Green for areas
# point_color_primary <- "red" # Red for primary points
# point_color_secondary <- "orange" # Orange for secondary points
# 
# # Update mapview layers
# mapview::mapview(kraftfalt_20_40$la, col.regions = area_color, alpha.regions = 0.5, cex = 6) +
#   mapview::mapview(kraftfalt_20_40$solitary, col.regions = point_color_secondary, cex = 4) +
#   mapview::mapview(kraftfalt_20_40$routes, color = road_color, lwd = 2)  +
#   mapview::mapview(kraftfalt_20_40$satellites, col.regions = point_color_primary, cex = 3)+
#   mapview::mapview(kraftfalt_20_40$primla_areas, col.regions = "blue", alpha.regions = 0.3)+
#   mapview::mapview(kraftfalt_20_40$secla_areas, col.regions = "orange", alpha.regions = 0.4)

# ------------------- funktion som skapar pendlingsnätverk -------------------------

pendling_natverk <- function(
    datafile = NA,
    con = NA,
    dist = 2000, # max avstånd till vägnätet
    write_to_gpkg = FALSE, # Flag to control output
    output_folder = NA, # Output folder for geopackage
    gpkg_name = NA # Name of the geopackage
) { 
  
  library(readxl)
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # Hantera NA i parametrar
  # indata med pendlingsrelationer hämtas förslagsvis på MONA
  if (is.na(datafile)) {
    datafile <- "G:/Samhällsanalys/GIS/grundkartor/mona/pendlingsrelationer_tatort_nattbef_filtrerad.csv" # lägg i geodatabasen
  }
  # där du eventuellt sparar data lokalt
  if (is.na(output_folder)) {
    output_folder <- "G:/skript/gis/sweco_dec_2022/utdata"
  }
  # det namn du ger din geopackage
  if (is.na(gpkg_name)) {
    gpkg_name <- "pendling_natverk.gpkg"
  }
  # uppkoppling till din Postgres databas
  if (is.na(con)) {
    con <- uppkoppling_db(service = "rd_geodata")
  }
  
  dbExecute(con, "SET search_path TO grafer, public;") # denna är lite mystisk, men det funkar...
  
  # vägnätet skapat med skapa_vagnatverk_tatort() ligger i schema grafer
  edges_table <- "nvdb_noded" 
  vertices_table <- "grafer.nvdb_noded_vertices_pgr" 
  
  cost_col = "dist_cost" # kolumn i nvdb_noded med kostnad
  reverse_cost_col = "dist_reverse_cost" # kolumn i nvdb_noded med omvänd kostnad
  
  pendlingsdata <- read_csv(datafile, locale = locale(encoding = "ISO-8859-1"))
  
  data <- pendlingsdata %>%
    mutate(
      from_id = substr(from_id, 3, 11),
      to_id = substr(to_id, 3, 11)
    ) %>% 
    select(from_id, to_id, n)
  
  # skriv till temporär tabell
  dbWriteTable(con, 'data', data, overwrite = TRUE, temporary = FALSE)
  
  
  # hitta närmaste vertex i vägnätet till varje tätort
  query <- str_glue("CREATE TEMP TABLE tatort_vertex AS
                      SELECT t.tokod, e.id, e.dist
                      FROM tatort t
                      JOIN lateral(
                        SELECT id, e.geom <-> t.geom as dist
                          FROM {vertices_table} e
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
                                {cost_col} AS cost, 
                                {reverse_cost_col} AS reverse_cost,
                                x1, y1, x2, y2
                    	  FROM {edges_table}',
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
                      JOIN {edges_table} r ON e.edge_id = r.id;")
  
  network <- st_read(con, query = query)
  
  # Check if output is to a geopackage or as R object
  if (write_to_gpkg) {
    # Write to geopackage
    output_path <- file.path(output_folder, gpkg_name)
    st_write(network, output_path, delete_dsn = TRUE)
    message(glue("Results written to {output_path}"))
  } else {
    # Return the network as an R object
    return(network)
  }
}

# network <- pendling_natverk(write_to_gpkg = FALSE)
# # 
# # Load necessary library
# library(mapview)
# 
# # Define a custom color palette
# custom_colors <- colorRampPalette(c("lightblue", "green", "yellow", "orange", "red"))
# 
# # Apply the mapview with custom colors
# mapview::mapview(
#   network,
#   zcol = "antal_pend",           # Column to control the color gradient
#   lwd = "antal_pend",            # Column to control line width
#   alpha = 0.5,                   # Transparency
#   color = custom_colors    # Apply the custom color palette
# )



# ------------------- funktion in och utpendling på ruta -------------------------

# funktionen är skapad av Henrik Aldén från SWECOS skript G:/skript/gis/sweco_dec_2022/orginalskript/ del3_rut_pendling.r

# två versioner av skriptet finns som argument. Ev är postgis versionen snabbare vid stora dataset?

# indata parameter ska läggas till

# testa med system.time() för att jämföra exekveringstid på r och pg

pendling_ruta <- function(version = c("PostGIS", "R"), # Default to "R"
                          con = NA,
                          tab,
                          grid,
                          input_data = NA,
                          output_folder = NA,
                          grid_epsg = 3006,
                          write_to_gpkg = FALSE,
                          gpkg_name = NA) {
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # Validate version
  version <- match.arg(version, choices = c("R", "PostGIS"))
  print(glue::glue("Version selected: {version}"))
  
  # Handle default file paths and connection
  if (is.na(input_data)) {
    input_data <- "G:/skript/gis/sweco_dec_2022/data/del3"
  }
  if (is.na(output_folder)) {
    output_folder <- "G:/skript/gis/sweco_dec_2022/utdata"
  }
  if (is.na(gpkg_name)) {
    gpkg_name <- "pendling_natverk.gpkg"
  }
  if (is.na(con)) {
    con <- uppkoppling_db(service = "rd_geodata")
  }
  
  # Validate version
  version <- match.arg(version)
  
  # File paths
  tab <- "RutPendtab.TAB"
  grid <- "RutPendmap.TAB"
  tabfile <- file.path(input_data, tab)
  gridfile <- file.path(input_data, grid)
  
  # Read files
  data_tab <- st_read(tabfile) %>%
    rename(boruta = boruta, arbruta = arbruta, antalpend = antalpend)
  
  grid <- st_read(gridfile) %>%
    rename(rut_id = Rut_Id)
  
  st_crs(grid) <- grid_epsg
  st_geometry(grid) <- "geom"
  
  # Select polygon
  pol <- data.frame(id = 1) # hur välja polygon?
  pol$geom <- ("POLYGON((1485116 6479039,
                        1487385 6472727,
                        1493118 6473596,
                        1494589 6478925,
                        1492357 6480547,
                        1485116 6479039))")
  selected_polygon <- st_as_sf(pol, wkt = "geom")
  st_crs(selected_polygon) <- grid_epsg
  
  if (version == "PostGIS") {
    
    dbWriteTable(con, "ruta", grid, overwrite = TRUE, temporary = TRUE)
    dbWriteTable(con, "rutpendling", data_tab, overwrite = TRUE, temporary = TRUE)
    
    selected <- st_read(con, layer = "ruta") %>% st_filter(selected_polygon)
    selected_ids <- paste(selected$rut_id, collapse = ", ")
    
    # In-commuting
    query <- str_glue("WITH commuters_in AS (
                      SELECT
                        boruta, 
                        sum(antalpend) AS total
                      FROM rutpendling
                      WHERE arbruta::BIGINT IN ({selected_ids}) 
                        AND boruta::BIGINT NOT IN ({selected_ids}) 
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
                      WHERE boruta::BIGINT IN ({selected_ids})
                        AND arbruta::BIGINT NOT IN ({selected_ids}) 
                      GROUP BY arbruta
                      )
                      SELECT c.*, r.geom 
                      FROM commuters_out c 
                        JOIN ruta r ON c.arbruta = r.rut_id")
    ut_pendling <- st_read(con, query = query)
    
    if (write_to_gpkg) {
      st_write(selected, file.path(output_folder, "rut_pendling_pg.gpkg"), "område", delete_dsn = TRUE, append = FALSE)
      st_write(in_pendling, file.path(output_folder, "rut_pendling_pg.gpkg"), "in_pend", append = FALSE)
      st_write(ut_pendling, file.path(output_folder, "rut_pendling_pg.gpkg"), "ut_pend", append = FALSE)
    }
    
    return(list("selected" = selected, "in_pendling" = in_pendling, "ut_pendling" = ut_pendling))
    
  } else if (version == "R") {
    selected <- st_filter(grid, selected_polygon)
    
    get_commuters_from_grid <- function(commute_data) {
      commute_data %>%
        group_by(boruta) %>%
        summarise(commute_from = sum(antalpend))
    }
    get_commuters_to_grid <- function(commute_data) {
      commute_data %>%
        group_by(arbruta) %>%
        summarise(commute_to = sum(antalpend))
    }
    filter_with_selected <- function(commute_data, selected) {
      from_selected <- filter(commute_data,
                              (boruta %in% selected$rut_id) &
                                !(arbruta %in% selected$rut_id))
      to_selected <- filter(commute_data,
                            (arbruta %in% selected$rut_id) &
                              !(boruta %in% selected$rut_id))
      list("from_selected" = from_selected, "to_selected" = to_selected)
    }
    
    data <- filter_with_selected(data_tab, selected)
    from_c <- get_commuters_from_grid(data$to_selected)
    to_c <- get_commuters_to_grid(data$from_selected)
    
    full_table <- full_join(from_c, to_c, by = c("boruta" = "arbruta")) %>%
      rename("rut_id" = "boruta")
    result <- merge(grid, full_table, by = "rut_id")
    
    in_pendling <- filter(result, !is.na(commute_from)) %>% select(-commute_to)
    ut_pendling <- filter(result, !is.na(commute_to)) %>% select(-commute_from)
    
    if (write_to_gpkg) {
      st_write(selected, file.path(output_folder, "rut_pendlingR.gpkg"), "område", delete_dsn = TRUE, append = FALSE)
      st_write(in_pendling, file.path(output_folder, "rut_pendlingR.gpkg"), "in_pend", append = FALSE)
      st_write(ut_pendling, file.path(output_folder, "rut_pendlingR.gpkg"), "ut_pend", append = FALSE)
    }
    
    return(list(
      "selected" = if (exists("selected") && nrow(selected) > 0) selected else NULL,
      "in_pendling" = if (exists("in_pendling") && nrow(in_pendling) > 0) in_pendling else NULL,
      "ut_pendling" = if (exists("ut_pendling") && nrow(ut_pendling) > 0) ut_pendling else NULL
    ))
    
  }
}

r <- pendling_ruta()

# # Example usage
# r <- pendling_ruta(version = "PostGIS")
# # s <- rut_pendling(version = "R")
# #
# mapview::mapview(r)
# # 
