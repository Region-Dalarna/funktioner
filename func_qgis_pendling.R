
# ------------------- funktion som skapar vägnätverk, tätorter och tabeller i postgis -------------------------

skapa_vagnatverk_tatort <- function(
    vagfil = NULL,
    tatortsfil = hamta_karta(karttyp = "tatortspunkter"),
    reparera = TRUE,
    extract_vertices = TRUE,
    con = uppkoppling_db(service = "rd_geodata")
) {
  
  
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  library(rlang)
  
  
  # Ladda externa funktioner från Region-Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # data input tätort från geodatabasen
  tatortsfil <- hamta_karta(karttyp = "tatortspunkter") # tatortsfil blir en parameter i funktionen
  
  # inställningar för tätorter
  regionkod <- 20 # regionkod blir en parameter i funktionen
  tatortskod <- "tatortskod" # tatortskod blir en parameter i funktionen? i formatet tex 2084TB023
  tatort <- "tatort" # tatortskod blir en parameter i funktionen? i formatet tex Svärdsjö
  
  # inställningar för vägar till hämtning av data från databasen
  schema_namn <- "nvdb"
  tabell_namn <- "dala_med_grannlan"
  query_namn = "SELECT * FROM nvdb.dala_med_grannlan 
           WHERE kommunlanreg_kommunkod::text LIKE '20%' 
             AND dala_med_grannlan.barighetsklass >= 1;"
  
  # hämta vägfilen från geodatabasen
  vagfil <- postgis_postgistabell_till_sf(            
    schema = schema_namn,
    tabell = tabell_namn,
    query = query_namn
  )
  
  # SKAPA TABELL ÖVER TÄTORTER 
  
  con <- uppkoppling_db(service = "test_geodata", db_name = "test_geodata") # Uppkoppling blir en parameter i funktionen
  
  dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis")
  dbExecute(con, "CREATE EXTENSION IF NOT EXISTS pgrouting")
  
  # definera tokod som användsi resterande skript, och gör alla andra kolumnnamn små 
  tatort <- tatortsfil %>%
    filter(lan == regionkod) %>%
    rename_with(tolower) %>% 
    rename(tokod = tatortskod,
           tobeteckn = tatort)
  
  #mutate(tokod = substr(tatortskod, nchar(tatortskod) - 4, nchar(tatortskod))) # mutate spara endast samma mängd som i swecos tatorter
  
  # byt namn på geometry kolumnen för använding i resterande skript
  st_geometry(tatort) <- 'geom'
  
  # Write to the database with additional checks
  tryCatch({
    dbWriteTable(con, 'tatort', tatort, overwrite = TRUE)
  }, error = function(e) {
    message("Error writing `tatort` to database: ", e)
  })
  
  # mer inställningar för vägar
  riktning_med_variabel <- "hastighetsgrans_f"
  riktning_mot_variabel <- "hastighetsgrans_b"
  hastighets_variabel <- "hastighetsgrans_f"
  
  r <- vagfil %>%
    mutate(
      riktning_med = ifelse(!is.na(!!sym(riktning_med_variabel)) & !!sym(riktning_med_variabel) > 0, TRUE, FALSE),
      riktning_mot = ifelse(!is.na(!!sym(riktning_mot_variabel)) & !!sym(riktning_mot_variabel) > 0, TRUE, FALSE),
      hastighet = !!sym(hastighets_variabel)
    ) %>%
    select(riktning_med, riktning_mot, hastighet)
  
  # skapa en id kolumn
  r$id <- seq.int(nrow(r))
  
  # Set geometry to 2D
  st_geometry(r) <- 'geom'
  r <- st_zm(r, drop = TRUE, what = "ZM")
  # st_crs(r) <- 3006  # Set the CRS for SWEREF99 TM
  
  r <- r %>%
    mutate(geom = st_line_merge(geom)) %>%
    filter(st_geometry_type(geom) == "LINESTRING")
  
  # Writing r to the database
  tryCatch({
    dbWriteTable(con, "nvdb", r, overwrite = TRUE)
  }, error = function(e) {
    message("Error writing `nvdb` to database: ", e)
  })
  # dbWriteTable(con, "nvdb2", r, temporary=FALSE, overwrite=TRUE)
  
  
  add_cost_columns <- function(edges) {
    query <- str_glue("ALTER TABLE {edges}
              ADD COLUMN dist_cost FLOAT GENERATED ALWAYS AS 
                (CASE WHEN riktning_med THEN ST_Length(geom) ELSE -1 END) STORED,
              ADD COLUMN dist_reverse_cost FLOAT GENERATED ALWAYS AS 
                (CASE WHEN riktning_mot THEN ST_Length(geom) ELSE -1 END) STORED,
              ADD COLUMN time_cost FLOAT GENERATED ALWAYS AS 
                (CASE WHEN riktning_med THEN 3.6*ST_Length(geom)/hastighet ELSE -1 END) STORED,
              ADD COLUMN time_reverse_cost FLOAT GENERATED ALWAYS AS
                (CASE WHEN riktning_mot THEN 3.6*ST_Length(geom)/hastighet ELSE -1 END) STORED")
    dbExecute(con, query)
  }
  
  add_coordinate_columns <- function(edges) {
    dbExecute(con, str_glue("ALTER TABLE {edges}
              ADD COLUMN x1 FLOAT GENERATED ALWAYS AS (ST_X(ST_StartPoint(geom))) STORED,
              ADD COLUMN y1 FLOAT GENERATED ALWAYS AS (ST_Y(ST_StartPoint(geom))) STORED,
              ADD COLUMN x2 FLOAT GENERATED ALWAYS AS (ST_X(ST_EndPoint(geom))) STORED,
              ADD COLUMN y2 FLOAT GENERATED ALWAYS AS (ST_Y(ST_EndPoint(geom))) STORED;"))
  }
  
  
  # använder pgrouting standard pgr_createTopology()
  build_topology <- function(edges) {
    # lägg till source/target kolumner
    dbExecute(con, str_glue('ALTER TABLE {edges}
                            ADD COLUMN IF NOT EXISTS source BIGINT,
                            ADD COLUMN IF NOT EXISTS target BIGINT;'))
    
    dbExecute(con, str_glue("DROP TABLE IF EXISTS {edges}_vertices_pgr;"))
    
    # bygg topologi
    dbExecute(con, str_glue("SELECT pgr_createTopology('{edges}', 0.1, 'geom', clean:=true);"))
    dbExecute(con, str_glue("ALTER TABLE {edges}_vertices_pgr RENAME COLUMN the_geom TO geom;"))
    
    add_coordinate_columns(edges)  
    add_cost_columns(edges)
  }
  
  # metod som fungerar om alla verticer är korrekta: connected med relevanta vägar 
  build_topology_extracted_vertices <- function(edges) {
    vertices_table <- str_glue("{edges}_vertices_pgr")
    # extrahera verticer
    query <- str_glue("SELECT * INTO {vertices_table}
            FROM pgr_extractVertices('SELECT id, geom FROM {edges} ORDER BY id');")
    dbExecute(con, str_glue("DROP TABLE IF EXISTS {vertices_table};"))
    dbExecute(con, query)
    
    # lägg till och fyll topologi kolumner
    dbExecute(con, str_glue("ALTER TABLE {edges}
                            ADD COLUMN IF NOT EXISTS source BIGINT,
                            ADD column IF NOT EXISTS target BIGINT,
                            ADD column IF NOT EXISTS x1 FLOAT,
                            ADD COLUMN IF NOT EXISTS y1 FLOAT,
                            ADD COLUMN IF NOT EXISTS x2 FLOAT,
                            ADD COLUMN IF NOT EXISTS y2 FLOAT;"))
    
    dbExecute(con, str_glue("UPDATE {edges}
                            SET source = NULL, target = NULL,
                            x1 = NULL, y1 = NULL,
                            x2 = NULL, y2 = NULL;"))
    
    dbExecute(con, str_glue("WITH out_going AS (
                           SELECT id AS vid, unnest(out_edges) AS eid, x, y
                           FROM {vertices_table}
                          )
                          UPDATE {edges}
                          SET source = vid, x1 = x, y1 = y
                          FROM out_going WHERE id = eid;"))
    
    dbExecute(con, str_glue("WITH in_coming AS (
                           SELECT id AS vid, unnest(in_edges) AS eid, x, y
                           FROM {vertices_table}
                          )
                          UPDATE {edges}
                          SET target = vid, x2 = x, y2 = y
                          FROM in_coming WHERE id = eid;"))
    
    dbExecute(con, str_glue("CREATE INDEX IF NOT EXISTS {edges}_id_idx ON {edges} (id);"))
    dbExecute(con, str_glue("CREATE INDEX IF NOT EXISTS {edges}_source_idx ON {edges} (source);"))
    dbExecute(con, str_glue("CREATE INDEX IF NOT EXISTS {edges}_target_idx ON {edges} (target);"))
    dbExecute(con, str_glue("CREATE INDEX IF NOT EXISTS {edges}_geom_idx ON {edges} USING GIST(geom);"))
    dbExecute(con, str_glue("CREATE INDEX IF NOT EXISTS {vertices_table}_idx ON {vertices_table} (id);"))
    dbExecute(con, str_glue("CREATE INDEX IF NOT EXISTS {vertices_table}_geom_idx ON {vertices_table} USING GIST(geom);"))
    
    add_cost_columns(edges)
  }
  
  # funktion som reparerar nätverket med pg_nodenetwork
  create_noded_network_extracted_vertices <- function(edges, extract_vertices=FALSE) {
    # reparera nätverket: lägger till noder där vägar korsar varandra.
    # Kommer felaktigt att koppla ihop vägar broar/tunnlar
    dbExecute(con, str_glue("DROP TABLE IF EXISTS {edges}_noded;"))
    dbExecute(con, str_glue("DROP TABLE IF EXISTS {edges}_noded_vertices_pgr;"))
    #dbExecute(con, str_glue("DROP TABLE IF EXISTS {vertices}_noded;"))
    
    dbExecute(con, str_glue("SELECT pgr_nodeNetwork('{edges}', 0.1, 'id', 'geom');"))
    
    dbExecute(con, str_glue("ALTER TABLE {edges}_noded
                ADD COLUMN IF NOT EXISTS hastighet FLOAT,
                ADD COLUMN IF NOT EXISTS riktning_med BOOLEAN,
                ADD COLUMN IF NOT EXISTS riktning_mot BOOLEAN;"))
    
    dbExecute(con, str_glue("UPDATE {edges}_noded rn
                SET hastighet=r.hastighet, 
                    riktning_med=r.riktning_med,
                    riktning_mot=r.riktning_mot
                FROM {edges} r
                WHERE rn.old_id=r.id;"))
    
    # extract vertices version av build_topology eller vanlig metod 
    if(extract_vertices) {
      build_topology_extracted_vertices(str_glue("{edges}_noded"))
    } else {
      build_topology(str_glue("{edges}_noded"))
    }
  }
  
  
  build_topology("nvdb")
  create_noded_network_extracted_vertices("nvdb")
  
  # snabbare om vertices är korrekta - men ingen punkt snapping
  #build_topology_extracted_vertices("nvdb2")
  #create_noded_network_extracted_vertices("nvdb2", extract_vertices=TRUE)
}

# skapa_vagnatverk_tatort()



# ------------------- funktion som skapa kraftfält -------------------------


# funktionen är skapad av Henrik Aldén från SWECOS skript G:/skript/gis/sweco_dec_2022/orginalskript/ del1_kraftfält_QGIS_plugin_uppdaterad.r

# Först  kör funktionen skapa_vagnatverk_tatort() för att skapa temporära tabeller i postgis
# skapa_vagnatverk_tatort()

# LOGIKEN FÖR ANALYSEN FÖLJER
# 
# enskilt_tröskelvärde=15%
# totalt_tröskelvärde=40%
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

pendling_kraftfalt <- function(
    datafile = "G:/Samhällsanalys/GIS/grundkartor/mona/pendlingsrelationer_tatort_nattbef_filtrerad.csv",
    output_folder = "G:/skript/gis/sweco_dec_2022/utdata", 
    gpkg = "kraftfält_qgis_test.gpkg",
    enskilt_tröskelvärde = 20,
    totalt_tröskelvärde = 35,
    primary_la_zone_buffer_length = 2000,
    secondary_la_zone_buffer_length = 1000,
    common_la_zone_buffer_length = 5000,
    con = uppkoppling_db(service = "test_geodata", db_name = "test_geodata"),
    write_to_gpkg = TRUE # Flag to control whether to write to GPKG or return as list
) {
  tryCatch({
    
    library(readxl)
    library(stringr)
    library(dplyr)
    library(sf)
    library(DBI)
    library(RPostgres)
    library(purrr)
    
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
    
    edges_table = "nvdb_noded"
    vertices_table = "nvdb_noded_vertices_pgr"
    cost_col = "dist_cost"
    reverse_cost_col = "dist_reverse_cost"
    
    # simpel funktion för att skriva postgis tabell till gpkg
    write_pgtable2gpkg <- function(lyrname, output_folder, gpkg, append=FALSE, delete_dsn=FALSE) {
      lyr <- st_read(con, lyrname)
      st_write(lyr, 
               file.path(output_folder, gpkg),
               lyrname, 
               append=append,
               delete_dsn=delete_dsn
      )  
    }  
    
    
    pendlingsdata <- read_csv(datafile, locale = locale(encoding = "ISO-8859-1"))
    
    data <- pendlingsdata %>%
      mutate(
        from_id = substr(from_id, 3, 11),
        to_id = substr(to_id, 3, 11)
      ) %>% 
      select(from_id, to_id, n)
    
    dbWriteTable(con, 'data', data, overwrite=TRUE, temporary=FALSE)
    
    query <- str_glue("CREATE TABLE tatort_vertex AS
                    SELECT t.tokod, e.id, e.dist
                    FROM tatort t
                    JOIN lateral(
                      SELECT id, e.geom <-> t.geom as dist
                        FROM {vertices_table} e
                      ORDER BY t.geom <-> e.geom
                      LIMIT 1
                    ) AS e
                    ON true
                    where dist < 2000;")
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
                        from data
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
    
    
 # la
    query <- str_glue("CREATE TEMP TABLE la AS
                    select 
                    	c.from_id as id, c.totalworkers, 
                    	c.localworkers, c.perc_total_commuters,
                    	t.tobeteckn, t.lan,t.kommun, 
                    	t.kommunnamn,t.geom 
                    from commute_combinations c
                    join tatort t on t.tokod=c.from_id
                    where ranking=1 and perc <= {enskilt_tröskelvärde} 
                    and perc_total_commuters <= {totalt_tröskelvärde}
                    ;")
    dbExecute(con, "DROP TABLE IF EXISTS la;")
    dbExecute(con, query)
    # skriv till geopackage
    # write_pgtable2gpkg(str_glue("la"), output_folder, gpkg)
    
# solitär
    query <- str_glue("CREATE TEMP TABLE solitary AS
                  select 
                    	c.from_id as id, c.totalworkers, 
                    	c.localworkers, c.perc_total_commuters,
                    	t.tobeteckn, t.lan,t.kommun, 
                    	t.kommunnamn,t.geom 
                    from commute_combinations c
                    join tatort t on t.tokod=c.from_id
                  where ranking=1 and perc <= {enskilt_tröskelvärde}
                  and perc_total_commuters > {totalt_tröskelvärde}
                  ;")
    dbExecute(con, "DROP TABLE IF EXISTS solitary;")
    dbExecute(con, query)
    # skriv till geopackage
    # write_pgtable2gpkg(str_glue("solitary"), output_folder, gpkg)
    
    
# common la
    query <- str_glue("CREATE TEMP TABLE common_la AS
                    with sats as (
                    	select *
                    	from commute_combinations
                    	where ranking>1 and perc>{enskilt_tröskelvärde}
                    ), common_la as (
                    	select a.*
                    	from sats a join sats b 
                    		on a.from_id=b.to_id and a.to_id=b.from_id
                    )
                    select
                    	c.from_id as id, c.to_id as common_la, 
                    	c.totalworkers, c.localworkers, c.perc_total_commuters,
                    	t.tobeteckn, t.lan,t.kommun, t.kommunnamn,t.geom
                    from common_la c
                    join tatort t on t.tokod=c.from_id
                  ;")
    dbExecute(con, "DROP TABLE IF EXISTS common_la;")
    dbExecute(con, query)
    # skriv till geopackage
    # write_pgtable2gpkg(str_glue("common_la"), output_folder, gpkg)
    
# satelit
    query <- str_glue("CREATE TEMP TABLE satellites AS
                    with sats as (
                    	select *
                    	from commute_combinations
                    	where ranking in (1,2) 
                    		and perc>{enskilt_tröskelvärde}
                    	and from_id not in (select id from common_la)
                    ), la_id as (
                    	select id from la 
                    	union 
                    	select id from common_la
                    ), agg as (
                    	select 
                    	  from_id, 
                    	  array_agg(to_id order by ranking) as id_array_all,
                    	  array_agg(to_id order by ranking) filter (where to_id in (select id from la_id)) as id_array
                    	from sats
                    	group by from_id
                    )
                    select 
                    	a.from_id as id,
                    	id_array[1] as primary_la, 
                    	id_array[2] as secondary_la,
                    	id_array_all[1] as primary_destination,
                    	id_array_all[2] as secondary_destination,
                    	s.totalworkers, s.localworkers, s.perc_total_commuters,
                    	t.tobeteckn, t.lan,t.kommun, t.kommunnamn,t.geom
                    from agg a
                    join sats s on a.from_id=s.from_id
                    join tatort t on t.tokod=s.from_id
                  ;")
    dbExecute(con, "DROP TABLE IF EXISTS satellites;")
    dbExecute(con, query)
    # skriv till geopackage
    # write_pgtable2gpkg(str_glue("satellites"), output_folder, gpkg)
    
# ruttningsanalys
    
    # tabell med allar rutter som ska köras (satelliter+commonLA)
    query <- str_glue("CREATE TEMP TABLE route_combinations AS
                    with sats as (
                    	select from_id, to_id, ranking
                    	from commute_combinations
                    	where ranking in (1,2) 
                    		and perc > {enskilt_tröskelvärde}
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
                  	   FROM {edges_table}',
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
                    FROM astar a JOIN {edges_table} r ON a.edge=r.id
                    GROUP BY a.start_vid, a.end_vid
                  )
                  SELECT c.*, p.agg_cost, p.geom
                  FROM route_combinations c 
                  JOIN paths p ON c.start_vid=p.start_vid 
                    and c.end_vid=p.end_vid")
    dbExecute(con, "DROP TABLE IF EXISTS routes;")
    dbExecute(con, query)
    # write_pgtable2gpkg(str_glue("routes"), output_folder, gpkg)
    
    
    
    # rutter till secondary LA
    query <- str_glue("CREATE TEMP TABLE secla_areas AS
          select
            to_id, max(ranking),
            st_buffer(st_collect(r.geom), {secondary_la_zone_buffer_length}) as geom
          from satellites s
          join routes r on s.id=r.from_id and s.secondary_la=r.to_id
          group by to_id
          ;")
    dbExecute(con, "DROP TABLE IF EXISTS secla_areas;")
    dbExecute(con, query)
    # write_pgtable2gpkg(str_glue("secla_areas"), output_folder, gpkg)
    
    
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
    # write_pgtable2gpkg(str_glue("primla_areas"), output_folder, gpkg)
    
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
    # write_pgtable2gpkg(str_glue("commonla_areas"), output_folder, gpkg)
    
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
    
    # Write layers to Geopackage or return as a list
    if (write_to_gpkg) {
      # Write each layer to the Geopackage
      map(
        layers, 
        ~ write_pgtable2gpkg(
          lyrname = str_glue(.x), 
          output_folder = output_folder, 
          gpkg = gpkg, 
          append = TRUE # Append layers to the same Geopackage
        )
      )
      message(glue("All layers written to {file.path(output_folder, gpkg)}"))
    } else {
      # Read each layer into a list and return
      results <- map(
        layers, 
        ~ st_read(con, query = str_glue("SELECT * FROM {.x}"))
      )
      names(results) <- layers # Name the list elements by their layer names
      return(results)
      # return(list("selected" = selected, "in_commuting" = in_commuting, "out_commuting" = out_commuting)) från rut_pendling
    }
  }, error = function(e){
    stop(glue("Ett fel inträffade vid skapandet av tabeller: {e$message}"))
  })
  
}


# pendling <- skapa_pendling_kraftfalt(gpkg = "kraftfält_qgis_test10.gpkg")

# ------------------- funktion som skapar pendlingsnätverk -------------------------

pendling_natverk <- function(
    datafile = "G:/Samhällsanalys/GIS/grundkartor/mona/pendlingsrelationer_tatort_nattbef_filtrerad.csv",
    con = uppkoppling_db(service = "test_geodata", db_name = "test_geodata"),
    dist = 2000, # max avstånd till vägnätet
    gpkg = TRUE, # Flag to control output
    output_folder = "G:/skript/gis/sweco_dec_2022/utdata", # Output folder for geopackage
    gpkg_name = "pendling_natverk.gpkg" # Name of the geopackage
) { 
  
  library(readxl)
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  edges_table <- "nvdb_noded"
  vertices_table <- "nvdb_noded_vertices_pgr"
  cost_col <- "dist_cost"
  reverse_cost_col <- "dist_reverse_cost"
  
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
  if (gpkg) {
    # Write to geopackage
    output_path <- file.path(output_folder, gpkg_name)
    st_write(network, output_path, delete_dsn = TRUE)
    message(glue("Results written to {output_path}"))
  } else {
    # Return the network as an R object
    return(network)
  }
}

# network <- pendling_natverk(gpkg = FALSE)
# 
# mapview::mapview(network, zcol = "antal_pend", lwd = "antal_pend", alpha = 0.5)


# ------------------- funktion in och utpendling på ruta -------------------------

# funktionen är skapad av Henrik Aldén från SWECOS skript G:/skript/gis/sweco_dec_2022/orginalskript/ del3_rut_pendling.r

# två versioner av skriptet finns som argument. Ev är postgis versionen snabbare vid stora dataset?

# indata parameter ska läggas till

pendling_ruta <- function(version = c("PostGIS", "R"),
                          con_params = list(service = "test_geodata", db_name = "test_geodata"),
                          files_path = "G:/skript/gis/sweco_dec_2022/data/del3",
                          output_folder = "G:/skript/gis/sweco_dec_2022/utdata",
                          grid_epsg = 3006,
                          output_gpkg = FALSE) {
  library(stringr)
  library(dplyr)
  library(sf)
  library(DBI)
  library(RPostgres)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # Validate version
  version <- match.arg(version)
  
  # File paths
  tab <- "RutPendtab.TAB"
  grid <- "RutPendmap.TAB"
  tabfile <- file.path(files_path, tab)
  gridfile <- file.path(files_path, grid)
  
  # Read files
  data_tab <- st_read(tabfile) %>%
    rename(boruta = boruta, arbruta = arbruta, antalpend = antalpend)
  
  grid <- st_read(gridfile) %>%
    rename(rut_id = Rut_Id)
  
  st_crs(grid) <- grid_epsg
  st_geometry(grid) <- "geom"
  
  # Select polygon
  pol <- data.frame(id = 1)
  pol$geom <- ("POLYGON((1485116 6479039,
                        1487385 6472727,
                        1493118 6473596,
                        1494589 6478925,
                        1492357 6480547,
                        1485116 6479039))")
  selected_polygon <- st_as_sf(pol, wkt = "geom")
  st_crs(selected_polygon) <- grid_epsg
  
  if (version == "PostGIS") {
    con <- uppkoppling_db(service = con_params$service, db_name = con_params$db_name)
    
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
    in_commuting <- st_read(con, query = query)
    
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
    out_commuting <- st_read(con, query = query)
    
    if (output_gpkg) {
      st_write(selected, file.path(output_folder, "rut_pendling_pg.gpkg"), "område", delete_dsn = TRUE, append = FALSE)
      st_write(in_commuting, file.path(output_folder, "rut_pendling_pg.gpkg"), "in_pend", append = FALSE)
      st_write(out_commuting, file.path(output_folder, "rut_pendling_pg.gpkg"), "ut_pend", append = FALSE)
    }
    
    return(list("selected" = selected, "in_commuting" = in_commuting, "out_commuting" = out_commuting))
    
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
    
    in_commuting <- filter(result, !is.na(commute_from)) %>% select(-commute_to)
    out_commuting <- filter(result, !is.na(commute_to)) %>% select(-commute_from)
    
    if (output_gpkg) {
      st_write(selected, file.path(output_folder, "rut_pendlingR.gpkg"), "område", delete_dsn = TRUE, append = FALSE)
      st_write(in_commuting, file.path(output_folder, "rut_pendlingR.gpkg"), "in_pend", append = FALSE)
      st_write(out_commuting, file.path(output_folder, "rut_pendlingR.gpkg"), "ut_pend", append = FALSE)
    }
    
    return(list("selected" = selected, "in_commuting" = in_commuting, "out_commuting" = out_commuting))
  }
}

# # Example usage
# r <- rut_pendling(version = "PostGIS")
# s <- rut_pendling(version = "R")
# 
# mapview::mapview(r$selected)
# 
