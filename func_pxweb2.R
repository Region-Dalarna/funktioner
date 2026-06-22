paket <- c(
  "dplyr",
  "purrr",
  "stringr",
  "tibble",
  "tidyr",
  "httr",
  "jsonlite",
  "rjstat",
  "rlang"
)

saknas <- paket[!vapply(paket, requireNamespace, logical(1), quietly = TRUE)]

if (length(saknas) > 0) {
  stop("Följande paket behöver installeras: ", paste(saknas, collapse = ", "))
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

pxweb2_hamta_data <- function(
    tabell = NULL, 
    query = NULL,               # skickas med som lista där varje variabel är namnet och värdet är de värden man vill ha med, kan vara vektorer
    lang = "sv",
    output_format = "json-stat2",
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/",
    query_om_enbart_ogiltiga_varden_for_variabel = "stop",          # om alla värden för en variabel är ogiltiga stoppas uttaget om = "stop", om "*" returneras alla värden för variabeln, om "null" returneras NULL (behändigt om man vill hämta ur fler tabeller men där olika år eller regioner finns i olika tabeller)
    allow_label_values = TRUE,                      # om TRUE så kan man skicka med klartext för värden i queries dvs. både "20" och "Dalarnas län" går bra
    rensakod_i_label = TRUE,                        # rensar bort koder som också ligger med i klartext-kolumnen. Om bara kod finns och inte klartext så får den vara klartext också
    senaste_tid_kod = "9999",                       # med detta värde hämtar senaste värdet för tidkolumnen, NULL = används inte
    allow_api_wildcards = TRUE,                     # TRUE = om query innehåller "*" så används API:ets inbyggda wildcard-funktion, FALSE = "*" hanteras som vanligt värde och matchas mot giltiga värden i tabellen
    variabelnamn_harmonisera = NULL,                # används när man hämtar flera tabeller på en gång. Skicka in en namnsatt vektor, tex. c("Region" = "Kommun"), då kommer variabeln "Kommun" döpas om till "Region" så att alla tabeller har samma variabelnamn för kommuner och kommunkoder
    deso_regso_versioner_hantera = "senaste",       # om det finns flera deso- och regsoversioner så används den senaste versionen som har ett värde, vid summering summeras alla versioner och vid NULL görs ingenting
    deso_regso_splitta_kommun = TRUE,               # lägger till kolumnerna kommun_kod och kommun samt behåller bara namnet på Regso och Deso i kolumnen region
    include_aggregations = "none",                  # styr aggregeringar i giltiga_varden_list (se pxweb2_varden): "none"/FALSE = inga, "all"/TRUE = alla, "auto" = auto, "codelists" = bara metadata, eller namnsatt vektor c(Region = "agg_RegionLA2018")
    auto_limit = 30L                               # max antal totala kodliste-anrop vid include_aggregations = "auto"
){
  if (is.null(tabell)) stop("table_id måste anges")
  if (!query_om_enbart_ogiltiga_varden_for_variabel %in% c("stop", "*", "null")) {
    stop("Ogiltigt värde för query_om_enbart_ogiltiga_varden_for_variabel. Tillåtna: \"stop\", \"*\", \"null\".")
  }
  
  # kontrollera om det är mer än en tabell
  if (!is.list(tabell) && length(tabell) > 1) {
    return(
      intern_pxweb2_hamta_flera_tabeller(
        tabeller = tabell,
        query = query,
        lang = lang,
        output_format = output_format,
        base_url = base_url,
        query_om_enbart_ogiltiga_varden_for_variabel = query_om_enbart_ogiltiga_varden_for_variabel,
        allow_label_values = allow_label_values,
        rensakod_i_label = rensakod_i_label,
        senaste_tid_kod = senaste_tid_kod,
        allow_api_wildcards = allow_api_wildcards,
        variabelnamn_harmonisera = variabelnamn_harmonisera,
        deso_regso_versioner_hantera = deso_regso_versioner_hantera,
        deso_regso_splitta_kommun = deso_regso_splitta_kommun,
        include_aggregations = include_aggregations
      )
    )
  }
  
  if (!is.null(deso_regso_versioner_hantera)) {
    deso_regso_versioner_hantera <- match.arg(
      deso_regso_versioner_hantera,
      choices = c("senaste", "summering")
    )
  }
  
  if (!is.list(tabell)) {
    if (!stringr::str_detect(tabell, "^TAB\\d+$")) stop("tabell-id är inte giltigt, ska vara av typen 'TAB' följt av siffror.")
    metadata <- pxweb2_meta(tabell)
  } else {
    metadata <- tabell
    tabell <- metadata$extension$px$tableid
  }
  
  data_url <- paste0(base_url, tabell, "/data")
  
  variabler_df <- pxweb2_variabler(metadata)          # hämta alla variabler
  giltiga_varden_list <- pxweb2_varden(metadata, include_aggregations = include_aggregations, auto_limit = auto_limit)      # hämta alla unika värden för alla variabler (inkl. aggregeringar om det är valt)
  
  # skapa querylista av medskickad query alternativt skapa en för alla giltiga värden
  query_list <- if (is.null(query)) {
    intern_pxweb2_create_variable_query_list(variabler_df)
  } else {
    if (intern_pxweb2_is_pxweb_query_list(query)) {
      query
    } else {
      intern_pxweb2_list_to_query_list(
        variabler_df,
        query,
        giltiga_varden_list = giltiga_varden_list,
        allow_label_values = allow_label_values
      )
    }
  }
  
  query_list <- query_list |>
    intern_pxweb2_resolve_senaste_tid(
      variabler_df = variabler_df,
      giltiga_varden_list = giltiga_varden_list,
      senaste_tid_kod = senaste_tid_kod
    ) |>
    intern_pxweb2_resolve_api_wildcards(
      giltiga_varden_list = giltiga_varden_list,
      allow_api_wildcards = allow_api_wildcards
    ) |>
    intern_pxweb2_sanitize_query_values(
      giltiga_varden_list = giltiga_varden_list,
      query_om_enbart_ogiltiga_varden_for_variabel = query_om_enbart_ogiltiga_varden_for_variabel
    )
  
  
  # om query_list = NULL så returneras NULL - bra att använda när vi vill hämta ett antal värden för variabler som ligger i olika tabeller
  
  # expandera ev. aggregations/”**” till flera requests
  if (any(purrr::map_lgl(query_list$selection, ~ is.null(.x$valueCodes)))) {
    return(NULL)
  }
  
  request_list <- intern_pxweb2_expand_requests_generic(query_list, giltiga_varden_list)
  
  
  # droppa requestar som saknar selection eller är NULL
  request_list <- purrr::compact(request_list)
  if (length(request_list) == 0) return(NULL)
  
  # dela upp i chunks om det är fler än 150.000 celler i uttaget
  query_chunks <- intern_pxweb2_make_request_chunks(
    variabler_df,
    request_list,
    giltiga_varden_list = giltiga_varden_list
  )
  

  # 1) Ta fram vilka kodkolumner som ska läggas till
  cols_to_add <- variabler_df |>
    dplyr::filter(
      show == "code_value" |
        role == "geo" |
        stringr::str_to_lower(code) == "region" |
        stringr::str_to_lower(label) == "region"
    ) |>
    dplyr::transmute(
      code_col = code,
      txt_col = label,
      new_name = paste0(stringr::str_to_lower(label), "_kod")
    )
  
  # här hämtas all data ============
  retur_tabell <- purrr::map(query_chunks, function(req) {
    
    # avgör om vi måste köra GET (codelist/outputValues funkar här) eller POST
    use_get <- length(req$extra_query) > 0
    
    if (use_get) {
      # Bygg query-parametrar valueCodes[...] från req$body$selection
      vc_query <- purrr::map(req$body$selection, function(s) {
        var  <- s$variableCode
        vals <- unlist(s$valueCodes, use.names = FALSE)
        stats::setNames(list(paste(vals, collapse = ",")), paste0("valueCodes[", var, "]"))
      }) |>
        purrr::flatten()
      
      data_resp <- intern_pxweb2_GET(
        data_url,
        query = c(list(lang = lang, outputFormat = output_format), vc_query, req$extra_query),
        httr::accept_json()
      )
    } else {
      data_resp <- intern_pxweb2_POST(
        data_url,
        body = jsonlite::toJSON(req$body, auto_unbox = TRUE),
        query = list(lang = lang, outputFormat = output_format),
        encode = "raw",
        httr::content_type_json(),
        httr::accept_json()
      )
    }
    
    if (httr::status_code(data_resp) >= 400) {
      cat("HTTP ", httr::status_code(data_resp), "\n", sep = "")
      cat(httr::content(data_resp, "text", encoding = "UTF-8"), "\n", sep = "")
    }
    httr::stop_for_status(data_resp)
    
    json_text <- httr::content(data_resp, "text")
    
    df_klartext <- rjstat::fromJSONstat(json_text, naming = "label")
    
    # om det finns kolumner som vi ska ta med koder för så hämtas de här
    df_koder <- if (nrow(cols_to_add) > 0) {
      rjstat::fromJSONstat(json_text, naming = "id") |> 
        dplyr::select(dplyr::all_of(setNames(cols_to_add$code_col, cols_to_add$new_name)))
    } else NULL
    
    # lägg ihop klartext- och kod-kolumner
    df_resultat <- dplyr::bind_cols(purrr::compact(list(df_koder, df_klartext)))
    
    if (isTRUE(rensakod_i_label) && nrow(cols_to_add) > 0) {
      df_resultat <- purrr::reduce(seq_len(nrow(cols_to_add)), function(acc, i) {
        
        kodkol <- cols_to_add$new_name[i]
        txtkol <- cols_to_add$txt_col[i]
        
        if (!all(c(kodkol, txtkol) %in% names(acc))) {
          return(acc)
        }
        
        
        acc[[txtkol]] <- intern_pxweb2_rensa_kod_i_label(
          label = acc[[txtkol]],
          kod = acc[[kodkol]]
        )
        
        acc
        
      }, .init = df_resultat)
      
    }
    
    return(df_resultat)
    
  }, .progress = TRUE) |> 
    purrr::list_rbind()
  
  # om hantering av deso/regso-versioner ska göras så görs det här (dvs. har värdet "senaste" eller "summera" men inte NULL)
  retur_tabell <- intern_pxweb2_hantera_deso_regso_versioner(
    retur_tabell,
    hantera = deso_regso_versioner_hantera,
    value_col = "value"
  )
  
  # om man vill splitta kommunkod och kommun till egna kolumner samt bara behåller regsonamn (och desokod som namn) i kolumnen region
  if (isTRUE(deso_regso_splitta_kommun)) {
    retur_tabell <- intern_pxweb2_splitta_deso_regso_kommun(retur_tabell)
    
    retur_tabell <- intern_pxweb2_fyll_deso_kommun_fran_metadata(
      df = retur_tabell,
      metadata = metadata
    )
  }
  
  # flytta varje kod-kolumn och placera framför sin klartext-kolumn
  df_resultat <- purrr::reduce(seq_len(nrow(cols_to_add)), function(acc, i) {
    dplyr::relocate(acc, dplyr::all_of(cols_to_add$new_name[i]),
                    .before = dplyr::all_of(cols_to_add$txt_col[i]))
  }, .init = retur_tabell)
  
  
  return(df_resultat)
  
}

# hämta när en tabell uppdaterades
pxweb2_tabell_uppdaterades <- function(
    tabell
) {
  pxweb2_meta(tabell)$updated
}

# funktion för att kontrollera om en tabell behöver uppdateras (vilket är fallet om datum_tid_txt är äldre än uppderingsdatum för scb-tabellen)
pxweb2_tabell_behover_uppdateras <- function(
    tabell,
    datum_tid_txt                  # datum + tid för en tabell man vill jämföra med, i samma format som SCB:s updated i metadata-tabellen,
) {                                # nämligen: 2026-06-02T00:59:31Z
  
  if (is.null(tabell) || length(tabell) != 1) {
    stop("tabell måste vara ett tabell-id med längd 1.", call. = FALSE)
  }
  
  if (is.null(datum_tid_txt) || length(datum_tid_txt) != 1) {
    stop("datum_tid_txt måste vara ett textvärde med längd 1.", call. = FALSE)
  }
  
  format_ok <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
  
  if (!stringr::str_detect(datum_tid_txt, format_ok)) {
    stop(
      "datum_tid_txt måste ha formatet 'YYYY-MM-DDTHH:MM:SSZ', t.ex. '2025-05-28T06:00:00Z'.",
      call. = FALSE
    )
  }
  
  scb_updated <- pxweb2_tabell_uppdaterades(tabell)
  
  if (is.na(scb_updated)) {
    return(NA)
  }
  
  if (!stringr::str_detect(scb_updated, format_ok)) {
    stop(
      "SCB:s updated-värde har inte förväntat format: ",
      scb_updated,
      call. = FALSE
    )
  }
  
  scb_updated > datum_tid_txt
} # slut funktion pxweb2_tabell_uppdaterades


# splitta upp kommunkod och kommunnamn till egna kolumner, renodla region som bara innehåller regso/deso-namnet (desonamnet är samma som koden)
intern_pxweb2_splitta_deso_regso_kommun <- function(df,
                                                    region_col = "region",
                                                    region_kod_col = "region_kod") {
  
  if (!all(c(region_col, region_kod_col) %in% names(df))) {
    return(df)
  }
  
  region_koder <- as.character(df[[region_kod_col]])
  
  grund_region_koder <- dplyr::if_else(
    stringr::str_detect(stringr::str_to_lower(region_koder), "deso|regso"),
    stringr::str_remove(region_koder, "_.*$"),
    region_koder
  )
  
  har_deso_regso <- stringr::str_detect(
    grund_region_koder,
    "^\\d{4}[ABC]\\d+|^\\d{4}R\\d+"
  ) |
    stringr::str_detect(
      stringr::str_to_lower(region_koder),
      "deso|regso"
    )
  
  if (!any(har_deso_regso, na.rm = TRUE)) {
    return(df)
  }
  
  df |>
    dplyr::mutate(
      .region_tmp = as.character(.data[[region_col]]),
      .region_kod_tmp = as.character(.data[[region_kod_col]]),
      
      .grund_region_kod = dplyr::if_else(
        stringr::str_detect(stringr::str_to_lower(.region_kod_tmp), "deso|regso"),
        stringr::str_remove(.region_kod_tmp, "_.*$"),
        .region_kod_tmp
      ),
      
      .ar_deso = stringr::str_detect(.grund_region_kod, "^\\d{4}[ABC]\\d+"),
      .ar_regso = stringr::str_detect(.grund_region_kod, "^\\d{4}R\\d+"),
      .ar_deso_regso = .ar_deso | .ar_regso,
      
      .region_text_ren = .region_tmp,
      .region_text_ren = stringr::str_remove(
        .region_text_ren,
        paste0("^", stringr::str_escape(.region_kod_tmp), "\\s+")
      ),
      .region_text_ren = stringr::str_remove(
        .region_text_ren,
        paste0("^", stringr::str_escape(.grund_region_kod), "\\s+")
      ),
      .region_text_ren = stringr::str_trim(.region_text_ren),
      
      .har_parentes = stringr::str_detect(.region_text_ren, "\\([^()]+\\)"),
      
      kommun_kod = dplyr::if_else(
        .ar_deso_regso & stringr::str_detect(.grund_region_kod, "^\\d{4}"),
        stringr::str_sub(.grund_region_kod, 1, 4),
        NA_character_
      ),
      
      kommun = dplyr::case_when(
        .har_parentes ~ stringr::str_trim(
          stringr::str_remove(.region_text_ren, "\\s*\\([^()]+\\)\\s*$")
        ),
        .ar_deso & .region_text_ren != "" & .region_text_ren != .grund_region_kod ~ .region_text_ren,
        TRUE ~ NA_character_
      ),
      
      "{region_col}" := dplyr::case_when(
        .har_parentes ~ stringr::str_match(.region_text_ren, "\\(([^()]+)\\)")[, 2],
        .ar_deso ~ .grund_region_kod,
        .ar_regso & .region_text_ren != "" ~ .region_text_ren,
        .ar_regso ~ .grund_region_kod,
        TRUE ~ .region_tmp
      ),
      
      "{region_kod_col}" := dplyr::if_else(
        .ar_deso_regso,
        .grund_region_kod,
        .region_kod_tmp
      )
    ) |>
    dplyr::select(
      -.region_tmp,
      -.region_kod_tmp,
      -.grund_region_kod,
      -.ar_deso,
      -.ar_regso,
      -.ar_deso_regso,
      -.region_text_ren,
      -.har_parentes
    ) |>
    dplyr::relocate(
      dplyr::any_of(c("kommun_kod", "kommun")),
      .after = dplyr::all_of(region_kod_col)
    )
}

# för att hantera flera olika versioner av deso eller regso i samma tabell
intern_pxweb2_hantera_deso_regso_versioner <- function(df,
                                                       hantera = c("senaste", "summera"),
                                                       region_col = "region",
                                                       region_kod_col = "region_kod",
                                                       value_col = "value") {
  
  if (is.null(hantera)) return(df)
  
  hantera <- match.arg(hantera)
  
  if (!all(c(region_col, region_kod_col, value_col) %in% names(df))) {
    return(df)
  }
  
  tmp <- df |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .region_kod_tmp = as.character(.data[[region_kod_col]]),
      .region_tmp = as.character(.data[[region_col]]),
      
      .grund_region_kod = stringr::str_remove(.region_kod_tmp, "_.*$"),
      
      .ar_deso_regso = stringr::str_detect(
        .grund_region_kod,
        "^\\d{4}[ABC]\\d+|^\\d{4}R\\d+"
      ),
      
      .version_ar = stringr::str_match(
        stringr::str_to_lower(.region_kod_tmp),
        "_(?:deso|regso)(\\d{4})"
      )[, 2],
      .version_ar = suppressWarnings(as.integer(.version_ar)),
      .version_ar = dplyr::if_else(is.na(.version_ar), 0L, .version_ar),
      
      .har_version = stringr::str_detect(
        stringr::str_to_lower(.region_kod_tmp),
        "_(?:deso|regso)\\d{4}"
      ),
      
      .value_num = suppressWarnings(as.numeric(.data[[value_col]])),
      .har_data = !is.na(.value_num) & .value_num != 0
    )
  
  tmp_ovriga <- tmp |>
    dplyr::filter(!.ar_deso_regso)
  
  tmp_deso_regso <- tmp |>
    dplyr::filter(.ar_deso_regso)
  
  if (nrow(tmp_deso_regso) == 0) {
    return(
      tmp |>
        dplyr::select(
          -.row_id,
          -.region_kod_tmp,
          -.region_tmp,
          -.grund_region_kod,
          -.ar_deso_regso,
          -.version_ar,
          -.har_version,
          -.value_num,
          -.har_data
        )
    )
  }
  
  grupperingskolumner <- setdiff(
    names(tmp_deso_regso),
    c(
      region_kod_col,
      region_col,
      value_col,
      "kommun_kod",
      "kommun",
      ".row_id",
      ".region_kod_tmp",
      ".region_tmp",
      ".grund_region_kod",
      ".ar_deso_regso",
      ".version_ar",
      ".har_version",
      ".value_num",
      ".har_data"
    )
  )
  
  grupperingskolumner <- c(".grund_region_kod", grupperingskolumner)
  
  if (hantera == "senaste") {
    
    tmp_deso_regso_hanterad <- tmp_deso_regso |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grupperingskolumner))) |>
      dplyr::arrange(
        dplyr::desc(.har_data),
        dplyr::desc(.version_ar),
        dplyr::desc(.har_version),
        .row_id,
        .by_group = TRUE
      ) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        "{region_kod_col}" := .grund_region_kod
      )
    
  } else if (hantera == "summera") {
    
    tmp_deso_regso_hanterad <- tmp_deso_regso |>
      dplyr::mutate(
        "{region_kod_col}" := .grund_region_kod
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(region_kod_col, grupperingskolumner)))) |>
      dplyr::arrange(
        dplyr::desc(.har_data),
        dplyr::desc(.version_ar),
        .row_id,
        .by_group = TRUE
      ) |>
      dplyr::summarise(
        "{region_col}" := dplyr::first(.data[[region_col]]),
        "{value_col}" := sum(.value_num, na.rm = TRUE),
        .row_id = dplyr::first(.row_id),
        .groups = "drop"
      )
  }
  
  dplyr::bind_rows(tmp_ovriga, tmp_deso_regso_hanterad) |>
    dplyr::arrange(.row_id) |>
    dplyr::select(
      -dplyr::any_of(c(
        ".row_id",
        ".region_kod_tmp",
        ".region_tmp",
        ".grund_region_kod",
        ".ar_deso_regso",
        ".version_ar",
        ".har_version",
        ".value_num",
        ".har_data"
      ))
    )
}


intern_pxweb2_hitta_kommun_valueset_url <- function(metadata,
                                                    region_var = "Region") {
  
  region_dim <- metadata$dimension[[region_var]]
  
  if (is.null(region_dim)) {
    return(NULL)
  }
  
  cl <- purrr::pluck(region_dim, "extension", "codelists", .default = NULL)
  
  if (is.null(cl) || length(cl) == 0) {
    return(NULL)
  }
  
  cl_df <- purrr::map_dfr(cl, function(item) {
    tibble::tibble(
      id = purrr::pluck(item, "id", .default = NA_character_),
      label = purrr::pluck(item, "label", .default = NA_character_),
      type = purrr::pluck(item, "type", .default = NA_character_),
      url = purrr::pluck(item, "links", 1, "href", .default = NA_character_)
    )
  })
  
  hit <- cl_df |>
    dplyr::filter(
      stringr::str_to_lower(type) == "valueset",
      stringr::str_detect(stringr::str_to_lower(label), "kommun") |
        stringr::str_detect(stringr::str_to_lower(id), "kommun")
    )
  
  if (nrow(hit) == 0) {
    return(NULL)
  }
  
  hit$url[[1]]
}


intern_pxweb2_hamta_kommunnyckel_fran_metadata <- function(metadata,
                                                           region_var = "Region") {
  
  url <- intern_pxweb2_hitta_kommun_valueset_url(
    metadata = metadata,
    region_var = region_var
  )
  
  if (is.null(url) || is.na(url) || identical(url, "")) {
    return(NULL)
  }
  
  resp <- intern_pxweb2_GET(url, httr::accept_json())
  httr::stop_for_status(resp)
  
  cl <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  
  values <- purrr::pluck(cl, "values", .default = NULL)
  
  if (is.null(values) || length(values) == 0) {
    return(NULL)
  }
  
  kommunnyckel <- purrr::map_dfr(values, function(x) {
    tibble::tibble(
      kommun_kod = as.character(purrr::pluck(x, "code", .default = NA_character_)),
      kommun = as.character(purrr::pluck(x, "label", .default = NA_character_))
    )
  }) |>
    dplyr::filter(!is.na(kommun_kod), !is.na(kommun)) |>
    dplyr::mutate(
      kommun = intern_pxweb2_rensa_kodprefix_i_label(
        label = kommun,
        kod = kommun_kod
      )
    ) |>
    dplyr::distinct(kommun_kod, .keep_all = TRUE)
  return(kommunnyckel)
}

intern_pxweb2_fyll_deso_kommun_fran_metadata <- function(df,
                                                         metadata,
                                                         region_kod_col = "region_kod",
                                                         kommun_kod_col = "kommun_kod",
                                                         kommun_col = "kommun") {
  
  if (!all(c(region_kod_col, kommun_kod_col, kommun_col) %in% names(df))) {
    return(df)
  }
  
  region_kod <- as.character(df[[region_kod_col]])
  
  ar_deso <- stringr::str_detect(region_kod, "^\\d{4}[ABC]\\d+")
  
  if (!any(ar_deso, na.rm = TRUE)) {
    return(df)
  }
  
  saknar_kommun <- is.na(df[[kommun_col]]) | df[[kommun_col]] == ""
  
  if (!any(ar_deso & saknar_kommun, na.rm = TRUE)) {
    return(df)
  }
  
  kommunnyckel <- intern_pxweb2_hamta_kommunnyckel_fran_metadata(metadata)
  
  if (is.null(kommunnyckel) || nrow(kommunnyckel) == 0) {
    return(df)
  }
  
  match_idx <- match(
    as.character(df[[kommun_kod_col]]),
    kommunnyckel$kommun_kod
  )
  
  kommun_fran_metadata <- kommunnyckel$kommun[match_idx]
  
  fyll <- ar_deso & saknar_kommun & !is.na(kommun_fran_metadata)
  
  df[[kommun_col]][fyll] <- kommun_fran_metadata[fyll]
  
  df
}


intern_pxweb2_rensa_kodprefix_i_label <- function(label, kod) {
  
  label_chr <- as.character(label)
  kod_chr <- as.character(kod)
  
  purrr::map2_chr(label_chr, kod_chr, function(lbl, k) {
    
    if (is.na(lbl) || is.na(k)) {
      return(lbl)
    }
    
    rensad <- stringr::str_remove(
      lbl,
      paste0("^", stringr::str_escape(k), "\\s+")
    ) |>
      stringr::str_trim()
    
    if (identical(rensad, "")) {
      lbl
    } else {
      rensad
    }
  })
}


# Hjälpfunktion: kontrollera om query innehåller "9999"
intern_pxweb2_query_innehaller_senaste_tid <- function(query, senaste_tid_kod = "9999") {
  
  if (is.null(query) || is.null(senaste_tid_kod)) {
    return(FALSE)
  }
  
  if (intern_pxweb2_is_pxweb_query_list(query)) {
    return(
      any(
        purrr::map_lgl(query$selection, function(x) {
          senaste_tid_kod %in% as.character(unlist(x$valueCodes, use.names = FALSE))
        })
      )
    )
  }
  
  any(
    purrr::map_lgl(query, function(x) {
      senaste_tid_kod %in% as.character(x)
    })
  )
}

# Hjälpfunktion: ersätt "9999" med vald gemensam tid
intern_pxweb2_ersatt_senaste_tid_i_query <- function(query, senaste_tid_kod, senaste_tid) {
  
  if (is.null(query) || is.null(senaste_tid_kod)) {
    return(query)
  }
  
  if (intern_pxweb2_is_pxweb_query_list(query)) {
    
    query$selection <- purrr::map(query$selection, function(x) {
      
      x$valueCodes <- purrr::map(x$valueCodes, function(v) {
        v <- as.character(v)
        v[v == senaste_tid_kod] <- senaste_tid
        v
      })
      
      x
    })
    
    return(query)
  }
  
  purrr::map(query, function(x) {
    x <- as.character(x)
    x[x == senaste_tid_kod] <- senaste_tid
    x
  })
}

# Hjälpfunktion: hitta tidsvariabel
# Den här bygger på att pxweb2_variabler() returnerar något i stil med code, label, role.
intern_pxweb2_hitta_tidvariabel <- function(variabler_df) {
  
  kandidat <- variabler_df |>
    dplyr::mutate(
      code_lower = stringr::str_to_lower(code),
      label_lower = stringr::str_to_lower(label),
      role_lower = stringr::str_to_lower(role)
    ) |>
    dplyr::filter(
      role_lower == "time" |
        code_lower %in% c("tid", "år", "ar", "time", "månad", "manad") |
        label_lower %in% c("tid", "år", "ar", "time", "månad", "manad")
    )
  
  if (nrow(kandidat) == 0) {
    stop("Kunde inte identifiera tidsvariabel i metadata.", call. = FALSE)
  }
  
  if (nrow(kandidat) > 1) {
    stop(
      "Flera möjliga tidsvariabler hittades: ",
      paste(kandidat$code, collapse = ", "),
      call. = FALSE
    )
  }
  
  kandidat$code[[1]]
}

# Hjälpfunktion: hämta värden för tidsvariabel
intern_pxweb2_hamta_tidvarden <- function(metadata) {
  
  variabler_df <- pxweb2_variabler(metadata)
  giltiga_varden_list <- pxweb2_varden(metadata)
  
  tid_var <- intern_pxweb2_hitta_tidvariabel(variabler_df)
  
  if (!tid_var %in% names(giltiga_varden_list)) {
    stop(
      "Tidsvariabeln ",
      tid_var,
      " finns inte som namn i giltiga_varden_list.",
      call. = FALSE
    )
  }
  
  tid_df <- giltiga_varden_list[[tid_var]]
  
  if (!is.data.frame(tid_df)) {
    return(as.character(tid_df))
  }
  
  mojliga_kodkolumner <- c(
    "code",
    "value",
    "id",
    "values",
    "valueCode",
    "value_code",
    "kod"
  )
  
  kodkolumn <- intersect(mojliga_kodkolumner, names(tid_df))[1]
  
  if (is.na(kodkolumn)) {
    kodkolumn <- names(tid_df)[1]
    
    warning(
      "Kunde inte identifiera kodkolumn för tidsvariabeln ",
      tid_var,
      ". Använder första kolumnen: ",
      kodkolumn,
      call. = FALSE
    )
  }
  
  tid_df[[kodkolumn]] |>
    as.character()
}


# hitta senaste tid när det skickas med flera tabeller
intern_pxweb2_senaste_gemensamma_tid <- function(metadata_lista) {
  
  tider_lista <- metadata_lista |>
    purrr::map(intern_pxweb2_hamta_tidvarden)
  
  gemensamma_tider <- Reduce(intersect, tider_lista)
  
  if (length(gemensamma_tider) == 0) {
    stop(
      "Det finns ingen gemensam tidsperiod mellan tabellerna.",
      call. = FALSE
    )
  }
  
  gemensamma_tider |>
    sort(decreasing = TRUE) |>
    (\(x) x[[1]])()
}

intern_pxweb2_senaste_tid_alla_tabeller <- function(metadata_lista) {
  
  tider_lista <- metadata_lista |>
    purrr::map(intern_pxweb2_hamta_tidvarden)
  
  alla_tider <- Reduce(union, tider_lista)
  
  if (length(alla_tider) == 0) {
    stop(
      "Kunde inte hitta några tidsvärden i tabellerna.",
      call. = FALSE
    )
  }
  
  alla_tider |>
    sort(decreasing = TRUE) |>
    (\(x) x[[1]])()
}


# Hjälpfunktion: varna om olika struktur
intern_pxweb2_varna_om_olika_struktur <- function(resultat_lista) {
  
  kolumner_lista <- resultat_lista |>
    purrr::map(names)
  
  alla_kolumner <- Reduce(union, kolumner_lista)
  
  saknade_lista <- kolumner_lista |>
    purrr::imap(function(kolumner, namn) {
      setdiff(alla_kolumner, kolumner)
    })
  
  har_skillnader <- any(lengths(saknade_lista) > 0)
  
  if (isTRUE(har_skillnader)) {
    
    detaljer <- saknade_lista |>
      purrr::imap_chr(function(saknade, namn) {
        if (length(saknade) == 0) {
          paste0(namn, ": inga saknade kolumner")
        } else {
          paste0(namn, ": saknar ", paste(saknade, collapse = ", "))
        }
      }) |>
      paste(collapse = "\n")
    
    warning(
      "Tabellerna har inte identisk kolumnstruktur. ",
      "Saknade kolumner fylls med NA vid dplyr::bind_rows().\n",
      detaljer,
      call. = FALSE
    )
  }
  
  invisible(NULL)
}

# Huvudfunktionen för flera tabeller
intern_pxweb2_hamta_flera_tabeller <- function(
    tabeller,
    query = NULL,
    lang = "sv",
    output_format = "json-stat2",
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/",
    query_om_enbart_ogiltiga_varden_for_variabel = "stop",
    allow_label_values = TRUE,
    rensakod_i_label = TRUE,
    senaste_tid_kod = "9999",
    allow_api_wildcards = TRUE,
    variabelnamn_harmonisera = NULL,
    deso_regso_versioner_hantera = "senaste",
    deso_regso_splitta_kommun = TRUE,
    include_aggregations = "none",
    auto_limit = 30L
) {
  
  if (!all(stringr::str_detect(tabeller, "^TAB\\d+$"))) {
    stop(
      "Alla tabell-id måste vara av typen 'TAB' följt av siffror.",
      call. = FALSE
    )
  }
  
  metadata_lista <- tabeller |>
    purrr::map(pxweb2_meta)
  
  names(metadata_lista) <- tabeller
  
  # Lös upp "auto" en gång baserat på totalt antal kodlistor för ALLA tabeller
  if (identical(include_aggregations, "auto")) {
    intern_cl_antal <- function(meta) {
      purrr::map_int(meta$dimension, function(dim_el) {
        cl <- purrr::pluck(dim_el, "extension", "codelists", .default = NULL)
        if (is.null(cl)) 0L else sum(purrr::map_lgl(cl, ~ tolower(purrr::pluck(.x, "type", .default = "")) == "aggregation"))
      }) |> sum()
    }
    totalt <- purrr::map_int(metadata_lista, intern_cl_antal) |> sum()
    include_aggregations <- if (totalt <= auto_limit) {
      message(
        "include_aggregations = \"auto\": hittade ", totalt, " kodlistor totalt (",
        length(tabeller), " tabeller, gräns ", auto_limit, ") och hämtar alla aggregeringar."
      )
      "all"
    } else {
      message(
        "include_aggregations = \"auto\": hittade ", totalt, " kodlistor totalt (",
        length(tabeller), " tabeller) vilket överstiger gränsen ", auto_limit,
        ". Hämtar bara kodlistemetadata (\"codelists\"). Sätt \"all\" för att hämta allt."
      )
      "codelists"
    }
  }
  
  innehaller_senaste_tid <- intern_pxweb2_query_innehaller_senaste_tid(
    query = query,
    senaste_tid_kod = senaste_tid_kod
  )
  
  query_justerad <- query
  tabeller_att_hamta <- tabeller
  
  if (isTRUE(innehaller_senaste_tid)) {
    
    tider_lista <- metadata_lista |>
      purrr::map(intern_pxweb2_hamta_tidvarden)
    
    senaste_tid <- tider_lista |>
      Reduce(f = union) |>
      sort(decreasing = TRUE) |>
      (\(x) x[[1]])()
    
    query_justerad <- intern_pxweb2_ersatt_senaste_tid_i_query(
      query = query,
      senaste_tid_kod = senaste_tid_kod,
      senaste_tid = senaste_tid
    )
    
    tabeller_att_hamta <- tabeller[
      purrr::map_lgl(tabeller, function(tabell_id) {
        senaste_tid %in% tider_lista[[tabell_id]]
      })
    ]
    
    message(
      "senaste_tid_kod = '",
      senaste_tid_kod,
      "' ersattes med senaste tid i tabellerna: ",
      senaste_tid
    )
    
    if (length(tabeller_att_hamta) < length(tabeller)) {
      tabeller_skippade <- setdiff(tabeller, tabeller_att_hamta)
      
      message(
        "Följande tabell(er) innehåller inte ",
        senaste_tid,
        " och hämtas därför inte: ",
        paste(tabeller_skippade, collapse = ", ")
      )
    }
  }
  
  if (length(tabeller_att_hamta) == 0) {
    return(NULL)
  }
  
  resultat_lista <- tabeller_att_hamta |>
    purrr::map(function(tabell_id) {
      
      metadata_tabell <- metadata_lista[[tabell_id]]
      variabler_df_tabell <- pxweb2_variabler(metadata_tabell)
      
      query_tabell <- query_justerad |>
        intern_pxweb2_harmonisera_querynamn(
          variabler_df = variabler_df_tabell,
          variabelnamn_harmonisera = variabelnamn_harmonisera
        )
      
      pxweb2_hamta_data(
        tabell = metadata_tabell,
        query = query_tabell,
        lang = lang,
        output_format = output_format,
        base_url = base_url,
        query_om_enbart_ogiltiga_varden_for_variabel = query_om_enbart_ogiltiga_varden_for_variabel,
        allow_label_values = allow_label_values,
        rensakod_i_label = rensakod_i_label,
        senaste_tid_kod = senaste_tid_kod,
        allow_api_wildcards = allow_api_wildcards,
        deso_regso_versioner_hantera = deso_regso_versioner_hantera,
        deso_regso_splitta_kommun = deso_regso_splitta_kommun,
        include_aggregations = include_aggregations,
        auto_limit = auto_limit
      ) |>
        intern_pxweb2_harmonisera_resultatnamn(
          variabelnamn_harmonisera = variabelnamn_harmonisera
        ) |>
        dplyr::mutate(
          tabell_id = tabell_id,
          .before = 1
        )
    })
  
  names(resultat_lista) <- tabeller_att_hamta
  
  resultat_lista <- purrr::compact(resultat_lista)
  
  if (length(resultat_lista) == 0) {
    return(NULL)
  }
  
  intern_pxweb2_varna_om_olika_struktur(resultat_lista)
  
  dplyr::bind_rows(resultat_lista)
} # slut funktion intern_pxweb2_hamta_flera_tabeller


# Hjälpfunktion: harmonisera querynamn
# Den här gör att användaren kan skriva Region = ..., även om en viss tabell egentligen har variabeln Kommun.
intern_pxweb2_harmonisera_querynamn <- function(
    query,
    variabler_df,
    variabelnamn_harmonisera = NULL
) {
  
  if (is.null(query) || is.null(variabelnamn_harmonisera)) {
    return(query)
  }
  
  if (intern_pxweb2_is_pxweb_query_list(query)) {
    return(query)
  }
  
  if (is.null(names(variabelnamn_harmonisera))) {
    stop(
      "variabelnamn_harmonisera måste vara en namngiven vektor, t.ex. c(\"Region\" = \"Kommun\").",
      call. = FALSE
    )
  }
  
  tabell_variabler <- variabler_df$code
  tabell_variabler_lag <- stringr::str_to_lower(tabell_variabler)
  
  query_namn <- names(query)
  query_namn_lag <- stringr::str_to_lower(query_namn)
  
  for (standardnamn in names(variabelnamn_harmonisera)) {
    
    alternativnamn <- unname(variabelnamn_harmonisera[[standardnamn]])
    
    standardnamn_lag <- stringr::str_to_lower(standardnamn)
    alternativnamn_lag <- stringr::str_to_lower(alternativnamn)
    
    query_standard_pos <- which(query_namn_lag == standardnamn_lag)
    tabell_standard_pos <- which(tabell_variabler_lag == standardnamn_lag)
    tabell_alternativ_pos <- which(tabell_variabler_lag == alternativnamn_lag)
    
    if (
      length(query_standard_pos) > 0 &&
      length(tabell_standard_pos) == 0 &&
      length(tabell_alternativ_pos) > 0
    ) {
      names(query)[query_standard_pos] <- tabell_variabler[tabell_alternativ_pos[[1]]]
    }
  }
  
  query
} # slut funktion intern_pxweb2_harmonisera_querynamn


intern_pxweb2_rensa_kod_i_label <- function(label, kod) {
  label_chr <- as.character(label)
  kod_chr <- as.character(kod)
  
  # Om kodkolumnen råkar innehålla "2082 Säter" i stället för bara "2082",
  # plocka ut första token som möjlig kod.
  kod_candidate <- stringr::str_extract(kod_chr, "^\\S+")
  
  # Rensa bara om kod_candidate faktiskt ser kodlik ut, dvs innehåller minst en siffra.
  # Detta förhindrar att "Stockholms län" blir "län" i tabeller där labeln redan är ren.
  kod_ser_ut_som_kod <- stringr::str_detect(kod_candidate, "\\d")
  
  rensad <- dplyr::if_else(
    is.na(label_chr) | is.na(kod_candidate) | !kod_ser_ut_som_kod,
    label_chr,
    stringr::str_remove(
      label_chr,
      paste0("^", stringr::str_escape(kod_candidate), "\\s+")
    )
  )
  
  rensad <- stringr::str_trim(rensad)
  
  # Om label bara var kod, t.ex. "0114A0010", behåll originalet
  dplyr::if_else(
    is.na(rensad) | rensad == "",
    label_chr,
    rensad
  )
}


# Hjälpfunktion: harmonisera resultatnamn
# Den här döper om resultatkolumner efter hämtning, t.ex.: Kommun     -> Region och kommun_kod -> region_kod
intern_pxweb2_harmonisera_resultatnamn <- function(
    df,
    variabelnamn_harmonisera = NULL
) {
  
  if (is.null(df) || is.null(variabelnamn_harmonisera)) {
    return(df)
  }
  
  if (is.null(names(variabelnamn_harmonisera))) {
    stop(
      "variabelnamn_harmonisera måste vara en namngiven vektor, t.ex. c(\"Region\" = \"Kommun\").",
      call. = FALSE
    )
  }
  
  for (standardnamn in names(variabelnamn_harmonisera)) {
    
    alternativnamn <- unname(variabelnamn_harmonisera[[standardnamn]])
    
    standardnamn_lag <- stringr::str_to_lower(standardnamn)
    alternativnamn_lag <- stringr::str_to_lower(alternativnamn)
    
    namn_lag <- stringr::str_to_lower(names(df))
    
    # Klartextkolumn, t.ex. kommun -> region
    alternativ_pos <- which(namn_lag == alternativnamn_lag)
    standard_pos <- which(namn_lag == standardnamn_lag)
    
    if (length(alternativ_pos) > 0 && length(standard_pos) == 0) {
      names(df)[alternativ_pos] <- standardnamn_lag
    }
    
    # Kodkolumn, t.ex. kommun_kod -> region_kod
    alternativ_kod <- paste0(alternativnamn_lag, "_kod")
    standard_kod <- paste0(standardnamn_lag, "_kod")
    
    namn_lag <- stringr::str_to_lower(names(df))
    
    alternativ_kod_pos <- which(namn_lag == alternativ_kod)
    standard_kod_pos <- which(namn_lag == standard_kod)
    
    if (length(alternativ_kod_pos) > 0 && length(standard_kod_pos) == 0) {
      names(df)[alternativ_kod_pos] <- standard_kod
    }
  }
  
  df
} # slut funktion intern_pxweb2_harmonisera_resultatnamn

intern_pxweb2_resolve_api_wildcards <- function(query_list,
                                                giltiga_varden_list,
                                                allow_api_wildcards = TRUE) {
  
  if (!isTRUE(allow_api_wildcards)) return(query_list)
  
  wildcard_to_regex <- function(x) {
    x <- stringr::str_replace_all(x, "([\\.\\+\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1")
    x <- stringr::str_replace_all(x, "\\*", ".*")
    x <- stringr::str_replace_all(x, "\\?", ".")
    paste0("^", x, "$")
  }
  
  query_list$selection <- purrr::map(query_list$selection, function(s) {
    
    var <- s$variableCode
    vals <- unlist(s$valueCodes, use.names = FALSE)
    
    if (length(vals) == 0 || is.null(vals)) return(s)
    
    ok_tbl <- giltiga_varden_list[[var]]
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) return(s)
    
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }
    
    ok_codes <- ok_tbl$code
    
    vals_expanded <- purrr::map(vals, function(v) {
      
      # Lämna hel wildcard, top/bottom och aggregationsspecialer ifred
      if (identical(v, "*") ||
          identical(v, "**") ||
          stringr::str_detect(v, "^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$") ||
          stringr::str_detect(v, "^agg_")) {
        return(v)
      }
      
      # Expandera bara om värdet innehåller * eller ?
      if (stringr::str_detect(v, "[\\*\\?]")) {
        pattern <- wildcard_to_regex(v)
        hits <- ok_codes[stringr::str_detect(ok_codes, pattern)]
        
        if (length(hits) == 0) {
          return(v)
        }
        
        return(hits)
      }
      
      v
    }) |>
      unlist(use.names = FALSE) |>
      unique()
    
    s$valueCodes <- as.list(vals_expanded)
    s
  })
  
  query_list
}


intern_pxweb2_resolve_senaste_tid <- function(query_list,
                                              variabler_df,
                                              giltiga_varden_list,
                                              senaste_tid_kod = "9999") {
  
  if (is.null(senaste_tid_kod)) return(query_list)
  
  time_vars <- variabler_df |>
    dplyr::filter(role == "time") |>
    dplyr::pull(code)
  
  if (length(time_vars) == 0) return(query_list)
  
  query_list$selection <- purrr::map(query_list$selection, function(s) {
    
    var <- s$variableCode
    
    if (!var %in% time_vars) return(s)
    
    vals <- unlist(s$valueCodes, use.names = FALSE)
    
    if (!senaste_tid_kod %in% vals) return(s)
    
    ok_tbl <- giltiga_varden_list[[var]]
    
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) {
      stop("Kan inte hitta giltiga tidsvärden för variabeln: ", var)
    }
    
    # Använd bara ordinarie variabelvärden, inte aggregationsrader
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }
    
    senaste_varde <- ok_tbl |>
      dplyr::pull(code) |>
      dplyr::last()
    
    if (is.na(senaste_varde) || length(senaste_varde) == 0) {
      stop("Kan inte avgöra senaste tidsvärde för variabeln: ", var)
    }
    
    vals <- dplyr::if_else(vals == senaste_tid_kod, senaste_varde, vals)
    
    s$valueCodes <- as.list(vals)
    s
  })
  
  query_list
}

intern_pxweb2_sanitize_query_values <- function(query, giltiga_varden_list, 
                                                query_om_enbart_ogiltiga_varden_for_variabel = "stop",
                                                warn = TRUE) {
  if (!query_om_enbart_ogiltiga_varden_for_variabel %in% c("stop", "*", "null")) {
    stop("Ogiltigt värde för query_om_enbart_ogiltiga_varden_for_variabel. Tillåtna: \"stop\", \"*\", \"null\".")
  }
  
  removed <- list()
  
  query$selection <- purrr::map(query$selection, function(s) {
    var  <- s$variableCode
    vals <- unlist(s$valueCodes, use.names = FALSE)
    
    # Lämna specialuttryck ifred
    if (length(vals) == 1 && (identical(vals, "*") ||
                              grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE))) {
      return(s)
    }
    
    ok_tbl <- giltiga_varden_list[[var]]
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) return(s)
    
    ok <- ok_tbl$code
    keep <- vals[vals %in% ok]
    bad  <- setdiff(vals, keep)
    
    if (length(bad) > 0) removed[[var]] <<- unique(c(removed[[var]], bad))
    
    # Om allt var ogiltigt: stoppa eller ersätt med "*"
    if (length(keep) == 0) {
      
      if (query_om_enbart_ogiltiga_varden_for_variabel == "stop") {
        stop(
          "Ogiltiga värden i query för variabeln '", var, "': ",
          paste(vals, collapse = ", "),
          ". Körningen stoppas enligt parametern ",
          "'query_om_enbart_ogiltiga_varden_for_variabel = \"stop\"'."
        )
      }
      
      if (query_om_enbart_ogiltiga_varden_for_variabel == "*") {
        if (warn) {
          cat(
            "Ogiltiga värden i query för variabeln '", var,
            "', samtliga värden tas med då ",
            "'query_om_enbart_ogiltiga_varden_for_variabel = \"*\"'.\n",
            sep = ""
          )
        }
        s$valueCodes <- list("*")
        return(s)
      }
      
      if (query_om_enbart_ogiltiga_varden_for_variabel == "null") {
        if (warn) {
          cat(
            "Ogiltiga värden i query för variabeln '", var,
            "', variabeln utesluts eftersom ",
            "'query_om_enbart_ogiltiga_varden_for_variabel = \"null\"'.\n",
            sep = ""
          )
        }
        s$valueCodes <- NULL
        return(s)
      }
    }
    
    s$valueCodes <- as.list(keep)
    s
  })
  
  if (warn && length(removed) > 0) {
    msg <- paste(
      purrr::imap_chr(removed, ~ paste0(.y, ": ", paste(.x, collapse = ", "))),
      collapse = " | "
    )
    cat(paste0("Följande värden finns inte i tabellen och togs därför bort:\n", msg, "\n"))
  }
  
  return(query)
}


intern_pxweb2_rate_limiter <- local({
  times <- numeric(0)              # tidpunkter (sek) för senaste anrop
  max_calls <- 30L
  window <- 10                      # sekunder
  safety <- 0.05                    # liten marginal (50 ms)
  
  function() {
    now <- as.numeric(Sys.time())
    
    # kasta bort anrop äldre än window
    times <<- times[now - times < window]
    
    # om vi redan har max_calls i fönstret: vänta tills det första faller ur
    if (length(times) >= max_calls) {
      wait <- window - (now - times[1]) + safety
      if (wait > 0) Sys.sleep(wait)
      now <- as.numeric(Sys.time())
      times <<- times[now - times < window]
    }
    
    # registrera att vi gör ett anrop nu
    times <<- c(times, now)
    invisible(NULL)
  }
})


.pxweb2_api_log <- new.env(parent = emptyenv())

.pxweb2_api_log$rows <- list()

intern_pxweb2_api_log_reset <- function() {
  .pxweb2_api_log$rows <- list()
  invisible(NULL)
}

intern_pxweb2_api_log_get <- function() {
  if (length(.pxweb2_api_log$rows) == 0) {
    return(tibble::tibble(
      time = as.POSIXct(character()),
      method = character(),
      url = character(),
      status = integer(),
      attempt = integer(),
      endpoint_type = character()
    ))
  }
  tibble::as_tibble(do.call(rbind, lapply(.pxweb2_api_log$rows, as.data.frame)))
}

intern_pxweb2_api_endpoint_type <- function(url) {
  url_chr <- as.character(url)
  
  if (grepl("/metadata", url_chr, fixed = TRUE)) {
    return("metadata")
  }
  
  if (grepl("/data", url_chr, fixed = TRUE)) {
    return("data")
  }
  
  if (grepl("/tables", url_chr, fixed = TRUE)) {
    return("tables")
  }
  
  if (grepl("/codelists", url_chr, fixed = TRUE) ||
      grepl("codelist", url_chr, ignore.case = TRUE)) {
    return("codelist")
  }
  
  "annat"
}

intern_pxweb2_api_log_add <- function(method, url, status, attempt) {
  .pxweb2_api_log$rows <- c(
    .pxweb2_api_log$rows,
    list(list(
      time = Sys.time(),
      method = as.character(method),
      url = as.character(url),
      status = as.integer(status),
      attempt = as.integer(attempt),
      endpoint_type = intern_pxweb2_api_endpoint_type(url)
    ))
  )
  
  invisible(NULL)
}

intern_pxweb2_GET <- function(url, ..., max_tries = 3, retry_wait_default = 10) {
  
  resp <- NULL
  
  for (attempt in seq_len(max_tries)) {
    
    intern_pxweb2_rate_limiter()
    
    resp <- httr::GET(url, ...)
    
    intern_pxweb2_api_log_add(
      method = "GET",
      url = url,
      status = httr::status_code(resp),
      attempt = attempt
    )
    
    if (httr::status_code(resp) != 429) {
      return(resp)
    }
    
    retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
    wait <- ifelse(is.na(retry_after), retry_wait_default, retry_after)
    Sys.sleep(wait + 0.2)
  }
  
  resp
}

intern_pxweb2_POST <- function(url, ..., max_tries = 3, retry_wait_default = 10) {
  
  resp <- NULL
  
  for (attempt in seq_len(max_tries)) {
    
    intern_pxweb2_rate_limiter()
    
    resp <- httr::POST(url, ...)
    
    intern_pxweb2_api_log_add(
      method = "POST",
      url = url,
      status = httr::status_code(resp),
      attempt = attempt
    )
    
    if (httr::status_code(resp) != 429) {
      return(resp)
    }
    
    retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
    wait <- ifelse(is.na(retry_after), retry_wait_default, retry_after)
    Sys.sleep(wait + 0.2)
  }
  
  resp
}


intern_pxweb2_make_chunks <- function(variabler_df, query, giltiga_varden_list,
                                      max_cells = 150000) {
  total_cells <- intern_pxweb2_count_cells(variabler_df, query)
  
  if (total_cells <= max_cells) {
    return(list(query))
  }
  
  split_var <- intern_pxweb2_choose_split_variable(variabler_df, query)
  
  selected_vals <- intern_pxweb2_get_valuecodes(query, split_var)
  
  if (is.null(selected_vals) || length(selected_vals) == 0 ||
      (length(selected_vals) == 1 && identical(selected_vals, "*"))) {
    
    ok_tbl <- giltiga_varden_list[[split_var]]
    
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }
    
    all_vals <- ok_tbl |>
      dplyr::pull(code)
    
  } else {
    
    all_vals <- selected_vals
  }
  
  
  # Greedy-packning (purrr::accumulate): fyll så nära max_cells som möjligt
  state <- purrr::accumulate(
    all_vals,
    .init = list(
      cur_vals = character(),
      cur_cells = 0L,
      chunks = list()
    ),
    .f = function(st, v) {
      # kostnaden för att lägga till v (inkl. andra dimensioner)
      cost <- intern_pxweb2_count_cells(
        variabler_df,
        intern_pxweb2_set_valuecodes_in_query(query, split_var, v)
      )
      
      # om v ensam är större än max_cells (osannolikt men skydda)
      if (cost > max_cells) {
        if (length(st$cur_vals) > 0) {
          st$chunks <- append(st$chunks, list(st$cur_vals))
          st$cur_vals <- character()
          st$cur_cells <- 0L
        }
        st$chunks <- append(st$chunks, list(v))
        return(st)
      }
      
      # om det inte ryms i nuvarande chunk: stäng chunk och starta ny
      if (st$cur_cells + cost > max_cells && length(st$cur_vals) > 0) {
        st$chunks <- append(st$chunks, list(st$cur_vals))
        st$cur_vals <- v
        st$cur_cells <- cost
        return(st)
      }
      
      # annars lägg till i nuvarande chunk
      st$cur_vals <- c(st$cur_vals, v)
      st$cur_cells <- st$cur_cells + cost
      st
    }
  ) 
  
  state <- state[[length(state)]]
  
  # lägg till sista chunk om den finns
  if (length(state$cur_vals) > 0) {
    state$chunks <- append(state$chunks, list(state$cur_vals))
  }
  
  purrr::map(state$chunks, ~ intern_pxweb2_set_valuecodes_in_query(query, split_var, .x))
}


intern_pxweb2_set_valuecodes_in_query <- function(query, variable, values) {
  idx <- purrr::detect_index(
    query$selection,
    ~ identical(.x$variableCode, variable)
  )
  
  if (idx == 0L) {
    stop("Variabel saknas i query: ", variable)
  }
  
  query$selection[[idx]]$valueCodes <- as.list(values)
  query
}


intern_pxweb2_choose_split_variable <- function(variabler_df, query = list()) {
  # Välj variabel att splitta på om det är fler än 150000 rader i ett uttag  
  
  candidates <- variabler_df |>
    dplyr::filter(
      role != "time",
      role != "contents"
    )
  
  if (nrow(candidates) == 0) {
    stop("Ingen lämplig dimension att splitta på")
  }
  
  # prioritera geo, annars störst
  candidates |>
    dplyr::mutate(priority = ifelse(role == "geo", 2, 1)) |>
    dplyr::arrange(desc(priority), desc(size)) |>
    dplyr::slice(1) |>
    dplyr::pull(code)
}

intern_pxweb2_list_to_query_list <- function(variabler_df,
                                             query = list(),
                                             giltiga_varden_list, 
                                             default_value = "*",
                                             allow_label_values = TRUE,
                                             warn = TRUE) {
  stopifnot(is.data.frame(variabler_df))
  
  if (!all(c("code", "label", "elimination") %in% names(variabler_df))) {
    stop("variabler_df måste minst ha kolumnerna: code, label, elimination")
  }
  
  # Normalisera: tillåt Civilstand men tabellen kan heta Civilstånd
  # (vi matchar exakt på code; om du vill ha "fuzzy" matchning kan vi lägga till det senare)
  valid_codes <- variabler_df$code
  elim_map <- stats::setNames(as.logical(variabler_df$elimination), variabler_df$code)
  
  # tillåt både code och label (case-insensitivt) i query-namn ---
  key_to_code <- c(
    stats::setNames(variabler_df$code, tolower(variabler_df$code)),
    stats::setNames(variabler_df$code, tolower(variabler_df$label))
  )
  
  # Normalisera query: mappar namn (code/label) -> code
  q_names <- names(query)
  q_keys  <- tolower(q_names)
  
  mapped_codes <- unname(key_to_code[q_keys])
  
  # okända nycklar = de som inte gick att mappa
  unknown <- q_names[is.na(mapped_codes)]
  
  # bygg normaliserad query med code som namn
  query_norm <- query[!is.na(mapped_codes)]
  names(query_norm) <- mapped_codes[!is.na(mapped_codes)]
  
  # --- Tillåt klartext som valueCodes (endast om de inte redan är giltiga koder) ---
  if (isTRUE(allow_label_values) && !is.null(giltiga_varden_list) && length(query_norm) > 0) {
    query_norm <- purrr::imap(query_norm, function(val, var) {
      # lämna specialfall ifred
      if (is.null(val) || (length(val) == 1 && (is.na(val) || identical(val, "*"))) ||
          (is.character(val) && length(val) == 1 &&
           grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", val, ignore.case = TRUE))) {
        return(val)
      }
      
      ok_tbl <- giltiga_varden_list[[var]]
      if (is.null(ok_tbl) || !all(c("code", "label") %in% names(ok_tbl))) return(val)
      
      codes <- ok_tbl$code
      lbl_map <- stats::setNames(ok_tbl$code, tolower(ok_tbl$label))
      
      v <- as.character(val)
      v2 <- purrr::map_chr(v, function(x) {
        #if (x %in% codes) x else (lbl_map[[tolower(x)]] %||% x)
        if (x %in% codes) x else {
          hit <- unname(lbl_map[tolower(x)])
          if (length(hit) == 0 || is.na(hit) || identical(hit, character(0))) x else hit
        }
        
      })
      v2 <- unique(v2)
      return(v2)
    })
  }
  
  
  # varna om samma code angivits flera gånger (via både code och label)
  dup_codes <- names(query_norm)[duplicated(names(query_norm))]
  if (warn && length(dup_codes) > 0) {
    warning("Samma variabel angavs flera gånger (via code/label). Sista vinner: ",
            paste(unique(dup_codes), collapse = ", "))
  }
  query_norm <- query_norm[!duplicated(names(query_norm), fromLast = TRUE)]
  
  # Hjälp: om användaren skickar okända variabler -> varna men ignorera
  if (warn && length(unknown) > 0) {
    warning("Okända variabler i `query` ignoreras: ", paste(unknown, collapse = ", "))
  }
  
  # Bygg selection, men:
  # - default "*" för alla variabler som finns i tabellen
  # - om query[[var]] är NA -> utelämna variabeln (om eliminerbar; annars varna)
  selection <- purrr::map(valid_codes, function(var) {
    val <- if (var %in% names(query_norm)) query_norm[[var]] else default_value
    
    # NA => utelämna (dvs returnera NULL så vi kan purrr::compact() senare)
    if (length(val) == 1 && is.na(val)) {
      if (warn && !isTRUE(elim_map[[var]])) {
        warning(
          "Variabeln `", var, "` satt till NA (utlämnas), men är inte eliminerbar enligt metadata. ",
          "Det kan innebära att API:t fortfarande kräver val för denna dimension."
        )
      }
      return(NULL)
    }
    
    # Tillåt att användaren skriver "*" eller TOP(1)/Top(1)/top(1) etc.
    # valueCodes ska alltid bli en list(...) för JSON
    if (identical(val, "*")) {
      vc <- list("*")
    } else if (is.character(val) && length(val) == 1) {
      vc <- list(val)
    } else {
      # vektor (t.ex. c("20","21","17")) -> list("20","21","17")
      vc <- as.list(val)
    }
    
    list(
      variableCode = var,
      valueCodes = vc
    )
  }) |>
    purrr::compact()
  
  list(selection = selection)
} 


intern_pxweb2_create_variable_query_list <- function(var_df,
                                                     default_value = "*",
                                                     overrides = list()) {
  selections <- purrr::map(
    var_df$code,
    function(var) {
      v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
      
      list(
        variableCode = var,
        valueCodes = if (length(v) == 1 && is.na(v)) {
          list()                       # tom = tas bort i nästa steg
        } else if (identical(v, "*") || (is.character(v) && length(v) == 1)) {
          list(v)                      # t.ex. "*", "top(4)"
        } else {
          as.list(as.character(v))     # t.ex. c("20","21","17") -> list("20","21","17")
        }
      )
    }
  ) |>
    purrr::keep(~ length(.x$valueCodes) > 0)  # droppa de som fick NA
  
  list(selection = selections)
}



pxweb2_meta <- function(
    table_id = NULL,
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
){
  if (is.null(table_id)) stop("table_id måste anges")
  
  meta_url <- paste0(base_url, table_id, "/metadata")
  
  resp <- intern_pxweb2_GET(meta_url, httr::accept_json())
  httr::stop_for_status(resp)
  
  meta <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  
  return(meta)
} 

intern_pxweb2_is_pxweb_query_list <- function(x) {
  is.list(x) &&
    !is.null(x$selection) &&
    is.list(x$selection) &&
    all(purrr::map_lgl(
      x$selection,
      ~ is.list(.x) && !is.null(.x$variableCode) && !is.null(.x$valueCodes)
    ))
}

intern_pxweb2_count_cells <- function(variabler_df, query = list()) {
  
  # Tom query => välj alla värden (dvs. samma som "*" för alla dimensioner)
  if (length(query) == 0) {
    return(prod(as.integer(variabler_df$size)))
  }
  
  if (!intern_pxweb2_is_pxweb_query_list(query)) {
    stop("`query` måste vara en PxWeb query_list: list(selection = list(list(variableCode=..., valueCodes=list(...)), ...))")
  }
  
  if (!is.data.frame(variabler_df) | !all(c("code", "label") %in% names(variabler_df))) {
    stop("variabler_df måste vara en dataframe med tabellens variabler, som kan hämtas med pxweb2_variabler()-funktionen.")
  }
  
  # named map: selection_map[["Region"]] = c("20","21"), osv.
  selection_map <- purrr::map(
    query$selection,
    ~{
      var  <- .x$variableCode
      vals <- unlist(.x$valueCodes, use.names = FALSE)
      stats::setNames(list(vals), var)
    }
  ) |>
    purrr::flatten()
  
  sizes <- purrr::map_int(variabler_df$code, function(id) {
    
    # Om variabeln finns i queryn men har NA => "vald bort" => cellbidrag = 1
    if (!is.null(selection_map[[id]]) &&
        length(selection_map[[id]]) == 1 &&
        is.na(selection_map[[id]][1])) {
      return(1L)
    }
    
    # Om variabeln finns i queryn med värden
    if (!is.null(selection_map[[id]])) {
      vals <- selection_map[[id]]
      
      # wildcard => full storlek för just den dimensionen
      if (length(vals) == 1 && identical(vals, "*")) {
        return(as.integer(variabler_df$size[variabler_df$code == id][1]))
      }
      
      # top(n)/bottom(n)
      if (length(vals) == 1 &&
          is.character(vals) &&
          grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE)) {
        n <- as.integer(gsub(".*\\(\\s*(\\d+)\\s*\\).*", "\\1", vals, perl = TRUE))
        return(n)
      }
      
      # explicita koder
      return(length(vals))
    }
    
    # Om variabeln saknas i query => anta full storlek
    as.integer(variabler_df$size[variabler_df$code == id][1])
  })
  
  prod(sizes)
}


pxweb2_variabler <- function(
    tabell = NULL,             # kan vara tabell-id eller meta-objekt
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {
  if (is.null(tabell)) stop("tabell måste anges, antingen som tabell-id eller som metadata-objekt.")
  
  if (!is.list(tabell)) {
    if (!stringr::str_detect(tabell, "^TAB\\d+$")) stop("tabell-id är inte giltigt, ska vara av typen 'TAB' följt av siffror.")    
    metadata <- pxweb2_meta(tabell)
  } else metadata <- tabell
  
  # här extraherar vi alla variabler med både kod och klartext samt hur många unika värden de har
  variabler <- tibble::tibble(
    code       = names(metadata$dimension),
    label      = purrr::map_chr(metadata$dimension, ~ .x$label %||% NA_character_),
    size = purrr::map_int(metadata$dimension, ~ length(.x$category$label %||% list())),
    elimination = purrr::map_lgl(metadata$id, ~ isTRUE(metadata$dimension[[.x]]$extension$elimination)),
    role = purrr::map_chr(metadata$id, ~ {
      if (!is.null(metadata$role$time) && .x %in% metadata$role$time) return("time")
      if (!is.null(metadata$role$metric) && .x %in% metadata$role$metric) return("contents")
      if (!is.null(metadata$role$geo) && .x %in% metadata$role$geo) return("geo")
      "other"
    }),
    show = purrr::map_chr(metadata$dimension, ~ .x$extension$show %||% NA_character_)
  )
  
  variabler <- variabler |> 
    dplyr::mutate(role = dplyr::if_else(tolower(code) %in% c("region"), "geo", role))
  
  return(variabler)
}

# hämta värden för en pxweb-tabell
pxweb2_varden <- function(
    tabell,                              # skicka med metadataobjekt som man får genom att köra pxweb2_meta()
    variabler = NULL,                    # skicka med variabelnamn om man inte vill ha värden för alla variabler
    return_df_if_only_one_variable = TRUE,
    include_aggregations = "auto",       # "auto"      = hämta allt om totalt antal kodlistor <= auto_limit, annars "codelists"
    # "all"       = hämta alltid alla aggregeringar fullt ut
    # "none"      = ingenting, inte ens kodlistemetadata
    # "codelists" = bara kodlistemetadata (id + label, typ "AggregationCodelist"), inga medlemmar
    # namnsatt vektor, t.ex. c(Region = "agg_RegionLA2018", Alder = "all", Tid = "codelists")
    #   => selektiv styrning per variabel; namn kan vara kod eller klartext (skiftlägesokänsligt)
    #   => värde per variabel: "all", "codelists", eller ett/flera agg-id:n (t.ex. "agg_RegionLA2018")
    auto_limit = 30L,                    # max antal kodliste-anrop vid "auto"
    aggregation_members_separation = ";",# aggregation members are separated with this character(s)
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {
  
  if (is.null(tabell)) stop("tabell måste anges, antingen som tabell-id eller som metadata-objekt.")
  if (!is.list(tabell)) {
    if (!stringr::str_detect(tabell, "^TAB\\d+$")) stop("tabell-id är inte giltigt, ska vara av typen 'TAB' följt av siffror.")
    metadata <- pxweb2_meta(tabell)
  } else metadata <- tabell
  
  # Alla variabler: kod + klartext
  var_df <- tibble::tibble(
    code  = names(metadata$dimension),
    label = purrr::map_chr(metadata$dimension, ~ .x$label %||% NA_character_)
  )
  
  tabell_variabler <- metadata$dimension
  
  # Filtrera variabler om variabler-parametern är satt
  if (!is.null(variabler)) {
    var_finns_ej <- variabler[!tolower(variabler) %in% tolower(var_df$code) &
                                !tolower(variabler) %in% tolower(var_df$label)]
    var_finns <- variabler[tolower(variabler) %in% tolower(var_df$code) |
                             tolower(variabler) %in% tolower(var_df$label)]
    if (length(var_finns_ej) > 0) {
      cat(paste0("Variablerna ", paste0(var_finns_ej, collapse = ", "), " finns inte i tabellen och utelämnas därmed."))
    }
    if (length(var_finns) > 0) {
      tabell_variabler <- tabell_variabler[
        tolower(names(tabell_variabler)) %in% tolower(var_finns) |
          tolower(purrr::map_chr(tabell_variabler, "label")) %in% tolower(var_finns)
      ]
    }
  }
  
  # ---------------------------------------------------------------------------
  # Hjälp: normalisera variabelnamn/koder -> kod
  intern_till_var_kod <- function(namn) {
    hits <- var_df$code[
      tolower(var_df$code)  %in% tolower(namn) |
        tolower(var_df$label) %in% tolower(namn)
    ]
    hits
  }
  
  # Hjälp: extrahera kodlisteinfo (id + label + url) från ett dimension-element
  intern_cl_info <- function(dim_el) {
    cl <- purrr::pluck(dim_el, "extension", "codelists", .default = NULL)
    if (is.null(cl) || length(cl) == 0) {
      return(tibble::tibble(code = character(), label = character(),
                            type = character(), url = character()))
    }
    purrr::map_df(cl, function(item) {
      lbl <- purrr::pluck(item, "label", .default = NA)
      if (is.list(lbl)) {
        lbl <- purrr::pluck(lbl, "sv",
                            .default = (unlist(lbl, use.names = FALSE)[1] %||% NA_character_))
      }
      tibble::tibble(
        code  = purrr::pluck(item, "id",    .default = NA_character_),
        label = lbl %||% NA_character_,
        type  = purrr::pluck(item, "type",  .default = NA_character_),
        url   = purrr::pluck(item, "links", 1, "href", .default = NA_character_)
      )
    }) |>
      dplyr::filter(tolower(type) == "aggregation")
  }
  
  # ---------------------------------------------------------------------------
  # Räkna totalt antal kodlistor i tabellen (för "auto"-läget)
  totalt_antal_kodlistor <- sum(purrr::map_int(tabell_variabler, function(dim_el) {
    nrow(intern_cl_info(dim_el))
  }))
  
  # ---------------------------------------------------------------------------
  # Tolka include_aggregations -> per-variabel instruktion
  # Resultat: en namnsatt lista  var_kod -> läge
  #   läge är ett av: "all", "codelists", "none", eller en character-vektor med agg-id:n
  #
  ia <- include_aggregations
  
  # Bakåtkompatibilitet: TRUE -> "all", FALSE -> "none"
  if (isTRUE(ia))        ia <- "all"
  if (identical(ia, FALSE)) ia <- "none"
  
  # Validera skalära strängvärden
  if (is.character(ia) && is.null(names(ia)) && length(ia) == 1) {
    if (!ia %in% c("auto", "all", "none", "codelists")) {
      stop('include_aggregations: ogiltigt värde "', ia,
           '". Tillåtna: "auto", "all", "none", "codelists", eller en namnsatt vektor.', call. = FALSE)
    }
  }
  
  # Lös upp "auto": välj "all" eller "codelists" beroende på totalt antal kodlistor
  auto_valde_all <- FALSE
  if (identical(ia, "auto")) {
    if (totalt_antal_kodlistor <= auto_limit) {
      ia <- "all"
      auto_valde_all <- TRUE
    } else {
      ia <- "codelists"
    }
  }
  
  # Bygg per-variabel instruktionsvektor
  # var_lage: namnsatt character-vektor  var_kod -> "all" | "codelists" | "none" | "agg_..."
  if (is.character(ia) && is.null(names(ia))) {
    # Skalärt läge ("all", "none", "codelists") -> gäller alla variabler
    skalart_lage <- ia
    var_lage <- stats::setNames(rep(skalart_lage, length(tabell_variabler)),
                                names(tabell_variabler))
  } else if (is.character(ia) && !is.null(names(ia))) {
    # Namnsatt vektor: c(Region = "agg_RegionLA2018", Alder = "all", ...)
    # Okända variabelnamn -> warning; ej nämnda variabler -> "codelists"
    var_lage <- stats::setNames(rep("codelists", length(tabell_variabler)),
                                names(tabell_variabler))
    
    ia_namn <- names(ia)
    okanda  <- ia_namn[
      !tolower(ia_namn) %in% tolower(var_df$code) &
        !tolower(ia_namn) %in% tolower(var_df$label)
    ]
    if (length(okanda) > 0) {
      warning("Följande variabelnamn i include_aggregations känns inte igen och ignoreras: ",
              paste(okanda, collapse = ", "), call. = FALSE)
    }
    
    # Grupp per variabelkod (kan ha flera poster med samma namn, t.ex. Region = "agg_X", Region = "agg_Y")
    for (i in seq_along(ia)) {
      vkoder <- intern_till_var_kod(ia_namn[i])
      if (length(vkoder) == 0) next
      val <- ia[[i]]
      
      for (vk in vkoder) {
        befintligt <- var_lage[[vk]]
        if (val %in% c("all", "none", "codelists")) {
          # Explicit läge: skriv alltid över
          var_lage[[vk]] <- val
        } else {
          # Specifikt agg-id: samla ihop (kan finnas flera poster)
          if (befintligt %in% c("codelists", "none")) {
            var_lage[[vk]] <- val          # första agg-id för denna variabel
          } else if (!befintligt %in% c("all")) {
            var_lage[[vk]] <- paste(c(befintligt, val), collapse = "\n")  # lägg till
          }
          # om befintligt == "all" -> lämna som "all"
        }
      }
    }
  } else {
    stop('include_aggregations måste vara "auto", "all", "none", "codelists" eller en namnsatt vektor.', call. = FALSE)
  }
  
  # ---------------------------------------------------------------------------
  # funktion för GET -> JSON med cache
  .codelist_cache <- new.env(parent = emptyenv())
  
  fetch_codelist <- purrr::possibly(function(u) {
    if (exists(u, envir = .codelist_cache, inherits = FALSE)) {
      return(get(u, envir = .codelist_cache, inherits = FALSE))
    }
    resp <- intern_pxweb2_GET(u, httr::accept_json())
    httr::stop_for_status(resp)
    out <- jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    assign(u, out, envir = .codelist_cache)
    out
  }, otherwise = NULL)
  
  # Hjälp: hämta fullständigt innehåll för ett urval kodlistor (filtrerat på agg-id:n om angett)
  intern_hamta_agg_varden <- function(cl_info_df, agg_ids_filter = NULL) {
    # agg_ids_filter: character-vektor med specifika agg-id:n att hämta, NULL = alla
    df <- cl_info_df
    if (!is.null(agg_ids_filter)) {
      df <- df |> dplyr::filter(tolower(code) %in% tolower(agg_ids_filter))
    }
    if (nrow(df) == 0) {
      return(tibble::tibble(code = character(), label = character(), type = character()))
    }
    
    aggr_parsed <- df |>
      dplyr::transmute(agg_id = code, agg_label = label, url) |>
      dplyr::mutate(
        codelist = purrr::map(url, fetch_codelist),
        values_long_df = purrr::map(codelist, ~ {
          v <- .x$values
          if (is.null(v) || length(v) == 0) {
            return(tibble::tibble(code = character(), label = character(), members = character()))
          }
          purrr::map_df(v, ~ tibble::tibble(
            code    = as.character(purrr::pluck(.x, "code",  .default = NA_character_)),
            label   = as.character(purrr::pluck(.x, "label", .default = NA_character_)),
            members = {
              vm <- purrr::pluck(.x, "valueMap", .default = NULL)
              if (is.null(vm)) NA_character_
              else paste(unlist(vm, use.names = FALSE), collapse = aggregation_members_separation)
            }
          )) |>
            dplyr::filter(!is.na(code))
        })
      ) |>
      dplyr::select(-codelist)
    
    if (nrow(aggr_parsed) == 0) {
      return(tibble::tibble(code = character(), label = character(), type = character()))
    }
    
    aggr_parsed |>
      dplyr::select(agg_id, agg_label, values_long_df) |>
      tidyr::unnest(values_long_df) |>
      dplyr::mutate(type = "Aggregation") |>
      dplyr::relocate(c(code, label, type), .before = 1)
  }
  
  # ---------------------------------------------------------------------------
  # Bygg listan: en tibble per dimension-element
  variabler_med_enbart_codelist <- character(0)
  
  varden <- purrr::imap(tabell_variabler, function(dim_el, dim_name) {
    
    # --- Variabelvärden (type = "Variable")
    cat_lab <- purrr::pluck(dim_el, "category", "label", .default = NULL)
    cat_df <- if (is.null(cat_lab)) {
      tibble::tibble(code = character(), label = character(), type = character())
    } else {
      v <- unlist(cat_lab, use.names = TRUE)
      tibble::tibble(code = names(v), label = unname(v), type = "Variable")
    }
    
    lage <- var_lage[[dim_name]] %||% "codelists"
    cl_info <- intern_cl_info(dim_el)
    
    if (lage == "none") {
      return(cat_df)
    }
    
    if (lage == "codelists") {
      if (nrow(cl_info) == 0) return(cat_df)
      variabler_med_enbart_codelist <<- c(variabler_med_enbart_codelist, dim_name)
      codelist_meta <- cl_info |>
        dplyr::select(code, label, type) |>
        dplyr::mutate(type = "AggregationCodelist")
      return(dplyr::bind_rows(cat_df, codelist_meta))
    }
    
    if (lage == "all") {
      if (nrow(cl_info) == 0) return(cat_df)
      return(dplyr::bind_rows(cat_df, intern_hamta_agg_varden(cl_info)))
    }
    
    # Specifika agg-id:n (en eller flera, newline-separerade internt)
    agg_ids <- unlist(stringr::str_split(lage, "\n"), use.names = FALSE)
    okanda_ids <- agg_ids[!tolower(agg_ids) %in% tolower(cl_info$code)]
    if (length(okanda_ids) > 0) {
      warning("Följande agg-id:n känns inte igen för variabeln '", dim_name, "' och ignoreras: ",
              paste(okanda_ids, collapse = ", "), call. = FALSE)
    }
    agg_ids <- agg_ids[tolower(agg_ids) %in% tolower(cl_info$code)]
    
    if (length(agg_ids) == 0) {
      # Inga giltiga agg-id:n -> falla tillbaka på codelist-metadata
      if (nrow(cl_info) > 0) {
        variabler_med_enbart_codelist <<- c(variabler_med_enbart_codelist, dim_name)
        codelist_meta <- cl_info |>
          dplyr::select(code, label, type) |>
          dplyr::mutate(type = "AggregationCodelist")
        return(dplyr::bind_rows(cat_df, codelist_meta))
      }
      return(cat_df)
    }
    
    dplyr::bind_rows(cat_df, intern_hamta_agg_varden(cl_info, agg_ids_filter = agg_ids))
  })
  
  # ---------------------------------------------------------------------------
  # Meddelanden
  if (auto_valde_all) {
    message(
      "include_aggregations = \"auto\": hittade ", totalt_antal_kodlistor,
      " kodlistor (<= ", auto_limit, ") och hämtade alla aggregeringar automatiskt."
    )
  } else if (length(variabler_med_enbart_codelist) > 0) {
    message(
      "Följande variabler har aggregeringskodlistor som inte hämtats fullt ut: ",
      paste(variabler_med_enbart_codelist, collapse = ", "), ".\n",
      "Sätt include_aggregations = \"all\" (eller ange variabelnamnen med specifika agg-id:n) ",
      "för att hämta alla aggregeringar och deras värden."
    )
  }
  
  if (return_df_if_only_one_variable && length(varden) == 1) {
    varden <- tibble::as_tibble(varden[[1]])
  }
  
  return(varden)
}




pxweb2_query_list_txt_create <- function(table_id,
                                         default_value = "*",
                                         overrides = list(),
                                         object_name = "query_list"
) {
  # create the text with a query list for a script based on the table sent to the function
  var_df <- pxweb2_variabler(tabell = table_id)
  vars <- var_df$code
  
  vals <- purrr::map_chr(vars, function(var) {
    v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
    
    if (length(v) == 1 && is.na(v)) return(NA_character_)
    
    if (is.character(v) && length(v) == 1) {
      paste0(var, ' = "', v, '"')
    } else {
      paste0(
        var, " = c(",
        paste(sprintf('"%s"', as.character(v)), collapse = ", "),
        ")"
      )
    }
  })
  
  vals <- vals[!is.na(vals)]
  
  cat(
    paste0(
      object_name, " <- list(\n  ",
      paste(vals, collapse = ",\n  "),
      "\n)"
    )
  )
}

pxweb2_get_data_script_create <- function(table_id,
                                          default_value = "*",
                                          overrides = list(),
                                          to_clipboard = TRUE
) {
  
  meta <- pxweb2_meta(table_id)
  title_txt <- meta$label
  var_df <- pxweb2_variabler(meta)
  varden_df <- pxweb2_varden(meta)
  vars <- var_df$code
  
  vals <- purrr::map_chr(vars, function(var) {
    v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
    
    if (length(v) == 1 && is.na(v)) return(NA_character_)
    
    if (is.character(v) && length(v) == 1) {
      paste0(var, ' = "', v, '"')
    } else {
      paste0(
        var, " = c(",
        paste(sprintf('"%s"', as.character(v)), collapse = ", "),
        ")"
      )
    }
  })
  
  vals <- vals[!is.na(vals)]
  
  retur_txt <- paste0(
    "# ", title_txt, "\n",
    "dataset_df <- pxweb2_hamta_data(\n", 
    '\ttabell = "', table_id, '",\n', 
    "\tquery = list(\n\t\t",
    paste(vals, collapse = ",\n\t\t"),
    "\n\t\t))"
  )
  
  if (to_clipboard) {
    writeLines(text = retur_txt, con = "clipboard", sep = "")
  }
  
  cat(retur_txt)
}


pxweb2_search_tables <- function(query = NULL,
                                 base_url = "https://statistikdatabasen.scb.se/api/v2/tables",
                                 lang = "sv",
                                 pastDays = NULL,
                                 includeDiscontinued = NULL,
                                 pageSize = 200,
                                 timeout_sec = 60,
                                 max_pages = Inf) {
  
  base_url <- sub("/+$", "", base_url)
  
  qs_base <- purrr::compact(list(
    lang = lang,
    query = query,
    pastDays = pastDays,
    includeDiscontinued = includeDiscontinued,
    pageSize = pageSize
  ))
  
  fetch_page <- function(pageNumber) {
    resp <- intern_pxweb2_GET(
      url = base_url,
      query = c(qs_base, list(pageNumber = pageNumber)),
      httr::accept_json(),
      httr::timeout(timeout_sec)
    )
    httr::stop_for_status(resp)
    
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    jsonlite::fromJSON(txt, simplifyVector = TRUE)
  }
  
  out1 <- fetch_page(1)
  
  # Plocka tabeller oavsett om svaret ligger i $tables eller är "direkt"
  extract_tables <- function(out) {
    if (is.data.frame(out)) return(tibble::as_tibble(out))
    if (is.list(out) && "tables" %in% names(out)) {
      if (is.data.frame(out$tables)) return(tibble::as_tibble(out$tables))
      if (is.list(out$tables)) return(tibble::as_tibble(out$tables))
    }
    if (is.list(out)) return(tibble::as_tibble(out))
    tibble::tibble()
  }
  
  page1 <- extract_tables(out1)
  
  # Avgör om det finns fler sidor: om vi får < pageSize så är vi klara
  if (nrow(page1) == 0) return(page1)
  
  pages <- list(page1)
  page <- 2
  
  while (page <= max_pages) {
    if (nrow(pages[[length(pages)]]) < pageSize) break
    
    outn <- fetch_page(page)
    pgn <- extract_tables(outn)
    
    if (nrow(pgn) == 0) break
    pages[[length(pages) + 1]] <- pgn
    
    if (nrow(pgn) < pageSize) break
    page <- page + 1
  }
  
  dplyr::bind_rows(pages) |> dplyr::distinct()
}

# function helpers to create queries for aggregations as well
intern_pxweb2_request <- function(body, extra_query = list(), tag = list()) {
  list(body = body, extra_query = extra_query, tag = tag)
}

intern_pxweb2_get_valuecodes <- function(query_list, var) {
  i <- which(purrr::map_chr(query_list$selection, "variableCode") == var)
  if (length(i) == 0) return(NULL)
  unlist(query_list$selection[[i]]$valueCodes, use.names = FALSE)
}

intern_pxweb2_set_valuecodes <- function(query_list, var, vals) {
  i <- which(purrr::map_chr(query_list$selection, "variableCode") == var)
  if (length(i) == 0) return(query_list)
  
  query_list$selection[[i]]$valueCodes <- if (length(vals) == 1 && is.na(vals)) {
    NULL
  } else if (length(vals) == 0) {
    list()
  } else if (length(vals) == 1 && identical(vals, "*")) {
    list("*")
  } else if (is.character(vals) && length(vals) == 1) {
    list(vals)
  } else {
    as.list(as.character(vals))
  }
  
  query_list
}


intern_pxweb2_make_request_chunks <- function(variabler_df, request_list, giltiga_varden_list, max_cells = 150000) {
  
  request_list <- purrr::compact(request_list)
  
  request_list |>
    purrr::map(function(.req) {
      bodies <- intern_pxweb2_make_chunks(
        variabler_df,
        .req$body,
        giltiga_varden_list = giltiga_varden_list,
        max_cells = max_cells
      )
      bodies <- purrr::compact(bodies)
      
      purrr::map(bodies, ~ intern_pxweb2_request(
        body = .x,
        extra_query = .req$extra_query,
        tag = .req$tag
      ))
    }) |>
    purrr::flatten()
}

intern_pxweb2_is_special_value <- function(vals) {
  length(vals) == 1 && (
    identical(vals, "*") ||
      identical(vals, "**") ||
      grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE) ||
      grepl("^agg_", vals)
  )
}

intern_pxweb2_var_alternatives <- function(query_list, 
                                           giltiga_varden_list, 
                                           var,
                                           output_values = "aggregated",    # kan också vara single
                                           warn = TRUE) {
  
  vals <- intern_pxweb2_get_valuecodes(query_list, var)
  if (is.null(vals) || length(vals) == 0) {
    return(list(list(var = var, vals = NULL, extra_query = list())))
  }
  
  vals <- as.character(vals)
  
  # special: top/bottom/* -> gör inget (ingen codelist)
  if (length(vals) == 1 && (identical(vals, "*") ||
                            grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE))) {
    return(list(list(var = var, vals = vals, extra_query = list())))
  }
  
  ok_tbl <- giltiga_varden_list[[var]]
  if (is.null(ok_tbl) || !all(c("code", "type") %in% names(ok_tbl))) {
    return(list(list(var = var, vals = vals, extra_query = list())))
  }
  
  agg_tbl <- ok_tbl |>
    dplyr::filter(tolower(type) == "aggregation") |>
    dplyr::select(dplyr::any_of(c("code", "agg_id","agg_label")))
  
  has_agg <- all(c("code", "agg_id") %in% names(agg_tbl)) && nrow(agg_tbl) > 0
  
  agg_code_to_id <- if (has_agg) stats::setNames(agg_tbl$agg_id, agg_tbl$code) else character()
  
  # 1) var="**" => default "*" + en per agg_id med "*"
  if (length(vals) == 1 && identical(vals, "**")) {
    if (!has_agg) {
      if (warn) warning("Variabeln `", var, "` saknar aggregations; '**' tolkas som '*'.")
      vals <- "*"
    } else {
      all_agg_ids <- unique(agg_tbl$agg_id)
      
      out <- list(
        list(var = var, vals = "*", extra_query = list())  # default
      )
      
      out <- c(out, purrr::map(all_agg_ids, ~ list(
        var = var,
        vals = "*",
        extra_query = setNames(list(.x), paste0("codelist[", var, "]")) |>
          c(setNames(list(output_values), paste0("outputValues[", var, "]")))
      )))
      
      return(out)
    }
  }
  
  # 2) var="agg_..." => en alt: vals="*" + codelist[var]=agg_...
  if (length(vals) == 1 && grepl("^agg_", vals)) {
    if (!has_agg) {
      if (warn) warning("Variabeln `", var, "` saknar aggregations; `", vals, "` ignoreras.")
      return(list(list(var = var, vals = "*", extra_query = list())))
    }
    return(list(list(
      var = var,
      vals = "*",
      extra_query = setNames(list(vals), paste0("codelist[", var, "]")) |>
        c(setNames(list(output_values), paste0("outputValues[", var, "]")))
    )))
  }
  
  
  # 3) blandade koder: dela i default + per agg_id (för agg-koder)
  is_agg <- vals %in% names(agg_code_to_id)
  default_vals <- vals[!is_agg]
  agg_vals <- vals[is_agg]
  
  out <- list()
  
  if (length(default_vals) > 0) {
    out <- c(out, list(list(var = var, vals = default_vals, extra_query = list())))
  }
  
  if (length(agg_vals) > 0) {
    by_id <- split(agg_vals, agg_code_to_id[agg_vals])
    out <- c(out, purrr::imap(by_id, ~ list(
      var = var,
      vals = .x,
      extra_query = setNames(list(.y), paste0("codelist[", var, "]")) |>
        c(setNames(list(output_values), paste0("outputValues[", var, "]")))
    )))
  }
  
  # om inget agg matchade -> bara default
  if (length(out) == 0) out <- list(list(var = var, vals = vals, extra_query = list()))
  
  # om flera agg_id i samma variabel (pga koder från olika agg) => vi returnerar flera alternativ
  out
}


intern_pxweb2_expand_requests_generic <- function(query_list, giltiga_varden_list,
                                                  output_values = "aggregated",    # kan också vara single
                                                  warn = TRUE) {
  
  vars_in_body <- purrr::map_chr(query_list$selection, "variableCode")
  
  # Bygg alternativ per variabel
  alts <- purrr::map(vars_in_body, ~ intern_pxweb2_var_alternatives(
    query_list, giltiga_varden_list, var = .x,
    output_values = output_values, warn = warn
  ))
  names(alts) <- vars_in_body
  
  # Kartesisk produkt av alternativ (vanligtvis blir detta 1)
  combos <- tidyr::expand_grid(!!!alts) |> purrr::transpose()
  
  purrr::map(combos, function(choice) {
    body  <- query_list
    extra <- list()
    
    for (opt in choice) {
      if (!is.null(opt$vals)) {
        body <- intern_pxweb2_set_valuecodes(body, opt$var, opt$vals)
      }
      if (length(opt$extra_query) > 0) {
        extra <- c(extra, opt$extra_query)
      }
    }
    
    intern_pxweb2_request(body = body, extra_query = extra, tag = list(kind = "expanded"))
  })
}