# conn <- dbConnect(
#   RPostgres::Postgres(),
#   dbname = 'postgis',
#   host = 'localhost',
#   port = 5432,
#   user = 'postgres',
#   password = 'postgres'
# )

##make a function to retrieve the watershed info
get_watershed <- function(fish_habitat_info){
  mapply(fwapgr::fwa_watershed, blue_line_key = fish_habitat_info$blue_line_key,
         downstream_route_measure = fish_habitat_info$downstream_route_measure) %>%
    purrr::set_names(nm = fish_habitat_info$crossing_id) %>% ##pscis_model_combined_id
    discard(function(x) nrow(x) == 0) %>% ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles
    data.table::rbindlist(idcol="pscis_model_combined_id") %>%
    distinct(pscis_model_combined_id, .keep_all = T) %>% ##there are duplicates we should get rid of
    st_as_sf()
}


##function to trim up sheet and get names (was previously source from altools package)
at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}


import_fish_data <- function(workbook_name = 'habitat_assessments.xls'){
  readxl::excel_sheets(path = paste0(getwd(),"/data/", workbook_name)) %>% 
  purrr::set_names() %>% 
  purrr::map(readxl::read_excel, 
             path = paste0(getwd(),"/data/", workbook_name), 
             .name_repair = janitor::make_clean_names) %>% 
  purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  purrr::map(readr::type_convert)
}

##the colors don't seem to work yet.  Might need to put a case_when for the actual google icon symbol.  Posting cutom symbols on a url and pointing to them will work too.
make_kml_col <- function(df){
  df %>% 
    mutate(pscis_crossing_id = as.integer(pscis_crossing_id),
           my_crossing_reference = as.integer(my_crossing_reference),
           color = case_when(barrier_result == 'Barrier' ~ 'red',
                             barrier_result == 'Passable' ~ 'green',
                             barrier_result == 'Potential' ~ 'purple',
                             T ~ 'white'),
           color = plotKML::col2kml(color),
           site_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                               is.na(pscis_crossing_id) ~ my_crossing_reference),
           label = paste0(site_id, '-', stream_name, '-', barrier_result, 
                          '-', habitat_value, ' habitat value')) 
  # mutate(across(where(is.numeric), round(.,2)))
  
}

##this is how we make html tables.  Can add colors or whatever -https://stackoverflow.com/questions/50199845/converting-dataframe-in-required-html-table-format-in-r
make_html_tbl <- function(df) {
  
  df2 <- df %>% 
    dplyr::mutate(photo_link = paste0(
      'https://github.com/NewGraphEnvironment/Bulkley_Planning/tree/master/data/photos/', site_id,
      '/crossing_all.JPG'),
      map_link = paste0(
        'https://hillcrestgeo.ca/outgoing/fishpassage/projects/bulkley/FishPassage_', map_tile_display_name,
        '.pdf') ) %>% 
    dplyr::mutate(photo_link = cell_spec('crossing', "html", link = photo_link),
                  map_link = cell_spec('mapsheet', "html", link = map_link)) %>%
    dplyr::select(road_name, crossing_subtype, cv_diam_m = diameter_or_span_meters,
                  cv_lngth_m = length_or_width_meters, out_drop_m = outlet_drop_meters, cv_slope = culvert_slope_percent, 
                  chan_wdth = downstream_channel_width_meters,
                  habitat_value, cv_lgth_score = culvert_length_score, embed_score, out_dro_score = outlet_drop_score,
                  culvert_slope_score, swr_score = stream_width_ratio_score, final_score,
                  barrier_result, photo_link, map_link, assessment_comment)
  df <- df %>% 
    mutate(html_tbl = knitr::kable(df2, 'html', escape = F)%>% 
             # All cells get a border
             row_spec(0:nrow(df2), extra_css = "border: 1px solid black;") %>% 
             row_spec(0, background = "yellow") %>% 
             kableExtra::column_spec(column = ncol(df2), width_min = '2in') %>% 
             kableExtra::column_spec(column = 1:17, width_min = '0.2in')
    )
  return(df)
}

## add a line to the function to make the comments column wide enough
make_html_tbl_hab <- function(df) {
  df2 <- df %>% janitor::remove_empty() 
  df %>% 
    mutate(html_tbl = knitr::kable(df2, 'html', escape = F) %>% 
             kableExtra::row_spec(0:nrow(df2), extra_css = "border: 1px solid black;") %>% # All cells get a border
             kableExtra::row_spec(0, background = "yellow") %>% 
             kableExtra::column_spec(column = ncol(df2) - 1, width_min = '0.5in') %>%
             kableExtra::column_spec(column = ncol(df2), width_min = '4in')
    )
}




# DBI::dbDisconnect(conn = conn)