library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)

get_acled_dat <- function(.country = NULL, .year = NULL) {
  
  # allow self-signed certs
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  
  # term acceptance is necessary with every request
  base_url <- "https://api.acleddata.com/acled/read?terms=accept"
  
  # fill in spaces
  my_country <- str_replace_all(.country, " ", "%20")
  
  my_year <- .year
  
  if (is.null(my_country) & is.null(my_year)) {
    get_url <- paste0(base_url, "?&limit=1000")
  } else if (!is.null(my_country) & is.null(my_year)) {
    get_url <- paste0(base_url, "&country=", my_country, "&country_where=%3D", "&limit=0")
  } else if (is.null(my_country) & !is.null(my_year)) {
    get_url <- paste0(base_url, "&year=", my_year, "&limit=0")
  } else {
    get_url <- paste0(base_url, "&country=", my_country, "&country_where=%3D", "&year=", my_year, "&limit=0")
  }
  
  tryCatch({
    
    message(paste0("... retrieving acled data for ", 
                   if (!is.null(my_country)) {str_replace_all(my_country, "%20", " ")} else {"all countries"},
                   " ", "for the year ", 
                   if(!is.null(my_year)) {my_year} else {"all years"}))
    
    result <- httr::GET(get_url)
    
  }, error = function(e) {
    message("\nfailed to get acled json\n")
  }
  )
  
  json_content <- content(result, as = "text")
  
  content_list <- fromJSON(json_content, flatten = TRUE)
  
  dat <- content_list$data
  
  # returns all classes as character class
  dat %>% 
    mutate(date = lubridate::ymd(event_date)) %>% 
    mutate(year = as.integer(year)) %>% 
    mutate_at(vars(event_type:assoc_actor_2, 
                   region, country, 
                   admin1:location, 
                   geo_precision,
                   iso3,
                   source,
                   source_scale), 
              ~as.factor(.)) %>% 
    mutate_at(vars(latitude, 
                   longitude, 
                   fatalities), 
              ~as.double(.))
}
