get_directions_custom <- function(coords){
  # stopifnot(!is.null(origin), !is.null(destination))
  # url_base <- "https://www.google.com/maps/api/directions/json?"
  # 
  # origin_url <- URLencode(origin)
  # destination_url <- URLencode(destination)
  # 
  # paste0(url_base, "origin=", origin_url,
  #        "&destination=", destination_url,
  #        "&key" = key)
  # 
  # args <- c(origin = origin_url, destination = destination_url, key = key )
  # 
  # url_final <- paste0(url_base, paste0("&", 
  #                        paste0(names(args)), "=", paste0(args), collapse = ""))
  # 
  # 
  # response <- httr::GET(url_final)
  # text <- httr::content(response, as = "text", encoding = "UTF-8")
  # 
  # coords <- jsonlite::fromJSON(text,
  #                              simplifyVector = F)
  
  if(coords$status == "OK"){
    
    k <- length(coords$routes[[1]]$legs[[1]]$steps)
    
    res <- lapply(1:k, function(i){
      
      txt_in <- coords$routes[[1]]$legs[[1]]$steps[[i]]
      
      res <- tibble(distance = txt_in$distance$text,
                    total_dist = txt_in$distance$value,
                    duration = txt_in$duration$text,
                    polyline = txt_in$polyline$points,
                    travel_mode = txt_in$travel_mode,
                    instructions =  gsub("<.*?>", "",txt_in$html_instructions)) 
      
      return(res)
    }) %>%  bind_rows() 
    
    
    res  <- res %>% mutate(lat_lng = purrr::map(polyline, decode_pl))
    
    route <- bind_rows(res$lat_lng)
   
    polyln  <- encode_pl(route$lat, route$lon)
   
  }else{
    res <- tibble(distance = NA_character_,
                  duration = NA_character_,
                  polyline = NA_character_,
                  travel_mode = NA_character_,
                  instructions = NA_character_,
                  lat_lng = NA) 
    polyln <- NA
    
  }
  
  return(list(data  = res, status = coords$status, polyline = polyln))
  
}