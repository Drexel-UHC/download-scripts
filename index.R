# 0. Setup ----------------------------------------------------------------
{
  ## Dependencies
  library(tidyverse)
  library(rstudioapi)
  library(prism)
  
  ## Directory managment
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  prism_set_dl_dir(path = "download")
  
  ## set longer time out
  options(timeout=99999999)
  
}


# 1. Pull Function   ------------------------------------------------------
#' To avoild time out errors, we just do smaller indivudal pulls split up in to 5 day periods

## 1.1 Split dates into 5 day periods --------------------------------------
all_dates = seq(as.Date("2007-01-01"), as.Date("2019-12-31"), by="days")
dates_split  = split(all_dates, ceiling(seq_along(all_dates)/3))


# 1.2 Create pull combinations --------------------------------------------
template = tibble(type = list( c( "ppt", "tmean", "tmin", "tmax", "tdmean", "vpdmin",  "vpdmax")),
                  dates = dates_split_5) %>% 
  rowwise() %>% 
  mutate(minDate = dates[[1]],
         maxDate = dates[[length(dates)]]) %>% 
  select(-dates) %>% 
  unnest(cols = type) %>% 
  mutate(id = row_number(),
         grouper = id  )

## 1.3 Write function to pull daily prism based on date input + type input date ---------
##' Error handling added to function.
politely_get_prism_dailys =function(template_row){
  message_tmp = paste("pull id:", template_row$id)
  tryCatch(                       # Applying tryCatch
    expr = {                      # Specifying expression
      message(paste("Starting",message_tmp))
      get_prism_dailys(
        type =  template_row$type, 
        minDate = template_row$minDate, 
        maxDate = template_row$maxDate, 
        keepZip = F)
      output = template_row %>% mutate(status = "sucess")
      message(paste("Sucessful",message_tmp))
      
    },
    
    error = function(e){},
    warning = function(w){        # Specifying warning message
      output = template_row %>% mutate(status = "timeout")
      message("There was a timeout")
    },
    finally = {}
  )
  
  return(output)
}



## 1.4 Iterate over all combinations of type and dates ---------------------
#' Note we may need to repeat some of the timed out pull id's... see next section
status_first_pass = template  %>%  
  sample_n(20) %>% 
  group_by(grouper) %>% 
  group_modify(~politely_get_prism_dailys(.x))


# 1.5 Rerun failed jobs until all done ------------------------------------
#' write logic to cehck for failed runs and then repeat until completion.
