# 0. Setup ----------------------------------------------------------------
{
  ## Dependencies
  library(tidyverse)
  library(rstudioapi)
  library(datasets)
  library(ndi)
  
  ## Directory managment
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}



# 1. Pull basic non-subgroup metrics  --------------------------------------------
#'    There are four metrics without sub groups. THe rest will need more complex 
#'    templates that specify combinations of population groups. Lets pull these
#'    simple ones first. 
#'  
#'    


## 1.1 template ------------------------------------------------------------
{
  metrics_no_subgroups = c( 
    'gini',
    'messer',
    'krieger',
    'powell_wiley'
  )
  
  template_basic = expand_grid(
    metric = metrics_no_subgroups, 
    state = state.abb, 
    year = 2017:2020) %>% 
    mutate(uhc_id = paste(state,year,metric, sep = '-'),
           row = row_number())
}

## 1.2.  Error robust pull function  --------------------------------------------
politely_get_ndi_basic = function(template_row){
  message_tmp = paste("pull uhc_id:", template_row$uhc_id)
  tryCatch(                        
    expr = {                      
      message(paste("Starting",message_tmp))
      ndi_output = get(template_row$metric)(
        state = template_row$state, 
        year = 2020)
      output = ndi_output[[1]]%>% 
        mutate(uhc_id = template_row$uhc_id) %>% 
        select(uhc_id, everything())
      message(paste("Sucessful",message_tmp))
    },
    error = function(e){
      output = template_row %>% mutate(status = "error")
      message(e)
      message("\n !!!!!!!!!!!! Error")
    },
    warning = function(w){       
      output = template_row %>% mutate(status = "warning")
      message(w)
      message("\n ####There was a warning")
    },
    finally = {}
  )
  return(output)
}



## 1.3 Iterate basic pulls ---------------------
#' Note we may need to repeat some of the timed out pull id's... see next section
ndi_pull_1 = template  %>%  
  sample_n(1) %>% 
  group_by(row) %>% 
  group_modify(~politely_get_ndi(.x)) %>% 
  ungroup()


# 1.4 QC basic runs ------------------------------------
#' write logic to check for failed runs and then repeat until completion.
