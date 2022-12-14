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


# 1. Pull metrics --------------------------------------------

## 1.1 template ------------------------------------------------------------
{
  metrics = c( 
    'gini',
    'messer',
    'krieger',
    'powell_wiley',
    'anthopolos',
    'bravo'
  )
  
  template = expand_grid(
    metric = metrics, 
    state = state.abb, 
    year = 2019) %>% 
    mutate(uhc_id = paste(state,year,metric, sep = '-'),
           row = row_number())
}



## 1.2.  Error robust pull function  --------------------------------------------
politely_get_ndi_basic = function(template_row){
  
  message_tmp = paste("pull uhc_id:", template_row$uhc_id)
  
  tryCatch( 
    
    expr = {                      
      message(paste("Starting",message_tmp))
      
      if (template_row$metric%in%c('bravo')){
        ndi_output = get(template_row$metric)(
          state = template_row$state, 
          year = template_row$year,
          subgroup = c("LtHS", "HSGiE", "SCoAD"))
      } else if (template_row$metric%in%c('anthopolos')){
        ndi_output = get(template_row$metric)(
          state = template_row$state, 
          year = template_row$year,
          subgroup = "NHoLB")
      } else {
        ndi_output = get(template_row$metric)(
          state = template_row$state, 
          year = template_row$year)
      }
      
      output = ndi_output[[1]] %>% mutate(year = template_row$year)
      
      # output %>% write_csv(paste0("download/",template_row$uhc_id,".csv"))
      
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

pull_ndi_metric = function(metric){
  template  %>% 
    filter(metric == metric) %>% 
    sample_n(3) %>%
    group_by(row) %>% 
    group_modify(~politely_get_ndi_basic(.x)) %>% 
    ungroup()
}

results = metrics %>% 
  map(~pull_ndi_metric(.x)) %>% 
  set_names(metrics)


# 1.4 Write datasets ------------------------------------

map2(results, names(results),
     function(result, metric){
       file_name = paste0("download/",metric,".csv")
       result %>% write_csv(file_name)
     })

results %>% map(~names(.x))

