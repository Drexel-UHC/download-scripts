
# 0. Setup ----------------------------------------------------------------
{
  ## Dependencies
  library(tidyverse)
  library(rstudioapi)
  library(prism)
  
  ## Directory managment
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  prism_set_dl_dir(path = "download")
}



# 1. Pull Function   ------------------------------------------------------
# Here we will develop a working function to pull data via prism

