
pacman::p_load(knitr, tidyverse, openxlsx, sf, rmarkdown)
# setwd("C:/Users/fabio/Dropbox/postdoc/microdashboards/wtm_iq/")
# setwd("..")
# getwd()
color_dat <- readRDS("data/color_dat.rds")

raw <- readRDS("data/election_dat30.rds") %>%
  rename(internal_id = page_id) %>%
  filter(is.na(no_data)) %>% 
  filter(sources == "wtm")

if(nrow(raw)==0){
  election_dat30 <- tibble()
} else {
  election_dat30 <- raw %>% 
    drop_na(party) %>% 
    filter(party %in% color_dat$party) 
}



# rstudioapi::jobRunScript("fbadlibrary.R")
try({
  
  
  
  
  if(nrow(election_dat30)!=0){
    
    # Sys.sleep(60*7)
    all_dat <- readRDS("data/all_dat.rds")
    
    write_lines(nrow(all_dat), file = "n_advertisers.txt")
    
    dir("_site", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(quarto::quarto_render)
    
    knitr::knit("README.Rmd")
    
    rmarkdown::render("logs/overview.Rmd")
    
    file.copy(from = "logs/overview.html", to = "docs/overview.html", overwrite = T)
    
    unlink("node_modules", recursive = T, force = T)
    unlink("out", recursive = T, force = T)
    
  } else {
    
    rmarkdown::render("logs/index.Rmd")
    
    file.copy(from = "logs/index.Rmd", to = "docs/index.html", overwrite = T)
    
    unlink("node_modules", recursive = T, force = T)
    unlink("out", recursive = T, force = T)
    
  }
  
  
  
  
  
})
