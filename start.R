
pacman::p_load(knitr, tidyverse, openxlsx, sf, rmarkdown)



# rstudioapi::jobRunScript("fbadlibrary.R")
try({
  
  # Sys.sleep(60*7)
  all_dat <- readRDS("data/all_dat.rds")
  
  write_lines(nrow(all_dat), file = "n_advertisers.txt")
  
  
  dir("_site", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(quarto::quarto_render)
  
  knitr::knit("README.Rmd")
  
  rmarkdown::render("logs/overview.Rmd")
  
  file.copy(from = "logs/overview.html", to = "docs/overview.html", overwrite = T)
  
  unlink("node_modules", recursive = T, force = T)
  unlink("out", recursive = T, force = T)
  
  
  
})
