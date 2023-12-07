
library(tidyverse)

download.file("https://storage.googleapis.com/ad-manager-political-ads-dump/political/2023/PoliticalAds.zip", 
              destfile = "data/PoliticalAds.zip")


unzip("data/PoliticalAds.zip",exdir = "data", overwrite = T)

padsunraw <-read_csv("data/PoliticalAds.csv") %>% janitor::clean_names()

pads <- padsunraw %>% filter(country_code == "netherlands") %>% 
  mutate(start_date = lubridate::ymd_hms(start_date)) %>% 
  filter(start_date >= as.Date("2023-08-01")) %>% #View()
  mutate(start_date = as.Date(start_date)) %>% #View()
  # filter(str_detect(organization_name, "partij|D66"))
  mutate(party = case_when(
    organization_name == "Volkspartij voor Vrijheid en Democratie" ~ "VVD",
    organization_name == "D66" ~ "D66",
  )) %>% 
  drop_na(party) %>% 
  group_by(party, start_date) %>% 
  summarize(spend = sum(spend)) %>% 
  ungroup()


dates <- read_csv("data/dates.csv")


pads7 <- pads %>% 
  filter(between(start_date,dates$begin7, dates$fin))

pads30 <- pads %>% 
  filter(between(start_date,dates$begin30, dates$fin))


# %>%
  
tobeadded <-  expand_grid(tibble(start_date = seq.Date(as.Date("2023-08-01"), lubridate::today()-1, by = "1 day")),
              party = pads %>% pull(party)) %>% 
    mutate(spend = 0)

finpads <- pads %>% 
  bind_rows(tobeadded) %>% 
  distinct(party, start_date, .keep_all = T)


saveRDS()

  # mutate()