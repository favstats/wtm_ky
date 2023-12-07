

library(tidyverse)

nl <- read_csv("data/FacebookAdLibraryReport_2023-10-20_NL_last_90_days_advertisers.csv")


all_dat <- readRDS("data/all_dat.rds")

nl %>% 
  janitor::clean_names() %>% 
  mutate(amount_spent_eur = readr::parse_number(amount_spent_eur)) %>% 
  arrange(desc(amount_spent_eur)) %>% 
  mutate(page_id = as.character(page_id)) %>% 
  mutate(fb_page = paste0("https://www.facebook.com/", page_id),
         fbad_page = paste0("https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=NL&view_all_page_id=", page_id, "&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped&search_type=page&media_type=all")) %>% 
  left_join(all_dat %>% select(page_id, party)) %>% 
  bind_rows(all_dat %>% select(page_id, disclaimer, page_name, party)) %>% 
  mutate(fb_page = paste0("https://www.facebook.com/", page_id),
         fbad_page = paste0("https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=NL&view_all_page_id=", page_id, "&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped&search_type=page&media_type=all")) %>% 
  distinct(page_id, .keep_all = T) %>% #View()
  write_csv("data/labelling.csv")
  
election_dat30 <- readRDS("data/election_dat30.rds")


election_dat30 %>% 
  filter(type == "detailed") %>% 
  arrange(party) %>% 
  distinct(detailed_type, value) %>% 
  write_csv("data/detailed.csv")
