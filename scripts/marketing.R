

library(fbRads)
library(tidyverse)

token <- Sys.getenv("yo")
#' Get account details of Ad Accounts that are accessible by the given token
#' @inheritParams fbad_get_owned_ad_accounts
#' @export
#' @return character vector of Ad Account ids
fbad_get_my_ad_accounts <- function(token, version) {
  
  ## try to look up token and version if not provided
  if (missing(token)) {
    if (is.null(fbacc$access_token)) {
      stop('Missing Facebook Ads API token')
    }
    token <- fbacc$access_token
  }
  if (missing(version)) {
    if (is.null(fbacc$api_version)) {
      version <- fb_api_most_recent_version()
    } else {
      version <- fbacc$api_version
    }
  }
  
  res <- jsonlite::fromJSON(fbRads:::fbad_request(
    path   = 'me',
    method = 'GET',
    params = list(access_token = token, fields = 'adaccounts'),
    version = version))[[1]]
  data <- list(res$data)
  
  ## iterate through all pages
  while (!is.null(res$paging$`next`)) {
    res  <- fbad_request_next_page(res$paging$`next`)
    data <- c(data, list(res$data))
  }
  
  ## return
  do.call(rbind, data)
  
}

accounts <- fbad_get_my_ad_accounts(token, version = '18.0')

# account <- sample(accounts$account_id, 1)
fbacc <- fbad_init(accountid = accounts$account_id[2], token = token, version = '18.0')



election_dat30 <- readRDS("data/election_dat30.rds")


res <- election_dat30  %>% 
  filter(type == "detailed") %>% 
  arrange(party) %>% 
  distinct(detailed_type, value) %>% 
  # slice(1) %>% 
  split(1:nrow(.)) %>% 
  map_dfr_progress(~{
    

    
    suppressMessages(
      cleanres <- fbad_get_search(q = .x$value, type = "targetingsearch", limit = 50000) %>% 
        mutate(detailed_type = .x$detailed_type,
               search_detail = .x$value) %>% 
        # slice(1:2) %>% 
        unnest_wider(path) %>% 
        janitor::clean_names() %>%
        rename_at(vars(contains("x")), ~str_replace(.x, "x", "level"))  
    )
    
    return(cleanres)
    
  }) %>%
  distinct(id, name, type, detailed_type, search_detail, .keep_all = T)


# res <- interests %>% 
#   map_dfr_progress(~fbad_get_search(q = .x, type = "targetingsearch", limit = 50000)) %>% 
#   # bind_rows(readRDS("data/fb_targeting_list.rds")) %>%
#   distinct(id, name, type, .keep_all = T)




