# 
# print("hello")
# 
# print(getwd())
# getwd()
# setwd("_site")

library(httr)

all_dat <- readRDS("../data/all_dat.rds")

# print("hello")

sets <- jsonlite::fromJSON("../settings.json")




options(scipen = 999)


# wtm_data %>% count(party,sort = T)


# sources("../party_utils.R")
setColors <- function(df) {
  # Check if the 'color' column exists
  if (!"color" %in% names(df)) {
    df <- df %>% mutate(color = NA)
  }
  
  # Function to generate a random color
  generateRandomColor <- function() {
    sprintf("#%06X", sample(0:16777215, 1)) # Generates a random hex color
  }
  
  # Apply the function to each row
  df$color <- sapply(df$color, function(color) {
    if (is.na(color) || nchar(color) < 5) {
      return(generateRandomColor())
    } else {
      return(color)
    }
  })
  
  return(df)
}

country_codes <- c("AD", "AL", "AM", "AR", "AT", 
                   "AU", "BA", "BE", "BG", "BR", 
                   "CA", "CH", "CL", "CO", "CY", 
                   "CZ", "DE", "DK", "EC", "EE", 
                   "ES", "FI", "FR", "GB", "GR", 
                   "GT", "HR", "HU", "IE", "IN", 
                   "IS", "IT", "LI", "LT", "LU", 
                   "LV", "MD", "ME", "MK", "MT",
                   "MX", "NL", "NO", "NZ", "PL", 
                   "PT", "RO", "RS", "SE", "SI", 
                   "SK", "SM", "TR", "UA", "US", 
                   "VE", "ZA")


download.file(paste0("https://data-api.whotargets.me/advertisers-export-csv?countries.alpha2=", str_to_lower(sets$cntry)), destfile = "data/wtm_advertisers.csv")

thedat <- read_csv("../data/wtm_advertisers.csv")

if(sets$cntry %in% country_codes & nrow(thedat)!=0){
  res <- GET(url = paste0("https://data-api.whotargets.me/entities?%24client%5BwithCountries%5D=true&countries.alpha2%5B%24in%5D%5B0%5D=", str_to_lower(sets$cntry)))
  color_dat <- content(res) %>% 
    flatten() %>% 
    map(compact)%>% 
    map_dfr(as_tibble) %>% 
    drop_na(id) %>% 
    rename(party = short_name) %>% 
    select(party, contains("color")) %>% 
    setColors() %>% 
    rename(colors = color)
} else {
  polsample <- readRDS("../data/polsample.rds")
  partycolorsdataset  <- readRDS("../data/partycolorsdataset.rds")
  
  color_dat <- polsample %>% 
    # count(cntry, partyfacts_id, sort = T) %>% View()
    filter(cntry == sets$cntry) %>%
    select(party = name_short, partyfacts_id) %>% 
    distinct(partyfacts_id, party) %>% 
    left_join(partycolorsdataset %>% mutate(partyfacts_id = as.character(partyfacts_id))) %>% 
    select(party, color = hex)  %>% 
    setColors() %>% 
    rename(colors = color) %>% 
    drop_na(party)
}

saveRDS(color_dat, "../data/color_dat.rds")




most_left_party <- color_dat$party[1]


scale_fill_parties <- function(...){
  ggplot2:::manual_scale(
    'fill',
    values = setNames(color_dat$colors, color_dat$party),
    ...
  )
}
scale_color_parties <- function(...){
  ggplot2:::manual_scale(
    'color',
    values = setNames(color_dat$colors, color_dat$party),
    ...
  )
}


if(sets$cntry %in% country_codes & nrow(thedat)!=0){
  
  election_dat30 <- readRDS("../data/election_dat30.rds") %>%
    rename(internal_id = page_id) %>%
    filter(is.na(no_data)) %>% 
    drop_na(party) %>% 
    filter(party %in% color_dat$party)
  
  
  election_dat7 <- readRDS("../data/election_dat7.rds") %>%
    rename(internal_id = page_id) %>%
    filter(is.na(no_data)) %>% 
    drop_na(party) %>% 
    filter(party %in% color_dat$party)
  
} else {
  
  raw <- readRDS("../data/election_dat30.rds") %>%
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
  
  
  
  raw <- readRDS("../data/election_dat7.rds") %>%
    rename(internal_id = page_id) %>%
    filter(is.na(no_data)) %>% 
    filter(sources == "wtm")
  
  if(nrow(raw)==0){
    election_dat7 <- tibble()
  } else {
    election_dat7 <- raw %>% 
      drop_na(party)  %>% 
      filter(party %in% color_dat$party)
  }
  
}



# election_dat30test <<- election_dat30

# saveRDS(election_dat30, "../data/election_dat30.rds")
# saveRDS(election_dat7, "../data/election_dat7.rds")

fin <- (as.Date(election_dat30$ds[1])-lubridate::days(1))
begin7 <- fin-lubridate::days(6)
begin30 <- fin-lubridate::days(29)

tibble(fin,
       begin7,
       begin30) %>% 
  write_csv("../data/dates.csv")


s
# Function to create Dutch date strings with suffixes
create_date <- function(x) {
  the_date <- format(x, "%e %b") # %e for day of the month without leading zeros, %B for full month name in Dutch
  # In Dutch, date suffixes are not commonly used so we can omit the 'append_date_suffix' part
  return(trimws(the_date)) # trimws to remove any leading or trailing whitespace which might be left after %e
}

last7days_string <- paste0(create_date(begin7), " - ", create_date(fin), " ", lubridate::year(fin)) %>% str_replace("Oct", "Okt")
last30days_string <- paste0(create_date(begin30), " - ", create_date(fin), " ", lubridate::year(fin)) %>% str_replace("Oct", "Okt")

# # Print the Dutch date range strings
# print(last7days_string)
# print(last30days_string)
# 
# # Reset locale back to the original if necessary
# Sys.setlocale("LC_TIME", "C")


if(nrow(election_dat30)!=0){
  
  the_currency <- election_dat30 %>%
    count(main_currency, sort = T) %>%
    slice(1) %>%
    pull(main_currency)
  
  currency_symbol <- priceR::currency_info %>% 
    filter(iso_code == the_currency) %>% 
    pull(symbol)
  
  if(is.null(currency_symbol)){
    currency_symbol <- the_currency
  }
  
}






