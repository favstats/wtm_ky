# 
# print("hello")
# 
# print(getwd())

library(httr)

all_dat <- readRDS("../data/all_dat.rds")

# print("hello")

sets <- jsonlite::fromJSON("../settings.json")




options(scipen = 999)


# wtm_data %>% count(party,sort = T)


# source("../party_utils.R")
setColors <- function(df) {
  # Check if the 'color' column exists
  if (!"color" %in% names(df)) {
    stop("The dataframe does not have a 'color' column.")
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


if(sets$cntry %in% country_codes){
  res <- GET(url = paste0("https://data-api.whotargets.me/entities?%24client%5BwithCountries%5D=true&countries.alpha2%5B%24in%5D%5B0%5D=", str_to_lower(sets$cntry)))
  color_dat <- content(res) %>% 
    flatten() %>% 
    map(compact)%>% 
    map_dfr(as_tibble) %>% 
    drop_na(id) %>% 
    rename(party = short_name) %>% 
    select(party, color) %>% 
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


if(sets$cntry %in% country_codes){
  
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
  
  election_dat30 <- readRDS("../data/election_dat30.rds") %>%
    rename(internal_id = page_id) %>%
    filter(is.na(no_data)) %>% 
    drop_na(party) %>% 
    filter(party %in% color_dat$party) %>% 
    filter(source != "tep")
  
  
  
  election_dat7 <- readRDS("../data/election_dat7.rds") %>%
    rename(internal_id = page_id) %>%
    filter(is.na(no_data)) %>% 
    drop_na(party) %>% 
    filter(party %in% color_dat$party) %>% 
    filter(source != "tep")

}


# saveRDS(election_dat30, "../data/election_dat30.rds")
# saveRDS(election_dat7, "../data/election_dat7.rds")

fin <- (as.Date(election_dat30$ds[1])-lubridate::days(1))
begin7 <- fin-lubridate::days(6)
begin30 <- fin-lubridate::days(29)

tibble(fin,
       begin7,
       begin30) %>% 
  write_csv("../data/dates.csv")


# Setting the system locale to Dutch for time/date formatting
Sys.setlocale("LC_TIME", "nl_NL")

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





the_currency <- election_dat30 %>%
  count(main_currency, sort = T) %>%
  slice(1) %>%
  pull(main_currency)

if(the_currency == "EUR"){
  currency_symbol <- "€"
} else if(the_currency=="INR"){
  currency_symbol <- "₹"
} else {
  currency_symbol <- the_currency
}

