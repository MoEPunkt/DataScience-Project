install.packages("cli")
install.packages("ellipsis")
install.packages("devtools")

library(httr)
library(rvest)
library(purrr)
library(lubridate)
library(spotifycharts)
library(readr)
library(stringr)
library(data.table)

devtools::install_github("56north/spotifycharts")

setwd("/Users/moritzerdt/Google Drive/Master/studium/03-WS2022/Datascience-projekt/")

source("/Users/moritzerdt/Google Drive/Master/studium/03-WS2022/Datascience-projekt/keys.R")

# https://msmith7161.github.io/what-is-speechiness/
#https://developer.spotify.com/documentation/web-api/
# https://developer.spotify.com/dashboard/applications/98d91ef62bf841d8a4211c9f0f4856fb
# https://artists.spotify.com/blog/how-fans-also-like-works
# https://artists.spotify.com/help/article/fans-also-like
# https://medium.com/analytics-vidhya/visualizing-spotify-top-200-data-in-tableau-implementing-a-fast-python-web-scraper-88a562495ad8
# https://medium.com/m2mtechconnect/predicting-spotify-song-popularity-with-machine-learning-7a51d985359b
# https://www.rcharlie.com/spotifyr/
#https://datacritics.com/2018/03/20/scrape-it-yourself-spotify-charts/

# https://spotifycharts.com/regional/global/daily/latest/download?
  
# https://spotifycharts.com/regional/global/daily/2021-10-29/download?


# Fix constant url
url <- "https://spotifycharts.com/regional"

# Get all dates
dates <- read_html(url) %>%
  xml_nodes(css = "#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(3) > ul > li") %>%
  html_text() %>% mdy()

# Get all regions
regions <- read_html(url) %>%
  xml_nodes(css = "#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(1) > ul > li") %>%
  as.character() %>%
  str_sub(17, 18)
regions[1] <- "global"

# Create a list of URLs
urls <- NULL
for (region in regions) {
  for (date in dates) {
    urls[length(urls) + 1] <- paste0(url, "/", region, "/daily/", as_date(date))
  }
}

charts <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(charts) <- c("Rank", "Track", "Artist", "Streams", "Region", "Date", "Track_id")
write_csv(charts, file = "/Users/moritzerdt/Documents/spotify_charts.csv")
i <- 1

# Get scraper running
# US 05.03.2021 doesn´t exist

#page <- "https://spotifycharts.com/regional/global/daily/2019-06-04"

SpotifyScrape <- function(x) {
  # Current URL
  page <- x
  
  track_id_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-image > a'
  
  # Get the data
  rank <- page %>% read_html() %>% html_nodes('td.chart-table-position') %>% html_text() %>% as.data.frame()
  track <- page %>% read_html() %>% html_nodes('td.chart-table-track strong') %>% html_text() %>% as.data.frame()
  artist <- page %>% read_html() %>% html_nodes('td.chart-table-track span') %>% html_text() %>% as.data.frame()
  streams <- page %>% read_html() %>% html_nodes('td.chart-table-streams') %>% html_text() %>% as.data.frame()
  track_id <- page %>% read_html() %>% xml_nodes(css = track_id_css) %>% as.character() %>% str_sub(41, 62) %>% as.data.frame()
  info <- page %>% read_html() %>% html_nodes('.responsive-select-value') %>% html_text() %>% as.data.frame()
  
  # Extract the date from info
  date <- paste0(substr(info[3,], 7, 10), "-", substr(info[3,], 1, 2), "-", 
                 substr(info[3,], 4, 5)) %>% as_date() %>% rep(nrow(rank)) %>% as.data.frame()
  
  # Extract the country from info
  country <- info[1,] %>% rep(nrow(rank)) %>% as.data.frame()
  
  # Create a dataframe
  charts <- data.frame(rank, track, artist, streams, country, date, track_id)

  # Write charts to the storage as a .csv file
  write_csv(charts, file = "/Users/moritzerdt/Documents/spotify_charts.csv", append = TRUE)
  
  # Document the progress
  print(paste0("Loading ", floor((i / length(urls)) * 100), "% (", i, "/", length(urls), ")"))
  
  # Set the system to sleep for two seconds
  #Sys.sleep(2)
}

# url 6197 skipped
# urls 6197-7076 not available 
# url 7948 not available

i <- 7752
for (u in 7077:8000){
  
  link <- urls[u]
  
  SpotifyScrape(link)
  
  i <- i + 1
  
  if (u == 8200 | u == 8400 | u == 8600 | u ==8800) Sys.sleep(60)
}

# 
for (j in 7:123) {
  if (j < 123) {
    for (u in 1:1000){
        position <- (j-1)*1000 + u
        
        link <- urls[position]
        
        SpotifyScrape(link)
        
        i <- i + 1
        
        if (u == 200 | u == 400 | u == 600 | u == 800) Sys.sleep(60)
      }
    Sys.sleep(500)
    
  } else {
    for(u in 123001:123830){
        
      link <- urls[u]
        
      SpotifyScrape(link)
        
      i <- i + 1
      
      if (u == 123200 | u == 123400 | u == 123600) Sys.sleep(60)
    }
  }
}

date <- dates

update <- function(date) {
  
  # Get the last date that is stored in the dataset
  lastDate <- max(date)
  
  # Get all the dates (until the most recent one) from spotifycharts
  recentDate <- read_html(url) %>%
    xml_nodes(css = "#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(3) > ul > li") %>%
    html_text() %>% mdy() %>% as_date() %>% max()
  
  # Create a vector from the last entry in the dataset until the most recent one
  # from spotifycharts
  missingDates <- seq(lastDate, recentDate, by = "day")[-1]
  
  # Get all the regions from spotifycharts
  regions <- read_html(url) %>%
    xml_nodes(css = "#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(1) > ul > li") %>%
    as.character() %>%
    str_sub(17, 18)
  regions[1] <- "global"
  
  # Create the URLs to scrape the data from spotifycharts
  urls <- NULL
  for (region in regions) {
    for (i in missingDates) {
      urls[length(urls) + 1] <- paste0(url, "/", region, "/daily/", as_date(i))
    }
  }
  
  # Loop over the URLs to scrape the data from spotifycharts
  i <- 1
  for (u in 1:length(urls)) {
    link <- urls[u]
    
    SpotifyScrape(link)
    
    i <- i + 1
  }
}

spotify <- fread("/Users/moritzerdt/Documents/ds_project/spotify_charts.csv")
