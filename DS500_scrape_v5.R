# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(rvest)
library(lubridate)

# set url
url <- "https://spotifycharts.com/regional"

# get all dates
dates <- read_html(url) %>%
  xml_nodes(css = "#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(3) > ul > li") %>%
  html_text() %>% mdy()

# get all regions
regions <- read_html(url) %>%
  xml_nodes(css = "#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(1) > ul > li") %>%
  as.character() %>%
  str_sub(17, 18)
regions[1] <- "global"

# create list of urls
urls <- NULL
for (region in regions) {
  for (date in dates) {
    urls[[length(urls) + 1]] <- paste0(url, "/", region, "/daily/", as_date(date))
  }
}

# set css paths
region_css <- '#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(1) > div'
date_css <- '#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(3) > div'
position_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-position'
title_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-track > strong'
artist_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-track > span'
streams_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-streams'
track_id_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-image > a'

# create subsample (optional)
#urls <- head(urls)

# create empty data.frame and set colnames
data <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(data) <- c("i", "position", "title", "artist", "streams", "region", "date", "track_id")
write_csv(data, file = "spotify_charts.csv")
i <- 1

# set crawlrate
cr <- 0

# iterate over all urls and write to .csv file
for (u in urls) {
  
  t1 <- Sys.time()
  
  page <- read_html(u)
  
  position <- page %>%
    html_elements(css = position_css) %>%
    html_text()
  
  title <- page %>%
    html_elements(css = title_css) %>%
    html_text()
  
  artist <- page %>%
    html_elements(css = artist_css) %>%
    html_text() %>% str_sub(4)
  
  streams <- page %>%
    html_elements(css = streams_css) %>%
    html_text() %>%
    gsub(",", "",.) %>%
    as.numeric()
  
  date <- page %>%
    html_element(css = date_css) %>%
    mdy()
  
  region <- page %>%
    html_element(css = region_css) %>%
    html_text()
  
  track_id <- page %>%
    xml_nodes(css = track_id_css) %>%
    as.character() %>%
    str_sub(41, 62)
  
  data <- data_frame(i, position, title, artist, streams, region, date, track_id)
  
  write_csv(data,
            file = "spotify_charts.csv", append = TRUE)
  
  t2 <- Sys.time()
  
  if (as.numeric(difftime(t2, t1, units = "secs")) < cr) {
    Sys.sleep(cr - as.numeric(difftime(t2, t1, units = "secs")))
  }
  
  t3 <- Sys.time()
  
  print(paste0("Loading... ", floor((i / length(urls)) * 100),
               "% (", i, "/", length(urls), ")", " - Time: ", round(t3 - t1, 2), " sec"))
  i <- i + 1
}
