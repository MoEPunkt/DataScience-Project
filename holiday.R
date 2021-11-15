library(httr)
library(data.table)
library(rlist)
library(countrycode)

spotify <- fread("/Users/moritzerdt/Documents/ds_project/spotify_charts.csv")

country <- unique(spotify$region)

cc <- data.frame(matrix(ncol = 2, nrow = length(country) - 1))

for (c in 2:length(country)) {
  cc[c-1, 1] <- country[c]
  cc[c-1, 2] <- countrycode(country[c], "country.name", "iso2c") 
}

cc <- countrycode("Indonesia", "country.name", "iso2c")

holidays <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(holidays) <- c("Date", "LocalName", "Name", "CountryCode", "Fixed", 
                      "Global", "Types")

write_csv(holidays, file = "/Users/moritzerdt/Documents/holidays.csv")

for (year in 2017:2021) {
  
  quest <- paste0("https://date.nager.at/api/v3/publicholidays/", year, "/DE")
  
  cinfo <- GET(quest)
  
  c_content <- content(cinfo)
  
  for (i in 1:length(c_content)) {
    holidays[i, 1] <- c_content[[i]]$date
    holidays[i, 2] <- c_content[[i]]$localName
    holidays[i, 3] <- c_content[[i]]$name
    holidays[i, 4] <- c_content[[i]]$countryCode
    holidays[i, 5] <- c_content[[i]]$fixed
    holidays[i, 6] <- c_content[[i]]$global
    holidays[i, 7] <- c_content[[i]]$types
  }
  write_csv(holidays, file = "/Users/moritzerdt/Documents/holidays.csv", 
            append = TRUE)
  
  print(paste0("Loaded: ", year, ", DE"))
}


