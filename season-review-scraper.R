# Install RStudio and import this script. 
# Have an Excel named ids with a single column named Id and a list of id's to scrape in the same directory
# Replace loop lengths in this file and run the script. It takes about 30 minutes for 31. Can be left running in the background.

# Libraries to import. Make sure you import their dependencies also
library(tidyverse)
library(RSelenium)
library(rvest)
library(XML)
library(jsonlite)
library(readxl)
library(fplscrapR)
library(haven)

# excel file with a list of team id's to request for in a column headered "Id"
id <- read_excel("ids.xlsx")

# Sleeps to allow the request to go through
testit <- function(x) {
  
  p1 <- proc.time()
  
  Sys.sleep(x)
  
  proc.time() - p1 # The cpu usage should be negligible
  
}

# (2) Connecting to server
# Must have firefox installed
rD <- rsDriver(port=4445L, browser = "firefox")
remDr <- rD[["client"]]
remDr$navigate("https://fplreview.com/season-review/")

data1 <- list()
data2 <- list()

# For each ID i in your list of id's
for (i in 1:21) {
  # Find the ID input box element
  webElem <- remDr$findElement(using = "css", "div.text-container:nth-child(1) > input:nth-child(2)")
  # Paste the ID i into the box
  webElem$sendKeysToElement(list(paste(ids$Id[i])))
  # Find the Search button
  webElem2 <- remDr$findElement(using = "css", ".article-body-inner > form:nth-child(5) > input:nth-child(3)")
  # Press enter on it
  webElem2$sendKeysToElement(list("R Cran", key = "enter"))
  
  # Sleep
  testit(65)
  
  # Get the html source of the returned html
  temp_html <- remDr$getPageSource()
  # Read it?
  temp_html <- temp_html[[1]] %>% read_html
  
  # Store the two tables of data
  temp_table1 <- data.frame(temp_html %>% html_nodes(xpath=paste('//*[@id="points_table"]')) %>% html_table())
  temp_table2 <- data.frame(temp_html %>% html_nodes(xpath=paste('//*[@id="display_table"]')) %>% html_table())
  
  data1[[i]] <- temp_table1
  data2[[i]] <- temp_table2
}

# Close resources
remDr$close()
rD$server$stop()

#Extra code to gather the data in a clean format which you can paste into excel

saveRDS(data1, "21/data1.rds")
saveRDS(data2, "21/data2.rds")

# Define a frame to put the data into, I've get it as elite250
elite250 <- data.frame(matrix(ncol=7,nrow=21))
# Define the column names
colnames(elite250) <- c("Id","points","xg","odds","points_rank","xg_rank","odds_rank")
# For each ID, set the stored data
for (i in 1:21) {
  elite250$Id[i] <- id$Id[i]
  elite250$points[i] <- data1[[i]][[1]][1]
  elite250$xg[i] <- data1[[i]][[2]][1]
  elite250$odds[i] <- data1[[i]][[3]][1]
  elite250$points_rank[i] <- str_remove_all(data1[[i]][[1]][2], ",")
  elite250$xg_rank[i] <- str_remove_all(data1[[i]][[2]][2], ",")
  elite250$odds_rank[i] <- str_remove_all(data1[[i]][[3]][2], ",")
}
# Sort?
elite250 <- data.frame(sapply(elite250, as.numeric))
# Deduce the data that is deduced
elite250 <- mutate(elite250, luck = points - odds)
elite250 <- mutate(elite250, xg_luck = points - xg)

