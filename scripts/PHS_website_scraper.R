#****************************************
# written by graeme
# April 2020
# scrapes PHS website for publications
# extracts info
# returns object for shiny app
# if structure of this will change
# might need updated
#****************************************

#****************************************
#load packages####
#****************************************
library(rvest)
library(tidyverse)
library(lubridate)

#****************************************
#process PHS website using search results####
#****************************************

#define publication page to use
#this might need to change once it's properly populated
page_to_use <- "https://publichealthscotland.scot/latest-publications"

#for working behind firewall
download.file(page_to_use, destfile = "scrapedpage.html", quiet=TRUE)
page <- read_html("scrapedpage.html")

#get number of pages, remove next and previous
pgs <- page %>% html_nodes(".pagination a") %>% html_text()
pgs <- pgs[!pgs %in% c("Next", "Previous")]

#make empty list
mylist <- list()

#loop across sequence
for (i in pgs) {
  
  #define page
  pg <- paste0("https://publichealthscotland.scot/latest-publications?&page=", i)
  
  #get page - helps if behind firewall
  download.file(pg, destfile = "scrapedpage.html", quiet=TRUE)
  pg <- read_html("scrapedpage.html")
  
  #get titles and dates
  #prevent parsing when date is in publication - takes only first 3 words
  pubs <- tibble(publication = pg %>% 
                        html_nodes(".category-item__title") %>% 
                        html_text()) %>%
    mutate(published = word(publication, start = 1, end = 3)) %>% #get first three words
    mutate(published = dmy(published)) %>% #parse to date
    mutate(to_remove = !is.na(published)) %>% #flag those for deletion 
    fill(published, .direction = "down") %>% #fill in published dates
    filter(to_remove != TRUE) %>% #get rid of the dates
    select(-to_remove) %>% #remove col
    mutate(link = pg %>% html_nodes(".category-item__title > a") %>% html_attr("href")) 
  
  #add to list
  mylist[[i]] <- pubs
  
  #clean up
  rm(pubs, pg)
}

#make dataframe
df <- mylist %>% map_df(as_tibble)

#process
df <- df %>% 
        mutate(is_isd = str_detect(link, "beta.isdscotland.org")) %>% 
        mutate(topic = case_when(is_isd == TRUE ~ str_extract(link, "find-publications-and-data/.*"))) %>% 
        mutate(topic = str_remove(topic, "find-publications-and-data/")) %>%
        mutate(theme = word(topic, 1, sep = "/"),
              subtheme = word(topic, 2, sep = "/")) %>% #get theme and subtheme
        mutate(theme = case_when(
                          str_detect(link, "hps.scot.nhs.uk") ~ "HPS",
                          str_detect(link, "healthscotland.scot") ~ "Health Scotland",
                          TRUE ~ theme)) %>%
        mutate(theme = str_replace_all(theme, "-", "_"),
               subtheme = str_replace_all(subtheme, "-", "_")) %>%
        mutate(link = paste0("<a href='", link, "' target='_blank'>", link, "</a>")) %>% 
        select(-is_isd, -topic)

#get rid of intermediate
rm(page, i, mylist, page_to_use, pgs)
