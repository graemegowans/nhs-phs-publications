#****************************************
#process website using search results####
#****************************************

#read archive
#arch <- read_rds("data/pubs.rds")
#maxdate <- max(arch$published)
#dy <-  format(maxdate, "%d")
#mn <- format(maxdate, "%m")
#yr <- format(maxdate, "%Y")

#define publication page to use
page_to_use <- "http://beta.isdscotland.org/find-publications-and-data/search-results?keywords=&date-from=&date-from_submit=&date-to=&date-to_submit=&filterButton="

#with date filter
#page_to_use <- glue("https://beta.isdscotland.org/find-publications-and-data/search-results/?date-from={dy}%2F{mn}%2F{yr}&date-from_submit={dy}%2F{mn}%2F{yr}&date-to=&date-to_submit=&filterButton=&keywords=")

#download page and read
download.file(page_to_use, destfile = "scrapedpage.html", quiet = TRUE)
page <- read_html("scrapedpage.html")

#all pages together
#get number of pages
#maxno <- page %>% html_nodes(".push__bottom a") %>% html_text() %>% str_squish() %>% length()

#get number of publications
maxno <- page %>% html_nodes(css = ".gamma") %>% html_text()

#extract number and round up
maxno <- gsub("[^0-9.]", "",  maxno) %>% as.numeric()
maxno <- ceiling(maxno/10)

mylist <- list()

for (i in seq(1:maxno)){
  pg <- glue("{page_to_use}&page={i}")
  download.file(pg, destfile = "scrapedpage.html", quiet = TRUE)
  pg <- read_html("scrapedpage.html")
  pubs <- tibble(
    publication = pg %>% html_nodes(".category-item__title") %>% html_text(),
    published = pg %>% html_nodes(".category-item__meta") %>% html_text(),
    category = pg %>% html_nodes(".epsilon") %>% html_text(),
    link = pg %>% html_nodes(".category-item__link") %>% html_attr(name = "href")# %>% html_attr(name = "href")
  )
  mylist[[i]] <- pubs
  rm(pubs)
}

#make df
df <- bind_rows(mylist)

#tidy up to get subtheme, change published to date and make link
df <- df %>% 
      separate(category, into = c("theme","subtheme"), sep = ">") %>% 
      select(-theme) %>% 
      mutate(publication = str_squish(publication)) %>% 
      mutate(subtheme = str_squish(subtheme)) %>%
      rename(theme = subtheme) %>% 
      mutate(published = dmy(published)) %>%
      mutate(link = glue("<a href={link} target='_blank'>{link}</a>"))

#write_rds(df, path = "data/pubs.rds")

#get rid of intermediate
rm(page, pg, i, maxno, mylist, page_to_use)
