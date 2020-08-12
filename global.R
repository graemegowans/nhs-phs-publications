#***************************************
# install packages and set theme
#***************************************
library(shiny)
library(tidyverse)
library(lubridate)
library(glue)
library(DT)
library(rvest)
library(plotly)
library(shinyWidgets)

#***************************************
# define variables
#***************************************

#taken from PHS website
font_col <- "#433684"

# Buttons to remove
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',  
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                     'hoverClosestCartesian')

# Style of x and y axis
xaxis_plots <- list(title = FALSE, 
                    tickfont = list(size = 12), 
                    titlefont = list(size = 12),
                    showline = TRUE, 
                    fixedrange = TRUE)

yaxis_plots <- list(title = FALSE, 
                    rangemode = "tozero", 
                    fixedrange = TRUE, size = 4,
                    tickfont = list(size = 12), 
                    titlefont = list(size = 12))

#***************************************
# data processing####
#***************************************

#scrape website for new data
#loads as df
#source("scripts/PHS_website_scraper.R")
#source("scripts/ISD_beta_website_scraper.R")

#faster for testing
df <- read_rds("data/pubs.rds")

#make column for year and month
df$year_published <- as_factor(year(df$published))
df$month_published <- month(df$published, abbr = TRUE, label = TRUE)

#make theme factor for plotting
df$theme <- as_factor(df$theme)

#extract levels to use in selection boxes
yrs <- levels(df$year_published)
theme <- levels(df$theme)

#arrange to show recent pubs first
df <- arrange(df, desc(published))

#***************************************
# plot functions####
#***************************************
#plot by year#
plot_year_bars <- function(dataset, area = T) {
  
  #generate counts and order by counts
  #count_data <- count(dataset, year_published, 
  #                    name = "number") %>% arrange(year_published)
  
  #make factor
  count_data <- dataset %>% 
                  mutate(year_published = fct_inorder(year_published))
  
  #Text for tooltip
  tooltip_trend <- glue("Year published: {count_data$year_published}<br>",
                        "Count: {count_data$number}")
  
  #Creating community hub plot
  count_data %>% 
    plot_ly(x = ~year_published, y = ~number) %>% 
    add_bars(color = ~year_published, #colour group
             #colors = pal_comm, #palette
             stroke = I("black"), #outline
             text = tooltip_trend, 
             hoverinfo = "text",
             name = ~year_published) %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, 
           xaxis = xaxis_plots,
           showlegend = FALSE,
           barmode = "group") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
}

#plot by theme#
plot_theme_bars <- function(dataset, area = T) {
 
  #generate counts
  #count_data <- count(dataset, theme, name = "number", sort = TRUE)
  
  #make factor
  count_data <- dataset %>% 
                  mutate(theme = fct_inorder(theme))
  
  #Text for tooltip
  tooltip_trend <- glue("Theme: {count_data$theme}<br>",
                        "Count: {count_data$number}")
  
  #Creating community hub plot
  count_data %>% 
    plot_ly(x = ~theme, y = ~number) %>% 
    add_bars(marker = list(color = ~font_col),
             stroke = I("black"), #outline
             text = tooltip_trend, 
             hoverinfo = "text",
             name = ~theme) %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, 
           xaxis = xaxis_plots,
           showlegend = FALSE,
           barmode = "group") %>% #split by group
    #leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
}

#plot by month
plot_month <- function(dataset, area = T) {
  
  #generate counts
  count_data <- count(dataset, year_published, month_published, 
                      name = "number") %>% arrange(year_published, month_published)
  
  #make factor
  count_data <- count_data %>% 
    mutate(year_published = fct_inorder(year_published))
  
  #Text for tooltip
  tooltip_trend <- glue("Date: {count_data$month_published}-{count_data$year_published}<br>",
                        "Count: {count_data$number}")
  
  #Creating community hub plot
  count_data %>% 
    plot_ly(x = ~month_published, y = ~number) %>% 
    add_lines(color = ~year_published,
             text = tooltip_trend, 
             hoverinfo = "text",
             name = ~year_published) %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           #yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5) #position of legend
            ) %>%
    #leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
}

#***************************************
# validation function####
#***************************************

val_func <- function(input){
  if(dim(input)[1] == 0) {
    "All publications have been filtered - please check"
  } else {
    NULL
  }
}
