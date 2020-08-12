#***************************************
# define UI
#***************************************

fluidPage(
  
  #for embedding on old ISD website
  HTML('<meta name = "viewport" content = "width=770">'),
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/4.2.1/iframeResizer.contentWindow.min.js", #new link
                type = "text/javascript"),
    
    #change color of the tabs and links
    tags$style(HTML(glue(".tabbable > .nav > li > a 
                         {color: #000000}
                         .tabbable > .nav > li[class = active] > a 
                         {background-color: {{font_col}}; 
                         color: #FFFFFF}
                         .well 
                         {background-color: #FFFFFF; 
                         border: 0px solid #336699}
                         a {color: {{font_col}}}
                         a:hover {color: #12875A}
                         a:focus {color: #12875A}
                         .btn.btn-primary 
                         {background-color: {{font_col}};
                         border: 0px solid {{font_col}};
                         color: #FFFFFF;}
                         .btn-primary:hover, 
                         .btn-primary:focus, 
                         .btn-primary:active, 
                         .btn-primary.active, 
                         .open>.dropdown-toggle.btn-primary 
                         {color: #FFFFFF;
                         background-color: #12875A;
                         border-color: #12875A;}
                         ", 
                         .open = "{{", .close = "}}")))
  ),
  
  #application title
  #titlePanel("DRAFT VERSION"),
  
  #set up main panel
  #fluidRow(column(width = 12,
                  
    #set up the tab panels
    tabsetPanel(type = "tabs",
                id = "Panels",
    #introduction tab
      tabPanel(title = "Information",
               icon = icon("info-circle"),
               tags$h1("PHS Publication Interactive Dashboard"),
               tags$br(),
               tags$b("This dashboard summarizes the number of PHS publications:"),
               tags$br(),
               tags$br(),
               tags$ul(
                tags$li(
                  tags$b(
                    actionLink("tabpanel_pub_charts", "Publication charts")),
                           icon("bar-chart"),
                           " - explore the publications by date/theme"),
                tags$li(
                 tags$b(
                    actionLink("tabpanel_all_pubs", "All publications")),
                               icon("table"),
                               "- shows the list of publications") 
                        ),
                p("Charts and data react to the filters in the dropdown menus. 
                   The filtered data can be downloaded as a csv file by using the `download publication list` button above the tables."),
                HTML('More information can be found on the 
                    <b> <a href="https://publichealthscotland.scot/latest-publications" 
                    target="_blank">PHS website</a> </b> (opens in new tab)'),
                tags$br(),
                tags$p("For any questions or feedback about the PHS Publication Interactive Dashboard, 
                        please contact the team at:",
                        HTML('<b> <a href="mailto:phs.statsgov@nhs.net">phs.statsgov@nhs.net</a></b>.'))
            ),
    #pubs by year tab
    tabPanel(title = "Publication charts",
             icon = icon("bar-chart"),
             wellPanel(column(3, pickerInput(inputId = "filter_year",
                                            label = "Filter by publication year(s)", 
                                            choices = yrs, 
                                            selected = yrs,
                                            multiple = TRUE,
                                            options = list(`actions-box` = TRUE))),
                      column(3, pickerInput(inputId = "filter_theme",
                                            label = "Filter by publication theme(s)",
                                            choices = theme, 
                                            selected = theme,
                                            multiple = TRUE,
                                            options = list(`actions-box` = TRUE))),
                      column(3, dateRangeInput(inputId = "filter_date_range",
                                              label = "Filter by date range",
                                              start = min(df$published),
                                              end = today()))
                    ),
            mainPanel(width = 12,
                        h1(htmlOutput("pub_count")),
                        p(glue("Data updated: {today()}")),
                        fluidRow(column(6, h4("Publications by year")),
                                column(6, h4("Publications by theme"))),
                        fluidRow(column(6, plotlyOutput("plot_yr")),
                                column(6, plotlyOutput("plot_theme"))),
                        fluidRow(column(6, HTML("<button data-toggle = 'collapse' href = '#hide_data_year' class='btn btn-primary'>
                                                <strong>Show/hide data</strong></button>"),
                                          HTML("<div id = 'hide_data_year' class = 'collapse'> "),
                                          downloadButton("download_year", "Download data"),
                                          dataTableOutput("tab_year"),
                                          HTML("</div>")),
                                  column(6, HTML("<button data-toggle = 'collapse' href = '#hide_data_theme' class='btn btn-primary'>
                                                 <strong>Show/hide data</strong></button>"),
                                          HTML("<div id = 'hide_data_theme' class = 'collapse'> "),
                                          downloadButton("download_theme", "Download data"),
                                          dataTableOutput("tab_theme"),
                                          HTML("</div>"))))
          ),
                         
    #show all pubs
    tabPanel(title = "All publications",
             icon = icon("table"),
      mainPanel(width = 12,
         downloadButton("download_pubs", "download publication list"),
               DT::dataTableOutput("all_input_data")))
#)
#)
),
  
  #designate page end
  HTML('<div data-iframe-height></div>')
  
  )
