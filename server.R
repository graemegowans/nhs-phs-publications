#***************************************
# server logic
#***************************************
function(input, output, session) {
  
  #***************************************
  # dynamic links to tabs
  #***************************************
  
  observeEvent(input$tabpanel_pub_charts, {
    updateTabsetPanel(session,
                      "Panels",
                      selected = "Publication charts")
  })
  
  observeEvent(input$tabpanel_all_pubs, {
    updateTabsetPanel(session,
                      "Panels",
                      selected = "All publications")
  })
  
  #***************************************
  # data processing and table generation
  #***************************************
  
  #filter data for year tab
  df_filtered <- reactive({
    df %>% 
      filter(year_published %in% input$filter_year,
             theme %in% input$filter_theme,
             between(published, input$filter_date_range[1], input$filter_date_range[2])
             ) %>% 
      mutate(theme = fct_drop(theme),
            year_published = fct_drop(year_published))
  })
  
  count_year <- reactive({
    count(df_filtered(), year_published, name = "number") %>% 
      arrange(desc(year_published))
  })
  
  count_theme <- reactive({
    count(df_filtered(), theme, name = "number") %>% arrange(desc(number))
  })

  #display counts of pubs after filter
  output$pub_count <- renderText({glue("Total number of publications included: 
                                       <font color='{{font_col}}'> {{nrow(df_filtered())}} </font>", 
                                       .open = "{{", .close = "}}")})
  
  #all pub table  
  output$all_input_data <- DT::renderDataTable({df %>% select(-year_published, -month_published)}, 
                                               escape = FALSE, 
                                               rownames = FALSE, 
                                               filter = "top",
                                               style = "bootstrap",
                                               options = list(pageLength = 20))

  #year data table
  output$tab_year <- DT::renderDataTable({count_year()},
                                         escape = FALSE, 
                                         rownames = FALSE,
                                         filter = "top",
                                         style = "bootstrap")
  
  #theme data table
  output$tab_theme <- DT::renderDataTable({count_theme()},
                                          escape = FALSE, 
                                          rownames = FALSE,
                                          filter = "top",
                                          style = "bootstrap")
  
  #display filtered data under each chart
  output$selected_input_data <- DT::renderDataTable({df_filtered() %>% select(-year_published, -month_published)}, escape = FALSE, rownames = FALSE)
  
  #***************************************
  # generate plots####
  #***************************************
  
  #plot by year 
  output$plot_yr <- renderPlotly({
  
  validate(need(input$filter_year != "", "Please select a year to display"),
          need(input$filter_theme != "", "Please select a theme to display"),
          val_func(df_filtered()))
  
  plot_year_bars(count_year())})
  
  #plot by theme
  output$plot_theme <- renderPlotly({
  
  validate(need(input$filter_year != "", "Please select a year to display"),
           need(input$filter_theme != "", "Please select a theme to display"),
           val_func(df_filtered()))
  
  plot_theme_bars(count_theme())})

#output$plot_month <- renderPlotly({
  
#  validate(need(input$filter_year != "", "Please select a year to display"),
#           need(input$filter_theme != "", "Please select a theme to display"),
#           val_func(df_filtered()))
#  
#  plot_month(df_filtered())}) 

  #***************************************
  # downloads
  #***************************************
  
  #control downloading of publication list
  #download all data
  output$download_pubs <- downloadHandler(
    filename =  function() {
      glue("{today()}_PHS_publications.csv")
    },
    #content is a function with argument file, writes the table
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  #download the year data
  output$download_year <- downloadHandler(
    filename =  function() {
      glue("{today()}_filtered_PHS_publications.csv")
    },
    #content is a function with argument file, writes the table
    content = function(file) {
      write.csv(count_year(), file, row.names = FALSE)
    }
  )
  
  #download the theme data
  output$download_theme <- downloadHandler(
    filename =  function() {
      glue("{today()}_filtered_PHS_publications.csv")
    },
    #content is a function with argument file, writes the table
    content = function(file) {
      write.csv(count_theme(), file, row.names = FALSE)
    }
  )
  
  #when browser closes then stop the app
  session$onSessionEnded(stopApp)
  
}