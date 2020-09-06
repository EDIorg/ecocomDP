# library(shiny)
# library(neonUtilities)
# library(dplyr)
# library(devtools)
# library(tidyverse)
# library(ecocomDP)
# 
# library(plotly)
# library(shinyWidgets)
# library(shinythemes)



#' Explore data shiny app
#'
#' @description  
#'     Wrapper function to run the explore data shiny app locally
#' 
#' @param my_NEON_TOKEN (character, default: NA_character_) NEON API token. If none is provided, the function will check if you have an API token stored as a system environmental variable set up using the directions here: https://www.neonscience.org/neon-api-tokens-tutorial
#' @param my_NEON_TOKEN_name (character, default: "NEON_TOKEN") Name of system environmental variable where the function will check for a NEON API token
#'
#' @return 
#'     Function call runs the shiny app, does not return an R object
#'
#' @examples 
#' \dontrun{
#' # Run explore data shiny app
#' ecocomDP::explore_data_app()
#' }
#' 
#' @export
explore_data_app <- function(
  my_NEON_TOKEN = NA_character_, 
  my_NEON_TOKEN_name = "NEON_TOKEN"){
  
  
  # check for NEON TOKEN in Sys env
  if(is.na(my_NEON_TOKEN)){
    env_value <- Sys.getenv(my_NEON_TOKEN_name)
    if(nchar(env_value) > 0) my_NEON_TOKEN <- env_value
    rm(env_value)
  }
  
  
  #options(shiny.reactlog = TRUE)
  # Define UI ----
  ui <- shiny::fluidPage(
    #background color
    shinyWidgets::setBackgroundColor(color = "#e6f5ff",
                                     gradient = "linear",
                                     direction = "bottom"),
    
    shiny::titlePanel("Explore ecocomDP Data"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        
        shiny::br(),
        
        shiny::textInput("search_value", shiny::h5("Search data"), 
                         value = "macroinvertebrate"),
        shiny::helpText("See 'Help' tab for more information."),
        
        shinyWidgets::actionBttn(
          inputId = "search_button",
          label = "Search",
          style = "jelly",
          color = "default"),
        
        
        shiny::uiOutput("id"),
        
        shiny::conditionalPanel(
          condition = "input.search_button >= '1'",
          shiny::uiOutput("start")
        ),
        
        shiny::conditionalPanel(
          condition = "input.search_button >= '1'",
          shiny::uiOutput("end")
        ),
        
        shiny::conditionalPanel(
          condition = "input.search_button >= '1'",
          shiny::uiOutput("sites") 
        ),
        
        shiny::conditionalPanel(
          condition = "input.search_button >= '1'",
          shiny::uiOutput("go_button")
        ),
        
        shiny::p(),
      ), #end of sidebarPanel
      
      shiny::mainPanel(
        
        shiny::tabsetPanel(type = "tabs",
                           shiny::tabPanel("Loaded Data Package",
                                           shiny::p(),
                                           shiny::textOutput("id_message"),
                           ), #end summary tabPanel
                           shiny::tabPanel("Explore Data",
                                           shiny::br(),
                                           shiny::textOutput("description"),
                                           shiny::uiOutput("x"),
                                           shiny::uiOutput("color"),
                                           shiny::uiOutput("type_graph"),
                                           shiny::radioButtons("y_transform", label = "Transform y-axis",
                                                               choices = list("none" = 1, "log (x+1)" = 2), 
                                                               selected = 1),
                                           shiny::uiOutput("graph_button"),
                                           shiny::br(),
                                           shiny::br(),
                                           shiny::br(),
                                           shiny::conditionalPanel(
                                             condition = "input.graph_button >= '1'",
                                             plotly::plotlyOutput("summary_plot", width = 800, height = 500, inline = TRUE)
                                           ) # end if conditionalPanel
                           ), # end of tabPanel
                           shiny::tabPanel("Explore Richness", 
                                           shiny::br(),
                                           shiny::uiOutput("x_richness"),
                                           shiny::uiOutput("color_richness"),
                                           shiny::uiOutput("graph_button_richness"),
                                           shiny::br(),
                                           shiny::br(),
                                           shiny::br(),
                                           shiny::conditionalPanel(
                                             condition = "input.graph_button_richness >= '1'",
                                             plotly::plotlyOutput("richness_plot", width = 800, height = 500, inline = TRUE)
                                           ) #end of conditionalPanel
                           ), # end of tabPanel
                           shiny::tabPanel("Help",
                                           shiny::br(),
                                           shiny::p("This Shiny App was built to allow you, the user, to easily look at the biodiversity data",
                                                    "from EDI and NEON. The data coming from EDI, or Environmental Data Initiative, is LTER data, ",
                                                    "which has up to 40 years of observations. NEON's data is the observational, biodiversity data, ",
                                                    "and can also be found at NEON's Data Portal online."),
                                           shiny::br(),
                                           shiny::h4("Inputs"),
                                           shiny::h5("Search Bar"),
                                           shiny::p("Type any word from the title, description, or abstract of the EDI or NEON data. ",
                                                    "Regular expression syntax is accepted. Press the 'Search' button when you're done."),
                                           shiny::h5("Select Bar: ID"),
                                           shiny::p("This spits out EDI and NEON data. ",
                                                    "Select the data set that you wish to interact with. ",
                                                    "For EDI, the whole data set is downloaded, so press 'Download Data' after selecting data."),
                                           shiny::h5("Options: start date, end date, and sites"),
                                           shiny::p("When there is a NEON selection, you will have the ability to input a start and end date (yyyy-mm)",
                                                    "and ability choose up to all sites. When at least one site is selected, press 'Download Data.'"),
                                           shiny::br(),
                                           shiny::br(),
                                           shiny::h4("Tools for you"),
                                           shiny::h5("Explore Data"),
                                           shiny::p("Here you are given an interactive graph where the y value is a fixed value. ",
                                                    "Choose your x axis , a value to group the data by, and the type of graph ",
                                                    "you wish to use. A bar plot shows the accumulative value, and the box plot ",
                                                    "displays a summary of the values. Press 'Generate Graph' when you are ready. "),
                                           shiny::h5("Explore Richness"),
                                           shiny::p("Richness is the number of unique species in an area. This interactive graph, has a fixed y value (richness). ",
                                                    "Choose your x axis and choice to ",
                                                    "group the data by. Press 'Generate Graph' when you are ready. ")
                                           
                           )# end of tabPanel
                           
                           
        ) # end of tabsetPanel
        
      ) # end mainPanel
    )# end of sidebarLayout
    
  ) # end of ui
  
  
  # Define server logic ----
  server <- function(input, output, session){
    
    output$id_message <- shiny::renderText({
      
      all_tables <- pulled_data()
      
      retrieved_id <- names(all_tables)
      
      return(retrieved_id)  
    })# end of output$id_message
    
    
    # Tab buttons ----
    
    output$graph_button <- shiny::renderUI({
      all_tables <- pulled_data()
      shinyWidgets::actionBttn(
        inputId = "graph_button",
        label = "Generate Graph",
        style = "jelly",
        color = "default")
    })
    
    output$type_graph <- shiny::renderUI({
      all_tables <- pulled_data()
      shiny::selectInput("type_graph", h5("Type of graph"), 
                  choices = c(
                    # "bar", 
                    "box",
                    "scatter")) #selectInput
    })
    
    output$graph_button_richness <- shiny::renderUI({
      all_tables <- pulled_data() 
      shinyWidgets::actionBttn(
        inputId = "graph_button_richness",
        label = "Generate Graph",
        style = "jelly",
        color = "default")
    })
    
    # NEON options ----
    pulled_data <- shiny::eventReactive( 
      input$go,{
        shiny::withProgress(message = 'Loading data',
                            detail= "This may take a while...",{
                              if(if_dp1()){
                                # get user inputs
                                shiny::req(only_id())
                                shiny::req(input$sites)
                                shiny::req(input$start)
                                shiny::req(input$end)
                                
                                # get neon data
                                list_of_tables <- ecocomDP::read_data(
                                  id = only_id(),
                                  site = input$sites,
                                  startdate = input$start,
                                  enddate = input$end,
                                  token = my_NEON_TOKEN)
                                
                                
                              }else{
                                
                                
                                list_of_tables <- ecocomDP::read_data(
                                  id = only_id()
                                )
                                
                                
                              } # end of else
                            }) # end of withProgress
        return(list_of_tables)
        
      })# end of pulled_data
    
    full_name <- shiny::eventReactive(
      input$search_button,{
        all_searches <- ecocomDP::search_data(input$search_value)
        all_searches <- all_searches  %>% 
          dplyr::select(id,title) %>% 
          tidyr::unite("id_and_title",id,title,sep = ":   ")
        return(all_searches)
      }
    )
    
    output$id <- shiny::renderUI({
      
      shiny::selectInput("id_choices", h5("Select id"), 
                         choices = full_name())
    })
    
    if_dp1 <- shiny::reactive({
      req(input$id_choices)
      if(grepl("DP1",input$id_choices)){
        return(TRUE)
      }
      else
        return(FALSE)
    }) # if_dp1
    
    output$start <- shiny::renderUI({
      if(if_dp1()){
        shiny::textInput("start", h5("Start date"), 
                         value = "2017-01")
      }
      
    })
    
    output$end <- shiny::renderUI({
      if(if_dp1()){
        shiny::textInput("end", h5("End date"), 
                         value = "2020-01")
      }
    })
    
    get_list_of_sites <- shiny::reactive({
      info <- neonUtilities::getProductInfo(
        dpID = only_id(),
        token = my_NEON_TOKEN)
      sites <- info$siteCodes$siteCode
      return(sites)
    })
    
    output$sites <- shiny::renderUI({
      if(if_dp1()){
        shinyWidgets::pickerInput(
          inputId = "sites", 
          label = shiny::h5("Selected sites (select items)"), 
          choices = c(get_list_of_sites()),
          options = list(`selected-text-format` = "count > 1"),
          multiple = TRUE)
      }
    })
    
    only_id <- shiny::eventReactive(
      input$id_choices,{
        id <- gsub(":.*","",input$id_choices)
        return(id)
      })
    
    # Go button ----
    
    output$go_button <- shiny::renderUI({
      if(!if_dp1())
        shinyWidgets::actionBttn(
          inputId = "go",
          label = "Load Data",
          style = "jelly",
          color = "default")
      else if(length(input$sites)>=1)
        shinyWidgets::actionBttn(
          inputId = "go",
          label = "Load Data",
          style = "jelly",
          color = "default")
    })
    
    # Flatten Tables ----
    
    flatten_tables <- shiny::reactive({
      
      # get data
      all_data <- pulled_data()
      
      # merge and flatted data package
      all_merged <- all_data[[1]]$tables %>% ecocomDP::flatten_ecocomDP()
      
      
      # check date_time format
      all_merged <- all_merged %>% 
        tidyr::separate(col = observation_datetime, 
                        into = c('year','month','day_time'), 
                        sep = "-",
                        extra = "drop",
                        remove = FALSE)
      
      return(all_merged)
      
    }) # end of flatten_tables
    
    
    # make table ----  make data table for plotting
    # rename "value" column
    
    make_table <- shiny::reactive({
      
      grouped_summary <- flatten_tables()
      
      names(grouped_summary)[names(grouped_summary)=="value"] <- 
        paste(grouped_summary$variable_name[1],grouped_summary$unit[1], sep = " in ")
      
      return (grouped_summary)
    })
    
    
    
    # # format data for richness
    # make_richness_data <- reactive({
    #   
    #   all_tables <- flatten_tables()
    #   
    #   richness <- all_tables %>% 
    #     separate(observation_datetime,c('year','month','day_time'),sep = "-") 
    #   
    #   return(richness)
    # })
    
    
    
    # Interactive Normal Graph ----
    
    output$description <- shiny::renderText({
      
      data <- make_table()
      
      return(paste("Fixed y value: ", data$variable_name[1]," (", data$unit[1], ")", sep = ""))
      
    })
    
    # get x variable
    output$x <- shiny::renderUI({
      x_values <- make_table() %>% 
        dplyr::select(-c(package_id, observation_id, unit, variable_name, day_time))
      
      shiny::selectInput("x", h5("X value"), choices = names(x_values),
                         selected = "year")
    })
    
    # get grouping variable
    output$color <- shiny::renderUI({
      grouped_summary <- make_table() %>% 
        dplyr::select(-c(observation_id, unit, day_time, variable_name))
      
      shiny::selectInput("color", h5("Group by"), 
                         choices = names(grouped_summary),
                         selected = "package_id") 
    })
    
    
    
    
    
    # make summary plot
    make_plot <- shiny::eventReactive(
      input$graph_button,{
        
        
        grouped_summary <- make_table() %>% as.data.frame()
        
        if(input$y_transform==1){
          y_to_plot <- grouped_summary[,paste(grouped_summary$variable_name[1],grouped_summary$unit[1], sep = " in ")]
          y_lab <- paste(grouped_summary$variable_name[1],grouped_summary$unit[1], sep = " in ")
        }else if(input$y_transform==2){
          y_to_plot <- log(
            grouped_summary[,paste(grouped_summary$variable_name[1],grouped_summary$unit[1], sep = " in ")] + 1)
          y_lab <- paste0("log ([",grouped_summary$variable_name[1]," in ",grouped_summary$unit[1],"] + 1)")
        }
        
        
        if(input$type_graph == "box"){
          fig <- plotly::plot_ly(
            x = grouped_summary[,input$x],
            y = y_to_plot,
            color = grouped_summary[,input$color],
            type = input$type_graph,
            boxpoints = FALSE) %>% 
            plotly::layout(
              boxmode = "group",
              xaxis = list(title = input$x),
              yaxis = list(title = y_lab))
        }else{
          fig <- plotly::plot_ly(
            x = grouped_summary[,input$x],
            y = y_to_plot,
            color = grouped_summary[,input$color],
            type = input$type_graph) %>% 
            plotly::layout(
              xaxis = list(title = input$x),
              yaxis = list(title = y_lab))
        }
        
        
        return(fig)
        
        
      }) # end of make_summary_plot
    
    output$summary_plot <- plotly::renderPlotly({
      make_plot()
    }) # end of summary_plot
    
    
    
    
    
    
    # Make Richness Graph ----
    
    
    
    # get x variable for plotting
    output$x_richness <- shiny::renderUI({
      x_values <- flatten_tables() %>% 
        dplyr::select(-c(package_id, observation_id, unit, variable_name, day_time))
      
      shiny::selectInput("x_richness", h5("X value"), choices = names(x_values),
                         selected = "year")
      
    })
    
    # get grouping variable for plotting
    output$color_richness <- shiny::renderUI({
      color_values <- flatten_tables() %>% 
        dplyr::select(-c(observation_id, unit, variable_name, day_time))
      
      shiny::selectInput("color_richness", h5("Group by"), choices = names(color_values),
                         selected = "package_id")
    })
    
    # make plot
    make_richness_plot <- shiny::eventReactive(
      input$graph_button_richness,{
        
        shiny::req(input$x_richness)
        shiny::req(input$color_richness)
        
        # summarize to get richness data by user defined groups
        richness_data <- flatten_tables() %>% 
          dplyr::group_by(.data[[input$color_richness]],.data[[input$x_richness]]) %>% 
          dplyr::summarise(taxon_id %>% unique() %>% length()) %>% 
          dplyr::rename(richness = "taxon_id %>% unique() %>% length()")
        
        
        fig3 <- plotly::plot_ly(
          x = richness_data[[input$x_richness]],
          y = richness_data$richness,
          color = richness_data[[input$color_richness]],
          type = "bar") %>% 
          plotly::layout( title = paste("Species Richness by ",input$x_richness, "and ", input$color_richness) ,
                          xaxis = list(title = input$x_richness),
                          yaxis = list(title = "richness"))
        
        return(fig3)
      })
    
    
    output$richness_plot <- plotly::renderPlotly({
      make_richness_plot()
    })
    
    
    
  } # end of server
  
  # Run the app ----
  shiny::shinyApp(ui = ui, server = server)
  
}
