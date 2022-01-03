## Covid19 analysis
## Chia F Lee (cflee23@gmail.com)

# "2021-01-01"
# Load packages ----
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(data.table)
library(shinythemes)
library(shinydashboard)
library(flexdashboard)
library(dashboardthemes)
library(shinycssloaders)
library(shinyWidgets)
library(htmltools)
library(leaflet)
library(maps)
library(lattice)
library(plotly)

rm(list=ls())
# Source helpers ----
source("helpers.R")

#@      ui starts here      #

ui <- #shinyUI (
  dashboardPage(
    dashboardHeader(
      title = span(
        img('HeyMi',src = 'heymi.png')
      )),
    dashboardSidebar(
      collapsed = F,
      width = 220,
      sidebarMenu(
        menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
        menuItem("Date Range", icon = icon("spinner"), tabName = "menu_date",
                 sliderInput("dates", 
                             label = "Update starting date",  
                             value = range(as.Date(data_globe$Date)), 
                             min = as.Date("2020-1-22"), 
                             max = as.Date(Sys.Date()), step = 5
                 )),
        div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
        menuItem("Trend", tabName = "tab_trend", icon = icon("star")),
        menuItem(
          "Pick a Region",
          tabName = "Delme1",
          icon = icon("spinner"),
          checkboxGroupInput(
            inputId = "Continents",
            label = "",
            choices = sort(unique(data_globe$continent_level)),
            selected = unique(data_globe$continent_level)
          ),
          checkboxGroupInput(
            inputId = "Countries",
            label = "Select Countries:",
            choices = c("Taiwan", "Japan","Spain", "Mexico"),
            selected = c("Taiwan", "Japan","Spain", "Mexico")
          ),
          checkboxInput("legend", "Show legend", FALSE)
        ),
        menuItem("Date Range", icon = icon("spinner"), tabName = "menu_date",
                 sliderInput("dates", 
                             label = "Update starting date",  
                             value = range(as.Date(data_globe$Date)), 
                             min = as.Date("2020-1-22"), 
                             max = as.Date(Sys.Date()), step = 5
                             )),
        menuItem("Download Data", icon = icon("spinner"), tabName = "Download Data",
                 downloadButton("Downloadx", "Download")),
        
        div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
        menuItem("About", icon = icon("question-circle-o"), tabName = "menu_about")
        #downloadButton("Downloadx", "Download")
    
      )
    ),
    
    dashboardBody(
      tabItems(
      # Frontpage - tab_dashboard -----------------------------------------------
      tabItem(
        "tab_dashboard",
        fluidRow(
          # Frontpage - boxes - start -----------------------------------------------
          infoBoxOutput("total_today"),
          infoBoxOutput("TW_today"),
          infoBoxOutput("top1"),
          infoBoxOutput("top2"),
          infoBoxOutput("top3"),
          infoBoxOutput("top4")
          # Frontpage - boxes - end -------------------------------------------------
        ),
        fluidRow(
          # Frontpage - tweet volume plots - start ----------------------------------
          tabBox(
            width = 12,
            #tabPanel("World Map", leafletOutput("map")%>% withSpinner(type = 4)),
            tabPanel("Map",
              box(
                status = "info", width = 9,leafletOutput("map")%>% withSpinner(type = 5)
              ),
              box(
                status = "success", width = 3,plotOutput('plot')%>% withSpinner(type = 5)
              )),
            tabPanel("Top-trend", 
                     plotOutput('hist')%>% withSpinner(type = 4),
                     plotOutput('dash_plot')%>% withSpinner(type = 4))
          )
          # Frontpage - tweet volume plots - end ------------------------------------
        )),
      tabItem(
        "tab_trend",
        tabPanel("",
                 tabBox(title = "", width = 12, 
                        id = "tabset1",
                        
                        tabPanel("World", plotOutput('plot_1')%>% withSpinner(type = 5)),
                        tabPanel("Data ",DT::dataTableOutput("table")%>% withSpinner(type = 5))
                 )
        ))
      )))

### server
server <- function(input, output, session) {
  
  # Empty reactive values object
  reactive_objects=reactiveValues()
  data_value   <- reactive({data_globe})
  data_map     <- reactive({ na.omit(map_info)})
  World_ncases <- reactive({World_new_cases})
  top_ncases   <- reactive({top_pct})
  daily_ncases   <- reactive({daily1})
  
  # ------------- Start data processing ------------
  # create a subset of data filtering
  dataInput <- reactive({
    data_globe <-
      data_globe %>% 
      mutate(Date = as.Date(Date))%>%
      filter(as.Date(Date) > min(as.Date(input$dates)))
    
    s_countries <- data_globe[data_globe$country  %in% input$Countries, ]
    
    data_globe <-
      data_globe %>% 
      filter(continent_level %in% input$Continents)%>% 
      filter(!(country %in% input$Countries)) # remove countries already in the target list
    
    
    data_globe <- rbind(data_globe,s_countries) 
    data_globe <- reshape2::dcast(data_globe, Date ~  country , value.var="case")
    data_globe <- na.omit(data_globe)
    # add target countries to compare
    select_col <- which(names(data_globe) %in% input$Countries)
    target_    <- na.omit(c(select_col[1],select_col[2],select_col[3],select_col[4]))
    
    data_globe <- data_globe %>% 
      mutate(Total_case = as.numeric(rowSums(dplyr::select(.,-Date))))%>%
      mutate(Daily_case = c(NA,diff(Total_case,1)))%>%
      rowwise() %>%
      mutate(Total_target_case = as.numeric(sum(c_across(target_))))

    data_globe$Daily_target_case <- c(NA,diff(data_globe$Total_target_case,1))
    data_globe
    
  })
  
  dataInputx <- reactive({
    data_globe <-
      data_globe %>% 
      mutate(Date = as.Date(Date))%>%
      filter(as.Date(Date) > min(as.Date(input$dates)))%>%
      group_by(Date, continent_level) %>%
      mutate(sum_case = as.numeric(sum(case)))%>%
      dplyr::select(Date,continent_level,sum_case)
    
    data_globe <- na.omit(data_globe) 

    data_globe <- reshape2::dcast(data_globe, Date ~ continent_level, value.var="sum_case",fun.aggregate = sum)
    
    target_    <- c("Asia" ,"Europe", "Africa","North America", "South America", "Oceania" )
    
    data_globe <- data_globe %>% 
      mutate(Total_case = as.numeric(rowSums(dplyr::select(.,-Date))))%>%
      mutate(Daily_case = c(NA,diff(Total_case,1)))%>%
      mutate(New_Asia = c(NA,diff(Asia,1)))%>%
      mutate(New_Europe = c(NA,diff(Europe,1)))%>%
      mutate(New_Africa = c(NA,diff(Africa,1)))%>%
      mutate(`New_North America` = c(NA,diff(`North America`,1)))%>%
      mutate(`New_South America` = c(NA,diff(`South America`,1)))%>%
      mutate(New_Oceania = c(NA,diff(Oceania,1)))
    lst1       <- c("Date",grep("New_",names(data_globe) ,value = TRUE) )
    data_globe <- data_globe[,lst1]
    colnames(data_globe) <- sub("New_", "", names(data_globe))
    data_globe           <-  reshape2::melt(data_globe , id = 'Date')
    colnames(data_globe) <- c('Date', 'continent_level', 'new_case' )
    data_globe
    
  })
  
  dataInputy <- reactive({
    data_globe <-
      data_globe %>% 
      mutate(Date = as.Date(Date))%>%
      filter(as.Date(Date) > min(as.Date(input$dates)))%>%
      group_by(Date, continent_level) %>%
      mutate(sum_case = as.numeric(sum(case)))%>%
      dplyr::select(Date,continent_level,sum_case)
    
    data_globe <- na.omit(data_globe) 
    data_globe
    
  })
  
  # --------------- Update value box  ---------------
  data_value <- reactive({data_globe})
  
  output$total_today <- renderInfoBox({
    
    #world <- World_ncases()
    
    infoBox(
      "Daily World Case :" ,World_ncases(), icon = icon("list"),
      color = "blue")
  })
  
  output$top1<- renderInfoBox({
    country_  <- top_ncases()$country[1]
    new_case  <- top_ncases()$case[1] 
    changex   <- top_ncases()$change[1]
    a <- ifelse(as.numeric(changex) > 0, " % increase", ' % decrease')
  
    infoBox(
      country_ , paste(as.numeric(new_case), "cases"), 
      paste(abs(as.numeric(changex)) , a), icon = icon("list"),
      color = "maroon", fill = TRUE
    )
    
  })
  output$top2<- renderInfoBox({
    country_  <- top_ncases()$country[2]
    new_case  <- top_ncases()$case[2] 
    changex   <- round(as.numeric(top_ncases()$change[2]) ,1)
    a <- ifelse(as.numeric(changex) > 0, " % increase", ' % decrease')
    
    infoBox(
      country_ , paste(as.numeric(new_case), "cases"), 
      paste(abs(as.numeric(changex)) , a), icon = icon("list"),
      color = "red", fill = TRUE
    )
  })
  output$top3<- renderInfoBox({
    country_  <- top_ncases()$country[3]
    new_case  <- top_ncases()$case[3] 
    changex   <- round(as.numeric(top_ncases()$change[3]) ,1)
    a <- ifelse(as.numeric(changex) > 0, " % increase", ' % decrease')
    
    infoBox(
      country_ , paste(as.numeric(new_case), "cases"), 
      paste(abs(as.numeric(changex)) , a), icon = icon("list"),
      color = "orange", fill = TRUE
    )
  })
  output$top4<- renderInfoBox({
    country_  <- top_ncases()$country[4]
    new_case  <- top_ncases()$case[4] 
    changex   <- round(as.numeric(top_ncases()$change[4]) ,1)
    a <- ifelse(as.numeric(changex) > 0, " % increase", ' % decrease')
    
    infoBox(
      country_ , paste(as.numeric(new_case), "cases"), 
      paste(abs(as.numeric(changex)) , a), icon = icon("list"),
      color = "yellow", fill = TRUE
    )
  })
  output$TW_today<- renderInfoBox({
    country_  <- top_ncases()$country[5]
    new_case  <- top_ncases()$case[5] 
    changex   <- round(as.numeric(top_ncases()$change[4]) ,1)
    a <- ifelse(as.numeric(changex) > 0, " % increase", ' % decrease')
    
    infoBox(
      country_ , paste(as.numeric(new_case), "cases"), 
      paste(abs(as.numeric(changex)) , a), icon = icon("list"),
      color = "green"
    )
  })

  # ----------------- plotly output ---------------
  output$dash_plot <- renderPlot({
  new_us <- 
    data_value() %>% 
    mutate(Date = as.Date(Date))%>%
    filter(as.Date(Date) > min(as.Date(input$dates)))%>%
    select(Date, case,country)%>%
    dplyr::group_by(Date,country)%>%
    mutate(sum_case = as.numeric(sum(case)))%>%
    select(-case)
  
  lst <- top_ncases()$country
  
  plotm <- function(n){
    new_us%>%
      filter(country == lst[n])%>%
      ggplot(aes(x = Date))+    
      geom_line(aes(y = sum_case),size=0.8)+
      geom_vline(xintercept=c(as.Date("2020-12-01"),as.Date("2021-11-15")), linetype='dashed', color=c('blue', 'red')) +
      ggtitle(lst[n]) +
      ylab("Number of Cases")+
      theme_minimal()
  }
  
  a <- plotm(1)
  b <- plotm(2)
  c  <- plotm(3)
  d  <- plotm(4)
  e  <- plotm(5)
  
  multiplot(a, b, c, d, e ,cols = 5)

  })
  
  # ------------ Output Histogram ---------------
  output$hist <- renderPlot({
    dt <- daily_ncases()
    dt$country <- factor(dt$country, levels=unique(dt$country))
    gg <- ggplot(dt, aes(x=country, y=case)) + 
      geom_bar(stat = "identity", fill="#336699", colour="black")+
      #geom_bar(aes(fill = country), position = "dodge", stat="identity")+
      #geom_line(aes(x=country, y=week_avg, color = "7-day average new cases"),stat="identity", group = 1) + scale_color_manual(values="orange") +
     # geom_line(aes(x=country,y = week_avg, group = 1, color = '7-day average new cases ')) +
      #geom_text(aes(x=country,y = week_avg, label = round(week_avg, 0)), vjust = 1.4, size = 3) +
      labs(title= "Countries with most reported new COVID-19 cases",x="Country",y="Cases")+
      theme_minimal()
    gg

  })
  
  
  # ------------- Output map and plots ------------
  # Map set up
  output$map <- renderLeaflet({
    pal <- colorpal()
    radius <- 400000/(1+as.numeric(data_map()$Past_ndays_count)) 
    leaflet(data_map()) %>%
      addTiles() %>%
      setView(lng = 120, lat = 23, zoom = 2)%>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~country,
                 stroke=FALSE, fillOpacity=0.4, fillColor= ~pal(Past_ndays_count)) 
  })
  
  # Show a popup at the given location
  showCountryPopup <- function(Country, lat, lng) {
    df   <- data_map()
    data <- df[df$country == Country,]
    content <- as.character(tagList(
      tags$h4(Country),
      tags$strong(HTML(sprintf(paste("New Cases:",as.character(data$day_7))))), tags$br(),
      tags$strong(HTML(sprintf(paste("7 Day average cases:",as.character(round(data$Past_ndays,1)))))), tags$br(),
      sprintf(paste("Population:", as.character(data$population) )), tags$br(),
      sprintf(paste("Infection: ", as.character(round(10000*(data$pop_infect),2)),"per 10,000 people")
    )))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = Country)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showCountryPopup(event$id, event$lat, event$lng)
     })
  })
  
  colorpal <- reactive({
    colorNumeric(palette = 'Spectral', data_map()$Past_ndays_count)
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = data_map())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomleft",
                          pal = pal, values = ~Past_ndays_count
      )
    }
  })
  
  # Map marker click (to identify selected country)
  observe({
    country_click <- input$map_shape_click
    country_click$id <- ifelse(is.null(input$map_shape_click), "Taiwan",input$map_shape_click$id)
    #if (is.null(country_click)){return()}
    siteid = country_click$id
    reactive_objects$country = siteid
  })
  
  output$plot <- renderPlot({
    dt <- dataInput() %>%
      select(Date, reactive_objects$country)%>%
      mutate_at(c(2), as.numeric)
    colnames(dt)[2] <- 'A'
    dt$Daily_case <- c(NA,diff(dt$A,1))
    max_case <- dt[which.max(dt$Daily_case),'Daily_case'] 
    max_date <- as.Date(as.numeric(dt[which.max(dt$Daily_case),'Date']), origin = "1970-01-01" ) 
    a <- ggplot(dt, aes(x = Date))+    
      geom_line(aes(y = A),size=0.5, color = "#cc4c02")+
      ggtitle(paste0("Accumulated COVID-19 in ",reactive_objects$country )) +
      ylab("Cases")+
      geom_vline(xintercept=c(as.Date("2020-12-01"),as.Date("2021-11-15")), linetype='dashed', color=c('blue', 'red'))
      theme_minimal()

    
    b <-  ggplot(dt, aes(x = Date))+   
      geom_line(aes(y= Daily_case),color="#69b3a2") +

      ggtitle(paste0("New COVID-19 cases  in ", reactive_objects$country )) +
      ylab("Cases")+
      geom_vline(xintercept=c(as.Date("2020-12-01"),as.Date("2021-11-15")), linetype='dashed', color=c('blue', 'red')) +
      xlab(paste0('Max new cases: ', max_case , " on ", max_date))+
      theme_minimal() 
    
    multiplot(a, b, cols = 1)
    
    
  })
  output$plot_1 <- renderPlot({
    
    filter_df <-
      dataInputx() %>% 
      filter(continent_level %in% input$Continents)

    a <- ggplot(filter_df, aes(x = Date))+    
      geom_line(aes(y = new_case, group = continent_level, colour = continent_level),size=0.8)+
      geom_vline(xintercept=c(as.Date("2020-12-01"),as.Date("2021-11-15")), linetype='dashed', color=c('blue', 'red')) +
      ggtitle("New COVID-19 cases") +
      ylab("Number of Cases")+
      theme_minimal()
    
    filter_dt <-
      dataInputy() %>% 
      filter(continent_level %in% input$Continents)
    

    b <- ggplot(filter_dt, aes(x = Date))+    
      geom_line(aes(y = sum_case, group = continent_level, colour = continent_level),size=0.8)+
      
      geom_vline(xintercept=c(as.Date("2020-12-01"),as.Date("2021-11-15")), linetype='dashed', color=c('blue', 'red')) +
      ggtitle("Total COVID-19 cases ") +
      ylab("Number of Cases")+
      theme_minimal()

    multiplot(a, b, cols = 1)
  })

  
  output$table <- DT::renderDataTable({

    DT::datatable(data = data_map(),
                  options = list(pageLength = 10, rownames = FALSE) 
    )  
  })

  
  output$Downloadx <- downloadHandler(
    filename = function(){"output_data.csv"},
    content = function(fname){
      write.csv(data_map(), fname, row.names = FALSE)
    }
  )
}
 
##### end server

shinyApp(ui,server)