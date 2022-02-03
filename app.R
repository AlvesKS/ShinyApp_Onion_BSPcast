# Packages -----

library(shiny)
library(shinyWidgets)
library(fresh)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(tidyverse)
library(cowplot)
library(plotly)
library(nasapower)
library(gbm)
library(readxl)
library(mgcv)

# theme -----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#B31B1B"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#222222",
    dark_hover_bg = "#B31B1B",
    dark_color = "#B31B1B"
  ),
  adminlte_global(
    content_bg = "#FFFFFF",
    box_bg = "#B31B1B", 
    info_box_bg = "#B31B1B"
  )
)

# functions-----------------------------------------------------------

BSP_model = function(W, Temp){
  log_S  = -1.70961637 + (0.02886124*Temp) +0.04943329*W +0.00868240*Temp*W -0.00236213*(W^2) -0.00023788*(Temp^2)*W
  
  R = 10^(log_S)/3.80864
  return(R)
}



# models --------------------------------------------------------------

gbm_np = readRDS("nasapower_model_wetness.RDS")
lm1 = readRDS("gam_model_np.RDS")


# locations -----------------------------------------------------------
field_loc = read_excel("field_loc.xlsx")
# field_loc =data.frame(
# fields = c("Mortellaro I", 
#            "Mortellaro II",
#            "Triple G",
#            "Bezon",
#            "Randon",
#            "S&SO",
#            "Datthyn",
#            "Disalvo"),
# 
# lon = c(-78.10985453,
#         -78.11643848,
#         -78.13469217,
#         -78.10722987,
#         -78.5,
#         -74.41809082835822,
#         -77.07551369279173,
#         -76.2549081875873),
# 
# lat = c(43.13038684,
#         43.13984844,
#         43.14194265,
#         43.15862743,
#         43.15862743,
#         41.31876147625445,
#         43.17231073762401,
#         43.33071676799995) )



# UI ----

header = dashboardHeader(title = "")

anchor <- tags$a(href='https://cornell.edu',
                 tags$img(src='logo.png', height = '40'))

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color:  }"))),
  anchor,
  class = 'name')



ui <- dashboardPage(title = "BSPcast",#skin = "red",

#Header ------------------------------------------------------------------------------------------------
  header =  header,


#Side bar ------------------------------------------------------------------------------------------------
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      
      ## 1st tab show the Main dashboard
      menuItem("BSPcast" , tabName = "main_page", icon = icon("chart-area")),
      
      
      ## 2nd Second tab shows information about the dashboard
      menuItem("About" , tabName = "about", icon = icon("info"))
      
    )
  ),



# APP Body  ------------------------------------------------------------------------------------------------
  dashboardBody(use_theme(mytheme),
                tags$head(
                  # tags$link(rel = "shortcut icon", href = "favicon.ico"),
                  # tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.ico"),
                  tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon.png"),
                  tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon.png")
                ),
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = "main_page",
              
# Body map -----
              fluidRow(
                column(width = 12,
                # h2(paste0("Select location")),
                       
                leafletOutput("map_people",
                              width = "100%",
                              height = 250) 
                )),
# Inputs  -----              
              fluidRow(
                
                column(
                  width = 4,
                  h2(paste0("Select period and field locations")),
                  
                  setSliderColor(sliderId =1,
                                 color = "#B31B1B"),
                  
                  sliderInput(inputId = "date_slide",
                              label = "Period",
                              min = Sys.Date()-90,
                              max = Sys.Date(),
                              value = c(Sys.Date()-60,Sys.Date()),
                              timeFormat="%b %d %Y"),
                  
                  selectizeInput(inputId = "location",
                                 label = "Locations",
                                 choices = field_loc$fields),
                  tags$head(
                    tags$style(HTML('#get_risk{background-color:#B31B1B;
                                              color:white}'))
                  ),
                  actionButton(inputId = "get_risk",
                               label = "Calculate risk"
                               )

                  
                  
                  ),
# Outputs  -----                 
                column(
                  width = 8,
                  
                  h2(paste0("Stemphylium daily risk of infection")),
                  
                 div(id = "prog",
                  progressBar(id = "pb",
                              value = 0,
                              size = "xxs",
                              display_pct = F)
                  ),
                  
                  
                  textOutput("meta_text"),
                 tags$head(tags$style("#meta_text{color: black;
                                 font-size: 15px;
                                 font-style: Bold;
                                 font-weight: bold
                                 }")),
                 tags$head(tags$style("#pb{background-color:#B31B1B}"
                 )
                 ),
                 
                 
                 tabsetPanel(type = "tabs",
                             tabPanel("Risk graph",
                               plotlyOutput(outputId = "risk_graph", height = 400), 
                             ),
                             tabPanel("Table",
                               DT::dataTableOutput(outputId = "table_risk")
                             )
                  
                  
                 )
                 )
              )
              ),
      
# About  -----       
      tabItem(tabName = "about",
              strong("About BSPcast"),
              p(),
              p("Description"),
              
              em("Shiny app developed by EVADE lab"))
    )
  )
  
)



# SERVER ----
server <- function(input, output, session) {

  # output$text = renderText({ paste(  field_click() ) })
  
  shinyjs::hide(id = "prog")
  # output$text_test <- renderText({ paste(input$date_slide)})
  observe({
    updateSliderInput(session,
                      inputId = "date_slide",
                      label = "Period",
                      min = input$date_slide[1]-90,
                      max = Sys.Date(),
                      value = c(input$date_slide[1],input$date_slide[2]),
                      timeFormat="%b %d %Y")
    
    
           }) 
  observe({
 
    
    updateSelectizeInput(session,
                         inputId = "location",
                         label = "Locations",
                         selected = field_click(),
                         choices = field_loc$fields)
    
  }) 
  
# Get data ---------------------------------------------------------------------  
  data_power = eventReactive(input$get_risk, {
    
    shinyjs::show(id = "prog")
    updateProgressBar(session = session,
                      id = "pb",
                      value = 5)  
      n <- 100
    
      # incProgress(1/n, detail = paste("Checking the weather"))
      
      selected_field = field_loc %>% 
      # filter(fields == "Triple G")
      filter(fields == input$location)
    
    power = get_power(community = 'ag',
                      pars = c("T2M", "RH2M","T2MDEW", "PRECTOTCORR","WS2M","QV2M"),
                      temporal_api = "hourly",
                      lonlat = c(selected_field$lon, selected_field$lat),
                      # dates = c("2021-06-02", "2021-09-20")
                      dates = c(input$date_slide[1], input$date_slide[2])
                      )%>%
      unite(date, YEAR, MO, DY, sep = "-") %>%
      mutate(date= as.Date(date))
    
    updateProgressBar(session = session,
                      id = "pb",
                      value = 80)
    
    wet_pred_nasa = (predict(gbm_np, power)>0.5)*1
    
    R3_nasa_df = power %>%
      mutate(temp = T2M,
             wet_positive = wet_pred_nasa)
    dates = unique(R3_nasa_df$date)
    box3 = data.frame()
    # i=4
    for(i in 1:length(dates)){
      daily = R3_nasa_df %>% 
        filter(date ==dates[i])
      
      
      temp_daily = daily %>% 
        filter(wet_positive>0)
      
      wet_per = sum(daily$wet_positive)
      mean_temp =mean(temp_daily$temp)
      
      R = BSP_model(W = wet_per, Temp = mean_temp)
      
      if(is.na(R)){
        R = 0
      }
      
      lil_box3 = data.frame(date = dates[i], R = R)
      
      box3 = box3 %>% 
        bind_rows(lil_box3)
      
    }
    
    R_nasawet_df_x = box3 %>% 
      mutate(sum_R = R + lag(R,1,)+lag(R,2)) %>% 
      mutate(R_prednasa = sum_R) 
      
    R_nasawet_df = R_nasawet_df_x %>% 
    mutate(sum_R = predict(lm1, R_nasawet_df_x ) ) %>% 
      dplyr::select(-R_prednasa) %>% 
      mutate(Recomendation = case_when(sum_R >0.5 ~ "Spray",
                               sum_R <=0.5 ~ "Standby"),
             location = selected_field$fields) 
    
    
    updateProgressBar(session = session,
                      id = "pb",
                      value = 100)
    
    R_nasawet_df

  })
  
# Map ---------------------------------------------------------------------
  map_people = createLeafletMap(session, 'map_people')
  
  session$onFlushed(once = T, function() {
    
        output$map_people = renderLeaflet({
          data = field_loc
          leaflet(data = data, width = "100%") %>%
            addCircleMarkers(
              layerId = ~fields,
              radius = 3,
              lng = ~lon,
              lat = ~lat,
              popup = paste("Field:",data$fields,"<br>",
              "Crop: Onion")
              ) %>% 
            setView(-77, 43, zoom = 8) %>%
            addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
            addProviderTiles("OpenTopoMap", group = "Terrain") %>%
            addScaleBar("bottomright") %>%
            addProviderTiles(providers$CartoDB.Voyager, group = "Default") %>%
            addLayersControl(
              baseGroups = c("Default", "Aerial", "Terrain"),
              # overlayGroups = "BSPcast - Onion",
              options = layersControlOptions(collapsed = T))
          
        })
    
  })
  
 field_click =  eventReactive(input$map_people_marker_click,{
    click <- input$map_people_marker_click
    if (is.null(click))
      return()
    
    # print(click)
    click$id
    
  })
  
# text over map --------------------------------------------------- 
 
 output$meta_text = renderText({ 
   if(is.null(data_power())) return(NULL)
   loc = unique(data_power()$location)
   min_Date = min(data_power()$date)
   max_Date =  max(data_power()$date)
   paste("Field: ",loc, " | ","Period: ",format(min_Date,"%b %d %Y"), " to ", format(max_Date,"%b %d %Y"))
   
 })

# Risk plot over time --------------------------------------------------- 
  
  output$risk_graph = renderPlotly({
    
    
    
    g = data_power() %>% 
      filter(!is.na(sum_R),
             !is.na(date)) %>% 
      rename(Risk = sum_R) %>% 
      mutate(Risk = round(Risk, 3)) %>% 
      ggplot(aes(date,Risk))+
      geom_hline(yintercept = c(0.5), size = c(0.6), color ="gray", linetype = "2")+
      # geom_errorbar(aes(ymin = 0, ymax = Risk,
      #                   color = Recomendation
      #                   ),
      #               width = 0,
      #               size = 0.1)+
      geom_step(color = "black", alpha =0.5, size = 0.2)+
      geom_point(
        aes(color = Recomendation),
        size = 1.5)+
      scale_y_continuous(breaks = seq(0,2.5,by = 0.2))+
      scale_x_date(#date_breaks = "1 day",
                   date_labels = "%b %d")+
      theme_half_open(font_size = 14)+
      background_grid()+
      scale_color_manual(values = c("Standby" = "#008000", "Spray" ="#D0342C"))+
      labs(y = "Risk",
           x = "",
           color = "",
           fill = "" )+
      coord_cartesian(ylim =c(0, max(data_power()$sum_R, na.rm = T)+0.2))+
      theme(legend.position = "bottom",
            plot.title = element_text(size = 12, face = "plain", margin=margin(0,0,80,0)),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.background = element_rect(color = NA),
            panel.background = element_rect(color = NA),
            panel.border = element_rect(color ="black"))
    
    shinyjs::hide(id = "prog")
    
    ggplotly(g) %>% 
    layout(legend = list(orientation = "h", x = 0, y =1.12))
  })

 
 # table  ---------------------------------------------------
 
output$table_risk = DT::renderDataTable(options = list(
  pageLength = 5),{
  data_power() %>% 
      rename(Risk = sum_R,
             Location = location,
             Date = date,) %>%
      mutate(Risk = round(Risk, 2)) %>% 
      dplyr::select(-R) %>%
      arrange(desc(Date)) %>% 
      mutate(Date = format(Date,"%b %d %Y"))
      
})

 
 
 
 
   
}

# Run the application 
shinyApp(ui = ui, server = server)
