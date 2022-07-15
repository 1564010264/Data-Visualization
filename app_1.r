library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinythemes)
library(shiny.semantic)
library(tidyverse)
library(DT)
library(leaflet)
library(plotly)
library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(dashboardthemes)

cuisine= read_csv("chefmozcuisine.csv")
ui = dashboardPage(
  # skin = "green",
  header = dashboardHeader(
    title = shinyDashboardLogo(
    theme = "blue_gradient",
    boldText = "Section 53 ",
    mainText = " Group",
    badgeText = "113"
    )
  ),
  sidebar = dashboardSidebar(
    minified = TRUE,
    collapsed = FALSE,
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("dashboard")),
      menuItem("Data Table", tabName = "page2", icon = icon("th")),
      menuItem("Map", tabName = "page3", icon = icon("area-chart")),
      menuItem("Plot",tabName = "page4",icon = icon("th"))
    )
  ),
  body = dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tabItems(
    tabItem(
      tabName = "page1",
      fluidRow(column(1),
               column(10,
                        h3("Project description"),
                        h5("· This project was inspired by the daily dining out experience. 
         From the perspective of consumers, 
         we think about how to allow users to quickly locate their favorite restaurant through the app when they want to eat out. 
         Based on this,the purpose of this project is to design a clear and easy-to-use restaurant map with a strong user experience."),  
                      
                      h3("Research Questions"),
                      h5("· What is the best restaurant when the food type is determined？",
                         "· What is the information about the target restaurant? " ,
                         "· What is the customer profile of target restaurant? "),
                      
                      h3("Introduction video"),
                      HTML(
                        '<iframe width="100%" height="500"
                  src="https://www.youtube.com/embed/BBT_FLYF65k"
                  frameborder="0" allowfullscreen></iframe>'
                      ),
                      
                      h3("Team Members"),
                      fluidRow(column(3,
                                      h4("Yixuan Chen")),
                               column(3,
                                      h4("Jimin Pei")),
                               column(3,
                                      h4("Yiyang Zhang")),
                               column(3,
                                      h4("Wenbo Liu")))
                      
                      ),
               
               column(1)),
     
      
    ),
    tabItem(
      tabName = "page2",
      h1("Restaurant Information"),
      dataTableOutput('table1'),
      
    ),
    
    tabItem(
      tabName = "page3",
      h1("Overview"),
      fluidRow(
        column(4,
      pickerInput(inputId = "cuisine", label = "cuisine", 
                  choices =  unique(cuisine$Rcuisine), 
                  selected = unique(cuisine$Rcuisine),
                  options = list(`actions-box` = TRUE),
                  multiple = T
      )),
        column(4,
      pickerInput(inputId = "price", label = "price", 
                  choices =  c("low", "medium", "high"), 
                  selected = c("low", "medium", "high"),
                  options = list(`actions-box` = TRUE),
                  multiple = T
      )),
        column(4,
               #sliderInput("slider", "Slider input:", 1, 100, 30),
      checkboxGroupInput("checkGroup", label = "Checkbox group", 
                          choices = list("5-star" = 5, "4-star" = 4, "3-star" = 3,"2-star" = 2),
                         selected = 1
      )
      ),
      numericInput("num", label = "Numeric input", value = 1),

      dataTableOutput('table2'),
      leafletOutput("map1"),
    )
    ),
    tabItem(
      tabName = "page4",
      h1("Rating Plot"),
      selectInput(inputId = list$Rcuisine,label = "Rating/Food_Rating/Service_Rating",
                  choices = c("Rating", "Food_Rating","Service_Rating")),
      plotOutput("plot1",height = 350)
    )
   
  )),


)
server = function(input, output) {
  output$table1 =  renderDataTable({
    return(datatable(rest_loc_info))
  })
 
  rating = read_csv("rating_final.csv")
  rest_rating = rating %>%
    group_by(placeID)%>%
    summarise_at(vars("rating", "food_rating", "service_rating"), mean)
  rest_rating[,c("rating", "food_rating", "service_rating")] <- round(rest_rating[,c("rating", "food_rating", "service_rating")] * 2.5, 1)
  
  parking=read_csv("chefmozparking.csv")
  rest_parking =parking%>%
    group_by(placeID)

  accepts=read_csv("chefmozaccepts.csv")
  rest_accepts =accepts%>%
    group_by(placeID) %>%
    summarise(accepts = paste0(Rpayment, collapse = ",")) %>%
    ungroup()
  
  hours=read_csv("chefmozhours4.csv")
  rest_hours =hours%>%
    group_by(placeID)
 
  loc= read_csv("geoplaces2.csv")
  cuisine= read_csv("chefmozcuisine.csv")
  rest_cuisine =cuisine%>%
    group_by(placeID) %>%
    summarise(cuisine = paste0(Rcuisine, collapse = ",")) %>%
    ungroup()

  rest_loc_info = loc[, c('placeID', 'latitude', 'longitude', 'name', 'price')]
  rest_loc_info[3,4]= "El Rincon de San Francisco"
  
  rest_loc_info= rest_loc_info%>%
    mutate(price_color = case_when(price == "low" ~ 1, # both tests: group A
                                   price =="medium" ~ 2, # one test: group B
                                   price == "high" ~ 3 ))
  rest_loc_info=left_join(rest_loc_info, rest_cuisine, by = "placeID")
  rest_loc_info=left_join(rest_loc_info, rest_rating, by = "placeID")
  rest_loc_info=left_join(rest_loc_info, rest_hours, by = "placeID")
  rest_loc_info=left_join(rest_loc_info, rest_accepts, by = "placeID")
  rest_loc_info=left_join(rest_loc_info, rest_parking, by = "placeID")

  randomcuisine = unique(cuisine$Rcuisine)
  # replace NA value by randomly assign 
  rest_loc_info$cuisine[is.na(rest_loc_info$cuisine)] = sample(randomcuisine, sum(is.na(rest_loc_info$cuisine)),replace=TRUE)
  
  
  
  fnrt = read_csv('rating_final.csv')
  genre = read_csv('chefmozcuisine.csv')
  
  rt = data.frame(placeID = unique(fnrt$placeID))
  
  for (i in 1:length(rt$placeID)) {
    rt$rating[i] = mean(filter(fnrt, placeID == rt$placeID[i])$rating)
  }
  
  for (i in 1:length(rt$placeID)) {
    rt$food_rating[i] = mean(filter(fnrt, placeID == rt$placeID[i])$food_rating)
  }
  
  for (i in 1:length(rt$placeID)) {
    rt$service_rating[i] = mean(filter(fnrt, placeID == rt$placeID[i])$service_rating)
  }
  
  list = inner_join(rt, genre, by = 'placeID')
  list = mutate(list, total_rating = (rating + food_rating + service_rating)/3)
  

  output$plot1 =renderPlot({
    sggnr = filter(list, Rcuisine == name)
    
    p <- ggplot(data=sggnr, aes(x=reorder(as.factor(placeID), total_rating), y=total_rating, fill = as.factor(placeID))) + 
      geom_bar(stat="identity") +
      ylab('Ratings') +
      xlab('Restuarant ID') +
      ggtitle(paste("Ratings for", name, "Restuarants are as follows")) +
      labs(fill = "Resturant ID")
    
    # Horizontal bar plot
    return(p + coord_flip())
  })
 

  output$table2 =  renderDataTable({
    data()
  })
  
  output$map1 =  renderLeaflet({
    rest_loc_info= rest_loc_info%>%
      filter(price %in% input$price)%>%
      filter(cuisine %in% input$cuisine)%>%
      filter(rating >= input$num)
    getColor <- function(rest_loc_info){
      sapply(rest_loc_info$price_color, function(price_color) {
        if(price_color == 1) {
          "green"
        } else if(price_color == 2) {
          "orange"
        } else {
          "red"
        } })
    }
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(rest_loc_info)
    )
    restuarant_locations_map <- leaflet(rest_loc_info) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~longitude,
                 lat = ~latitude,
                 icon = icons,
                 label=~as.character(name),
                 clusterOptions = markerClusterOptions()
                 )
    
    return(restuarant_locations_map)
    restuarant_locations_map
  })
  

 # 
 # output$plot1 = renderPlot({
 #   p = mpg %>%
  #    ggplot(mapping = aes(x = displ, y = hwy)) +
  #   geom_point() +
  #    geom_smooth()
  #  
  #  return(p)
    
 # })
  
  
 # output$plot2 = renderPlotly({
  #  p = mpg %>%
  #    ggplot(mapping = aes(x = displ, y = hwy)) +
  #    geom_point() +
  #    geom_smooth()
    
 #   return(ggplotly(p))
    
 # })
}

shinyApp(ui = ui,  server = server)