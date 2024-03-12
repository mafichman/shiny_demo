# Introduction to R Shiny

# Instructor - Michael Fichman
# mfichman@design.upenn.edu

# MUSA Practicum, Spring, 2019

# This code base provides several templates for building an app to run locally
# in Shiny and it 

# For the record, making a markdown to demo Shiny is difficult, because
# all Shiny apps need to be hosted somewhere for a published markdown to work

# OK - Let's start by loading a few libraries and setting your working directory
# to the folder where this script is located

library(shiny)
library(tidyverse)
library(leaflet)
library(ggmap)
library(sf)
library(rgdal)

setwd("C:/Users/mfich/Documents/Clients/MUSA_Teaching_and_Admin/SPRING_STUDIO_2018/Shiny_demo/shiny_demo_32118")

# ---- What is shiny? ------

# It's a way to present data analyzed in R to a user and let them view or manipulate it
# It is an alternative to apps which use Javascript/CSS/HTML
# You do not need to know javascript to make a Shiny app
# If you want to know more about how R and Javascript communicate in a shiny app
# and how to customize javascript stuff inside Shiny, look - https://www.r-bloggers.com/communication-between-r-and-d3js/

# Let's look at some examples http://shiny.rstudio.com/gallery/

# We will be running our shiny apps locally on your own machine for this demo
# Let's see what that looks like by using the runExample command
# to see some demo apps

runExample("01_hello")
runExample("02_text")

#----- What is the architecture of a shiny app? -----

# There are two parts to a shiny app - a User Interface (UI) function 
# and a Server (server) function
# These are the static building blocks of a shiny app

# The UI part describes the elements of the dashboard - their size and location
# The server part describes the nature of these objects and then "serves" them
# To the UI

# They are stitched together in a "shinyApp" function
# In deploying an app, they may be represented as separate files

# Here's a simple shiny app to show you what each part does
# The Server function has nothing in it at the moment

ui <- fluidPage("hello world")
server <- function(input, output, session){}
shinyApp(ui, server)


# There are numerous UI elements you can add to your sidebar and text elements
# You can move them around and change font sizes like you can using HTML/CSS

# Here is a list of some of UI elements:
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
# Here's a cheatsheet of Shiny features and functions: 
# https://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf

# Let's start adding some UI elements - a sidebar and some basic text

ui <- fluidPage(titlePanel("Here is my title"),
                sidebarLayout(
                  sidebarPanel("Here is my sidebar"),
                  mainPanel("Here is my main page")
                )
                )
server <- function(input, output, session){}
shinyApp(ui, server)

# You could lay out your app using a split layout
# https://shiny.rstudio.com/reference/shiny/1.0.1/splitLayout.html

# Notice the function "verbatimTextOutput" - that is a function defined in shiny

ui <- fluidPage(titlePanel("Check it out - two panels"),
                splitLayout(
                  verbatimTextOutput("panel1Text"),
                  verbatimTextOutput("panel2Text")
                )
)
server <- function(input, output, session){
  output$panel1Text <- renderText('panel1')
  output$panel2Text <- renderText('panel2')
}
shinyApp(ui, server)

# Let's try adding some data visualizations.
# Let's use mtcars, one of the data sets that comes with R

head(mtcars)

# Let's start by adding a plot - vehicle MPG as a function of vehicle weight
# Notice that in the server function we use the command renderPlot to send 
# a ggplot we create to the output object "output$mpgPlot" and specify in 
# the ui where it is supposed to go.

# The "language" of Shiny consists of functions such as 
# "renderPlot" and "headerPanel" - these are NOT user defined
# Check out a cheat-sheet which contans a lot of the common functions - https://www.rstudio.com/wp-content/uploads/2015/02/shiny-cheatsheet.pdf


ui <- fluidPage(titlePanel("MPG as function of weight"),
                sidebarPanel("Here is my sidebar"),
                mainPanel(plotOutput("mpgPlot"))) # Put the output here

server <- function(input, output, session){
  
  output$mpgPlot <- renderPlot({ #we are telling the UI that this plot is "output"
    ggplot(mtcars, aes(wt, mpg))+
      geom_point(size=3)})
  }
  
shinyApp(ui, server)


# What if we'd like to make this output a bit more informationally rich and
# output the plot's R-squared in the sidebar?

# Let's add a regression line to the plot and grab the R-Squared item from 
# an lm object that models the relationship and "renderText" into the sidebar panel. 
#We can print the R-squared value using a specified font "h6." 
# (This font specification is a lot like HTML.)


ui <- fluidPage(titlePanel("MPG as function of weight"),
                sidebarPanel(h6("The R-Squared value is "),
                  h6(textOutput("rSquared")),
                  width = 4),
                mainPanel(plotOutput("mpgPlot")))

server <- function(input, output, session){
  
  output$mpgPlot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg))+
      geom_point(size=3)+
      stat_smooth(method=lm)
  })
  #call the r-squared value in a linear model  
  output$rSquared <- renderText({summary(lm(mpg~wt, mtcars))$r.squared 
  })
 
}
shinyApp(ui, server)

# We are building a UI dashboard from scratch.
# However, there is a package called
# shinydashboard that has native menu and window structures
# https://rstudio.github.io/shinydashboard/

# ----- Let's try to do something with spatial data. ------

# The leaflet package is superior to ggplot for dynamic mapping because 
# it renders faster and has many features.
# It's the workhorse of javascript web mapping. 
# Let's put a leaflet map of Philadelphia in our dashboard 
# We can chose Stamen map styles here: 
# http://maps.stamen.com/#terrain/12/37.7706/-122.3782

# Leaflet uses shapes from the spatial package 'rgdal' instead of the more modern 'sf'
# As of recently, you can also use the 'mapview' package with shiny
# mapview can handle sf objects

ui <- fluidPage(
  titlePanel("Map of Philadelphia, PA"),
  mainPanel(leafletOutput("mymap")))


server <- function(input, output, session) {
  #Make a leaflet map
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(-75.137259, 39.977981, zoom = 11)
    })
  }

# Now create the app by calling the ui and the server
shinyApp(ui, server)

# Now let's add a geocoding function to pass an address to ggmap and the 
# dsk "data science toolkit' geocoder and reset our map.
# The default geocoder is the google geocoder, which is no longer supported
# Notice how bad the geocoder is! You have to cut and paste basically one whole block of text.

# NOTE FOR 2024 - DSK is defunct - the following code doesn't function - you'll need to find a new geocoder if you want this functionality.

# Check out more Leaflet/Shiny tricks, like geolocating a user device 
# Here: https://www.r-bloggers.com/4-tricks-for-working-with-r-leaflet-and-shiny/


ui <- fluidPage(
  titlePanel("Map of Philadelphia, PA"),
  mainPanel(leafletOutput("mymap"),
            absolutePanel(top=20, left=70, 
            textInput("target_zone", "" , "Input Location"))
            )
      )


server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    if(input$target_zone=="Input Location"){
      ZOOM=11
      LAT=39.977981
      LNG=-75.137259
    }else{
      target_pos=geocode(input$target_zone, source = "dsk")
      LAT=target_pos$lat
      LNG=target_pos$lon
      ZOOM=15
    }
    
    # Plot it!
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng=LNG, lat=LAT, zoom=ZOOM )
  })
}

shinyApp(ui, server)


# ----- Let's get more complex and start adding more spatial data and viz. -----
# we are going to create a leaflet map to visualize crime and map a shapefile of districts
# This data was collected from OpenDataPhilly and the Philly Police Dept.
# and subset to include only a few months

# The city of Philadelphia uses this data feed to create a javascript dashboard, which
# we will use Shiny to try to replicate some of these features

# Start by loading some data - the crime points and a districts shapefile

districts <- readOGR("police_districts_proj.shp")
crimes <- read.csv("crime_oct_dec_2017.csv")

# Let's start by making one that only has one little widget, a slider
# The reaction to the slider requires an "observe" function that updates the map
# Each reactive element in your app needs to update a map within an "observe" call

# We will also add a shapefile and some pop-ups for the markers and features

ui <- fluidPage(
  titlePanel("Type I Crimes, Oct-Dec, 2017"),
  sidebarPanel(
    sliderInput("range", 
                "Choose an Hour Range", 
                min = 0,
                max = 23,
                step = 1,
                value = c(0,23))
              ),
  mainPanel(leafletOutput("mymap", height = "600px"))
              )

server <- function(input, output, session) {
  
  # Reproject the shapefile into web mercator
  # Learn about spatial references and transformations here: http://spatialreference.org/ref/sr-org/7483/
  
  districts <- spTransform(districts, CRSobj = ("+proj=longlat +datum=WGS84"))
  
  # Make a pop-up for the shapefile
  # Specify what the districts should say when clicked on
  # This requires writing a bit of HTML
  # We will call this in leaflet below
  district_popup <- paste0("<strong>District: </strong>", districts$DIST_NUM)
  
  # Make a pop-up for the points 
  # Specify what the crime points should say when clicked on
  # We will call this in leaflet below
  marker_popup <- paste0("<strong>Dispatch Date & Time: </strong>", 
                         crimes$dispatch_date_time, 
                         "<br><strong>Location: </strong>", 
                         crimes$location_block,
                         "<br><strong>Description: </strong>", 
                         crimes$text_general_code,
                         "<br><strong>PPD District: </strong>",
                         crimes$dc_dist)
  
  #Make a leaflet map, adding options to highlight polygons and display popups
  # The map is only drawn ONCE - changes are made in a "reactive" data call later
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Terrain") %>%
      setView(-75.137259, 39.977981, zoom = 11) %>% 
      addPolygons(data = districts, 
                  weight = 3, fill = FALSE, stroke = TRUE, color = "black",
                  highlightOptions = highlightOptions(color = "yellow", weight = 5,
                                                      bringToFront = TRUE), 
                  popup = district_popup) %>%
      addCircleMarkers(lat = crimes$lat, lng = crimes$lng,
                       color = "red", 
                       stroke = FALSE,
                       fillOpacity = 0.3, 
                       radius = 5,
                       popup = marker_popup
      )
  }
  
  )
  
  observe({
    
    # Reactive expression to create data frame of slider-dependent values
    
    range <- input$range
    
    subset_crimes <- crimes %>%
        filter(hour_ >= range[1] & hour_ <= range[2]) 
    
    subset_marker_popup <- paste0("<strong>Dispatch Date & Time: </strong>", 
                                  subset_crimes$dispatch_date_time, 
                                  "<br><strong>Location: </strong>", 
                                  subset_crimes$location_block,
                                  "<br><strong>Description: </strong>", 
                                  subset_crimes$text_general_code,
                                  "<br><strong>PPD District: </strong>",
                                  subset_crimes$dc_dist
    )
    
    #leafletProxy allows you to manipulate a map already rendered
    leafletProxy("mymap") %>%
      clearMarkers() %>% 
      addCircleMarkers(lat = subset_crimes$lat, lng = subset_crimes$lng,
                       color = "red", #this could be made into a pallette to match charts as well 
                       stroke = FALSE,
                       fillOpacity = 0.3, 
                       radius = 5,
                       popup = subset_marker_popup
      )
  })
}


shinyApp(ui, server)

# ---- Now let's add a dropdown menu, a dynamic plot and a dynamic table ----

ui <- fluidPage(
  titlePanel("Type I Crimes, Oct-Dec, 2017"),
  sidebarPanel(
    sliderInput("range", 
                "Choose an Hour Range", 
                min = 0,
                max = 23,
                step = 1,
                value = c(0,23)),
  selectInput(inputId = "type_of_crime", 
                        label = "Type of Crime",
                        selected = "single",
                        selectize = TRUE,
                        choices = sort(unique(crimes$text_general_code))
              ),
  plotOutput("summaryPlot")
  ),
  mainPanel(leafletOutput("mymap", height = "600px")),
  fluidRow(column(10, dataTableOutput('table')))
)

# we are going to create a leaflet map to visualize police complaints and map a shapefile of districts
# We can chose Stamen map styles here: http://maps.stamen.com/#terrain/12/37.7706/-122.3782

server <- function(input, output, session) {
  
  
  
  # Reproject the shapefile into web mercator
  # Learn about spatial references and transformations here: http://spatialreference.org/ref/sr-org/7483/
  
  districts <- spTransform(districts, CRSobj = ("+proj=longlat +datum=WGS84"))
  
  # Specify what the districts should say when clicked on
  district_popup <- paste0("<strong>District: </strong>", districts$DIST_NUM)
  
  # Specify what the crime points should say when clicked on
  marker_popup <- paste0("<strong>Dispatch Date & Time: </strong>", 
                        crimes$dispatch_date_time, 
                        "<br><strong>Location: </strong>", 
                        crimes$location_block,
                        "<br><strong>Description: </strong>", 
                        crimes$text_general_code,
                        "<br><strong>PPD District: </strong>",
                        crimes$dc_dist)
  
  #Make a leaflet map
  output$mymap <- renderLeaflet({
  leaflet() %>% 
    addProviderTiles("Stamen.Terrain") %>%
    setView(-75.137259, 39.977981, zoom = 11) %>% 
    addPolygons(data = districts, 
                weight = 3, fill = FALSE, stroke = TRUE, color = "black",
                 highlightOptions = highlightOptions(color = "yellow", weight = 5,
                 bringToFront = TRUE), 
                popup = district_popup) %>%
    addCircleMarkers(lat = crimes$lat, lng = crimes$lng,
                         color = "red", 
                         stroke = FALSE,
                         fillOpacity = 0.3, 
                         radius = 5#,
                     #popup = marker_popup
                     )
  }
  
  )
  #Render a DataTable - DataTable is dynamic javascript, table is HTML and static
  output$table <- renderDataTable(crimes)
  
  #Render a summary plot of crime by month
  output$summaryPlot <- renderPlot({
    ggplot(crimes, aes(x= month))+
    geom_bar(aes(y = (..count..)))+
      scale_y_continuous("Total incidents", limits= c(0,500))+
      xlab("Month")
  })
 observe({
    
    type_of_crime <- input$type_of_crime
    range <- input$range
    
    subset_crimes <- crimes %>% 
     filter(crimes$text_general_code == type_of_crime &
      hour_ >= range[1] & hour_ <= range[2])
    
    subset_marker_popup <- paste0("<strong>Dispatch Date & Time: </strong>", 
                            subset_crimes$dispatch_date_time, 
                           "<br><strong>Location: </strong>", 
                           subset_crimes$location_block,
                           "<br><strong>Description: </strong>", 
                           subset_crimes$text_general_code,
                           "<br><strong>PPD District: </strong>",
                           subset_crimes$dc_dist
                           )
    
    #output a new DataTable
    output$table <- renderDataTable(subset_crimes)
    
    #Output a new summary plot
    output$summaryPlot <- renderPlot({
      ggplot(subset_crimes, aes(x= month))+
        geom_bar(aes(y = (..count..)))+
        scale_y_continuous("Total incidents", limits= c(0,500))+
        xlab("Month")
    })
    
    #leafletProxy allows you to manipulate a map already rendered
    leafletProxy("mymap") %>% 
      clearMarkers() %>% 
      addCircleMarkers(lat = subset_crimes$lat, lng = subset_crimes$lng,
                       color = "red", #this could be made into a pallette to match charts as well 
                       stroke = FALSE,
                       fillOpacity = 0.3, 
                       radius = 5,
                       popup = subset_marker_popup
                       )
  })
}

# Now create the app by calling the ui and the server
shinyApp(ui, server)

# ----- Deploying Shiny in the Wild ------

# Here is how you get started using shinyapps.io:
# http://shiny.rstudio.com/articles/shinyapps.html
# http://docs.rstudio.com/shinyapps.io/index.html

# How/where do you store your data?

# These sites have a codebase for many interactive storage options including
# Google sheets, Dropbox, MySQL, Amazon S3 and more
# https://daattali.com/shiny/persistent-data-storage/
# http://googledrive.tidyverse.org/

# There are ways to deploy shiny apps on a website, though that is more difficult:

# https://www.cultureofinsight.com/blog/2018/03/15/2018-03-15-responsive-iframes-for-shiny-apps/

# Shiny is kind of slow and clunky - and big apps can be difficult to launch or use
# Keep your shiny app nimble - try to give it the lightest processing load possible
# This means trying to use the most simplified front end version of your data, and as little
# complex shape rendering/drawing as possible
# Just for fun, let's try to break leaflet with a very big shapefile

parcels <- readOGR("PWD_PARCELS/PWD_PARCELS.shp")

ui <- fluidPage(
  titlePanel("parcels"),
  sidebarPanel(),
  mainPanel(leafletOutput("mymap", height = "600px"))
)

server <- function(input, output, session) {
  
  # Reproject the shapefile into web mercator
  # Learn about spatial references and transformations here: http://spatialreference.org/ref/sr-org/7483/
  
  parcels <- spTransform(parcels, CRSobj = ("+proj=longlat +datum=WGS84"))
  
  
  #Make a leaflet map, adding options to highlight polygons and display popups
  # The map is only drawn ONCE - changes are made in a "reactive" data call later
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(-75.137259, 39.977981, zoom = 11) %>% 
      addPolygons(data = parcels, 
                  weight = 3, fill = FALSE, stroke = TRUE, color = "black",
                  highlightOptions = highlightOptions(color = "yellow", weight = 5,
                                                      bringToFront = TRUE))
  }
  
  )
  
}


shinyApp(ui, server)

