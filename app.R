# Load packages ----
library(shiny)           # Web application framework
library(shinydashboard)  # Dashboard templates for Shiny
library(readr)           # Read in rectangular text files
library(dplyr)           # Data manipulation
library(tidyr)           # Data cleanup
library(ggplot2)         # Grammar of graphics data viz
library(lubridate)       # Working with dates
library(leaflet)         # JS leaflet library

# Load climate data ----
# Read in climate data
url <- "https://raw.githubusercontent.com/jeremymack-LU/shinyapps/master/data/abe_climate.csv"
df <- read_csv(url, col_types=cols(tavg=col_double()))
# Fix extreme outlier
df[df$date==as.Date("1954-05-20"),6] <- NA
# Calculate daily mean temperature (dmt) and day of year (doy)
df <- df %>%
  # Convert from units of tenths
  mutate(prcp=prcp/10,
         tmax=tmax/10,
         tmin=tmin/10,
         tavg=tavg/10) %>%
  # Convert to from degrees C to degrees F
  mutate(dmt=(tmax+tmin)/2,
         doy=yday(date),
         tmin=tmin * 1.8 + 32,
         tmax=tmax * 1.8 + 32,
         dmt=dmt * 1.8 + 32,
         year=year(date))

# Summarize temperature data ----
# Calculate monthly means by year
monthly.means <- df %>%
  group_by(month(date), year(date)) %>%
  summarize(avg=mean(dmt, na.rm=TRUE)) %>%
  rename(month=`month(date)`,
         year=`year(date)`)
# Calculate the overall means by month
monthly.sum <- df %>%
  group_by(month(date)) %>%
  summarize(avg=mean(dmt, na.rm=TRUE)) %>%
  rename(month=`month(date)`)
# Long to wide format
monthly.sum.W <- monthly.sum %>%
  mutate(Year='1948-2021',label=month.abb[month]) %>% 
  pivot_wider(id_cols=Year,names_from=label,values_from=avg) %>% 
  mutate(Annual_mean=rowMeans(.[,2:13], na.rm=TRUE),
         Annual_diff=0)

# Assign labels and map settings
labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
map    <- tibble(lat=40.65, lng=-75.442, zoom=13)

# Create the Shiny App ----
# User Interface (ui)
ui <- dashboardPage(skin="black",
                    dashboardHeader(
                      title="Lehigh Valley Temperature", titleWidth=300),
                    dashboardSidebar(
                      width=300,
                      selectInput(
                        inputId="years",
                        label="Select year(s):",
                        choices=seq(1948,2022,1),
                        multiple=TRUE,
                        selected=2022)),
                    dashboardBody(
                      fluidRow(column(12,plotOutput("plot", height='500px'))),
                      br(),
                      fluidRow(column(8,tableOutput("data")),
                               column(4,leafletOutput("map"),
                                      fluidRow(column(12, 'Location of Lehigh Valley weather station.'))))
                    )
)

# Server function (server)
server <- function(input, output, session) {
  df.sub <- reactive(monthly.means %>% filter(year %in% input$years))
  output$plot <- renderPlot(ggplot() +
                              geom_boxplot(aes(month,avg,group=month), monthly.means) +
                              geom_point(aes(month,avg), monthly.sum,size=4,fill='darkgray',shape=22) +
                              geom_point(aes(month,avg,color=factor(year)),df.sub(),size=3) +
                              scale_x_continuous(breaks=seq(1,12,1),labels=labels,expand=c(0.02,0)) +
                              labs(x="Month",y="Temperature (°F)",color="Year") + theme_dark())
  
  output$data <- renderTable({
    if(is.null(input$years)) monthly.sum.W %>% rename('Annual Mean'=Annual_mean, 'Annual Diff'=Annual_diff)
    else df.sub() %>%
      mutate(month=as.integer(month),label=month.abb[month],Year=as.integer(year)) %>%
      pivot_wider(id_cols=Year,names_from=label,values_from=avg) %>%
      mutate(Annual_mean=rowMeans(.[,-1], na.rm=TRUE)) %>%
      mutate(Annual_diff=Annual_mean-mean(df$dmt, na.rm=TRUE)) %>%
      mutate(Year=as.character(Year)) %>%
      bind_rows(monthly.sum.W) %>%
      rename('Annual Mean'=Annual_mean, 'Annual Diff'=Annual_diff)}, digits=1)
  
  output$map <- renderLeaflet(
    leaflet() %>%
      setView(lat=map$lat, lng=map$lng, zoom=map$zoom) %>%
      addTiles("https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png") %>%
      addCircleMarkers(lng=map$lng, lat=map$lat,
                       popup='Lehigh Valley International Airport<br>NOAA Weather Station', stroke=FALSE, fillOpacity=0.5))}

# Run the app
shinyApp(ui, server)