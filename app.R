#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(ggplot2)
library(leaflet)
library(dplyr)

pengs <- penguins[!is.na(penguins$sex),]

summary(pengs)

islands = c("Biscoe", "Dream", "Torgersen")
colors = c("blue", "green", "orange")
lats = c(-65.67431687652159, -64.73183354043242, -64.76629048892288)
lons = c(-65.94666214883208, -64.23378445084403, -64.08323121306947)

island_loc = data.frame(island=islands, lat=lats, lon=lons, color=colors)

sex_options = unique(pengs$sex)
year_options = unique(pengs$year)
species_options = unique(pengs$species)

peng_summary <- pengs %>% 
  group_by(island, species, sex, year) %>% 
  summarize(population=n(), 
            body_mass=mean(body_mass),
            flipper_len=mean(flipper_len))

data_with_location <- merge(peng_summary, island_loc, by="island")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Penguin population map"),
    p("This website was built to visualize total penguin population per species on every year and island where they live."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Filter data"),
            br(),
            p("You can analyze the total population by penguins species, sex and island where they live, by selecting one of the following options."),
            radioButtons("year", "Year: ", choices = year_options, selected = year_options[1]),
            radioButtons("sex", "Sex: ", choices = sex_options, selected = sex_options[1]),
            radioButtons("species", "Species: ", choices = species_options, selected = species_options[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("Updated results"),
           p("Here you will see the results of the information provided.  You will see the map changing based on the data available after selecting options.   There are some penguin species that only live in one island, while others live in all three islands."),
           leafletOutput("populationPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$populationPlot <- renderLeaflet({
      
      sex=input$sex
      species=input$species
      year=input$year
      
      data_to_show <- data_with_location[data_with_location$sex==sex & 
                                           data_with_location$species==species &
                                           data_with_location$year== year,]
      
      final_plot <- data_to_show %>% 
        leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(weight = data_to_show$population, 
                         lat = data_to_show$lat, 
                         lng = data_to_show$lon,
                         radius = data_to_show$population,
                         col = data_to_show$color,
                         popup = paste("<b>Island:</b> ", data_to_show$island, "<br>", 
                                       "<b>Total Population:</b> ", data_to_show$population, "<br>",
                                       "<b>Average body mass:</b> ", data_to_show$body_mass, " kg<br>") ) %>% 
        addLegend(labels=data_to_show$island, colors=data_to_show$color,)
      
      
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #     xlab = 'Waiting time to next eruption (in mins)',
        #     main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
