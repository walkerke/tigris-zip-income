library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(readr)
library(sp)
# library(tigris)

zips <- readRDS('zips.rds')

metros <- readRDS('metros.rds')

get_zips <- function(metro_name) {
  
  # Subset for specific metro area (be careful with duplicate cities like "Washington")
  my_metro <- metros[grepl(sprintf("^%s", metro_name), metros$NAME, ignore.case = TRUE), ]
  
  # Find all ZCTAs that intersect the metro boundary
  metro_zips <- over(my_metro, zips, returnList = TRUE)[[1]]
  
  my_zips <- zips[zips$ZCTA5CE10 %in% metro_zips$ZCTA5CE10, ]
  
  # Return those ZCTAs
  return(my_zips)
  
}

# Write function to format the legend appropriately 

quantile_labels <- function(vec, n) {
  qs <- round(quantile(vec, seq(0, 1, 1/n), na.rm = TRUE), 0)
  len <- length(qs) - 1
  qlabs <- c()
  for (i in 1:len) {
    j <- i + 1
    v <- paste0("$", as.character(qs[i]), " - ", "$", as.character(qs[j]))
    qlabs <- c(qlabs, v)
  }
  final_labs <- c(qlabs, "Data unavailable")
  final_labs
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   fixedPanel(
     id = 'fullscreen', 
     top = 0, left = 0, right = 0, bottom = 0, 
     leafletOutput('zipmap', width = '100%', height = '100%')
   ), 
   absolutePanel(id = 'controls', draggable = TRUE, right = 20, top = 0, 
                 h4('Average total income reported to the IRS, 2013'), 
                   selectInput('metro', 'Select a metropolitan area', 
                               choices = sort(metros$NAME), 
                               selected = 'Dallas-Fort Worth-Arlington, TX'), 
                 p('Data source: ', a('US Internal Revenue Service', 
                                      href = 'https://www.irs.gov/uac/soi-tax-stats-individual-income-tax-statistics-2013-zip-code-data-soi'))
                 )
  
)

server <- function(input, output) {
   
   zip_data <- reactive({
     
     return(get_zips(input$metro))
     
    })
   
   output$zipmap <- renderLeaflet({
     
     pal <- colorQuantile('Greens', NULL, n = 7, na.color = '#DBDBDB')
     
     label <- paste0("Zip Code ", zip_data()$ZCTA5CE10, ": $", 
                     as.character(round(1000 * zip_data()$incpr, 2)))
     
     leaflet() %>%
       addProviderTiles("CartoDB.Positron") %>%
       addPolygons(data = zip_data(), 
                   fillColor = ~pal(zip_data()$incpr), 
                   fillOpacity = 0.7, 
                   weight = 0.2, 
                   smoothFactor = 0.2, 
                   label = label, 
                   color = 'grey') %>%
       addLegend(colors = c("#F7FCF5", "#DBF1D5", "#AEDEA7", "#74C476", "#37A055", "#0E7734", 
                            "#00441B", "#DBDBDB"), 
                 bins = 7, 
                 opacity = 0.7, 
                 labels = quantile_labels(1000 * zip_data()$incpr, 7), 
                 position = "bottomright")
     
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

