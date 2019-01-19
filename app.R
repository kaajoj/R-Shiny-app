# Load packages ----
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

# Load data ----
#data <- read.csv("data/top100globalterrorismdb.csv") 
data <- read.csv("data/globalterrorismdb.csv") #10 MB
#data <- read.csv("data/globalterrorismdb_0718dist.csv")  #150 MB
#data2 <- read.csv("data/terrorist-incidents.csv")



# User interface ----
ui <- fluidPage(
  
  titlePanel("Globalny terroryzm"),
  
  sidebarLayout(
    
    sidebarPanel(
            sliderInput("sliderYear", label = h4("Lata"), 1970, 
                        2017, value = c(1970, 2017)),
            sliderInput(inputId = "num",
                        label = h4("Liczba ofiar"), 
                        value = 25, min = 0, max = 500),
            checkboxInput("gestoscCheckBox", strong("Gestosc"), FALSE),
            selectInput(inputId = "wybranePanstwo",
                        label = "Wybierz panstwo",
                        choices = data$country_txt)
      ),
  
    mainPanel( 
      tabsetPanel(
        tabPanel("Mapa",
                 leafletOutput(outputId = "mymap")
        ),
        tabPanel("Wykres",
                 plotOutput("plot",height = "550px")
        ), 
        tabPanel("Histogram",
                 plotOutput("hist",height = "550px")
        ),
        tabPanel("Regiony",
                plotOutput("regiony",height = "550px")
        )
        
      )

  ))
)

# Server logic ----
server <- function(input, output, session) { 

  output$plot <- renderPlot({
    data <- filter(data, nkill > input$num)
    if(input$wybranePanstwo != "" & input$wybranePanstwo != "Wszystkie")
      data <- filter(data, data$country_txt == input$wybranePanstwo)
    plot(data$iyear, data$nkill, xlim = c(input$sliderYear[1], input$sliderYear[2]), type = "p", 
         main = "Liczba ofiar w poszczegolnych latach", xlab = "rok", ylab = "liczba ofiar", col="Blue",lwd = 2,cex = 2.0)
    if (input$gestoscCheckBox)
  plot(density(data$iyear), data$nkill, xlim = c(input$sliderYear[1], input$sliderYear[2]), type = "p", 
       main = "Liczba ofiar w poszczegolnych latach", xlab = "rok", ylab = "liczba ofiar", col="Blue",lwd = 2,cex = 2.0)     
    })

  output$hist <- renderPlot({
    data <- filter(data, iyear > input$sliderYear[1] & iyear < input$sliderYear[2])
    data <- filter(data, nkill > input$num)
    if(input$wybranePanstwo != "" & input$wybranePanstwo != "Wszystkie")
      data <- filter(data, data$country_txt == input$wybranePanstwo)
    hist(data$iyear, breaks = 35, col="blue", border="white", las=1, probability = TRUE, 
         main = "Histogram", ylab="czestosc", xlab="rok")
    lines(density(data$iyear),col="red", lwd=2)
     })
  
  
  output$regiony <- renderPlot({
    data <- filter(data, iyear > input$sliderYear[1] & iyear < input$sliderYear[2])
    data <- filter(data, nkill > input$num)
    if(input$wybranePanstwo != "" & input$wybranePanstwo != "Wszystkie")
      data <- filter(data, data$country_txt == input$wybranePanstwo)
    plot(data$region_txt,data$nkill, type = "h", main = "Liczba ofiar w poszczegolnych regionach", 
         xlab = "region", ylab = "liczba ofiar",lwd = 2,cex = 2.0)
    if (input$gestoscCheckBox)
      plot(density(data$region), data$nkill, type = "p", 
     main = "Liczba ofiar w poszczegolnych regionach", xlab = "region", ylab = "liczba ofiar", col="Blue", lwd=2, cex = 2.0)   
  })
  
  
  #mapa
  output$mymap <- renderLeaflet({
    data <- filter(data, iyear > input$sliderYear[1] & iyear < input$sliderYear[2])
    data <- filter(data, nkill > input$num)
    if(input$wybranePanstwo != "" & input$wybranePanstwo != "Wszystkie")
    data <- filter(data, data$country_txt == input$wybranePanstwo)
    
    leaflet(data) %>% 
      addTiles() %>%
      
      addCircles(data, lat = data$latitude, lng = data$longitude, radius = 3)
  })
  
}

# Run app ----
shinyApp(ui, server)