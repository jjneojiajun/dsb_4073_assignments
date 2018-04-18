# =======================================================
# Simulation of ARIMA(p,d,q) Model for Time Series
# =======================================================
#
# This is Shiny web application, a powerful R front-end.
# You can run the demo using the 'Run App' button above.
#
# Resources on building similar RShiny web applications:
# RShiny website  : http://shiny.rstudio.com/
# RShiny tutorial : https://shiny.rstudio.com/tutorial/
# RShiny examples : https://shiny.rstudio.com/gallery/

# install.packages("shiny")
# install.packages("rvest)

library(Quandl)
library(shiny)
library(tseries)
library(forecast)
library(rvest)

Quandl.api_key('ept8xkvWs3rySahsZqoy')

# -------------------------------------------------------
# Define UI for the application (for input parameters)

# -------------------------------------------------------
# Scrape Country and Country Code to submit into the API

# Download the webpage as an HTML file
download.file("https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes",
              destfile = "assignment4_countries.html")

# Access the locally stored HTML file
wikiPage <- read_html("assignment4_countries.html")

# Extract and print the desired table
countries <- data.frame(wikiPage %>% html_nodes(".wikitable") %>% .[[2]] %>% html_table())
head(countries)
names(countries)

final_countries <- data.frame(cbind(countries$English.short.name..upper.lower.case., countries$Alpha.3.code))
colnames(final_countries) <- c("country_name", "country_code")
final_countries$country_name <- as.character(as.factor(final_countries$country_name))
final_countries$country_code <- as.character(as.factor(final_countries$country_code))

head(final_countries)

# Get the indicators ! 
indicators <- read.csv("wwdi_indicators.txt", sep = "|")
head(indicators)
indicators$INDICATOR <- as.character(as.factor(indicators$INDICATOR))
indicators$CODE <- as.character(as.factor(indicators$CODE))


ui <- fluidPage(
  
  # Application title
  titlePanel("Singapore Vs Country X"),
  
  # Sidebar for inputs 
  sidebarLayout(
    sidebarPanel(
    
      # Selection for Country 
      selectInput("country", "Country:", 
                  choices = final_countries$country_name),
      
      selectInput("indicator", "Indicator:", 
                  choices = indicators$INDICATOR)
    
    ),
    
    # Plot the simulated time series
    mainPanel(
      tags$h4(textOutput("Title")),
      tags$p(textOutput("Indicator")),
      plotOutput("distPlot"),
      tags$p("Forecast"),
      plotOutput("predPlots")
    )
  )
)


# -------------------------------------------------------
# Define server logic for the application (for output)
#
# Take ARIMA parameters p, q, q as input from the user
# Choose the AR, MA coefficients randomly from (-1, 1)
#
# Construct the simulated ARIMA(p,d,q) data to display
# Display the randomly chosen ARIMA coefficients below
# Display the ACF and PACF corresponsing to the series

server <- function(input, output) {

  # Record the input ARIMA order parameters
  params <- reactive({ c(input$country,  input$indicator) })
  
  # Print the title of the simulation
  output$Title <- renderText({ 
    paste("Singapore Vs ", input$country)
    })

  # Indicator1
  output$Indicator <- renderText({
    paste("Singapore ", input$indicator, " VS ", input$country )
  })
  
  
  # Plot the time series to the "distPlot" output
  output$distPlot <- renderPlot({
    base <- Quandl(paste("WWDI", "/SGP", "_",indicators$CODE[indicators$INDICATOR==as.character(input$indicator)], sep=""), type = "ts")
    country <- Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator)], sep=""), type = "ts")
    
    ts.plot(window(base),
            window(country),
        
            gpars=list(xlab="Year",
                       ylab=input$indicator,
                       col=c("darkred", "navy"),
                       lwd=2))
    
    legend("topleft",
           bty="n",
           c("Singapore" , input$country),
           lty=c(1, 1, 1, 1),
           lwd=c(2, 2, 2, 2),
           col=c("darkred", "navy"))
    
  })
  
  output$Forecast <- renderText({
    paste("Forecast for Singapore VS ", input$country)
  })
  
  output$predPlots <- renderPlot({
    baseOut <- Quandl(paste("WWDI", "/SGP", "_",indicators$CODE[indicators$INDICATOR==as.character(input$indicator)], sep=""), type = "ts")
    countryOut <- Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator)], sep=""), type = "ts")
    
    baseFit <- auto.arima(baseOut)
    countryFit <- auto.arima(countryOut)
    
    op <- par(mfrow = c(1, 2))
    plot(forecast(baseFit), xlab = "Years", ylab = input$indicator, main = "Singapore Forecast")
    plot(forecast(countryFit),  xlab = "Years", ylab = input$indicator, main = paste(input$country, "Forecast"))
    par(op)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
