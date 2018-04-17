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
  titlePanel("ARIMA Simulation"),
  
  # Sidebar for inputs 
  sidebarLayout(
    sidebarPanel(
    
      # Selection for Country 1
      selectInput("country1", "Country1:", 
                  choices = final_countries$country_name),
      
      # Selection for Country 2
      selectInput("country2", "Country2:", 
                  choices = final_countries$country_name),
      
      # Selection for Country 3
      selectInput("country3", "Country3:", 
                  choices = final_countries$country_name),
      
      # Selection for Country 4
      selectInput("country4", "Country4:", 
                  choices = final_countries$country_name),
      
      # Selection for Country 5
      selectInput("country5", "Country5:", 
                  choices = final_countries$country_name),
      
      # Selection for Indicator 1
      selectInput("indicator1", "Indicator1:", 
                  choices = indicators$INDICATOR),
      
      # Selection for Indicator 2
      selectInput("indicator2", "Indicator2:", 
                  choices = indicators$INDICATOR),
      
      # Selection for Indicator 3
      selectInput("indicator3", "Indicator3:", 
                  choices = indicators$INDICATOR),
      
      # Selection for Indicator 4
      selectInput("indicator4", "Indicator4:", 
                  choices = indicators$INDICATOR),
      
      # Selection for Indicator 5
      selectInput("indicator5", "Indicator5:", 
                  choices = indicators$INDICATOR)
    ),
    
    # Plot the simulated time series
    mainPanel(
      tags$h4(textOutput("Title")),
      tags$p(textOutput("Indicator1")),
      plotOutput("indicator1"),
      tags$p(textOutput("Indicator2")),
      plotOutput("indicator2"),
      tags$p(textOutput("Indicator3")),
      plotOutput("indicator3"),
      tags$p(textOutput("Indicator4")),
      plotOutput("indicator4"),
      tags$p(textOutput("Indicator5")),
      plotOutput("indicator5")
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
  params <- reactive({ c(input$country1, input$country2, input$country3, input$country4, input$country5, input$indicator1, input$indicator2,
                         input$indicator3, input$indicator4, input$indicator5) })

  
  # Print the title of the simulation
  output$Title <- renderText({ 
    paste("Singapore Vs ", input$country1, " Vs ", input$country2, " Vs ", input$country3, " Vs ", input$country4, " Vs ", input$country5)
    })

  # Indicator1
  output$Indicator1 <- renderText({
    paste("Singapore ", input$indicator1, " VS Other Countries ")
  })
  
  
  # Plot the time series to the "distPlot" output
  output$indicator1 <- renderPlot({
    base <- Quandl(paste("WWDI", "/SGP", "_",indicators$CODE[indicators$INDICATOR==as.character(input$indicator1)], sep=""), type = "ts")
    country1 <- Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country1)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator1)], sep=""), type = "ts")
    country2 <- Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country2)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator1)], sep=""), type = "ts")
    country3 <- Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country3)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator1)], sep=""), type = "ts")
    country4 <- Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country4)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator1)], sep=""), type = "ts")
    
    fit <- auto.arima(base)
    fitOP <- forecast(fit, h=2)
    fit2 <- auto.arima(country1)
    fit2OP <- forecast(fit2, h=2)
    fit3 <- auto.arima(country2)
    fit3OP <- forecast(fit3, h=2)
    fit4 <- auto.arima(country3)
    fit4OP <- forecast(fit4, h=2)
    fit5 <- auto.arima(country4)
    fit5OP <- forecast(fit5, h=2)
    
    ts.plot(window(fitOP$x),
            window(fit2OP$x),
            window(fit3OP$x),
            window(fit4OP$x),
            window(fit5OP$x),
            
            gpars=list(xlab="Year",
                       ylab=input$indicator1,
                       col=c("darkred", "navy",
                             "darkgreen", "darkorange", "blue", 
                             "purple"),
                       lwd=2))
    
    legend("topleft",
           bty="n",
           c("Singapore" , input$country1, input$country2, input$country3, input$country4, input$country5),
           lty=c(1, 1, 1, 1),
           lwd=c(2, 2, 2, 2),
           col=c("darkred", "navy",
                 "darkgreen", "darkorange", "blue", "purple"))
    

    
    
    
  })
  
  # Indicator2
  output$Indicator2 <- renderText({
    paste("Singapore ", input$indicator2, " VS Other Countries ")
  })
  
  # Plot the time series to the "distPlot" output
  output$indicator2 <- renderPlot({
    ts.plot(window(Quandl(paste("WWDI", "/SGP", "_",indicators$CODE[indicators$INDICATOR==as.character(input$indicator2)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country1)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator2)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country2)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator2)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country3)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator2)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country4)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator2)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country5)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator2)], sep=""), type = "ts")),
            
            
            gpars=list(xlab="Year",
                       ylab=input$indicator2,
                       col=c("darkred", "navy",
                             "darkgreen", "darkorange", "blue", "purple"),
                       lwd=2))
    
    legend("topleft",
           bty="n",
           c("Singapore" , input$country1, input$country2, input$country3, input$country4, input$country5),
           lty=c(1, 1, 1, 1),
           lwd=c(2, 2, 2, 2),
           col=c("darkred", "navy",
                 "darkgreen", "darkorange", "blue", "purple"))
  })
  
  # Indicator3
  output$Indicator3 <- renderText({
    paste("Singapore ", input$indicator3, " VS Other Countries ")
  })
  
  # Plot the time series to the "distPlot" output
  output$indicator3 <- renderPlot({
    ts.plot(window(Quandl(paste("WWDI", "/SGP", "_",indicators$CODE[indicators$INDICATOR==as.character(input$indicator1)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country1)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator3)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country2)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator3)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country3)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator3)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country4)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator3)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country5)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator3)], sep=""), type = "ts")),
            
            
            gpars=list(xlab="Year",
                       ylab=input$indicator3,
                       col=c("darkred", "navy",
                             "darkgreen", "darkorange", "blue", "purple"),
                       lwd=2))
    
    legend("topleft",
           bty="n",
           c("Singapore" , input$country1, input$country2, input$country3, input$country4, input$country5),
           lty=c(1, 1, 1, 1),
           lwd=c(2, 2, 2, 2),
           col=c("darkred", "navy",
                 "darkgreen", "darkorange", "blue", "purple"))
  })
  
  # Indicator4
  output$Indicator4 <- renderText({
    paste("Singapore ", input$indicator4, " VS Other Countries ")
  })
  
  # Plot the time series to the "distPlot" output
  output$indicator4 <- renderPlot({
    ts.plot(window(Quandl(paste("WWDI", "/SGP", "_",indicators$CODE[indicators$INDICATOR==as.character(input$indicator1)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country1)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator4)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country2)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator4)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country3)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator4)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country4)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator4)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country5)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator4)], sep=""), type = "ts")),
            
            
            gpars=list(xlab="Year",
                       ylab=input$indicator4,
                       col=c("darkred", "navy",
                             "darkgreen", "darkorange", "blue", "purple"),
                       lwd=2))
    
    legend("topleft",
           bty="n",
           c("Singapore" , input$country1, input$country2, input$country3, input$country4, input$country5),
           lty=c(1, 1, 1, 1),
           lwd=c(2, 2, 2, 2),
           col=c("darkred", "navy",
                 "darkgreen", "darkorange", "blue", "purple"))
  })
  
  # Indicator5
  output$Indicator5 <- renderText({
    paste("Singapore ", input$indicator5, " VS Other Countries ")
  })
  
  # Plot the time series to the "distPlot" output
  output$indicator5 <- renderPlot({
    ts.plot(window(Quandl(paste("WWDI", "/SGP", "_",indicators$CODE[indicators$INDICATOR==as.character(input$indicator5)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country1)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator5)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country2)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator5)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country3)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator5)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country4)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator5)], sep=""), type = "ts")),
            window(Quandl(paste("WWDI", "/", final_countries$country_code[final_countries$country_name == as.character(input$country5)], "_" ,indicators$CODE[indicators$INDICATOR==as.character(input$indicator5)], sep=""), type = "ts")),
            
            gpars=list(xlab="Year",
                       ylab=input$indicator5,
                       col=c("darkred", "navy",
                             "darkgreen", "darkorange", "blue", "purple"),
                       lwd=2))
    
    legend("topleft",
           bty="n",
           c("Singapore" , input$country1, input$country2, input$country3, input$country4, input$country5),
           lty=c(1, 1, 1, 1),
           lwd=c(2, 2, 2, 2),
           col=c("darkred", "navy",
                 "darkgreen", "darkorange", "blue", "purple"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
