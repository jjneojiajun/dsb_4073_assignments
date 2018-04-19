# DSB Assignment 4

There are two questions that I am tackling in this assignment. 

Question 1 is about Cryptocurrencies and using ARIMA to plot the forecast of these cryptocurrencies. The main issue about cryptocurrencies when I am dealing with the data will be that it is too highly volatile and thus it is difficult to actually forecast the cryptocurrencies in the future due to this volatility. The approach that I made was to use a moving average for the graph. This way, it will be much easier for me to predict whether the cryptocurrency will go up or down. The file that I created is `cryptocurrencies.Rmd` to generate a html file with the capability of the Table of Content. This allow you to navigate to which cryptocurrency that you are actually interested in such as: Bitcoin, Ripple or Ethereum.

Question 2 is about create an application to allow users to select a country and a indicator and predict a forecast for the country's indicator and compare it with Singapore. The interesting part is how Singapore are competiting against developed countries such as China in the GDP for example. The code is currently using Quandl. Please have a Quandl account and fill in the API key to be able to fully utilize the feature.


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. 

`git clone https://github.com/jjneojiajun/dsb_4073_assignments.git` 

`cd Assignment4`  

## Question 1

Running Question 1 is not a difficult process, please feel free to knit the file and the RMarkDown will be able to generate the HTML file for you to upload into a server to allow more users to see the results in the world wide web.

## Question 2

Built using Shiny R Application which I personally feel is a efficient way to create active application for data. Simply open the `App.R` file and add in your personal API Key from Quandl to allow more calls into the API. However, you can also comment it out but you may be able to use the application fully since you are not a member of Quandl and they are unable to contact you via their platform. 

Active Problems:

* Not all countries work in the application. However, major countries such as USA, China, Russia and Germany works. 

* Certain Indicators are not found in the API as well. 

## Author

Neo Jia Jun - *initial works* - (https://github.com/jjneojiajun)

## Acknowlegement 
Professor Sourav for coming out with these interactive questions for us to tackle.

