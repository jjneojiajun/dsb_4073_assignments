# =======================================================
# Dimensionality Reduction : Principal Component Analysis
# =======================================================

# Load and explore the dataset
attach(iris)
str(iris)
irisData <- as.data.frame(iris)
pairs(irisData[,1:4], pch = 19, col = as.numeric(irisData$Species) + 1)

# Perform Principal Component Analysis
irisPCA <- prcomp(irisData[,1:4], center = TRUE, scale. = TRUE)
irisPCA
plot(irisPCA)

# Project on Comp.1 and Comp.2
# install.packages("ggfortify")
library(ggfortify)
autoplot(irisPCA, col = as.numeric(irisData$Species) + 1, 
         loadings = TRUE, loadings.label = TRUE)



# =======================================================
# Dimensionality Reduction : Neighborhood Embedding
# =======================================================

# Load and explore the dataset
attach(iris)
str(iris)
irisData <- as.data.frame(iris)
pairs(irisData[,1:4], pch = 19, col = as.numeric(irisData$Species) + 1)

# Perform t-SNE analysis
# install.packages("Rtsne")
library(Rtsne)
irisTSNE <- Rtsne(irisData[,1:4], check_duplicates = FALSE, 
                  pca = FALSE, perplexity = 30, theta = 0.5, dims = 2)

# Project on Comp.1 and Comp.2
plot(irisTSNE$Y, pch = 19, col = as.numeric(irisData$Species) + 1,
     xlab = "t-SNE Comp.1", ylab = "t-SNE Comp.2")



# =======================================================
# Data Scraping in R : Using the "rvest" package
# =======================================================

install.packages("rvest")
library(rvest)


# -------------------------------------------------------
# Example 1 : Scrape Table from Wikipedia

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

head(final_countries)

# -------------------------------------------------------
# Example 2 : Scrape Movie Data from IMDb

# Access the Movie Webpage at IMDb
moviePage <- read_html("http://www.imdb.com/title/tt0441773/")

# Scrape the Rating as Number
rating <- moviePage %>% 
  html_nodes("strong span") %>% 
  html_text() %>% 
  as.numeric()
rating

# Scrape the Cast as List of Text
cast <- moviePage %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast

# Scrape the Poster as Link to Image
poster <- moviePage %>%
  html_nodes(".poster img") %>%
  html_attr("src")
poster

# Download the Image as a JPG file
download.file(poster, destfile = "Week13MoviePoster.jpg")

# Display the Image from JPG file
library(jpeg)
posterImage <- readJPEG("Week13MoviePoster.jpg")
plot(1:2, type='n', xlab = "", ylab = "")
rasterImage(posterImage, 1, 2, 2, 1)
