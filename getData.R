#getData.R is the function to call out prior files, merge them,
#and output them into a csv file
dir <- getwd()
setwd(dir)

data <- getData()

getData <- function() {
  if (!file.exists('Data/output.csv')) {
    data <- read.csv('Data/total-deaths-and-cases-covid-19.csv',stringsAsFactors = FALSE)
    data$Date <- as.Date(data$Date, format = "%m %d, %Y")  
    data$Entity <- as.character(data$Entity)
    data$Entity[data$Code == "USA"]  <- "United States of America" 
    
    population_data <- read.csv("Data/WPP2019_TotalPopulationBySex.csv")
    population_data <-subset(population_data, subset = (Variant == "High" & Time == 2020))
    population_data$PopTotal  <- population_data$PopTotal * 1000 
    
    df <- merge(data,  population_data, by.x=c("Entity"), by.y=c("Location"))
    df <- df[, c("Date","Entity","Code","Total.confirmed.deaths..deaths.","Total.confirmed.cases..cases.","PopTotal")]
    
    df$PerCapita.Death <-  df$Total.confirmed.deaths..deaths. / df$PopTotal
    df$PerCapita.Infection <-  df$Total.confirmed.cases..cases. / df$PopTotal
    df$Death.Rate <-   df$Total.confirmed.deaths..deaths. / df$Total.confirmed.cases..cases. 
    
    write.csv(df, file="Data/output.csv")   
    df
    
  } else {
    df <- read.table('Data/output.csv',header = TRUE, sep = ",", na.strings = "NA")
  }
  
}