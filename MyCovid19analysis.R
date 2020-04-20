# Read in excel file data
deaths.cases <- read.csv("total-deaths-and-cases-covid-19.csv", stringsAsFactors = FALSE)


#Format the date column so that it is recognized as a date "YYYY-MM-DD"
library(lubridate)
deaths.cases$Date <- mdy(deaths.cases$Date)


#Creation of a dataframe for subsetting of the data by date(March 30)
dataframe <- subset(deaths.cases, Date > as.Date("2020-03-30"))


#Format USA label
dataframe$Entity <- as.character(dataframe$Entity)
dataframe$Entity[dataframe$Code == "USA"] <- "United States of Amerrica"



# Read in excel file data
popdata <- read.csv("WPP2019_TotalPopulationBySex.csv", stringsAsFactors = FALSE)


#Create second data frame for population data
dataframe2 <- subset(popdata, subset = Variant == "High" & Time == 2020)


#Convert the total population to actual number by X 1,000
dataframe2$PopTotal <- dataframe2$PopTotal * 1000


#Merge data from both files into one dataframe
dataframe3 <- merge(dataframe, dataframe2, by.x = c("Entity"), by.y = c("Location"))


#Now, create a data frame with column data we wish to analyze
df4 <- dataframe3 [, c("Entity","Total.confirmed.deaths..deaths.","Total.confirmed.cases..cases.","PopTotal")]


#Combine the calculations with our dataframe we want to analyze
df4$PerCapita.Death <- df4$Total.confirmed.deaths..deaths. / df4$PopTotal
df4$PerCapita.Infection <- df4$Total.confirmed.cases..cases./ df4$PopTotal
df4$Death.Rate <- df4$Total.confirmed.deaths..deaths. / df4$Total.confirmed.cases..cases.

#Lastly, save your outputted data analysis as a csv
write.csv(df4, file = "outputanalysis.csv")
