# Read in excel file data
cases.tests <- read.csv("covid-19-total-confirmed-cases-vs-total-tests-conducted.csv", stringsAsFactors = FALSE)


#Format the date column so that it is recognized as a date "YYYY-MM-DD"
library(lubridate)
cases.tests$Date <- dmy(cases.tests$Date)


#Creation of a dataframe for subsetting of the data by date(April 20)
dataframe1 <- subset(cases.tests, Date > as.Date("2020-03-30"))


#Format USA label
dataframe1$Entity <- as.character(dataframe1$Entity)
dataframe1$Entity[dataframe1$Code == "USA"] <- "United States of America"



# Read in excel file data
popdata <- read.csv("WPP2019_TotalPopulationBySex.csv", stringsAsFactors = FALSE)


#Create second data frame for population data
dataframe2 <- subset(popdata, subset = Variant == "High" & Time == 2020)


#Convert the total population to actual number by X 1,000
dataframe2$PopTotal <- dataframe2$PopTotal * 1000


#Merge data from both files into one dataframe
dataframe_3 <- merge(dataframe1, dataframe2, by.x = c("Entity"), by.y = c("Location"))


#Now, create a data frame with column data we wish to analyze
df_4 <- dataframe_3 [, c("Entity", "Date", "Total.tests","Total.confirmed.cases.of.COVID.19..cases.","PopTotal", "Code")]


#Combine the calculations with our dataframe we want to analyze
df_4$PerCapita.Testing <- df_4$Total.tests / df_4$PopTotal
df_4$PerCapita.Infection <- df_4$Total.confirmed.cases.of.COVID.19..cases./ df_4$PopTotal
df_4$COVID.Positive <- df_4$Total.tests / df_4$Total.confirmed.cases.of.COVID.19..cases.


#Lastly, save your outputted data analysis as a csv
write.csv(df_4, file = "outputanalysis2.csv")






