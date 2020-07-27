#Updated COVID-19 Analysis to display the steep increase in cases and death rates.

# Read in excel file data
updatedeaths.cases <- read.csv("recent-total-deaths-and-cases-covid-19.csv", stringsAsFactors = FALSE)


#Format the date column so that it is recognized as a date "YYYY-MM-DD"
library(lubridate)
updatedeaths.cases$Date <- mdy(updatedeaths.cases$Date)


#Creation of a dataframe for subsetting of the data by date (July)
dat <- subset(updatedeaths.cases, Date > as.Date("2020-03-20"))


#Format USA label
dat$Entity <- as.character(dat$Entity)
dat$Entity[dat$Code == "USA"] <- "United States of America"

# Read in excel file data
pop <- read.csv("recent-WPP2019_TotalPopulationBySex.csv", stringsAsFactors = FALSE)


#Create second data frame for population data
dat2 <- subset(pop, subset = (Variant == "High" & Time == 2020))


#Convert the total population to actual number by X 1,000
dat2$PopTotal <- dat2$PopTotal * 1000

#Merge data from both files into one dataframe
dat3 <- merge(dat, dat2, by.x=c("Entity"), by.y=c("Location"))


#Now, create a data frame with column data we wish to analyze
dat4 <- dat3 [, c("Entity", "Date", "Total.confirmed.deaths..deaths.","Total.confirmed.cases..cases.","PopTotal", "Code")]

#Combine the calculations with our dataframe we want to analyze
dat4$PerCapita.Mortality <- dat4$Total.confirmed.deaths..deaths. / dat4$PopTotal
dat4$PerCapita.InfectionRate <- dat4$Total.confirmed.cases..cases./ dat4$PopTotal
dat4$Mortality.Rate <- dat4$Total.confirmed.deaths..deaths. / dat4$Total.confirmed.cases..cases.

#Lastly, save your outputted data analysis as a csv
write.csv(dat4, file = "updatedoutputanalysis.csv")


#Create Interactive World Map to Display Death Rate by Country
require(ggplot2)
library(grid)

dir <- getwd()
setwd(dir)

updatedeaths.cases <- read.csv("updatedoutputanalysis.csv", stringsAsFactors = FALSE)
updatedeaths.cases$Date <- as.Date(updatedeaths.cases$Date,"%Y-%m-%d")
updatedeaths.cases <- subset(updatedeaths.cases, subset = PopTotal > 80000000 & PopTotal < 400000000)

png("plot-updated.png")

p.new <- ggplot(updatedeaths.cases) + 
  geom_line(aes(x = Date, y = Mortality.Rate, group = Code, colour = Code)) +
  geom_text(data = subset(updatedeaths.cases, Date > "2020-07-21"),
            aes(label = Code, colour = Code, x = Date, y = Mortality.Rate), check_overlap = TRUE) +
  coord_cartesian(clip = 'off') +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(2,3,2,2), "lines")) 
print(p.new)

# Code to turn off clipping
gtnew <- ggplotGrob(p.new)
gtnew$layout$clip[gtnew$layout$name == "panel"] <- "off"
grid.draw(gtnew)


#Add graph title
p.new + labs(title = "2020 COVID-19 Mortality Rate by Country",
         caption = "Data: covid.ourworldindata.org and population.un.org/wpp") +
  theme(
    plot.title = element_text(hjust = 0.7, size = 15),
    plot.subtitle = element_text(hjust = 0.7)
  )
p.new


png("plot-updated.png")

dev.off()

