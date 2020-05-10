#plot.R is the ggplot program file
require(ggplot2)
library(grid)

dir <- getwd()
setwd(dir)

deaths.cases <- read.csv("outputanalysis.csv", stringsAsFactors = FALSE)
deaths.cases$Date <- as.Date(deaths.cases$Date,"%Y-%m-%d")
deaths.cases <- subset(deaths.cases, subset = PopTotal > 60000000 & PopTotal < 350000000 )

png("plot-all.png")

p <- ggplot(deaths.cases) + 
  geom_line(aes(x = Date, y = Death.Rate, group = Code, colour = Code)) +
  geom_text(data = subset(deaths.cases, Date > "2020-04-19"),
            aes(label = Code, colour = Code, x = Date, y = Death.Rate), vjust = -0.25, check_overlap = TRUE) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,3,1,1), "lines")) 
print(p)

# Code to turn off clipping
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


#Add graph title
p + labs(title = "COVID-19 Mortality Rate by Country",
         caption = "Data: covid.ourworldindata.org and population.un.org/wpp") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5)
  )
p


png("plot-all.png")

dev.off()

