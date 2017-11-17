## Loading requires the following packages
library(magrittr) ## Mechanism for chaining commands
library(ggplot2) # For graphics
library(dplyr)

# Working with many files is hectic, so i decided to merge
# my data from January 2015 to  January 2017 into single files
# for both outcomes and street
multMerge = function(mypath){
   filenames = list.files(path = mypath, full.names = TRUE)
   datalist = lapply(filenames,
                     function(x){read.csv(file = x,
                                          header = TRUE,
                                          stringsAsFactors = FALSE)})
   Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
}
all_outcomes_data <- multMerge("/Users/kenmbuki/Desktop/R/outcomes")
all_street_data <- multMerge("/Users/kenmbuki/Desktop/R/street")

# Write data into single files
# This is optional
write.csv(all_outcomes_data, file ="outcomes_file.csv")
write.csv(all_street_data, file ="street_file.csv")

# Read files
outcomes <- read.csv("outcomes_file.csv", header = TRUE)
street <- read.csv("street_file.csv", header = TRUE)

# Tell R to be looking at this data frame for columns referenced
attach(street) 

summary(street)
summary(outcomes)


# Which is the most common Crime type in West Yokrshire
library(ggplot2) # You need to load this everytime
street %>%
  count(Crime.type) %>%
  ggplot(aes(x = reorder(Crime.type, n),
             y = n)) + 
  geom_col() + 
  labs(x = "Crime Type",
       y = "Number of Crimes",
       title = paste0("Most common crime commited between January and August")) +
  coord_flip() +
  theme_minimal()

# Bar plot for Violence and Sexual Offences by Month.
sexual_violence <- street %>%
  filter(Crime.type == "Violence and sexual offences") %>%
  select(Month, Longitude, Latitude, Crime.type)
bar = summary(sexual_violence$Month)
barplot(bar,xlab = "Month of the year in 2017", ylab = "Number of Crimes in Thousands",
        main = "Violence and Sexual Offences by Month", col=rainbow(12))


# Heatmap for violence crime locations by
library(ggmap)

map <- get_map(location=c(lon=-1.548567,
                          lat=53.801277), zoom=11, maptype='roadmap', color='bw')
ggmap(map, extent = "device") +
  geom_density2d(data = sexual_violence, aes(x = Longitude, y = Latitude), size = 0.3) +
  stat_density2d(data = sexual_violence, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01,
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)


# Time series of violence crimes between January to August
library(plotly)
m <- list(
  l = 190,
  r = 90,
  b = 200,
  t = 100,
  pad = 4
)
plot_ly(street, y = ~Month, x = ~Crime.type) %>% 
  layout(autosize = F, width = 1000, height = 700, margin = m)

# Findings
# -> Violence and Sexual Offences are the most commonly reported crimes in West Yorkshire betwwen January and August
# -> Leeds seems to be the center of all Sexual Violences Crimes
# -> July had the highest recorded cases of sexual violence Crimes
