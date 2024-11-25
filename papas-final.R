#Set Work Directory 
setwd("C:/Users/eleni/OneDrive/Desktop/sta215")


#Install "heaven" package
#install,packages("haven")

#Load "haven" package
library ("haven")

# Load final data
raw_data <- read.csv("raw_data.csv")

table(raw_data$location, raw_data$interaction_with_sister) 

table(raw_data$romantic,raw_data$lighting) 