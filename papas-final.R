#Set Work Directory 
setwd("H:/sta215")

#Install "heaven" package
#install,packages("haven")

#Load "haven" package
library ("haven")

# Load final data
raw_data <- read.csv("raw_data.csv")

#table 1
table(raw_data$location)
mean(raw_data$location)
sd(raw_data$location)

table(raw_data$romantic)
mean(raw_data$romantic)
sd(raw_data$romantic)

table(raw_data$interaction_with_sister)

#box plot 
anova <- aov(interaction_with_sister ~lighting, data = raw_data)
summary(anova)
boxplot(interaction_with_sister ~lighting, data = raw_data)

# Table 1: Descriptive Statistics
# Quantitative Variables
cat("Location:\n")
print(table(raw_data$location))
cat("Mean:", mean(raw_data$location, na.rm = TRUE), "\n")
cat("SD:", sd(raw_data$location, na.rm = TRUE), "\n\n")

cat("Interaction with Sister:\n")
print(table(raw_data$interaction_with_sister))
cat("Mean:", mean(raw_data$interaction_with_sister, na.rm = TRUE), "\n")
cat("SD:", sd(raw_data$interaction_with_sister, na.rm = TRUE), "\n\n")

# Qualitative Variables
cat("Romantic:\n")
print(table(raw_data$romantic))

cat("Lighting:\n")
print(table(raw_data$lighting))

# Boxplot: Interaction with Sister by Lighting
boxplot
boxplot(
interaction_with_sister ~ lighting,
data = raw_data,
main = "Boxplot of Interaction with Sister by Lighting",
xlab = "Lighting",
ylab = "Interaction with Sister",
notch = TRUE          # Adds notches for median comparison
)
# ANOVA for Interaction with Sister by Lighting
fit_aov <- aov(interaction_with_sister ~ lighting, data = raw_data)
summary(fit_aov)

# Scatter Plot: Location vs. Interaction with Sister
plot(
  raw_data$funniness, raw_data$interaction_with_sister,
  main = "Scatter Plot of Interaction with Sister vs. funiness",
  xlab = "Interaction with Sister",
  ylab =  "Funniness",
  pch = 19,
)
# Linear Model: Location vs. Interaction with Sister
fit_lm <- lm(interaction_with_sister ~ location, data = raw_data)
abline(fit_lm, col = "red", lwd = 2)
summary(fit_lm)

# Residual Plot
residuals <- resid(fit_lm)
plot(
  raw_data$location, residuals,
  main = "Residual Plot of Interaction with Sister vs. Location",
  xlab = "Location (Predictor)",
  ylab = "Residuals",
  pch = 19,
  col ="purple"
)
abline(h = 0, col = "red", lwd = 2, lty = 2)

fit_lm_romantic <- lm (romantic ~ lighting, data = raw_data) 
residuals_romantic<-resid(fit_lm_romantic)
plot( 
  raw_data$lighting, residuals_romantic,
  main = "Residual Plot of Romantic vs. Lighting",
  xlab = "Lighting (Predictor)",
  ylab = "Residuals",
  pch = 19,
  col = "purple" )
 
# Contingency Tables for Qualitative Variables
cat ("Contingency Table: Location and Interaction with Sister\n")
print (table(raw_data$location, raw_data$interaction_with_sister))

cat("Contingency Table: Romantic and Lighting\n")
print(table(raw_data$romantic, raw_data$lighting))
