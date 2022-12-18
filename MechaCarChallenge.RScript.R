# Deliverable 1 -- Linear Regression to Predict MPG
# Import dpylr library
library(tidyverse)
library(dplyr)
# Import and read in the MechaCar_mpg.csv file as a dataframe
mpg_data <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)
head(mpg_data)
# Perform linear regression using the lm() function
mpg_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mpg_data)
# Using the summary() function, determine the p-value and the 
#r-squared value for the linear regression model.
summary(mpg_lm)

# Deliverable 2 -- Create Visualizations for the Trip Analysis
# Import and read Suspension_Coil.csv file as a table
suspension_coil <- read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
# Create a total_summary dataframe using the summarize() function to get the mean, median, variance, and standard deviation.
total_summary <- suspension_coil %>% summarize(mean = mean(PSI), median = median(PSI), variance = var(PSI), stdev = sd(PSI))

# Create a lot_summary dataframe using the group_by() and the summarize() functions to group each manufacturing lot 
# by the mean, median, variance, and standard deviation of the suspension coil’s PSI column
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(mean = mean(PSI), median = median(PSI), variance = var(PSI), stdev = sd(PSI),.groups = "keep")

# Deliverable 3
#t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.  
t.test(suspension_coil$PSI, mu=1500)

t.test(subset(suspension_coil,Manufacturing_Lot == "Lot1")$PSI, mu=1500)
t.test(subset(suspension_coil,Manufacturing_Lot == "Lot2")$PSI, mu=1500)
t.test(subset(suspension_coil,Manufacturing_Lot == "Lot3")$PSI, mu=1500)
