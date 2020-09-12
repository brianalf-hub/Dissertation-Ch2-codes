#Ch.2 PCA

#Started 13-Jan-2019
#Chapter Two of Brian Alfaro's dissertation, on phenotypic plasticity

#This script was made for creating composite PCA variables for reananalysis for chapter 2

rm(list=ls(all=TRUE)) # Poof, all gone. Tabula rasa!
setwd("K:/Phenotypic plasticy")

#Import data set for selection analysis, four outliers removed in Excel
library(readr)
lifepc <-read_csv("K:/Phenotypic plasticy/reanalysis/lifepc.csv")

#Import data set for selection analysis, four outliers removed in Excel
library(readr)
repropc <-read_csv("K:/Phenotypic plasticy/reanalysis/repropc.csv")


# call up the following installed packages-----------------
library(car)
library(Hmisc)
library(RColorBrewer)
library(wesanderson)
library(piecewiseSEM)
library(MASS)
library(splines)
library(extrafont)
library(ggplot2)
library(extrafont)
library(plyr)
#font_import()
loadfonts(device="win")
library(mice)
library(miceadds)
library(mi)
#transformations
library(rcompanion)
library(MASS)

number <-rawdata$number
block <- rawdata$block
time <- rawdata$time
treatment <- rawdata$treatment
family <- rawdata$family	
leafnumber <- rawdata$leafnumber	
rosdiam <- rawdata$rosdiam	
flwtime <- rawdata$flwtime	
height <- rawdata$height	
branchnumber <- rawdata$branchnumber
branchlength <- rawdata$branchlength
fruits <- rawdata$fruits	
lobes <- rawdata$lobes
leaflength <- rawdata$leaflength
fruitmass <- rawdata$fruitmass
seednumber <- rawdata$seednumber
biomass <- rawdata$biomass
repalloc <- rawdata$repalloc
vegalloc <- rawdata$vegalloc
repromass <- rawdata$repromass
relfit <-rawdata$relfit
range <- rawdata$range
population <- rawdata$pop
heightbran <- rawdata$heightbran

leafnumber <-as.numeric(leafnumber) 
rosdiam <- as.numeric(rosdiam)
flwtime <- as.numeric(flwtime)
height <- as.numeric(height)
branchnumber <- as.numeric(branchnumber)
branchlength <- as.numeric(branchlength)
fruits <- as.numeric(fruits)
lobes <- as.numeric(lobes)
leaflength <- as.numeric(leaflength)
fruitlength <- as.numeric(fruitlength)
fruitmass <- as.numeric(fruitmass)
seednumber <- as.numeric(seednumber)
biomass <- as.numeric(biomass)
time <- as.numeric(time)
repalloc <- as.numeric(repalloc)
vegalloc <- as.numeric(vegalloc)
repromass <- as.numeric(repromass)
relfit <-as.numeric(relfit)
heightbran <-as.numeric(heightbran)

rawdata$block <- factor(rawdata$block)
rawdata$treatment <- factor(rawdata$treatment)
rawdata$family <- factor(rawdata$family)
rawdata$pop<- factor(rawdata$pop)

rawdata$range <- factor(rawdata$range, levels=c("Native", "Invasive", "Landrace"))
rawdata$treatment <- factor(rawdata$treatment, levels=c("250", "450", "750"))


# Data Transformation

# Levene's tests: I wanted to make sure that I knew which trait variables did not 
# show Homogeneity of variances, so I ran Levene's test 

# Brown-Forsythe test: I also wanted to make sure that I knew that which groups did not 
# show homogeneity of variances (an assumption of ANOVA-type tests) so I ran B-F test

# After identifying which trait variables have non-normal distributions/non-homogeneous variances
# I performed boxcox transformations. Below is just an example script, but I ran several of these for
# each trait variable (need to create some type of helper function so I don't have to do this repeatedly in the future
# for multiple variables)

box <- boxcox(leafnumber ~ 1,              # Transform variable as a single vector
              lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
cox <- data.frame(box$x, box$y)            # Create a data frame with the results
cox2 <- cox[with(cox, order(-cox$box.y)),] # Order the new data frame by decreasing y
cox2[1,]                        # Display the lambda with the greatest log likelihood
lambda <- cox2[1, "box.x"]                 # Extract that lambda
leafnumber.box <- (leafnumber ^ lambda - 1)/lambda   # Transform the original data
plotNormalHistogram(leafnumber.box)
shapiro.test(leafnumber.box)



# Note: some variables that are ratio values need to be transformed (if required)
# using arcsine transformation



