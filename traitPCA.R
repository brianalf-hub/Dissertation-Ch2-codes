# Codes for Ch. 2 manuscript by Brian Alfaro

# The garden expriment was started in spring 2017, sample processing and data entry was completed in 2018.
# This experiment had half the variables of the plasticgarden2 greenhouse experiment, but we still wanted to reduce variables for the first couple of 
# steps in our analyses. We used the trusty prcomp function of base R to reduce it to 3 variables.
# This is the general code for the PCA. 

#Start
rm(list=ls(all=TRUE)) #Clears previous work in R#maptools, 

#Set working directory
setwd("G:/RCodes")

library(readr)
plasticgarden2 <- read_csv("CH2/plasticgarden2.csv")
View(plasticgarden2)

# call up the following installed packages-----------------
library(car)
library(Hmisc)
library(RColorBrewer)
library(ggplot2)
require(mgcv)
library(plotly)

# PCA for one traits

#Set names of trait (continuous) and categorical variables

#Plant architecture trait group
height	<- plasticgarden2$height
rosdiam	<- plasticgarden2$rosdiam
leaflength	<- plasticgarden2$rosdiam

# These are the variables I wanted to include for the one trait PCA. 
# I centered the data (z-scored) so I can compare the variation of different trait values

one.pca <- prcomp(~ height + rosdiam + leaflength, center=TRUE, scale=TRUE)
summary(one.pca)
loadings(one.pca)
head(one.pca$scores)
one.pca

# prcomp codes; Use your pC axes as regression predictors
# these are a few lines of code to include your PCA axes as predictor variables; you need to save a new .csv file
# but make sure to remove the first column (R automatically numbers each row) to make your variable names match actual data columns

one.axes <- predict(one.pca, newdata = plasticgarden2)
one.dat <- cbind(plasticgarden2, one.axes[,1:3])
head(one.axes, 4)
onepc1 <- plasticgarden2
onepc1[names(one.dat)] <- one.dat
onepc1
write.table(onepc1, "G:/CH1/a_reanalysis/onepc1.csv", sep="\t") 
