# Ch2 Hypothesis 1

# R script for Alfaro Dissertation Chapter 2

#Lines 7 to 52 are to call up packages, name and order variables, 
rm(list=ls(all=TRUE)) # Poof, all gone. Tabula rasa!
setwd("G:/RCodes/CH2")

#Import data set for selection analysis, four outliers removed in Excel
library(readr)
rawdata <-read_csv("G:/RCodes/CH2/plasticgarden2.csv")
View(rawdata)

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
loadfonts(device="win")
library(mi)
library(rcompanion)
library(agricolae)
library(ggpubr)
theme_set(theme_pubr())
library(viridis)
library(mgcv)

number <-rawdata$number
block <- rawdata$block
time <- rawdata$time
treatment <- rawdata$treatment
relfit <-rawdata$relfit
range <- rawdata$range
population <- rawdata$pop
fruitmass.neg <- rawdata$Rep.PC1
rosdiam.neg <- rawdata$Life.PC1
height.neg <- rawdata$Life.PC2

rawdata$block <- factor(rawdata$block)
rawdata$treatment <- factor(rawdata$treatment)
rawdata$family <- factor(rawdata$family)
rawdata$pop <- factor(rawdata$pop)
rawdata$treatment <- factor(rawdata$treatment)
rawdata$range <- factor(rawdata$range, levels=c("Native", "Invasive", "Landrace"))
rawdata$treatment <- factor(rawdata$treatment, levels=c("250", "450", "750"))

################################################################################

# GLM mixed models. So I ran mixed effects models in SAS, it was just a quicker 
# and cleaner output for what I'm trying to do. The model is: 

# Trait value = Planting Date + Range + Soil Moisture Level + Block + Population within Range 
#               + Block × Range + Range × Soil Moisture Level + 
#                 Planting Date × Soil Moisture Level + Planting date × Range

# This SAS code produces several 

################################################################################

# Boxplots for Figure 1
# I would have used the regular boxplots functions of R to do this, but because 
# this is for publication-quality figures, I used ggplot2 and my new favorite 
# color schemes in the viridis package
# I also wanted to annotate p-values etc. in R, so ggplot2 allows me to do that


pal <- viridisLite::viridis(3)
pal

p <- ggplot(rawdata, aes(x=range, y=fruitmass.neg, fill=range)) + 
  ylab("Reproductive Trait PC1\n(Individual fruit mass &\nReproductive biomass)") + 
  xlab("Range") +
  guides(color=guide_legend(title="Range", alpha=0.75, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", notch="TRUE", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p + annotate("text", 
                  x = c(0.68, 0.68),
                  y = c(-6, 6),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p + scale_y_reverse()
p1 <- p + theme(legend.position="none")
print(p1) 


p <- ggplot(rawdata, aes(x=range, y=rosdiam.neg, fill=range)) + 
  ylab("Vegetative Trait PC1\n(Leaf traits)") + 
  xlab("Range") +
  guides(color=guide_legend(title="Range", alpha=0.5, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", notch="TRUE", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p + annotate("text", 
                  x = c(1,2,3),
                  y = c(-4.2, -3.6, -3.2),
                  label = c("b", "b", "a"),
                  color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(0.68, 0.68),
                  y = c(-7, 5),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p + scale_y_reverse()
p2 <- p + theme(legend.position="none")
print(p2) 



p <- ggplot(rawdata, aes(x=range, y=height.neg, fill=range)) + 
  ylab("Vegetative Trait PC2\n(Branch architecture)") + 
  xlab("Range")
guides(color=guide_legend(title="Range", alpha=0.5, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", notch="TRUE", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p + annotate("text", 
                  x = c(0.925,1.95,2.95),
                  y = c(-3.6, -2.7, -2.3),
                  label = c("b", "a", "a"),
                  color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(0.68, 0.68),
                  y = c(-6, 6),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p + scale_y_reverse()
p3 <- p + theme(legend.position="none")
print(p3) 

# this script at the bottom puts it all together in one panel

library(ggpubr)
library(egg)
require(gridExtra)
tiff('figure3.tiff', units="in", width=14, height=4.5, res=1000)
figure3 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1, labels = c("a)", "b)", "c)"))
figure3



summary(relfit.hsd <- aov(relfit ~range, data = rawdata))
HSD.test(relfit.hsd, "range", console=TRUE)

summary(fruitmass.neg.hsd <- aov(fruitmass.neg ~range, data = rawdata))
HSD.test(fruitmass.neg.hsd, "range", console=TRUE)

summary(repalloc.pos.hsd <- aov(repalloc.pos~range, data = rawdata))
HSD.test(repalloc.pos.hsd, "range", console=TRUE)

summary(rosdiam.neg.hsd <- aov(rosdiam.neg~range, data = rawdata))
HSD.test(rosdiam.neg.hsd, "range", console=TRUE)

summary(height.neg.hsd <- aov(height.neg~range, data = rawdata))
HSD.test(height.neg.hsd, "range", console=TRUE)

