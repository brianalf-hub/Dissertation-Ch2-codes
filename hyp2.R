# Chapter 2 - Hypothesis 2


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



#### Reaction norms - Figure 2

trait <- ddply(rawdata,.(treatment,range),summarise, val = mean(Rep.PC1))
p <- ggplot(rawdata, aes(x = factor(treatment), y =Rep.PC1, colour = range)) + 
  geom_point(data = trait, aes(y = val)) +
  geom_line(data =trait, aes(y =val, group = range)) + 
  theme_bw() +  ylab("Reproductive Trait PC1\n(Individual fruit mass &\nReproductive biomass)") + 
  xlab("Soil moisture level (ml/day)") 
p <- p  + annotate("text", 
                   x = c(2.55),
                   y = c(1),
                   label = "paste(italic(P), \" < 0.001\")", parse = TRUE,
                   color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(0.7, 0.7),
                  y = c(-.85, 1.15),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p +  scale_colour_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p + stat_summary(fun = mean, geom = "point", size=5, aes(group=range))
p <- p + stat_summary(fun = mean, geom = "line", size=2, aes(group = range))
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.25))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"), axis.text.x  = element_text(vjust=0.5, size=18,  colour = "gray38"))
p <- p + theme(axis.title.y = element_text(size=18, colour = "gray38"), axis.text.y  = element_text(vjust=0.5, size=18,  colour = "gray38"))
p4<- p + theme(legend.position="none")
print(p4)


trait <- ddply(rawdata,.(treatment,range),summarise, val = mean(Life.PC1))
p <- ggplot(rawdata, aes(x = factor(treatment), y =Life.PC1, colour = range)) + 
  geom_point(data = trait, aes(y = val)) +
  geom_line(data =trait, aes(y =val, group = range)) + 
  theme_bw() +  ylab("Vegetative Trait PC1\n(Leaf traits)") +
  xlab("Soil moisture level (ml/day)") 
p <- p  +guides(color=guide_legend(title="Range", alpha=0.5, title.position = "bottom", title.hjust = 0.5))
p <- p  + annotate("text", 
                   x = c(2.55),
                   y = c(1),
                   label = "paste(italic(P), \" = 0.099\")", parse = TRUE,
                   color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(0.7, 0.7),
                  y = c(-.85, 1.15),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p +  scale_colour_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p + stat_summary(fun = mean, geom = "point", size=5, aes(group=range))
p <- p + stat_summary(fun = mean, geom = "line", size=2, aes(group = range))
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.title = element_text(size = 18)) 
p5<- p + theme(legend.position="none")
print(p5)


trait <- ddply(rawdata,.(treatment,range),summarise, val = mean(Life.PC2))
p <- ggplot(rawdata, aes(x = factor(treatment), y =Life.PC2, colour = range)) + 
  geom_point(data = trait, aes(y = val)) +
  geom_line(data =trait, aes(y =val, group = range)) + 
  theme_bw() +  ylab("Vegetative Trait PC2\n(Branch architecture)") + xlab("Soil moisture level (ml/day)") 
p <- p  + annotate("text", 
                   x = c(2.55),
                   y = c(1),
                   label = "paste(italic(P), \" < 0.001\")", parse = TRUE,
                   color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(0.7, 0.7),
                  y = c(-.85, 1.15),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p +  scale_colour_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p + stat_summary(fun = mean, geom = "point", size=5, aes(group=range))
p <- p + stat_summary(fun = mean, geom = "line", size=2, aes(group = range))
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.25))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"), axis.text.x  = element_text(vjust=0.5, size=18,  colour = "gray38"))
p <- p + theme(axis.title.y = element_text(size=18, colour = "gray38"), axis.text.y  = element_text(vjust=0.5, size=18,  colour = "gray38"))
p6<- p + theme(legend.position="none")
print(p6)



library(ggpubr)
library(egg)
require(gridExtra)
tiff('figure4z.tiff', units="in", width=14, height=4.5, res=1000)
figure4z <- ggarrange(p4, p5, p6, ncol = 3, nrow = 1, labels = c("a)", "b)", "c)"))
figure4z


