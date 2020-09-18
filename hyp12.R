# Other sources of variation
# GGplot codes

rm(list=ls(all=TRUE)) # Poof, all gone. Tabula rasa!
setwd("G:/RCodes/CH2")


########################################################################################################################
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

### Sources of variation other than water levels

pal <- viridisLite::viridis(3)
pal

#Time x range
p <- ggplot(rawdata, aes(x = time2, y = fruitmass.neg, colour = factor(range))) +
  ylab("Reproductive Trait PC1\n(Fruit mass\n& Reproductive biomass)") + xlab("Planting day\nfrom last frost") +
  guides(color=guide_legend(title="Range", alpha=0.5, title.position = "top", title.hjust = 0.5))
p <- p + stat_smooth(method = "gam", formula = y ~ s(x, k = 3),
                     se = F, size = 2.5) 
p <- p +  scale_color_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p + annotate("text", 
                  x = c(2, 2),
                  y = c(-.85, 1.15),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.title = element_text(size = 18)) 
p1<- p + theme(legend.position = "top", legend.box = "horizontal")
print(p1) 



#Time x range
p <- ggplot(rawdata, aes(x = time2, y = rosdiam.neg, colour = factor(range))) +
  ylab("Vegetative Trait PC1\n(Leaf traits)") + 
  xlab("Planting day\nfrom last frost") + 
  guides(color=guide_legend(title="Range", alpha=0.5, title.position = "top", title.hjust = 0.5))
p <- p + stat_smooth(method = "gam", formula = y ~ s(x, k = 3),
                     se = F, size = 2.5) 
p <- p +  scale_color_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p  + annotate("text", 
                   x = c(30),
                   y = c(1),
                   label = "paste(italic(P), \" < 0.001\")", parse = TRUE,
                   color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(2, 2),
                  y = c(-1.5, 1.15),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.title = element_text(size = 18)) 
p2<- p + theme(legend.position = "top", legend.box = "horizontal")
print(p2) 

#Time x range
p <- ggplot(rawdata, aes(x = time2, y = height.neg, colour = factor(range))) +
  ylab("Vegetative Trait PC2\n(Branch traits)") + xlab("Planting day\nfrom last frost") +
  guides(color=guide_legend(title="Range", alpha=0.5, title.position = "top", title.hjust = 0.5))
p <- p + stat_smooth(method = "gam", formula = y ~ s(x, k = 3),
                     se = F, size = 2.5) 
p <- p +  scale_color_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p  + annotate("text", 
                   x = c(30),
                   y = c(1),
                   label = "paste(italic(P), \" < 0.001\")", parse = TRUE,
                   color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(2, 2),
                  y = c(-1.5, 1.15),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.title = element_text(size = 18)) 
p3<- p + theme(legend.position = "top", legend.box = "horizontal")
print(p3) 



pal <- viridisLite::magma(6)
pal

#Time x water
p <- ggplot(rawdata, aes(x = time2, y = fruitmass.neg, colour = factor(treatment))) +
  ylab("Reproductive Trait PC1\n(Fruit mass &\nReproductive biomass)") + xlab("Planting day\nfrom last frost") +
  guides(color=guide_legend(title="Soil moisture level (ml/day)", alpha=0.5, title.position = "top", title.hjust = 0.5))
p <- p + stat_smooth(method = "gam", formula = y ~ s(x, k = 3),
                     se = F, size = 2.5) 
p <- p +  scale_color_manual(name  ="Range",values=c("#FCFDBFFF", "#FE9F6DFF", "#DE4968FF"))
p <- p  + annotate("text", 
                   x = c(30),
                   y = c(1),
                   label = "paste(italic(P), \" = 0.033\")", parse = TRUE,
                   color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(2, 2),
                  y = c(-1.5, 1.15),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.title = element_text(size = 18)) 
p4<- p + theme(legend.position = "top", legend.box = "horizontal")
print(p4) 

#Time x water
p <- ggplot(rawdata, aes(x = time2, y = rosdiam.neg, colour = factor(treatment))) +
  ylab("Vegetative Trait PC1\nLeaf traits") + 
  xlab("Planting day\nfrom last frost") + 
  guides(color=guide_legend(title="Soil moisture level (ml/day)", alpha=0.5, title.position = "top"))
p <- p + stat_smooth(method = "gam", formula = y ~ s(x, k = 3),
                     se = F, size = 2.5) 
p <- p +  scale_color_manual(name  ="Range",values=c("#FCFDBFFF", "#FE9F6DFF", "#DE4968FF"))
p <- p  + annotate("text", 
                   x = c(30),
                   y = c(2),
                   label = "paste(italic(P), \" < 0.001\")", parse = TRUE,
                   color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(2, 2),
                  y = c(-1.5, 2),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.title = element_text(size = 18)) 
p5<- p + theme(legend.position = "top", legend.box = "horizontal")
print(p5) 

#Time x water
p <- ggplot(rawdata, aes(x = time2, y = height.neg, colour = factor(treatment))) +
  ylab("Vegetative Trait PC2\n(Branch traits)") + xlab("Planting day\nfrom last frost") +
  guides(color=guide_legend(title="Soil moisture level (ml/day)", alpha=0.5, title.position = "top"))
p <- p + stat_smooth(method = "gam", formula = y ~ s(x, k = 3),
                     se = F, size = 2.5) 
p <- p +  scale_color_manual(name  ="Range",values=c("#FCFDBFFF", "#FE9F6DFF", "#DE4968FF"))
p <- p  + annotate("text", 
                   x = c(30),
                   y = c(1.75),
                   label = "paste(italic(P), \" < 0.001\")", parse = TRUE,
                   color="gray38", size=7)
p <- p + annotate("text", 
                  x = c(2, 2),
                  y = c(-1.5, 2),
                  label = c("High", "Low"),
                  color="gray38", size=7)
p <- p + scale_y_reverse()
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.text = element_text(size = 18)) 
p <- p+ theme(legend.title = element_text(size = 18)) 
p6<- p + theme(legend.position = "top", legend.box = "horizontal")
print(p6) 


library(ggpubr)
library(egg)
require(gridExtra)

tiff('timefig3.tiff', units="in", width=16, height=12, res=900)
timefig3b<- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2, labels = c("a)", "b)", "c)", "d)", "e)", "f)"))
timefig3b
