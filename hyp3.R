#Hypothesis 3

rm(list=ls(all=TRUE)) # Poof, all gone. Tabula rasa!
setwd("G:/RCodes/CH2")


########################################################################################################################
library(readr)
fitness <-read_csv("G:/RCodes/CH2/popmeans.csv")
View(fitness)


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

total3 <- fitness$total.3
relfit2 <- fitness$relfit2
total <- fitness$total
pop <-fitness$pop
range <- fitness$range
relfit <- fitness$relfit.mean
branchlength <- fitness$branchlength.cv
seednumber <- fitness$seednumber.cv
rosdiam <- fitness$rosdiam.cv
leaflength <- fitness$leaflength.cv
fruitmass <- fitness$fruitmass.cv
height <- fitness$height.cv
repalloc <- fitness$repalloc.cv
repromass <- fitness$repromass.cv
fitness$range <- factor(fitness$range, levels=c("Native", "Invasive", "Landrace"))




################################################################################################
#Figure 3
pal <- viridisLite::viridis(3)
pal

p <- ggplot(fitness, aes(x=range, y=fruitmass, fill=range)) + ylab("Individual fruit mass\n Population CV") + xlab("Range") +
  guides(color=guide_legend(title="Range", alpha=0.75, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p1 <- p + theme(legend.position="none")
print(p1) 


#tiff('boxquantity.tiff', units="in", width=4, height=4, res=1600)
p <- ggplot(fitness, aes(x=range, y=repromass, fill=range)) + ylab("Reproductive biomass\n Population CV") + xlab("Range")
guides(color=guide_legend(title="Range", alpha=0.75, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p2 <- p + theme(legend.position="none")
print(p2) 


#tiff('boxquantity.tiff', units="in", width=4, height=4, res=1600)
p <- ggplot(fitness, aes(x=range, y=rosdiam, fill=range)) + ylab("Rosette diameter\n Population CV") + xlab("Range")
guides(color=guide_legend(title="Range", alpha=0.75, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p3 <- p + theme(legend.position="none")
print(p3) 

p <- ggplot(fitness, aes(x=range, y=leaflength, fill=range)) + ylab("Leaf length\n Population CV") + xlab("Range")
guides(color=guide_legend(title="Range", alpha=0.75, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p4 <- p + theme(legend.position="none")
print(p4) 

p <- ggplot(fitness, aes(x=range, y=height, fill=range)) + ylab("Height\n Population CV") + xlab("Range")
guides(color=guide_legend(title="Range", alpha=0.75, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p5 <- p + theme(legend.position="none")
print(p5) 

p <- ggplot(fitness, aes(x=range, y=branchlength, fill=range)) + ylab("Lateral branch length\n Population CV") + xlab("Range")
guides(color=guide_legend(title="Range", alpha=0.75, title.position = "top", title.hjust = 0.5)) 
p <- p + geom_boxplot(color="gray45", size=0.5, width=0.5, outlier.size = 2, alpha=1) + 
  stat_summary(fun=mean, colour="gray38", geom="point", shape=18, size=5,show.legend = FALSE) 
p <- p + annotate("text", 
                  x = c(1,2,3),
                  y = c(47, 71, 49.5),
                  label = c("b", "a", "b"),
                  color="gray38", size=7)
p <- p +  scale_fill_manual(values=pal)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(vjust=0.5, size=18, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p6 <- p + theme(legend.position="none")
print(p6) 


library(ggpubr)
library(egg)
require(gridExtra)
tiff('figure6.tiff', units="in", width=10, height=10, res=900)
figure6 <- ggarrange(p1, p2, p3, p4, p5, p6, ncol= 2, nrow=3,
                      labels = c("a)", "b)", "c)", "d)", "e)", "f)"))
figure6



#CVs test

p <- ggplot(fitness, aes(x = branchlength, y = relfit, colour = range)) + xlab("Lateral branch length\n Population CV") + ylab("Relative fitness\n Population mean") + geom_point(size=5)
p <- p +  scale_colour_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p + geom_smooth(method="lm", se = FALSE, size=2, fullrange=T)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p + theme(legend.position="bottom")
print(p) 

library(ggpubr)
library(egg)
require(gridExtra)

tiff('figure7.tiff', units="in", width=4, height=4, res=1200)
figure7 <- ggarrange(p, ncol= 1)
figure7


#CVs test Total plasticity

p <- ggplot(fitness, aes(x = total3, y = relfit, colour = range)) + xlab("Lateral branch length\n Population CV") + ylab("Relative fitness\n Population mean") + geom_point(size=5)
p <- p +  scale_colour_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p + geom_smooth(method="lm", se = FALSE, size=2, fullrange=T)
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p + theme(legend.position="bottom")
print(p) 


library(ggpubr)
require(gridExtra)

dev.off()

tiff('figure6e.tiff', units="in", width=4, height=4, res=1200)
figure6e <- ggarrange(p, ncol= 1,common.legend = TRUE, legend="bottom")
figure6e

#Global graphs
p <- ggplot(fitness, aes(x = total3, y = relfit2, colour = total)) + xlab("Lateral branch length\n Population CV") + ylab("Relative fitness\n Population mean") + geom_point(size=5)
#p <- p +  scale_colour_manual(name  ="Range",values=c("#440154FF", "#21908CFF", "#FDE725FF"))
p <- p + stat_smooth(method = "gam", formula = y ~ s(x, k = 6),
                     se =F, size = 2.5, fullrange=T) 
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=0.5))
p <- p + theme(axis.title.x = element_text(size=18, colour = "gray38"),
               axis.text.x  = element_text(vjust=0.5, size=16, colour = "gray38"))
p <- p+ theme(axis.title.y = element_text(size=18, colour = "gray38"),
              axis.text.y  = element_text(size=16, colour = "gray38"))
p <- p + theme(legend.position="bottom")
print(p) 
