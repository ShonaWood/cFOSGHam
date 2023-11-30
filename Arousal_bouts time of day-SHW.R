install.packages('circular')
install.packages('ggeasy')
install.packages("ggpubr")
library(circular)
library(readxl)
library(viridis)
library(ggforce)
library(tidyverse)
library(tidyquant)
library(concaveman)
library(gridExtra)
library(ggeasy)
library(ggpubr)

####Read data in######

Arousals <-  read_excel("Arousaltimes m_aur_updated.xlsx")
summary(Arousals)
Arousals$bout_nr <- as.factor(Arousals$bout_nr)
mean(Arousals$bout_d, na.rm = T)

#################################################
####statistical tests and probabilty plots##########
na<-circular(Arousals$nexit, units = 'hours', template = 'clock24')
ne<-circular(Arousals$nentry, units = 'hours', template = 'clock24')

median.circular(na, na.rm = T)
mean.circular(na, na.rm = T)
sd.circular(na, na.rm = T)
median.circular(ne, na.rm = T)
mean.circular(ne, na.rm = T)
sd.circular(ne, na.rm = T)


rayleigh.test(na) ###test whether uniform distribution in time of day of arousal (exit)###
rayleigh.test(ne) ###test whether uniform distribution in time of day of entry (entry)### 

kuiper.test(na, alpha = 0.05) ###test whether uniform distribution in time of day of arousal (exit)###
kuiper.test(ne, alpha = 0.05) ###test whether uniform distribution in time of day of entry (entry))###

##plot the statistical density - not sure what density is or how computed##
den <- ggplot(Arousals, aes(x=nentry))+ stat_density()+
  theme_minimal() + 
  ggtitle("Probabillity of ENTRY by Time of day") + 
  scale_y_continuous(limits = c(0.00, 0.1))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
den

dex <- ggplot(Arousals, aes(x=nexit))+ stat_density()+
  theme_minimal() + 
  ggtitle("Probabillity of EXIT by Time of day") + 
  scale_y_continuous(limits = c(0.00, 0.1))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dex


######################################################################################
##Circular histograms on 24hour clock - time of all arousals and entries for all animals in experiment##

##bins = this allows the data to be binned by time, 96 is 15 minute bins###

e<- ggplot(Arousals, aes(x=nentry, fill=ID)) + geom_histogram(bins = 96,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("Entry - all animals") +
  ggeasy::easy_center_title()

e

a<-ggplot(Arousals, aes(x=nexit)) + geom_histogram(bins = 96,colour="black" ,fill="firebrick") +
  scale_y_continuous(position = "right")+
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,1.2,-.5,-.5), "cm"))+
  ggtitle("Arousal - all animals") +
  ggeasy::easy_center_title()

a

ggarrange(e, a, ncol=2, nrow=1)

###########################################################################
###All animals entry by bout number on circular histograms on 24hour clock - binned hourly######


Arousals$bout_nr <- as.factor(Arousals$bout_nr)

entryb1<- ggplot(subset(Arousals, bout_nr %in% c("1") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("1st bout") +
  ggeasy::easy_center_title()
entryb1

entryb2<- ggplot(subset(Arousals, bout_nr %in% c("2") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("2nd bout") +
  ggeasy::easy_center_title()
entryb2

entryb3<- ggplot(subset(Arousals, bout_nr %in% c("3") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("3rd bout") +
  ggeasy::easy_center_title()
entryb3

entryb4<- ggplot(subset(Arousals, bout_nr %in% c("4") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("4th bout") +
  ggeasy::easy_center_title()
entryb4

entryb5<- ggplot(subset(Arousals, bout_nr %in% c("5") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("5th bout") +
  ggeasy::easy_center_title()
entryb5

entryb6<- ggplot(subset(Arousals, bout_nr %in% c("6") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("6th bout") +
  ggeasy::easy_center_title()
entryb6

entryb7<- ggplot(subset(Arousals, bout_nr %in% c("7") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("7th bout") +
  ggeasy::easy_center_title()
entryb7

entryb8<- ggplot(subset(Arousals, bout_nr %in% c("8") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("8th bout") +
  ggeasy::easy_center_title()
entryb8

entryb9<- ggplot(subset(Arousals, bout_nr %in% c("9") ),aes(x=nentry)) + geom_histogram(bins = 24,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_y_continuous(limits= c(0,5))+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("9th bout") +
  ggeasy::easy_center_title()
entryb9

grid.arrange(entryb1,entryb2,entryb3,entryb4,entryb5,entryb6,entryb7,entryb8, entryb9, ncol=3 )



##################################################################################
##Entry timing per individual###
entryb1H298 <- ggplot(subset(Arousals, ID %in% c("H298")), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H298 entry times")

entryb1H298

entryb1H300 <- ggplot(subset(Arousals, ID %in% c("H300") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H300")

entryb1H300

entryb1H301 <- ggplot(subset(Arousals, ID %in% c("H301") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H301")

entryb1H301

entryb1H319 <- ggplot(subset(Arousals, ID %in% c("H319") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H319")

entryb1H319

entryb1H211 <- ggplot(subset(Arousals, ID %in% c("H211") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H211")

entryb1H211

entryb1H216 <- ggplot(subset(Arousals, ID %in% c("H216") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H216")

entryb1H216

entryb1H230 <- ggplot(subset(Arousals, ID %in% c("H230") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H230")

entryb1H230

entryb1H236 <- ggplot(subset(Arousals, ID %in% c("H236") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H236")

entryb1H236

entryb1H241 <- ggplot(subset(Arousals, ID %in% c("H241") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H241")

entryb1H241

entryb1H243 <- ggplot(subset(Arousals, ID %in% c("H243") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H243")

entryb1H243

entryb1H248 <- ggplot(subset(Arousals, ID %in% c("H248") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H248")

entryb1H248

entryb1H250 <- ggplot(subset(Arousals, ID %in% c("H250") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H250")

entryb1H250

entryb1H254 <- ggplot(subset(Arousals, ID %in% c("H254") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H254")

entryb1H254

entryb1H263 <- ggplot(subset(Arousals, ID %in% c("H263") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H263")

entryb1H263

entryb1H273 <- ggplot(subset(Arousals, ID %in% c("H273") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H273")

entryb1H273

entryb1H275 <- ggplot(subset(Arousals, ID %in% c("H275") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H275")

entryb1H275

entryb1H276 <- ggplot(subset(Arousals, ID %in% c("H276") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H276")

entryb1H276

entryb1H278 <- ggplot(subset(Arousals, ID %in% c("H278") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H278")

entryb1H278

entryb1H307 <- ggplot(subset(Arousals, ID %in% c("H307") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H307")

entryb1H307

entryb1H327 <- ggplot(subset(Arousals, ID %in% c("H327") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H327")

entryb1H327

entryb1H224 <- ggplot(subset(Arousals, ID %in% c("H224") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H224")

entryb1H224

entryb1H274 <- ggplot(subset(Arousals, ID %in% c("H274") ), aes(x=nentry, fill= bout_nr)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("H274")

entryb1H274

ggarrange(entryb1H211,entryb1H216,entryb1H224,entryb1H230,entryb1H236,entryb1H241,entryb1H243,entryb1H248,entryb1H250,entryb1H254,entryb1H263,entryb1H273, entryb1H274, entryb1H275, entryb1H276, entryb1H278, entryb1H298, entryb1H300, entryb1H301, entryb1H307, entryb1H319, ncol=7, nrow=4, common.legend=TRUE, legend="bottom")
ggarrange(entryb1H211,entryb1H216,entryb1H224,entryb1H230,entryb1H236,entryb1H241, ncol=3, nrow=2, common.legend=TRUE, legend="bottom")
ggarrange(entryb1H243,entryb1H248,entryb1H250,entryb1H254,entryb1H263,entryb1H273, ncol=3, nrow=2, common.legend=TRUE, legend="bottom")
ggarrange(entryb1H274, entryb1H275, entryb1H276, entryb1H278, entryb1H298, entryb1H300, ncol=3, nrow=2, common.legend=TRUE, legend="bottom")
ggarrange(entryb1H301, entryb1H307, entryb1H319, ncol=3, nrow=1,  common.legend=TRUE, legend="bottom") 

#####################################################################
###Boxplot of Torpor and interbout duration based on bout number#####


bout_IBE<- ggplot(subset(Arousals, bout_nr %in% c("1","2","3","4", "5", "6", "7", "8")), aes(x=bout_nr, y= bout_d,fill=bout_nr), na.rm = T )+
  geom_boxplot( alpha=0.5, na.rm = T)+
  geom_point( size=1, na.rm = T, position = position_dodge2(width = 0.25))+
  scale_fill_viridis(discrete = T, alpha=1, option = "D", direction=-1) +
  theme_bw()+ ggtitle("Torpor bout duration")
bout_IBE

IBE_all<- ggplot(subset(Arousals, bout_nr %in% c("1","2","3","4", "5", "6", "7", "8")), aes(x=bout_nr, y= IBE_d, fill=bout_nr), na.rm = T )+
  geom_boxplot( alpha=0.5, na.rm = T)+
  geom_point(size=1, na.rm = T, position = position_dodge2(width = 0.25))+
  scale_fill_viridis(discrete = T, alpha=1, option = "D", direction=-1) +
  theme_bw()+ ggtitle("Interbout duration")
IBE_all
ggarrange(bout_IBE, IBE_all, common.legend = TRUE, legend ="right")

#######################################################################


