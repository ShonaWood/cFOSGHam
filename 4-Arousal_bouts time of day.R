
library(circular)
library(readxl)
library(viridis)
library(ggforce)
library(tidyverse)
library(tidyquant)
library(concaveman)
library(gridExtra)
library(ggeasy)

Arousals <-  read_excel("./arousaltimes/Arousaltimes m_aur_updated.xlsx", sheet =2)
td <-  read_excel("./arousaltimes/Arousaltimes m_aur_updated.xlsx", sheet =3)
summary(Arousals)

#clock plots
################################################################################
#convert number to circular format and adjust clocktime to zt time.
#arousals:
na<-circular(Arousals$nexit, units = 'hours', template = 'clock24')
to_zt <- circular(9, units = 'hours', template = 'clock24')
zt_nexit <- na - to_zt
zt_nexit <- ifelse(zt_nexit < 0, zt_nexit + 24, zt_nexit) #convert negative nr to positive clock format
zt_nexit <- circular(zt_nexit, units = 'hours', template = 'clock24')


#entries:
ne<-circular(Arousals$nentry, units = 'hours', template = 'clock24')
zt_nentry <- ne - to_zt
zt_nentry <- ifelse(zt_nentry < 0, zt_nentry + 24, zt_nentry)
zt_nentry <-circular(zt_nentry, units = 'hours', template = 'clock24')


#arousals and entries
par(mar=c(.5,.5,.5,.5))
par(mfrow=c(1,1))


zt_arousal <- function() {
  plot(zt_nexit, stack = TRUE, sep = 0.025, shrink = 1.1, cex = 2.1, col = 'gray30')
  arrows.circular(mean.circular(zt_nexit, na.rm = TRUE), y = rho.circular(zt_nexit, na.rm = TRUE), lty = 1, lwd = 3, shrink = 1.25)
  arrows.circular(mean.circular(zt_nexit, na.rm = TRUE) + sd.circular(zt_nexit, na.rm = TRUE) * 2, y = rho.circular(zt_nexit, na.rm = TRUE), lty = 5, lwd = 1, shrink = 1.25, col = alpha("black", 0.4), pch = 16)
  arrows.circular(mean.circular(zt_nexit, na.rm = TRUE) - sd.circular(zt_nexit, na.rm = TRUE) * 2, y = rho.circular(zt_nexit, na.rm = TRUE), lty = 5, lwd = 1, shrink = 1.25, col = alpha("black", 0.4), pch = 16)
}

#svg("./graphix/zt_arousal.svg", width = 10, height = 8)
#zt_arousal()
#dev.off()

zt_entry <- function() {
  plot(zt_nentry , stack = T, sep=0.025, shrink=1.1, cex = 2.1, col = 'gray30')
  arrows.circular(mean.circular(zt_nentry , na.rm = T),y=rho.circular(zt_nentry , na.rm = T), lty=1, lwd=3, shrink = 1.25)
  arrows.circular(mean.circular(zt_nentry , na.rm = T)+sd.circular(zt_nentry , na.rm = T)*2, y=rho.circular(zt_nentry, na.rm = T), lty=5, lwd=1, shrink =1.25, col = alpha("black", 0.4), pch=16)
  arrows.circular(mean.circular(zt_nentry , na.rm = T)-sd.circular(zt_nentry , na.rm = T)*2, y=rho.circular(zt_nentry, na.rm = T),lty=5, lwd=1, shrink = 1.25, col = alpha("black", 0.4), pch=16)
}

  
#svg("./graphix/zt_entry.svg", width = 10, height = 8)
#zt_entry()
#dev.off()



#################################################
#statistical tests


median.circular(zt_nexit, na.rm = T)
mean.circular(zt_nexit, na.rm = T)
sd.circular(zt_nexit, na.rm = T)
median.circular(zt_nentry, na.rm = T)
mean.circular(zt_nentry, na.rm = T)
sd.circular(zt_nentry, na.rm = T)


rayleigh.test(na)
rayleigh.test(zt_nentry)


kuiper.test(zt_nexit, alpha = 0.05)
kuiper.test(zt_nentry, alpha = 0.01)





################################################################################
#Test Drops
summary(td)


tda<-circular(td$nexit_test, units = 'hours', template = 'clock24')
to_zt <- circular(9, units = 'hours', template = 'clock24')

zt_test_arousal <- tda - to_zt

#entries:
tde<-circular(td$nentry_test, units = 'hours', template = 'clock24')

zt_test_nentry <- tde - to_zt

par(mar=c(.5,.5,.5,.5))
par(mfrow=c(1,1))


pl_zt_test_arousal <- function() {
  plot(zt_test_arousal, stack = TRUE, sep = 0.025, shrink = 1.1, cex = 2.1, col = 'gray30')
  arrows.circular(mean.circular(zt_test_arousal, na.rm = TRUE), y = rho.circular(zt_test_arousal, na.rm = TRUE), lty = 1, lwd = 3, shrink = 1.25)
  arrows.circular(mean.circular(zt_test_arousal, na.rm = TRUE) + sd.circular(zt_test_arousal, na.rm = TRUE) * 2, y = rho.circular(zt_test_arousal, na.rm = TRUE), lty = 5, lwd = 1, shrink = 1.25, col = alpha("black", 0.4), pch = 16)
  arrows.circular(mean.circular(zt_test_arousal, na.rm = TRUE) - sd.circular(zt_test_arousal, na.rm = TRUE) * 2, y = rho.circular(zt_test_arousal, na.rm = TRUE), lty = 5, lwd = 1, shrink = 1.25, col = alpha("black", 0.4), pch = 16)
}

#svg("./graphix/zt_test_arousal.svg", width = 10, height = 8)
#pl_zt_test_arousal ()
#dev.off()

pl_zt_test_nentry<- function() {
  plot(zt_test_nentry , stack = T, sep=0.025, shrink=1.1, cex = 2.1, col = 'gray30')
  arrows.circular(mean.circular(zt_test_nentry , na.rm = T),y=rho.circular(zt_test_nentry , na.rm = T), lty=1, lwd=3, shrink = 1.25)
  arrows.circular(mean.circular(zt_test_nentry, na.rm = T)+sd.circular(zt_test_nentry , na.rm = T)*2, y=rho.circular(zt_test_nentry, na.rm = T), lty=5, lwd=1, shrink =1.25, col = alpha("black", 0.4), pch=16)
  arrows.circular(mean.circular(zt_test_nentry, na.rm = T)-sd.circular(zt_test_nentry, na.rm = T)*2, y=rho.circular(zt_test_nentry, na.rm = T),lty=5, lwd=1, shrink = 1.25, col = alpha("black", 0.4), pch=16)
}

#svg("./graphix/zt_test_nentry.svg", width = 10, height = 8)
#pl_zt_test_nentry()
#dev.off()


median.circular(zt_test_arousal, na.rm = T)
mean.circular(zt_test_arousal, na.rm = T)
sd.circular(zt_test_arousal, na.rm = T)
median.circular(zt_test_nentry , na.rm = T)
mean.circular(zt_test_nentry , na.rm = T)
sd.circular(zt_test_nentry , na.rm = T)


rayleigh.test(zt_test_arousal)
rayleigh.test(zt_test_nentry )


kuiper.test(zt_test_arousal, alpha = 0.01)
kuiper.test(zt_test_nentry, alpha = 0.01)

###############################################################################
#Using ggplot and exprimenting with ID checker

e<- ggplot(Arousals, aes(x=nentry, fill=ID)) + geom_histogram(bins = 96,colour="black",fill="steelblue") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,-.5,-.5,1.2), "cm"))+
  ggtitle("Entry - all animals") +
  ggeasy::easy_center_title()

a<-ggplot(Arousals, aes(x=nexit)) + geom_histogram(bins = 96,colour="black" ,fill="firebrick") +
  scale_y_continuous(position = "right")+
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),plot.margin=unit(c(0.5,1.2,-.5,-.5), "cm"))+
  ggtitle("Arousal - all animals") +
  ggeasy::easy_center_title()
e
a

e1<- ggplot(subset(Arousals, ID %in% c("H216", "H254", "H230", "H236") ), aes(x=nentry, fill= ID)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left" ,plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm"))+
  ggtitle("Entry") +
  ggeasy::easy_center_title()

a1 <- ggplot(subset(Arousals, ID %in% c("H216", "H254", "H230", "H236") ), aes(x=nexit, fill= ID)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(),plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm") )+
  ggtitle("Arousal" ) +
  ggeasy::easy_center_title()

e2 <- ggplot(subset(Arousals, ID %in% c("H241", "H243", "H248", "H250") ), aes(x=nentry, fill= ID)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="mako") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left" ,plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm") )+
  ggtitle("")

a2 <- ggplot(subset(Arousals, ID %in% c("H241", "H243", "H248", "H250") ), aes(x=nexit, fill= ID)) + geom_bar(colour="black",width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="mako") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank() ,plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm"))+
  ggtitle("")
a2


e3 <- ggplot(subset(Arousals, ID %in% c("H263", "H273", "H275", "H276", "H307") ), aes(x=nentry, fill= ID)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="D") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("")

a3 <- ggplot(subset(Arousals, ID %in% c("H263", "H273", "H275", "H276", "H307") ), aes(x=nexit, fill= ID)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="D") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank() ,plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm"))+
  ggtitle("")

e4 <- ggplot(subset(Arousals, ID %in% c("H278", "H298", "H300", "H301", "H319") ), aes(x=nentry, fill= ID)) + geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "left",plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm")  )+
  ggtitle("")

a4 <- ggplot(subset(Arousals, ID %in% c("H298", "H300", "H301", "H319","H278") ),
             aes(x=nexit, fill= ID)) +
  geom_bar(colour="black", width=0.35, position = "dodge") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option="A") +
  theme_minimal() + 
  coord_polar(start = 0)+
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))+
  theme(text = element_text(size=20),  axis.title = element_blank(), axis.text.y = element_blank(),plot.margin=unit(c(-.5,-.5,-.5,-.5), "cm") )+
  ggtitle("")


gridExtra::grid.arrange(e,a,e1,a1,e2,a2,e3,a3,e4,a4, ncol=2 )
#Arousals seems not depend on ID


#calcs for bout lengths
Arousals$bout_nr <- as.factor(Arousals$bout_nr) #for discrete x axis and category
Arousals%>%filter(bout_nr==6)%>%summarize(mean= mean(bout_d), sd= sd(bout_d))


#bout duration agains ID and bout nr
Bout_d <- ggplot(Arousals)+
  geom_boxplot(aes(x=ID, y=bout_d, na.rm=T), alpha=0.1 , color ="gray", fill="darkgray")+
  geom_point(aes(x=ID, y=bout_d, colour= bout_nr), size=5, na.rm=T,position = position_dodge2(width = 0.25))+
  scale_colour_viridis(discrete = T, alpha=0.99, option = "C", direction=-1) +
  theme_bw()+ ggtitle("Bout duration")
(Bout_d)

#IBE dur again ID and nr
IBE_d <- ggplot(Arousals)+
  geom_boxplot(aes(x=ID, y=IBE_d/24, na.rm=T), alpha=0.1 , color ="gray", fill="darkgray")+
  geom_point(aes(x=ID, y=IBE_d, colour= bout_nr), size=5, na.rm=T)+
  scale_colour_viridis(discrete = T, alpha=0.99, option = "C", direction=-1) +
  theme_bw() +ggtitle("IBE duration")

IBE_d

Arousals$bout_nr <- as.character(Arousals$bout_nr) #for cat x x axis
Arousals$bout_nr

bout_dur <- ggplot(Arousals, aes(x=bout_nr, y=bout_d), na.rm = TRUE) +
  geom_boxplot(alpha=0.5, na.rm = TRUE, fill='gray85', outlier.shape = NA) +
  geom_point(aes(color=ID), size=1, na.rm = TRUE, position = position_dodge2(width = 0.25)) +
  scale_color_viridis(discrete = TRUE, alpha=1, option = "H", direction=-1) +
  theme_classic() +
  scale_y_continuous(n.breaks = 20, name = "MDT bout duration (hrs)") +
  scale_x_discrete(name = "MDT Bout number", limits = c(1:8))


(bout_dur)

#summary stats:
summary(Arousals)

Arousals%>%
  group_by(bout_nr)%>%
  summarize(mean=mean(IBE_d, na.rm=T), sd= sd(IBE_d, na.rm=T))


ibe_dur <- ggplot(Arousals, aes(x=bout_nr, y= IBE_d), na.rm = T )+
  geom_boxplot( alpha=0.5, na.rm = T, fill='gray85',  outlier.shape = NA)+
  geom_point( aes(color=ID), size=1, na.rm = T, position = position_dodge2(width = 0.25))+
  scale_color_viridis(discrete = T, alpha=1, option = "H", direction=-1) +
  theme_classic()+ 
  scale_y_continuous(n.breaks = 20, name = "Interbout euthermia duration (hrs)")+
  scale_x_discrete(name = "MDT Bout number", limits = c(1:8)) +
  theme(axis.line.x = element_blank(),  
        axis.title.x = element_blank()) 

gridExtra::grid.arrange(bout_dur, ibe_dur)

#save plot (uncomment)
#svg(filename = "./graphix/MDT_IBE_dur.svg", width = 8, height = 6)
#gridExtra::grid.arrange(ibe_dur,bout_dur)
#dev.off()



##############################################################################



