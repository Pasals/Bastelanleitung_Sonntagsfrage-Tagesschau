#### Stichwort Bundestagswahl ####
#### 22. Sept 21 ####

### Felder mit XXX müssen selbst ausgefüllt werden

rm(list = ls())
library(magick)
library(ggthemes)
library(ggplot2)
library(jpeg)
library(grid)
library(tidyverse)

## Daten runterladen
setwd("XXX") # Anpassen! Der Ordner in dem alle benötigten Daten liegen.

dt <- read.csv2("XXX") #Datensatz einfügen
dt[dt < -10] <- NA #ggfs. anpassen

## Daten vorbereiten
dt$Partei<-as.factor(dt$XXX) #Variablenname für Sonntagsfrage einfügen
dtbppartei<-dt
dtbppartei$Partei<-dplyr::na_if(dtbppartei$Partei, NA)
dtbppartei<-subset(dtbppartei, !is.na(Partei))


## Bilder runterladen
#Source: https://htmlcolorcodes.com/ 
#Fabcodes wurden über den Firefox Color-Picker identifiziert
imgage1 <- readJPEG("IMG_6357.jpg") 
imgage2 <- readJPEG("IMG_6352.jpg") 
imgage3 <- readJPEG("IMG_6358.jpg")
imgage4 <- readJPEG("IMG_6360.jpg")
imgage5 <- readJPEG("IMG_6359.jpg")

## Count in Prozentzahlen umwandeln
plotdata <- dtbppartei %>%
  count(Partei) %>%
  mutate(pct = n / sum(n)*100,
         pctlabel = paste0(round(pct))) #"%")) #,it oder ohne Prozentzahl

## Ordnen der Parteien
x.ordered <- factor(plotdata$Partei, levels=c("1", "2", "6", "4", "5", "3", "7"))

a<-ggplot(plotdata, aes(x = x.ordered, 
                        y = pct,
                        fill = Partei)) + 
  annotation_custom(rasterGrob(imgage1, width=unit(1,"npc"),  height=unit(1,"npc")), -Inf, Inf, 1, 35) +
  annotation_custom(rasterGrob(imgage2, width=unit(1,"npc"),  height=unit(1,"npc")), -2, 10, -3, 1) +
  geom_bar(stat = "identity", colour="white", width = 0.8) +
  theme_economist(base_size = 10) +
  annotation_custom(rasterGrob(imgage1, width=unit(1,"npc"),  height=unit(1,"npc")), -Inf, Inf, 1, 2) +
  annotation_custom(rasterGrob(imgage2, width=unit(1,"npc"),  height=unit(1,"npc")), -2, 10, 35.5, 39.0) +
  annotation_custom(rasterGrob(imgage3, width=unit(1,"npc"),  height=unit(1,"npc")), -2, 10, 39.0, 42.5) +
  annotation_custom(rasterGrob(imgage4, width=unit(1,"npc"),  height=unit(1,"npc")), -2, 10, 33.0, 35.5) +
  annotation_custom(rasterGrob(imgage5, width=unit(1,"npc"),  height=unit(1,"npc")), -2, 10, -5.2, -3) + 
  ylim(-5, 42) +
  labs(x="", 
       y="",
       title="EPKO-Studie - Herbst 2020",
       subtitle="Sonntagsfrage zur Bundestagswahl") +
  geom_text(aes(label = pctlabel), fontface="bold",
            y = -4, size = 5, colour="black", vjust = 'center') +
  theme(axis.title = element_text(), axis.title.x=element_blank(), 
        axis.text.x = element_text(margin = margin(-60,0,20,0), size = 14, face="bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 16),
        plot.background = element_rect(fill = "#b3d5f1"),
        legend.position="none",
        plot.subtitle=element_text(family = "sans", size=18, hjust=0.03, color="black",face="bold", vjust = -3.5, margin=margin(0,0,-55,0)),
        plot.title = element_text(family = "sans", size = 18, hjust=0.03, colour="white", vjust = 0, face="bold", margin=margin(0,0,0,0)))  +
  xlab("class") +
  theme(legend.position="none") +
  annotate(geom="text", x=5, y=22, label="EPKO-Studie",color="red", fontface='bold', size=16, alpha = 0.2) +
  scale_x_discrete(breaks=1:7, labels=c("CDU","SPD","Grüne","FDP", "Linke","AfD","Andere")) +
  scale_fill_manual(values=c("#32302e", "#E3000F", "#64A12D",  "#ffed00", "#B1003A",  "#009ee0","grey")) #Source: Jeweilige Web-Präsenz der Parteien
a
ggsave("BTW.emf", a, dpi = 300, units = "in", device='png') #Speichert Plot als emf Datei
