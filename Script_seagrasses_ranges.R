##################### Seagrass distribution ranges##################### 
library(tidyverse)
library(ggridges)
library(dplyr)
library(mgcv)
library(ggpubr)
library(maptools)
library(ggiraphExtra)
library(ggiraph)
library(plyr)
library(cowplot)

# Marine ecoregions of the world; "meow_ecos.shp"
library(maptools)
meow=rgdal::readOGR("./meow_ecos.shp")
plot(meow, col=sf.colors(232, categorical = TRUE), border="gray49")
plot(world_borders, col="Light Gray", border="gray49", add=TRUE)
plot(world_borders,col="Light Gray")
rgdal::readOGR
world_borders=rgdal::readOGR("./world_borders.shp")
world_borders2=readShapePoly("./world_borders.shp")

ggplot()+ geom_polygon(world_borders, aes(x,y,), col="Light Gray") +
  geom_polygon(meow, aes(x,y,))

# Load seagrass distribution data 
Seagrass_MeanLat=read.table("./data/data.txt", sep="\t", header=TRUE) %>% glimpse()
Seagrass_MeanLat$Family<-as.factor(Seagrass_MeanLat$Family)
view(Seagrass_MeanLat)

# Relationship between lat and long.range
ggplot(Seagrass_MeanLat, aes(Dif.Long, Dif.Lat)) +
  theme_classic() +
  labs(x="Longitudinal range", y="Latitudinal range") +
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16)) +
  geom_point(aes(size = Area), alpha = 1) + 
  scale_size(range = c(0, 8)) + theme(legend.position = "none") 
  ggsave("Fig.1.tiff")
  
# Histograms of size range metrics
p1 = Seagrass_MeanLat %>% gghistogram(x= "Lat.average",add = "mean", fill = "lightgray", rug = TRUE, size = 2) +
    xlab("Latitudinal midpoint") + ylab(" ") +
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16)) 

p2 = Seagrass_MeanLat %>% gghistogram(x= "Dif.Lat",add = "mean", fill = "lightgray", rug = TRUE, size = 2) +
  xlab("Latitudinal range") + ylab(" ") +
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16))  

p3 = Seagrass_MeanLat %>% gghistogram(x= "Dif.Long",add = "mean", fill = "lightgray", rug = TRUE, size = 2) +
  xlab("Longitudinal range") + ylab(" ") +
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16))  

p4 = Seagrass_MeanLat %>% gghistogram(x= "Area", add = "mean", fill = "lightgray", rug = TRUE, size = 2) +
  labs(x= y.expression, y = " ") +
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16))

p5 = Seagrass_MeanLat %>% gghistogram(x= "N.ecoregions",add = "mean", fill = "lightgray", rug = TRUE, size = 2) +
  xlab("Number of ecoregions") + ylab(" ") +
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16))  

plot_grid(p1, p2, p3, p4, p5, labels = c("A", "B", "C", "D", "E"))
ggsave("Fig.1.tiff")

# Lat.extent GAM 
GAMSeagrass_MeanLat = gam(Dif.Lat~s(Lat.average), data=Seagrass_MeanLat, family=poisson())
GAMSeagrass_MeanLat
plot(GAMSeagrass_MeanLat)
plot(GAMSeagrass_MeanLat, residuals = TRUE, pages = 1, all.terms = TRUE)
plot(ggeffects::ggpredict(GAMSeagrass_MeanLat), facets = TRUE) # facet by group,
plot(GAMSeagrass_MeanLat, residuals = TRUE,pch = 1, cex = 0.8, shade = TRUE, shade.col = "lightblue") # enseña los residuos. y standard errors on your plots. These show the 95% confidence interval for the mean shape of the effect.
par(mfrow=c(2,2))
gam.check(GAMSeagrass_MeanLat)

plot_gam_Dif.Lat = ggplot(Seagrass_MeanLat,aes(x = Lat.average, y = Dif.Lat))+
  theme_classic() +
  labs(y= "Latitudinal range", x = "Mean latitude") +
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16)) +
  geom_point() + geom_smooth(method="gam", size = 2)

# Long.extent GAM 
GAMSeagrass_Long = gam(Dif.Long~s(Lat.average), data=Seagrass_MeanLat, family=poisson())
summary(GAMSeagrass_Long)
par(mfrow=c(2,2))
gam.check(GAMSeagrass_Long)

plot_gam_Dif.Long = ggplot(Seagrass_MeanLat,aes(x = Lat.average, y = Dif.Long))+
  theme_classic() +
  labs(y= "Longitudinal range", x = "Mean latitude") +
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16)) +
  geom_point() + geom_smooth(method="gam", size = 2)

# Area GAM
GAMSeagrass_Area = gam(Area~s(Lat.average), data=Seagrass_MeanLat, family=poisson())
summary(GAMSeagrass_Area)
par(mfrow=c(2,2))
gam.check(GAMSeagrass_Area)

y.expression=expression(Area ~ (km^2 * 10^5))
plot_gam_Area = ggplot(Seagrass_MeanLat,aes(x = Lat.average, y = Area))+
  theme_classic() +
  labs(y= y.expression, x = "Mean latitude") +  
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16)) +
  geom_point() + geom_smooth(method="gam", size = 2)

# Nº ecoregions GAM 
GAMSeagrass_N.EcoReg = gam(N.ecoregions~s(Lat.average), data=Seagrass_MeanLat,family=poisson())
summary(GAMSeagrass_N.EcoReg)
par(mfrow=c(2,2))
gam.check(GAMSeagrass_N.EcoReg)
plot_gam_N.EcoReg = ggplot(Seagrass_MeanLat, aes(x = Lat.average, y = N.ecoregions))+
  theme_classic() +
  labs(y= "Number of ecoregions", x = "Mean latitude") + 
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 16)) +
  geom_point() + geom_smooth(method="gam", size =2)

plot_grid(plot_gam_Dif.Lat, plot_gam_Dif.Long, plot_gam_Area, plot_gam_N.EcoReg, labels = "AUTO")
ggsave("Fig.2.tiff")

