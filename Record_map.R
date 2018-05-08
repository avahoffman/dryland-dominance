###########################################################################################
##
## R source code to accompany Hoffman et al. (2018), last updated 21 November 2017.
## Please contact Ava Hoffman (avamariehoffman@gmail.com) with questions.
##
## If you found this code useful, please use the citation below:
## 
##
###########################################################################################
## NOTE: If you get a 'cannot establish the connection' type error be aware that the raster 
## file information from Koppen Geiger needs to be in this directory!! These are the .gri
## and .grd files
setwd("/Users/avahoffman/Dropbox/Research/Dryland_Dominance/World_map")

###########################################################################################
## load libraries
library(ggplot2)  # FYI you need v2.0
library(ggthemes) # theme_map and tableau colors
library(reshape2) # to melt data matrix
library(raster)
library(maps)

###########################################################################################
datapoints <- read.csv("Drylands_dominance.csv",header=T)
head(datapoints)
period='1986-2010'
r <- raster(paste('KG_', period, '.grd', sep=''))
climate.colors=c("#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64")
datapoints$size <- datapoints$WOS_record
r.matrix <- as.matrix(r) ## give KG biome for each coordinate
KG.colors <- melt(r.matrix)
KG.colors$Var2 <- (KG.colors$Var2 / 12) - 180 ##rescale to coordinates
KG.colors$Var1 <- ((KG.colors$Var1 / 12) - 90) *-1 ##rescale to coordinates
KG.colors._8 <- KG.colors[(KG.colors$value <= 8),]
KG.colors.5_8 <- KG.colors._8[(KG.colors._8$value >= 5),]
world <- map_data("world")
world <- world[world$region != "Antarctica",]

gg <- ggplot() +
  geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region), fill='black', alpha=0.1) + #add size command to make country lines visible
  theme_map() +
  theme(strip.background=element_blank()) +
  scale_size_area(max_size = 10,breaks=c(1,10,100,500,1000,5000)) +
  guides(size=guide_legend(title="# Studies/Records")) +
  guides(color=element_blank()) +
  theme(legend.position="right") +
  geom_tile(data=KG.colors.5_8, aes(x=Var2, y=Var1, fill=as.factor(value))) +
  scale_fill_manual(values=climate.colors,labels=c("Hot semi-arid","Cool semi-arid","Hot arid","Cool arid")) +
  geom_point(data=datapoints,
            aes(x=LONGITUDE, y=LATITUDE, size=size),
           color="black",alpha=0.7) +
  #geom_point(aes(x=-105,y=40),size=1,color="black")+ ## Fort Collins, CO, USA reference point
  guides(fill=guide_legend(title="KG Biome"))

gg
ggsave(file="Record_map.pdf",height=4,width=8)

