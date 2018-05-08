###########################################################################################
##
## R source code to accompany Hoffman et al. (2018), last updated 10 Mar 2018.
## Please contact Ava Hoffman (avamariehoffman@gmail.com) with questions.
##
## If you found this code useful, please use the citation below:
## 
##
###########################################################################################
## Set your working directory
wd <- "/Users/avahoffman/Dropbox/Research/Dryland_Dominance/Traits/"

###########################################################################################
## load libraries
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(reshape2)
library(plyr)
library(sunburstR)
library(ggalluvial)

###########################################################################################
setwd(wd)
df.raw <- read.csv("Traits_studies.csv",header=T)
## ecosystem implied is counted as a species only study for our purposes
df.raw$Type[df.raw$Type == "ecosystem implied"] <- "species"
## convert traits into long form, have already been comma separated
## first, remove any marked 'no' .. includes some studies that don't qualify
df.raw <- df.raw[(df.raw$Used == "yes"),]
df.melting <- df.raw[,c(1,2,7,13:ncol(df.raw))]
## print counts of studies (publications) in all categories
count(df.melting, c("Species","Driver","Type"))
count(df.melting, c("Species"))
count(df.melting, c("Type"))
count(df.melting, c("Driver"))
## convert to long format by trait measurement
df.melted <- melt(df.melting, id=c("Species","Driver","Type"))
df.melted <- df.melted[,-4] ## not a real descriptive column
df.melted <- df.melted[!(df.melted$value == ""),] ## blank traits
## print counts of Traits in all categories
count(df.melted, c("Species","Driver","Type"))
count(df.melted, c("Species"))
count(df.melted, c("Type"))
count(df.melted, c("Driver"))
counts.all <- df.melted[!(df.melted$value == ""),] ## blank traits
counts.all <- count(df.melted, c("value"))
counts.all <- counts.all[!(counts.all$value == ""),] ## blank traits
head(counts.all,20)
## write counts for all traits
write.csv(counts.all, file="traits_by_count.csv")
## summarize
by.pub <- count(df.melting, c("Species","Driver","Type"))
by.trait <- count(df.melted, c("Species","Driver","Type"))
tab.print <- cbind(by.pub,by.trait[,4])
colnames(tab.print) <- c("Species","Driver","Type","Number of publications","Number of traits")
write.csv(tab.print, file="Traits_summary.csv")

###########################################################################################
###########################################################################################
## Figures
## word map of traits where size is related to frequency

## is the trait a direct one?
df.melted.direct <- df.melted[(df.melted$Type == "ecosystem + species"),]
counts.all.direct <- count(df.melted.direct, c("value"))
col.id <- counts.all$value %in% counts.all.direct$value
counts.wcolor <- cbind(counts.all,col.id)

b <- ggplot(counts.wcolor, aes(x = 1, y = 1, size = freq, label = value, color=col.id)) +
  geom_text_repel(segment.size = 0, force=2, segment.alpha = 0) +
  scale_size(range = c(1, 30)) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  labs(x = '', y = '') +
  theme_classic()

b + scale_color_manual(values=c("black","red")) + theme(legend.position = "none")
ggsave("Trait_cloud_col.pdf",width=13,height=7.5)


###########################################################################################
## facet chart to show difference in traits, interactive 

counts.chart <- count(df.melted, c("Species","Driver","Type","value"))
counts.chart <- counts.chart[!(counts.chart$value == ""),] # blanks
counts.chart$path <- paste(counts.chart$Species, counts.chart$Driver, counts.chart$Type, counts.chart$value, sep='-') 
sunburst.data <- counts.chart[,c(6,5)]
sunburst.data$V1 <- counts.chart$path ; sunburst.data$V2 <- counts.chart$freq
sunburst.data <- sunburst.data[,3:4]

sunburst(sunburst.data, colors = c("#1b9e77","#d95f02",
  "#7570b3","#e7298a", "#66a61e","#e6ab02","#a6761d","#666666")) ## dark palette

###########################################################################################
## other way to show difference in traits
## alluvial chart
counts.chart <- count(df.melted, c("Species","Driver","Type","value"))
counts.chart <- counts.chart[!(counts.chart$value == ""),] ## blanks
counts.chart.simp <- within(counts.chart, value[freq < 2] <- 'other') ## lump studies with less than x number of studies into "other"
head(counts.chart.simp,100)

## below just for visual organizing because alluvium is alphabetical by default..

## No labels, will add with powerpoint because not possible to just label some strata
g <- ggplot(as.data.frame(counts.chart.simp),
           aes(weight = freq, axis1 = Species, axis2 = Driver, axis3 = value)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  geom_alluvium(aes(fill = Type), width = 1/12) +
  scale_fill_brewer(palette="Dark2") +
  geom_stratum(width = 1/20, fill = "white", color = "grey", decreasing=F) +
  scale_x_continuous(breaks = 1:3, labels = c("Driver", "Type", "Trait")) 
g
ggsave("Trait_alluvial.pdf",width=8,height=4)

## plot large so that strata can be identified
g <- ggplot(as.data.frame(counts.chart.simp),
            aes(weight = freq, axis1 = Species, axis2 = Driver, axis3 = value)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  geom_alluvium(aes(fill = Type), width = 1/12) +
  scale_fill_brewer(palette="Dark2") +
  geom_stratum(width = 1/20, fill = "white", color = "grey", decreasing=F) +
  geom_text(stat = "stratum", label.strata = TRUE, decreasing=F) +
  scale_x_continuous(breaks = 1:3, labels = c("Driver", "Type", "Trait")) 
g
ggsave("Trait_alluvial_big.pdf",width=400,height=200,limitsize = F) ## so you can see labels
