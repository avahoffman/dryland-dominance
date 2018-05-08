###########################################################################################
##
## R source code to accompany Hoffman et al. (2018), last updated 8 Mar 2018.
## Please contact Ava Hoffman (avamariehoffman@gmail.com) with questions.
##
## If you found this code useful, please use the citation below:
## 
##
###########################################################################################
setwd("/Users/avahoffman/Dropbox/Research/Dryland_Dominance/Dry_v_mesic/")

###########################################################################################
## load libraries
library(ggplot2) 
library(ggthemes)
library(reshape2) 
library(dplyr)

###########################################################################################
datapoints <- read.csv("Dry_v_mesic.csv",header=T)
datapoints <- datapoints[!(is.na(datapoints$WOS_record)),] ## get rid of species that were not dominant, etc .
datapoints <- datapoints[(datapoints$WOS_record < 10000),]
means <- aggregate(datapoints$WOS_record,by=list(datapoints$dryland),FUN = mean)
medians <- aggregate(datapoints$WOS_record,by=list(datapoints$dryland),FUN = median)
sums <- aggregate(datapoints$WOS_record,by=list(datapoints$dryland),FUN = sum)


gg <- ggplot(data=datapoints, aes(x=log(WOS_record),color=dryland,fill=dryland)) +
  geom_density() +
  theme_classic() +
  geom_vline(xintercept=log(means[1,2]),color="pink",lty=1) +
  geom_vline(xintercept=log(means[2,2]),color="lightblue",lty=1) +
  xlab("# Studies/Records")+
  ylab("Density") +
  xlim(c(0,10)) +
  scale_color_manual(values=c("pink","lightblue"),labels=c("Dryland","Non-dryland"), guide=F) +
  scale_fill_manual(values=alpha(c("pink","lightblue"), 0.7),labels=c("Dryland","Non-dryland")) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="bottom")

gg
ggsave(file="Dry_v_mesic.pdf",height=2,width=3)

## summarize
tab.print <- cbind(means,medians[,2],sums[,2])
colnames(tab.print) <- c("category","mean","median","total publication records")
write.csv(tab.print, file="Dry_v_mesic_summary.csv")

