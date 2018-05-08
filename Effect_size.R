###########################################################################################
##
## R source code to accompany Hoffman et al. (2018), last updated 8 Mar 2018.
## Please contact Ava Hoffman (avamariehoffman@gmail.com) with questions.
##
## If you found this code useful, please use the citation below:
## 
##
###########################################################################################
## Set your working directory
wd <- "/Users/avahoffman/Dropbox/Research/Dryland_Dominance/Effect_size"

###########################################################################################
## load libraries
library(ggplot2)
library(car)
library(viridis)
library(RColorBrewer)

###########################################################################################
setwd(wd)
df.raw <- read.csv("Effect_size.csv",header=T)
## make any necessary data changes
## remove any missing data by removing NAs from the regression column
df.raw <- df.raw[!(is.na(df.raw$Note.regression)),]
## summarize
sum <- df.raw[!(is.na(df.raw$ES_ecosystem)),]
sum <- count(sum, c("Species","Note.regression"))
colnames(sum) <- c("Species","Driver","Number of experiments")
write.csv(sum, file="Effectsize_summary.csv")
## log modulus transform (modulus retains negative values)
df.raw[,13:14] <- sign(df.raw[,13:14]) * log(abs(df.raw[,13:14]) + 1)

## print overall linear models
lm.nitrogen.agb <- df.raw[(df.raw$Note.regression == "Nitrogen - AGB"),]
  summary(glm(ES_ecosystem~ES_SPP,data=lm.nitrogen.agb))
lm.nitrogen.cover <- df.raw[(df.raw$Note.regression == "Nitrogen - Cover"),]
  summary(lm(ES_ecosystem~ES_SPP,data=lm.nitrogen.cover))
lm.drought <- df.raw[(df.raw$Note.regression == "Drought"),]
  summary(lm(ES_ecosystem~ES_SPP,data=lm.drought))
lm.warming <- df.raw[(df.raw$Note.regression == "Warming"),]
  summary(lm(ES_ecosystem~ES_SPP,data=lm.warming))
  
###########################################################################################
###########################################################################################
###########################################################################################
## Figures
  
df.whole <- df.raw

## subdivide by driver
df.whole.drought <- df.whole[(df.whole$Note.regression == "Drought"),]
df.whole.warming <- df.whole[(df.whole$Note.regression == "Warming"),]
df.whole.nitrogen.anpp <- df.whole[(df.whole$Note.regression == "Nitrogen - AGB"),]
df.whole.nitrogen.cover <- df.whole[(df.whole$Note.regression == "Nitrogen - Cover"),]

## Remove outliers based on Bonferroni p-val in test below
get.rid.of.outliers <- function(d = df.whole.drought) {
  outlier.list <- (outlierTest(lm(ES_ecosystem ~ ES_SPP, data = d), cutoff = Inf, n.max = Inf))$bonf.p
  outlier.line.ids <- as.numeric(names(outlier.list[outlier.list < 0.05]))
  print(c("outlier(s): ", outlier.line.ids))
  d <- d[!rownames(d) %in% outlier.line.ids ,]
  return(d)
}
df.whole.drought <- get.rid.of.outliers(df.whole.drought)
df.whole.nitrogen.anpp <- get.rid.of.outliers(df.whole.nitrogen.anpp)
df.whole.nitrogen.cover <- get.rid.of.outliers(df.whole.nitrogen.cover)
df.whole.warming <- get.rid.of.outliers(df.whole.warming)

##pull r2
df.whole.drought.lm <- summary(lm(data=df.whole.drought,ES_ecosystem~ES_SPP))
df.whole.warming.lm <- summary(lm(data=df.whole.warming,ES_ecosystem~ES_SPP))
df.whole.nitrogen.anpp.lm <- summary(lm(data=df.whole.nitrogen.anpp,ES_ecosystem~ES_SPP))
df.whole.nitrogen.cover.lm <- summary(lm(data=df.whole.nitrogen.cover,ES_ecosystem~ES_SPP))
df.whole.drought.lm.r2 <- format(df.whole.drought.lm$r.squared,digits=2)
df.whole.warming.lm.r2 <- format(df.whole.warming.lm$r.squared,digits=2)
df.whole.nitrogen.anpp.lm.r2 <- format(df.whole.nitrogen.anpp.lm$r.squared,digits=2)
df.whole.nitrogen.cover.lm.r2 <- format(df.whole.nitrogen.cover.lm$r.squared,digits=2)

df.final <- rbind(df.whole.drought,df.whole.nitrogen.anpp,df.whole.nitrogen.cover,df.whole.warming)

## All experiments combined on a single plot
g <-
  ggplot(df.raw, aes(x=ES_SPP, y=ES_ecosystem, color=Species))+
  geom_vline(xintercept=0,color="gray",lty=2) +
  geom_hline(yintercept=0,color="gray",lty=2) +
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette="Dark2", name="Species") +
  theme(legend.position = "right") +
  xlab("Dominant species simple effect size")+
  ylab("Ecosystem simple effect size") +
  ylim(c(-6.3,6.3)) +
  xlim(c(-6.3,6.3))
ggsave("effectsize_alldrivers.pdf",width=5,height=3)
g

## All experiments combined, faceted by driver, colored by SPECIES
p <-
  ggplot(df.final, aes(x=ES_SPP, y=ES_ecosystem, color=Species))+
  geom_vline(xintercept=0,color="gray",lty=2) +
  geom_hline(yintercept=0,color="gray",lty=2) +
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette="Dark2", name="Species") +
  facet_grid(.~Note.regression, scales='fixed') +
  theme(legend.position = "bottom") +
  stat_smooth(data=df.whole.drought,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.nitrogen.anpp,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.nitrogen.cover,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.warming,method=lm,se=F,show.legend = F,color="black") +
  geom_text(data=df.whole.drought,x=2,y=-4,label=paste0("r^2==", df.whole.drought.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.nitrogen.anpp,x=2,y=-4,label=paste0("r^2==", df.whole.nitrogen.anpp.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.nitrogen.cover,x=2,y=-4,label=paste0("r^2==", df.whole.nitrogen.cover.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.warming,x=2,y=-4,label=paste0("r^2==", df.whole.warming.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  xlab("Dominant species simple effect size")+
  ylab("Ecosystem simple effect size") +
  ylim(c(-6.3,6.3)) +
  xlim(c(-6.3,6.3))
ggsave("effectsize_bydriver.pdf",width=7,height=3)
p

## All experiments combined, faceted by driver, colored by SPECIES trait
p <-
  ggplot(df.final, aes(x=ES_SPP, y=ES_ecosystem, color=units_spp))+
  geom_vline(xintercept=0,color="gray",lty=2) +
  geom_hline(yintercept=0,color="gray",lty=2) +
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette="Dark2", name="Species") +
  facet_grid(.~Note.regression, scales='fixed') +
  theme(legend.position = "bottom") +
  stat_smooth(data=df.whole.drought,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.nitrogen.anpp,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.nitrogen.cover,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.warming,method=lm,se=F,show.legend = F,color="black") +
  geom_text(data=df.whole.drought,x=2,y=-4,label=paste0("r^2==", df.whole.drought.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.nitrogen.anpp,x=2,y=-4,label=paste0("r^2==", df.whole.nitrogen.anpp.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.nitrogen.cover,x=2,y=-4,label=paste0("r^2==", df.whole.nitrogen.cover.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.warming,x=2,y=-4,label=paste0("r^2==", df.whole.warming.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  xlab("Dominant species simple effect size")+
  ylab("Ecosystem simple effect size") +
  ylim(c(-6.3,6.3)) +
  xlim(c(-6.3,6.3))
ggsave("effectsize_bydriver_bytrait.pdf",width=7,height=3)
p

## All experiments combined, faceted by driver, colored by ECOSYSTEM response variable
p <-
  ggplot(df.final, aes(x=ES_SPP, y=ES_ecosystem, color=units_ecosystem))+
  geom_vline(xintercept=0,color="gray",lty=2) +
  geom_hline(yintercept=0,color="gray",lty=2) +
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette="Dark2", name="Ecosystem") +
  facet_grid(.~Note.regression, scales='fixed') +
  theme(legend.position = "bottom") +
  stat_smooth(data=df.whole.drought,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.nitrogen.anpp,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.nitrogen.cover,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.whole.warming,method=lm,se=F,show.legend = F,color="black") +
  geom_text(data=df.whole.drought,x=2,y=-4,label=paste0("r^2==", df.whole.drought.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.nitrogen.anpp,x=2,y=-4,label=paste0("r^2==", df.whole.nitrogen.anpp.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.nitrogen.cover,x=2,y=-4,label=paste0("r^2==", df.whole.nitrogen.cover.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  geom_text(data=df.whole.warming,x=2,y=-4,label=paste0("r^2==", df.whole.warming.lm.r2), parse = TRUE,show.legend = FALSE,color="darkgray") +
  xlab("Dominant species simple effect size")+
  ylab("Ecosystem simple effect size") +
  ylim(c(-6.3,6.3)) +
  xlim(c(-6.3,6.3))
ggsave("effectsize_bydriver_byresponsevar.pdf",width=7,height=3)
p

###########################################################################################
## All experiments faceted by driver and species, colored by SPECIES trait

## B. gracilis
df <- df.raw[(df.raw$Species == "Bouteloua gracilis"),]
##subdivide so that regression lines can be drawn on plot
df.drought <- df[(df$Note.regression == "Drought"),]
df.warming <- df[(df$Note.regression == "Warming"),]
df.nitrogen.agb <- df[(df$Note.regression == "Nitrogen - AGB"),]
df.nitrogen.cover <- df[(df$Note.regression == "Nitrogen - Cover"),]
##pull r2
df.drought.lm <- summary(lm(data=df.drought,ES_ecosystem~ES_SPP))
df.warming.lm <- summary(lm(data=df.warming,ES_ecosystem~ES_SPP))
df.nitrogen.agb.lm <- summary(lm(data=df.nitrogen.agb,ES_ecosystem~ES_SPP))
df.nitrogen.cover.lm <- summary(lm(data=df.nitrogen.cover,ES_ecosystem~ES_SPP))
df.drought.lm.r2 <- format(df.drought.lm$r.squared,digits=3)
df.warming.lm.r2 <- format(df.warming.lm$r.squared,digits=3)
df.nitrogen.agb.lm.r2 <- format(df.nitrogen.agb.lm$r.squared,digits=3)
df.nitrogen.cover.lm.r2 <- format(df.nitrogen.cover.lm$r.squared,digits=3)

df.Ac <- df.raw[(df.raw$Species == "Agropyron cristatum"),]
##subdivide so that regression lines can be drawn on plot
df.Ac.drought <- df.Ac[(df.Ac$Note.regression == "Drought"),]
df.Ac.warming <- df.Ac[(df.Ac$Note.regression == "Warming"),]
df.Ac.nitrogen.agb <- df.Ac[(df.Ac$Note.regression == "Nitrogen - AGB"),]
df.Ac.nitrogen.cover <- df.Ac[(df.Ac$Note.regression == "Nitrogen - Cover"),]
##pull r2
df.Ac.drought.lm <- summary(lm(data=df.Ac.drought,ES_ecosystem~ES_SPP))
df.Ac.warming.lm <- summary(lm(data=df.Ac.warming,ES_ecosystem~ES_SPP))
df.Ac.nitrogen.agb.lm <- summary(lm(data=df.Ac.nitrogen.agb,ES_ecosystem~ES_SPP))
df.Ac.nitrogen.cover.lm <- summary(lm(data=df.Ac.nitrogen.cover,ES_ecosystem~ES_SPP))
df.Ac.drought.lm.r2 <- format(df.Ac.drought.lm$r.squared,digits=3)
df.Ac.warming.lm.r2 <- format(df.Ac.warming.lm$r.squared,digits=3)
df.Ac.nitrogen.agb.lm.r2 <- format(df.Ac.nitrogen.agb.lm$r.squared,digits=3)
df.Ac.nitrogen.cover.lm.r2 <- format(df.Ac.nitrogen.cover.lm$r.squared,digits=3)

df.Tt <- df.raw[(df.raw$Species == "Themeda triandra"),]
##subdivide so that regression lines can be drawn on plot
df.Tt.drought <- df.Tt[(df.Tt$Note.regression == "Drought"),]
df.Tt.warming <- df.Tt[(df.Tt$Note.regression == "Warming"),]
df.Tt.nitrogen.agb <- df.Tt[(df.Tt$Note.regression == "Nitrogen - AGB"),]
df.Tt.nitrogen.cover <- df.Tt[(df.Tt$Note.regression == "Nitrogen - Cover"),]
##pull r2
df.Tt.drought.lm <- summary(lm(data=df.Tt.drought,ES_ecosystem~ES_SPP))
df.Tt.warming.lm <- summary(lm(data=df.Tt.warming,ES_ecosystem~ES_SPP))
df.Tt.nitrogen.agb.lm <- summary(lm(data=df.Tt.nitrogen.agb,ES_ecosystem~ES_SPP))
df.Tt.nitrogen.cover.lm <- summary(lm(data=df.Tt.nitrogen.cover,ES_ecosystem~ES_SPP))
df.Tt.drought.lm.r2 <- format(df.Tt.drought.lm$r.squared,digits=3)
df.Tt.warming.lm.r2 <- format(df.Tt.warming.lm$r.squared,digits=3)
df.Tt.nitrogen.agb.lm.r2 <- format(df.Tt.nitrogen.agb.lm$r.squared,digits=3)
df.Tt.nitrogen.cover.lm.r2 <- format(df.Tt.nitrogen.cover.lm$r.squared,digits=3)

df.Ps <- df.raw[(df.raw$Species == "Pascopyrum smithii"),]
##subdivide so that regression lines can be drawn on plot
df.Ps.drought <- df.Ps[(df.Ps$Note.regression == "Drought"),]
df.Ps.warming <- df.Ps[(df.Ps$Note.regression == "Warming"),]
df.Ps.nitrogen.agb <- df.Ps[(df.Ps$Note.regression == "Nitrogen - AGB"),]
df.Ps.nitrogen.cover <- df.Ps[(df.Ps$Note.regression == "Nitrogen - Cover"),]
##pull r2
df.Ps.drought.lm <- summary(lm(data=df.Ps.drought,ES_ecosystem~ES_SPP))
df.Ps.warming.lm <- summary(lm(data=df.Ps.warming,ES_ecosystem~ES_SPP))
df.Ps.nitrogen.agb.lm <- summary(lm(data=df.Ps.nitrogen.agb,ES_ecosystem~ES_SPP))
df.Ps.nitrogen.cover.lm <- summary(lm(data=df.Ps.nitrogen.cover,ES_ecosystem~ES_SPP))
df.Ps.drought.lm.r2 <- format(df.Ps.drought.lm$r.squared,digits=3)
df.Ps.warming.lm.r2 <- format(df.Ps.warming.lm$r.squared,digits=3)
df.Ps.nitrogen.agb.lm.r2 <- format(df.Ps.nitrogen.agb.lm$r.squared,digits=3)
df.Ps.nitrogen.cover.lm.r2 <- format(df.Ps.nitrogen.cover.lm$r.squared,digits=3)

df.Bd <- df.raw[(df.raw$Species == "Bouteloua dactyloides"),]
##subdivide so that regression lines can be drawn on plot
df.Bd.drought <- df.Bd[(df.Bd$Note.regression == "Drought"),]
df.Bd.warming <- df.Bd[(df.Bd$Note.regression == "Warming"),]
df.Bd.nitrogen.agb <- df.Bd[(df.Bd$Note.regression == "Nitrogen - AGB"),]
df.Bd.nitrogen.cover <- df.Bd[(df.Bd$Note.regression == "Nitrogen - Cover"),]
##pull r2
df.Bd.drought.lm <- summary(lm(data=df.Bd.drought,ES_ecosystem~ES_SPP))
df.Bd.warming.lm <- summary(lm(data=df.Bd.warming,ES_ecosystem~ES_SPP))
df.Bd.nitrogen.agb.lm <- summary(lm(data=df.Bd.nitrogen.agb,ES_ecosystem~ES_SPP))
df.Bd.nitrogen.cover.lm <- summary(lm(data=df.Bd.nitrogen.cover,ES_ecosystem~ES_SPP))
df.Bd.drought.lm.r2 <- format(df.Bd.drought.lm$r.squared,digits=3)
df.Bd.warming.lm.r2 <- format(df.Bd.warming.lm$r.squared,digits=3)
df.Bd.nitrogen.agb.lm.r2 <- format(df.Bd.nitrogen.agb.lm$r.squared,digits=3)
df.Bd.nitrogen.cover.lm.r2 <- format(df.Bd.nitrogen.cover.lm$r.squared,digits=3)

df.Hc <- df.raw[(df.raw$Species == "Heteropogon contortus"),]
##subdivide so that regression lines can be drawn on plot
df.Hc.drought <- df.Hc[(df.Hc$Note.regression == "Drought"),]
df.Hc.warming <- df.Hc[(df.Hc$Note.regression == "Warming"),]
df.Hc.nitrogen.agb <- df.Hc[(df.Hc$Note.regression == "Nitrogen - AGB"),]
df.Hc.nitrogen.cover <- df.Hc[(df.Hc$Note.regression == "Nitrogen - Cover"),]
##pull r2
df.Hc.drought.lm <- summary(lm(data=df.Hc.drought,ES_ecosystem~ES_SPP))
df.Hc.warming.lm <- summary(lm(data=df.Hc.warming,ES_ecosystem~ES_SPP))
df.Hc.nitrogen.agb.lm <- summary(lm(data=df.Hc.nitrogen.agb,ES_ecosystem~ES_SPP))
df.Hc.nitrogen.cover.lm <- summary(lm(data=df.Hc.nitrogen.cover,ES_ecosystem~ES_SPP))
df.Hc.drought.lm.r2 <- format(df.Hc.drought.lm$r.squared,digits=3)
df.Hc.warming.lm.r2 <- format(df.Hc.warming.lm$r.squared,digits=3)
df.Hc.nitrogen.agb.lm.r2 <- format(df.Hc.nitrogen.agb.lm$r.squared,digits=3)
df.Hc.nitrogen.cover.lm.r2 <- format(df.Hc.nitrogen.cover.lm$r.squared,digits=3)

df.Bi <- df.raw[(df.raw$Species == "Bromus inermis"),]
##subdivide so that regression lines can be drawn on plot
df.Bi.drought <- df.Bi[(df.Bi$Note.regression == "Drought"),]
df.Bi.warming <- df.Bi[(df.Bi$Note.regression == "Warming"),]
df.Bi.nitrogen.agb <- df.Bi[(df.Bi$Note.regression == "Nitrogen - AGB"),]
df.Bi.nitrogen.cover <- df.Bi[(df.Bi$Note.regression == "Nitrogen - Cover"),]
##pull r2
df.Bi.drought.lm <- summary(lm(data=df.Bi.drought,ES_ecosystem~ES_SPP))
df.Bi.warming.lm <- summary(lm(data=df.Bi.warming,ES_ecosystem~ES_SPP))
df.Bi.nitrogen.agb.lm <- summary(lm(data=df.Bi.nitrogen.agb,ES_ecosystem~ES_SPP))
df.Bi.nitrogen.cover.lm <- summary(lm(data=df.Bi.nitrogen.cover,ES_ecosystem~ES_SPP))
df.Bi.drought.lm.r2 <- format(df.Bi.drought.lm$r.squared,digits=3)
df.Bi.warming.lm.r2 <- format(df.Bi.warming.lm$r.squared,digits=3)
df.Bi.nitrogen.agb.lm.r2 <- format(df.Bi.nitrogen.agb.lm$r.squared,digits=3)
df.Bi.nitrogen.cover.lm.r2 <- format(df.Bi.nitrogen.cover.lm$r.squared,digits=3)

df.Lc <- df.raw[(df.raw$Species == "Leymus chinensis"),]
##subdivide so that regression lines can be drawn on plot
df.Lc.drought <- df.Lc[(df.Lc$Note.regression == "Drought"),]
df.Lc.warming <- df.Lc[(df.Lc$Note.regression == "Warming"),]
df.Lc.nitrogen.agb <- df.Lc[(df.Lc$Note.regression == "Nitrogen - AGB"),]
df.Lc.nitrogen.cover <- df.Lc[(df.Lc$Note.regression == "Nitrogen - Cover"),]
##pull r2
df.Lc.drought.lm <- summary(lm(data=df.Lc.drought,ES_ecosystem~ES_SPP))
df.Lc.warming.lm <- summary(lm(data=df.Lc.warming,ES_ecosystem~ES_SPP))
df.Lc.nitrogen.agb.lm <- summary(lm(data=df.Lc.nitrogen.agb,ES_ecosystem~ES_SPP))
df.Lc.nitrogen.cover.lm <- summary(lm(data=df.Lc.nitrogen.cover,ES_ecosystem~ES_SPP))
df.Lc.drought.lm.r2 <- format(df.Lc.drought.lm$r.squared,digits=3)
df.Lc.warming.lm.r2 <- format(df.Lc.warming.lm$r.squared,digits=3)
df.Lc.nitrogen.agb.lm.r2 <- format(df.Lc.nitrogen.agb.lm$r.squared,digits=3)
df.Lc.nitrogen.cover.lm.r2 <- format(df.Lc.nitrogen.cover.lm$r.squared,digits=3)

p <-  ggplot(df.raw, aes(x=ES_SPP, y=ES_ecosystem, color=units_spp))+
  geom_vline(xintercept=0,color="gray",lty=2) +
  geom_hline(yintercept=0,color="gray",lty=2) +
  geom_point() +
  theme_classic() +
  scale_color_viridis(discrete=T, end=0.6, name="Species response variable") +
  facet_grid(Species~Note.regression, scales="free_x") +
  theme(legend.position = "bottom") +
  
  stat_smooth(data=df.drought,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.nitrogen.agb,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.nitrogen.cover,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.warming,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Ac.nitrogen.agb,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Ac.nitrogen.cover,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Tt.nitrogen.agb,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Tt.drought,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Ps.nitrogen.agb,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Ps.nitrogen.cover,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Ps.drought,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Bd.nitrogen.agb,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Bd.nitrogen.cover,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Bd.drought,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Hc.nitrogen.agb,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Hc.drought,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Hc.warming,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Bi.nitrogen.agb,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Bi.nitrogen.cover,method=lm,se=F,show.legend = F,color="black") +
  stat_smooth(data=df.Lc.drought,method=lm,se=F,show.legend = F,color="black") +
  
  geom_text(data=df.drought,x=2,y=-4,label=paste0("r^2==", df.drought.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.nitrogen.agb,x=2,y=-4,label=paste0("r^2==", df.nitrogen.agb.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.nitrogen.cover,x=2,y=-4,label=paste0("r^2==", df.nitrogen.cover.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.warming,x=2,y=-4,label=paste0("r^2==", df.warming.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Ac.nitrogen.agb,x=2,y=-4,label=paste0("r^2==", df.Ac.nitrogen.agb.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Ac.nitrogen.cover,x=2,y=-4,label=paste0("r^2==", df.Ac.nitrogen.cover.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Tt.nitrogen.agb,x=2,y=-4,label=paste0("r^2==", df.Tt.nitrogen.agb.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Tt.drought,x=2,y=-4,label=paste0("r^2==", df.Tt.drought.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Ps.nitrogen.agb,x=2,y=-4,label=paste0("r^2==", df.Ps.nitrogen.agb.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Ps.nitrogen.cover,x=2,y=-4,label=paste0("r^2==", df.Ps.nitrogen.cover.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Ps.drought,x=2,y=-4,label=paste0("r^2==", df.Ps.drought.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Bd.nitrogen.agb,x=2,y=-4,label=paste0("r^2==", df.Bd.nitrogen.agb.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Bd.nitrogen.cover,x=2,y=-4,label=paste0("r^2==", df.Bd.nitrogen.cover.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Bd.drought,x=2,y=-4,label=paste0("r^2==", df.Bd.drought.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Hc.nitrogen.agb,x=2,y=-4,label=paste0("r^2==", df.Hc.nitrogen.agb.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Hc.warming,x=2,y=-4,label=paste0("r^2==", df.Hc.warming.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Hc.drought,x=2,y=-4,label=paste0("r^2==", df.Hc.drought.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Bi.nitrogen.agb,x=2,y=-4,label=paste0("r^2==", df.Bi.nitrogen.agb.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Bi.nitrogen.cover,x=2,y=-4,label=paste0("r^2==", df.Bi.nitrogen.cover.lm.r2), parse = TRUE,show.legend = FALSE) +
  geom_text(data=df.Lc.drought,x=2,y=-4,label=paste0("r^2==", df.Lc.drought.lm.r2), parse = TRUE,show.legend = FALSE) +
  
  xlab("Dominant species simple effect size")+
  ylab("Ecosystem simple effect size") +
  ylim(c(-6.3,6.3)) +
  xlim(c(-6.3,6.3))
ggsave("effect_size_byspecies_bydriver.pdf",width=8,height=12)
p

