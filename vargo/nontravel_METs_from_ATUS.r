library(ggplot2)
library(reshape2)
library(plyr)

#readLines("~/Box Documents/work/CoBenefits/MaggieShare/AmerTimeUse/atusact_2003.dat", n=10)




# activity <- read.table("~/Box Documents/work/CoBenefits/MaggieShare/MET Analysis/AmerTimeUse/atusact_2003.dat", header=T, skip=0, sep=",")

summary <- read.table("~/Box Documents/work/CoBenefits/MaggieShare/MET Analysis/AmerTimeUse/atussum_0311.dat", header=T, skip=0, sep=",")

# respondants <- read.table("~/Box Documents/work/CoBenefits/MaggieShare/MET Analysis/AmerTimeUse/atusresp_2003.dat", header=T, skip=0, sep=",")

# roster <- read.table("~/Box Documents/work/CoBenefits/MaggieShare/MET Analysis/AmerTimeUse/atusrost_2003.dat", header=T, skip=0, sep=",")


CPS <- read.table("~/Box Documents/work/CoBenefits/MaggieShare/MET Analysis/AmerTimeUse/atuscps_0311.dat", header=T, skip=0, sep=",")

CPS.2 <- CPS[,c("TUCASEID", "GESTFIPS", "GEREG","GEMETSTA")]
CPS.2$IDCHAR <- as.character(CPS.2[,"TUCASEID"])

summary.2 <- summary[, c("TUCASEID", "TEAGE",    "TESEX",    "t050203",  "t130101",  "t130102",  "t130103",  "t130104",  "t130106",  "t130108", "t130109", "t130111", "t130113",  "t130115", "t130116",  "t130117", "t130119",  "t130120",  "t130122",  "t130123", "t130124",  "t130126",  "t130127", "t130128", "t130130",  "t130131",  "t130132", "t130133",  "t130134", "t130135", "t130136" , "t130199" , "t139999")]
summary.2$IDCHAR <- as.character(summary.2[,"TUCASEID"])

summary.2a <- summary.2[,c(1:3, length(summary.2))]
summary.2b <- summary.2[,c(5:length(summary.2)-1)]
minutes <- rowSums(summary.2b)
summary.2a$minutes <- minutes

metkey <- as.matrix(c(3.0,7.3,5.0,6.5,7.5,3.5,5.8,7.8,6.0,8.0,3.8,7.8,8.0,5.3,7.3,9.8,6.3,7.0,7.0,5.0,6.8,3.0,3.5,5.5,6.0,4.0,6.0,3.0,3.0,3.0), nrow = 1, ncol=30)
summary.2c <- mapply(`*`,summary.2b,metkey)
mets <- rowSums(summary.2c)
summary.2a$mets <- mets

summary.2a$agecat <- ifelse(summary.2a$TEAGE <5, 1, 
                     ifelse(summary.2a$TEAGE > 4 & summary.2a$TEAGE < 15, 2, 
                     ifelse(summary.2a$TEAGE > 14 & summary.2a$TEAGE < 30, 3, 
                     ifelse(summary.2a$TEAGE > 29 & summary.2a$TEAGE < 45, 4, 
                     ifelse(summary.2a$TEAGE > 44 & summary.2a$TEAGE < 60, 5, 
                     ifelse(summary.2a$TEAGE > 59 & summary.2a$TEAGE < 70, 6, 
                     ifelse(summary.2a$TEAGE > 69 & summary.2a$TEAGE < 80, 7, 8
)))))))

summary.2a$agecat2 <- ifelse(summary.2a$TEAGE <5, "age0-4", 
                     ifelse(summary.2a$TEAGE > 4 & summary.2a$TEAGE < 15, "age5-14", 
                     ifelse(summary.2a$TEAGE > 14 & summary.2a$TEAGE < 30, "age15-29", 
                     ifelse(summary.2a$TEAGE > 29 & summary.2a$TEAGE < 45, "age30-44", 
                     ifelse(summary.2a$TEAGE > 44 & summary.2a$TEAGE < 60, "age45-59", 
                     ifelse(summary.2a$TEAGE > 59 & summary.2a$TEAGE < 70, "age60-69", 
                     ifelse(summary.2a$TEAGE > 69 & summary.2a$TEAGE < 80, "age70-79", "age80+"
)))))))

use <- merge(summary.2a, CPS.2, by.x="TUCASEID", by.y="TUCASEID", all.x=T, sorted=F)

use$sexcat <- ifelse(use$TESEX == 1, "male", "female")

use$regcat <- ifelse(use$GEREG == 1, "northeast",
              ifelse(use$GEREG == 2, "midwest",
              ifelse(use$GEREG == 3, "south",  "west")))

use$metcat <- ifelse(use$GEMETSTA == 1, "metro",
              ifelse(use$GEMETSTA == 2, "non-metro", "unclassified"))


use <- use[,c("minutes","mets","agecat2","GESTFIPS","sexcat","regcat","metcat")]

head(use)

use.region <- ddply(use, .(sexcat, agecat2, regcat, metcat), summarise, 
             mean_min = mean(minutes, na.rm=T),
             mean_mets = mean(mets, na.rm=T),
             n = length(minutes)
)


use.state <- ddply(use, .(sexcat, agecat2, GESTFIPS, metcat), summarise, 
             mean_min = mean(minutes, na.rm=T),
             mean_mets = mean(mets, na.rm=T),
             n = length(minutes)
)


use2 <- subset(use, metcat == "metro")

use.state2 <- ddply(use2, .(sexcat, agecat2, GESTFIPS), summarise, 
              percentile10 <- quantile(mets, 0.10, na.rm=T), 
              percentile30 <- quantile(mets, 0.3, na.rm=T),  
              percentile50 <- quantile(mets, 0.5, na.rm=T),
              percentile70 <- quantile(mets, 0.7, na.rm=T), 
              percentile90 <- quantile(mets, 0.9, na.rm=T), 
              mean <- mean(mets, na.rm=T), 
              n = length(mets) 
)


use.region2 <- ddply(use2, .(sexcat, agecat2, regcat), summarise, 
              percentile10 <- quantile(mets, 0.10, na.rm=T), 
              percentile30 <- quantile(mets, 0.3, na.rm=T),  
              percentile50 <- quantile(mets, 0.5, na.rm=T),
              percentile70 <- quantile(mets, 0.7, na.rm=T), 
              percentile90 <- quantile(mets, 0.9, na.rm=T), 
              mean <- mean(mets, na.rm=T), 
              n = length(mets) 
)





state.table <- subset(use.state, metcat == "metro")
write.csv(state.table, "~/Box Documents/work/CoBenefits/MaggieShare/MET Analysis/AmerTimeUse/stateresults.csv", row.names=F)
write.csv(use.state2, "~/Box Documents/work/CoBenefits/MaggieShare/MET Analysis/AmerTimeUse/stateresults_quintile.csv", row.names=F)
write.csv(use.region2, "~/Box Documents/work/CoBenefits/MaggieShare/MET Analysis/AmerTimeUse/regionresults_quintile.csv", row.names=F)


ggplot(use.region, aes(x=factor(sexcat), y=mean_mets, fill=factor(agecat2))) + geom_bar(stat="identity", alpha=1) +facet_grid(regcat ~ metcat)

ggplot(subset(use.region, metcat != "unclassified"), aes(x=factor(sexcat), y=mean_mets, fill=factor(agecat2))) + geom_bar(stat="identity", alpha=1, position="dodge") +facet_grid(regcat ~ metcat)


ggplot(subset(use.state, metcat != "unclassified" & GESTFIPS == 17), aes(x=factor(sexcat), y=mean_min, fill=factor(agecat2))) + geom_bar(stat="identity", alpha=1, position="dodge") +facet_grid(GESTFIPS ~ metcat)



