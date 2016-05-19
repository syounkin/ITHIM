#library(ggplot2)
#library(reshape2)
#library(plyr)
#http://www.cdc.gov/nchs/data_access/Vitalstatsonline.htm

lengths <- c(58,1,2,7,2,53,2,3,13,4,295)

# dataDir <- "~/Box Documents/work/CoBenefits/MaggieShare/VitalStats/"
dataDir <- "~/ITHIM/R/data/"

mortality2001 <- read.fwf(paste0(dataDir,"/Mort01us.dat"), lengths, header=F, as.is=T)

#check column numbers when changing years
mort <- mortality2001[,c(2,3,5,7,8,10)]
names(mort) <- c("sex","race","age","state","county","cause.of.death")

# mort.MW <- subset(mort.limited, state == 17 | state == 18 | state == 26 | state == 27 | state == 39 | state == 55)

# write.csv(mort.MW, "~/Box Documents/work/CoBenefits/MaggieShare/VitalStats/MW_mortalities_from_CDCvs.csv", row.names=F)

# mort.MW <- read.csv("~/Box Documents/work/CoBenefits/MaggieShare/VitalStats/MW_mortalities_from_CDCvs.csv", header=T)

# stats <- ddply(mort.MW, .(age), summarise, 
               # number.obs  = length(county)
# )


#write.csv(stats, "~/Box Documents/work/CoBenefits/MaggieShare/VitalStats/MW_IDC_Codes.csv", row.names=F)

ICD.key  <- read.csv(paste0(dataDir, "IDC_Codes_key.csv"), header=T)
county.key  <- read.csv(paste0(dataDir,"TripleWinCountyKey.csv"), header=T)


age <- as.data.frame(matrix(c(1:27,"age0-4","age0-4","age0-4","age0-4","age0-4","age0-4","age5-14","age5-14","age15-29","age15-29","age15-29","age30-44","age30-44","age30-44","age45-59","age45-59","age45-59","age60-69","age60-69","age70-79","age70-79","age80+","age80+","age80+","age80+","age80+",""), ncol = 2, nrow = 27))
names(age) <- c("age","ITHIM.age")

mort <- merge(mort, age, by.x="age", by.y="age")
mort <- merge(mort, ICD.key, by.x="cause.of.death", by.y="cause.of.death")
#mort <- merge(mort, county.key, by.x=c("state","county"), by.y=c("STATE","COUNTY"))


mort.ITHIM <- subset(mort, Include == 1)

age.sex <- read.csv(paste0(dataDir,"county_age_sex2002.csv"), header=T)

names(age.sex) <- c("county.name","state.name", "MSA","sex","all.ages","age0-4","age5-14","age15-29","age30-44","age45-59","age60-69","age70-79","age80+", "STATE","COUNTY") 

age.sex.2 <- melt(age.sex[,c(14,15,4,6:13)], id=c("STATE","COUNTY","sex"))

age.sex.3 <- dcast(age.sex.2, STATE + COUNTY + variable ~ sex, mean)

mort.age <- merge(mort.ITHIM, age.sex.3, by.x=c("state","county", "ITHIM.age"), by.y=c("STATE","COUNTY","variable"))

mort.age$year <- 2001

mort.age$app.pop <- ifelse(mort.age$sex ==1, mort.age$male, mort.age$female)

#ICDsub <- subset(ICD.key, Include ==1)

stats <- ddply(mort.age, .(state, county, ITHIMCause, sex, ITHIM.age, year), summarise, 
               number.obs  = length(Include)
)

stats$ITHIM.age <- as.factor(stats$ITHIM.age)

write.csv(stats, paste0(dataDir,"Disease_stats_4_ITHIM2001.csv"), row.names=F)
