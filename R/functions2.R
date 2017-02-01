#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes the RR for an exposure of x MET
#'
#' We use the transformation RR_x = RR_1^(x^k), where RR_1 is the
#' relative risk for one MET.
#'
#' @return A list of matrices of quintiles of RR_x stratified by age
#'     class and sex
#'
#' @note k is fixed at 0.5 for now
#'
#' @export
MET2RR <- function(RR,MET){
    mapply(FUN = function(x, y) x^(y^0.5), RR, MET, SIMPLIFY = FALSE)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes AF given baseline and scenario
#'
#' Computes AF given baseline and scenario RRs relative to baseline.
#'
#' @return A list of AFs stratified by age and sex
#'
#' @export
AFForList <- function(scenario,baseline){
    mapply(function(scenario,baseline) (rowSums(scenario)-rowSums(baseline))/rowSums(scenario), scenario, baseline)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes AF given baseline and scenario
#'
#' Computes AF given baseline and scenario RRs relative to baseline.
#'
#' @param scenario RR compared with no exposure
#'
#' @param baseline RR compared with no exposure
#'
#' @return A list of AFs stratified by age and sex
#'
#' @export
AFForList2 <- function(scenario,baseline){
    mapply(function(scenario,baseline) 1 - rowSums(scenario)/rowSums(baseline), scenario, baseline)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Foo
#'
#' Foo
#'
#' @return Foo
#'
#' @export
normalizeDiseaseBurden <- function(diseaseBurden){
    lapply(diseaseBurden, function(x) x/x[,1])
    }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plots a RR matrix
#'
#' Plots a RR matrix
#'
#' @return A ggplot object
#'
#' @export
plotRR <- function(RR.baseline,RR.scenario){
D <- melt(list(baseline = RR.baseline, scenario = RR.scenario), c("age","quint"), value.name = "RR")
D <- subset(D, !(age %in% paste0("ageClass",1:2)))
names(D) <- c("age","quint","RR","sex","vision")
p <- ggplot(D, aes(age,  RR)) + geom_bar(aes(fill=vision), stat = "identity", position = "dodge")
p <- p + facet_grid( quint ~ sex )
return(p)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ??
#'
#' ??
#'
#' @return ??
#'
#'
#' @export
burdenFunction <- function(x2,y2,z2,burden,baseline=FALSE){
    if(!baseline){
        mapply(function(x,y,z){x[,burden] * y / z}, x2, y2, z2, SIMPLIFY = FALSE)
    }else{
        mapply(function(x,y,z){x[,burden] / z}, x2, y2, z2, SIMPLIFY = FALSE)
    }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Internal function.
#'
#' ??
#'
#' @return ??
#'
#'
#' @export
calculateBurden <- function(burden, normalizedDiseaseBurden){

    foo <- function(x,y){
        matrix(x, nrow = length(x), ncol = ncol(y)) * y
        }

    foo2 <- function(x,y){
        mapply(foo, x, y, SIMPLIFY = FALSE)
        }

    List <- mapply(foo2, burden, normalizedDiseaseBurden, SIMPLIFY = FALSE)

    Burden <- lapply(List, function(x){
        lapply(x,rowSums)
        })

        return(Burden)

        }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' This function coverts the 2001 CDC Age variable to years.
#'
#' @param value The value for age, i.e., characters 2-4 of the code
#'     (sloppy with class!)
#' @param unit The units for age, i.e., chatater 1 of the code (sloppy
#'     with class!)
#' @note This function is likely only applicable to the 2001 data
#'     file.
#' @return A numeric vector of age in years - this is a test
#'
#' @export
convertAge <- function(value, unit){

    value <- ifelse(value == "999", NA, value)

    convertedValue <- ifelse( unit == "1", value,
    ifelse( unit == "2", value/12,
    ifelse( unit == "4", value/365,
    ifelse( unit == "5", value/365/24,
    ifelse( unit == "6", value/365/24/60,
    ifelse( unit == "9", NA, 999 ))))))

    return(as.numeric(convertedValue))

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Transforms the RR object
#'
#' Transforms the RR object into something more convenient.
#'
#' @return A list of two matrices of RRs stratified by age class and
#'     sex
#'
#' @export
reshapeRR <- function(RR, nQuantiles = 5){
    nAgeClass <- 8
    list( M = matrix(RR[,"M"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), paste0("quint",1:nQuantiles))),F = matrix(RR[,"F"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), paste0("quint",1:nQuantiles))))
    }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes a ratio of elements in two lists
#'
#' Computes a ratio of elements in two lists.
#'
#' @return A list of ratios
#'
#' @export
ratioForList <- function(baseline,scenario){
mapply(FUN = "/", baseline, scenario, SIMPLIFY = FALSE)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieve a density vector for a logNormal distribution
#'
#' Retrieve a density vector for a logNormal distribution
#'
#' @param mu The mean (on log scale or not?  Figure this out.
#' @param sd The standard deviation (on log scale or not?  Figure this out.
#'
#' @return A vector of length 2000 with density values over the
#'     interval 0 to 2000
#'
#' @export
getLogNormal <- function(mu,sd){
    dlnorm(seq(0,2000,length.out=1e3), log(mu/sqrt(1+sd^2/mu^2)), sqrt(log(1+sd^2/mu^2)))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Converts age to ITHIM age class
#'
#' Converts age to ITHIM age class
#'
#' @param age A vector of ages.  This vector will be coerced to numeric.
#'
#' @return A character vector of ITHIM age categories
#'
#' @export
convertToAgeClass <- function(age){
  age <- as.numeric(age)
  agecat <- ifelse( age <= 4, "00-04", ifelse( age <= 14, "05-14", ifelse( age <= 29, "15-29", ifelse( age <= 44, "30-44", ifelse( age <= 59, "45-59", ifelse( age <= 69, "60-69", ifelse( age <= 79, "70-79", ifelse( age > 80, "80+", NA))))))))
  return(agecat)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns the mean walk time matrix
#'
#' Returns the mean walk time matrix
#'
#' @param ITHIM An ITHIM object
#'
#' @return A numerical matrix of mean walk time
#'
#' @export
getWalkTime <- function(ITHIM, form = 2){
    if( form == 1 ){
        walkTime <- with(as(ITHIM@parameters,"list"), Rwt*muwt)
    }else if (form == 2){
        walkTime <- with(as(ITHIM@parameters,"list"), melt(Rwt*muwt))
        names(walkTime) <- c("ageClass","sex","mu")
    }else{
        message("Bad form for getWalkTime()")
    }
    return(data.frame(walkTime, type = "walk time"))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns the mean cycle time matrix
#'
#' Returns the mean cycle time matrix
#'
#' @param ITHIM An ITHIM object
#'
#' @return A numerical matrix of mean cycle time
#'
#' @export
getCycleTime <- function(ITHIM, form = 2){
    if( form == 1 ){
        cycleTime <- with(as(ITHIM@parameters,"list"), Rct*muct)
    }else if (form == 2){
        cycleTime <- with(as(ITHIM@parameters,"list"), melt(Rct*muct))
        names(cycleTime) <- c("ageClass","sex","mu")
    }else{
        message("Bad form for getCycleTime()")
    }
    return(data.frame(cycleTime, type = "cycle time"))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns the non-travel METs matrix
#'
#' Returns the non-travel METs matrix
#'
#' @param ITHIM An ITHIM object
#'
#' @return A numerical matrix of mean non-travel means
#'
#' @export
getNonTravelMETs <- function(ITHIM, form = 2){
    if( form == 1 ){
        nonTravelMETs <- with(as(ITHIM@parameters,"list"), muNonTravelMatrix*muNonTravel)
    }else if (form == 2){
        nonTravelMETs <- with(as(ITHIM@parameters,"list"), melt(muNonTravelMatrix*muNonTravel))
        names(nonTravelMETs) <- c("ageClass","sex","mu")
    }else{
        message("Bad form for getCycleTime()")
    }
    return(data.frame(nonTravelMETs, type = "non-travel METs"))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns the mean walk speed matrix
#'
#' Returns the mean walk speed matrix
#'
#' @param ITHIM An ITHIM object
#'
#' @return A numerical matrix of mean walk speed
#'
#' @export
getWalkSpeed <- function(ITHIM, form = 2){
    if( form == 1 ){
        walkSpeed <- with(as(ITHIM@parameters,"list"), Rws*muws)
    }else if (form == 2){
        walkSpeed <- with(as(ITHIM@parameters,"list"), melt(Rws*muws))
        names(walkSpeed) <- c("ageClass","sex","mu")
    }else{
        message("Bad form for getWalkSpeed()")
    }
    return(data.frame(walkSpeed, type = "walk speed"))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns
#'
#' Returns
#'
#' @param ITHIM.baseline An ITHIM object
#' @param ITHIM.scenario An ITHIM object
#'
#' @return
#'
#' @export
sumDALY <- function(ITHIM.baseline, ITHIM.scenario){
    ITHIM.baseline <- as(ITHIM.baseline, "list")
    ITHIM.scenario <- as(ITHIM.scenario, "list")
  return(sum(unlist(data.frame(compareModels(ITHIM.baseline,ITHIM.scenario)$daly.delta)[-1,]))) # AgeClass 1 is removed from totals
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns
#'
#' Returns
#'
#' @param ITHIM.baseline An ITHIM object
#' @param ITHIM.scenario An ITHIM object
#'
#' @return
#'
#' @export
sumCVD <- function(ITHIM.baseline, ITHIM.scenario){
    ITHIM.baseline <- as(ITHIM.baseline, "list")
    ITHIM.scenario <- as(ITHIM.scenario, "list")
  return(sum(unlist(data.frame(compareModels(ITHIM.baseline,ITHIM.scenario)$daly.delta$CVD)[-1,]))) # AgeClass 1 is removed from totals
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns
#'
#' Returns
#'
#' @param ITHIM.baseline An ITHIM object
#' @param ITHIM.scenario An ITHIM object
#'
#' @return
#'
#' @export
sumDiabetes <- function(ITHIM.baseline, ITHIM.scenario){
    ITHIM.baseline <- as(ITHIM.baseline, "list")
    ITHIM.scenario <- as(ITHIM.scenario, "list")
  return(sum(unlist(data.frame(compareModels(ITHIM.baseline,ITHIM.scenario)$daly.delta$Diabetes)[-1,]))) # AgeClass 1 is removed from totals
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns
#'
#' Returns
#'
#' @param ITHIM.baseline An ITHIM object
#' @param ITHIM.scenario An ITHIM object
#'
#' @return
#'
#' @export
sumDepression <- function(ITHIM.baseline, ITHIM.scenario){
    ITHIM.baseline <- as(ITHIM.baseline, "list")
    ITHIM.scenario <- as(ITHIM.scenario, "list")
  return(sum(unlist(data.frame(compareModels(ITHIM.baseline,ITHIM.scenario)$daly.delta$Depression)[-1,]))) # AgeClass 1 is removed from totals
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns
#'
#' Returns
#'
#' @param ITHIM.baseline An ITHIM object
#' @param ITHIM.scenario An ITHIM object
#'
#' @return
#'
#' @export
sumBreastCancer <- function(ITHIM.baseline, ITHIM.scenario){
    ITHIM.baseline <- as(ITHIM.baseline, "list")
    ITHIM.scenario <- as(ITHIM.scenario, "list")
  return(sum(unlist(data.frame(compareModels(ITHIM.baseline,ITHIM.scenario)$daly.delta$BreastCancer)[-1,]))) # AgeClass 1 is removed from totals
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns
#'
#' Returns
#'
#' @param ITHIM.baseline An ITHIM object
#' @param ITHIM.scenario An ITHIM object
#'
#' @return
#'
#' @export
sumColonCancer <- function(ITHIM.baseline, ITHIM.scenario){
    ITHIM.baseline <- as(ITHIM.baseline, "list")
    ITHIM.scenario <- as(ITHIM.scenario, "list")
  return(sum(unlist(data.frame(compareModels(ITHIM.baseline,ITHIM.scenario)$daly.delta$ColonCancer)[-1,]))) # AgeClass 1 is removed from totals
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns
#'
#' Returns
#'
#' @param ITHIM.baseline An ITHIM object
#' @param ITHIM.scenario An ITHIM object
#'
#' @return
#'
#' @export
sumDementia <- function(ITHIM.baseline, ITHIM.scenario){
    ITHIM.baseline <- as(ITHIM.baseline, "list")
    ITHIM.scenario <- as(ITHIM.scenario, "list")
  return(sum(unlist(data.frame(compareModels(ITHIM.baseline,ITHIM.scenario)$daly.delta$Dementia)[-1,]))) # AgeClass 1 is removed from totals
}

#'@export
createParameterSet <- function(x){

    parList <- as(createParameterList(),"list")
    for(name in names(x)){
        parList[[name]] <- x[[name]]
        }

    pSet <- new("ParameterSet",Rwt = parList$Rwt,
                                        Rct = parList$Rct,
                                        Rws = parList$Rws,
                                        muwt = parList$muwt,
                                        muws = parList$muws,
                                        muct = parList$muct,
                                        cv = parList$cv,
                                        cvNonTravel = parList$cvNonTravel,
                                        nAgeClass = parList$nAgeClass,
                                        muNonTravel = parList$muNonTravel,
                                        muNonTravelMatrix = parList$muNonTravelMatrix,
                                        GBD = parList$GBD,
                                        meanType = parList$meanType,
                quantiles = parList$quantiles,
                roadInjuries = parList$roadInjuries
                )
    return(pSet)
    }
#'@export
s4Methods <- function(class)
{
    methods <-
      showMethods(classes = class, printTo = FALSE)
    methods <- methods[grep("^Function:", methods)]
    sapply(strsplit(methods, " "), "[", 2)
}

#'@export
getMethodsITHIM <- function()
{
    class <- "ITHIM"
    methods <-
      showMethods(classes = class, printTo = FALSE)
    methods <- methods[grep("^Function:", methods)]
    sapply(strsplit(methods, " "), "[", 2)
}
#'@export
computeMultiplier <- function(base, scenario){
    perRatio <- (scenario$perMiles[,"Arterial"]/base$perMiles[,"Arterial"])^0.5
    vehRatio <- (scenario$vehMiles[,"Arterial"]/base$vehMiles[,"Arterial"])^0.5
    arterial <- outer(perRatio,vehRatio,"*")*1*1 # Hard-coded parameters
    }
