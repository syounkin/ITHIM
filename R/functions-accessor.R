getWalkTimeFunction <- function(ITHIM, form = 2){
    if( form == 1 ){
        walkTime <- ITHIM@means$meanWalkTime
    }else if (form == 2){
        walkTime <- melt(ITHIM@means$meanWalkTime)
        names(walkTime) <- c("ageClass","sex","mean")
    }else{
        message("Bad form for getWalkTime()")
    }
    return(data.frame(walkTime, type = "walk time"))
}
getCycleTimeFunction <- function(ITHIM, form = 2){
    if( form == 1 ){
        walkTime <- ITHIM@means$meanCycleTime
    }else if (form == 2){
        walkTime <- melt(ITHIM@means$meanCycleTime)
        names(walkTime) <- c("ageClass","sex","mean")
    }else{
        message("Bad form for getCycleTime()")
    }
    return(data.frame(walkTime, type = "cycle time"))
}
getNonTravelMETsFunction <- function(ITHIM, form = 2){
    if( form == 1 ){
        nonTravelMETs <- ITHIM@means$meanNonTravel
    }else if (form == 2){
        nonTravelMETs <- melt(ITHIM@means$meanNonTravel)
        names(nonTravelMETs) <- c("ageClass","sex","mean")
    }else{
        message("Bad form for getnonTravelMETs()")
    }
    return(data.frame(nonTravelMETs, type = "non-travel METs"))
}
