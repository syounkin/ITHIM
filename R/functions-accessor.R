getWalkTimeFunction <- function(ITHIM, form = 2){
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
getCycleTimeFunction <- function(ITHIM, form = 2){
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
getNonTravelMETsFunction <- function(ITHIM, form = 2){
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
