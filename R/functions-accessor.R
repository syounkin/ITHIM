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
## Bugs below.  See above code.  Copy it.
## getCycleTimeFunction <- function(ITHIM, form = 2){
##     if( form == 1 ){
##         cycleTime <- with(as(ITHIM@parameters,"list"), Rct*muct)
##     }else if (form == 2){
##         cycleTime <- with(as(ITHIM@parameters,"list"), melt(Rct*muct))
##         names(cycleTime) <- c("ageClass","sex","mu")
##     }else{
##         message("Bad form for getCycleTime()")
##     }
##     return(data.frame(cycleTime, type = "cycle time"))
## }
## getNonTravelMETsFunction <- function(ITHIM, form = 2){
##     if( form == 1 ){
##         nonTravelMETs <- with(as(ITHIM@parameters,"list"), muNonTravelMatrix*muNonTravel)
##     }else if (form == 2){
##         nonTravelMETs <- with(as(ITHIM@parameters,"list"), melt(muNonTravelMatrix*muNonTravel))
##         names(nonTravelMETs) <- c("ageClass","sex","mu")
##     }else{
##         message("Bad form for getCycleTime()")
##     }
##     return(data.frame(nonTravelMETs, type = "non-travel METs"))
## }
