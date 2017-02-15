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
