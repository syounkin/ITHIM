#' @export
setMethod("show", signature(object="ParameterSet"), function(object){
  cat("Hello vargo.\n", sep = "")
})

#' @export
setAs("ParameterSet", "list", function(from) list(Rwt = from@Rwt, Rct = from@Rct, Rws = from@Rws, muwt = from@muwt, 
    muws = from@muws, muct = from@muct, cv = from@cv, cvNonTravel = from@cvNonTravel, 
    nAgeClass = from@nAgeClass, muNonTravel = from@muNonTravel, muNonTravelMatrix = from@muNonTravelMatrix, 
    GBD = from@GBD, meanType = from@meanType, quantiles = from@quantiles)
)
#' @export
setMethod("buildITHIM", signature(x = "ParameterSet"), function(x){
    ITHIM <- createITHIM()
    ITHIM <- update(ITHIM, as(x,"list")) # this is the slow step
    return(ITHIM)
})
#' @export
setMethod("buildITHIM", signature(x = "missing"), function(x){
    ITHIM <- createITHIM()
    return(ITHIM)
})
#' @export
setMethod("update", signature(x = "ParameterSet", parName = "list", parValue = "missing"), function(x, parName, parValue){
    parList <- as(x,"list")
    for(i in 1:length(parName) ){
        parList[[names(parName)[i]]] <- parName[[i]]
    }
    return(createParameterSet(parList))
})
