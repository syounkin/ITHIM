#' @export
setMethod("show", signature(object="ParameterSet"), function(object){
    cat("Hello vargo, this is a ParameterSet object.  The parameter names are listed below.\n", sep = "")
    cat(c(slotNames(getParameterSet(foo)), "\n"), sep = ", ")
    cat("Any questions?")
})

#' @export
setAs("ParameterSet", "list", function(from) list(Rwt = from@Rwt, Rct = from@Rct, Rws = from@Rws, muwt = from@muwt,
    muws = from@muws, muct = from@muct, cv = from@cv, cvNonTravel = from@cvNonTravel,
    nAgeClass = from@nAgeClass, muNonTravel = from@muNonTravel, muNonTravelMatrix = from@muNonTravelMatrix,
    GBD = from@GBD, meanType = from@meanType, quantiles = from@quantiles)
    )

#' @export
setMethod("createITHIM", signature(x = "ParameterSet"), function(x){
    ITHIM <- createITHIMFunction()
    ITHIM <- update(ITHIM, x)
    return(ITHIM)
})

#' @export
setMethod("createITHIM", signature(x = "missing"), function(x){
    ITHIM <- createITHIMFunction()
    return(ITHIM)
})

#' @export
setMethod("update", signature(x = "ParameterSet", parList = "list"), function(x, parList){
    x <- as(x, "list")
    for(i in 1:length(parList) ){
        x[[names(parList)[i]]] <- parList[[i]] # Also kind of slow
    }
    return(createParameterSet(x))
})
