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
