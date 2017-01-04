#' @export
setMethod("show", signature(object="ParameterSet"), function(object){
    cat("\n~~~~~ ITHIM Parameters ~~~~\n")
    cat(c("Walking Time:\n  Mean = ", object@muwt, " min./week\n"), sep = "")
    cat("  Relative Means = ")
    cat(round(object@Rwt,2), sep = ", ")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat(c("Cycling Time:\n  Mean = ", object@muct, " min./week\n"), sep = "")
    cat("  Relative Means = ")
    cat(round(object@Rct,2), sep = ", ")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat(c("Walking Speed:\n  Mean = ", object@muws, " mph\n"), sep = "")
    cat("  Relative Means = ")
    cat(round(object@Rws,2), sep = ", ")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat(c("Physical Activity (non-travel):\n  Mean = ", object@muNonTravel, " MET-hrs./week\n"), sep = "")
    cat("  Relative Means = ")
    cat(round(object@muNonTravelMatrix,2), sep = ", ")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Coefficients of Variation:\n")
    cat(c("  Active Transport: ", object@cv, "\n"), sep = "")
    cat(c("  Physical Activity (non-travel): ", object@cvNonTravel), sep = "")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Parameter names: ", sep = "")
    cat(c(slotNames(object), "\n"), sep = ", ")
    cat("Enjoy.\n")
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
        x[[names(parList)[i]]] <- parList[[i]]
    }
    return(createParameterSet(x))
})

#' @export
setMethod("getMeans", signature(x = "ParameterSet"), function(x){
    return(data.frame(walk = x@muwt, cycle = x@muct, nonTravel = x@muNonTravel))
})
