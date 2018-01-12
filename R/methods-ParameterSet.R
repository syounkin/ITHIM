setMethod("show", signature(object="ParameterSet"), function(object){
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("~~~~~ Physical Activity ~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat(c("Walking:\n  Mean = ", round(object@muwt,2), " min./week\n"), sep = "")
    cat("  Relative Means = ")
    cat(head(round(object@Rwt,2), n = 3), "...", sep = ", ")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat(c("Cycling:\n  Mean = ", round(object@muct,2), " min./week\n"), sep = "")
    cat("  Relative Means = ")
    cat(head(round(object@Rct,2), n = 3), "...", sep = ", ")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat(c("Physical Activity (non-travel):\n  Mean = ", object@muNonTravel, " MET-hrs./week\n"), sep = "")
    cat("  Relative Means = ")
    cat(head(round(object@muNonTravelMatrix,2), n = 3), "...", sep = ", ")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Coefficients of Variation:\n")
    cat(c("  Active Transport: ", round(object@cv,2), "\n"), sep = "")
    cat(c("  Physical Activity (non-travel): ", round(object@cvNonTravel,2)), sep = "")
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    if( object@meanType == "referent" ){
        cat("Means given above are for the referent class \n(women aged 15-30 yrs.).  ")
    } else if( object@meanType == "overall" ){
        cat("Means given above are overall means.  ")
    }else{
        message("Problem with meanType parameter")
        }
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("~~~~~ Road Injuries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Not fully implemented yet.\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Road Injury Count: Use getRoadInjuries() to display.\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Safety in Numbers: Use getSIN() to display.\n(Not implemented yet.)\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("~~~~~ Air Pollution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Not yet implemented\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("Parameter Names: ", sep = "")
    cat(c(slotNames(object), "\n"), sep = ", ")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
})

#' @export
setAs("ParameterSet", "list", function(from) list(Rwt = from@Rwt, Rct = from@Rct, muwt = from@muwt,
    muct = from@muct, cv = from@cv, cvNonTravel = from@cvNonTravel,
    nAgeClass = from@nAgeClass, muNonTravel = from@muNonTravel, muNonTravelMatrix = from@muNonTravelMatrix, F = from@F,
    GBD = from@GBD, meanType = from@meanType, quantiles = from@quantiles, roadInjuries = from@roadInjuries, distRoadType = from@distRoadType, safetyInNumbers = from@safetyInNumbers, EXCEL = from@EXCEL)
    )

#' @rdname update-methods
#' @aliases update
#' @export
setMethod("update", signature(x = "ParameterSet", parList = "list"), function(x, parList){
    x <- as(x, "list")

#    excelIndex <- which(str_to_upper(names(parList)) == "EXCEL")
#    names(parList)[excelIndex] <- "EXCEL"

    for(i in 1:length(parList) ){
        x[[names(parList)[i]]] <- parList[[i]]
    }
    return(createParameterSet(x))
})

#' @rdname getMeans-methods
#' @aliases getMeans
#' @export
setMethod("getMeans", signature(x = "ParameterSet"), function(x){
    return(data.frame(walk = x@muwt, cycle = x@muct, nonTravel = x@muNonTravel))
})

#' @rdname getRoadInjuries-methods
#' @aliases getRoadInjuries
#' @export
setMethod("getRoadInjuries", signature(x = "ParameterSet"), function(x){
    ## RI <- lapply(x@roadInjuries, function(df) {
    ##     df <- data.frame(walk = df$walk,cycle = df$cycle,bus = df$bus,car = df$car,HGV = df$HGV,LGV = df$LGV,mbike = df$mbike,NOV = df$NOV)
    ##     dimnames(df) <- list(c("walk","cycle","bus","car","HGV","LGV","mbike"),c("walk","cycle","bus","car","HGV","LGV","mbike","NOV"))
    ##     return(df)
    ##     })
##    return(RI)
    return(x@roadInjuries)
})

#' @rdname getDistRoadType-methods
#' @aliases getDistRoadType
#' @export
setMethod("getDistRoadType", signature(x = "ParameterSet"), function(x){
    return(x@distRoadType)
})
#' @rdname getParameterNames-methods
#' @aliases getParameterNames
#' @export
setMethod("getParameterNames", signature(x = "ParameterSet"), function(x){
    return(slotNames(x))
})
#' @rdname getSiN-methods
#' @aliases getSiN
#' @export
setMethod("getSiN", signature(x = "ParameterSet"), function(x){
    return(x@safetyInNumbers)
})
#' @rdname getF-methods
#' @aliases getF
#' @export
setMethod("getF", signature(x = "ParameterSet", prob = "logical"), function(x, prob){
    F <- x@F
    if(prob){
        return(F/sum(F))
    }else{
        return(F)
    }
})
#' @rdname getF-methods
#' @aliases getF
#' @export
setMethod("getF", signature(x = "ParameterSet", prob = "missing"), function(x, prob){
    return(getF(x, prob = FALSE))
})
