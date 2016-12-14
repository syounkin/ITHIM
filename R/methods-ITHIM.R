#' @export
setMethod("show", signature(object="ITHIM"), function(object){
  cat("Hello vargo.\n", sep = "")
})

#' @export
setMethod("plot", signature(x = "ITHIM"), function(x){
  barplot(x@parameters$Rwt)
})

#' @export
setMethod("update", signature(x = "ITHIM", parName = "character", parValue = "numeric"), function(x, parName, parValue){
    x <-  as(x, "list")
    x <- updateITHIM(x, parName = parName, parValue = parValue)
    x <- new("ITHIM", parameters = x$parameters, means = x$means, quintiles = x$quintiles)    
    return(x)
})

#' @export
setMethod("update", signature(x = "ITHIM", parName = "character", parValue = "character"), function(x, parName, parValue){
    x <-  as(x, "list")
    x <- updateITHIM(x, parName = parName, parValue = parValue)
    x <- new("ITHIM", parameters = x$parameters, means = x$means, quintiles = x$quintiles)
    return(x)
})


#' @export
setAs("ITHIM", "list", function(from) list(parameters = from@parameters,
                                           means = from@means,
                                           quintiles = from@quintiles))


