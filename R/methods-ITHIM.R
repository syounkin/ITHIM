#' @export
setMethod("show", signature(object="ITHIM"), function(object){
  cat("Hello vargo.\n", sep = "")
})

#' @export
setMethod("plot", signature(x = "ITHIM"), function(x){
  tilePlot(x, n = 5)
})

#' @export
setMethod("update", signature(x = "ITHIM", parList = "missing"), function(x, parList){
    x <- new("ITHIM", parameters = parameters <- x@parameters, means = means <- computeMeanMatrices(as(parameters,"list")), quintiles = getQuintiles(means, as(parameters,"list")))

    return(x)
})

#' @export
setMethod("update", signature(x = "ITHIM", parList = "ParameterSet"), function(x, parList){
    x@parameters <- parList
    x <- update(x)
    return(x)
})

#' @export
setMethod("update", signature(x = "ITHIM", parList = "list"), function(x, parList){

    parList <- createParameterSet(parList)
    x <- update(x, parList)

    return(x)
})

#' @export
setAs("ITHIM", "list", function(from) list(parameters = as(from@parameters,"list"),
                                           means = from@means,
                                           quintiles = from@quintiles))

#' @export
setMethod("tilePlot", signature(x = "ITHIM", n = "numeric"), function(x, n){

    ITHIM.baseline <- as(x, "list")
    ITHIM.baseline$parameters <- as(ITHIM.baseline$parameters, "list")
    baseWalk <- ITHIM.baseline$parameters$muwt
    baseCycle <- ITHIM.baseline$parameters$muct
    upper <- 4*max(c(baseWalk,baseCycle))
    results <- data.frame()
    wVec <- seq(0,upper,length.out = n)
    cVec <- wVec

    for(muwt in wVec){
        ITHIM.scenario <- updateITHIM(ITHIM.baseline, "muwt", muwt)
        for(muct in cVec){
            if(muwt !=0 | muct !=0){
                ITHIM.scenario <- updateITHIM(ITHIM.scenario, "muct", muct)
                comparativeRisk <- data.frame(cycleTime = muct,
                                              walkTime= muwt,
                                              DALYS = sumDALY(ITHIM.baseline, ITHIM.scenario)
                                              )
                results <- rbind(comparativeRisk, results)
            }
        }
    }

    p <- ggplot(results, aes(x = walkTime, y = cycleTime, fill = (DALYS + getDALYs(x))/1e6))
    p + geom_tile() + geom_hline(yintercept=baseCycle, linetype = 2) + geom_vline(xintercept=baseWalk, linetype = 2) + scale_fill_gradientn(colours = terrain.colors(10),name = "DALYs (millions)")

})


#' @export
setMethod("getDALYs", signature(x = "ITHIM"), function(x){

    sum(subset(melt(x@parameters@GBD,),variable == "daly")$value) # very shaky

}
)
