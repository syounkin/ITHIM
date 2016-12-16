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
