#' @export
setMethod("show", signature(object="ITHIM"), function(object){
  cat("Hello Vargo, this is an ITHIM object.  Currently the methods plot, show, get DALYs and update are available.  This is the show method.  Try plot().  I can't get summary to work.\n", sep = "")
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

    parList <- update(x@parameters, parList)
    x <- update(x, parList)

    return(x)
})

#' @export
setAs("ITHIM", "list", function(from) list(parameters = as(from@parameters,"list"),
                                           means = from@means,
                                           quintiles = from@quintiles))

#' @export
setMethod("tilePlot", signature(x = "ITHIM", n = "numeric"), function(x, n){

    baseWalk <- getMeans(x)$walk
    baseCycle <- getMeans(x)$cycle

    upper <- 4*max(c(baseWalk,baseCycle))
    results <- data.frame()
    wVec <- seq(0,upper,length.out = n)
    cVec <- wVec

    for(muwt in wVec){
        ITHIM.scenario <- update(x, list(muwt = muwt))
        for(muct in cVec){
            if(muwt !=0 | muct !=0){
                ITHIM.scenario <- update(ITHIM.scenario, list(muct = muct))
                comparativeRisk <- data.frame(cycleTime = muct,
                                              walkTime= muwt,
                                              DALYS = sumDALY(x, ITHIM.scenario)
                                              )
                results <- rbind(comparativeRisk, results)
            }
        }
    }

    p <- ggplot(results, aes(x = walkTime, y = cycleTime, fill = (DALYS + getDALYs(x))))
    p + geom_tile() + geom_hline(yintercept=baseCycle, linetype = 2) + geom_vline(xintercept=baseWalk, linetype = 2) + scale_fill_gradientn(colours = terrain.colors(10),name = "DALYs")

})


#' @export
setMethod("getDALYs", signature(x = "ITHIM", bur = "character", dis = "character"), function(x, bur, dis){
    suppressMessages(
        if( dis[1] == "all" ){
        return(sum(subset(getGBD(x), variable == bur)$value, na.rm = TRUE))
    }else{
        return(sum(subset(getGBD(x), variable == bur & disease %in% dis)$value, na.rm = TRUE))
    })
})

#' @export
setMethod("getDALYs", signature(x = "ITHIM", bur = "character", dis = "missing"), function(x, bur){
    return(getDALYs(x, bur = bur, dis = "all"))
})

#' @export
setMethod("getDALYs", signature(x = "ITHIM", bur = "missing", dis = "missing"), function(x){
    return(getDALYs(x, bur = "daly", dis = "all"))
})

#' @export
setMethod("getParameterSet", signature(x = "ITHIM"), function(x){
    return(x@parameters)
})

#' @export
setMethod("getMeans", signature(x = "ITHIM"), function(x){
    return(getMeans(getParameterSet(x)))
})

#' @export
setMethod("getGBD", signature(x = "ITHIM", format = "character"), function(x, format){
    if(format == "list"){
        return(x@parameters@GBD)
    }else if(format == "data.frame"){
        return(melt(x@parameters@GBD))
    }else{
        message("Error with getGBD format argument.")
        }
})

#' @export
setMethod("getGBD", signature(x = "ITHIM", format = "missing"), function(x){
    getGBD(x, format = "data.frame")
})
