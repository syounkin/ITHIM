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
results <- data.frame()
wVec <- seq(0,210,length.out = n)
cVec <- wVec

for(muwt in wVec){
  for(muct in cVec){
    ITHIM.scenario <- updateITHIM(ITHIM.baseline, "muwt", muwt)
    ITHIM.scenario <- updateITHIM(ITHIM.scenario, "muct", muct)
    comparativeRisk <- data.frame(cycleTime = muct, walkTime= muwt, 
                                  DALYS = sumDALY(ITHIM.baseline, ITHIM.scenario), 
                                  BreastCancer= sumBreastCancer(ITHIM.baseline, ITHIM.scenario), 
                                  ColonCancer= sumColonCancer(ITHIM.baseline, ITHIM.scenario), 
                                  CVD = sumCVD(ITHIM.baseline, ITHIM.scenario), 
                                  Dementia = sumDementia(ITHIM.baseline, ITHIM.scenario), 
                                  Depression = sumDepression(ITHIM.baseline, ITHIM.scenario),
                                  Diabetes = sumDiabetes(ITHIM.baseline, ITHIM.scenario))
  
    results <- rbind(comparativeRisk, results)    
  }  
} 

p <- ggplot(results,aes(x = walkTime,y = cycleTime, fill = DALYS))
    p + geom_tile() + scale_fill_gradient2() + geom_hline(yintercept=baseCycle)+ geom_vline(xintercept=baseWalk)

})
