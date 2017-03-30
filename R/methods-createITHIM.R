#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "character", GBDFile = "character", roadInjuriesFile = "character", distRoadTypeFile = "character", safetyInNumbersFile = "character"), function(activeTransportFile, GBDFile, roadInjuriesFile, distRoadTypeFile, safetyInNumbersFile){
    ITHIM <- createITHIMFunction(activeTransportTimeFile = activeTransportFile, roadInjuriesFile = roadInjuriesFile, GBDFile = GBDFile, distRoadTypeFile = distRoadTypeFile, safetyInNumbersFile = safetyInNumbersFile)
    return(ITHIM)
})

#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "character", GBDFile = "character", roadInjuriesFile = "missing", distRoadTypeFile = "character", safetyInNumbersFile = "character"), function(activeTransportFile, GBDFile, roadInjuriesFile, distRoadTypeFile, safetyInNumbersFile){
    ITHIM <- createITHIMFunction(activeTransportTimeFile = activeTransportFile, GBDFile = GBDFile, distRoadTypeFile = distRoadTypeFile, safetyInNumbersFile = safetyInNumbersFile)
    return(ITHIM)
})

#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "character", GBDFile = "missing", roadInjuriesFile = "missing", distRoadTypeFile = "character", safetyInNumbersFile = "character"), function(activeTransportFile, GBDFile, roadInjuriesFile, distRoadTypeFile, safetyInNumbersFile){
    ITHIM <- createITHIMFunction(activeTransportTimeFile = activeTransportFile, distRoadTypeFile = distRoadTypeFile, safetyInNumbersFile = safetyInNumbersFile)
    return(ITHIM)
})

#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "character", GBDFile = "missing", roadInjuriesFile = "character", distRoadTypeFile = "character", safetyInNumbersFile = "character"), function(activeTransportFile, GBDFile, roadInjuriesFile, distRoadTypeFile, safetyInNumbersFile){
    ITHIM <- createITHIMFunction(activeTransportTimeFile = activeTransportFile, roadInjuriesFile = roadInjuriesFile, distRoadTypeFile = distRoadTypeFile, safetyInNumbersFile = safetyInNumbersFile)
    return(ITHIM)
})

#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "missing", GBDFile = "missing", roadInjuriesFile = "missing", distRoadTypeFile = "missing", safetyInNumbersFile = "missing"), function(activeTransportFile, GBDFile, roadInjuriesFile, distRoadTypeFile, safetyInNumbersFile){
    ITHIM <- createITHIMFunction()
    return(ITHIM)
})
