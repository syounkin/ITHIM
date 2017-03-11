#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "character", GBDFile = "character", roadInjuriesFile = "character"), function(activeTransportFile, GBDFile, roadInjuriesFile){
    ITHIM <- createITHIMFunction(activeTransportTimeFile = activeTransportFile, roadInjuriesFile = roadInjuriesFile, GBDFile = GBDFile)
    return(ITHIM)
})

#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "character", GBDFile = "character", roadInjuriesFile = "missing"), function(activeTransportFile, GBDFile, roadInjuriesFile){
    ITHIM <- createITHIMFunction(activeTransportTimeFile = activeTransportFile, GBDFile = GBDFile)
    return(ITHIM)
})

#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "character", GBDFile = "missing", roadInjuriesFile = "missing"), function(activeTransportFile, GBDFile, roadInjuriesFile){
    ITHIM <- createITHIMFunction(activeTransportTimeFile = activeTransportFile)
    return(ITHIM)
})

#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "character", GBDFile = "missing", roadInjuriesFile = "character"), function(activeTransportFile, GBDFile, roadInjuriesFile){
    ITHIM <- createITHIMFunction(activeTransportTimeFile = activeTransportFile, roadInjuriesFile = roadInjuriesFile)
    return(ITHIM)
})

#' @rdname createITHIM-methods
#' @aliases createITHIM
#' @export
setMethod("createITHIM", signature(activeTransportFile = "missing", GBDFile = "missing", roadInjuriesFile = "missing"), function(activeTransportFile, GBDFile, roadInjuriesFile){
    ITHIM <- createITHIMFunction()
    return(ITHIM)
})
