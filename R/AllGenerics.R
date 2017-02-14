#' @export
setGeneric("update", function(x, parList) standardGeneric("update"))
#' @export
setGeneric("tilePlot", function(x, n) standardGeneric("tilePlot"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Gets Burden
#'
#' Gets burden
#'
#' @param x ITHIM object
#' @param bur ?
#' @param dis ?
#'
#' @return A burden value
#'
#' @export
setGeneric("getBurden", function(x, bur, dis) standardGeneric("getBurden"))
#' @export
setGeneric("createITHIM", function(x) standardGeneric("createITHIM"))
#' @export
setGeneric("getParameterSet", function(x) standardGeneric("getParameterSet"))
#' @export
setGeneric("getMeans", function(x) standardGeneric("getMeans"))
#' @export
setGeneric("getGBD", function(x, format = "data.frame") standardGeneric("getGBD"))
#' @export
setGeneric("getRoadInjuries", function(x) standardGeneric("getRoadInjuries"))
#' @export
setGeneric("getDistRoadType", function(x, format = "data.frame") standardGeneric("getDistRoadType"))
