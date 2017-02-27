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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Gets Change in Disease Burden due to Physical Activity
#'
#' Gets Change in Disease Burden due to Physical Activity
#'
#' @param x ITHIM object
#' @param bur ?
#' @param dis ?
#'
#' @return A burden value
#'
#' @export
setGeneric("deltaBurden", function(baseline, scenario, bur, dis) standardGeneric("deltaBurden"))
#' @export
setGeneric("createITHIM", function(x) standardGeneric("createITHIM"))
#' @export
setGeneric("getParameterSet", function(x) standardGeneric("getParameterSet"))
#' @export
setGeneric("getMeans", function(x) standardGeneric("getMeans"))
#' @export
setGeneric("getGBD", function(x, format = "data.frame") standardGeneric("getGBD"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Gets Road Injury counts from ITHIM object
#'
#' Gets Road Injury counts from ITHIM object
#'
#' @param x ITHIM object
#'
#' @return A list of matrices of road injury counts.
#'
#' @note The matrices have striking modes as rows and victim modes as
#'     columns.  NOV is considered a striking mode.
#' 
#' @export
setGeneric("getRoadInjuries", function(x) standardGeneric("getRoadInjuries"))
#' @export
setGeneric("getDistRoadType", function(x, format = "data.frame") standardGeneric("getDistRoadType"))
