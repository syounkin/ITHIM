#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Update an ITHIM object
#'
#' Takes an ITHIM object as input and returns a new ITHIM object with
#' parameters updated accordingly.
#'
#' @param x ITHIM object or a ParameterSet object
#' @param parList A named list of parameters
#'
#' @return An ITHIM object
#'
#' @note For a list of parameter names use \code{getParameterNames}.
#'
#' @examples
#' ITHIM <- createITHIM()
#' getMeans(ITHIM)
#' ITHIM.new <- update(ITHIM, list(muwt = 10, muct = 5))
#' getMeans(ITHIM.new)
#'
#' @export
setGeneric("update", function(x, parList) standardGeneric("update"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Tile plot
#'
#' Creates a tile plot
#'
#' @param x ITHIM object
#' @param n A numeric value indicating the length of the walk and
#'     cycle vectors
#'
#' @return A figure created by ggplot2
#'
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
#' @param bur A character string indicating the type of disease burden measure.  Possible values are 
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Returns the names of the parameters in the ITHIM object
#'
#' Returns the names of the parameters in the ITHIM object
#'
#' @param x ITHIM object or a ParameterSet object
#'
#' @return A character vector of parameter names
#'
#' @export
setGeneric("getParameterNames", function(x) standardGeneric("getParameterNames"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an ITHIM object
#'
#' Returns an ITHIM object.  
#'
#' @param x A list of file names or a ParameterSet object
#'
#' @return An object of class ITHIM
#'
#' @note If run with no arguments this function will return the
#'     default ITHIM object
#'
#' @export
setGeneric("createITHIM", function(x) standardGeneric("createITHIM"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieve a ParameterSet object
#'
#' Returns the ParameterSet object embedded in an ITHIM object.
#'
#' @param x An ITHIM object
#'
#' @return An object of class ParameterSet
#'
#' @export
setGeneric("getParameterSet", function(x) standardGeneric("getParameterSet"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieve mean values
#'
#' Retrieves walking, cycling and nonTravel activity means from an
#' ITHIM object.
#'
#' @param x An ITHIM object
#'
#' @return A numeric vector of means.
#'
#' @export
setGeneric("getMeans", function(x) standardGeneric("getMeans"))
#' @export
setGeneric("getGBD", function(x, format = "data.frame") standardGeneric("getGBD"))
