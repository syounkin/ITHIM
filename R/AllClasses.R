#' The ITHIM ParameterSet class
#'
#' This class defines the set of parameters needed for an
#' ITHIM analysis.
#'
#'
#'
#'@section Slots:
#' \describe{
#' \item{\code{}:}{}
#' }
#'
#' @name ParameterSet
#' @rdname ParameterSet
#' @aliases ParameterSet-class
#' @exportClass ParameterSet
setClass("ParameterSet", representation(Rwt = "matrix",
                                        Rct = "matrix",
                                        muwt = "numeric",
                                        muct = "numeric",
                                        cv = "numeric",
                                        cvNonTravel = "numeric",
                                        nAgeClass = "integer",
                                        muNonTravel = "numeric",
                                        muNonTravelMatrix = "matrix",
                                        GBD = "list",
                                        meanType = "character",
                                        quantiles = "vector",
                                        roadInjuries = "array",
                                        distRoadType = "array",
                                        safetyInNumbers = "array"
                                        ))

#' The ITHIM class
#'
#' This is the primary class in the ITHIM package.
#'
#' This line and the next ones go into the details.
#' This line thus appears in the details as well.
#'
#'@section Slots:
#' \describe{
#' \item{\code{parameters}:}{}
#' \item{\code{means}:}{}
#' \item{\code{quintiles}:}{}
#' }
#'
#' @name ITHIM
#' @rdname ITHIM
#' @aliases ITHIM-class
#' @exportClass ITHIM
setClass("ITHIM", representation(parameters = "ParameterSet",
                                 means = "list",
                                 quintiles = "list"
                                 ))


#' The ITHIMList class
#'
#' Foo.
#'
#' This line and the next ones go into the details.
#' This line thus appears in the details as well.
#'
#' @name ITHIMList
#' @rdname ITHIMList
#' @aliases ITHIMList-class
#' @exportClass ITHIMList
setClass("ITHIMList",
         prototype = prototype(elementType = "ITHIM"),
         contains = "list")

## setClass("MosaicsBinDataList",
##          prototype = prototype(elementType = "BinData"),
##          contains = "list")

## setClass("BinDataList",
##          contains = "GRangesList")

## setClass("MosaicsFitList",
##          prototype = prototype(elementType = "MosaicsFit"),
##          contains = "list")

## setClass("MosaicsFitHMMList",
##          prototype = prototype(elementType = "MosaicsFitHMM"),
##          contains = "list")

## setClass("MosaicsPeakList",
##          prototype = prototype(elementType = "MosaicsPeak"),
##          contains = "list")

## setClass("PeakSetList",
##          contains = "GRangesList")

## setClass("MosaicsSummitList",
##          prototype = prototype(elementType = "GRanges"),
##          contains = "list")

## setClass("SPP",
##          prototype = prototype(elementType = "list"),
##          contains = "list")

## setClass("SPPList",
##          prototype = prototype(elementType = "SPP"),
##          contains = "list")

## setClassUnion("MosaicsBinDataListOrNull", c("MosaicsBinDataList", "NULL"))
## setClassUnion("MosaicsFitListOrNull", c("MosaicsFitList", "NULL"))
## setClassUnion("MosaicsPeakListOrNull", c("MosaicsPeakList", "NULL"))


## setClass("ReadsObject", representation(chips = "GRangesList",
##                                        input = "GRanges",
##                                        depth = "integer",
##                                        cap = "integer",
##                                        input.depth = "integer",
##                                        files = "character",
##                                        input.file = "character",
##                                        chr.length = "integer",
##                                        cores = "integer"
##                                              ) )
