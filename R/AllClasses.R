#' The MosaicsExperiment class
#'
#' This is the primary class in the MosaicsPipeline package.  It
#' contains all results and parameters for the entire pipeline run.
#'
#' This line and the next ones go into the details.
#' This line thus appears in the details as well.
#'
#'@section Slots:
#' \describe{
#' \item{\code{alignedFilesDir}:}{An object of class
#' \code{"character"}, with length one, that identifies the directory that contains the
#' aligned read files.}
#' \item{\code{repFiles}:}{Object of class \code{"character"},
#' containing the names of the aligned read files.  A typical
#' experiment will have three reps; rep0, rep1, and rep2}
#' \item{\code{inputFile}:}{Object of class \code{"character"}, with
#' length one, that identifies the name of the input file.}
#' \item{\code{alignedFileFormat}:}{Object of class \code{"character"}}
#' \item{\code{pairedEnd}:}{Object of class \code{"logical"}}
#' \item{\code{fragLen}:}{Object of class \code{"integer"}}
#' \item{\code{inputFragLen}:}{Object of class \code{"integer"}}
#' \item{\code{binSize}:}{Object of class \code{"integer"}}
#' \item{\code{capping}:}{Object of class \code{"integer"}}
#' \item{\code{genome}:}{Object of class \code{"BSgenome"}}
#' \item{\code{cores}:}{Object of class \code{"integer"}}
#' \item{\code{binFilesDir}:}{Object of class \code{"character"}}
#' \item{\code{repBinFiles}:}{Object of class \code{"character"}}
#' \item{\code{inputBinFile}:}{Object of class \code{"character"}}
#' \item{\code{binNames}:}{Object of class \code{"character"}}
#' \item{\code{MosaicsBinDataList}:}{Object of class \code{"MosaicsBinDataList"}}
#' \item{\code{BinDataList}:}{Object of class \code{"BinDataList"}}
#' \item{\code{MosaicsFitParameters}:}{Object of class \code{"list"}}
#' \item{\code{MosaicsFitList}:}{Object of class \code{"MosaicsFitList"}}
#' \item{\code{MosaicsFitHMMList}:}{Object of class \code{"MosaicsFitHMMList"}}
#' \item{\code{MosaicsPeakList}:}{Object of class \code{"MosaicsPeakList"}}
#' \item{\code{PeakSetList}:}{Object of class \code{"PeakSetList"}}
#' \item{\code{MosaicsSummitList}:}{Object of class \code{"MosaicsSummitList"}}
#' \item{\code{memeDir}:}{Object of class \code{"list"}}
#' \item{\code{fimoDir}:}{Object of class \code{"list"}}
#' \item{\code{scanWindow}:}{Object of class \code{"integer"}}
#' \item{\code{discoverWindow}:}{Object of class \code{"integer"}}
#' \item{\code{scanTop}:}{Object of class \code{"integer"}}
#' \item{\code{nRelaxed}:}{Object of class \code{"integer"}}
#' \item{\code{quantRelaxed}:}{Object of class \code{"numeric"}}
#' \item{\code{peakParams}:}{Object of class \code{"list"}}
#' \item{\code{idrParams}:}{Object of class \code{"list"}}
#' \item{\code{idrResults}:}{Object of class \code{"data.frame"}}
#' \item{\code{idrOptimalPeaks}:}{Object of class \code{"list"}}
#' \item{\code{relaxed}:}{Object of class \code{"logical"}}
#' \item{\code{type}:}{Object of class \code{"character"}}
#' \item{\code{fdr}:}{Object of class \code{"numeric"}}
#' \item{\code{thresType}:}{Object of class \code{"character"}}
#' \item{\code{SPPList}:}{Object of class \code{"SPPList"}}
#' \item{\code{enrichThres}:}{Object of class \code{"numeric"}}
#' \item{\code{minReadThres}:}{Object of class \code{"integer"}}
#' \item{\code{signal}:}{Object of class \code{"character"}}
#' }
#'
#' @name MosaicsExperiment
#' @rdname MosaicsExperiment
#' @aliases MosaicsExperiment-class
#' @exportClass MosaicsExperiment
setClass("MosaicsExperiment", representation(alignedFilesDir = "character",
                                             repFiles = "character",
                                             inputFile = "character",
                                             alignedFileFormat = "character",
                                             pairedEnd = "logical",
                                             fragLen = "integer",
                                             inputFragLen = "integer",
                                             binSize = "integer",
                                             capping = "integer",
                                             genome = "BSgenome",
                                             cores = "integer",
                                             binFilesDir = "character",
                                             repBinFiles = "character",
                                             inputBinFile = "character",
                                             binNames = "character",
                                             MosaicsBinDataList = "MosaicsBinDataList",
                                             BinDataList = "BinDataList",
                                             MosaicsFitParameters = "list",
                                             MosaicsFitList = "MosaicsFitList",
                                             MosaicsFitHMMList = "MosaicsFitHMMList",
                                             MosaicsPeakList = "MosaicsPeakList",
                                             PeakSetList = "PeakSetList",
                                             MosaicsSummitList = "MosaicsSummitList",
                                             memeDir = "list",
                                             fimoDir = "list",
                                             scanWindow = "integer",
                                             discoverWindow = "integer",
                                             scanTop = "integer",
                                             nRelaxed = "integer",
                                             quantRelaxed = "numeric",
                                             peakParams = "list",
                                             idrParams = "list",
                                             idrResults = "data.frame",
                                             idrOptimalPeaks = "list",
                                             relaxed = "logical",
                                             type = "character",
                                             fdr = "numeric",
                                             thresType = "character",
                                             SPPList = "SPPList",
                                             enrichThres = "numeric",
                                             minReadThres = "integer",
                                             signal = "character"
                                             ) )



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
