#' The ITHIM ParameterSet class
#'
#' This class defines the set of parameters needed for an
#' ITHIM analysis.  See ITHIM class for more information.
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
                                        F = "matrix",
                                        meanType = "character",
                                        quantiles = "vector",
                                        roadInjuries = "array",
                                        distRoadType = "array",
                                        safetyInNumbers = "array",
                                        EXCEL = "logical"
                                        ))
#' The ITHIM class
#'
#' This class defines an ITHIM object.
#'
#' Most importantly, an ITHIM object contains a ParameterSet object
#' which contains the set of parameters needed to build the ITHIM
#' model.  Once an ITHIM object is created methods for the three
#' components, physical activity, road injuries and air pollution, can
#' be used in a modular fashion to compare baseline and scenario
#' models.
#'
#'@section Parameter List:
#' \describe{
#' \item{Rwt}{A numerical matrix for the walking time, relative to females in ageClass3}
#' \item{Rct}{A numerical matrix for the cycling time, relative to females in ageClass3}
#' \item{muwt}{A numerical value for the mean walking time}
#' \item{muct}{A numerical value for the mean cycling time}
#' \item{cv}{A numerical value for the coefficient of variation for active transport time}
#' \item{cvNonTravel}{A numerical value for the coefficient of variation for leisure activity}
#' \item{nAgeClass}{THe number of age classes.  Cannot currently be changed, but we hope to make the age classes dynamic sometime.}
#' \item{muNonTravel}{A numerical vale for the mean non-travel physical activity time}
#' \item{muNonTravelMatrix}{A numerical matrix for the non-travel physical activity time (min./week), relative to females in ageClass3}
#' \item{GBD}{A list of lists of data.frames.  This should be improved.  More to come.}
#' \item{meanType}{Either "overall" or "referent".  If the value is referent than the means given above, muwt, muct and muNonTravel are for females in ageClass3.  If the value is overall than it is the oveall population mean and F, the population density is needed.  The option "overall" is therefore defunct right now, as F is not included.}
#' \item{quantiles}{A numeric vector of quantiles to be used in the population attributable fraction approximation.  The default value is to use percentiles, i.e., 0.01, 0.02,..., 0.99.}
#' \item{roadInjuries}{A list of data.frames.  One data.frame for each of "Fatalmajor","FatalmotorOnly","Fatalminor","Seriousmajor", "SeriousmotorOnly", "Seriousminor".  The data frames contain counts stratified by striking and victim vehicles.}
#' \item{distRoadType}{Soon to be defunct.}
#' \item{safetyInNumbers}{A matrix of safety in number parameters.  Rows are vehicle mode, columns are victim, striking, NOV.  More details to come.}
#' }
#'
#' @note There are 9 parameters in the ITHIM physical activity
#'     component. (Many of which are matrices of parameters);
#'
#' 1-4. mean walking and cycling times (muwt, muct, Rwt, Rct)
#'
#' 5. standard deviation of active travel time (cv),
#'
#' 6. ratio of regional disease-specific mortality to national disease-specific mortality (GBD)
#'
#' 7-8. non-travel related physical activity means (muNonTravel, muNonTravelMatrix)
#'
#' 9. standard deviation of leisure activity (cvNonTravel),
#'
#' @name ITHIM-class
#' @rdname ITHIM
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
