#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an ITHIM object
#'
#' An ITHIM object is a list which contains three elements;
#' parameters, means and quintiles.  The parameters are listed in
#' \code{\link{createParameterList}}.  The elements means and
#' quintiles mimic the computation presented in the original EXCEL
#' workbook.  Use \code{\link{updateITHIM}} to change values of the
#' parameters.
#'
#' @return A list of parameters, means and quintiles.
#' @seealso \code{\link{updateITHIM}},
#'     \code{\link{createParameterList}},
#'     \code{\link{computeMeanMatrices}}, \code{\link{getQuintiles}}
#'
#' @export
createITHIMFunction <- function(activeTransportTimeFile = system.file("activeTransportTime.csv",package = "ITHIM"), GBDFile = system.file("gbd.csv",package = "ITHIM")){

        new("ITHIM", parameters = parameters <- createParameterList(activeTransportTimeFile = activeTransportTimeFile, GBDFile = GBDFile), means = means <- computeMeanMatrices(as(parameters,"list")), quintiles = getQuintiles(means, as(parameters,"list")))

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Define ITHIM model parameters
#'
#' This function is used to generate a complete list of default
#' parameters
#'
#' @return A list with parameters and estimates
#'
#' \item{Rwt}{A numerical matrix for the walking time, relative to ?value?}
#' \item{Rct}{A numerical matrix for the cycling time, relative to ?value?}
#' \item{muwt}{A numerical value for the mean walking time}
#' \item{muct}{A numerical value for the mean cycling time}
#' \item{cv}{A numerical value for the coefficient of variation for active transport time}
#' \item{cvNonTravel}{A numerical value for the coefficient of variation for leisure activity}
#' \item{muNonTravel}{}
#' \item{muNonTravelMatrix}{}
#' \item{GBD}{}
#' \item{meanType}{}
#' \item{quantiles}{}
#'
#' @note There are 11 parameters in the ITHIM active transport component;
#'
#' 1-4. mean walking and cycling times (muwt, muct, Rwt, Rct)
#'
#' 5. standard deviation of active travel time (cv),
#'
#' 8. ratio of regional disease-specific mortality to national disease-specific mortality (GBD)
#'
#' 9-10. non-travel related physical activity means by age and sex (muNonTravel, muNonTravelMatrix)
#'
#' 11. standard deviation of leisure activity (cvNonTravel),
#'
#' @seealso \code{\link{readGBD}}
#'
#' @export
createParameterList <- function(
                                roadInjuriesFile = system.file("roadInjuries.csv", package = "ITHIM"),
                                activeTransportTimeFile = system.file("activeTransportTime.csv", package = "ITHIM"),
                                GBDFile = system.file("gbd.csv", package = "ITHIM")){

    nAgeClass <- 8L

    activeTransportTimeList <- readActiveTransportTime(activeTransportTimeFile)

    Mwt <- activeTransportTimeList$walk
    Mct <- activeTransportTimeList$cycle

    muwt <- Mwt[3,2] # min per week
    muct <- Mwt[3,2] # min per week

    Rwt <- Mwt/muwt
    Rct <- Mct/muct

    cv <- 3.0288 # coefficient of variation for active transport time

    muNonTravel <- 2 # MET-hrs./week leisure activity
    muNonTravelMatrix <- matrix(c(0.0000000,0.0000000,
                                  0.9715051,1.0354205,
                                  0.9505718,0.8999381,
                                  0.8315675,0.7180636,
                                  0.0000000,0.0000000,
                                  1.0000000,1.1171469,
                                  0.9878429,0.9434823,
                                  0.8782254,0.7737818),
                                ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")
                                                          ))
    cvNonTravel <- 1 # coefficient of variation for leisure activity
    roadInjuries <- array()
    modeNames <- unlist(unique(lapply(roadInjuries, rownames)))
    sinMatrix <- array()
    distRoadType <- array()
    inputDims <- list()

    GBD <- readGBD(file = GBDFile)

    meanType <- "referent"
    n <- 100 # percentiles instead of quintiles
    quantiles <- seq(1/n, (n-1)/n, by = 1/n)


    return( new("ParameterSet",
        Rwt = Rwt,
        Rct = Rct,
        muwt = muwt,
        muct = muct,
        cv = cv,
        cvNonTravel = cvNonTravel,
        nAgeClass = nAgeClass,
        muNonTravel = muNonTravel,
        muNonTravelMatrix = muNonTravelMatrix,
        GBD = GBD,
        meanType = meanType,
        quantiles = quantiles,
        roadInjuries = roadInjuries,
        distRoadType = distRoadType,
        safetyInNumbers = sinMatrix,
        inputDims = inputDims
    ))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Updates an ITHIM object
#'
#' Change a parameter and recreate the object.
#'
#' @return An updated ITHIM object
#'
#' @export
updateITHIM <- function( ITHIM, parName, parValue){

    ITHIM$parameters[[parName]] <- parValue
    parameters <- ITHIM$parameters
    means <- computeMeanMatrices(parameters)
    quintiles <- getQuintiles(means, parameters)
    ITHIM <- list(
            parameters = parameters,
            means = means,
            quintiles = quintiles
    )
    return(ITHIM)
    }
