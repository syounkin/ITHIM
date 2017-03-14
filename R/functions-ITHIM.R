#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Create an ITHIM object
###
### An ITHIM object is a list which contains three elements;
### parameters, means and quintiles.  The parameters are listed in
### \code{\link{createParameterList}}.  The elements means and
### quintiles mimic the computation presented in the original EXCEL
### workbook.  Use \code{\link{updateITHIM}} to change values of the
### parameters.
###
### @return A list of parameters, means and quintiles.
### @seealso \code{\link{updateITHIM}},
###     \code{\link{createParameterList}},
###     \code{\link{computeMeanMatrices}}, \code{\link{getQuintiles}}
###
###
createITHIMFunction <- function(roadInjuriesFile = system.file("roadInjuriesUS.csv", package = "ITHIM"), activeTransportTimeFile = system.file("activeTransportTime.csv",package = "ITHIM"), GBDFile = system.file("gbd.csv",package = "ITHIM")){
        new("ITHIM", parameters = parameters <- createParameterList(activeTransportTimeFile = activeTransportTimeFile, GBDFile = GBDFile), means = means <- computeMeanMatrices(as(parameters,"list")), quintiles = getQuintiles(means, as(parameters,"list")))

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Define ITHIM model parameters
###
### This function is used to generate a complete list of default
### parameters
###
### @return A list with parameters and estimates
###
###
### @seealso \code{\link{readGBD}}
###
###
createParameterList <- function(
                                roadInjuriesFile = system.file("roadInjuriesUS.csv", package = "ITHIM"),
                                activeTransportTimeFile = system.file("activeTransportTime.csv", package = "ITHIM"),
                                GBDFile = system.file("gbd.csv", package = "ITHIM")){

    nAgeClass <- 8L

    activeTransportTimeList <- readActiveTransportTime(activeTransportTimeFile)

    Mwt <- activeTransportTimeList$walk
    Mct <- activeTransportTimeList$cycle

    muwt <- Mwt[3,2] # min per week
    muct <- Mct[3,2] # min per week

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

    GBD <- readGBD(filename = GBDFile)

    F <- matrix(c(0.0000000,0.0000000,
                                  0.9715051,1.0354205,
                                  0.9505718,0.8999381,
                                  0.8315675,0.7180636,
                                  0.0000000,0.0000000,
                                  1.0000000,1.1171469,
                                  0.9878429,0.9434823,
                                  0.8782254,0.7737818),
                                ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")
))

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
        F = F,
        meanType = meanType,
        quantiles = quantiles,
        roadInjuries = roadInjuries,
        distRoadType = distRoadType,
        safetyInNumbers = sinMatrix
    ))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Updates an ITHIM object
###
### Change a parameter and recreate the object.
###
### @return An updated ITHIM object
###
###
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
