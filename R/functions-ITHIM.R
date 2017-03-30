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
createITHIMFunction <- function(roadInjuriesFile = system.file("roadInjuries.csv", package = "ITHIM"),
                                activeTransportTimeFile = system.file("activeTransportTime.csv",package = "ITHIM"),
                                GBDFile = system.file("gbd.csv",package = "ITHIM"),
                                FFile = system.file("F.csv",package = "ITHIM"),
                                meanType = "overall"){

    new("ITHIM", parameters = parameters <- createParameterList(activeTransportTimeFile = activeTransportTimeFile,
                                                                GBDFile = GBDFile,
                                                                FFile = FFile,
                                                                meanType = meanType),
        means = means <- computeMeanMatrices(as(parameters,"list")),
        quintiles = getQuintiles(means, as(parameters,"list")))

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
createParameterList <- function(roadInjuriesFile = system.file("roadInjuries.csv", package = "ITHIM"),
                                activeTransportTimeFile = system.file("activeTransportTime.csv",package = "ITHIM"),
                                GBDFile = system.file("gbd.csv",package = "ITHIM"),
                                FFile = system.file("F.csv",package = "ITHIM"),
                                meanType = "overall"){

    nAgeClass <- 8L

    activeTransportTimeList <- readActiveTransportTime(activeTransportTimeFile)

    Mwt <- activeTransportTimeList$walk
    Mct <- activeTransportTimeList$cycle

    Rwt <- Mwt/Mwt[3,2]
    Rct <- Mct/Mct[3,2]

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

    F <- readF(filename = FFile)

    if( meanType == "referent" ){
        muwt <- Mwt[3,2]
        muct <- Mct[3,2]
    }else if( meanType == "overall" ){
        muwt <- sum(F*Mwt)
        muct <- sum(F*Mct)
    }else{
        stop("Wrong meanType value.")
    }

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
readF <- function(filename){
    F <- read.csv(file = filename, header = TRUE, stringsAsFactors = FALSE)

    if(!all(colnames(F)==c("ageClass","sex","value"))){
        stop("Column names for F file must be ageClass, sex and value.")
    }

    foo <- split(F,F$sex)

    F <- matrix(c(foo$M$value,foo$F$value),ncol = 2, dimnames = list(foo$F$ageClass,c("M","F")))

    return(F)
}
