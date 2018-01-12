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
createITHIMFunction <- function(roadInjuriesFile = system.file("roadInjuriesUS.csv", package = "ITHIM"),
                                activeTransportTimeFile = system.file("activeTransport.portland.csv", package = "ITHIM"),
                                GBDFile = system.file("burden.portland.csv", package = "ITHIM"),
                                distRoadTypeFile = system.file("distByRoadTypeBaseline.csv", package = "ITHIM"),
                                safetyInNumbersFile = system.file("SiN.csv", package = "ITHIM"),
                                FFile = system.file("F.portland.csv",package = "ITHIM"),
                                meanType = "overall",
                                EXCEL = TRUE){

    new("ITHIM", parameters = parameters <- createParameterList(
                                  activeTransportTimeFile = activeTransportTimeFile,
                                  distRoadTypeFile = distRoadTypeFile,
                                  safetyInNumbersFile = safetyInNumbersFile,
                                  GBDFile = GBDFile,
                                  FFile = FFile,
                                  meanType = meanType,
                                  EXCEL = EXCEL),
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
createParameterList <- function(
                                roadInjuriesFile = system.file("roadInjuriesUS.csv", package = "ITHIM"),
                                activeTransportTimeFile = system.file("activeTransport.portland.csv", package = "ITHIM"),
                                GBDFile = system.file("burden.portland.csv", package = "ITHIM"),
                                FFile = system.file("F.portland.csv", package = "ITHIM"),
                                distRoadTypeFile = system.file("distByRoadTypeBaseline.csv", package = "ITHIM"),
                                safetyInNumbersFile = system.file("SiN.csv", package = "ITHIM"),
                                meanType = "overall", EXCEL = TRUE){

    nAgeClass <- 8L

    activeTransportTimeList <- readActiveTransportTime(activeTransportTimeFile)

    Mwt <- activeTransportTimeList$walk
    Mct <- activeTransportTimeList$cycle

    Rwt <- Mwt/Mwt[3,2]
    Rct <- Mct/Mct[3,2]

    cv <- 1.65 # coefficient of variation for active transport time

    muNonTravel <- mean(c(500,1000)/60) # MET-hrs./week leisure activity


    muNonTravelMatrix <- matrix(1,ncol = 2, nrow = 8, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    ## muNonTravelMatrix <- matrix(c(0.0000000,0.0000000,
    ##                               0.9715051,1.0354205,
    ##                               0.9505718,0.8999381,
    ##                               0.8315675,0.7180636,
    ##                               0.0000000,0.0000000,
    ##                               1.0000000,1.1171469,
    ##                               0.9878429,0.9434823,
    ##                               0.8782254,0.7737818),
    ##                             ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")
    ##                                                      ))
    cvNonTravel <- cv # coefficient of variation for leisure activity
    roadInjuries <- array()
    modeNames <- unlist(unique(lapply(roadInjuries, rownames)))

    # read safetyInNumbers using helper function which converts "normalize" csv into array

    safetyInNumbers <- readSafetyInNumbers(file = safetyInNumbersFile)

    # read distRoadType using helper function which converts "normalize" csv into array

    distRoadType <- readDistByRoadType(file = distRoadTypeFile)

    GBD <- readGBD(filename = GBDFile)

    N <- readF(filename = FFile)
    P <- N/sum(N)

    if( meanType == "referent" ){
        muwt <- Mwt[3,2]
        muct <- Mct[3,2]
    }else if( meanType == "overall" ){
        muwt <- sum(P*Mwt)
        muct <- sum(P*Mct)
    }else{
        stop("Wrong meanType value.")
    }

    n <- 100 # percentiles instead of quintiles
    quantiles <- seq(1/n, (n-1)/n, by = 1/n)

    EXCEL <- TRUE

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
        F = N,
        meanType = meanType,
        quantiles = quantiles,
        roadInjuries = roadInjuries,
        distRoadType = distRoadType,
        safetyInNumbers = safetyInNumbers,
        EXCEL = EXCEL
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tabulateDeltaBurdenFunction <- function(ITHIM.baseline, ITHIM.scenario){

    deltaBurdenWrapper <- function( bur, dis ){
        burden <- deltaBurden(baseline = ITHIM.baseline, scenario = ITHIM.scenario, bur = bur, dis = dis, type = "percent")
        return(burden)
    }


    results <- data.frame()
    disVec <- c("BreastCancer","ColonCancer","CVD","Dementia","Depression","Diabetes")
    burVec <- c("deaths", "daly", "yll", "yld")
    for( dis in disVec ){
        for( bur in burVec ){
            results <- rbind(results,data.frame(bur,dis,(deltaBurdenWrapper(bur, dis))))
        }}

    names(results) <- c("burdenType","disease","percentDeltaBurden")

    return(results)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Update an ITHIM object
#'
#' Takes an ITHIM object as input and returns a new ITHIM object with
#' parameters updated accordingly.
#'
#' @param ITHIM.baseline Baseline
#' @param ITHIM.scenario.list An ITHIM list
#'
#' @return A data frame of change in burden
#'
#' @export
tabulateResults <- function(ITHIM.baseline, ITHIM.scenario.list){

    results <- data.frame()
    vision.names <- names(ITHIM.scenario.list)
    i <- 1
    for( ITHIM.scenario in ITHIM.scenario.list ){
        vision <- vision.names[i]
        i <- i + 1
        for( bur in c("daly","yll", "yld", "deaths") ){
            for(dis in c("BreastCancer", "ColonCancer","Depression", "Dementia", "Diabetes","CVD")){
                percent <- 100*deltaBurden(ITHIM.baseline, ITHIM.scenario, bur = bur, dis = dis, type = "percent")
                results <- rbind(results, data.frame(vision = vision, bur,dis,percent))
            }
        }
    }
    results <- results %>% spread(vision, percent) %>% arrange(bur, dis)
    return(results)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Tabulate the results of Comparative Risk Assessment
#'
#' Given an ITHIM baseline object and a list of ITHIM scenario objects
#' this function returns a data frame of change in burden (absolute,
#' not percent)
#'
#' @param ITHIM.baseline Baseline
#' @param ITHIM.scenario.list An ITHIM list
#'
#' @return A data frame of change in burden
#'
#' @export
superTabulate <- function(ITHIM.baseline, ITHIM.scenario.list){
    results <- data.frame()
    scenarioNames <- names(ITHIM.scenario.list)
    if(is.null(scenarioNames)){
        scenarioNames <- paste0("scenario",1:length(ITHIM.scenario.list))
        message("Try using a named list of scenario objects.  Otherwise the scenarios will be named scenario1, scenario2, scenario3...")
    }
    i <- 1
    for( ITHIM.scenario in ITHIM.scenario.list ){

        foo <- ITHIM:::compareModels(ITHIM.baseline, ITHIM.scenario)

        for (burden in c("deaths.delta","daly.delta","yll.delta","yld.delta")){

            foobar <- foo[[burden]] %>% melt()
            names(foobar) <- c("value","sex", "disease")
            foobar <- data.frame(foobar, ageClass = paste0("ageClass",1:8), burdenType = gsub(".delta","",burden), vision = scenarioNames[i])
            foobar <- foobar %>% select(vision, disease, sex, ageClass, burdenType, value)
            results <- rbind(results, foobar)
        }
        i <- i+1
    }
    return(results)
}

