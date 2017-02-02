#' Integrated Transport and Health Impacts Model (ITHIM)
#'
#' ITHIM is a mathematical model that integrates data on travel
#' patterns, physical activity, fine particulate matter, GHG
#' emissions, and disease and injuries. Based on population and travel
#' scenarios. The model has been used to calculate the health impacts
#' of walking and bicycling short distances usually traveled by car or
#' driving low-emission automobiles.  Please Cite: Woodcock, J.,
#' Givoni, M., & Morgan, A. S. (2013). Health impact modelling of
#' active travel visions for England and Wales using an Integrated
#' Transport and Health Impact Modelling Tool (ITHIM). PLoS One, 8(1),
#' e51462. and Maizlish, N., Woodcock, J., Co, S., Ostro, B., Fanai,
#' A., & Fairley, D. (2013). Health cobenefits and
#' transportation-related reductions in greenhouse gas emissions in
#' the San Francisco Bay area. American Journal of Public Health,
#' 103(4), 703-709.)
#'
#' The model uses comparative risk assessment through which it
#' formulates a change in the disease burden, resulting from the shift
#' in the exposure distribution from a baseline scenario to an
#' alternative scenario.
#'
#' ITHIM characterizes exposure distributions in several ways:
#'
#' -- Physical Activity --
#' Described as quintiles of a log-normal distribution on the basis of
#' the mean weekly active transport time per person, its standard
#' deviation and coefficient of variation (the standard deviation
#' divided by the mean), mean weekly non-transport physical activity,
#' and the ratio between bicycling and walking times. The activity
#' times were multiplied by weights to give metabolic-equivalent task
#' hours (METS), which reflect energy expenditures for walking and
#' cycling at average speeds and for performing occupational tasks.
#'
#' Descriptive statistics were obtained from published research on
#' walking and bicycling speeds and analysis of travel and health
#' surveys with large probability samples for the Bay Area.
#'
#'
#' -- Air Pollution --
#' To estimate exposure to air pollution, they used
#' population-weighted means of airborne fine particulate matter
#' (PM2.5), based on models calibrated for Bay Area automobile
#' emissions and air shed. The RR-PM2.5 gradient in the comparative
#' risk assessment analysis reflected the change in risk over an
#' increment of 10 micrograms per cubic meter PM2.5.
#'
#' -- Traffic Injuries --
#' Data on injuries was extracted from from a geocoded collision
#' database of fatal and serious collisions reported to police.
#'
#' Roadway type: determined roadway type associated with the collision
#' by a spatial join in mapping software (ArcGIS 10, ESRI, Redlands,
#' CA) to a street layer and categorized it as highway, arterial, or
#' local on the basis of federal and state classifications of facility
#' type.
#'
#' Daily distances walked, bicycled, and driven by drivers and
#' passengers of cars, buses, and rail from geocoded coordinates of
#' trip origins and estimations recorded in diaries of participants of
#' the 2000 Bay Area Travel Survey.
#'
#' @name ITHIM-package
#' @docType package
#' @author Samuel G. Younkin \email{syounkin@@wisc.edu}
#' @references \url{http://www.cedar.iph.cam.ac.uk/research/modelling/ithim/}, \url{https://ithim.ghi.wisc.edu/}
#' @seealso \code{\link{createITHIM}}, \code{\link{compareModels}}
#' @examples
#'
#' ITHIM.baseline <- createITHIM()
#' ITHIM.scenario <- updateITHIM(ITHIM.baseline, "muwt", 120)
#' comparativeRisk <- compareModels(ITHIM.baseline, ITHIM.scenario)
#' names(ITHIM.baseline)
#' names(ITHIM.baseline$parameters)
#' names(comparativeRisk)
#' comparativeRisk$AF$BreastCancer
NULL
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
createITHIMFunction <- function(){

        new("ITHIM", parameters = parameters <- createParameterList(), means = means <- computeMeanMatrices(as(parameters,"list")), quintiles = getQuintiles(means, as(parameters,"list")))

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
#' \item{Rws}{A numerical matrix for the walking speed, relative to ?value?}
#' \item{muwt}{A numerical value for the mean walking time}
#' \item{muws}{A numerical value for the mean walking speed}
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
#' 6-7. walk speed (muws, Rws)
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
createParameterList <- function(){

    nAgeClass <- 8L

    Rwt <- matrix(c(0.4305,0.3471,1.0700,0.8200,1.0100,1.0000,0.8600,1.1700,1.0600,1.1700,0.9900,0.9200,0.8000,0.7500,0.8200,0.7800),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rct <- matrix(c(0.2935,0.1231,6.4500,3.1500,4.0000,1.0000,3.4800,0.8200,4.6700,1.1800,2.7000,0.6100,3.4200,0.2100,0.7000,0.0900),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rws <- matrix(c(1.0662510447,0.8753344725,1.0662510447,0.8753344725,1.0206231847,1.000210721,1.0590466458,1.0338312494,1.0392345486,0.947378462,1.03022905,0.9329696641,0.9509806615,0.8969476694,0.9509806615,0.8969476694),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    muNonTravelMatrix <- matrix(c(0.0000000,0.0000000,0.9715051,1.0354205,0.9505718,0.8999381,0.8315675,0.7180636,0.0000000,0.0000000,1.0000000,1.1171469,0.9878429,0.9434823,0.8782254,0.7737818), ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    meanType <- "referent"
    n <- 100
    quantiles <- seq(1/n, (n-1)/n, by = 1/n)
    GBDFile <- "gbd.csv"
    GBD <- readGBD(file = GBDFile)
    muwt <- 47.3900 # min per week
    muws <- 2.7474 # mph
    muct <- 6.1600 # min per week
    cv <- 3.0288 # coefficient of variation for active transport time

    muNonTravel <- 2 # MET-hrs./week leisure activity
    cvNonTravel <- 1 # coefficient of variation for leisure activity

    filename <- system.file( "roadInjuries.csv", package = "ITHIM")
 roadInjuries <- read.csv(file = filename, header = FALSE)

roadInjuries <- rbind(roadInjuries,rep(NA,9))
roadInjuries <- split(roadInjuries, c(t(matrix(1:6, nrow = 6, ncol = 8))))
    names(roadInjuries) <- c("FatalLocal","FatalArterial","FatalHighway","SeriousLocal","SeriousArterial","SeriousHighway")
    roadInjuries <- lapply(roadInjuries,function(x){dimnames(x) <- list(c("walk","cycle","bus","car","HGV","LGV","mbike","ebike"),c("walk","cycle","bus","car","HGV","LGV","mbike","ebike","NOV"));x})

    distRoadType <- list()

    modeNames <- c("walk","cycle","bus","car","HGV","LGV","mbike","ebike")
sinMatrix <- matrix(0.5, nrow = length(modeNames), ncol = length(modeNames), dimnames = list(modeNames, modeNames))

    return( new("ParameterSet",
        Rwt = Rwt,
        Rct = Rct,
        Rws = Rws,
        muwt = muwt,
        muws = muws,
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
        safetyInNumbers = sinMatrix
    ))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compute Matrices of Active Transport Means
#'
#' This function computes mean walking/cycling time/speed, as well as
#' active transport METs mean and standard deviation
#'
#' @param parList The list of parameters generated by
#'     \code{\link{createParameterList}}
#'
#' @return A list with matrices of means
#'
#' \item{meanWalkTime}{A numerical matrix of mean weekly time (hours?) for walking as transport}
#' \item{meanWalkSpeed}{A numerical matrix of mean weekly speed for walking as transport}
#' \item{meanCycleTime}{A numerical matrix of mean weekly time for cycling as transport}
#' \item{meanWalkMET}{A numerical matrix of mean weekly METs for walking as transport}
#' \item{meanCycleMET}{A numerical matrix of mean weekly METs for cycling as transport}
#' \item{meanActiveTransportTime}{A numerical matrix containing mean weekly active transport time}
#' \item{sdActiveTransportTime}{A numerical matrix containing standard deviation of weekly active transport time}
#' \item{propTimeCycling}{The proportion of time walking out of walking or cycling as active transport}
#'
#' @note Currently all age by sex classes are assigned 6 for weekly
#'     cycling for transport METs.  This means we assume that, unlike
#'     walking, cycling energy is not a function of speed.
#'
#' @note meanCycleMET is constant.  So, it's really a parameter and not a function of parameters.
#' @note cycling speed has been removed
#' @note We use a constant coefficient of variation across strata to compute standard deviations
#' @seealso \code{\link{createITHIM}}
#'
#' @export
computeMeanMatrices <- function(parList){

    with(parList, {
        if( meanType == "overall" ){
            alphawt <- sum(F*Rwt)
            alphact <- sum(F*Rct)
            alphaws <- sum(F*Rws)
            meanWalkTime <- muwt/alphawt*Rwt
            meanCycleTime <- muct/alphact*Rct
            meanWalkSpeed <- muws/alphaws*Rws
        }else if( meanType == "referent" ){
            meanWalkTime <- muwt*Rwt
            meanCycleTime <- muct*Rct
            meanWalkSpeed <- muws*Rws
        }else{
            message("Wrong mean type.")
        }
        propTimeCycling <-  meanCycleTime/(meanCycleTime+meanWalkTime)
        meanActiveTransportTime <- meanWalkTime + meanCycleTime
        sdActiveTransportTime <- meanActiveTransportTime*cv
        pWalk <- 1 - propTimeCycling #meanWalkTime/(meanWalkTime + meanCycleTime)

        return(list(meanWalkTime = meanWalkTime, meanCycleTime = meanCycleTime, meanWalkSpeed = meanWalkSpeed, meanActiveTransportTime = meanActiveTransportTime, sdActiveTransportTime = sdActiveTransportTime, propTimeCycling = propTimeCycling, pWalk = pWalk)) # meanWalkMET = meanWalkMET, meanCycleMET = meanCycleMET,
        })
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compute Quintiles of Active Transport Time
#'
#' Compute Quintiles of Active Transport Time
#'
#' @param means A list of means generated by the \code{\link{computeMeanMatrices}}
#' @param parameters A list of parameters as created with \code{\link{createParameterList}}
#'
#' @return A list of lists containing quintiles of active transport
#'     time and METs, by sex and age class.
#'
#' \item{ActiveTransportTime}{Foo}
#' \item{WalkingTime}{Foo}
#' \item{CyclingTime}{Foo}
#'
#' @seealso \code{\link{computeQuintiles}}
#'
#' @export
getQuintiles <- function(means, parameters){

  ActiveTransportTime <- computeQuintiles(means$meanActiveTransportTime, means$sdActiveTransportTime, parameters$quantiles)
  WalkingTime <- list(M = ActiveTransportTime[["M"]] * (1-means$propTimeCycling[,"M"]), F = ActiveTransportTime[["F"]] * (1-means$propTimeCycling[,"F"]))
  CyclingTime <- list(M = ActiveTransportTime[["M"]] * (means$propTimeCycling[,"M"]), F = ActiveTransportTime[["F"]] * (means$propTimeCycling[,"F"]))
  #WalkingMET <- list(M = means$meanWalkMET[,"M"]*WalkingTime[["M"]]/60, F = means$meanWalkMET[,"F"]*WalkingTime[["F"]]/60)
  #CyclingMET <- list(M = means$meanCycleMET[,"M"]*CyclingTime[["M"]]/60, F = means$meanCycleMET[,"F"]*CyclingTime[["F"]]/60)
  #TravelMET <- list(M = WalkingMET[["M"]] + CyclingMET[["M"]], F = WalkingMET[["F"]] + CyclingMET[["F"]])

  muNonTravel <- parameters$muNonTravel
  muNonTravelMatrix <- parameters$muNonTravelMatrix

  TotalMETSample <- mapply(getTotalDistribution,
                                 muTravel = means$meanActiveTransportTime,
                                 cvTravel = parameters$cv,
                                 muNonTravel = muNonTravelMatrix*muNonTravel,
                                 cvNonTravel = parameters$cvNonTravel,
                                 pWalk = means$pWalk, # parameters$pWalk
                                 vWalk = means$meanWalkSpeed,
                                 size = 1e5, SIMPLIFY = FALSE)
  TotalMETQuintiles <- lapply(TotalMETSample,function(x) quantile(x, parameters$quantiles, na.rm = TRUE))

  TotalMET <- list( M = matrix(unlist(TotalMETQuintiles[1:8]),ncol = length(parameters$quantiles), byrow = TRUE), F = matrix(unlist(TotalMETQuintiles[9:16]),ncol = length(parameters$quantiles), byrow = TRUE ) )

  TotalMET <- mapply(function(x,y) ifelse(x < 0.1, 0.1, x), TotalMET, SIMPLIFY=FALSE)

  #TotalMET <- mapply(function(x,y) ifelse(x+y<2.5,0.1,x+y),TravelMET,parameters$NonTravelMETs,SIMPLIFY=FALSE) # This is the old way of doing things.

 return(list(ActiveTransportTime=ActiveTransportTime, WalkingTime=WalkingTime, CyclingTime=CyclingTime, TotalMET = TotalMET)) # WalkingMET=WalkingMET, CyclingMET = CyclingMET, TravelMET = TravelMET,
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compute Quintiles of the Lognormal Distribution
#'
#' Compute quintiles of the lognormal distribution given matrices for
#' mean and standard deviation of active transport time.  This
#' function is used by the function \code{\link{getQuintiles}}
#'
#' @param mean A numerical matrix of means for active transport time
#' @param sd A numerical matrix of standard deviations for active
#'     transport time
#'
#' @return A vector of quintiles
#'
#' @note This function needs to be cleaned up so it is more user
#'     friendly
#' @note Quintiles are defined as 10, 30, 50, 70, 90 percentiles
#'
#' @seealso \code{\link{getQuintiles}}
#'
#' @export
computeQuintiles <- function( mean, sd, quantiles ){

    nAgeClass <- nrow(mean)
    ncol <- length(quantiles)

    logMean <- log(mean)-1/2*log(1+(sd/mean)^2)
    logSD <- sqrt(log(1+(sd/mean)^2))

    quintVec <- c()

    for( quant in quantiles ){

        quintVec <- c(quintVec, mapply(qlnorm, logMean, logSD, p = quant))

    }

    quintMat <- matrix(quintVec, nrow = 2*nAgeClass, ncol = ncol, dimnames = list(paste0("ageClass", rep(1:nAgeClass,2)),paste0("q",1:ncol)))

    quintList = list(M = quintMat[1:nAgeClass,], F = quintMat[nAgeClass+1:8,])

    return(quintList)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set Risk Ratios for Avctive Transport
#'
#' Set risk ratios for a list of diseases given MET exposure.  These
#' values are used to compute change in disease burden due to active
#' transport increase.
#'
#' @return A numerical vector of risk ratios given MET exposure
#'
#' @note To see the default values and how they are computed run
#'     \code{createActiveTransportRRs} with no parentheses
#'
#' @seealso \code{\link{compareModels}}
#'
#' @export
createActiveTransportRRs <- function(nQuantiles = 5){

    diseaseNames <- c("BreastCancer","ColonCancer","CVD","Dementia","Depression","Diabetes")
    nAgeClass <- 8

    RR.lit <- exposure <- rep(list((matrix(NA,nrow=nAgeClass,ncol=2,dimnames=list(paste0("agClass",1:nAgeClass),c("F","M"))))), length(diseaseNames))

    names(RR.lit) <- names(exposure) <- diseaseNames

    exposure[["BreastCancer"]][1:nAgeClass,"F"] <- 4.5
    RR.lit[["BreastCancer"]][1:nAgeClass,"F"] <- 0.944

    exposure[["BreastCancer"]][1:nAgeClass,"M"] <- 1
    RR.lit[["BreastCancer"]][1:nAgeClass,"M"] <- 1

    exposure[["ColonCancer"]][1:nAgeClass,"M"] <- 30.9
    RR.lit[["ColonCancer"]][1:nAgeClass,"M"] <- 0.8

    exposure[["ColonCancer"]][1:nAgeClass,"F"] <- 30.1
    RR.lit[["ColonCancer"]][1:nAgeClass,"F"] <- 0.86

    exposure[["CVD"]][1:nAgeClass,1:2] <- 7.5
    RR.lit[["CVD"]][1:nAgeClass,1:2] <- 0.84

    exposure[["Dementia"]][1:nAgeClass,1:2] <- 31.5
    RR.lit[["Dementia"]][1:nAgeClass,1:2] <- 0.72

    exposure[["Diabetes"]][1:nAgeClass,1:2] <- 10
    RR.lit[["Diabetes"]][1:nAgeClass,1:2] <- 0.83

    exposure[["Depression"]][1:3,1:2] <- 11.25
    RR.lit[["Depression"]][1:3,1:2] <- 0.927945490148335

    exposure[["Depression"]][4:nAgeClass,1:2] <- 11.25
    RR.lit[["Depression"]][4:nAgeClass,1:2] <- 0.859615572255727

    exposure[["Stroke"]] <- exposure[["CVD"]]
    RR.lit[["Stroke"]] <- RR.lit[["CVD"]]

    exposure[["HHD"]] <- exposure[["CVD"]]
    RR.lit[["HHD"]] <- RR.lit[["CVD"]]

    k <- 0.5
    RR <- mapply(function(x,y,k) x^(1/y)^k, RR.lit, exposure, 0.5, SIMPLIFY=FALSE)
    RR <- lapply(RR, reshapeRR, nQuantiles = nQuantiles)

    return(RR)

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Estimates change in disease burden
#'
#' Performs the ITHIM model analysis using two ITHIM objects; baseline
#' and scenario.  These objects were created with
#' \code{\link{createITHIM}} and updated with
#' \code{\link{updateITHIM}}.
#'
#'
#' @param baseline An ITHIM object created with
#'     code{\link{createITHIM}} representing the baseline
#'
#' @param scenario An ITHIM object representing the scenario
#'
#' @return A list of estimates from the ITHIM model
#'
#' \item{RR.baseline}{Baseline relative risk compared with no exposure}
#' \item{RR.scenario}{Scenario relative risk compared with no exposure}
#' \item{RRnormalizedToBaseline}{}
#' \item{AF}{Attributable fraction.  Computed with \code{\link{AFForList2}}}
#' \item{normalizedDiseaseBurden}{}
#' \item{dproj.delta}{Change in number of deaths. (the variable name dproj comes from Geoff's projected data)}
#' \item{yll.delta}{Change in YLL}
#' \item{yld.delta}{Change in YLD}
#' \item{daly.delta}{Change in DALY}
#'
#' @seealso \code{\link{createITHIM}}, \code{\link{AFForList2}}
#'
#' @export
compareModels <- function(baseline, scenario){

    baseline <- as(baseline, "list")
    scenario <- as(scenario, "list")
    baseline$parameters <- as(baseline$parameters, "list")
    scenario$parameters <- as(scenario$parameters, "list")
    ## if( identical(baseline$parameters$GBD,scenario$parameters$GBD) ){
    ##     GBD <- baseline$parameters$GBD # GBD must be the same between baseline and scenario
    ##     }else{
    ##         #error message
    ##         }

    GBD <- baseline$parameters$GBD

    RR <- createActiveTransportRRs(nQuantiles = length(baseline$parameters$quantiles))
    RR.baseline <- lapply(RR, MET2RR, baseline$quintiles$TotalMET)
    RR.scenario <- lapply(RR, MET2RR, scenario$quintiles$TotalMET)

    RRnormalizedToBaseline.scenario <- mapply(ratioForList,RR.baseline, RR.scenario, SIMPLIFY = FALSE) # ratioForList simply computes the ratio
    RRnormalizedToBaseline.baseline <- mapply(ratioForList,RR.baseline, RR.baseline, SIMPLIFY = FALSE) # What!  Always 1!

#    AF <- mapply(AFForList, RRnormalizedToBaseline.scenario,RRnormalizedToBaseline.baseline, SIMPLIFY = FALSE) # Neil and Geoff compute AF diifferently.  This is Neil's way
    AF <- mapply(AFForList2, RR.scenario,RR.baseline, SIMPLIFY = FALSE) # Neil and Geoff compute AF diifferently.  This is Geoff's way.

    normalizedDiseaseBurden <- lapply(RR.scenario, normalizeDiseaseBurden)
    normalizedDiseaseBurden.baseline <- lapply(RR.baseline, normalizeDiseaseBurden)

    NewBurden <- lapply(AF,function(x) 1-x)
    NewBurdenList <- lapply(NewBurden,function(x) list(M = x[,"M"], F = x[,"F"]))
    denom <- lapply(normalizedDiseaseBurden, function(x) lapply(x, rowSums))
    denom.baseline <- lapply(normalizedDiseaseBurden.baseline, function(x) lapply(x, rowSums))

    # diseases <- intersect(intersect(names(NewBurdenList),names(GBD)),names(normalizedDiseaseBurden))
    diseases <- c("BreastCancer","ColonCancer","Depression","Dementia","Diabetes", "CVD")

    GBD <- GBD[diseases]
    NewBurdenList <- NewBurdenList[diseases]
    denom <- denom[diseases]
    denom.baseline <- denom.baseline[diseases]
    normalizedDiseaseBurden <- normalizedDiseaseBurden[diseases]
    normalizedDiseaseBurden.baseline <- normalizedDiseaseBurden.baseline[diseases]

    dproj <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom, MoreArgs = list(burden = "dproj"), SIMPLIFY = FALSE)
    dproj.baseline <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom.baseline, MoreArgs = list(burden = "dproj", baseline = TRUE), SIMPLIFY = FALSE)
    dprojBurden <- calculateBurden(dproj, normalizedDiseaseBurden)
    dprojBurden.baseline <- calculateBurden(dproj.baseline, normalizedDiseaseBurden.baseline)
    dproj.delta <- mapply(function(x,y){
        mapply("-",x,y, SIMPLIFY = FALSE)
        },dprojBurden,dprojBurden.baseline, SIMPLIFY = FALSE)

    yll <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom, MoreArgs = list(burden = "yll"), SIMPLIFY = FALSE)
    yll.baseline <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom.baseline, MoreArgs = list(burden = "yll", baseline = TRUE), SIMPLIFY = FALSE)
    yllBurden <- calculateBurden(yll, normalizedDiseaseBurden)
    yllBurden.baseline <- calculateBurden(yll.baseline, normalizedDiseaseBurden.baseline)
    yll.delta <- mapply(function(x,y){
        mapply("-",x,y, SIMPLIFY = FALSE)
        },yllBurden,yllBurden.baseline, SIMPLIFY = FALSE)

    yld <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom, MoreArgs = list(burden = "yld"), SIMPLIFY = FALSE)
    yld.baseline <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom.baseline, MoreArgs = list(burden = "yld", baseline = TRUE), SIMPLIFY = FALSE)
    yldBurden <- calculateBurden(yld, normalizedDiseaseBurden)
    yldBurden.baseline <- calculateBurden(yld.baseline, normalizedDiseaseBurden.baseline)
    yld.delta <- mapply(function(x,y){
        mapply("-",x,y, SIMPLIFY = FALSE)
        },yldBurden,yldBurden.baseline, SIMPLIFY = FALSE)

    daly <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom, MoreArgs = list(burden = "daly"), SIMPLIFY = FALSE)
    daly.baseline <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom.baseline, MoreArgs = list(burden = "daly", baseline = TRUE), SIMPLIFY = FALSE)
    dalyBurden <- calculateBurden(daly, normalizedDiseaseBurden)
    dalyBurden.baseline <- calculateBurden(daly.baseline, normalizedDiseaseBurden.baseline)
    daly.delta <- mapply(function(x,y){
        mapply("-",x,y, SIMPLIFY = FALSE)
        },dalyBurden,dalyBurden.baseline, SIMPLIFY = FALSE)

#    APRR <- createAirPollutionRRs(baseline,scenario)

    return(list(RR.baseline = RR.baseline,
                RR.scenario = RR.scenario,
                RRnormalizedToBaseline = RRnormalizedToBaseline.scenario,
                AF = AF,
                normalizedDiseaseBurden = normalizedDiseaseBurden,
                dproj.delta = dproj.delta,
                yll.delta = yll.delta,
                yld.delta = yld.delta,
                daly.delta = daly.delta
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read in Global Burden of Disease Data
#'
#' Read in Global Burden of Disease Data
#'
#' @return A list of lists of matrices with dproj, yll, yld and daly
#'     by age and sex and disease
#'
#' @export
readGBD <- function(file = "gbd.csv"){
    filePath <- system.file(file, package="ITHIM")
    gbd <- read.csv(file=filePath)
    gbdList <- split(gbd,gbd$disease)
    gbdList[["CVD"]] <- data.frame(disease = "CVD", gbdList$IHD[,c("sex",  "ageClass")], gbdList$IHD[,c("dproj","yll","yld","daly")] + gbdList$InflammatoryHD[,c("dproj","yll","yld","daly")] + gbdList$HHD[,c("dproj","yll","yld","daly")])
    gbdList2 <- lapply(gbdList,function(x) split(x,as.factor(x$sex)))
    gbdList2 <- lapply(gbdList2, function(x) list(M=x$M,F=x$F))
    return(gbdList2)
    }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes simulated distribution of nonTravel METs
#'
#' Computes simulated distribution of nonTravel METs
#'
#' @return A random sample from the distribution.
#'
#' @export
getNonTravelDistribution <- function(mu, cv, size = 1e4){
    mu <- ifelse(mu == 0, 0.01, mu)
    sd <- mu*cv
    simLogNorm <- rlnorm(size, log(mu/sqrt(1+sd^2/mu^2)), sqrt(log(1+sd^2/mu^2)))
    #simData <- ifelse(sample(0:1, size = size, prob = c(1-p,p), replace = TRUE) == 1, simLogNorm, 0)
    simData <- simLogNorm
    return(simData)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes simulated distribution of Travel METs
#'
#' Computes simulated distribution of Travel METs
#'
#' @return A random sample from the distribution.
#'
#' @export
getTravelDistribution <- function(mu, cv, pWalk, vWalk, size = 1e4){
    mu <- ifelse(mu == 0, 0.01, mu)
    sd <- mu*cv
    activeTransportTime <- rlnorm(size, log(mu/sqrt(1+sd^2/mu^2)), sqrt(log(1+sd^2/mu^2)))

    walkingTime <- activeTransportTime*pWalk
    cyclingTime <- activeTransportTime*(1-pWalk)

    walkingMETs <- computeWalkingMETs(vWalk)*walkingTime/60
    cyclingMETs <- computeCyclingMETs()*cyclingTime/60

    travelMETs <- walkingMETs + cyclingMETs

    return(travelMETs)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compute METs given walking speed
#'
#' Compute METs given walking speed
#'
#' @return An estimate for MET expenditure
#'
#' @export
computeWalkingMETs <- function(v){

    METs <- 1.2216*v + 0.0838

    return(ifelse( METs < 2.5, 2.5, METs ))

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compute cycling METs
#'
#' Compute METs
#'
#' @return An estimate for MET expenditure
#'
#' @export
computeCyclingMETs <- function(){

    return(6)

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compute distribution of Total METs
#'
#' Compute distribution of Total METs
#'
#' @return An estimate for total MET distribution
#'
#' @export
getTotalDistribution <- function( muTravel, cvTravel, muNonTravel, cvNonTravel, pWalk, vWalk, size ){

    return(getTravelDistribution( mu = muTravel, cv=cvTravel, pWalk = pWalk, vWalk = vWalk, size = size) + getNonTravelDistribution(mu = muNonTravel, cv = cvNonTravel, size = size))

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~ Deprecated functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #' Generate matrices of non-travel related MET quintiles
## #'
## #' Estimate non-travel related MET quintiles.  Currently we use a
## #' fixed matrix coded into the function.
## #'
## #' @param region character string; either "national" or "SFBayArea"
## #'
## #' @return A list of two matrices of quintiles of non-transport METs
## #'     per week stratified by age class and sex
## #'
## #' @export
## computeNonTravelMETs <- function(region = NA, propActive = NA, mean = NA, cv = NA, knownValues = FALSE){
##     nAgeClass <- 8
##     dimnames <- list(rep(paste0("ageClass",1:nAgeClass),2),paste0("quint",1:5))

##     if( knownValues ){

##     if( region == "cook" ){
##     nonTravelMETs <- matrix(c(0,0,0,0,0,0,0,0,0,0,27.21890244,27.21890244,27.21890244,27.21890244,27.21890244,8.42785658,8.42785658,8.42785658,8.42785658,8.42785658,7.600940041,7.600940041,7.600940041,7.600940041,7.600940041,11.33717949,11.33717949,11.33717949,11.33717949,11.33717949,13.06196237,13.06196237,13.06196237,13.06196237,13.06196237,18.10175439,18.10175439,18.10175439,18.10175439,18.10175439,0,0,0,0,0,0,0,0,0,0,6.858209571,6.858209571,6.858209571,6.858209571,6.858209571,10.76793103,10.76793103,10.76793103,10.76793103,10.76793103,5.40369146,5.40369146,5.40369146,5.40369146,5.40369146,1.829166667,1.829166667,1.829166667,1.829166667,1.829166667,3.037973485,3.037973485,3.037973485,3.037973485,3.037973485,4.063888889,4.063888889,4.063888889,4.063888889,4.063888889),nrow=2*nAgeClass,ncol = 5, byrow=TRUE, dimnames=dimnames)
##         }else if(region == "national"){
##     nonTravelMETs <- matrix(c(0.000,0.000,0.000,0.000,0.000,37.667,26.000,31.667,91.000,62.000,54.000,48.000,46.000,56.000,72.000,30.000,32.000,30.000,32.000,40.000,32.000,22.000,14.000,40.000,48.000,14.000,28.000,16.000,24.000,20.000,8.000,22.000,14.000,10.667,76.000,0.000,7.000,0.000,10.000,56.000,0.000,0.000,0.000,0.000,0.000,26.000,18.000,40.000,18.667,40.000,15.000,12.000,8.000,20.000,24.000,12.000,12.000,9.000,30.000,36.000,10.000,8.000,15.000,8.000,8.000,4.000,12.000,0.000,0.000,36.000,0.000,4.667,0.000,0.000,0.000,0.000,0.000,4.667,0.000,24.000),nrow=2*nAgeClass,ncol = 5, byrow=TRUE, dimnames=dimnames)
##         }else if(region == "SFBayArea"){
##     nonTravelMETs <- matrix(c(0,0,0,0,0,0,0,0,0,0,57.8,41,45.5,37.925,41,51.25,51.25,64.75,46.125,44.8,58.275,61.5,53.725,52.2,46.7,41,31.25,44.0833333333,42.5,32.8,4.375,5,8.3333333333,3.75,13.125,0,10,3.75,5.2083333333,0,0,0,0,0,0,0,0,0,0,0,8.8666666667,24.6,29.85,30.4,41,41,35.875,38.85,41,42.2666666667,41.65,43.05,46.0666666667,41,41,32.8,18,31.75,20.5,4.5,5.8333333333,5,2.5,3.75,0,6.5,3.125,0.8333333333,0,0),nrow=2*nAgeClass,ncol = 5, byrow=TRUE, dimnames=dimnames)
##     }else{
##         # error message here
##         }

##         return(list(M = nonTravelMETs[1:nAgeClass,], F = nonTravelMETs[nAgeClass+(1:nAgeClass),]))
##     }else{

##         #relativeMETs <- matrix(c(0.0000000,0.0000000,1.4774036,0.9174753,0.8070258,0.6548583,0.5872301,0.5568559,0.0000000,0.0000000,1.0000000,0.7421562,0.6413120,0.5372390,0.4628888,0.4040676),ncol = 2)

##         relativeMETs <- matrix(1, nrow = 8, ncol = 2)
##         meanMETReferent <- mean

##         METQuintiles <- t(mapply(getMETQuintiles, relativeMETs*meanMETReferent, MoreArgs = list(cv = cv, p = propActive, size = 1e5), SIMPLIFY = TRUE))
##         dimnames(METQuintiles) <- list(paste0("ageClass",rep(1:8,2)), paste0("quint",1:5))

##         return(list(M = METQuintiles[1:8,], F = METQuintiles[9:16,]))

##         }

## }
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #' Sets a model parameter
## #'
## #' Sets a model parameter
## #'
## #' @return An updated list of parameters
## #'
## #' @export
## setParameter <- function( parName, parValue, parList ){
##     parList[[parName]] <- parValue
##     return(parList)
##     }
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #' Plots means
## #'
## #' Plots means
## #'
## #' @return A ggplot object
## #'
## #' @export
## plotMean <- function(means.baseline, means.scenario, var = "meanActiveTransportTime"){
##     D <- melt(list(baseline = means.baseline[[var]], scenario = means.scenario[[var]]), c("age","quint"), value.name = "mean")
##     names(D) <- c("age","sex","mean","vision")
##     p <- ggplot(D, aes(age,  mean)) + geom_bar(aes(fill=vision), stat = "identity", position = "dodge")
##     return(p)
## }
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #' Plots burden
## #'
## #' Plots burdens
## #'
## #' @param burden A list like as "comparativeRisk$daly.delta"
## #' @param varName A character string indicating the name to use for
## #'     the y axis label
## #'
## #' @return A ggplot object
## #'
## #' @export
## plotBurden <- function(burden, varName = "daly"){
##     foo <- cbind(melt(burden),c("00-04","05-14","15-29","30-44","45-59","60-69","70-79","80+"))
##     names(foo) <- c("burden","sex","disease","Age")
## # what does geom_freqpoly(aes(group = Age, color = Age)) look like? may allow us to bring back in the quintiles?
##     p <- ggplot(foo, aes(disease, -burden)) + geom_bar(aes(fill=Age), stat = "identity", position = "dodge") + labs( y = varName, x = "")
##     p <- p + facet_grid(sex ~ .)
##     return(p)
## }
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #' Set Risk Ratios for Air Pollution
## #'
## #' Set risk ratios for a list of diseases given air pollution exposure.  These
## #' values are used to compute change in disease burden due to air pollution.
## #'
## #' @return A numerical vector of risk ratios given air pollution exposure
## #'
## #' @note Hypertensive HD is done using a combination of RR by METs and RR by air
## #'     pollution.
## #'
## #' @export
## createAirPollutionRRs <- function(baseline, scenario){

##     diseaseNames <- c("LungCancer","AcuteRespInfect","InflammatoryHD","RespiratoryDisease", "CVD", "HHD", "Stroke")

##     k <- rep(0.008618,length(diseaseNames))

##     exposure.baseline <- baseline$parameters$pm25
##     exposure.scenario <- scenario$parameters$pm25

##     RR <- exp(k*(exposure.scenario-exposure.baseline))
##     names(RR) <- diseaseNames

##     RR.list <- lapply(as.list(RR), function(x) list(M=matrix(x,nrow=8,ncol=5,dimnames=list(paste0("ageClass",1:8),paste0("quint",1:5))),F=matrix(x,nrow=8,ncol=5,dimnames=list(paste0("ageClass",1:8),paste0("quint",1:5)))))

##     return(RR.list)

## }
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #' Set Risk Ratios for Active Transport and Air Pollution
## #'
## #' Set risk ratios for a list of diseases given air pollution exposure and active transport exposure.  These
## #' values are used to compute change in disease burden..
## #'
## #' @return A numerical vector of risk ratios given air pollution exposure and active transport exposure
## #'
## #' @export
## createATandAPRRs <- function(){

##     diseaseNames <- c("Hypertensive HD", "CVD")
##     RR <- rep(1.02, length(diseaseNames))
##     names(RR) <- diseaseNames
##     return(RR)

## }
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #' Computes MET quintiles
## #'
## #' Uses a mixture distribution to estimate non-travel METs
## #'
## #' @return A vector of quintiles.
## #'
## #' @export
## getMETQuintiles <- function(mu, p, cv, quantiles = seq(0.1,0.9,0.1), size = 1e3){
##     mu <- ifelse(mu == 0, 0.01, mu)
##     sd <- mu*cv
##     simLogNorm <- rlnorm(size, log(mu/sqrt(1+sd^2/mu^2)), sqrt(log(1+sd^2/mu^2)))
##     simData <- ifelse(sample(0:1, size = size, prob = c(1-p,p), replace = TRUE) == 1, simLogNorm, 0)
##     return(quantile(simData, probs = quantiles, na.rm = TRUE))
## }
