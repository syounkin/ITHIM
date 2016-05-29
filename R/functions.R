#' Integrated Transport and Health Impacts Model (ITHIM)
#'
#' Implementation in R
#'
#' @name ITHIM-package
#' @docType package
#' @author Samuel G. Younkin \email{syounkin@@wisc.edu}
#' @seealso \code{\link{createParameterList}}, \code{\link{computeMeanMatrices}}
#' @examples
#'
#' ITHIMParameterList <- createParameterList()
#' meansList <- computeMeanMatrices(ITHIMParameterList)
#' names(meansList)
#' meansList$meanActiveTransportTime
#'
NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' This function coverts the 2001 CDC Age variable to years.
#'
#' @param value The value for age, i.e., characters 2-4 of the code
#'     (sloppy with class!)
#' @param unit The units for age, i.e., chatater 1 of the code (sloppy
#'     with class!)
#' @note This function is likely only applicable to the 2001 data
#'     file.
#' @return A numeric vector of age in years - this is a test
#'
#' @export
convertAge <- function(value, unit){

    value <- ifelse(value == "999", NA, value)

    convertedValue <- ifelse( unit == "1", value,
    ifelse( unit == "2", value/12,
    ifelse( unit == "4", value/365,
    ifelse( unit == "5", value/365/24,
    ifelse( unit == "6", value/365/24/60,
    ifelse( unit == "9", NA, 999 ))))))

    return(as.numeric(convertedValue))

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a list of input parameters
#'
#' @param baseline A logical value indicating if we are using the baseline scenario
#'
#' @return A list with parameters and estimates
#'
#' \item{F}{A numerical matrix for the population density, stratified by age class and sex}
#' \item{Rwt}{A numerical matrix for the walking time, relative to ?value?}
#' \item{Rws}{A numerical matrix for the walking speed, relative to ?value?}
#' \item{Rct}{A numerical matrix for the cycling time, relative to ?value?}
#' \item{muwt}{A numerical value for the mean walking time}
#' \item{muws}{A numerical value for the mean walking speed}
#' \item{muct}{A numerical value for the mean cycling time}
#' \item{cv}{A numerical value for the coefficient of variation for active transport time}
#'
#' @note A note about the default values.
#'
#'
#' @export
createParameterList <- function(baseline = TRUE){

    nAgeClass <- 8

    F <- matrix(c(0.0368276992,0.0353723566,0.0746989673,0.0716902674,0.1123490977,0.1104366009,0.1163649132,0.1182206842,0.0808260989,0.0891264801,0.0308720468,0.037493344,0.0223834475,0.0321797163,0.0098989332,0.0212593465), byrow = TRUE, nrow = nAgeClass, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

if(baseline){

    Rwt <- matrix(c(0.43053,0.34715,0.49337,0.48135,0.93248,1.00000,0.76528,0.73350,0.68250,0.65805,0.56376,0.77155,0.58923,0.62678,0.56524,0.39604),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rct <- matrix(c(0.29350,0.12305,1.31944,0.82780,1.86938,1.00000,1.56016,0.73770,1.45792,0.24667,0.45162,0.18923,0.49021,0.16502,0.07503,0.02941),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rws <- matrix(c(1.0663,0.8753,1.0663,0.8753,1.0206,1.0002,1.0590,1.0338,1.0392,0.9474,1.0302,0.9330,0.9510,0.8969,0.9510,0.8969),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    muwt <- 54.4 # min per week
    muws <- 2.7 # mph
    muct <- 9.7 # min per week

}else{

    Rwt <- matrix(c(0.430529164,0.347145072,0.493373244,0.481350453,0.932484686,1,0.765284191,0.733501073,0.682502249,0.65804925,0.563755831,0.771546332,0.589228976,0.626782877,0.565238338,0.396044894),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rct <- matrix(c(0.5,0.3,1.1,0.9,1.5,1,1.3,1,1.3,0.8,1,0.6,0.7,0.5,0.25,0.2),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rws <- matrix(c(1.06625,0.87533,1.06625,0.87533,1.02062,1.00021,1.05905,1.03383,1.03923,0.94738,1.03023,0.93297,0.95098,0.89695,0.95098,0.89695),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    muwt <- 107.1 # min per week
    muws <- 2.8 # mph
    muct <- 39.0 # min per week

}

    cv <- 1.723 # coefficient of variation

    return(list(F = F, Rwt = Rwt, Rws = Rws, Rct = Rct, muwt = muwt, muws = muws, muct = muct, cv = cv, nAgeClass = nAgeClass))

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
#' @seealso \code{\link{createParameterList}}, \code{\link{ITHIM-package}}
#'
#' @export
computeMeanMatrices <- function(parList){
    with(parList, {
        alphawt <- sum(F*Rwt)
        alphact <- sum(F*Rct)
        alphaws <- sum(F*Rws)

        meanWalkTime <- muwt/alphawt*Rwt
        meanCycleTime <- muct/alphact*Rct
        propTimeCycling <-  meanCycleTime/(meanCycleTime+meanWalkTime)
        meanWalkSpeed <- muws/alphaws*Rws

        meanWalkMET <- ifelse(1.2216*meanWalkSpeed + 0.0838 < 2.5, 2.5,  1.2216*meanWalkSpeed + 0.0838)
        meanCycleMET <- matrix(6, byrow=TRUE, ncol = 2, nrow =nrow(F),dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
        meanActiveTransportTime <- meanWalkTime + meanCycleTime
        sdActiveTransportTime <- meanActiveTransportTime*cv

        return(list(meanWalkTime = meanWalkTime, meanCycleTime = meanCycleTime, meanWalkSpeed = meanWalkSpeed, meanWalkMET = meanWalkMET, meanCycleMET = meanCycleMET, meanActiveTransportTime = meanActiveTransportTime, sdActiveTransportTime = sdActiveTransportTime, propTimeCycling = propTimeCycling))
        })
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
computeQuintiles <- function( mean, sd ){

    nAgeClass <- nrow(mean)
    ncol <- 5

    logMean <- log(mean)-1/2*log(1+(sd/mean)^2)
    logSD <- sqrt(log(1+(sd/mean)^2))

    quintVec <- c(mapply(qlnorm, logMean, logSD, p = 0.1),
    mapply(qlnorm, logMean, logSD, p = 0.3),
    mapply(qlnorm, logMean, logSD, p = 0.5),
    mapply(qlnorm, logMean, logSD, p = 0.7),
    mapply(qlnorm, logMean, logSD, p = 0.9))

    quintMat <- matrix(quintVec, nrow = 2*nAgeClass, ncol = ncol, dimnames = list(paste0("ageClass", rep(1:nAgeClass,2)),paste0("q",1:ncol)))

    quintList = list(M = quintMat[1:nAgeClass,], F = quintMat[nAgeClass+1:8,])

    return(quintList)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compute Quintiles of Active Transport Time
#'
#' Compute Quintiles of Active Transport Time
#'
#' @param ITHIM A list of values generated by the \code{\link{computeMeanMatrices}}
#'
#' @return A list of lists containing quintiles of active transport
#'     time and METs, by sex and age class.
#'
#' \item{ActiveTransportTime}{Foo}
#' \item{WalkingTime}{Foo}
#' \item{CyclingTime}{Foo}
#' \item{WalkingMET}{Foo}
#' \item{CyclingMET}{foo}
#' \item{TotalTravelMET}{foo}
#'
#' @seealso \code{\link{computeMeanMatrices}}
#'
#' @export
getQuintiles <- function(ITHIM){
  with(ITHIM,{
    ActiveTransportTime <- computeQuintiles(means$meanActiveTransportTime, means$sdActiveTransportTime)
  WalkingTime <- list(M = ActiveTransportTime[["M"]] * (1-means$propTimeCycling[,"M"]), F = ActiveTransportTime[["F"]] * (1-means$propTimeCycling[,"F"]))
  CyclingTime <- list(M = ActiveTransportTime[["M"]] * (means$propTimeCycling[,"M"]), F = ActiveTransportTime[["F"]] * (means$propTimeCycling[,"F"]))
  WalkingMET <- list(M = means$meanWalkMET[,"M"]*WalkingTime[["M"]]/60, F = means$meanWalkMET[,"F"]*WalkingTime[["F"]]/60)
  CyclingMET <- list(M = means$meanCycleMET[,"M"]*CyclingTime[["M"]]/60, F = means$meanCycleMET[,"F"]*CyclingTime[["F"]]/60)
    TotalTravelMET <- list(M = WalkingMET[["M"]] + CyclingMET[["M"]], F = WalkingMET[["F"]] + CyclingMET[["F"]])

  TotalMET <- mapply(function(x,y) ifelse(x+y<2.5,0.1,x+y),TotalTravelMET,computeNonTravelMETs(),SIMPLIFY=FALSE)
    
 return(list(ActiveTransportTime=ActiveTransportTime, WalkingTime=WalkingTime, CyclingTime=CyclingTime, WalkingMET=WalkingMET, CyclingMET = CyclingMET, TotalTravelMET = TotalTravelMET, TotalMET = TotalMET))})
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
#' @examples
#'
#' createActiveTransportRRs
#'
#' @export
createActiveTransportRRs <- function(){

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

    exposure[["Depression"]][1:2,1:2] <- 11.25
    RR.lit[["Depression"]][1:2,1:2] <- 0.927945490148335

    exposure[["Depression"]][3:nAgeClass,1:2] <- 11.25
    RR.lit[["Depression"]][3:nAgeClass,1:2] <- 0.859615572255727

    k <- 0.5
    RR <- mapply(function(x,y,k) x^(1/y)^k, RR.lit, exposure, 0.5, SIMPLIFY=FALSE)
    RR <- lapply(RR, reshapeRR)

    return(RR)

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set Risk Ratios for Air Pollution
#'
#' Set risk ratios for a list of diseases given air pollution exposure.  These
#' values are used to compute change in disease burden due to air pollution.
#'
#' @return A numerical vector of risk ratios given air pollution exposure
#'
#' @note Hypertensive HD is done using a combination of RR by METs and RR by air
#'     pollution.
#'
#' @export
createAirPollutionRRs <- function(){

    diseaseNames <- c("Lung Cancer","Acute resp infections","Inflammatory HD","Respiratory diseases")
    RR <- rep(1.02, length(diseaseNames))
    names(RR) <- diseaseNames
    return(RR)

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set Risk Ratios for Active Transport and Air Pollution
#'
#' Set risk ratios for a list of diseases given air pollution exposure and active transport exposure.  These
#' values are used to compute change in disease burden..
#'
#' @return A numerical vector of risk ratios given air pollution exposure and active transport exposure
#'
#' @export
createATandAPRRs <- function(){

    diseaseNames <- c("Hypertensive HD", "CVD")
    RR <- rep(1.02, length(diseaseNames))
    names(RR) <- diseaseNames
    return(RR)

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate matrices of non-travel related MET quintiles
#'
#' Estimate non-travel related MET quintiles.  Currently we use a
#' fixed matrix coded into the function.
#'
#' @return A list of two matrices of quintiles of non-transport METs
#'     per week stratified by age class and sex
#'
#' @export
computeNonTravelMETs <- function(){
    nAgeClass <- 8
    dimnames <- list(rep(paste0("ageClass",1:nAgeClass),2),paste0("quint",1:5))
    nonTravelMETs <- matrix(c(0,0,0,0,0,0,0,0,0,0,27.21890244,27.21890244,27.21890244,27.21890244,27.21890244,8.42785658,8.42785658,8.42785658,8.42785658,8.42785658,7.600940041,7.600940041,7.600940041,7.600940041,7.600940041,11.33717949,11.33717949,11.33717949,11.33717949,11.33717949,13.06196237,13.06196237,13.06196237,13.06196237,13.06196237,18.10175439,18.10175439,18.10175439,18.10175439,18.10175439,0,0,0,0,0,0,0,0,0,0,6.858209571,6.858209571,6.858209571,6.858209571,6.858209571,10.76793103,10.76793103,10.76793103,10.76793103,10.76793103,5.40369146,5.40369146,5.40369146,5.40369146,5.40369146,1.829166667,1.829166667,1.829166667,1.829166667,1.829166667,3.037973485,3.037973485,3.037973485,3.037973485,3.037973485,4.063888889,4.063888889,4.063888889,4.063888889,4.063888889),nrow=2*nAgeClass,ncol = 5, byrow=TRUE, dimnames=dimnames)

    return(list(M = nonTravelMETs[1:nAgeClass,], F = nonTravelMETs[nAgeClass+(1:nAgeClass),]))

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Transforms the RR object
#'
#' Transforms the RR object into something more convenient.
#'
#' @return A list of two matrices of RRs stratified by age class and
#'     sex
#'
#' @export
reshapeRR <- function(RR){
    nAgeClass <- 8
    list( M = matrix(RR[,"M"], nrow = nAgeClass, ncol = 5, dimnames = list(paste0("ageClass",1:nAgeClass), paste0("quint",1:5))),F = matrix(RR[,"F"], nrow = nAgeClass, ncol = 5, dimnames = list(paste0("ageClass",1:nAgeClass), paste0("quint",1:5))))
    }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes the RR for an exposure of x MET
#'
#' We use the transformation RR_x = RR_1^(x^k), where RR_1 is the
#' relative risk for one MET.
#'
#' @return A list of matrices of quintiles of RR_x stratified by age
#'     class and sex
#'
#' @note k is fixed at 0.5 for now
#'
#' @export
MET2RR <- function(RR,MET){
    mapply(FUN = function(x, y) x^(y^0.5), RR, MET, SIMPLIFY = FALSE)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes a ratio of elements in two lists
#'
#' Computes a ratio of elements in two lists.
#'
#' @return A list of ratios
#'
#' @export
ratioForList <- function(baseline,scenario){
mapply(FUN = "/", baseline, scenario, SIMPLIFY = FALSE)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes AF given baseline and scenario ?disease burdens?
#'
#' Computes AF given baseline and scenario
#'
#' @return A list of AFs stratified by age and sex
#'
#' @export
AFForList <- function(scenario,baseline){
    mapply(function(scenario,baseline) (rowSums(scenario)-rowSums(baseline))/rowSums(scenario), scenario, baseline)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes AF given baseline and scenario models
#'
#' Computes AF given baseline and scenario
#'
#' @return A list of AFs stratified by age and sex
#'
#' @export
compareModels <- function(baseline,scenario){
    RR <- createActiveTransportRRs()
    RR.baseline <- lapply(RR, MET2RR, baseline$quintiles$TotalMET)
    RR.scenario <- lapply(RR, MET2RR, scenario$quintiles$TotalMET)

    diseaseBurden.scenario <- mapply(ratioForList,RR.baseline, RR.scenario, SIMPLIFY = FALSE)
    diseaseBurden.baseline <- mapply(ratioForList,RR.baseline, RR.baseline, SIMPLIFY = FALSE) # What!
    AF <- mapply(AFForList, diseaseBurden.scenario,diseaseBurden.baseline, SIMPLIFY = FALSE)

    return(list(RR.baseline = RR.baseline, RR.scenario = RR.scenario, diseaseBurden = diseaseBurden.scenario, AF = AF))
    }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plots a RR matrix
#'
#' Plots a RR matrix
#'
#' @return A ggplot object
#'
#' @export
plotRR <- function(RR.baseline,RR.scenario){
D <- melt(list(baseline = RR.baseline, scenario = RR.scenario), c("age","quint"), value.name = "RR")
names(D) <- c("age","quint","RR","sex","vision")
p <- ggplot(D, aes(age,  RR)) + geom_bar(aes(fill=vision), stat = "identity", position = "dodge")
p <- p + facet_grid( . ~ sex)
return(p)
}
