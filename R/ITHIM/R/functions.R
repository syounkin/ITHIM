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

createParameterList <- function(){

    nAgeClass <- 8
    
    F <- matrix(c(0.0368276992,0.0353723566,0.0746989673,0.0716902674,0.1123490977,0.1104366009,0.1163649132,0.1182206842,0.0808260989,0.0891264801,0.0308720468,0.037493344,0.0223834475,0.0321797163,0.0098989332,0.0212593465), byrow = TRUE, nrow = nAgeClass, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rwt <- matrix(c(0.43053,0.34715,0.49337,0.48135,0.93248,1.00000,0.76528,0.73350,0.68250,0.65805,0.56376,0.77155,0.58923,0.62678,0.56524,0.39604),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rct <- matrix(c(0.29350,0.12305,1.31944,0.82780,1.86938,1.00000,1.56016,0.73770,1.45792,0.24667,0.45162,0.18923,0.49021,0.16502,0.07503,0.02941),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rws <- matrix(c(1.06625,0.87533,1.06625,0.87533,1.02062,1.00021,1.05905,1.03383,1.03923,0.94738,1.03023,0.93297,0.95098,0.89695,0.95098,0.89695),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    Rcs <- matrix(c(0.847395,0.837454,0.892011,0.981718,1.072100,0.956966,1.073487,0.980472,1.156495,0.962792,1.063072,0.929156,1.029339,0.848509,0.902283,0.830812),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))

    muwt <- 54.4 # min per week
    muws <- 2.7 # mph
    muct <- 9.7 # min per week
    mucs <- 7.4 # mph

    cv <- 1.723 # coefficient of variation

    #meanTw.Mat <- muwt/alphawt*Rwt
    
    return(list(F = F, Rwt = Rwt, Rws = Rws, Rct = Rct, Rcs = Rcs, muwt = muwt, muws = muws, muct = muct, mucs = mucs, cv = cv, nAgeClass = nAgeClass))

}


computeMeanMatrices <- function(parList){
    with(parList, {
        alphawt <- sum(F*Rwt)
        alphact <- sum(F*Rct)
        alphaws <- sum(F*Rws)
        alphacs <- sum(F*Rcs)
        meanWalkTime <- muwt/alphawt*Rwt
        meanCycleTime <- muct/alphact*Rct
        meanWalkSpeed <- muws/alphaws*Rws
        meanCycleSpeed <- mucs/alphacs*Rcs
        meanWalkMET <- ifelse(1.2216*meanWalkSpeed + 0.0838 < 2.5, 2.5,  1.2216*meanWalkSpeed + 0.0838)
        meanCycleMET <- matrix(6, byrow=TRUE, ncol = 2, nrow =nrow(F),dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
        meanActiveTransportTime <- meanWalkTime + meanCycleTime
        sdActiveTransportTime <- meanActiveTransportTime*cv
        return(list(meanWalkTime = meanWalkTime, meanCycleTime = meanCycleTime, meanWalkSpeed = meanWalkSpeed, meanCycleSpeed = meanCycleSpeed, meanWalkMET = meanWalkMET, meanCycleMET = meanCycleMET, meanActiveTransportTime = meanActiveTransportTime, sdActiveTransportTime = sdActiveTransportTime))
        })
}

computeQuintiles <- function( mean, sd ){

    logMean <- log(mean)-1/2*log(1+(sd/mean)^2)
    logSD <- sqrt(log(1+(sd/mean)^2))

    quintVec <- c(mapply(qlnorm, logMean, logSD, p = 0.1),
    mapply(qlnorm, logMean, logSD, p = 0.3),
    mapply(qlnorm, logMean, logSD, p = 0.5),
    mapply(qlnorm, logMean, logSD, p = 0.7),
    mapply(qlnorm, logMean, logSD, p = 0.9))
    return(quintVec)
}
