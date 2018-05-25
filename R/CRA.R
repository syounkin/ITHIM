#' Perform comparative risk assesment
#'
#' This function accepts up to eight parameters for use with the
#' parameteric physical activity model, or two vectors of quantiles
#' for the non-paraetric model.  The function returns the population
#' attributable fraction.
#'
#' @param meanlog.baseline Mean of exposure in baseline (log-scale, parameteric model)
#' @param sdlog.baseline Standard deviation of exposure in baseline (log-scale, parameteric model)
#' @param p0.baseline Proportion of inactive travelers in baseline
#' @param meanlog.scenario Mean of exposure in scenario (log-scale, parameteric model)
#' @param sdlog.scenario Standard deviation of exposure in scenario (log-scale, parameteric model)
#' @param p0.scenario Proportion of inactive travelers in scenario
#' @param meanlog.leisure Mean of exposure in leisure physical activity (log-scale, parameteric model)
#' @param sdlog.leisure Standard deviation of leisure physical activity in baseline (log-scale, parameteric model)
#' @param n Number of quantiles (parametric)
#' @param B Sample size (parametric model)
#' @param P Quantiles of exposure in baseline (non-parametric model)
#' @param Q Quantiles of exposure in scenario (non-parametric model)
#' @param R Relative risk function in terms of total exposure (physical activity)
#' @param type A character string.  Either "parametric" or "non-parametric"
#'
#' @return The population attributable fraction
CRA.function <- function(meanlog.baseline = log(5),
                sdlog.baseline = 1,
                p0.baseline = 0.5,
                meanlog.scenario = log(5),
                sdlog.scenario = 1,
                p0.scenario = 0.5,
                meanlog.leisure = log(10),
                sdlog.leisure = 1,
                n = 1e3,
                B = 1e5,
                P,
                Q,
                R = function(x) exp(-0.03*x), # shape parameter for colon cancer from Excel workbook
                type = "parametric"){

    if(type == "parametric"){

        LA <- rlnorm(n = B, meanlog = meanlog.leisure, sdlog = sdlog.leisure)
        runif <- runif(n = B)

        TA.sample.baseline <- rlnorm(n = B, meanlog = meanlog.baseline, sdlog = sdlog.baseline)
        TA.baseline <- ifelse(runif < p0.baseline, 0, TA.sample.baseline)
        P <- quantile(TA.baseline + LA, probs = (1:(n-1))/n)

        TA.sample.scenario <- rlnorm(n = B, meanlog = meanlog.scenario, sdlog = sdlog.scenario)
        TA.scenario <- ifelse(runif < p0.scenario, 0, TA.sample.scenario)
        Q <- quantile(TA.scenario + LA, probs = (1:(n-1))/n)

    }else if( type == "non-parametric" ){

        # Nothing to be processed.  P and Q are being entered directly.

    }else{

        stop("Error with variable type.  Must be parametric or non-parametric.")

    }

    return((sum(R(Q)) - sum(R(P)))/sum(R(P)))

}
#' Perform comparative risk assesment
#'
#' Foo
#'
#' @param leisue A character string, either "low", "medium" or "high"
#'     indicating the amonut of leisure activity on the population
#' @param n Number of quantiles (parametric)
#'
#' @return A data frame with population attributable fractions
CRA.DF <- function(leisure = "medium"){
    if(leisure == "low"){
        leisure.value <- log(1)} # 3 min/day walking
    else if(leisure == "medium"){
        leisure.value <- log(10) # 30 min/day walking
    }else if( leisure == "high"){
        leisure.value <- log(50) # 2.5 hrs/day walking
    }else{
        message("Problem with leisure argument.")
    }
    CRA.interior <- function(x, y) {
        cra.value <- CRA.function(meanlog.baseline = x$meanlog,
                         meanlog.scenario = y$meanlog,
                         p0.baseline = x$p0,
                         p0.scenario = y$p0,
                         meanlog.leisure = leisure.value,
                         type = "parametric"
                         )
        return(cra.value)
    }
    return(CRA.interior)
}
#' Perform comparative risk assesment
#'
#' Foo
#'
#' @param activityFile.baseline Name of activity file for baseline
#' @param activityFile.scenario Name of activity file for scenario
#'
#' @return A data frame with population attributable fractions
#' @export
CRA <- function(activityFile.baseline = system.file("activity.baseline.csv", package = "ITHIM"), activityFile.scenario = system.file("activity.scenario.csv", package = "ITHIM")){

    activityDF.baseline <- read.csv(file = activityFile.baseline, row.names = 1)
    activityDF.scenario <- read.csv(file = activityFile.scenario, row.names = 1)
    activityList.baseline <- setNames(split(activityDF.baseline,seq(nrow(activityDF.baseline))),rownames(activityDF.baseline))
    activityList.scenario <- setNames(split(activityDF.scenario,seq(nrow(activityDF.scenario))),rownames(activityDF.scenario))

    return(data.frame(low = mapply( CRA.DF(leisure = "low"), activityList.baseline, activityList.scenario), medium = mapply( CRA.DF(leisure = "medium"), activityList.baseline, activityList.scenario), high = mapply( CRA.DF(leisure = "high"), activityList.baseline, activityList.scenario)))

}
