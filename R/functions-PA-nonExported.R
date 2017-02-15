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
#' Computes AF given baseline and scenario
#'
#' Computes AF given baseline and scenario RRs relative to baseline.
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
#' Computes AF given baseline and scenario
#'
#' Computes AF given baseline and scenario RRs relative to baseline.
#'
#' @param scenario RR compared with no exposure
#'
#' @param baseline RR compared with no exposure
#'
#' @return A list of AFs stratified by age and sex
#'
#' @export
AFForList2 <- function(scenario,baseline){
    mapply(function(scenario,baseline) 1 - rowSums(scenario)/rowSums(baseline), scenario, baseline)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Foo
#'
#' Foo
#'
#' @return Foo
#'
#' @export
normalizeDiseaseBurden <- function(diseaseBurden){
    lapply(diseaseBurden, function(x) x/x[,1])
    }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ??
#'
#' ??
#'
#' @return ??
#'
#'
#' @export
burdenFunction <- function(x2,y2,z2,burden,baseline=FALSE){
    if(!baseline){
        mapply(function(x,y,z){x[,burden] * y / z}, x2, y2, z2, SIMPLIFY = FALSE)
    }else{
        mapply(function(x,y,z){x[,burden] / z}, x2, y2, z2, SIMPLIFY = FALSE)
    }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Internal function.
#'
#' ??
#'
#' @return ??
#'
#'
#' @export
calculateBurden <- function(burden, normalizedDiseaseBurden){

    foo <- function(x,y){
        matrix(x, nrow = length(x), ncol = ncol(y)) * y
        }

    foo2 <- function(x,y){
        mapply(foo, x, y, SIMPLIFY = FALSE)
        }

    List <- mapply(foo2, burden, normalizedDiseaseBurden, SIMPLIFY = FALSE)

    Burden <- lapply(List, function(x){
        lapply(x,rowSums)
        })

        return(Burden)

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
reshapeRR <- function(RR, nQuantiles = 5){
    nAgeClass <- 8
    list( M = matrix(RR[,"M"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), paste0("quint",1:nQuantiles))),F = matrix(RR[,"F"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), paste0("quint",1:nQuantiles))))
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
#' Retrieve a density vector for a logNormal distribution
#'
#' Retrieve a density vector for a logNormal distribution
#'
#' @param mu The mean (on log scale or not?  Figure this out.
#' @param sd The standard deviation (on log scale or not?  Figure this out.
#'
#' @return A vector of length 2000 with density values over the
#'     interval 0 to 2000
#'
#' @export
getLogNormal <- function(mu,sd){
    dlnorm(seq(0,2000,length.out=1e3), log(mu/sqrt(1+sd^2/mu^2)), sqrt(log(1+sd^2/mu^2)))
}
