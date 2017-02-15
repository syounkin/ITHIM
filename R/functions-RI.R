#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Computes road injury multiplier
#'
#' This multiplier is used to estimate the road injury count in the
#' scenario.  It is a function of distance by road type (person-miles
#' and vehicle-miles) and safey in number parameters (one for
#' person-miles and one for vehicle miles.  Currently both values are
#' fixed at 0.5 for both person and vehicle.
#'
#' @param base a list containing two data frames (one for person-miles
#'     and one for vehicle miles) indicating the distance travelled in
#'     the baseline
#'
#' @param scenario Same as above but for the scenario.
#'
#' @return A list of multipliers.
#'
#'@export
computeMultiplier <- function(base, scenario, safetyInNumbers){

    local <- outer((scenario$perMiles[,"Local"]/base$perMiles[,"Local"])^safetyInNumbers[,"victim"],(scenario$vehMiles[,"Local"]/base$vehMiles[,"Local"])^safetyInNumbers[,"striking"],"*")
        arterial <- outer((scenario$perMiles[,"Arterial"]/base$perMiles[,"Arterial"])^safetyInNumbers[,"victim"],(scenario$vehMiles[,"Arterial"]/base$vehMiles[,"Arterial"])^safetyInNumbers[,"striking"],"*")
        highway <- outer((scenario$perMiles[,"Highway"]/base$perMiles[,"Highway"])^safetyInNumbers[,"victim"],(scenario$vehMiles[,"Highway"]/base$vehMiles[,"Highway"])^safetyInNumbers[,"striking"],"*")

        local <- cbind(local, NOV = (scenario$perMiles[,"Local"]/base$perMiles[,"Local"])^safetyInNumbers[,"NOV"])
        arterial <- cbind(arterial, NOV = (scenario$perMiles[,"Arterial"]/base$perMiles[,"Arterial"])^safetyInNumbers[,"NOV"])
        highway <- cbind(highway, NOV = (scenario$perMiles[,"Highway"]/base$perMiles[,"Highway"])^safetyInNumbers[,"NOV"])

    list(local = local,arterial = arterial,highway = highway)
}
#'@export
computeInjuryRR <- function(RI.baseline, RI.scenario){
# written by Tomek
injuryTypes <- c("Fatal", "Serious")
roadTypes <- c("Local","Arterial","Highway")
dsNames <- c("Baseline", "Scenario")
injuryResultsTotals <- as.data.frame(matrix(nrow = length(injuryTypes),
                              ncol = length(dsNames),
                              dimnames = list(injuryTypes, dsNames)))

injuryRRbyModes <- injuryRR <- vector(mode="list", length=length(injuryTypes))
names(injuryRRbyModes) <- injuryTypes
names(injuryRR) <- injuryTypes

# for every injuryType

for (it in injuryTypes){

  # generate combinations of injuryTypes - roadTypes

  injuryRoadTypes <- paste0(it, roadTypes)

  # for every dataSource
  i <- 0
  for (ds in list(RI.baseline, RI.scenario)){

    # run some checks regarding structure of input (passed via method args) data

    # check data type (should be list)

    if (is.list(ds)){

      # check if needed injury-road type combinations are here

      if(length(setdiff(injuryRoadTypes, names(ds))) == 0){

        # check data type of every entry (should be data.frame)

        lapply(ds, function(irtentry) if(!(is.data.frame(irtentry))){
          stop('computeInjuryRR: wrong entry data type: data.frame check')
          })

      } else {
        stop('computeInjuryRR: lack of needed injury-road type combinations')
      }

    } else {
      stop('computeInjuryRR: wrong input data type: list check')
    }

    i <- i+1

    # get data source variable from env

    dsVar <- ds

    dsResults <- NULL

    # iterate over injuryRoadTypes - sum matrices

    for (irt in injuryRoadTypes){

      if(is.null(dsResults)){
        dsResults <- dsVar[[irt]]
      } else {
        dsResults <- dsResults + dsVar[[irt]]
      }

    }

    # row totals

    dsResults <- transform(dsResults, total=rowSums(dsResults, na.rm = T))

    # save matrix with totals in corresponding cell

    injuryResultsTotals[[it, dsNames[i]]] <- dsResults[,c("total"), drop = F]

  }
}

# RR by modes

# for every injuryType

for (it in injuryTypes){
  injuryRRbyModes[[it]] <- injuryResultsTotals[[it, "Scenario"]] / injuryResultsTotals[[it, "Baseline"]]
}

# RR

# for every injuryType

for (it in injuryTypes){
  injuryRR[[it]] <- sum(injuryResultsTotals[[it, "Scenario"]], na.rm = T) / sum(injuryResultsTotals[[it, "Baseline"]], na.rm = T)
}
return(injuryRR)
}
#'@export
multiplyInjuries <- function(ITHIM.baseline, ITHIM.scenario){

multiplier <- computeMultiplier(base = getDistRoadType(ITHIM.baseline), scenario = getDistRoadType(ITHIM.scenario), safetyInNumbers = getParameterSet(ITHIM.baseline)@safetyInNumbers)

multiplier <- lapply(multiplier, function(x) ifelse(is.na(x),1,x))

    RI <- getRoadInjuries(ITHIM.baseline)

    #RI <- lapply(RI, function(x) x[,-c(1,2)])

RI.scenario <- list(
     FatalLocal = RI$FatalLocal*multiplier$local,
     FatalArterial = RI$FatalArterial*multiplier$arterial,
     FatalHighway = RI$FatalHighway*multiplier$highway,
     SeriousLocal = RI$SeriousLocal*multiplier$local,
     SeriousArterial = RI$SeriousArterial*multiplier$arterial,
     SeriousHighway = RI$SeriousHighway*multiplier$highway
     )
return(RI.scenario)

}
#'@export
computeRoadInjuryBurden <- function(ITHIM.baseline, ITHIM.scenario){
  injuryRR <- computeInjuryRR(getRoadInjuries(ITHIM.baseline), getRoadInjuries(ITHIM.scenario))
  RTI.GBD <- subset(getGBD(ITHIM.baseline),disease == "RTIs")
  RIburden <- data.frame(ageClass = RTI.GBD$ageClass, sex = RTI.GBD$sex, burden = RTI.GBD$variable,delta = RTI.GBD$value*(1-injuryRR$Fatal))
  return(RIburden)
}
#'@export
updateRoadInjuries <- function(ITHIM.baseline, ITHIM.scenario){
ITHIM.scenario <- update(ITHIM.scenario, list(roadInjuries = multiplyInjuries(ITHIM.baseline, ITHIM.scenario)))
return(ITHIM.scenario)
}
#'@export
readRoadInjuries <- function(filename){
#    filename <- system.file( filename, package = "ITHIM")
    roadInjuries <- read.csv(file = filename, header = TRUE, stringsAsFactors = FALSE)
    roadInjuries <- within(roadInjuries, ebike <- as.numeric(ebike))
    roadInjuries <- within(roadInjuries, NOV <- as.numeric(NOV))


    roadInjuries <- split(roadInjuries, roadInjuries$SeverityByRoadType)
#    names(roadInjuries) <- c("FatalLocal","FatalArterial","FatalHighway","SeriousLocal","SeriousArterial","SeriousHighway")
 #   roadInjuries <- lapply(roadInjuries,function(x){dimnames(x) <- list(c("walk","cycle","bus","car","HGV","LGV","mbike","ebike"),c("walk","cycle","bus","car","HGV","LGV","mbike","ebike","NOV"));x})
return(roadInjuries)
}
