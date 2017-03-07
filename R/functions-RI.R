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
computeMultiplier <- function(baseline, scenario, safetyInNumbers){
  
  # TODO: update doc
  
  # which dim should be used to calc ratio. TODO: could be set via params?
  
  dimForRatio <- c('distType')
  
  # which index represents person part. TODO: could be set via params?
  
  personIdx <- c('person')
  
  # which index represents vehicle part. TODO: could be set via params?
  
  vehicleIdx <- c('vehicle')
  
  # which dim represents modes and should be renamed to victim/striking. TODO: could be set via params?
  
  dimForMode <- c('mode')
  
  # output renamed mode victim, striking (in that order!).
  # Bacause of renaming inputDims for baseline/scenario objects should be upadated somewhere!
  
  renamedModes <- c('victim', 'striking')
  
  # get dim position for distRoadType (dimForRatio) in baseline/scenario. 
  # TODO: check if dim positions in baseline/scenario are equal?
  
  distByRoadDimPosition <- match(dimForRatio, names(baseline@parameters@distRoadType))
  
  # get dim position for distRoadType (dimForRatio) in safetyInNumbers
  
  safetyInNumbersDimPosition <- match(dimForRatio, names(safetyInNumbers))
  
  # get dim position for mode (dimForMode) in baseline/scenario.
  
  modeDimPosition <- match(dimForMode, names(baseline@parameters@distRoadType))
  
  # TODO: distByRoadDimPosition must be equal safetyInNumbersDimPosition.
  # This is obligatory on input level - so question: if any check is needed here?
  
  # person part rename mode dim
  
  personPartNameOfDims <- names(baseline@parameters@distRoadType)
  personPartNameOfDims[modeDimPosition] <- renamedModes[1]
  
  # remove NA and dim(dimForRatio) which is going to be reduced
  
  personPartNameOfDims <- personPartNameOfDims[!(is.na(personPartNameOfDims) | personPartNameOfDims %in% dimForRatio)]
  
  # person part calcs
  
  personPartBaseline <- abind::asub(baseline@parameters@distRoadType, personIdx, distByRoadDimPosition)
  personPartScenario <- abind::asub(scenario@parameters@distRoadType, personIdx, distByRoadDimPosition)
  personPartSafetyInNumbers <- abind::asub(safetyInNumbers, personIdx, safetyInNumbersDimPosition)
  
  personPart <- (personPartScenario / personPartBaseline) ^ personPartSafetyInNumbers
  
  # vehicle part rename mode dim
  
  vehiclePartNameOfDims <- names(baseline@parameters@distRoadType)
  vehiclePartNameOfDims[modeDimPosition] <- renamedModes[2]
  
  # remove NA and dim(dimForRatio) which is going to be reduced
  
  vehiclePartNameOfDims <- vehiclePartNameOfDims[!(is.na(vehiclePartNameOfDims) | vehiclePartNameOfDims %in% dimForRatio)]
  
  # vehicle part calcs
  
  vehiclePartBaseline <- abind::asub(baseline@parameters@distRoadType, vehicleIdx, distByRoadDimPosition)
  vehiclePartScenario <- abind::asub(scenario@parameters@distRoadType, vehicleIdx, distByRoadDimPosition)
  vehiclePartSafetyInNumbers <- abind::asub(safetyInNumbers, vehicleIdx, safetyInNumbersDimPosition)
  
  vehiclePart <- (vehiclePartScenario / vehiclePartBaseline) ^ vehiclePartSafetyInNumbers
  
  # create outer product of personPart and vehiclePart
  # In the results only "diagonal" cells for non-modes dimension are important. The rest is a garbage, but the structure of
  # array is exactly the same as further data inputs.
  # The first mode-like dimension is victim mode, while the second mode-like dimension is striking mode
  
  outputArray <- personPart %o% vehiclePart
  
  # hack - more in helperCreateArray()
  
  names(outputArray) <- c(personPartNameOfDims, vehiclePartNameOfDims)
  
  return(outputArray)
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
readRoadInjuries <- function(ITHIM.object, file){
  
  # TODO: how to deal with double roadType column
  
  # get already used ("input") variables with unique entries
  
  inputDims <- ITHIM.object@parameters@inputDims
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array and update inputDims
  
  arrayAndDims <- helperCreateArray(inputData, inputDims)
  
  # return updated object
  
  ITHIM.updated <- update(ITHIM.object, list(roadInjuries = arrayAndDims$createdArray, inputDims = arrayAndDims$inputDims))
  
  return(ITHIM.updated)
}
#' @export
createSINmatrix <- function(modeNames){
    victimVec <- c(0.432,0.511,rep(0.4999,5))
    strikingVec <- victimVec
    NOVVec <- c(0.64,0.64,rep(0.8,5))
    sinMatrix <- matrix(c(victimVec,strikingVec,NOVVec), nrow = length(modeNames), ncol = 3, dimnames = list(modeNames, c("victim","striking","NOV")))
    print(sinMatrix)
    return(sinMatrix)
}
#' @export
helperSymDiff <- function(a, b){
  # this is just symmetric difference
  return(setdiff(union(a,b), intersect(a,b)))
}
#' @export
helperCreateArray <- function(inputData, inputDims){
  
  # column with values (TODO: could be defined as param?)
  
  columnWithValues <- c("value")
  
  # all columns except this with values
  
  columnsWithVariables <- setdiff(colnames(inputData), columnWithValues)
  
  # rearrange columns in inputData to match columnsWithVariables, than column with the values at the end
  
  inputData <- inputData[c(columnsWithVariables, columnWithValues)]
  
  # for every column
  
  for (colname in columnsWithVariables){
    
    # get unique entries. Sort is used to be sure that every future array would have identically defined dims
    
    uniqueEntires <- sort(unique(subset(inputData, select = colname, drop = T)))
    
    # check if variable (column with the same name) was already processed (thus it is in inputDims) and
    # it has exactly the same list of entires. If not -> error.
    # In other case add variable with entires to inputDims
    
    if ((colname %in% names(inputDims)) &&
        length(helperSymDiff(inputDims[[colname]], uniqueEntires)) > 0){
      
      stop(paste0("readDistByRoadType: column has different entries than one read previously: ", colname))
      
    } else {
      
      inputDims[[colname]] <- uniqueEntires
      
    }
    
  }
  
  # if everything ok at this point -> create array
  
  # only needed "variables" (columns)
  
  neededColumnsWithVariables <- inputDims[columnsWithVariables]
  
  # create array using needed columns with unique entries as definition of dims
  
  outputArray <- array(NA,
                       unname(sapply(neededColumnsWithVariables, function(x) length(x), simplify = T)),
                       unname(neededColumnsWithVariables))
  
  # TODO: other way to pass name of the dims? 
  # This is hack to pass name of columns (variables) to outputArray.
  # It could be set by attr() command, but "name attr" is better since it is not lost during array modification.
  
  names(outputArray) <- names(neededColumnsWithVariables)
  
  # iterate over input file -> fill every cell of array
  
  for (row in seq_len(nrow(inputData))){
    
    rowValues <- inputData[row, , drop = FALSE]
    
    # thanks to trick with matrix used to subset, it is possible to address particular cell using vector with particular combination of entries
    
    outputArray[matrix(unname(unlist(rowValues[columnsWithVariables])), nrow = 1)] <- unname(unlist(rowValues[columnWithValues]))
    
  }
  
  # return as list 
  
  return(list(createdArray = outputArray, inputDims = inputDims))
}
#' @export
readDistByRoadType <- function(ITHIM.object, file){
  
  # get already used ("input") variables with unique entries
  
  inputDims <- ITHIM.object@parameters@inputDims
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array and update inputDims
  
  arrayAndDims <- helperCreateArray(inputData, inputDims)
  
  # return updated object
  
  ITHIM.updated <- update(ITHIM.object, list(distRoadType = arrayAndDims$createdArray, inputDims = arrayAndDims$inputDims))
  
  return(ITHIM.updated)
  
}
#' @export
readSafetyInNumbers <- function(ITHIM.object, file){
  
  # get already used ("input") variables with unique entries
  
  inputDims <- ITHIM.object@parameters@inputDims
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array and update inputDims
  
  arrayAndDims <- helperCreateArray(inputData, inputDims)
  
  # return updated object
  
  ITHIM.updated <- update(ITHIM.object, list(safetyInNumbers = arrayAndDims$createdArray, inputDims = arrayAndDims$inputDims))
  
  return(ITHIM.updated)
  
}