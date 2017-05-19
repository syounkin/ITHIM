#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Computes road injury multiplier
###
### This multiplier is used to estimate the road injury count in the
### scenario.  It is a function of distance by road type (person-miles
### and vehicle-miles) and safey in number parameters (one for
### person-miles and one for vehicle miles.  Currently both values are
### fixed at 0.5 for both person and vehicle.
###
### @param base a list containing two data frames (one for person-miles
###     and one for vehicle miles) indicating the distance travelled in
###     the baseline
###
### @param scenario Same as above but for the scenario.
###
### @return A list of multipliers.
###
computeMultiplier <- function(baseline, scenario, safetyInNumbers){
  
  # TODO: update doc
  
  # which dim should be used to calc ratio. TODO: could be set via params?
  
  dimForRatio <- c('distType')
  
  # which index represents person part in distRoadType. TODO: could be set via params?
  
  personIdx <- c('person')
  
  # which index represents vehicle part in distRoadType. TODO: could be set via params?
  
  vehicleIdx <- c('vehicle')
  
  # which index represents person part(/victim) in safetyInNumbers TODO: could be set via params?
  
  victimIdx <- c('victim')
  
  # which index represents vehicle part(/striking) in safetyInNumbers TODO: could be set via params?
  
  strikingIdx <- c('striking')
  
  # which dim represents modes and should be renamed to victim/striking. TODO: could be set via params?
  
  dimForMode <- c('mode')
  
  # output renamed mode victim, striking (in that order!).
  
  renamedModes <- c('victim', 'striking')
  
  #TODO: check dims!
  
  # get dim position for distRoadType (dimForRatio) in baseline/scenario. 
  
  distByRoadDimPosition <- match(dimForRatio, names(baseline@parameters@distRoadType))
  
  # get dim position for distRoadType (dimForRatio) in safetyInNumbers
  
  safetyInNumbersDimPosition <- match(dimForRatio, names(safetyInNumbers))
  
  # get dim position for mode (dimForMode) in baseline/scenario.
  
  modeDimPosition <- match(dimForMode, names(baseline@parameters@distRoadType))
  
  # person part rename mode dim
  
  personPartNameOfDims <- names(baseline@parameters@distRoadType)
  personPartNameOfDims[modeDimPosition] <- renamedModes[1]
  
  # remove NA and dim(dimForRatio) which is going to be reduced
  
  personPartNameOfDims <- personPartNameOfDims[!(is.na(personPartNameOfDims) | personPartNameOfDims %in% dimForRatio)]
  
  # person part calcs
  
  personPartBaseline <- abind::asub(baseline@parameters@distRoadType, personIdx, distByRoadDimPosition)
  personPartScenario <- abind::asub(scenario@parameters@distRoadType, personIdx, distByRoadDimPosition)
  personPartSafetyInNumbers <- abind::asub(safetyInNumbers, victimIdx, safetyInNumbersDimPosition)
  
  # check if needed arrays have same dims with exactly the same indices (even same order!)
  
  if(!helperCheckIfArraysHaveSameDims(personPartBaseline, personPartScenario)){
    stop('computeMultiplier: dims: Baseline-Scenario')
  }
  
  if(!helperCheckIfArraysHaveSameDims(personPartBaseline, personPartSafetyInNumbers)){
    stop('computeMultiplier: dims: Baseline-SafetyInNumbers')
  }
  
  # if ok - calc personPart
  
  personPart <- (personPartScenario / personPartBaseline) ^ personPartSafetyInNumbers
  
  # vehicle part rename mode dim
  
  vehiclePartNameOfDims <- names(baseline@parameters@distRoadType)
  vehiclePartNameOfDims[modeDimPosition] <- renamedModes[2]
  
  # remove NA and dim(dimForRatio) which is going to be reduced
  
  vehiclePartNameOfDims <- vehiclePartNameOfDims[!(is.na(vehiclePartNameOfDims) | vehiclePartNameOfDims %in% dimForRatio)]
  
  # vehicle part calcs
  
  vehiclePartBaseline <- abind::asub(baseline@parameters@distRoadType, vehicleIdx, distByRoadDimPosition)
  vehiclePartScenario <- abind::asub(scenario@parameters@distRoadType, vehicleIdx, distByRoadDimPosition)
  vehiclePartSafetyInNumbers <- abind::asub(safetyInNumbers, strikingIdx, safetyInNumbersDimPosition)
  
  # check is not needed here since vehicle part uses same matrices as person part
  
  vehiclePart <- (vehiclePartScenario / vehiclePartBaseline) ^ vehiclePartSafetyInNumbers
  
  # create outer product of personPart and vehiclePart
  # In the results only "diagonal" cells for non-modes dimensions are important. The rest is a garbage, but the structure of
  # array is exactly the same as further data inputs.
  # The first mode-like dimension is victim mode, while the second mode-like dimension is striking mode
  
  outputArray <- personPart %o% vehiclePart
  
  # hack with names(array) - more in helperCreateArray()
  
  names(outputArray) <- c(personPartNameOfDims, vehiclePartNameOfDims)
  
  return(outputArray)
}
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
multiplyInjuries <- function(ITHIM.baseline, ITHIM.scenario){
  
  # which dim stores severity. TODO: could be set via params?
  
  dimForSeverity <- c('severity')
  
  # get dim position for severity (dimForSeverity) in baseline (must be baseline!)
  
  severityDimPosition <- match(dimForSeverity, names(ITHIM.baseline@parameters@roadInjuries))
  
  # get all indices for roadInjuries. It should unique already!
  
  severityIndices <- dimnames(ITHIM.baseline@parameters@roadInjuries)[[severityDimPosition]]
  
  # compute multiplier for scenario

  multiplier <- computeMultiplier(baseline = ITHIM.baseline,
                                  scenario = ITHIM.scenario,
                                  safetyInNumbers = ITHIM.baseline@parameters@safetyInNumbers)

  # create output array. Workaround - abind is used (instead of afill), so output array must be created,
  # without any indices in severity dimension (these would be added via abind)
  
  outputArrayDimensionWithIndices <- dimnames(ITHIM.baseline@parameters@roadInjuries)
  
  outputArrayDimensionWithIndices[[severityDimPosition]] <- vector()
  
  outputArray = array(NA,
                      dim = unname(sapply(outputArrayDimensionWithIndices, function(x) length(x), simplify = T)),
                      dimnames = unname(outputArrayDimensionWithIndices))
  
  # iterate over severity indices
  
  for (idx in severityIndices){
    
    # extract particular severity from baseline roadInjuries
    
    severityData <- abind::asub(ITHIM.baseline@parameters@roadInjuries, idx, severityDimPosition)
    
    # check if arrays have same structure
    
    if(!helperCheckIfArraysHaveSameDims(severityData, multiplier)){
      stop(paste0('multiplyInjuries: severityData-multiplier: index: ', idx))
    }
    
    # baseline * multiplier
    
    outputData <- severityData * multiplier
    
    # add to output array
    
    outputArray <- abind(outputArray, outputData, along = severityDimPosition)
    
  }
  
  # bacause of workaround - add indices for severity manually 
  
  dimnames(outputArray) <- dimnames(ITHIM.baseline@parameters@roadInjuries)

  return(outputArray)

}
helperCheckIfArraysHaveSameDims <- function(array1, array2){
  
  # check if arrays
  
  if (!(is.array(array1) && is.array(array2))){
    return(FALSE)
  }
  
  # check def of dimensions
  
  if (!( (length(dim(array1)) == length(dim(array2))) && all(dim(array1) == dim(array2)) )){
    return(FALSE)
  }
  
  # check indices - if ok at this point -> return TRUE
  
  if(identical(dimnames(array1), dimnames(array2))){
    return(TRUE)
  }
  
  return(FALSE)
}
helperCreateArray <- function(inputData){
  
  # column with values (TODO: could be defined as param?)
  
  columnWithValues <- c("value")
  
  # all columns except this with values
  
  columnsWithVariables <- setdiff(colnames(inputData), columnWithValues)
  
  # rearrange columns in inputData to match columnsWithVariables, than column with the values at the end
  
  inputData <- inputData[c(columnsWithVariables, columnWithValues)]
  
  # list which should stored names of future dimension with sorted values (future indices)
  
  outputArrayDimsWithIndices <- as.list(setNames(columnsWithVariables, columnsWithVariables))
  
  # get unique entries. Sort is used to be sure that every future array would have identically defined dims
  
  outputArrayDimsWithIndices <- sapply(outputArrayDimsWithIndices, function(x, y) {sort(unique(subset(y, select = x, drop = T)))}, y = inputData, simplify = F)

  # create array using needed columns with unique entries as definition of dims
  
  outputArray <- array(NA,
                       unname(sapply(outputArrayDimsWithIndices, function(x) length(x), simplify = T)),
                       unname(outputArrayDimsWithIndices))
  
  # TODO: other way to pass name of the dims? 
  # This is hack to pass name of columns (variables) to outputArray.
  # It could be set by attr() command, but "name attr" is better since it is not lost during array modification.

  names(outputArray) <- names(outputArrayDimsWithIndices)
  
  # iterate over input file -> fill every cell of array
  
  for (row in seq_len(nrow(inputData))){
    
    rowValues <- inputData[row, , drop = FALSE]
    
    # thanks to trick with matrix used to subset, it is possible to address particular cell using vector with particular combination of entries
    
    outputArray[matrix(unname(unlist(rowValues[columnsWithVariables])), nrow = 1)] <- unname(unlist(rowValues[columnWithValues]))
    
  }
  
  # return as list 
  
  return(list(createdArray = outputArray))
}
