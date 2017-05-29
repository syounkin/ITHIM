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
  
  distByRoadDimPosition <- match(dimForRatio, names(dimnames(baseline@parameters@distRoadType)))
  
  # get dim position for distRoadType (dimForRatio) in safetyInNumbers
  
  safetyInNumbersDimPosition <- match(dimForRatio, names(dimnames(safetyInNumbers)))
  
  # get dim position for mode (dimForMode) in baseline/scenario.
  
  modeDimPosition <- match(dimForMode, names(dimnames(baseline@parameters@distRoadType)))
  
  # person part rename mode dim
  
  personPartNameOfDims <- names(dimnames(baseline@parameters@distRoadType))
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
  
  vehiclePartNameOfDims <- names(dimnames(baseline@parameters@distRoadType))
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

  # rename dimnames to included value from renamedModes
  
  names(dimnames(outputArray)) <- c(personPartNameOfDims, vehiclePartNameOfDims)
  
  return(outputArray)
}
computeInjuryRR <- function(RI.baseline, RI.scenario){
  
  victimsBaseline <- computeTotalVictimsBySeverity(RI.baseline@parameters@roadInjuries)
  
  victimsScenario <- computeTotalVictimsBySeverity(RI.scenario@parameters@roadInjuries)
  
  # check if arrays have same structure
  
  if(!helperCheckIfArraysHaveSameDims(victimsBaseline, victimsScenario)){
    stop("computeInjuryRR: different dims")
  }
  
  return(victimsScenario/victimsBaseline)
  
}
computeTotalVictimsBySeverity <- function(RI){
  
  # which dim stores severity. TODO: could be set via params?
  
  dimForSeverity <- c('severity')
  
  # which dims store roadType for victim and striking. TODO: could be set via params?
  
  dimsForRoadType <- c("victimRoadType", "strikingRoadType")
  
  # which dims store victim and striking values. TODO: could be set via params?
  
  dimsForValues <- c("victim", "striking")
  
  # core function which calculates victims by severity
  
  calcVictimsBySeverity <- function(RI){
    
    # get dim position for severity (dimForSeverity) in RI
    
    severityDimPosition <- match(dimForSeverity, names(dimnames(RI)))
    
    # get all indices for roadInjuries. It should be unique already!
    
    severityIndices <- dimnames(RI)[[dimForSeverity]]
  
    # verify if indices for roadType (for victim and striking) are exactly the same
    
    if(!all.equal(sort(dimnames(RI)[[dimsForRoadType[1]]]), sort(dimnames(RI)[[dimsForRoadType[2]]]))){
      
      stop("computeTotalVictimsByMode: different indices for roadType: victim vs. striking")
    }
    
    # indices for roadType - if passed above test, it doesn't matter which is used victimRoadType/strikingRoadType
    
    roadTypesIndices <- unname(dimnames(RI)[[dimsForRoadType[1]]])
    
    # output as array
    
    outputVictimsArray <- array(NA,
                                dim = length(severityIndices),
                                dimnames = dimnames(RI)[dimForSeverity])
    
    # iterate over severity indices
    
    for (idx in severityIndices){
      
      # extract particular severity RI
      
      severityData <- abind::asub(RI, idx, severityDimPosition)
      
      # init reduced roadType
      
      reducedRoadType <- NULL
      
      # iterate over roadTypes
      
      for (roadType in roadTypesIndices){
        
        # for roadType only diagonal values are proper
        
        tempOut <- abind::asub(severityData, list(roadType, roadType), match(dimsForRoadType, names(dimnames(severityData))))
        
        # sum victims over all roadTypes
        
        if(is.null(reducedRoadType)){
          reducedRoadType <- tempOut
        } else {
          reducedRoadType <- reducedRoadType + tempOut
        }
        
      }
      
      # IF INPUT DATA IS CORRECT SO IT HAS ONLY DIAGONAL VALUES FOR ROADTYPES (LIKE LOCAL - LOCAL, NOT LOCAL - ARTERIAL), THEN
      # ABOVE CODE COULD BE REPLACED WITH SIMPLE SUM: sum(severityData, na.rm = T) because not diagonal should equal NA
      
      outputVictimsArray[[idx]] <- sum(reducedRoadType)
      
    }
    
    return(outputVictimsArray)
  }
  
  # check if there are any extra dims (not functional)

  extraDims <- setdiff(names(dimnames(RI)), c(dimForSeverity, dimsForRoadType, dimsForValues))

  if(length(extraDims) > 0){
    
    # if there are any extra dims - every combination of indices from different extra dims should be calculated separately and the results
    # should be combined into one output array
    
    allIndicesCombinationOfExtraDims <- expand.grid(dimnames(RI)[extraDims], stringsAsFactors = F)
    
    # dims (+ all indices) for extra dims and severity
    
    extraDimsAndSeverity <- append(dimnames(RI)[extraDims], dimnames(RI)[dimForSeverity])
    
    # output array
    
    outputArray <- array(NA,
                         dim = sapply(extraDimsAndSeverity, function(x) length(x), simplify = T),
                         dimnames = extraDimsAndSeverity)
    
    # iterate over every combination
    
    for (i in seq_len(nrow(allIndicesCombinationOfExtraDims))){
      
      comb <- unlist(allIndicesCombinationOfExtraDims[i, ])
      
      # extract particular combination of indices (different dims)
      
      combArray <- abind::asub(RI, setNames(as.list(unname(comb)), names(comb)), match(names(comb), names(dimnames(RI))))
      
      combResult <- calcVictimsBySeverity(combArray)
      
      # create list of extra dims (with indices) + severity
      # VERY IMPORTANT: dimForSeverity at the end
      
      combDimnames <- append(comb, dimnames(RI)[dimForSeverity])
      
      # little hack: create array using values from the flatten results of calcTemp(combArray), thus order
      # of dims matters
      
      combOutputArray <- array(unname(unlist(as.list(combResult))[combDimnames[[dimForSeverity]]]),
                               dim = sapply(combDimnames, function(x) length(x), simplify = T),
                               dimnames = combDimnames)
      
      # save results
      
      abind::afill(outputArray) <- combOutputArray

    }
    
    return(outputArray)
    
  } else {
    
    return(calcVictimsBySeverity(RI))
  }
}
multiplyInjuries <- function(ITHIM.baseline, ITHIM.scenario){
  
  # which dim stores severity. TODO: could be set via params?
  
  dimForSeverity <- c('severity')
  
  # get dim position for severity (dimForSeverity) in baseline (must be baseline!)
  
  severityDimPosition <- match(dimForSeverity, names(dimnames(ITHIM.baseline@parameters@roadInjuries)))
  
  # get all indices for roadInjuries. It should be unique already!
  
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
helperCheckIfArraysHaveSameDims <- function(array1, array2, check.dimnames = FALSE){
  
  # check if arrays
  
  if (!(is.array(array1) && is.array(array2))){
    return(FALSE)
  }
  
  # check def of dimensions
  
  if (!( (length(dim(array1)) == length(dim(array2))) && all(dim(array1) == dim(array2)) )){
    return(FALSE)
  }
  
  # should the names of dims be checked
  
  if(check.dimnames){
  
    # check indices and names of dims - if ok at this point -> return TRUE
    
    if(identical(dimnames(array1), dimnames(array2))){
      return(TRUE)
    }
    
  } else {
    
    # check indices - if ok at this point -> return TRUE
    
    if(identical(unname(dimnames(array1)), unname(dimnames(array2)))){
      return(TRUE)
    }
    
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
  
  # save names of dims with indices
  
  dimnames(outputArray) <- outputArrayDimsWithIndices
  
  # iterate over input file -> fill every cell of array
  
  for (row in seq_len(nrow(inputData))){
    
    rowValues <- inputData[row, , drop = FALSE]
    
    # thanks to trick with matrix used to subset, it is possible to address particular cell using vector with particular combination of entries
    
    outputArray[matrix(unname(unlist(rowValues[columnsWithVariables])), nrow = 1)] <- unname(unlist(rowValues[columnWithValues]))
    
  }
  
  # return as list 
  
  return(list(createdArray = outputArray))
}
