## #' @export
computeRoadInjuryBurden <- function(ITHIM.baseline, ITHIM.scenario){
  injuryRR <- computeInjuryRR(getRoadInjuries(ITHIM.baseline), getRoadInjuries(ITHIM.scenario))
  GBD <- getGBD(ITHIM.baseline)
  RTI.GBD <- RTI.GBD[RTI.GBD$disease == "RTIs",]
  RIburden <- data.frame(ageClass = RTI.GBD$ageClass, sex = RTI.GBD$sex, burden = RTI.GBD$variable,delta = RTI.GBD$value*(1-injuryRR$Fatal))
  return(RIburden)
}
## #' @export
updateRoadInjuries <- function(ITHIM.baseline, ITHIM.scenario, add.NOV = FALSE){
  
  ITHIM.scenario <- update(ITHIM.scenario, list(roadInjuries = multiplyInjuries(ITHIM.baseline, ITHIM.scenario, add.NOV = add.NOV)))
  
return(ITHIM.scenario)
}
## #' @export
readRoadInjuries <- function(file){
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array
  
  producedArray <- helperCreateArray(inputData)
  
  # return producedArray
  
  return(producedArray$createdArray)
  
}
## #' @export
readDistByRoadType <- function(file){
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array
  
  producedArray <- helperCreateArray(inputData)
  
  # return producedArray
  
  return(producedArray$createdArray)
  
}
## #' @export
readSafetyInNumbers <- function(file){
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array and update inputDims
  
  producedArray <- helperCreateArray(inputData)
  
  # return producedArray
  
  return(producedArray$createdArray)
  
}
