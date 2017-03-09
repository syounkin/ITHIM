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
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array
  
  producedArray <- helperCreateArray(inputData)
  
  # return updated object
  
  ITHIM.updated <- update(ITHIM.object, list(roadInjuries = producedArray$createdArray))
  
  return(ITHIM.updated)
}
#' @export
readDistByRoadType <- function(ITHIM.object, file){
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array
  
  producedArray <- helperCreateArray(inputData)
  
  # return updated object
  
  ITHIM.updated <- update(ITHIM.object, list(distRoadType = producedArray$createdArray))
  
  return(ITHIM.updated)
  
}
#' @export
readSafetyInNumbers <- function(ITHIM.object, file){
  
  # read file
  
  inputData <- read.csv(file, stringsAsFactors = FALSE)
  
  # create array and update inputDims
  
  producedArray <- helperCreateArray(inputData)
  
  # return updated object
  
  ITHIM.updated <- update(ITHIM.object, list(safetyInNumbers = producedArray$createdArray))
  
  return(ITHIM.updated)
  
}
