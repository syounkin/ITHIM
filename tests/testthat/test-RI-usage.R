library(ITHIM)
context("Testing RI usage (typical flow)")

test_that("calculation of scenario injuries causes no error. 2 dimensions (roadType, mode), without NOV", {
  
  # init ITHIM baseline and scenario using data for helper
  
  # this part can be prone to errors since createITHIM() by default loads SafetyInNumbers and DistByRoadType Baseline from /inst .
  # In case of lack of these files, test won't work
  
  ITHIM.baseline <- createITHIM()
  
  # get SafetyInNumbers and DistByRoadType Baseline data from helper, not file (this is why read* methods are not used)
  
  ITHIM.baseline <- update(ITHIM.baseline, list(distRoadType = helperCreateArray(excelDistByRoadTypeBaselineWithoutNOV)$createdArray))
  
  ITHIM.baseline <- update(ITHIM.baseline, list(safetyInNumbers = helperCreateArray(excelSafetyInNumbersWithoutNOV)$createdArray))
  
  # Scenario DistByRoadType
  
  ITHIM.scenario <- update(ITHIM.baseline, list(distRoadType = helperCreateArray(excelDistByRoadTypeScenarioWithoutNOV)$createdArray))
  
  # RoadInjuries for baseline

  ITHIM.baseline <- update(ITHIM.baseline, list(roadInjuries = helperCreateArray(excelRoadInjuriesBaselineWithoutNOV)$createdArray))
  
  # RoadInjuries for scenario using scenario multiplier and baseline RoadInjuries
  
  ITHIM.scenario <- updateRoadInjuries(ITHIM.baseline, ITHIM.scenario)
  
  # prepare RoadInjuries data from Excel model - create array
  
  roadInjuriesFromExcel <- helperCreateArray(excelRoadInjuriesScenarioWithoutNOV)$createdArray
  
  # test data
  
  expect_equal(ITHIM.scenario@parameters@roadInjuries, roadInjuriesFromExcel, tolerance = 0.0002)
  
  # test RR data
  
  RR <- computeInjuryRR(ITHIM.baseline,ITHIM.scenario)
  
  # compare with the results from Excel model
  
  expect_equal(RR[["severity" = "fatal"]], c(1.044543), tolerance = 0.0001)
  expect_equal(RR[["severity" = "serious"]], c(1.046931), tolerance = 0.0001)
  
})

test_that("calculation of scenario injuries causes no error. 3 dimensions (ageGroup [reduced - only diagonal results: ag1-ag1, ag2-ag2, without ag2-ag1, ag1-ag2], roadType, mode), without NOV", {
  
  # init ITHIM baseline and scenario using data for helper
  
  # use new data and data from the 2 dimensions test, just adjust it to ageGroup dimension (ag1 - new data, ag2 - old data):
  
  # * SiN
  
  threeDimsSafetyInNumbers <- excelSafetyInNumbersWithoutNOV[rep(row.names(excelSafetyInNumbersWithoutNOV), times = 2),]
  rownames(threeDimsSafetyInNumbers) <- NULL
  threeDimsSafetyInNumbers$ageGroup <- rep(c("ag1", "ag2"), each = (nrow(threeDimsSafetyInNumbers)/2) )
  threeDimsSafetyInNumbers <- threeDimsSafetyInNumbers[c("distType", "ageGroup", "roadType", "mode", "value")]
  
  # * distRoadTypeBaseline
  
  threeDimsDistRoadTypeBaseline <- excelDistByRoadTypeBaselineWithoutNOV  # old data without ageGroup column
  threeDimsDistRoadTypeBaseline$ageGroup <- c("ag2")
  threeDimsDistRoadTypeBaseline <- threeDimsDistRoadTypeBaseline[c("distType", "ageGroup", "roadType", "mode", "value")]
  threeDimsDistRoadTypeBaseline <- rbind(threeDimsDistRoadTypeBaseline, excelDistByRoadTypeBaselineAgeGroupsWithoutNOV)
  
  # * distRoadTypeScenario
  
  threeDimsDistRoadTypeScenario <- excelDistByRoadTypeScenarioWithoutNOV  # old data without ageGroup column
  threeDimsDistRoadTypeScenario$ageGroup <- c("ag2")
  threeDimsDistRoadTypeScenario <- threeDimsDistRoadTypeScenario[c("distType", "ageGroup", "roadType", "mode", "value")]
  threeDimsDistRoadTypeScenario <- rbind(threeDimsDistRoadTypeScenario, excelDistByRoadTypeScenarioAgeGroupsWithoutNOV)
  
  # * roadInjuries
  
  threeDimsRoadInjuriesBaseline <- excelRoadInjuriesBaselineWithoutNOV[rep(row.names(excelRoadInjuriesBaselineWithoutNOV), times = 2),]
  rownames(threeDimsRoadInjuriesBaseline) <- NULL
  threeDimsRoadInjuriesBaseline$victimAgeGroup <- rep(c("ag1", "ag2"), each = (nrow(threeDimsRoadInjuriesBaseline)/2) )
  threeDimsRoadInjuriesBaseline$strikingAgeGroup <- rep(c("ag1", "ag2"), each = (nrow(threeDimsRoadInjuriesBaseline)/2) )
  threeDimsRoadInjuriesBaseline <- threeDimsRoadInjuriesBaseline[c("severity", "victimAgeGroup", "victimRoadType", "victim", "strikingAgeGroup", 
                                                                                                     "strikingRoadType", "striking", "value")]
  
  # this part can be prone to errors since createITHIM() by default loads SafetyInNumbers and DistByRoadType Baseline from /inst .
  # In case of lack of these files, test won't work
  
  ITHIM.baseline <- createITHIM()
  
  # get SafetyInNumbers and DistByRoadType Baseline data from helper, not file (this is why read* methods are not used)
  
  ITHIM.baseline <- update(ITHIM.baseline, list(distRoadType = helperCreateArray(threeDimsDistRoadTypeBaseline)$createdArray))
  
  ITHIM.baseline <- update(ITHIM.baseline, list(safetyInNumbers = helperCreateArray(threeDimsSafetyInNumbers)$createdArray))
  
  # Scenario DistByRoadType
  
  ITHIM.scenario <- update(ITHIM.baseline, list(distRoadType = helperCreateArray(threeDimsDistRoadTypeScenario)$createdArray))
  
  # RoadInjuries for baseline
  
  ITHIM.baseline <- update(ITHIM.baseline, list(roadInjuries = helperCreateArray(threeDimsRoadInjuriesBaseline)$createdArray))
  
  # RoadInjuries for scenario using scenario multiplier and baseline RoadInjuries
  
  ITHIM.scenario <- updateRoadInjuries(ITHIM.baseline, ITHIM.scenario)
  
  # prepare Scenario RoadInjuries data from Excel model
  
  threeDimsRoadInjuriesScenarioAG1 <- excelRoadInjuriesScenarioWithAgeGroupWithoutNOV
  threeDimsRoadInjuriesScenarioAG1$victimAgeGroup <- c("ag1")
  threeDimsRoadInjuriesScenarioAG1$strikingAgeGroup <- c("ag1")
  threeDimsRoadInjuriesScenarioAG1 <- threeDimsRoadInjuriesScenarioAG1[c("severity", "victimAgeGroup", "victimRoadType", "victim", 
                                                                         "strikingAgeGroup", "strikingRoadType", 
                                                                         "striking", "value")]
  
  threeDimsRoadInjuriesScenarioAG2 <- excelRoadInjuriesScenarioWithoutNOV
  threeDimsRoadInjuriesScenarioAG2$victimAgeGroup <- c("ag2")
  threeDimsRoadInjuriesScenarioAG2$strikingAgeGroup <- c("ag2")
  threeDimsRoadInjuriesScenarioAG2 <- threeDimsRoadInjuriesScenarioAG2[c("severity", "victimAgeGroup", "victimRoadType", "victim", 
                                                                         "strikingAgeGroup", "strikingRoadType", 
                                                                         "striking", "value")]
  
  roadInjuriesFromExcel <- helperCreateArray(rbind(threeDimsRoadInjuriesScenarioAG1, threeDimsRoadInjuriesScenarioAG2))$createdArray
  
  # test data
  
  expect_equal(ITHIM.scenario@parameters@roadInjuries, roadInjuriesFromExcel, tolerance = 0.0002)
  
  # test RR data
  
  RR <- computeInjuryRR(ITHIM.baseline,ITHIM.scenario)
  
  # compare with the results from Excel model
  
  expect_equal(RR[["victimAgeGroup" = "ag1", "strikingAgeGroup" = "ag1", "severity" = "fatal"]], c(1.95281), tolerance = 0.0001)
  expect_equal(RR[["victimAgeGroup" = "ag2", "strikingAgeGroup" = "ag2", "severity" = "fatal"]], c(1.044523), tolerance = 0.0001)
  expect_equal(RR[["victimAgeGroup" = "ag1", "strikingAgeGroup" = "ag1", "severity" = "serious"]], c(2.07377), tolerance = 0.0001)
  expect_equal(RR[["victimAgeGroup" = "ag2", "strikingAgeGroup" = "ag2", "severity" = "serious"]], c(1.046925), tolerance = 0.0001)
  
})

### with NOV

test_that("calculation of scenario injuries causes no error. 2 dimensions (roadType, mode), with NOV", {
  
  # init ITHIM baseline and scenario using data for helper
  
  # this part can be prone to errors since createITHIM() by default loads SafetyInNumbers and DistByRoadType Baseline from /inst .
  # In case of lack of these files, test won't work
  
  ITHIM.baseline <- createITHIM()
  
  # get SafetyInNumbers and DistByRoadType Baseline data from helper, not file (this is why read* methods are not used)
  
  ITHIM.baseline <- update(ITHIM.baseline, list(distRoadType = helperCreateArray(excelDistByRoadTypeBaselineWithoutNOV)$createdArray))
  
  ITHIM.baseline <- update(ITHIM.baseline, list(safetyInNumbers = helperCreateArray(excelSafetyInNumbersWithoutNOV)$createdArray))
  
  # Scenario DistByRoadType
  
  ITHIM.scenario <- update(ITHIM.baseline, list(distRoadType = helperCreateArray(excelDistByRoadTypeScenarioWithoutNOV)$createdArray))
  
  # RoadInjuries for baseline
  
  ITHIM.baseline <- update(ITHIM.baseline, list(roadInjuries = helperCreateArray(excelRoadInjuriesBaselineWithNOV)$createdArray))
  
  # RoadInjuries for scenario using scenario multiplier and baseline RoadInjuries
  
  ITHIM.scenario <- updateRoadInjuries(ITHIM.baseline, ITHIM.scenario, add.NOV = TRUE)
  
  # prepare RoadInjuries data from Excel model - create array
  
  roadInjuriesFromExcel <- helperCreateArray(excelRoadInjuriesScenarioWithNOV)$createdArray
  
  # test data
  
  expect_equal(ITHIM.scenario@parameters@roadInjuries, roadInjuriesFromExcel, tolerance = 0.0002)
  
  # test RR data
  
  RR <- computeInjuryRR(ITHIM.baseline,ITHIM.scenario)
  
  # compare with the results from Excel model
  
  expect_equal(RR[["severity" = "fatal"]], c(1.001083), tolerance = 0.0001)
  expect_equal(RR[["severity" = "serious"]], c(1.02555), tolerance = 0.0001)
  
})
