library(ITHIM)
context("Testing helperCreateArray function")

test_that("DF should have a functional column storing values (defined via columnWithValues, default: 'value')", {
  
  # without 'value' column
  
  csvContent <- "distType,roadType,mode,val
                victim,local,bus,0.5
                victim,local,car,0.5"
  
  inputData <- read.csv(text = csvContent, stringsAsFactors = F)
  
  expect_error(helperCreateArray(inputData), "undefined columns selected")
  
  # with 'value' column
  
  csvContent <- paste0("distType,roadType,mode,", columnWithValues,"
                victim,local,bus,0.5
                striking,arterial,car,0.5")
  
  inputData <- read.csv(text = csvContent, stringsAsFactors = F)
  
  producedArray <- helperCreateArray(inputData)
  
  expect_match(names(producedArray), c("createdArray"))
  
})

test_that("rows (indices) order doesn't matter, because indices are sorted", {
  
  csvContent1 <- paste0("distType,roadType,mode,", columnWithValues,"
                victim,local,bus,0.5
                striking,local,car,0.5
                striking,arterial,truck,0.5")
  
  inputData1 <- read.csv(text = csvContent1, stringsAsFactors = F)
  
  producedArray1 <- helperCreateArray(inputData1)
  
  csvContent2 <- csvContent1 <- paste0("distType,roadType,mode,", columnWithValues,"
                striking,arterial,truck,0.5
                victim,local,bus,0.5
                striking,local,car,0.5")
  
  inputData2 <- read.csv(text = csvContent2, stringsAsFactors = F)
  
  producedArray2 <- helperCreateArray(inputData2)
  
  expect_identical(producedArray1, producedArray2)
  
})

test_that("position of functional column (in this case storing values) doesn't matter since it is not used to create dimensions/indices only to fill it with values", {
  
  # functional column as the first column 
  
  csvContent1 <- paste0(columnWithValues, ",distType,roadType,mode
                0.1,victim,local,bus
                0.2,striking,local,car
                0.3,striking,arterial,truck")
  
  inputData1 <- read.csv(text = csvContent1, stringsAsFactors = F)
  
  producedArray1 <- helperCreateArray(inputData1)
  
  # functional column as the third column 
  
  csvContent2 <- paste0("distType,roadType,", columnWithValues, ",mode
                victim,local,0.1,bus
                striking,local,0.2,car
                striking,arterial,0.3,truck")
  
  inputData2 <- read.csv(text = csvContent2, stringsAsFactors = F)
  
  producedArray2 <- helperCreateArray(inputData1)
  
  expect_identical(producedArray1, producedArray2)
  
})

# names(outputArray)

# check import 3, 4, 10 columns

# test_that("if processed DF is very small, dimnames can't be stored as `names` attr", {
#   csvContent <- "distType,roadType,mode,value
#                 victim,local,bus,0.5
#                 victim,local,car,0.5"
#   
#   inputDF <- read.csv(text = csvContent, stringsAsFactors = F)
#   
#   expect_error(helperCreateArray(inputDF), "undefined columns selected")
# })