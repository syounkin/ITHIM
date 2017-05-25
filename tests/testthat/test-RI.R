library(ITHIM)
context("Testing helper functions")

test_that("helperCreateArray: DF should have a functional column storing values (defined via columnWithValues, default: 'value')", {
  
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

test_that("helperCreateArray: rows (indices) order doesn't matter, because indices are sorted", {
  
  csvContent1 <- paste0("distType,roadType,mode,", columnWithValues,"
                victim,local,bus,0.5
                striking,local,car,0.5
                striking,arterial,truck,0.5")
  
  inputData1 <- read.csv(text = csvContent1, stringsAsFactors = F)
  
  producedArray1 <- helperCreateArray(inputData1)
  
  csvContent2 <- paste0("distType,roadType,mode,", columnWithValues,"
                striking,arterial,truck,0.5
                victim,local,bus,0.5
                striking,local,car,0.5")
  
  inputData2 <- read.csv(text = csvContent2, stringsAsFactors = F)
  
  producedArray2 <- helperCreateArray(inputData2)
  
  expect_identical(producedArray1, producedArray2)
  
})

test_that("helperCreateArray: position of functional column (in this case storing values) doesn't matter since it is not used to create dimensions/indices only to fill it with values", {
  
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
  
  producedArray2 <- helperCreateArray(inputData2)
  
  expect_identical(producedArray1, producedArray2)
  
})

test_that("helperCheckIfArraysHaveSameDims: names of dims of tested arrays are not used if check.dimnames = FALSE (default value)", {
  
  csvContent1 <- "distTypeAAAAAAAAA,roadType,value,mode
                victim,local,0.1,bus
                striking,local,0.2,car
                striking,arterial,0.3,truck"
  
  inputData1 <- read.csv(text = csvContent1, stringsAsFactors = F)
  
  producedArray1 <- helperCreateArray(inputData1)
  
  # functional column as the third column 
  
  csvContent2 <- "distType,roadType,value,mode
                victim,local,0.1,bus
                striking,local,0.2,car
                striking,arterial,0.3,truck"
  
  inputData2 <- read.csv(text = csvContent2, stringsAsFactors = F)
  
  producedArray2 <- helperCreateArray(inputData2)
  
  expect_true(helperCheckIfArraysHaveSameDims(producedArray1$createdArray, producedArray2$createdArray))
  
  expect_true(helperCheckIfArraysHaveSameDims(producedArray1$createdArray, producedArray2$createdArray, check.dimnames = FALSE))
})

test_that("helperCheckIfArraysHaveSameDims: names of dims of tested arrays are used if check.dimnames = TRUE", {
  
  csvContent1 <- "distTypeAAAAAAAAA,roadType,value,mode
                victim,local,0.1,bus
                striking,local,0.2,car
                striking,arterial,0.3,truck"
  
  inputData1 <- read.csv(text = csvContent1, stringsAsFactors = F)
  
  producedArray1 <- helperCreateArray(inputData1)
  
  # functional column as the third column 
  
  csvContent2 <- "distType,roadType,value,mode
                victim,local,0.1,bus
                striking,local,0.2,car
                striking,arterial,0.3,truck"
  
  inputData2 <- read.csv(text = csvContent2, stringsAsFactors = F)
  
  producedArray2 <- helperCreateArray(inputData2)
  
  expect_false(helperCheckIfArraysHaveSameDims(producedArray1$createdArray, producedArray2$createdArray, check.dimnames = TRUE))
  
  csvContent1 <- "distType,roadType,value,mode
                victim,local,0.1,bus
                striking,local,0.2,car
                striking,arterial,0.3,truck"
  
  inputData1 <- read.csv(text = csvContent1, stringsAsFactors = F)
  
  producedArray1 <- helperCreateArray(inputData1)
  
  # functional column as the third column 
  
  csvContent2 <- "distType,roadType,value,mode
                victim,local,0.1,bus
                striking,local,0.2,car
                striking,arterial,0.3,truck"
  
  inputData2 <- read.csv(text = csvContent2, stringsAsFactors = F)
  
  producedArray2 <- helperCreateArray(inputData2)
  
  expect_true(helperCheckIfArraysHaveSameDims(producedArray1$createdArray, producedArray2$createdArray, check.dimnames = TRUE))
})

test_that("helperCreateArray: it should work for very small DF - improvement: previously dimnames were stored as `names` attr,
          so it didn't work in case if number of rows < number of dims", {
  csvContent <- "distType,roadType,mode,value
victim,local,bus,0.5
victim,local,car,0.7"

  inputDF <- read.csv(text = csvContent, stringsAsFactors = F)
  
  # test is prone to different order of column (caused for example by different LOCALE)
  
  valuePosition <- c("distType" = "victim", "mode" = "car", "roadType" = "local")

  expect_equal(helperCreateArray(inputDF)$createdArray[matrix(unname(valuePosition[names(dimnames(helperCreateArray(inputDF)$createdArray))]), nrow=1)], c(0.7))
})