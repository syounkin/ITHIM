#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plots a RR matrix
#'
#' Plots a RR matrix
#'
#' @return A ggplot object
#'
#' @export
plotRR <- function(RR.baseline,RR.scenario){
D <- melt(list(baseline = RR.baseline, scenario = RR.scenario), c("age","quint"), value.name = "RR")
D <- subset(D, !(age %in% paste0("ageClass",1:2)))
names(D) <- c("age","quint","RR","sex","vision")
p <- ggplot(D, aes(age,  RR)) + geom_bar(aes(fill=vision), stat = "identity", position = "dodge")
p <- p + facet_grid( quint ~ sex )
return(p)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' This function coverts the 2001 CDC Age variable to years.
#'
#' @param value The value for age, i.e., characters 2-4 of the code
#'     (sloppy with class!)
#' @param unit The units for age, i.e., chatater 1 of the code (sloppy
#'     with class!)
#' @note This function is likely only applicable to the 2001 data
#'     file.
#' @return A numeric vector of age in years - this is a test
#'
#' @export
convertAge <- function(value, unit){

    value <- ifelse(value == "999", NA, value)

    convertedValue <- ifelse( unit == "1", value,
    ifelse( unit == "2", value/12,
    ifelse( unit == "4", value/365,
    ifelse( unit == "5", value/365/24,
    ifelse( unit == "6", value/365/24/60,
    ifelse( unit == "9", NA, 999 ))))))

    return(as.numeric(convertedValue))

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Converts age to ITHIM age class
#'
#' Converts age to ITHIM age class
#'
#' @param age A vector of ages.  This vector will be coerced to numeric.
#'
#' @return A character vector of ITHIM age categories
#'
#' @export
convertToAgeClass <- function(age){
  age <- as.numeric(age)
  agecat <- ifelse( age <= 4, "00-04", ifelse( age <= 14, "05-14", ifelse( age <= 29, "15-29", ifelse( age <= 44, "30-44", ifelse( age <= 59, "45-59", ifelse( age <= 69, "60-69", ifelse( age <= 79, "70-79", ifelse( age > 80, "80+", NA))))))))
  return(agecat)
}
#'@export
s4Methods <- function(class)
{
    methods <-
      showMethods(classes = class, printTo = FALSE)
    methods <- methods[grep("^Function:", methods)]
    sapply(strsplit(methods, " "), "[", 2)
}

#'@export
getMethodsITHIM <- function()
{
    class <- "ITHIM"
    methods <-
      showMethods(classes = class, printTo = FALSE)
    methods <- methods[grep("^Function:", methods)]
    sapply(strsplit(methods, " "), "[", 2)
}
