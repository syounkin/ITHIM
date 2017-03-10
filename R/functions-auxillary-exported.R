#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieves the names of methods for ITHIM Objects
#'
#' Retrieves the names of methods for ITHIM Objects
#'
#' @return A character vector of method names
#' 
#' @export
getMethodsITHIM <- function()
{
    class <- "ITHIM"
    methods <-
      showMethods(classes = class, printTo = FALSE)
    methods <- methods[grep("^Function:", methods)]
    sapply(strsplit(methods, " "), "[", 2)
}
