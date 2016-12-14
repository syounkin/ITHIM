#' @export
setMethod("show", signature(object="ITHIM"), function(object){
  cat("Hello vargo.\n", sep = "")
})

#' @export
setMethod("plot", signature(x = "ITHIM"), function(x){
  barplot(x@parameters$Rwt)
})
