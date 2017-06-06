#' @rdname getPopulation-methods
#' @aliases getPopulation
#' @export
setMethod("getPopulation", signature(state = "vector", county = "vector"), function(state, county){
    return(Reduce("+",mapply(getTractAgeSex, state = state, county = county, SIMPLIFY = FALSE)))
})
