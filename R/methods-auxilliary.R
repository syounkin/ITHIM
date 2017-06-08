#' @rdname getPopulation-methods
#' @aliases getPopulation
#' @export
setMethod("getPopulation", signature(msa = "missing", state = "vector", county = "vector"), function(msa, state, county){
    return(Reduce("+",mapply(getTractAgeSex, state = state, county = county, SIMPLIFY = FALSE)))
})
#' @rdname getPopulation-methods
#' @aliases getPopulation
#' @export
setMethod("getPopulation", signature(msa = "numeric", state = "missing", county = "missing"), function(msa, state, county){

    foo <- read.csv(system.file("join2.csv", package = "ITHIM"), stringsAsFactors = FALSE)
    msaMatrix <- foo %>% select(msaNHTS,stateFIPS,countyFIPS) %>% dplyr::filter(., msaNHTS == msa)
    F <- getPopulation(state = msaMatrix$stateFIPS, county = msaMatrix$countyFIPS)
    return(F)
})
#' @rdname getPopulation-methods
#' @aliases getPopulation
#' @export
setMethod("getPopulation", signature(msa = "list", state = "missing", county = "missing"), function(msa, state, county){

    F <- mapply(getPopulation, msa, SIMPLIFY = FALSE)

    names(F) <- names(msa)
    
    return(F)
})
