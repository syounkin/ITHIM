#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Read in Global Burden of Disease Data
###
### Read in Global Burden of Disease Data
###
### @return A list of lists of matrices with deaths, yll, yld and daly
###     by age and sex and disease
###
###
readGBD <- function(filename){

    gbd <- read.csv(file=filename, stringsAsFactors = FALSE)

    gbd <- gbd %>% arrange(disease,sex,ageClass,burdenType)

    gbdDiseaseVec <- sort(unique(gbd$disease))
    diseaseVec <- sort(c("BreastCancer", "ColonCancer", "CVD", "Dementia", "Diabetes", "Depression"))

    if(!identical(diseaseVec, gbdDiseaseVec)){
        stop("The disease burden file must contain the following diseases; BreastCancer, ColonCancer, CVD, Dementia, Diabetes, Depression.  RTIs are no longer accepted.  For more information see the help page for createITHIM by running help(createITHIM).")
        }

    gbdList2 <- lapply(split(gbd, gbd$disease), function(x) {
        foo <- split(x,x$sex)
        foo <- list(M = foo$M, F = foo$F)
        })
    return(gbdList2)

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Reformat GBD data frame
###
### Reformat GBD data frame
###
### @return A list of lists of matrices with deaths, yll, yld and daly
###     by age and sex and disease
###
###
reformatGBD <- function(gbd){
    gbdList <- split(gbd,gbd$disease)
    gbdList2 <- lapply(gbdList,function(x) split(x,as.factor(x$sex)))
    gbdList2 <- lapply(gbdList2, function(x) list(M=x$M,F=x$F))
    return(gbdList2)
}
readGBD2 <- function(filename){
    foo <- read.csv(file = filename)
    gbdArray <- array(foo$value, dim = c(13,2,8,4), dimnames = list(unique(foo$disease),unique(foo$sex), unique(foo$ageClass), unique(foo$burdenType)))
    return(gbdArray)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Foo
###
### Foo
###
### @return Foo
###
###
summarizeBurden <- function(ITHIM.baseline, ITHIM.scenario, burdenType = "deaths"){

    GBDList <- getGBD(ITHIM.baseline) %>%
        split(., .$burdenType) %>%
        lapply(., function(x) split(x, x$disease))

    reformatGBD2 <- function(x){
        x %>%
            spread(., key = sex, value = value) %>%
            arrange(., ageClass) %>% select(M,F) %>%
            return(.)
    }

    burdenList <- lapply(lapply(GBDList[[burdenType]],reformatGBD2),function(x) return(x))
    AFList <- ITHIM:::compareModels(ITHIM.baseline, ITHIM.scenario)$AF

    diseaseVec <- intersect(names(burdenList),names(AFList))
    burdenList <- burdenList[diseaseVec]
    AFList <- AFList[diseaseVec]

    burdenList.scenario <- mapply(function(x,y) (1-x)*y, AFList, burdenList, SIMPLIFY = FALSE)

    rho <- mapply(function(x,y) 1 - sum(y)/sum(x), burdenList, burdenList.scenario, SIMPLIFY = TRUE)

    return(rho)

}
