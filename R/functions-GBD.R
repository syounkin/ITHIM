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

    if("burdenType" %in% names(gbd)){
        normalized <- TRUE
    }else{
        normalized <- FALSE
    }

    if(normalized){
        gbd <- gbd %>% spread(burdenType, value)
    }

    if(!(
        setEquality(names(gbd), c("region","disease","sex","ageClass","deaths","yll","yld","daly"))
        ||
        setEquality(names(gbd), c("disease","sex","ageClass","deaths","yll","yld","daly")))){
        stop("Error with column names")
    }

    if( !(all(unique(gbd$disease) %in% c("BreastCancer", "ColonCancer", "CVD", "Dementia", "Diabetes", "Depression", "RTIs")))){
        stop("Extraneous diseases are included in the disease burden file.  Currently the ITHIM package does not support this.  Please see the help page for createITHIM and remove extraneous diseases from disease burden file.")
    }

    if(length(names(gbd))==7){
        gbdList2 <- reformatGBD(gbd)
    }else if(length(names(gbd))==8){
        gbdList2 <- lapply(split(gbd,gbd$region), reformatGBD)
    }else{
        stop("Wrong number of columns in GBD file.")
    }
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
