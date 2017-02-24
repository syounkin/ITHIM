#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read in Global Burden of Disease Data
#'
#' Read in Global Burden of Disease Data
#'
#' @return A list of lists of matrices with dproj, yll, yld and daly
#'     by age and sex and disease
#'
#' @export
readGBD <- function(filename){
    gbd <- read.csv(file=filename, stringsAsFactors = FALSE)

    if(!(
        setEquality(names(gbd), c("region","disease","sex","ageClass","dproj","yll","yld","daly"))
        ||
        setEquality(names(gbd), c("disease","sex","ageClass","dproj","yll","yld","daly")))){
        stop("Error with column names")
    }

    if( !(setEquality(unique(gbd$disease),c("BreastCancer", "ColonCancer", "HHD", "IHD", "Stroke", "Dementia", "Diabetes", "Depression", "LungCancer", "InflammatoryHD", "RespiratoryDisease", "RTIs")))){
        stop("Error with disease names")
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
#' Reformat GBD data frame
#'
#' Reformat GBD data frame
#'
#' @return A list of lists of matrices with dproj, yll, yld and daly
#'     by age and sex and disease
#'
#' @export
reformatGBD <- function(gbd){
            gbdList <- split(gbd,gbd$disease)
        gbdList[["CVD"]] <- data.frame(disease = "CVD", gbdList$IHD[,c("sex",  "ageClass")], gbdList$IHD[,c("dproj","yll","yld","daly")] + gbdList$InflammatoryHD[,c("dproj","yll","yld","daly")] + gbdList$HHD[,c("dproj","yll","yld","daly")])
        gbdList2 <- lapply(gbdList,function(x) split(x,as.factor(x$sex)))
        gbdList2 <- lapply(gbdList2, function(x) list(M=x$M,F=x$F))
            return(gbdList2)
            }
