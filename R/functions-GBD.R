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
    #filePath <- system.file(file, package="ITHIM")
    gbd <- read.csv(file=filename, stringsAsFactors = FALSE)
    gbdList <- split(gbd,gbd$disease)
    gbdList[["CVD"]] <- data.frame(disease = "CVD", gbdList$IHD[,c("sex",  "ageClass")], gbdList$IHD[,c("dproj","yll","yld","daly")] + gbdList$InflammatoryHD[,c("dproj","yll","yld","daly")] + gbdList$HHD[,c("dproj","yll","yld","daly")])
    gbdList2 <- lapply(gbdList,function(x) split(x,as.factor(x$sex)))
    gbdList2 <- lapply(gbdList2, function(x) list(M=x$M,F=x$F))
    return(gbdList2)
    }
