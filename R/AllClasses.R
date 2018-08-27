#' The TravelSurvey class
#'
#' This class defines the format for the travel survey data
#'
#' @name TravelSurvey
#' @rdname TravelSurvey
#' @aliases TravelSurvey-class
#' @exportClass TravelSurvey
setClass(Class = "TravelSurvey", representation = representation(person = "data.frame", trip = "data.frame", house = "data.frame"),
         prototype = prototype(person = data.frame(houseID=NA,subjectID=NA,sex=NA,age=NA), trip = data.frame(houseID = NA, subjectID = NA, duration = NA, mode = NA), house = data.frame(houseid=NA, location=NA, year=NA)),

         validity = function(object){

             msg <- NULL

             if( length(names(object@person)) == 0 ) msg <- "Person data frame needs columns."
             if( is.null(msg) && !identical( names(object@person), c("houseID","subjectID","sex","age"))) msg <- "Person data frame has the wrong column names (or wrong column order.)"
             if( !is.character(object@person$houseID)) msg <- "The houseID variable in the person table must be character class."
             if( !is.character(object@person$subjectID)) msg <- "The subjectID variable in the person table must be character class."
             if( !is.factor(object@person$sex)) msg <- "The sex variable in the person table must be a factor."
             if( !is.factor(object@person$age)) msg <- "The age variable in the person table must be a factor."

             if( !is.character(object@trip$houseID)) msg <- "The houseID variable in the trip table must be character class."
             if( !is.character(object@trip$subjectID)) msg <- "The subjectID variable in the trip table must be character class."
             if( !is.numeric(object@trip$duration)) msg <- "The duration variable in the trip table must be numeric."
             if( !is.factor(object@trip$mode)) msg <- "The mode variable in the trip table must be a factor."
             if( !identical(levels(object@trip$mode), c("walk","cycle","other"))) msg <- "The mode variable must have levels walk, cycle, other in that order."

             if( !is.character(object@house$houseID)) msg <- "The houseID variable in the house table must be character class."
             if( !is.factor(object@house$location)) msg <- "The location variable in the house table must be a factor."
             if( !is.character(object@house$year)) msg <- "The year variable in the house table must be a character."

             if( length(names(object@trip)) == 0 ) msg <- "Trip data frame needs columns."
             if( is.null(msg) && !identical( names(object@trip), c("houseID","subjectID","duration","mode"))) msg <- "Trip data frame has the wrong column names (or wrong column order.)"

             if( length(names(object@house)) == 0 ) msg <- "House data frame needs columns."
             if( is.null(msg) && !identical( names(object@house), c("houseID","location", "year"))) msg <- "House data frame has the wrong column names (or wrong column order.)"

             if(is.null(msg)) TRUE else msg
         }
)
