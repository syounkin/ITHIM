#' The TravelSurvey class
#'
#' This class defines the format for the travel survey data
#'
#' @name TravelSurvey
#' @rdname TravelSurvey
#' @aliases TravelSurvey-class
#' @exportClass TravelSurvey
setClass(Class = "TravelSurvey", representation = representation(person = "data.frame", trip = "data.frame", house = "data.frame"),
         prototype = prototype(person = data.frame(houseID=NA,subjectID=NA,sex=NA,age=NA), trip = data.frame(houseID = NA, subjectID = NA, duration = NA, mode = NA), house = data.frame(houseid=NA, location=NA)),

         validity = function(object){

             msg <- NULL

             if( length(names(object@person)) == 0 ) msg <- "Person data frame needs columns."
             if( is.null(msg) && !identical( names(object@person), c("houseID","subjectID","sex","age"))) msg <- "Person data frame has the wrong column names (or wrong column order.)"
             if( !is.integer(object@person$houseID)) msg <- "The houseID variable in the person table must be integer class."
             if( !is.integer(object@person$subjectID)) msg <- "The subjectID variable in the person table must be integer class."
             if( !is.factor(object@person$sex)) msg <- "The sex variable in the person table must be a factor."
             if( !is.numeric(object@person$age)) msg <- "The age variable in the person table must be numeric."

             if( !is.integer(object@trip$houseID)) msg <- "The houseID variable in the trip table must be integer class."
             if( !is.integer(object@trip$subjectID)) msg <- "The subjectID variable in the trip table must be integer class."
             if( !is.numeric(object@trip$duration)) msg <- "The duration variable in the trip table must be numeric."
             if( !is.factor(object@trip$mode)) msg <- "The mode variable in the trip table must be a factor."

             if( !is.integer(object@house$houseID)) msg <- "The houseID variable in the house table must be integer class."
             if( !is.factor(object@house$location)) msg <- "The location variable in the house table must be a factor."

             if( length(names(object@trip)) == 0 ) msg <- "Trip data frame needs columns."
             if( is.null(msg) && !identical( names(object@trip), c("houseID","subjectID","duration","mode"))) msg <- "Trip data frame has the wrong column names (or wrong column order.)"

             if( length(names(object@house)) == 0 ) msg <- "House data frame needs columns."
             if( is.null(msg) && !identical( names(object@house), c("houseID","location"))) msg <- "House data frame has the wrong column names (or wrong column order.)"

             if(is.null(msg)) TRUE else msg
         }
)
