
#'@export
createParameterSet <- function(x){

    parList <- as(createParameterList(),"list")
    for(name in names(x)){
        parList[[name]] <- x[[name]]
        }

    pSet <- new("ParameterSet",Rwt = parList$Rwt,
                                        Rct = parList$Rct,
                                        muwt = parList$muwt,
                                        muct = parList$muct,
                                        cv = parList$cv,
                                        cvNonTravel = parList$cvNonTravel,
                                        nAgeClass = parList$nAgeClass,
                                        muNonTravel = parList$muNonTravel,
                                        muNonTravelMatrix = parList$muNonTravelMatrix,
                                        GBD = parList$GBD,
                                        meanType = parList$meanType,
                quantiles = parList$quantiles,
                roadInjuries = parList$roadInjuries,
                distRoadType = parList$distRoadType,
                safetyInNumbers = parList$safetyInNumbers
                )
    return(pSet)
    }
