setMethod("[", c("TravelSurvey", "character", "missing", "ANY"),
    ## we won't support subsetting on j; dispatching on 'drop' doesn't
    ## make sense (to me), so in rebellion we'll quietly ignore it.
    function(x, i, j, ..., drop=TRUE)
{
    ## less clever: update slot, return instance
    ## x@slt = x@slt[i]
    ## x
    ## clever: by default initialize is a copy constructor, too
    house <- x@house[as.character(x@house$year) %in% i,]
    person <- x@person %>% dplyr::filter(houseID %in% house$houseID)
    trip <- x@trip %>% dplyr::filter(houseID %in% house$houseID)
    initialize(x, house = house, person = person, trip = trip)
})
