#' Compute Travel Activity Parameters
#'
#' Foo
#'
#' @param ts A TravelSurvey object
#'
#' @return A data frame with travel activity mean and standard deviation
#' @export
getTA <- function(ts, type = "day", fAT = 1){

    ts.df <- full_join(ts@trip, full_join(ts@person, ts@house, by = "houseID"), by = c("houseID","subjectID"))

    if( type == "day" ){

        ts.df %>%
            group_by(location, houseID, subjectID, mode) %>%
            summarise(T = sum(duration)) %>%
            spread(mode, T, fill = 0)    %>%
            select(location, houseID, subjectID, walk, cycle) %>%
            ungroup() %>%
            dplyr::filter(walk + cycle > 0 ) %>%
            mutate( TA = 7*fAT*(3*walk/60 + 6*cycle/60) ) %>%
            group_by(location) %>%
            summarise(mean = mean(TA), sd = sd(TA))
    }else if( type == "week"){
        ts.df %>%
            group_by(location, houseID, subjectID, mode) %>%
            summarise(T = sum(duration)) %>%
            spread(mode, T, fill = 0)    %>%
            select(location, houseID, subjectID, walk, cycle) %>%
            ungroup() %>%
            dplyr::filter(walk + cycle > 0 ) %>%
            mutate( TA = 3*walk/60 + 6*cycle/60 ) %>%
            group_by(location) %>%
            summarise(mean = mean(TA), sd = sd(TA))
    }else{
        message("Problem with type variable")
    }

}
#' @export
getp0 <- function(ts, type = "day", fAT = 1){

    ts.df <- full_join(ts@trip, full_join(ts@person, ts@house, by = "houseID"), by = c("houseID","subjectID"))

    if( type == "day" ){

        ts.df %>%
            group_by(location, houseID, subjectID, mode) %>%
            summarise(T = sum(duration)) %>%
            spread(mode, T, fill = 0)    %>%
            select(location, houseID, subjectID, walk, cycle) %>%
            ungroup() %>%
            group_by(location) %>% summarise(n0 = sum(walk + cycle == 0), n = n()) %>%
            mutate(p0 = fAT*n0/n) %>% select(location, p0)

    }else if( type == "week"){
        ts.df %>%
            group_by(location, houseID, subjectID, mode) %>%
            summarise(T = sum(duration)) %>%
            spread(mode, T, fill = 0)    %>%
            select(location, houseID, subjectID, walk, cycle) %>%
            ungroup() %>%
            group_by(location) %>% summarise(n0 = sum(walk + cycle == 0), n = n()) %>%
            mutate(p0 = n0/n) %>% select(location, p0)

    }else{
        message("Problem with type variable")
    }



}
