#' Compute Travel Activity Parameters
#'
#' Foo
#'
#' @param ts A TravelSurvey object
#'
#' @return A data frame with travel activity mean and standard deviation
#' @export
getTA <- function(ts, alpha = 1){

    ts.df <- full_join(ts@trip, full_join(ts@person, ts@house, by = "houseID"), by = c("houseID","subjectID"))

    ts.df %>%
        group_by(location, houseID, subjectID, sex, age, mode) %>%
        summarise(T = sum(duration)) %>%
        spread(mode, T, fill = 0)    %>%
        select(location, houseID, subjectID, sex, age, walk, cycle) %>%
        ungroup() %>%
        dplyr::filter(walk + cycle > 0 ) %>%
        mutate( TA = alpha *(3*walk/60 + 6*cycle/60) ) %>%
        group_by(location, sex, age) %>%
        dplyr::filter(!is.na(location) & !is.na(sex) & !is.na(age)) %>%
        summarise(mean = mean(TA), sd = sd(TA))

}
#' @export
getp0 <- function(ts, fAT = 1){

    validObject(ts)


    ts.df <- full_join(ts@trip, full_join(ts@person, ts@house, by = "houseID"), by = c("houseID","subjectID"))

    ts.df %>%
        group_by(location, houseID, subjectID, sex, age, mode) %>%
        summarise(T = sum(duration)) %>% ungroup() %>%
        spread(mode, T, fill = 0) %>%
        select(location, houseID, subjectID, sex, age, walk, cycle) %>%
        dplyr::filter(!is.na(location) & !is.na(sex) & !is.na(age)) %>% group_by(location, sex, age) %>%
        summarise(n0 = sum(walk + cycle == 0), n = n()) %>%
        mutate(p0 = (1/fAT)*(n0/n) + 1 - 1/fAT) %>%
        mutate(p0 = ifelse(p0 < 0, 0, p0)) %>%
        select(location, sex, age, p0)

}
#' @export
getMeans <- function(ts, activeTravelers = TRUE, alpha = 1){

    ts.df <- full_join(ts@trip, full_join(ts@person, ts@house, by = "houseID"), by = c("houseID","subjectID"))

    if(!activeTravelers){

        ts.df %>%
            group_by(location, houseID, subjectID, sex, age, mode) %>%
            summarise(T = sum(duration)) %>%
            spread(mode, T, fill = 0) %>%
            group_by(location, sex, age) %>%
            summarise(walk = alpha*mean(walk), cycle = alpha*mean(cycle), other = alpha*mean(other))

    }else{

        ts.df %>%
            group_by(location, houseID, subjectID, sex, age, mode) %>%
            summarise(T = sum(duration)) %>%
            spread(mode, T, fill = 0) %>%
            dplyr::filter(walk + cycle > 0 ) %>%
            group_by(location, sex, age) %>%
            summarise(walk = alpha*mean(walk), cycle = alpha*mean(cycle), other = alpha*mean(other))

    }
}
