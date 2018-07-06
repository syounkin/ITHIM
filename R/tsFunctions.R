#' Compute Travel Activity Parameters
#'
#' Foo
#'
#' @param ts A TravelSurvey object
#'
#' @return A data frame with travel activity mean and standard deviation
#' @export
getTA <- function(ts){

ts.df <- full_join(ts@trip, full_join(ts@person, ts@house, by = "houseID"), by = c("houseID","subjectID"))

ts.df %>%
  group_by(location, houseID, subjectID, mode) %>%
    summarise(T = sum(duration)) %>%
    spread(mode, T, fill = 0)    %>%
    select(location, houseID, subjectID, walk, cycle) %>%
    ungroup() %>%
    dplyr::filter(walk + cycle > 0 ) %>%
    mutate( TA = 7*(3*walk/60 + 6*cycle/60) ) %>%
    group_by(location) %>%
    summarise(mean = mean(TA), sd = sd(TA))
}
