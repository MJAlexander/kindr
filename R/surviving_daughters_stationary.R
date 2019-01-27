#' Surviving daughters (stationary popualtion)
#'
#' Given Lx and Fx, calculate the implied number of surviving daughters assuming a stationary population
#'
#' @param df dataframe containing Lx and Fx values by age
#' @param age_a age of mother
#' @param ffab fraction female at birth, default is 0.4886.
#' @return a number representing the implied number of surviving daughters
#' @export

surviving_daughters_stationary <- function(df,
                                age_a,
                                ffab = 0.4886
){

  x_vec <- seq(15,age_a, by = 5)
  LF_prod <- c()
  for(x in x_vec){
    this_L <- df %>% filter(age == age_a - x) %>% select(Lx) %>% pull()
    this_F <- df %>% filter(age == x) %>% select(Fx) %>% pull()
    LF_prod <- c(LF_prod, this_L/10^5*this_F/10^3*ffab)
  }

  return(sum(LF_prod))
}
