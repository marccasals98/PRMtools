#' Filter the Data
#'
#' This function removes the answers that have said one specific code more times
#' that max_times. For example, in a poll we descart people who said 'Ns/Nr' more
#' than 12 times.
#'
#' @param df the Dataframe that you introduced from the function haven::read_sav()
#' @param variable_list the variables that we want to filter.
#' @param max_times The number of maximum times that a code can appear.
#' @param codes_list The array of codes that we want to filter.
#'
#' @return returns the df without these rows.
#' @examples filt_data(df=data_frame, variable_list= c("p15_1", "p15_2"), max_times=7, codes_list=c(97,98,99))
#' @export

filt_data <- function(df, variables_list, max_times, codes_list) {
  colnames(df) <- tolower(colnames(df))
  times <- apply(
    X = df[, variables_list],
    MARGIN = 1,
    FUN = function(k) {
      sum(k %in% codes_list)
    }
  )
  to_descart <- rep(0, nrow(df))
  to_descart[times > 7] <- 1
  to_descart_logical <- (to_descart == 0)
  df <- df[to_descart_logical, ]
  return(df)
}
