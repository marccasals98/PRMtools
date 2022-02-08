#' Filter the Data
#'
#'This function is used to subset your dataframe if you want to remove some rows
#'which some speciefic code appears more than max_times.
#'
#'For example: If you are doing a poll and a person is saying "DK/NA" multiple
#'times. It is normal to invalid this opinion. With this function you can subset
#'your dataset easily.
#'
#'
#'
#' @param variables_list the variables that we want to filter.
#' Let df be a dataframe, the variable_list will be df[,questions_to_filter].
#' @param max_times The number of maximum times that a code can appear.
#' @param codes_list The array of codes that we want to filter.
#'
#' @return returns an array with boolean variables that remark which columns are
#' NOT to be removed
#'
#' @examples p15_1 <- c(0, 2, 3, 99, 99, 99, 2)
#' p15_2 <- c(1, 2, 5, 99, 4, 52, 2)
#' data_frame <- data.frame(p15_1, p15_2)
#' data_frame_output <-data_frame[ filt_data(
#'   variables_list = data_frame[,c ("p15_1", "p15_2")],
#'   max_times =  1,
#'   codes_list =   99
#' ),]
#'
#' @export

filt_data <- function(variables_list, max_times, codes_list) {
  times <- apply(
    X =  variables_list,
    MARGIN = 1,
    FUN = function(k) {
      sum(k %in% codes_list)
    }
  )
  to_descart <- (times<=max_times)
  return(to_descart)
}
