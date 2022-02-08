#' count
#'
#' It counts the times that
#' some codes appear in some variables and adds them up.
#'
#' @param x the vector of the variables whose values we want to count
#' @param codes The array of codes that we want to count
#'
#' @return returns an integer with the value of the count.
#' @examples
#' p15_1 <- c(0, 2, 3, 99, 99, 99, 2)
#' p15_2 <- c(1, 2, 5, 3, 4, 52, 2)
#' data_frame <- data.frame(p15_1, p15_2)
#' count(data_frame[, c("p15_1","p15_2")], c(99,2))
#' @export


count <- function(x, codes){
  total_sum <- 0
  for (code in codes){
    total_sum = total_sum + sum(x==code)
  }
  return(total_sum)
}


#' frec
#'
#'This function returns a table which first column is the frequencies of the event
#'and the second column is the percentatge of this frequencies.
#'
#' @param x is the vector that counts how many times an event has occured
#' @param w is a vector of the weight.
#'
#' @return returns a vector with all of the information (it's a cbind of the table
#' and the prop.table).
#'
#' @examples
#' p15_1 <- c(0, 2, 3, 99, 99, 99, 2)
#' p15_2 <- c(1, 2, 5, 3, 4, 52, 2)
#' weight <- c(200,400,50,200,100,300,90)
#' data_frame <- data.frame(p15_1, p15_2)
#' frec(data_frame[,"p15_1"],weight)
#'
#' @export

frec <- function(x,w){
  observaciones <- tapply(w,x,sum)
  porcentaje <- prop.table(observaciones)*100
  table<-cbind(observaciones,porcentaje)
  return(table)
}




