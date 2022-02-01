#' COUNT
#'
#' This function replicates the count function in SPSS, it counts the times that
#' some codes appear in some variables and adds them up.
#' @param df the Dataframe that you introduced from the function haven::read_sav()
#' @param variables_list the variables whose values we want to count
#' @param codes The array of codes that we want to count
#'
#' @return returns an integer with the value of the count.
#' @examples
#' p15_1 <- c(0, 2, 3, 99, 99, 99, 2)
#' p15_2 <- c(1, 2, 5, 3, 4, 52, 2)
#' data_frame <- data.frame(p15_1, p15_2)
#' COUNT(data_frame, c("p15_1","p15_2"), c(99,2))
#' @export


COUNT <- function(df,variables_list, codes){
  total_sum <- 0
  for (code in codes){
  total_sum = total_sum + sum(df[,variables_list]==code)
  }
  return(total_sum)
}


#' FRE (FREQUENCIES)
#'
#' This function replicates the FREQUENCIES function of SPSS. It accepts a
#' dataframe and returns a table in the format of a dataframe with the number of
#' cases and the propotion.
#'
#' @param df the Dataframe that you introduced from the function haven::read_sav()
#' @param variable the variable we want to see the frequencies
#'
#' @return returns a dataframe whose columns are the rows of the tables.
#'
#' @examples
#' p15_1 <- c(0, 2, 3, 99, 99, 99, 2)
#' p15_2 <- c(1, 2, 5, 3, 4, 52, 2)
#' data_frame <- data.frame(p15_1, p15_2)
#' FRE(data_frame,"p15_1")
#'
#' @export

FRE <- function(df, variable){
  table_frame <- data.frame(table(df[,variable]),
                            as.vector(prop.table(table(df[,variable]))*100))
  colnames(table_frame) <- c("Valores", "Frecuencia", "Porcentaje")
  return(table_frame)
}









