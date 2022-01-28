# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

filt_data <- function(df, variables_list, max_times, codes_list) {
  colnames(df) <- tolower(colnames(df))
  times <- apply(X = df,
                 MARGIN = 1,
                 FUN = function(k){ sum(k%in%codes_list)})
  to_descart<-rep(0,nrow(df))
  to_descart[times>7]<-1
  to_descart_logical <- (to_descart==0)
  df<- df[to_descart_logical,]
  return(df)
}
