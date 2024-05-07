#' Check for Missing Value Labels
#'
#' @param x Data set
#'
#' @return Vector of variables containing unlabelled values
#' @export
#'
#' @examples
check_labs <- function(x) {
  a <- lapply(x, FUN = labelled::val_labels)
  b <- tidyr::drop_na(x)
  c <- lapply(x, unique)
  vars <- c()
  for (i in 1:length(c)) {
    if (any(c[[i]] %in% a[[i]] == F)) {
      vars <- c(vars,names(x[i]))
    }
  }
  return(vars)
}
