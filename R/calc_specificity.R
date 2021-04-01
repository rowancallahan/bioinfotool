#' @title calc_specificity
#'
#' @description calculates different specificity scores as defined by https://academic.oup.com/bib/article/18/2/205/2562739
#'
#' @param dataframe
#'
#' @return dataframe
#'
#' @examples
#' dataset <- data.frame(a = c(1,2,3), b = c(1,2,3), c = c(1,2,3))
#' normalized_data <- bioinfotool::calc_specificity(dataset, type='tau')
#'
#'
#' @export

calc_specificity <- function(x, type='tau') {
  #x <- hpa_tau_df
  max_val <- apply(x,1,max)
  from_which <- apply(x,1, function(x) names(which(x==max(x,na.rm=F))))
  from_which <- lapply(from_which, function(x) paste0(x, collapse= ", "))
  normalized <- sweep(x, MARGIN=1, max_val, `/`)
  tau_list <-  apply((1-normalized), 1, sum)/(length(colnames(x)) -1)
  combined <- as.data.frame( cbind(tau_list, from_which) )
  return(combined)
}

