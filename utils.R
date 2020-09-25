setwd("~/Documents/flight-delay-update")

as.num <- function(x, na.strings = "NA") {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

feature_plot <- function(df, x, y=df$delay_time_num){
  new_order <- with(df, reorder(x, y, median , na.rm=T))
  boxplot(
    y ~ new_order,
    data = df,
    col = "orange",
    border = "brown",
    ylim = c(0, quantile(y, 0.95)),
    xlab = deparse(substitute(x)),
    ylab = 'delay_time_num'
  )
  par(cex.axis = 1)
}

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat) 
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = (cormat)[ut],
    p = pmat[ut]
  )
}
