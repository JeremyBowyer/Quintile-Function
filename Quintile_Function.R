
## Function that transforms numeric vector into quintiles
quint <- function(x, descending = TRUE) {
  labels <- if(descending) 5:1 else 1:5 
  cutoff <- quantile(x, probs = c(0,0.2,0.4,0.6,0.8,1), na.rm = TRUE)
  if(sum(is.na(cutoff)) == 0) {
    as.numeric(as.character(cut(x,
                                breaks = cutoff,
                                labels = labels,
                                include.lowest = TRUE)))
  } else rep(NA, length(x))
}
