
## Function that transforms numeric vector into quintiles
quint <- function(x) {
  cutoff <- quantile(x, probs = c(0,0.2,0.4,0.6,0.8,1), na.rm = TRUE)
  if(sum(is.na(cutoff)) == 0) {
    as.numeric(as.character(cut(x,
                                breaks = cutoff,
                                labels = 5:1,
                                include.lowest = TRUE)))
  } else rep(NA, length(x))
}