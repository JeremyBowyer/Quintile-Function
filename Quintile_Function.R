
## Function that transforms numeric vector into quintiles
quint <- function(x, descending = TRUE) {
  
  cutoff <- unique(quantile(x, probs = c(0,0.2,0.4,0.6,0.8,1), na.rm = TRUE))
  labels <- if(descending) (length(cutoff) - 1):1 else 1:(length(cutoff) - 1)
  labels <- replace(labels, labels == max(labels), 5)

  if(sum(is.na(cutoff)) == 0 && length(cutoff) > 1) {

    tryCatch({
      cuts <- as.numeric(as.character(cut(x,
                                          breaks = cutoff,
                                          labels = labels,
                                          include.lowest = TRUE)))
      return(cuts)
    }, error = function(e) {
      print(e$message)
      return(rep(NA, length(x)))
    })

  } else return(rep(NA, length(x)))
  
}
