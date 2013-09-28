plotSpectrum <-
function (x, y=NULL, top = 20, type = "h", scale100 = FALSE, digit.label=1, 
    col = "black", pos = 0, main = "", clickAddLabels = FALSE, ...) {
    if(is.null(y)){
      y=x[,2]
      x=x[,1]
    }
    if (scale100 == TRUE) 
        y = y/max(y) * 100
    tmp = topIons(x, y, top)
    x1 = tmp[, 1]
    y1 = tmp[, 2]
    plot(x, y, type = type, main = main, xlab = "m/z", ylab = "intensity", 
        ...)
    mzLabels = as.character(round(x1[1:top], digit.label))
    if (!clickAddLabels) {
        text(x1[1:top], y1[1:top] + pos, mzLabels, col = col, 
            cex = 0.8)
    }
    else {
        identify(x1[1:top], y1[1:top] + pos, labels = mzLabels, 
            col = col)
    }
}#>>>


