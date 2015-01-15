winsorize <- function (x, method = c("IQR", "PERC"), k = 2, fraction = 0.01, 
    na.rm = TRUE, trim = FALSE) 
{
    t <- typeof(x)
    if (is.numeric(x)) {
        if (method == "IQR") {
            qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
            H <- k * IQR(x, na.rm = na.rm)
            if (trim == FALSE) {
                x[x < (qnt[1] - H)] <- (qnt[1] - H)
                x[x > (qnt[2] + H)] <- (qnt[2] + H)
            }
            else {
                x[x < (qnt[1] - H)] <- NA
                x[x > (qnt[2] + H)] <- NA
            }
            return(switch(t, integer = as.integer(x), double = as.numeric(x)))
        }
        else if (method == "PERC") {
            lim <- quantile(x, probs = c(fraction, 1 - fraction), 
                na.rm = na.rm)
            if (trim == FALSE) {
                x[x < lim[1]] <- lim[1]
                x[x > lim[2]] <- lim[2]
            }
            else {
                x[x < lim[1]] <- lim[1]
                x[x > lim[2]] <- lim[2]
            }
            return(switch(t, integer = as.integer(x), double = as.numeric(x)))
        }
        else {
            stop("Specify a winsorization method!")
        }
    }
    else {
        stop("x must be a numeric vector!")
    }
}
