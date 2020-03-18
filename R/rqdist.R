#' Linear Quantile Regression Process Distribution Model
#'
#' Fits a quantile regression process model based on an
#' assumption of linear conditional quantiles. A wrapper
#' for \code{quantreg::rq()}.
#'
#' @param formula An object of class "formula".
#' @param data Data frame containing the data.
#' @param ... Other parameters to pass to \code{quantreg::rq()},
#' aside from the \code{tau} option.
#'
#' @return An object of class "rqdist" that returns
#' distributions of class "dst" from the distplyr package.
#' @export
rqdist <- function(formula, data, ...) {
    model <- quantreg::rq(formula, data, tau = -1, ...)
    class(model) <- c("rqdist", class(model))
    model
}

#' Predict Quantile Regression Distributions
#'
#' Predict distributions from a Linear Quantile Regression
#' Process Model. A wrapper around
#' \code{quantreg::predict.rq.process()},
#' bundling the distribution functions into a "dst" object.
#'
#' @param object Object of class "rqdist", returned from the
#' \code{rqdist()} function.
#' @param newdata Data frame from which to make predictions.
#' @param rearrange The linear assumption sometimes results in
#' distributions with non-monotonic quantile functions and cdf's.
#' If \code{TRUE}, uses the \code{quantreg::rearrange()} function
#' to make these functions non-decreasing. If \code{FALSE},
#' leaves the functions as-is.
#' @return List of distributions ("dst" objects) corresponding
#' to each row of \code{newdata}, corresponding to the
#' estimated distribution of the response given the covariates
#' in \code{newdata}.
#' @rdname predict
#' @export
predict.rqdist <- function(object, newdata, rearrange = TRUE) {
	if (missing(newdata)) newdata <- object$model
	n <- nrow(newdata)
    if (n == 0) return(list())
    Qhat <- quantreg::predict.rq.process(
        object, newdata, type = "Qhat", stepfun = TRUE
    )
    if (n == 1) Qhat <- list(Qhat)
    pre_taus <- lapply(Qhat, stats::knots)
    pre_heights <- lapply(Qhat, distplyr::plateaus)
    if (rearrange) {
    	Qhat <- lapply(Qhat, quantreg::rearrange)
    }
    post_taus <- lapply(Qhat, stats::knots)
    post_heights <- lapply(Qhat, distplyr::plateaus)
    Fhat <- list()
    for (i in 1:n) {
    	if (any(diff(pre_heights[[i]]) < 0) && !rearrange) {
    		Fhat[[i]] <- NA
    	} else {
    		y <- post_heights[[i]][-c(1, length(post_heights[[i]]))]
    		Fhat[[i]] <- stepfun(y, post_taus[[i]])
    	}
    }

    fhat <- quantreg::predict.rq.process(
        object, newdata, type = "fhat"
    )
    if (n == 1) fhat <- list(fhat)
    name <- "Linear Quantile Regression Process Distribution"
    if (rearrange) name <- paste(name, "(rearranged)")
    n <- length(Qhat)
    out <- list()
    for (i in 1:n) {
        out[[i]] <- distplyr::dst(
            fun_cumu = Fhat[[i]],
            fun_quant = Qhat[[i]],
            fun_prob = fhat[[i]],
            name = name
        )
    }
    out
}

#' @rdname predict
#' @import broom
#' @export
augment.rqdist <- function(object, newdata, rearrange = TRUE) {
	if (missing(newdata)) newdata <- object$model
    yhat <- predict.rqdist(object, newdata, rearrange)
    dplyr::mutate(as_tibble(newdata), .fitted = yhat)
}
