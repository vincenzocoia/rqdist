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
    # qf should be left-continuous, but is made right-continuous.
    Qhat <- lapply(Qhat, distplyr::swap_step_continuity_direction)
    if (rearrange) {
    	Qhat <- lapply(Qhat, quantreg::rearrange)
    }
    if (n == 1) {
    	## This is needed until n==1 bug is fixed in next version of quantreg.
    	newdata <- rbind(newdata, newdata)
    	fhat <- quantreg::predict.rq.process(
    		object, newdata, type = "fhat"
    	)
    	fhat <- fhat[[1]]
    } else {
    	fhat <- quantreg::predict.rq.process(
    		object, newdata, type = "fhat"
    	)
    }
    if (n == 1) fhat <- list(fhat)
    name <- "Linear Quantile Regression Process Distribution"
    if (rearrange) name <- paste(name, "(rearranged)")
    out <- list()
    for (i in 1:n) {
    	out[[i]] <- stepqf_to_stepdst(Qhat[[i]], fun_prob = fhat[[i]], name = name)
    }
    length(out) <- n
    out
}

#' Convert a Step Quantile Function to a Step Distribution
#'
#' @param stepqf Step function representing a quantile function.
#' @param ... Other arguments to pass to the \code{stepdst} or
#' \code{dst} function (for example, a density).
#' @return Object of class "stepdst" from the \code{distplyr}
#' package. If the step function provided has a portion that
#' decreases, a distribution object ("dst") is still output,
#' but only the quantile function is available.
stepqf_to_stepdst <- function(stepqf, ...) {
	taus <- stats::knots(stepqf)
	y <- distplyr::plateaus(stepqf)
	probs <- diff(c(0, taus, 1))
	if (any(diff(y) < 0)) {
		distplyr::dst(fun_cumu = NULL,
					  fun_quant = stepqf,
					  ...)
	} else {
		df <- data.frame(y = y, probs = probs)
		distplyr::stepdst(y, data = df, weights = probs, ...)
	}
}



#' @rdname predict
#' @import broom
#' @export
augment.rqdist <- function(object, newdata, rearrange = TRUE) {
	if (missing(newdata)) newdata <- object$model
    yhat <- predict.rqdist(object, newdata, rearrange)
    dplyr::mutate(tibble::as_tibble(newdata), .fitted = yhat)
}
