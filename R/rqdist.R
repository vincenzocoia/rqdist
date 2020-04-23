#' Linear Quantile Regression Process Distribution Model
#'
#' Fits a quantile regression process model based on an
#' assumption of linear conditional quantiles. A wrapper
#' for \code{quantreg::rq()}.
#'
#' @param formula An object of class "formula".
#' @param data Data frame containing the data.
#' @param grid_n Approximate the fitted distributions by
#' evaluating the quantile function on an equally spaced
#' grid of this many values. \code{Inf} (default) allows for
#' the entire quantile function to be obtained, but may be
#' slow when the dataset is large.
#' @param ... Other parameters to pass to \code{quantreg::rq()},
#' aside from the \code{tau} option.
#'
#' @return An object of class "rqdist" that returns
#' distributions of class "dst" from the distplyr package.
#' @export
rqdist <- function(formula, data, grid_n = Inf, ...) {
	if (identical(grid_n, Inf)) {
		model <- quantreg::rq(formula, data, tau = -1, ...)
	} else {
		tau <- (1:grid_n) / (grid_n + 1)
		model <- quantreg::rq(formula, data, tau = tau, ...)
	}
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
	if (missing(newdata)) {
		newdata <- object$model
	}
	n <- nrow(newdata)
    if (identical(n, 0L)) {
    	return(list())
    }
	if (inherits(object, "rq.process")) {
		Qhat <- quantreg::predict.rq.process(
			object, newdata, type = "Qhat", stepfun = TRUE
		)
		if (identical(n, 1L)) {
			Qhat <- list(Qhat)
		}
		# qf should be left-continuous, but is made right-continuous
		#   by predict.rq.process
		Qhat <- lapply(Qhat, distplyr::swap_step_continuity_direction)
	} else {
		tau <- object[["tau"]]
		yhat <- quantreg::predict.rq(object, newdata)
		n_tau <- length(tau)
		if (identical(n_tau, 1L)) {
			res <- lapply(yhat, distplyr::stepdst)
			return(res)
		} else {
			Qhat <- apply(yhat, 1L, function(y) {
				# dups <- duplicated(y)
				dups <- logical(n_tau)
				dups[n_tau] <- FALSE
				for (i in seq_len(n_tau - 1L)) {
					dups[[n_tau - i]] <- identical(y[[n_tau - i]], y[[n_tau - i + 1L]])
				}
				this_tau <- tau[!dups]
				this_y <- y[!dups]
				stats::stepfun(this_tau[-length(this_tau)], this_y, right = TRUE)
			})
		}
	}
    if (rearrange) {
    	Qhat <- lapply(Qhat, quantreg::rearrange, xmin = 0, xmax = 1)
    }
    # if (identical(n, 1L)) {
    # 	## This is needed until n==1 bug is fixed in next version of quantreg.
    # 	newdata <- rbind(newdata, newdata)
    # 	fhat <- quantreg::predict.rq.process(
    # 		object, newdata, type = "fhat"
    # 	)
    # 	fhat <- fhat[[1]]
    # } else {
    # 	fhat <- quantreg::predict.rq.process(
    # 		object, newdata, type = "fhat"
    # 	)
    # }
    # if (n == 1) fhat <- list(fhat)
    # name <- "rqdist"
    # if (rearrange) name <- paste(name, "(rearranged)")
    out <- list()
    for (i in 1:n) {
    	out[[i]] <- stepqf_to_dst(Qhat[[i]])
    }
    length(out) <- n
    out
}

#' Convert a Step Quantile Function to a Distribution
#'
#' @param stepqf Step function representing a quantile function.
#' @return Object of class "stepdst" from the \code{distplyr}
#' package. If the step function provided has a portion that
#' decreases, a distribution object ("dst") is still output,
#' but only the quantile function is available.
stepqf_to_dst <- function(stepqf, ...) {
	taus <- stats::knots(stepqf)
	y <- distplyr::plateaus(stepqf)
	probs <- diff(c(0, taus, 1))
	if (any(diff(y) < 0)) {
		distplyr::new_dst(list(representations = list(fun_quant = stepqf)),
						  variable = "discrete")
	} else {
		stopifnot(identical(length(y), length(probs)))
		df <- data.frame(y = y, probs = probs)
		distplyr::stepdst(y, data = df, weights = probs)
	}
}



#' @rdname predict
#' @import broom
#' @export
augment.rqdist <- function(object, newdata, rearrange = TRUE) {
	if (missing(newdata)) newdata <- object$model
    yhat <- predict(object, newdata, rearrange)
    dplyr::mutate(tibble::as_tibble(newdata), .fitted = yhat)
}
