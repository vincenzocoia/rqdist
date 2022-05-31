#' Swap continuity direction of a step function
#'
#' @param stepfun Object of class "stepfun".
#' @return The same `stepfun` object with the continuity direction swapped
#' between left and right.
swap_step_continuity_direction <- function(stepfun) {
	x <- stats::knots(stepfun)
	y <- plateaus(stepfun)
	l <- is_left_continuous(stepfun)
	r <- is_right_continuous(stepfun)
	if (!l && !r) {
		stop("Step function provided is neither left nor right continuous.")
	}
	if (l) {
		stats::stepfun(x, y, right = FALSE)
	} else {
		stats::stepfun(x, y, right = TRUE)
	}
}

#' Extract Heights of a Step Function
#'
#' Extracts the heights/"y values"/plateaus of a
#' step function. Sister function to \code{stats::knots()},
#' which returns the breakpoints/"x values".
#'
#' @param object Object of class "stepfun".
#' @return Vector of y values from left to right.
plateaus <- function(object) {
	e <- environment(object)
	fval <- with(e, f)
	y <- with(e, y)
	if (fval == 1) {
		yright <- with(e, yright)
		return(c(y, yright))
	} else {
		yleft <- with(e, yleft)
		return(c(yleft, y))
	}
}

#' Check Continuity of Step Function
#'
#' Check for left and right continuity of
#' a step function.
#'
#' @param object Object of class "stepfun" to check.
#' @return Single logical indicating the result.
#' @rdname check_continuous
is_left_continuous <- function(object) {
	if (!stats::is.stepfun(object)) {
		stop("Object being tested is not a step function.")
	}
	f <- with(environment(object), f)
	if (f == 1) TRUE else FALSE
}

#' @rdname check_continuous
is_right_continuous <- function(object) {
	if (!stats::is.stepfun(object)) {
		stop("Object being tested is not a step function.")
	}
	f <- with(environment(object), f)
	if (f == 0) TRUE else FALSE
}

#' Convert a Step Quantile Function to a Distribution
#'
#' @param qf_stepfun Step function representing a quantile function.
#' @return Discrete distionary/distplyr distribution.
#' If the step function provided has a portion that
#' decreases, NULL is returned.
qf_stepfun_to_dst <- function(qf_stepfun) {
	k <- stats::knots(qf_stepfun)
	n_k <- length(k)
	if (k[1] != 0) {
		warning("First knot in quantile step function is not 0. First
				quantile may not be accurate.")
	}
	if (k[n_k] != 1) {
		warning("Last knot in quantile step function is not 1. Last
				quantile may not be accurate.")
	}
	probs <- diff(k)
	if (sum(probs) != 1) {
		warning(paste0("Sum of discrete probabilities from quantile step function
				equals ", sum(probs), ", not 1. Re-scaling probabilities to
					   sum to 1."))
	}
	plat <- plateaus(qf_stepfun)
	if (any(diff(plat) < 0)) {
		return(NULL)
	}
	n_plat <- length(plat)
	y <- plat[-c(1, n_plat)]
	distionary::dst_empirical(y, weights = probs)
}
