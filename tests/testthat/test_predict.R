set.seed(1)
n <- 10L
dat <- data.frame(x = rnorm(10), y = rnorm(10))
model <- rqdist(y ~ x, data = dat)

x0 <- data.frame(x = 0)
yhat_proper <- predict(model)
yhat_raw    <- predict(model, rearrange = FALSE)
yhat_x0     <- predict(model, newdata = x0)
newtib_proper <- augment(model)
newtib_raw    <- augment(model, rearrange = FALSE)
newtib_x0     <- augment(model, newdata = x0)



test_that("model is an rq.process object", {
	names <- names(model)
	expect_true("sol" %in% names)
	expect_true("dsol" %in% names)
	expect_identical(nrow(model$sol), 5L)
	expect_identical(nrow(model$dsol), n)
	expect_identical(ncol(model$dsol), ncol(model$sol))
})

test_that("predictions output proper classes", {
	expect_true(is.list(yhat_proper))
	expect_true(is.list(yhat_raw))
	expect_true(is.list(yhat_x0))
	expect_true(distplyr::is_dst(yhat_proper[[1]]))
	expect_true(distplyr::is_dst(yhat_raw[[1]]))
	expect_true(distplyr::is_dst(yhat_x0[[1]]))
	expect_true(tibble::is_tibble(newtib_proper))
	expect_true(tibble::is_tibble(newtib_raw))
	expect_true(tibble::is_tibble(newtib_x0))
	expect_true(".fitted" %in% names(newtib_proper))
	expect_true(".fitted" %in% names(newtib_raw))
	expect_true(".fitted" %in% names(newtib_x0))
	expect_equal(yhat_proper, newtib_proper$.fitted)
	expect_equal(yhat_raw, newtib_raw$.fitted)
	expect_equal(yhat_x0, newtib_x0$.fitted)
})

cdf1 <- distplyr::get_cdf(yhat_proper[[1]])
cdf2 <- distplyr::get_cdf(yhat_raw[[1]])
cdf3 <- distplyr::get_cdf(yhat_x0[[1]])
qf1  <- distplyr::get_quantfn(yhat_proper[[1]])
qf2  <- distplyr::get_quantfn(yhat_raw[[1]])
qf3  <- distplyr::get_quantfn(yhat_x0[[1]])

# is_right_continuous(qf1)

# qf2 is non-monotonic:
# plot(qf2)

test_that("cdf2 is NULL, but cdf1 isn't", {
	expect_true(is.null(cdf2))
	expect_false(is.null(cdf1))
})

test_that("cdf's evaluate correctly", {
	qf1(cdf1(dat$x))
})

