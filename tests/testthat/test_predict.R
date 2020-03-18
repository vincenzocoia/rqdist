model <- rqdist(mpg ~ disp, data = mtcars)
n <- nrow(mtcars)

test_that("model is an rq.process object", {
	names <- names(model)
	expect_true("sol" %in% names)
	expect_true("dsol" %in% names)
	expect_identical(nrow(model$sol), 5L)
	expect_identical(nrow(model$dsol), n)
	expect_identical(ncol(model$dsol), ncol(model$sol))
})

test_that("predictions output proper classes", {
	yhat <- predict(model)
	expect_true(is.list(yhat))
	expect_true(is_dst(yhat[[1]]))
	newtib <- augment(model)
	expect_true(tibble::is_tibble(newtib))
	expect_true(".fitted" %in% names(newtib))
	expect_equal(yhat, newtib$.fitted)
})
