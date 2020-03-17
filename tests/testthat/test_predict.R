library(testthat)
library(rqdist)

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

test_that("predictions are distributions ('dst' objects)", {
	yhat <- predict(model)

})
