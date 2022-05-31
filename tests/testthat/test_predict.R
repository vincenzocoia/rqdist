x <- c(0.919, 0.782, 0.075, -1.989, 0.62, -0.056, -0.156, -1.471, -0.478, 0.418)
y <- c(1.359, -0.103, 0.388, -1.054, -1.377, -0.415, -0.394, -0.059, 1.1, 0.763)
dat <- data.frame(x = x, y = y)
x1 <- data.frame(x = 0)
x2 <- data.frame(x = c(-2, 0))
model <- rqdist(y ~ x, data = dat)

yhat_proper <- predict(model)
yhat_x1     <- predict(model, newdata = x1)
yhat_x2     <- predict(model, newdata = x2)
yhat_raw     <- predict(model, newdata = x2, rearrange = FALSE)
# newtib_proper <- augment(model)
# newtib_raw    <- augment(model, rearrange = FALSE)
# newtib_x1     <- augment(model, newdata = x1)


test_that("model is an rq.process object", {
	expect_true(inherits(model, "rq.process"))
})

test_that("predictions output proper classes", {
	expect_true(is.list(yhat_proper))
	expect_true(is.list(yhat_raw))
	expect_true(is.list(yhat_x1))
	expect_true(is.list(yhat_x2))
	expect_true(distionary::is_distribution(yhat_proper[[1]]))
	expect_true(distionary::is_distribution(yhat_x1[[1]]))
	expect_true(distionary::is_distribution(yhat_x2[[1]]))
	expect_null(yhat_raw[[1]])
})

test_that("Distributions are discrete", {
	expect_true(distionary::is_finite_dst(yhat_proper[[1]]))
	expect_true(distionary::is_finite_dst(yhat_x1[[1]]))
	expect_true(distionary::is_finite_dst(yhat_x2[[1]]))
})

rm("x", "y", "dat", "x1", "x2", "model", "yhat_proper",
   "yhat_x1", "yhat_x2", "yhat_raw")
