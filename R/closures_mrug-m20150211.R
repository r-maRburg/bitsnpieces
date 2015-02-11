### generate some random data
set.seed(123)
pred <- data.frame(v1 = rnorm(100, 2, 1),
                   v2 = 1:100,
                   v3 = rpois(100, 2),
                   v4 = 200:101)

set.seed(234)
resp <- data.frame(v1 = 1:100,
                   v2 = rnorm(100, 2, 1),
                   v3 = 200:101,
                   v4 = rpois(100, 2))

### define closure
runLM <- function(pred) {

  function(y) {
    summary(lm(y ~ pred))$r.squared
  }

}

### apply closure of one predictor to one response
runLMv1 <- runLM(pred[, 1])
runLMv1(resp[, 1])

runLMv4 <- runLM(pred[, 4])
runLMv4(resp[, 2])

### apply closure of one predictor to each response
apply(resp, 2, runLMv1)
apply(resp, 2, runLMv4)

### apply closure to each rsponse/predictor combination
res <- lapply(seq(ncol(pred)), function(j) {

  f <- runLM(pred[, j])
  out <- apply(resp, 2, FUN = f)
  return(out)

})

res
