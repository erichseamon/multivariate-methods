CV <- function(form, x, fold = 10, cp = 0.01) {
  # x is the data
  n <- nrow(x)
  prop <- n%/%fold
  set.seed(7)
  newseq <- rank(runif(n))
  k <- as.factor((newseq - 1)%/%prop + 1)
  
  y <- unlist(strsplit(as.character(form), " "))[2]
  vec.accuracy <- vector(length = fold)
  for (i in seq(fold)) {
    # It depends on which classification method you use
    fit <- rpart(form, data = x[k != i, ], method = "class")
    fit.prune <- prune(fit, cp = cp)
    fcast <- predict(fit.prune, newdata = x[k == i, ], type = "class")
    cm <- table(x[k == i, y], fcast)
    accuracy <- (cm[1, 1] + cm[2, 2])/sum(cm)
    vec.accuracy[i] <- accuracy
  }
  avg.accuracy <- mean(vec.accuracy)
  avg.error <- 1 - avg.accuracy
  cv <- data.frame(Accuracy = avg.accuracy, Error = avg.error)
  return(cv)
}