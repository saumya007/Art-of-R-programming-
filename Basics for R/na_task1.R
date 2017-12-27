df1 <- data.frame(x = sample(c(1 ,NA, 2, 3, 4, 5), 1000, replace = TRUE), y = runif(1000),
                  z = sample(c(1 ,NA), 1000, replace = TRUE, prob = c(0.3, 0.7)))

mean(is.na(df1$x))
mean(is.na(df1$y))
mean(is.na(df1$z))

x <- sample(100)
sapply(x, function(x) {
  x^2
})

cols_na_val <- function(df, thres) {
  res <- apply(df, 2, function(x) {
    mean(is.na(x))
  })
  
  names(res[res > thres])  
}

na_val_cols(df1, .5)
na_val_cols(df1, .1)

cols_type <- function(df) {
  res <- apply(df, 2, function(x) {
    typeof(x)
  })
  
  res
}

cols_type(iris[, -5])

typeof(iris$Sepal.Length)

lapply(iris, function(x) {
  typeof(x)
})

generate_blobs <- function(n_datapoints, n_blobs, min, max, spread) {
  
}
