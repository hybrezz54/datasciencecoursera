makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMean <- function(mean) m <<- mean
  getMean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cacheMean <- function(x, ...) {
  m <- x$getMean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setMean(m)
  m
}
