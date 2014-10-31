# Coursera R Programming Assignment 2
# Matrix inversion

# creates a special "vector" that sets/gets vector and sets/gets mean
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# calculates the mean of the special "vector"
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# sample call...
# > vector2 <- makeVector(c(2,4,6))
# > cachemean(vector2)
# [1] 4
# > cachemean(vector2)
# getting cached data
# [1] 4