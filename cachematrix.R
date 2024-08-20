## makeCacheMatrix and cacheSolve are based on code provided by R. D. Peng
## and are altered to perform the same caching behavior for a matrix and
## its inverse

## The description for the behavior of makeCacheMatrix is based on the
## description of the original code provided by R. D. Peng.
## makeCacheMatrix does the following, directly reflecting the code for
## a vector and its mean, but for a matrix and its inverse:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) I <<- solve
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve does the following, directly reflecting the code provided by
## R. D. for a vector and its mean, but for a matrix and its inverse.  The
## description of the behavior of this function is modified from the description
## of the original code provided by R. D. Peng.
## cacheSolve calculates the mean of the special "vector" created with makeCacheMatrix
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation. Otherwise, it
## calculates the mean of the data and sets the value of the mean in the cache via
##the setmean function.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}
