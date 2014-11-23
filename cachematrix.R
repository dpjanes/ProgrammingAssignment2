## Programming Assignment 2
## David Janes
##
## This provides two functions
##
## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
##     that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
##     "matrix" returned by `makeCacheMatrix` above. If the inverse has
##     already been calculated (and the matrix has not changed), then
##     `cacheSolve` should retrieve the inverse from the cache.
##

## makeCacheMatrix:
## create a vector (of functions) that allows for matrix inverse
## caching. Matrix inverses cache be computed (and cached)
## using the cacheSolve() function
##
## Notes:
## - m$get will return the underlying matrix
## - use cacheSolve(m) to get matrix inverse (efficiently)
## - calling m$set will clear the inverse cache, as expected
## - m$setinv/m$getenv are for cacheSolve's use only
makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    get <- function() x

    setinv <- function(xi) {
        xi <<- xi
    }
    getinv <- function() {
        return(xi)
    }

    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve:
## given 'x' created by 'makeCacheMatrix()', return the inverse matrix of 'x'.
## The R function 'solve()' is used to compute the inverse
##
## Notes:
## - matrix results are cached and will be return the second time
## - additional arguments will be passed to solve()
## - changing the additional arguments won't reset the cached result
## - the result is a proper R matrix, not a makeCacheMatrix()
cacheSolve <- function(x, ...) {
    message(...)
    xi <- x$getinv(...)
    if(!is.null(xi)) {
        message("getting cached data")
        return(xi)
    }

    xi <- solve(x$get(), ...)
    x$setinv(xi)

    xi
}
