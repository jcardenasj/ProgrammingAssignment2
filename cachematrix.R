## These set of functions support the creation and cache evaluation of the 
## inverse of a matrix.

## makeCacheMatrix: this function create a cache matrix object, whose inverse is
## allocated on cache and it is set to null where the matrix is updated.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: this function return the inverse of a cache matrix object x. If the
## cache matrix has its inverse already calculated it returns the cache value.
## Otherwise, it calculates the inverse, updates the value of the inverse in the
## cache matrix object and return the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
