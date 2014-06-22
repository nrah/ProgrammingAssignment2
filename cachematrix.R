## These functions allow you to cache the results of matrix inverse calculations
## and use them later without having to compute them again

## Create a special "matrix" that can cache its inverse for future use

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


## If there is already an inverse for 'x', get it from the cache and return it
## Otherwise, calculate the inverse first

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
