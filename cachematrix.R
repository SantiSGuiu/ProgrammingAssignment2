## GENERAL COMMENT:

## The next two functions allow us to compute the inverse of an 
## invertible matrix and to store that value in the cache of our
## machine in order to avoid its iterated calculation when we need
## to compute it repeteadly. This is specially useful for an 
## operation that might take a very long time and where the value
## is referenced often.



## FIRST FUNCTION:

## This function is passed an invertible matrix and creates two 
## objects, the original matrix and what will be the cached value
## of the inverse of the matrix, that is originally set to NULL.
## The last 3 functions are defined here but will be only used by 
## "cacheSolve" to: fetch the value of x, store its inverse in the
## cache with the superassignment and fetch it from the cache. I
## have added "{}" in these functions just for clarity.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinv <- function(solve) {inv <<- solve}
    getinv <- function() {inv}
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## SECOND FUNCTION:

## This function accesses the object (the invertible matrix) created
## by "makeCacheMatrix". If its inverse has not been calculated yet,
## this function computes it and stores it in the cache. Otherwise, it 
## fetches it from the cache and returns its value.

cacheSolve <- function(x, ...) {
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
