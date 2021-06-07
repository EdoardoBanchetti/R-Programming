## with the makeCacheMatrix function I'm building a special matrix, 
# special because it is nothing else than a list of functions

## The cacheSolve function calculates the inverse of the matrix
# created with the makeCacheMatrix function. in doing this, it first checks to see 
# if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets 
# it in the cache via the setinverse function.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix_to_invert <- x$get()
    inv <- solve(matrix_to_invert, ...)
    x$setinverse(inv)
    inv
}

NewMatrix <- makeCacheMatrix(matrix(runif(9), 3, 3))
NewMatrix$get()

cacheSolve(NewMatrix)
