## This pair of functions cache the inverse of a matrix


## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x1 = matrix()) {
    inv <- NULL
    set <- function(y) {
        x1 <<- y
        inv <<- NULL
    }
    get <- function() x1
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x2, ...) {
    inv <- x2$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x2$get()
    inv <- solve(data, ...)
    x2$setinverse(inv)
    inv
}
