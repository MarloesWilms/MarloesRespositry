## This pair of functions cache the inverse of a matrix


## This function creates a special "matrix" object that 
## can cache its inverse. 

## Inversible matrix is passed in this function as x1.
## inv is set at NULL
## Set functions stores matrix data.
## Get functions retreive matrix data.
## All information will be printed in a list.

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

## Result of makeCacheMatrix is passed in this matrix as
## x2.
## Stored data retreived by inv.
## If inv = NULL new inversion is of matrix is calculated
## with solve() and stored as inv and printed.
## If inv != NULL message and inv are printed.

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
