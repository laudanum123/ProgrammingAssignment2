## This script allows creation of a matrix using the makeCacheMatrix function and computation of the 
## inverse of that matrix using the cacheSolve matrix. If the inverse for a matrix has been computed once 
## the value is not computed again but retrieved from a stored variable.

## This function creates a special matrix with getter and setter methods for value of x 
## as well as the matrix inverse.


makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) s <<- solve
    
    getinverse <- function() s
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function inverts the matrix and cashes the result using the $setinverse method of the matrix object.
## In case the inverse has already been computed the method retrieves the inverse from memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    s <- x$getinverse()
    
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    data <- x$get()

    s <- solve(data)
    x$setinverse(s)
    s
}
