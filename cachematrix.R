## These functions cache the inverse of a matrix for future use.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inverseOfMatrix <- NULL
  
    set <- function(y) {
            x <<- y
            inverseOfMatrix <<- NULL
    }
  
    get <- function() x
  
    setInverse <- function(inverse) inverseOfMatrix <<- inverse
  
    getInverse <- function() inverseOfMatrix
  
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
  
}


## This function computes the inverse of the special "matrix" returned
#  by makeCacheMatrix above. If the inverse has already been calculated
#  (and the matrix has not changed), then the cacheSolve retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    inverseOfMatrix <- x$getInverse
  
    if(!is.null(inverseOfMatrix)) {
        message("getting cached inverse")
        return(inverseOfMatrix)
    }
  
    data <- x$get()
  
    inverseOfMatrix <- solve(data, ...)
  
    x$setInverse(inverseOfMatrix)
  
    inverseOfMatrix
}
