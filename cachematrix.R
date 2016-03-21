## This function creates a matrix object that caches its inverse through makeCacheMatrix and
## then computes the inverse returned by makeCacheMatrix.

## This function sets the matrix, gets the matrix, sets the inverse of the matrix (using solve function) and gets it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL;
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the matrix delivered by makeCacheMatrix, first checks if the inverse is already
## calculated. If not,it calculates it, stores it in the cache and prints it.

cacheSolve <- function(x, ...) { 
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}
