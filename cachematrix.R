## Calculates the inverse of a Matrix using functions makeCacheMatrix,
## and cacheSolve.

## The makeCacheMatrix function creates a matrix to be inverted and contains
## a list of functions to:
##  *set the value of the matrix -- set()
##  *get the value of the matrix -- get()
##  *set the value of the inverse of a matrix in cache -- setInverse()
##  *get the value of the inverse of a matrix from cache -- getInverse()

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL      ## m is the cache.    
    set <- function(y) {  
        x <<- y
        m <<- NULL   ## clear the  cache
    }
    
    get <- function() x 
    setInverse <- function(inverse) m <<- inverse  ## sets the cache
    getInverse <- function() m                     ## gets the cache 
    list(set = set, get = get,  
         setInverse = setInverse, 
         getInverse = getInverse)
}


## The function cacheSolve calculates the inverse of the matrix 
## created with the above function. It first checks to see if 
## the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {  
    m <- x$getInverse()
    if(!is.null(m)) {     ##check the cache
        message("getting cached data")
        return(m)
    }       
    data <- x$get()
    m <- solve(data, ...) ##calculate the inverse of the matrix.
    x$setInverse(m)
    m
}
