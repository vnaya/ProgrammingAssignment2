## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix: This function creates
    ## a special "matrix" object that can cache its inverse.
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    ## saves inverse       
    setinverse <- function(inverse) i <<- inverse
    ## get inverse       
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## This function computes the inverse of the
    ## special "matrix" returned by makeCacheMatrix above.
    ## If the inverse has already been calculated (and the
    ## matrix has not changed), then the cachesolve should
    ## retrieve the inverse from the cache
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## reads data of the object       
    data <- x$get()
    ## inverts matrix
    i <- solve(data, ...)
    ## exports the inverse of the matrix       
    x$setinverse(i)
    i
}