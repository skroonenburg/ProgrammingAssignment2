## The makeCacheMatrix and cacheSolve functions in this file are useful
## for caching the output of the solve function, such that solve needs
## to be executed only once for any given matrix. Subsequent calls to
## cacheSolve will return the cached matrix result, rather than invoking
## additional calls to solve - thereby reducing computational load.
##
## For example, to calculate the inverse of a matrix m:
##     cacheMatrix <- makeCacheMatrix(m)
##     inverseMatrix <- cacheSolve(cacheMatrix)
##
## Subsequent calls to calculate the matrix inverse won't require an
## invocation of solve(), instead the cached matrix will be used.
##     inverseMatrix2 <- cacheSolve(cacheMatrix)
##
## In addition, the original matrix can be updated or retrieved using 
## functions on the matrix cache:
##     cacheMatrix$set(newMatrix)
##     cacheMatrix$get()


## makeCacheMatrix will produce a list object, containing references to
## functions that can be used to manage the cache of the result of a 
## call to the solve() function to find the inverse of the provided matrix
## Call makeCacheMatrix and pass the matrix to be inverted:
##   cm <- makeCacheMatrix(m)
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedInverse <<- inverse
    getinverse <- function() cachedInverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve will return the result of the solve function 
## for the matrix passed previously into makeCacheMatrix
## If the solve operation has been invoked previously for this
## maxtrix and cached, then the cached result will be returned.
## Otherwise, solve will be executed, the result cached and 
## then returned.
## Execute by passing in the object returned from makeCacheMatrix:
##   inverseMatrix <- cacheSolve(cm)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...) 
    x$setinverse(i)
    i
}