## Below functions are used to calculate the inverse of a matrix
## If the inverse is already previously calculated,
## it will take the cached version of previously calculated inverse.


# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y){
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(solve) matrixInverse <<- solve 
    getMatrixInverse <- function() matrixInverse
    list(set = set, get = get, 
         setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
    
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getMatrixInverse()
    if(!is.null(matrixInverse)){
        message("getting cached data")
        return (matrixInverse)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setMatrixInverse(matrixInverse)
	return(matrixInverse)
}