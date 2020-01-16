## First function: makeCacheMatrix, creates a special "matrix" for caching inverse of matrix
## Second function: cacheSolve, return the inverse matrix 

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse_mx <- matrix()
        set <- function(y) {
                x <<- y
                inverse_mx <- matrix()
        }
        get <- function() x
        setinverse <- function(solve) inverse_mx <<- solve
        getinverse <- function() inverse_mx
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        inverse_mx <- x$getinverse()
        if(! any(is.na(inverse_mx))){
                message("getting cached data")
                return(inverse_mx)
        }
        data <- x$get()
        if(det(data) == 0){
                message("the matrix is not invertible")
                return()
        }
        inverse_mx <- solve(data, ...)
        x$setinverse(inverse_mx)
        inverse_mx
}
