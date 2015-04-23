## The two functions below, 'makeCacheMatrix' and 'cacheSolve' work together to
## invert a matrix and then make it available in the cache environment for easy
## retrieval

## The function makeCacheMatrix stores 4 functions: set, get, setinverse & getinverse.

## The function 'set' changes the matrix stored in the function makeCacheMatrix to
## the argument provided to it and resets the value of anything
## that was computed using the previous matrix.

## The function 'get' retrieves the matrix stored in the main function 'makeCacheMatrix'

## The function 'setinverse' stores the value of the inverted matrix in the
## function makeCacheMatrix

## The function 'getinverse' returns the inverted matrix stored by 'setinverse'

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve initilly uses 'getinverse' stored in the main
## function makeCacheMatrix to check if the value of matrix 'mat' exists. If it
## does then it messages "getting cached data" and then returns the previously
## stored inverted matrix. If the value of 'mat' does not exists it uses the
## 'get' function stored in the main function 'makeCacheMatrix', then uses the
## function 'solve' to invert the matrix and finally uses the function
## 'setinverse' stored in the main function 'makeCacheMatrix' to store the newly
## inverted matrix in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}