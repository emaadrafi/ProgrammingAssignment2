## The following functiona have been modified from the "Caching the Mean of a Vector" example
## These functions compute the inverse of a matrix as opposed to mean of a numeric vector

## As a simple example, the following matrix can be provided to the function as input;
##      A =     [,1] [,2]           Inverse(A) =     [,1] [,2]
##         [1,]   4    3                        [1,]  -2    3
##         [2,]   3    2                        [2,]   3   -4

## This function creates a special "matrix" which is really a list containing functions

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                ## set inverse to NULL each time a new matrix is set
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inv) {
                inverse <<- inv
        }
        getinverse <- function() {
                inverse
        }
        ## A list is assembled where each element contains a function
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks if the inverse of a matrix has already been computed and returns
## either an existing inverse of the matrix or computes a new one

cacheSolve <- function(x) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        message("computing inverse")
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
}
