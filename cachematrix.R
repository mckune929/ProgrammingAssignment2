## The makeCacheMatrix and cacheSolve functions save clock cycles when repetitively calculating the input
## of a square matrix. This is accomplished by creating a cache in which the inverse is saved. So long as 
## the input does not change, the inverse matrix will simply be pulled from memory, rather than repeatedly
## (and unnecessarily) recalculated.

## makeCacheMatrix takes a square, invertible matrix as its input. The function returns a list of four
## functions to set the input matrix, pull the input matrix from the cache, set the inverse of the input
## matrix, and pull the inverse matrix from the cache. 

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        setInput <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        getInput <- function() x
        setInverse <- function(inverseValue) {
                inverseMatrix <<- inverseValue
        }
        getInverse <- function() inverseMatrix
        list (setInput = setInput, getInput = getInput, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve attempts to pull the cached inverse of the input matrix, and returns the cached value
## if the inverse matrix is not null. If the inverse matrix is null, cacheSolve calculates the inverse,
## saves it to the cache, and returns the inverse matrix as the only output.

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("Pulling cached inverse matrix")
                return(inverseMatrix)
        }
        inputMatrix <- x$getInput()
        inverseMatrix <- solve(inputMatrix, ...)
        x$setInverse(inverseMatrix)
}
