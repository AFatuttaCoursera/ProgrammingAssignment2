## This package has been created to manage caching for inverse matrix
## 

## function makeCacheMatrix create a set of functions related to the management
## and caching of the inverse of an input matrix ( x )

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        if (!identical(x,y))
        {
            x <<- y
            i <<- NULL
        }
    }
    get <- function() x
    setInverseMatrix <- function(inv_matrix) i <<- inv_matrix
    getInverseMatrix <- function() i
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve receives in input an object that has been created with makeCacheMatrix
## and solves the inverse matrix of the input, saving it into the input object itself

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverseMatrix()
    if(!is.null(i)) {
        message("getting cached inverse data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverseMatrix(i)
    i
}
