## This module contains functions to calculate the inverse of matrices, and
## once we have the inverse, we use that value instead of calculating it again.
##
## Usage example:
##
### builds the object with the matrix whose inverse we want to calculate
## > z <- makeCacheMatrix(matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3))
##
### here we have to do the calculation, as we haven't got the inverse yet
## > cacheSolve(z)
## Calculating inverse matrix
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
##
### calling cacheSolve again, we get the previously calculated inverse
## > cacheSolve(z)
## Returning cached inverse
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
###############################################################################

##
## Builds a list with functions to get or set the
## internal matrix and its inverse.
##
## Parameters:
##  x: the matrix for which we want to calculate the inverse
##
## Return:
##  a list containing functions to access the internal matrix and
##  its inverse, if already calculated
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(i) {
        inv <<- i
    }

    getInverse <- function() {
        inv
    }

    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##
## Calculates the inverse, or return the cached value, of the matrix that
## belongs to the object created by the makeCacheMatrix function.
##
## Parameters:
##  x: the list returned by makeCacheMatrix
##
## Return:
##  the inverse of the matrix accessed through x functions
##
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        # Inverse has already been calculated
        message("Returning cached inverse")
        return(inv)
    }

    # Inverse has not been calculated yet
    data <- x$get()
    message("Calculating inverse matrix")
    inv <- solve(data)

    # Save the inverse matrix to be used the next time cacheSolve is called
    x$setInverse(inv)
    inv
}
