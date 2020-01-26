## These functions work together to calculate the inverse of a 
## matrix, only if it has not already been done, and then retun
## the inverted value either from a previously stored state or
## or after it is calculated and then stored for later retrival.

## This function creates a list of functions able to be called
## in the global enviornment by other functions (namely
## 'casheSolve')

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get, 
    setmatrix = setmatrix, 
    getmatrix = getmatrix)
}


## This function takes the list of functions created by 
## 'makeCacheMatrix' and uses them to first check if the (now
## global) 'm' variable is emply (NULL).  If it is not, that means
## the inverse of the desired matrix has already been calculated
## and does not need to be recalcluated.  Instead it can just be 
## returned.  However, if 'm' is empty, the matrix inverse has not
## yet been calculated and needs to be done, using the 'solve()'
## function and storing it in the matrix object using the 
## 'setmatrix' function included in the list of functions from 
## 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cashed data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m
}
