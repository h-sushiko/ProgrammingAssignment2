## These functions below give you a special matrix and compute an inverse of
## the matrix in a time-saving way.

## makeCacheMatrix: creates a special matrix.
## input: a matrix (this must be a square and invertible)
## output: a list which contains 4 functions
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInv <- function(inv) invMatrix <<- inv
        getInv <- function() invMatrix
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve: calculates the inversion of the special matrix.
##             returns cached data if it has already been done.
## input: the return value of "makeCacheMatrix"
## output: a (inversed) matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getInv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        matrix <- x$get()
        im <- solve(matrix, ...)
        x$setInv(im)
        im
}
