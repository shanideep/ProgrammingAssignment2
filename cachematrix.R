## This set of functions inputs a matrix and calculates it's inverse
## The inverse is done only once and during every other recall of inverse, cache stored inverse is returned

## This function takes a matrix as input and converts it to list which has 4 matrix elements

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(inversematrix) m <<- inversematrix
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## This function calculates inverse of matrix if inverse has not been done before.
## If inverse has been already calculated then the return value is done though cache stored value.

cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}
