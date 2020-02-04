## This pair of functions allow us to calculate the inverse of an invertible matrix
## and to cache the result in order to avoid a recalculation

## The following function sets a list of functions allowing us to set/retrieve the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        setinv <- function(solve) inv <<- solve

        getinv <- function() inv

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function check if the inverse of the matrix has already been calculatled, 
## if not it will calculate it and then return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()

        inv <- solve(data, ...)

        x$setinv(inv)

        inv
}

