## Functions very similar to that given in the example of this assignment.
## Instead of finding the mean of a numerical vector, we find the 
## inverse of a matrix
## We cache potentially time consuming methods of getting and setting both the 
## matrix and it's inverse

## In the first function we do the caching

makeCacheMatrix <- function(x = matrix(1:9, 3, 3)) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached result")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}