## This pair of functions caches the inverse of a matrix to speed up computation
## time rather than recalculating. It is created as an R Programming assignment.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached inverse value")
                return(inv)
        } else {
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
                inv
        }
}
