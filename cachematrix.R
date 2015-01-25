## There is a pair of functions that cache the inverse of a matrix.
##  Assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(rexp(4), ncol=2)) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix"(lets call it 'X') returned by 
##   makeCacheMatrix above. 
##   If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve the 
##   inverse from the cache. - This part is not implemented yet due to lack of time.
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m<-x$set(data)
    m <- solve(data, ...)    
    x$setsolve(m)
    m ## Return a matrix that is the inverse of 'X'
}

