## These functions compute the inverse of a matrix. If the matrix inverse has 
## already been computed, it can be cached, allowing it to be returned without
## computing it again in the future.

## This function makes a cache of a matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmat <- function(solve) m <<- solve
    getmat <- function() m
    list(set = set, get = get,
         setmat = setmat,
         getmat = getmat)
}


## This function checks to see if the inverse of a matrix has already been 
## computed. If so, it returns the cached matrix. If not, it calculates the 
## matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmat()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmat(m)
    m
}


##test scripts
mtest<-matrix(1:4,2,2)
mtest
solve(mtest)

x<-matrix(1:4,2,2)
cacheSolve(makeCacheMatrix(x))

solve(mtest)==cacheSolve(makeCacheMatrix(x))
