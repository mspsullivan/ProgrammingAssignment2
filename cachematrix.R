##  This function will cache the inverse: Bob Sullivan

## call makeCacheMatrix to create one followed by a call to cacheSolve()
## Then, when you pull the inverse with getinverse() you will get the inverse 
## from the cache, if it is available.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will create and cache the inverse of a makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
## sample code works like this:
## mc <- makeCacheMarix(matrix(c(4,3,2,1),2,2))
## cacheSolve(mc)
## mc$getinverse()
