## makeCacheMartix -->  function is to create an inverted matrix
##                      Only square matrices are inverted, all other matrices will return an error
##                      No matrix is returned, instead it is put into cache and can be retrieved by cacheSole()
##
## cacheSolve -->       Returns a matrix from cache, absolutely to computation is done in this function


## 
makeCacheMatrix <- function(x = matrix()) {
        
        ## firstly make sure the matrix is square!
        if (nrow(x) != ncol(x)) { 
                stop("only square matrices can be inverted") 
        }
        
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
