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
        
        ## creating m as the cache variable
        m <- NULL
        
        ## a set function, it sets the origional data
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## a get function, to retrieve the origional data
        get <- function() x
        
        ## a setsolve function to set the inverted data
        setsolve <- function(solve) m <<- solve
        
        ## a getsolve data to get the inverted data
        getsolve <- function() m
        
        ## saving the bunch in a list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## a function for retrieving information from makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        ## first attempt to find the information in m
        m <- x$getsolve()
        
        ## if found retrieve from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if not found, calculate the invertion and save it. 
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
