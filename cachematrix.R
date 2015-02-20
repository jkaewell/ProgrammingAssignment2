##  This file contains R scripts which caches a matrix inverse
##  The first call to the cacheSolve function 
##  uses the solve() function to initialize the cache with the inverted matrix.
##  Subsequent calls to cacheSolve() returns the cached inverted matrix
##  
## 
##  makeCacheMatrix() returns a list of functions which
##  are used to initialize state and return state stored
##  in the function's environment to enable the caching 
##  of a matrix's inverse.
##


makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL                          ## initialize invMatrix to NULL
##
  set <- function(y) {                       ## the set function allows re-initialization
      x <<- y                                ## of the original matrix and the
      invMatrix <<- NULL                     ## inverted Matrix
    }
##
    get <- function() x            ## get function returns the original input matrix 
##
    setInverse <- function(inverse) invMatrix <<- inverse  ## function to assign matrix
                                                           ## inverse to invMatrix
##
    getInverse <- function() invMatrix        ## function returns cached matrix inverse or 
                                              ## NULL if cache hasn't been initialized
      list( set = set,                        ## return list of functions
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## cacheSolve() checks to see if the invMatrix cache contains the matrix inverse (i.e. 
## invMatrix is not NULL). if invMatrix is equal to NULL, cacheSolve calls 
## the setInverse function contained in the x list which inverts the matrix and saves the
## result into invMatrix.  getInverse is used to retrieve the invMatrix cache.
##

cacheSolve <- function(x, ...) {     ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if (!is.null(invMatrix)) {         ## check to see if the cache is not NULL
    message ("getting cached matrix inverse")
    return (invMatrix)               ## cache contains the inverted matrix, return cache
  }
  data <- x$get()                    ## cache doesn't contain the inverted matrix, get matrix
  invMatrix <- solve(data, ...)      ## use solve() to invert the matrix`
  x$setInverse(invMatrix)            ## set the cache to the matrix inverse
  invMatrix                          ## return the inverted matrix.
}
