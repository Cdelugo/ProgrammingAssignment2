## Functions created for ProgrammingAssignment2 on local scoping for Coursera R programming course

##This function creates a special "matrix" object that can cache its inverse
##Contains the following functions:
## * setMatrix      set the value of a matrix
## * getMatrix      get the value of a matrix
## * cacheInverse   get the cached value (inverse of the matrix)
## * getInverse     get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
# holds the cached value or NULL if nothing is cached
        # initially set it to NULL
        cache <- NULL
        
        # stores a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # flushes the cache
                cache <<- NULL
        }

        # returns stored matrix
        getMatrix <- function() {
                x
        }
        # caches given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        # gets value
        getInverse <- function() {
                cache
        }
        # returns a list of functions
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# This function calculates the inverse of a "special" matrix created above
cacheSolve <- function(y, ...) {
        # gets cached value
        inverse <- y$getInverse()
        # if a cached value exists returns it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise gets matrix & caclulates inverse and stores in cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        # returns inverse
        inverse
}
