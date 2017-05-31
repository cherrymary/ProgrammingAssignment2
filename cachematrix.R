## makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## x and m initialization, x is a n argument to function, m is set to NULL
      m <- NULL
      set <- function(y) { ## assignes variables from global environment ( x from global to y) to local functionn variables
            x <<- y
            m <<- NULL
      }
      get <- function() x  ## defines the getter for the vector x
      setinvert <- function(invert) m <<- invert   ##  assignes the input argument to the value of m
      getinvert <- function() m ## defines getter for invert function
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}



##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
      m <- x$getinvert()
      if(!is.null(m)) {  ## if inverted matrix was found, returns that matrix
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...) ## if there was no inverted matrix used, calculates matrix and assigns to m
      x$setinvert(m) #Returns a matrix that is the inverse of 'x'

      m
}       




# example for testing
#z <-makeCacheMatrix(matrix(c(1,1,4,0,3,1,4,4,0), nrow = 3, ncol = 3))
#b <- cacheSolve(z)
#b
