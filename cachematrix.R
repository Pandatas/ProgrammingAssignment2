## Coursera - John Hopkins Data Science Specialization, R-Programming week 3 assignment

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## data object
  set <- function(y) { ##set the value of the matrix
    x <<- y ## data object
    m <<- NULL
  }
  get <- function() x ##get the value of th matrix
  setsolve <- function(solve) m <<- solve ##set the inverse of the matrix
  getsolve <- function() m ##get the inverse of the matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.


cacheSolve <- function(x, ...)  {
  ## return the inverse of x, or calculate & return if cache is empty
  m <- x$getsolve()
  if(!is.null(m)) { ##check if inverse has already been calculated (and the matrix has not changed) and cached data is used
    message("getting cached data")
    return(m) ## return inverse from cache
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m # return inverse for changed matrix with respect to the one used in makeCacheMatrix above
}

