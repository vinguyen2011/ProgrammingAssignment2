## Those two functions can cache the inverse of a matrix

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache. 
## If the matrix is not invertible, the function will return a null value

cacheSolve <- function(x, ...) {
    ## Check if x is a square invertible matrix
    if (nrow(x$get())==ncol(x$get()))
    {  
      ## Return a matrix that is the inverse of 'x'
    
      m <- x$getSolve()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setSolve(m)
      m
    }
    else
    {
      m <- NULL
      return(m)
    }
  
}
