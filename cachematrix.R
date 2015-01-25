
## This function creates a special "matrix" object that can cache its inverse
## Assumption is that the matrix is Invertible 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  ## Intialising inverse matrix value incase matrix is changed
  if(!identical(get(), m)) {  
      x <<- y
      m <<- NULL
    }
  }
  
  get <- function() x
  ## Using R function Solve to calculate the inverse of the Matrix
  setInvmatrix <- function(solve) m <<- solve 
  getInvmatrix <- function() m
  list(set = set, get = get,
      setInvmatrix = setInvmatrix,
      getInvmatrix = getInvmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvmatrix(m)
  m
  
}
