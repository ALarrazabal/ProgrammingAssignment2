## Put comments here that give an overall description of what your
## functions do: this function creates an object containing a matrix and able to store the computed inverse matrix in order to later retrieved if needed.

## Write a short comment describing this function: First x is stored as part of the function and as a numeric type of matrix. The object m is defined to be the recipient of the result from applying the solve function to the x matrix. the setter and getter functions stablished to set the information of the matrix inverse into m. and the get function to get the cached data if it exist.

makeCacheMatrix <- function(x = data.matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setminv <- function(solve) m <<- solve
  getminv <- function() m
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## Describe the function: The function takes the matrix-object generated before and get the cached data if exists otherwise it calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getminv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve (data, ...)
  x$setminv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
