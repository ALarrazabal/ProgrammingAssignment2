## Put comments here that give an overall description of what your
## functions do: this function creates an object containing a matrix another object called m and able to store the computed inverse matrix in order to later retrieved if needed.

## Write a short comment describing this function: First x is stored as part of the function and as a numeric type of matrix. The object m is defined to be NULL and the later recipient of the result from applying the solve function to the x matrix. the original matrix values are assign to a "dummy" object called "y" using the <<- operator.
# The setter and getter functions are stablished. First to set a function which will change "y" but taking as input values those from the "x" object and with assign a NULL value to "m". The get function retrieves the x values from the parent environment of makeCacheMatrix.
#Finally the setter for the solve function is defined using the <<- operator to assign the input argument to the value of m in the parent environment making possible to access it after makeCacheMatrix is used.  information of the matrix inverse into m and the get function to get the cached data if it exist.

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

## Describe the function: The function takes the matrix-object generated before and get the cached data, if it exists, otherwise it calculate the inverse of the matrix. To do this it uses the functions stored inside the matrix previously created by makeCacheMatrix.
## Return a matrix that is the inverse of 'x'

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
}
