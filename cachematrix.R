# The functions below calculate the inverse of matrix and store it in memory using
# cacheSolve function. The cacheSolve function which uses a vector returned from makeCacheMatrix function



# makeCacheMatrix does the following:
# 1. Sets and gets the value of input matrix using set and get functions
# 2. Sets and gets the value of inverse of matrix using setInverse and getInverse functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse )
}

# cacheSolve functions checks whether the matrix input is new or not. If old, it
# retrieves the inverse from the memory, else computes the new inverse 
# of the input matrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("Getting Cached Data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i  
  ##  Returns a matrix that is the inverse of 'x'
}