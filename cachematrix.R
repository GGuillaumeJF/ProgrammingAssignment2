## Put comments here that give an overall description of what your
## functions do
## These are two functions that compute the inverse of a matrix and cache it



## Write a short comment describing this function
## The following function creates a special "Matrix" object that can cache its inverse
## It is a list of 4 functions :
## 1. To set the value of the matrix
## 2. To get the value of the matrix
## 3. To set the inverse of the matrix
## 4. To get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMAT <- NULL
  set <- function(y) {
    x <<- y
    invMAT <<- NULL
  }
  get <- function() x
  setinv <- function(inverseMAT) invMAT <<- inverseMAT
  getinv <- function() invMAT
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMAT <- x$getinv()
  if(!is.null(invMAT)) {
    message("getting cached data")
    return(invMAT)
  }
  data <- x$get()
  invMAT <- solve(data, ...)
  x$setinv(invMAT)
  invMAT
}
