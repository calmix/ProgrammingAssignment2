## Put comments here that give an overall description of what your
## functions do

## The functions I created is meant for calculating the inverse of the matrix we input.
## Since it may takes a lot of time to recalculate the same matrix that have been 
## calculated, using these functions can get the result form cache and save a lot of time.


## Write a short comment describing this function
## The "makeCacheMatrix" function first generates a special "matrix", which in fact is a 
## list containing functions to set and get value of the matrix. Unlike the example, 
## the functions set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_matrix) inv <<- inv_matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The function will check if the "matrix" have been calculated before. If the result
## was stored in cache, it will print the message "getting cached data" and return it,
## if not, the function will calculate the inverse matrix and return it 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
