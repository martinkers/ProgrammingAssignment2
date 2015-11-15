############################
## There are two functions in this script: makeCacheMatrix and cacheSolve,
## which help to inverse a matrix and store it in cache 
## Date created: 15 November 2015

############################
## Function: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

## inputs for this function: vector with numbers, # of matrix rows, # of matrix columns
## example: x <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get function
  get <- function() x
  
  # set inverse 
  setinverse <- function(inverse) m <<- inverse
  
  # get inverse
  getinverse <- function() m
  
  # return list with set, get, setinverse, getinverse functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

############################
## Function: cacheSolve
## This function returns the inverse of the matrix 'x' returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

# input for this function: matrix 'x' from makeCacheMatrix
# example: cacheSolve(x)

cacheSolve <- function(x, ...) {
  
  # check if cached inverse matrix exists, if yes, return cached inverse matrix
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if no cached matrix exists, get data from generate matrix x
  data <- x$get()
  
  # calculate inverse matrix with solve() function
  m <- solve(data, ...)
  
  # set inverse matrix
  x$setinverse(m)
  
  # print inverse matrix to console
  m
}
