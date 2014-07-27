## Put comments here that give an overall description of what your
## functions do
## Purpose of these functions is to cache the resul of costly inverse operation
## on a Matrix, this relies on lexical scoping here we compute and store the
## inverse of mean in parent env of accessor function.
##

## Write a short comment describing this function
## Usage:
## myCacheMat <- makeCacheMatrix() To get this new Matrix
## myCacheMat$set(matrix(c(1,-1/4,-1/4,1),2,2)) You can set any matrix to this. It assumes 
## this is a square invertible matrix.
## myCacheMat$get() to get and print Mat

makeCacheMatrix <- function(x = matrix()) {
  invCache <- NULL
  
  set <- function(y) {
    x <<- y
    invCache <<- NULL
  }
  get <- function() x
  
  getInv <- function() invCache
  setInv <- function(inv) invCache <<- inv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve(myCacheMat) to get inv matrix it can take any argument you may want to Solve.
## Please note argumen to solve will be ignored when it returns cahced result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
