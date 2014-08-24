##
## The following two functions can be used to compute and cache the inverse of a matrix.
##

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the inverse matrix
  inverseMat <- NULL  
  
  # set matrix $y into cache
  set <- function(y) {
    x <<- y    
    inverseMat <<- NULL
  }
  
  # get the cached matrix
  get <- function() x
  
  # set the inverse matrix of $x into cache
  setInverseMatrix <- function(mat) inverseMat <<- mat
  
  # get the cached inverse matrix 
  getInverseMatrix <- function() inverseMat
  
  # return function list
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  
  # get the cached inverse matrix
  inverseMat <- x$getInverseMatrix()

  # return the cached inverse matrix if it's not null
  if(!is.null(inverseMat)) {
    message("get inverse matrix from cache")    
    return(inverseMat)
  }
  
  # otherwise, get the underlying matrix of $x and return its inverse
  mat <- x$get()  
  inverseMat <- solve(mat, ...)
  
  # set the inverse of matrix into cache  
  x$setInverseMatrix(inverseMat)
  
  # return the inverse
  inverseMat
}
