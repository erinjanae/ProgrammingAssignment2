## These functions work together to create a special "matrix" object that
## contains a matrix object and a cached version of the inverse of the
## object if the inverse has been solved for without the matrix being
## changed. The first function creates the special object and contains
## the matrix and inverse cached matrix. The second function computes
## the inverse of the matrix if the inverse of the matrix hasn't been
## solved for since the matrix last changed.

## This function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the cached inverse matrix to null
  i <- NULL
  
  ## setter function to set the matrix
  set <- function(y) {
    ## set the matrix
    x <<- y
    ## set the cached inverse matrix to null since the matrix is being 
    ## changed
    i <<- NULL
  }
  
  ## getter function to get the matrix
  get <- function() x
  
  ## setter function to set the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## getter function to get the inverse matrix
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then it will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## attempt to get the cached inverse of the matrix from the special
  ## "matrix" object incase it already has it cached
  i <- x$getinverse()

  ## check to see if cached inverse matrix isn't null. If it is not null, 
  ## it can just be returned and won't need to be recomputed.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## get the matrix object
  data <- x$get()
  
  ## compute the inverse of a square matrix with the solve function
  i <- solve(data, ...)
  
  ## set the inverse of the square matrix in the special "matrix" object
  x$setinverse(i)
  
  ## return a matrix that is the inverse of 'x' 
  i
}