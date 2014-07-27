##Two functions that cache the inverse of a matrix

##The below functions caches the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  ##initializing the inverse of matrix
  i <- NULL
  ##setting the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##method to get the matrix
  get <- function() x
  
  ##method to set the inverse of the matix
  setinverse <- function(inverse) i <<- inverse
  
  ##method to get the inverse of the matix
  getinverse <- function() i
  
  ##returning all the methods
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##Below function computes the inverse of the matrix. If the 
##inverse has been calculated before and the matrix has not been
##changed then 'cachesolve' will return the inverse from the cache
cacheSolve <- function(x, ...) {
  ##returns the inverse of matrix 'x'
  i <- x$getinverse()
  ##if inverse is already computed just return it
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  ##get the computed matrix
  data <- x$get()
  ##solving inverse of the matrix
  i <- solve(data)
  ##set the inverse of the matrix
  x$setinverse(i)
  ##return the matrix
  i
  