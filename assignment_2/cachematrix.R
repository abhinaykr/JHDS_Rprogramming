## makeCacheMatrix takes an arugment of type matrix
## eg. a<-makeCacheMatrix(x<-matrix(1:4,2,2))
## sub functions, set, get, setinverse and getinverse let you access & modify the matrix
## cacheSolve function returns the inverse of the matrix
## the function stores the inverse matrix and returns the stored (cached) matrix when
## the same matrix argument is passed again to save computation time
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m<<- inverse
  getinverse <- function() m
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve function returns the inverse of the matrix
## the function stores the inverse matrix and returns the stored (cached) matrix when the same matrix argument is passed again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
