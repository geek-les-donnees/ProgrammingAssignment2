## This set of functions caches the inverse of a matrix, if it has been operated before
## This procedure allows to reduce the computation needed to return a value runed before


## makeCacheMatrix creates a fake "Matrix", containing a list of functions... 
## ... to set and get the value of a matrix, and to set an get the inverse value of the matrix.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns a matrix that is the inverse of 'x'. 
## makeCacheMatrix arguments should be inserted in this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)

m1 %*% n1

solve(m1)


myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)
