## This function cache a Matrix and then return its inverse

## The first step: create a function to cache the Matrix. 
##Watchout: This matrix has to be invertible! Example: matrix(c(1, 2, 3, 4),nrow=2,ncol=2)

makeCacheMatrix <- function(x = matrix()) {
  ##set up an empty cache for the inverse "I"
  I <- NULL
  ##create the "set" function: change the initial value "x" by a new one "y" and empy the inverse "I" cache.
  set <- function (y){
    x <<- y
    I <<- NULL
  }
## create the "get" function: store the matrix inputed previously  
  get <- function() {x}
## create the "setinv" function : Create the Inverse of the Matrix by calling the solve function
  setinv <- function(solve) I <<- solve
## return the inverse matrix calculated
  getinv <- function(){I}
## create a list of function that can be called outside this function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## cacheSolve: will return the inverse matrix if already cached in the previous function else will inverse the matrix.

cacheSolve <- function(x, ...) {
  ##go get the inverse matrix if already calculated in the previous function.
  I <- x$getinv()
  ## if "I" is not empty return the inverse matrix I
  if(!is.null(I)) { message("getting cached data")
  return(I)}
  ## is "I" is empty take the new matrix "x" and inverse it
  data <- x$get()
  I<- solve(data,...)
  ## Store the the inverse matrix
  x$setinv(I)
  ## return the inverse matrix
  I }

# Example: Inverse of 2x2 matrix 
# x <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=T)
# test <- makeCacheMatrix(x)
# cacheSolve(test)
# Expected Answer:
#       [,1]    [,2]
# [1,] -2.0     1.0
# [2,]  1.5     -0.5     
