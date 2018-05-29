## Put comments here that give an overall description of what your
## functions do

## make matrix design to variable x, and initiate m to NULL

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  ## if user wants to reset matrix
    x <<- y             ## reassign "new" matrux to x
    m <<- NULL          ## reinitialize m to NULL
  }
  get <- function()x
  SetInvmatrix <- function(InvMatrix) m <<0 InvMatrix
  getInvmatrix <-function() m
  list( set = set, get = get,
        SetInvmatrix = SetInvmatrix
        getInvmatrix = getInvmatrix)

}





        ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getInvmatrix()              
  if(!is.null(m)) {           ## if user had calculated the same matrix before
    message("getting cached data")  
    return(m)               ## return old result(m) directly 
  }
  data <- x$get()             ## otherwise, get the uncalculated matrix
  m <- solve(data, ...)       ## calculate the inverse matrix
  x$setInvmatrix(m)           ## reassign inverse matrix 
  m                           ## print the inverse matrix 
}

