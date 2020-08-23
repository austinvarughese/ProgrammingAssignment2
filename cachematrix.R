## makeCacheMatrix takes input in the form of a 
## matrix and solves for its inverse, which is stored onto a variable
##cacheSolve commits the solved inverse of the matrix to memory;
##so that the value could be pulled upon requirement later.

## makeCacheMatrix finds the inverse of a matrix,

makeCacheMatrix <- function(x = matrix()) 
{
  a<-NULL
  set<-function(y)
  {
    x<<-y
    a<<-NULL
  }
  get<-function()x
  setMatrix<-function(solve){m<<-solve}
  getMatrix<-function()m
  list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## cacheSolve; caches the inverse matrix 

cacheSolve <- function(x, ...) {
        a<x$getMatrix()
  if(!is.null(a))
  {
    message("getting cached data")
    return("m")
  }
  Final<-x$get()
  a<-solve(Final,...)
  x$setMatrix(a)
  a
}
