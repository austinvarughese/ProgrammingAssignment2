## makeCacheMatrix takes input in the form of a 
## matrix and solves for its inverse, which is stored onto a variable
##cacheSolve commits the solved inverse of the matrix to memory;
##so that the value could be pulled upon requirement later.

## makeCacheMatrix finds the inverse of a matrix,
## x is assigned to the variable y in the parent env,
## x is retrieved from the GlobalEnv by get, since its not defined within it
## setMatrix solves for the matrix, since no matrix is defined it looks
## within the local envrionment, using x
## getMatrix retrieves the inverse matrix

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
## it checks if "a" already has a value in the parent environment
## then replaces overwrites whatever value is stored in it,
## with what is calculated by setMatrix in makeCacheMatrix

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
