## The porpuse of these two functions makeCacheMatrix and cacheSolve is to
## illustrate how lexical scoping works, and how can it be used to save time
## performing calcultations which may be done already in previous stages.

## makeCacheMatrix is a function that takes an square and invertible matrix x
## and stablish an specific environment where some functions like set, get, 
## setinv and getinv are defined. Then this functions are organized in a list.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(solve) inv<<-solve
  getinv<-function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve is a function that uses as input the resulting list of 
## makeCacheMatrix. If cacheSolve is run for the first time sing a list asociated
## to a matrix it will use that list to calculate the value of the inverted matrix
## first stablishing the matrix as the variable data (from x$get() of the list) 
## and then performing the calculation (using solve(data)). As it, is clear this
## value will be stored in the list (x$setinv(inv)), so the solution will exist
## in the enviroment created in makeCacheMatrix, and will be available any time
## cacheSolve calls the same list asociated with this matrix in which case it
## will display "getting cached data" while retrieving the stored value.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
