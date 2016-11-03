## Caching the Inverse of a Matrix 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<-function(x=matrix()){
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function()x
  setinverse<-function(inv)inverse<<-inverse
  getinverse<-function()inverse
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve<-function(x,...){
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  ## solve (x): Computing the inverse of a square matrix can be done with the solve function.
  data<-x$get() ##get
  inverse<-solve(data,...) ##solve
  x$setinverse(inverse) ##set
  inverse ##return
}
