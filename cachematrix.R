
# makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.#

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve #set will set the matrix
  getmatrix<-function() m  #get will get the matrix
  list(set=set, get=get, 
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


# cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) { 
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}