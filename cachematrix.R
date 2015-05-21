## These functions cache the results of operations that may take a long time to compute. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Specifically, I use solve() to find the inverse matrix and then cache that result.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##            If the inverse has already been calculated (and the matrix has not changed),
##              then cacheSolve should retrieve the inverse from the cache.
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
        ## Return a matrix that is the inverse of 'x'
x <- matrix(seq(2,8,2),2,2) #create a matrix 'x'
x
z <- makeCacheMatrix(x) #create the special matrix
cacheSolve(z) #return the inverse matrix

