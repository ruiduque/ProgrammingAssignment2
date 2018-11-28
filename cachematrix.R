## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function
## Programming Assignment 2: Lexical Scoping
##

makeCacheMatrix <- function(x = matrix()) {
  sMat <- NULL
 
  set <- function(v) {
          x <<- v
          sMat <<- NULL
  }
 
  get <- function() x
  setinverse <- function(invm) sMat <<- invm
  getinverse <- function() sMat
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## - When a cached inverse matrix is available...
        invm <- x$getinverse()
        if ( !is.null(invm) ) {
                message("Cached inverse matrix: ")
                return(invm)
        }

        ## - Calculate new inverse matrix and store it in the cache
        invmatrix <- x$get()
        if (det(invmatrix) != 0) 
                m <- solve(invmatrix)
        x$setinverse(m)
        m
}
