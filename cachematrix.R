##
## Programming Assignment 2: Lexical Scoping
## makeCacheMatrix <- function(x = matrix())
## This function creates a special "matrix" object that can cache its inverse.
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


##
## Programming Assignment 2: Lexical Scoping
## cacheSolve <- function(x, ...)
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.
##
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
