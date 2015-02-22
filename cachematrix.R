###############################################################################
## This function returns a list of functions, allowing users to interactively 
## do the following:
##  * set a matrix
##  * get a matrix
##  * set the inverse of the matrix
##  * get the inverse of the matrix
###############################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    m <<- inverse
  }
  
  getinverse <- function() {
    m
  }
  
  list(  set = set
       , get = get
       , setinverse = setinverse
       , getinverse = getinverse
      )
  
}


###############################################################################
## This function checks if the inverse of the matrix has already been  
## calculated, and cached. If so, the cached inverse is retrieved. If not, the
## program calculates the inverse.
###############################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setinverse(m)
    
    m
}
