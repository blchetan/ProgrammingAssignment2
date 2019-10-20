## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      # set the value of the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      # get the value of the matrix
      get <- function() x
      # set the value of the inverse
      setinverse <- function(inverse) inv <<- inverse
      # get the value of the inverse
      getinverse <- function() inv
      # return:
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse() # function() inv
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv) # return ends the function
      }
      data <- x$get() # function() x
      inv <- solve(data, ...) # calculates the inverse and stores it as inv
      x$setinverse(inv) # function(inverse) inv <<- inverse
      inv        
}
