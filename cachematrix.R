## makeCacheMatrix creates a special "vector", which contains 4 functions :
## 1- set : is a function that changes the vector x stored in the parent envirement (main function).
## 2- get : get the value of the vector x stored in the main function
## 3- setinverse : Store the value of the input in a variable m (inverse) stored in the parent envirement (main function).
## 4- getinverse : returns t he value of the variable m stored in the parent envirement (main function)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}
