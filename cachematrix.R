## makeCacheMatrix is a function that returns a list of functions  
## that apply to an matrix in the defined environment
## the list of available functions are:
## set() alter/sets the matrix
## get() returns the matrix
## setinverse() set/alter the inverse of the matrix 
## getinverse() return the matrix set as the inverse of the matrix (NULL if not set yet)


makeCacheMatrix <- function(x = matrix()) {
	  inverse <- NULL
	  	
        set <- function(y) {
                x <<- y
                inverse <<- NULL
	  }
        get <- function() x
        setinverse <- function(inv)	inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that receives x (list returned from makeCacheMatrix) and returns
## the inverse of the matrix. If the inverse is already set, it just returns its value. Otherwise, it 
## computes the inverse and sets the inverse in the list before returning it  

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
      if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
