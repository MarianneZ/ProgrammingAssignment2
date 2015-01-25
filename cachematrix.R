## Functions "makeCacheMatrix" and "cacheSolve" are caching the inverse of a matrix

## Function "makeCacheMatrix" creates matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL  
  
  set <- function(y) { ## creates the function which sets the function for storing the value of the matrix
    x <<- y  
    s <<- NULL
  }
  
  get <- function() x ## creates the function which gets the matrix
  setinverse <- function(solve) s <<- solve ## creates the function which sets the value of 's'
  getinverse <- function() s ## creates the function which gets the value of 's'
  ## creates a list to store the four functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## Function "cacheSolve" computes the inverse of the matrix returned by 'makeCacheMatrix' above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## receives 'x' from 'makeCacheMatrix' above
  
  s <- x$getinverse() ## gets the value of 's' from 'makeCacheMatrix' above
  if(!is.null(s)) { ## Checks if  's' is NULL
    message("getting cached data") ## if 's' is not NULL returns the message
    return(s) ## and returs the value 's'
  }
  
  data <- x$get() ## gets the matrix from 'makeCacheMatrix' above and assigns to 'data'
  s <- solve(data) ## makes invertion of 'data' and assigns to 's'
  x$setinverse(s) ## sets 's' in the cache
  s ## Return a matrix that is the inverse of 'x'   
}
