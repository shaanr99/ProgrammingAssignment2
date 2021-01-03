## Title:   Programming Assignment #2
## Author:  Shaanr
## Date:    1/3/2021

## This function will serve as the caching mechanism.
## It will retrieve a cached version of an object (get) 
## and it will cache an object (set)

makeCacheMatrix <- function(x = matrix()) {
  # initialize cached object to NULL
  m <- NULL
  
  # Setter for cached object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Getter for cached object
  get <- function() x
  
  # Cache the object
  setInv <- function(inv) m <<- inv
  
  # Retrieve the object
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
##
## This function performs the actual solve (inverse)
## operation, but first it checks to see if there
## is already a cached version.  If there is, it uses
## the cached version rather than find the inverse again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # check to see if the object has already been cached
  m <- x$getInv()
  
  # If already cached, use the cached version
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise, cache it
  data <- x$get()
  
  # Solve it
  m <- solve(data, ...)
  
  # Cache it
  x$setInv(m)
  
  # Return it
  m
}


