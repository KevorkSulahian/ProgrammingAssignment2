## two function
# First: a function to cache the inverse of the matrix
# Second: if the matrix's inverse is not computed then computes it, if it is then then just return it

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(u) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)

}

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse
  if(!is.null(m)) {
    message("getting the cached data")
    return(m)
  }
  data <- x$get
  #solve?
  m <- solve(data) %*% data
  x$setInverse
  m
}
