## Create a cacheMatrix to reduce time calculation of
##matrix invertion

## create a list to store an invert matrix on Global environment 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  ## create the list with the previous calculated elements
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}

## calulate invert of matrix and check if its invert already
##existe in the list created by makeCacheMatrix() function

cacheSolve <- function(x=matrix(), ...) {
      
  m <- x$getmatrix()
  ## check if the invert already exist
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if it doesn't exist yet, calculate it
  matrixdata <- x$get()
  m <- solve(matrixdata, ...)
  x$setmatrix(m)
  m
}

##Thank you for reading :)
