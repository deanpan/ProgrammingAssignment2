## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix Function stores the original matrix and its inverse matrix
##
## cacheSolve Function takes the object created by makeCacheMatrix function and returns the inverse matrix
##
## Write a short comment describing this function
## makeCacheMatrix Function stores the original matrix and its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## set inverse matrix cached to NULL
  set <- function(y) { 
    x <<- y   ## store the matrix
    m <<- NULL  ## set the inverse matrix to NULL
  }
  get <- function() x   ## get matrix stored
  setinverse <- function(inverse) m <<- inverse  ## store inverse matrix
  getinverse <- function() m  ## get inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve Function takes the object created by makeCacheMatrix function and returns the inverse matrix
## It first gets the inverse matrix stored and check if it is a NULL.
## If the stored inverse matrix is not NULL, it returns the inverse matrix stored; otherwise,
## it retrives the matrix and compute its inverse matrix, and stores the result into the object and returns the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()   ## retrive the inverse matrix stored
  if(!is.null(m)) {     ## check if it is not a NULL
    message("getting cached inverse Matrix") ## message indicating cache hit
    return(m)  ## return the result from the cache.
  }
  ## cache missing
  data <- x$get() ## retrive the matrix stored 
  m <- solve(data, ...) ## computes the inverse matrix
  x$setinverse(m)  ## cache the inverse matrix
  m  ## return the inverse matrix computed
}

