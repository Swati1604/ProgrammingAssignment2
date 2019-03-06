## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## The following two functions are used to cache the inverse of a matrix.
## The first function makeCacheMatrix initializes the inverse. A list is created with the matrix and the inverse
## The second function cacheSolve calculates the inverse of the matrix if it hasn;t been computed, else returns the cached inverse of the matrix


## ----------------------------------------------------------------------------##
## makeCacheMatrix creates a list containing a function for the following:
# -> set the value of the matrix
# -> get the value of the matrix
# -> set the value of inverse of the matrix
# -> get the value of inverse of the matrix
##---------------------------------------------------------------------------------------------------------##

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  ## setter and getter for the matrix object
  
  ## set() is used to assign the input argument to the x object in the parent environment.
  ## set() also assigns the value of NULL to the 'inv' object in the parent environment; it also clears any value of inv if it had been cached by a prior execution of cacheSolve().
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x
  
  ##setter and getter for the inverse matrix object
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  ## each of these functions assigned as an element within a list(); returned to the parent environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##---------------------------------------------------------------------------------------------------------##
## CacheSolve() returns the inverse of the matrix.
## It first checks if the inverse has already been computed. If yes, then the function gets the result and does not perform any computation.
## However, if the inverse hasn't been calculated, it will computes the inverse and then set the value in the cache via setInverse function.
## Also this function assumes that the inverse of the matrix exists.
##---------------------------------------------------------------------------------------------------------##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}


##PROGRAM OUTPUT (EXAMPLE)
## test_matrix <- makeCacheMatrix(matrix(1:9, 3, 3))
## test_matrix$get()
##       [,1] [,2] [,3]
##  [1,]    1    2    3
##  [2,]    0    1    5
##  [3,]    5    6    0
## test_matrix$getInverse()
## NULL
## cacheSolve(test_matrix)
##      [,1] [,2] [,3]
## [1,]   -6  3.6  1.4
## [2,]    5 -3.0 -1.0
## [3,]   -1  0.8  0.2
## cachSolve(test_matrix)
## getting cached data
## [,1] [,2] [,3]
## [1,]   -6  3.6  1.4
## [2,]    5 -3.0 -1.0
## [3,]   -1  0.8  0.2