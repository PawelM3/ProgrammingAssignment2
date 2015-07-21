## Creting the inverse of a matrix may be time consuming, especially in case of a large matrix
## The following functions calculate the inverse of a matrix and store it for later use
## Usage:
## Let myMatrix is the matrix you want to inverse
## To store the original matrix:
## myStoredMatrix <- makeCacheMatrix(myMatrix)
## To retrieve stored matrix:
## myStoredMatrix$get()
## To calculate the inverse of myStoredMatrix:
## cacheSolve(myStoredMatrix)
## If you again issue cacheSolve with the same matrix, the solved, cached matrix
## will be retrieved from memory with the comment: "Getting cached data..."
## To retrieve the inverse of myStoredMatrix:
## myStoredMatrix$getinverse()

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  myCachedMatrix <- NULL  ## myCachedMatrix is empty
  set <- function(y) {    ## define set function - we use it when we calculate new matrix
      x <<- y             ## permanently substitues x with y - new matrix in the whole function
      myCachedMatrix <<- NULL ## since new matrix is given, old cache should be emptied
  }
  get <- function() x ## retrieves the original matrix
  setinverse <- function(solve) myCachedMatrix <<- solve ## stores the inverted matrix
  getinverse <- function() myCachedMatrix                ## retrieves the inverted matrix
  list(set = set, get = get,                             ## stores four functions within makeCacheMatrix
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This functions calculates the inverse of a matrix and stores it for further use

cacheSolve <- function(x, ...) {
  myCachedMatrix <- x$getinverse()
  if(!is.null(myCachedMatrix)) {      ## checking, whether the inverse has been already calculated
    message("Getting cached data...") ## if it is so, then the cached version
    return(myCachedMatrix)            ## is returned thanks to this line
  }                                   ## if it is not the case
  dataMatrix <- x$get()               ## retrieve the new matrix
  myCachedMatrix <- solve(dataMatrix, ...) ## calculate the inverse of the new matrix
  x$setinverse(myCachedMatrix)        ## safely storing the inverse of the new matrix for further use
  myCachedMatrix                      ## presenting the inverse of the matrix
}
