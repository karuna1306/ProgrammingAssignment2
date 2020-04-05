## makeCacheMatrix: This function creates a special "matrix" object that
##can cache its invrerse.


makeCacheMatrix <- function(Inputmatrix = matrix()) {
  invrInputmatrix <- NULL
  set <- function(x) {
    Inputmatrix <<- x       ## Inputmatrix: Matrix object submitted in console
    invrInputmatrix <<- NULL
  }
  get <- function() Inputmatrix
  setinvrerse <- function(invrerse) invr <<- invrerse
  getinvrerse <- function() invrInputmatrix
  list(set = set,
       get = get,
       setinvrerse = setinvrerse,
       getinvrerse = getinvrerse)
}



## This function computes the invrerse of the special "matrix" created by makeCacheMatrix above. If the invrerse has already been calculated (and the 
## matrix has not changed), then it should retrieve the invrerse from the cache.

cacheSolve <- function(Inputmatrix, ...) {
  ##  Below function will return a matrix that is the invrerse of 'Inputmatrix'(input matrix)
  invr <- Inputmatrix$getinvrerse()
  if (!is.null(invr)) {
    message("getting cached data")
    return(invrInputmatrix)
  }
  ## If inverse is not present in cache then below function will calculate Inverse of input matrix
  mat <- Inputmatrix$get()           
  invrInputmatrix <- solve(mat, ...)
  Inputmatrix$setinvrerse(invrInputmatrix)
  invrInputmatrix
}
