## Put comments here that give an overall description of what your
## functions do

## function that creates the "special" matrix that stores its inverse once it is computed
##
## exmple use:
#
# > m.good <- matrix(c(9,2,3,4,5,6,7,8,9),3,3)
# > mc.good <- makeCacheMatrix(m.gppd)
# > cacheSolve(mc.good) 
# > mc.good$get_inverse() --- return inverted matrixx
# > cacheSolve(mc.good)
# getting cached data   --- invertedd matrix is not recomputed
#
#
# > m.bad <- matrix(c(1,2,3,4,5,6,7,8,9),3,3)
# > det(m.bad)
# [1] 0    this matrix cannot be inverted
# > cacheSolve(mc.bad) 
# > cacheSolve(mc.bad)  ---- ERROR because matrix is singuar
# Error in solve.default(x.matrix) : 
#  Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
# > mc.bad$get_inverse()
# NULL

makeCacheMatrix <- function(x = matrix()) {
  # store for the inverse matrix
  inverse <- NULL
  # setting/getting functions
  set <- function(data) {
    matrix <<- data
    inverse <<- NULL
  }
  get <- function() matrix
  set_inverse <- function(matrix.inverse) inverse <<- matrix.inverse
  get_inverse <- function() inverse
  # return list of setters and getters
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Compute the inverse of a matrix 
## If cached version exists,  return the cached version
## For the computation exploits the solve() function in R
## if the matrix cannot be inverted it sends an error.  
## In this case the error codes are the same of solve()

cacheSolve <- function(x, ...) {
  x.inverse <- x$get_inverse()
  if(!is.null(x.inverse)) {
    message("getting cached data")
    return(x.inverse)
  }
  # compute inverse of x
  x.matrix <- x$get()
  x.inverse <- solve(x.matrix)
  x$set_inverse(x.inverse)
  ## Return a matrix that is the inverse of 'x'
  x.inverse
}
