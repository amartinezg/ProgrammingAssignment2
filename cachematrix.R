## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  get <- function() x
  get_inverse <- function() inverse

  set <- function(new_matrix){
    x <<- new_matrix
    inverse <<- NULL
  }
  set_inverse <- function(matrix_inverted){
    inverse <<- matrix_inverted
  }

  list(set = set,
    get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }

  inverse <- solve(x$get())
  x$set_inverse(inverse)
  inverse
}

### EXAMPLE

# > x <- matrix(rnorm(9), 3, 3)
# > x
#            [,1]       [,2]       [,3]
# [1,]  0.5230134  1.2164793 -2.3803949
# [2,]  0.7989519 -1.1094451  0.5203308
# [3,] -0.9880864  0.2375257 -0.5959359
# > m = makeCacheMatrix(x)
# > cacheSolve(m)
#            [,1]       [,2]       [,3]
# [1,]  0.2246748  0.0666789 -0.8392170
# [2,] -0.0158852 -1.1132958 -0.9086027
# [3,] -0.3788515 -0.5542892 -0.6487235
# > cacheSolve(m)
# getting cached data
#            [,1]       [,2]       [,3]
# [1,]  0.2246748  0.0666789 -0.8392170
# [2,] -0.0158852 -1.1132958 -0.9086027
# [3,] -0.3788515 -0.5542892 -0.6487235

