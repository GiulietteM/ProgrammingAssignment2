## Functions "cache" the inverse of a  given matrix
## Create a special "matrix", that contain:
#1. A set the value of the matrix given
#2. Get the values of the matrix given 
#3. A Set the value of the inverse matrix with the step previous
#4. Values of the inverse matrix given with te function

make_matrix <- function(x = matrix()) {
  g <- NULL
  set <- function(y) {
    x <<- y
    g <<- NULL
  }
  get <- function() x
  setinverse <- function(invs) g <<- invs
  getinverse <- function() g
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Function fot calculate the inverse of the  "matrix" created with the previous
## function "make_matrix" , reusing cached result if it is available

cacheSolve_Inverse <- function(x, ...) {
  iv <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(iv)
  }
  m <- x$get()
  iv <- solve(m, ...)
  x$setinverse(iv)
  iv
}

#In this part it is use the functions previous for get the matrix
#inverse.
example <- make_matrix(matrix(c(6, 5, 5, 6), c(2, 2)))
cacheSolve_Inverse(example)

## Use in the  example:
##example <- make_matrix(matrix(c(6, 5, 5, 6), c(2, 2)))
##cacheSolve_Inverse(example)
## [,1] [,2]
##[1,]  0.5454545 -0.4545455
##[2,] -0.4545455  0.5454545

