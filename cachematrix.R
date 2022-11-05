rm(list=ls())

## This function returns a matrix to cache its inverse
makeCacheMatrix <- function(matrix) {
  aux <- NULL 
  fix <- function(y) {
    matrix <<- y
    aux <<- NULL}
  call <- function() {matrix}
  set_inverse <- function(inverse) {aux <<- inverse}
  get_inverse <- function() {aux}
  list(fix = fix, call = call, set_inverse = set_inverse, get_inverse = get_inverse)}


## This function uses the previous function returns to calculates the 
##inverse of the matrix is its exists otherwise it returns 
##"Matrix inverse result is not allowed:" 
cacheSolve <- function(matrix_MCM, ...) {
  matrix_result <- matrix_MCM$get_inverse()
  if (!is.null(matrix_result)) {
    message("Matrix inverse result is not allowed:")
    return(matrix_result)}
  results <- matrix_MCM$call()
  matrix_result <- solve(results, ...)
  matrix_MCM$set_inverse(matrix_result)
  message('The matrix inversion is:')
  print(matrix_result)} 
