# Programming Assignment 2: Lexical Scoping
# Caching the Inverse of a Matrix

# Task description:
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The assignment is to write a pair of functions that cache the inverse of a matrix.

# The excessive use of comments is for the learning purpose only.



# 1) makeCacheMatrix 
# create a list containing four subfunctions
makeCacheMatrix<- function(x = matrix()) {
  
  inverse_of_matrix <- NULL
  
  # subfunction 1
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse_of_matrix <<- NULL
  }
  
  # subfunction 2
  # get the value of the matrix stored in x
  get <- function() {
    x
  }
  
  # subfunction 3
  # take the inverse passed into the function and store it in inverse_of_matrix (the cache)
  set_inverse_of_matrix <- function(inverse) {
    inverse_of_matrix <<- inverse
  }
  
  # subfunction 4
  # get the value of invese_of_matrix (the cache)
  get_inverse_of_matrix <- function() {
    inverse_of_matrix
  }
  
  # create list of subfunctions 1-4
  list(set = set, 
       get = get,
       set_inverse_of_matrix = set_inverse_of_matrix,
       get_inverse_of_matrix = get_inverse_of_matrix)
}



# 2) cacheSolve
# Retun the inverse of the matrix 
# condition: x = must be an invertible matrix

cacheSolve <- function(x, ...) {
  
  # get content of x's cache
  inverse_of_matrix <- x$get_inverse_of_matrix()
  
  # check if there is content in the cache 
  # if yes: show message and return content 
  # if no:  get matrix, invert matrix, save inverted matrix to the cache of x and return inverse of the matrix
  
  if(!is.null(inverse_of_matrix)) {
    message("getting the cached inverse of matrix:")
    return(inverse_of_matrix) 
  } else {
    data <- x$get()
    inverse_of_matrix <- solve(data, ...)
    x$set_inverse_of_matrix(inverse_of_matrix)
    inverse_of_matrix
  }
}



# # Sample code
# m <- makeCacheMatrix(matrix(c(4,3,3,2),2,2))
# m$get()
# # >       [,1] [,2]
# # > [1,]    4    3
# # > [2,]    3    2
# 
# # first run (nothing in cache)
# cacheSolve(m)
# # >       [,1] [,2]
# # > [1,]   -2    3
# # > [2,]    3   -4
# 
# # second run (return cache)
# cacheSolve(m)
# 
# # > getting the cached inverse of matrix:
# # >       [,1] [,2]
# # > [1,]   -2    3
# # > [2,]    3   -4

