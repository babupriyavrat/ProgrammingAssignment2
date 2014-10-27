#THe two functions have two different purposes.
#makeCacheMatrix(m ) -takes a matrix m and cache it. 
#cacheSolve(cachedm) - takes a cached matrix generated from function makeCacheMatrix
# and generate inverse of Matrix using inbuilt function of R, solve for calculating
# inverse of matrix
#
# Example:
##create a matrix
#a<-matrix(1:4,2,2)
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
##putting the matrix in cache
#cachea <-makeCacheMatrix(a)
##calculate the inverse of matrix 
# cacheSolve(cachea)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
##retrieving the inverse of matrix from cache
#cacheSolve(cachea)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
  
  #check if matrix is passed or something else to this function
  if(is.matrix( x ))
  {
  # in the beginning, initialize the stored inverse value to NULL
  invofx <- NULL
  
  # set value of the matrix x to y
  set <- function(y) {
    x <<- y
    invofx <<- NULL # matrix x has changed, thus reassigning invofx to NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set inverse of matrix
  setinverse <- function(inverse) invofx <<- inverse

  
  # get inverse of matrix
  getinverse <- function() invofx
  
  # return a list containing all functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }
 else
{
  message("This is not a matrix!")
}  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get inverse
  invofx <- x$getinverse()
  
  # if inverse of x exists, check if already cached
  # if yes, return cached inverse of x
  if(!is.null(invofx)) {
    message("getting cached data")
    return(invofx)
  }
  else
  
    { # if inverse is not cached, get matrix of x
      data <- x$get()
  
    # calculate inverse of matrix
      invofx <- solve(data,...)
  
    # set the inverse of matrix
    x$setinverse(invofx)
  
    # return inverse of marix
    invofx
    }
}
