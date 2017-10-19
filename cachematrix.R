##RProgramming Assignment by John Baker Hernandez

#Function number 1 (makeCacheMatrix)

## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, this is based upon the MakeVector() function that was used as an example for the assignment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#With the following: The Stepss below describe what is going on with the function, where this list that is being created shall be used as the input to cacheSolve()
#1. Set the value of the matrix
#2. Get the value of the matrix
#3. Set the value of the inverse
#4. Get the value of the inverse

#Function Number2 (cacheSolve)
## This function will return a matrix that is the inverse of 'x'## where x will consist of an output of makeCacheMatrix() and the return being an inverse of the original matrix input to makeCacheMatrix(). This function takes in any invertible matrix, and also sets the value of the inverse in the cache via the setinverse fnction, for which then cacheSolve will retrieve the inverse from the cache. Basically if you need to just invert a matrix, solve is the function you have to use in this case.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting Cached Invertible Matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  i$setinverse(i)
  i
}

#In order to test this, I created a matrix with matrix()function,and try to compue the inverse from the cache list and change the call matrix to the inverse and compute. 

B <- matrix(c(1:12), nrow = 4, ncol = 3, byrow = TRUE, dimnames = list(c("A", "B", "C", "D"),c("I", "II", "III")))
B


##*MakeCacheMatrix2... Below you can find a section Function that integrates the solve() into the overall function instead of having to write two seperate functions and can be used to express better the workability of the function(). There is also attached below how this can be used.

makeCacheMatrix2 <- function(x = matrix()) {
  inv <- NULL
  set2 <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get2 <- function() x
  setInverse2 <- function() inv <<- solve(x) #calculate the inverse
  getInverse2 <- function() inv
  list(set2 = set2,
       get2 = get2,
       setInverse2 = setInverse2,
       getInverse2 = getInverse2)
}
funs <- makeCacheMatrix2()
funs$set2(matrix(1:4, 2))
funs$get2()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
funs$setInverse2()
funs$getInverse2()
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5