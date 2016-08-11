#<<- assign a value to an object in an environment that is different from the current environment
#using MakeVector and cachemean as the basis
#https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
#
#below you can see an example of how to set and solve the cache
#> matrix(c(1,10,2,11),nrow=2,ncol=2,byrow=TRUE)
#[,1] [,2]
#[1,]    1   10
#[2,]    2   11
#
#> matrixinput <- makeCacheMatrix(matrix(c(1,10,2,11),nrow=2,ncol=2,byrow=TRUE))
#> cacheSolve(matrixinput)
#[,1]       [,2]
#[1,] -1.2222222  1.1111111
#[2,]  0.2222222 -0.1111111

makeCacheMatrix <- function(input = matrix()){
  s <- NULL
  set <- function(y){
    input <<- y
    s <<- NULL
  }
  get <- function() input
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(input, ...){
  s <- input$getinv()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- input$get()
  s <- solve(data, ...)
  input$setinv(s)
  s
}
