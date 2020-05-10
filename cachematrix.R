
################################################################################
# Programming Assignment 2: Lexical Scoping 
# Janardhana Rao V
################################################################################
## In this code we will have two functions
#   1. makeCacheMatrix 
#   2. cacheSolve
#   First function basically holds the data but will not do any calculations
#   second function will access the list object, which will come out of first function and performs calculation (here inverse), 
#      if already not calculated and saved inside object with latest data.
################################################################################
# We will define the code and then test them. You can see comments at each testing line.
################################################################################
makeCacheMatrix <- function(x = matrix()){
  m      <- NULL
  set    <- function(y) {  
            x <<- y 
            m <<- NULL }
  get    <- function() x
  setinv <- function(invs) m <<- invs
  getinv <- function() m
  
  list(set = set, get = get,     setinv = setinv,     getinv = getinv)
  
}


cacheSolve <- function(x,...){
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  print("printed the calculated inv")
  x$setinv(m)
  m
}

################################################################################

#testing start
#create a matrix to pass into first function to cache, here fixing the random data with a seed
set.seed(11052020)  
mat1<-matrix(rnorm(16),nrow=4,ncol=4)
mat1
solve(mat1)  # to test wheter matrix is invertable


catch1       <-  makeCacheMatrix(mat1)  # "catch1" is the object that catches the return data from makeCacheMatrix
catch1$get()            # testing makeCacheMatrix.get    function of cache
catch1$getinv()         # testing makeCacheMatrix.getinv function of cache

#calling CACHESOLVE to check for inverse and derive as needed via output object of MAKECACHEMATRIX
cacheSolve(catch1)      # testing cachesolve             function of cache, if inverse not run earlier or inverse in null then it should re-calculate inverse.  Expectation is it should derive
cacheSolve(catch1)      # testing by running again, to see whether it is fetching from cache or re-calculating it. Expectation is data from cache.

#create a new matrix to check makeCacheMatrix.setinv is working or not
mat2<-matrix(c(1,4,6,9,9,22,4,7,2,11,19,6,4,9,8,20) ,nrow=4,ncol=4 )
mat2
solve(mat2)             # ensuring the newly created matrix has inverse derivable

catch1$set(mat2)
catch1$get()
catch1$getinv()         #Since data is updated just now, inverse should be null when run beofre coling CACHESOLVE function
cacheSolve(catch1) 
cacheSolve(catch1) 
catch1$getinv()

# no need to test setinv function, as it will be called as part of CACHESOLVE if inverse is blank and re-calculated
################################################################################



