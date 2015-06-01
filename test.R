# The makeCacheMatrix performs 4 distinct operations-
# Sets the value for a matrix
# Gets the value of a matrix
# Sets the inverse for a matrix
# Gets the inverse of a matrix
# The function retuns a list of the 4 functions mentioned above
# ------------------------------ #
makeCacheMatrix <- function(x = matrix()) 
  {
  mtrx<-NULL
  set<-function(y)
  {
    x<<-y
    mtrx<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) mtrx<<- solve
  getinverse<-function() mtrx
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
#--------------------------------#
# cacheSolve inverts an input matrix if the inverse is not already cached
# when computing the inverse for the first time, the function relays the appropriate message
# if the inverse is already cached, it is fetched from cache, and again a relevant message is displayed 
cacheSolve <- function(x=matrix(), ...) {
  mtrx_inverse<-x$getinverse()
# check if the inverse already exists
# if it does, fetch it from cache
  if(!is.null(mtrx_inverse)){
    message("fetching inverse from cache")
    return(mtrx_inverse)
  }
  matrix<-x$get()
# if the inverse does not exist, compute it using solve
# and then set the inverse
  mtrx_inverse<-solve(matrix, ...)
  x$setinverse(mtrx_inverse)
  message("computing and setting inverse")
  mtrx_inverse
}
#--------------------------------#
# An example to test out the functions
# mtrx<- matrix(c(2, 4, 3, 1, 5, 7,12,29,31),nrow=3,ncol=3)
# mtrx_1<-makeCacheMatrix(mtrx)
# cacheSolve(mtrx_1)
# cacheSolve(mtrx_1)
