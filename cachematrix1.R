## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
  m<-NULL
  set<-function(y){             ##set the vlaue
    x<<-y
    m<<-NULL
}
  get<-function()x              ##get the value
  setinverse<-function(inverse)m<<-inverse  ##set the inverse matrix
  getinverse<-function()m                   ##get the inverse matrix
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m<-x$getinverse()                  ##check if the inverse matrix has been calculated
  if(!is.null(m)){                   ##if it did, skip the calculation and get the value
    message("getting cached data")
    return(m)
  }
  data<-x$get()                      ##otherwise calculate it and get the inverse of 'x'
  m<-solve(data,...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
