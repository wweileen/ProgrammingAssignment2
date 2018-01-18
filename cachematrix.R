## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){                            ##set the value
    x<<-y
    m<<-NULL
  }
  get<-function()x                             ##get the value
  setsolve<-function(slove)m<<-solve           ##set the value of matrix inversion
  getsolve<-function()m                        ##get the value of matrix inversion
  list(set=set,
       get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  if(!is.null(m)){                            ##check if the value has already been calculated
    message("getting cached data")            ##already calculated and skip the process
    return(m)
  }
  data<-x$get()                               ##not already calculated start to calculate
  m<-solve(x)                                 
  x$setsolve(m)                               ##get the result
  m   ## Return a matrix that is the inverse of 'x'
}
