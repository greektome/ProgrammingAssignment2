## makeCacheMatrix and cacheSolve functions:
##    create a "matrix" object that can cache its inverse,
##    calculate the inverse, and
##    store the inverse value which can then be retrieved from the cache
## The matrix is assumed to be both square and invertible.
## The functions were modified from the given makeVector and cachemean functions
##    given in the homework assignment example.

## makeCacheMatrix sets and then gets the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL     ## inverse matrix
  set<-function(y) {   ## sets value of matrix
    x<<-y
    i<<-NULL
  }  
  get<-function() {   ## gets value of matrix
    x  
  }  
  setinverse<-function(inverse) {   ## sets value of inverse
    i <<-inverse  
  }  
  getinverse<-function() {  ## gets value of inverse
    i 
  }  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve calculates the matrix inverse after first checking the cache
##    to see if the value has already been calculated and stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)) {   ## checks cache for inverse value
    message("Getting cache data")
    return(i)
  }
  data<-x$get()    ## calculates inverse if value not in cache
  i<-solve(data)
  x$setinverse(i)
  i
}
