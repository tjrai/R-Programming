CacheMatrix : This function creates a special matrix object that can cache its inverse.
## cacheSolve:       This function computes the inverse of the special matrix returned by 
##                   'makeCacheMatrix'.If the inverse has already been calculated (and the 
##                   matrix has not changed), then 'cacheSolve' will retrieve the inverse 
##                   from the cache.

## The first function `makeCacheMatrix` creates a special 'matrix', which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

# Creates a special matrix 'makeCacheMatrix'
makeCacheMatrix <- function(x=matrix()){
    i <- NULL
    set <- function(y=matrix()){ 
      x <<- y
      i <<- NULL
     }
    get <- function() {x}
    setinverse <- function(solve) {i <<- solve}
    getinverse <- function() {i}
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
   }

## The second function `cacheSolve` calculates the inverse of the 
## special 'matrix' created with the above function.
## The function first checks to see if the inverse has already been
## calculated and the matrix has not changed.
## If so, it gets the inverse from the cache and 
## skips the computation.Otherwise, it calculates the inverse of the data 
## and sets the value of inverse in the cache via `setinverse` function


cacheSolve <- function(x, ...) {
    # returns a matrix that is inverse of 'x'   
    i2 <- x$getinverse()     
    # checks if the inverse is computed already   
    if(!is.null(i2)) {     
      # displays the message if inverse already computed
      message("getting cached data")
      # returns the already computed value of inverse from the cache
      return(i2)           
     }
    # If the cache is empty,assign the matrix value in variable 'data'
    data <- x$get()        
    # compute the inverse of the matrix 
    i2 <- solve(data, ...) 
    # assigns the result of inverse to cache
    x$setinverse(i2)       
    # return the result of inverse
    i2
    }


