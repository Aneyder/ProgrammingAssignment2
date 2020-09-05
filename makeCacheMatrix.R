## put comments her that give an overall description of what your 
## functions do

## There are two functions makeCacheMatrx, 
## makeCacheMatrix consists of set, get, setinv, getinv
## library(MASS) is used to  calculate inverse for non squared as well 
## ..as square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
    get <- function() {x}       ## function to get matrix x 
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() 
                     {inver<-ginv(x)
                      inver%*%x    ## function to obtain inverse of the matrix
    }
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){  ## gest cache data
      inv <- x$getInverse()
      if(!is.null(inv)){         ## Chcking whether inverse is NULL
         message("getting cached data!")
        return(inv)              ## returns inverse value
      }
      mat <- x$get()
      inv <- solve(mat, ...)      ## calculates inverse value
      x$setInverse(inv)
      inv
      
}
