## Two R functions were created to compute a matrix inverse and cache it, in order 
## to save CPU and program run time:
## 1- makeCacheMatrix creates a special "matrix" object that can cache its inverse
## 2- cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    setM <- function(y) { ## setM creates a matrix in the cache environment
        x <<- y
        m <<- NULL
    }
    getM <- function() x  ## getM retrieves a matrix from the cache environment
    setMinv <- function(inv) m <<- inv  ## setMinv puts an inverse matrix in the cache
    getMinv <- function() m             ## getMinv gets the inverse matrix from the cache
    list(setM = setM, getM = getM,      ## creates a list with the new function names
         setMinv = setMinv, getMinv = getMinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and wasn't changed) the cached inverse 
## matrix is returned, in order to save CPU time
  
cacheSolve <- function(x, ...) {
 
    m <- x$getMinv()
    if(!is.null(m)) { ## if the 'x' inverse matrix is found in cache, return it!
        message("getting cached data")
        return(m) 
    }
    dados <- x$getM() ## otherwise compute 'x' inverse matrix
    m <- solve(dados)
    x$setMinv(m)      ## put it in the cache
    m                 ## and return 'x' inverse matrix

}
