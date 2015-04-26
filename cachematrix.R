## The following functions are to be used for the calculation of the inverse of a matrix.  
## In advance, the inverse is saved to the cache in order to avoid the repetition of the 
## matrix inverse calculation; when the inverse is called the saved value is returned instead of
## repeating the calculation.

## The function 'makeCacheMatrix' creates a special "matrix" object; a list that contains a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## create a matrix object x and the respective sub-functions/methods
        
        ## define the cache m
        m <- NULL
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x
                m <<- NULL ## re-initialize m in the parent environment to null
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) m <<- inverse ## set the cache m equal to matrix x inverse
        getinverse <- function() m ## return the cached inverse of x
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function 'cacheSolve' calculates the inverse of the special "matrix" created with 'makeCacheMatrix'.
## Before the calculation it checks if the inverse has already been caclulated. 
## If so, 'cacheSolve' gets the inverse from the cache and skips the computation. 
## Else, it calculates the matrix inverse
## and sets the value of the inverse in the cache through the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
