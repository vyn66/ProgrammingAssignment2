## WE want to creat a pair of functions that cache the inverse of a matrix. We are assuming the matrix presented-
## - is always invertible.

## This function creates a special matrix object than can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                x <<- y
                m <<- NULL
                
            }
            
            get <- function(){
                x
            } 
            
            setinverse <- function(inverse) {
                m <<- inverse
            }
            
            getinverse <- function() {
                m
            }
            
            list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
            

}


## this function computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    if(!is.null(m)){
        message("Getting cached data")
        return(m)
        
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
        
}

##########Test
test_matrix <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(test_matrix)
cacheSolve(test_matrix)
