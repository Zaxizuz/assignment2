## This function can compute the inverse of the square matrix 
## created by function makeCasheMatrix.If you operated the 
## cacheSolve before, the function can show the answer without  
## calculating it again.

## Input a matrix you want to get its invese.

makeCacheMatrix <- function(x = matrix()) {
        
        inv =NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<-solve
        getinverse <- function() inv
        list(set=set, get = get, setinverse = setinverse,
             getinverse=getinverse)
        
}


## Calculate the inverse of the matrix created by function 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
 
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv

}
