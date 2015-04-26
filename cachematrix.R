## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        # set m to NULL as placeholder
        m <- NULL
        
        # function that sets matrix 'x' to new matrix 'y'; resets 'm' to NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        # returns matrix 'x'
        get <- function() x
        
        # sets matrix 'm' to cacheSolve function
        setmatrix <- function(solve) m<<- solve
        
        # returns matrix 'm'
        getmatrix <- function() m
        
        # returns vector of functions defined above
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # returns inverse matrix of 'x'
        m <- x$getmatrix()
        
        # checks to see if matrix exists
        if(!is.null(m)){
                message("getting cached matrix")
                return(m)
        }
        
        # returns matrix
        matrix <- x$get() 
        
        # solves inverse of matrix and assigns to 'm'
        m <- solve(matrix, ...)
        
        # sets matrix to object 'x'
        x$setmatrix(m)
        
        # returns matrix 'm'
        m
}
