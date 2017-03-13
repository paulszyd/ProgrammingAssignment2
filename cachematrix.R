## makeCacheMatrix 
## Provided x, a square invertible matrix (eg, x<-matrix(rnorm(100),10,10))
## makeCacheMatrix will create four functions to:
##      1. set the matrix in parent environment
##      2. get the matrix from the parent environment
##      3. set the inverse of the matrix in the parent environment
##      4. get the inverse of the matrix from the parent environment
## Create a list with the function names that is available as input for cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        myMat <- NULL
        set <- function(y) {
                x <<- y
                myMat <<- NULL               
        }
        get <- function() x
        setinverse <- function(solve) myMat <<- solve
        getinverse <- function() myMat
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)        
}


## Write a short comment describing this function
## cacheSolve takes the vector created by makeCacheMatrix as an argument, then:
##      1. Uses the getinverse function from makeCacheMatrix to assign the cached inverse
##      2. Checks to see if the inverse was assigned (if(!is.null))
##      3. Reurns cached inverse if it exists
##      4. Otherwise gets original matrix data, inverts (solves) it, then returns it.
##      5. Informs user if cached or calculated inverse is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myMat <- x$getinverse()
        if(!is.null(myMat)) {
                message("getting cached data.")
                return(myMat)
        }
        data <- x$get()
        myMat <- solve(data)
        x$setinverse(myMat)
        message("inverse not cached, calculating inverse")
        myMat
}