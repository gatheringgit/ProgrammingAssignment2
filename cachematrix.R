##This function contains a list of functions and creates a special "matrix" object
##that can cache its inverse. It makes use of the solve function that will return
##the inverse of an invertible square matrix. Caching the value can save on future
##computing time. If the matrix input changes, the cached value goes back to NULL.

makeCacheMatrix <- function(x = matrix()) {
##initial value of solv is NULL
        solv <- NULL
##set changes the vector stored in this main function, and solv goes back
##to NULL
        set <- function(y){
                x <<- y
                solv <<- NULL
        }
        ## get returns the vector x stored in the main function
        get <- function() x

        ##setsolve sets the value of solv to output of solve function
        setsolve <- function(solve) solv <<- solve

        ##getsolve gets the saved value for solv 
        getsolve <- function() solv

        ##below is the list of the 4 functions included in makeCacheMatrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



##cacheSolve consults the stored value of solv for the inverse of...in this case
##...an invertible square matrix and returns that, unless the matrix has changed, in
## which case the inverse is solved for again and the new value of solv is cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##solv gets the cached value for solve and the 'if loop' verifies the
        ##value is not null.Given that it's not null, it returns
        ##that cached value

        solv <- x$getsolve()
        if(!is.null(solv)){
                message("getting cached inverse")
                return(solv)
        }
 
        ##else when the value is null, it gets the data saved for the matrix, 
        ##and performs the solve function on the data, and sets that solve value
        ##as the new cached inverse

        data <- x$get()
        solv <- solve(data)
        x$setsolve(solv)
        solv
}
