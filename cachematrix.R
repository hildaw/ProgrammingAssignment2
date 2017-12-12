## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
}  ##set the matrix
        
        get <- function() x  ##get the matrix
        
        setinverse <- function(inverse) invs <<-inverse
        ## set inverse of the matrix
        
        getinverse <- function() invs 
        ##get the inverse of the matrix
        
        list(set = set,get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        invs <- x$getinverse()
                if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }  ## if there If the inverse has already been calculated
        ## (and the matrix has not changed), 
        ## then the cacheSolve should retrieve the inverse from the cache
       
        data <- x$get()  ##  if not, get the matrix
        invs <- solve(data, ...)   ## calculate the inverse of the matrix
        x$setinverse(invs) ##get inverse of matrix
        invs
        
        ## Return a matrix that is the inverse of 'x'
}

source("ProgrammingAssignment2/cachematrix.R")
getwd()
setwd(file.path("E:","Data Science","course 2 R","Assignment 2","ProgrammingAssignment2-master"))
getwd()
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
my_matrix$getinverse()
