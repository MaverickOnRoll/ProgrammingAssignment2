## The function stores a cached version of a inverse of a square matrix 
## If the cached version exists, then the cached version is returned, else then 
## inverse is created and then stored in cache.

## The makeCacheMatrix function will take an inversible matix as input 
## and create a list to store the matrix, the inverse if available and can 
## set and get the inverse of the input matrix
## 

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    set_mat_inv <- function(mi) mat_inv <<- mi
    get_mat_inv <- function() mat_inv
    
    ## store the list in the object
    list(set = set, get = get,
         set_mat_inv = set_mat_inv,
         get_mat_inv = get_mat_inv)
}


## The cacheSolve function retrives the inverse of the matrix from cache, if it 
## exists. If it does not exist, then it creates the inverse of the matrix. 

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    mat_inv <- x$get_mat_inv()
    
    ## check if the cached version of the matrix exists, if yes, then return 
    ## from cache
    if(!is.null(mat_inv)) {
        message("getting cached data")
        ## return the inverse of matrix and exit the function
        return(mat_inv)
        
    }
    
    ## if control flows through, it means that the inverse does not exist
    ## and the inverse is created, and stored in teh cache
    data <- x$get()
    mat_inv <- solve(data, ...)
    x$set_mat_inv(mat_inv)
    mat_inv
}
