## Put comments here that give an overall description of what your functions do

#the two functions below work together to find the inverse of matrix, 
#and cache the inverse value to reduce the computation effort if a repeated matrix has been set by the user.



## Write a short comment describing this function

#fist function is for caching the matrix by setting, getting matrix its self and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                     # a list (inv) with zero length is being defined
    set_matrix <- function(y){      # This function receives the matrix from the user. 
        x <<- y                     # the "<<-" operator used to assign the value of y to x in the global environment
        inv <<- NULL
    }
    
    # This function returns the matrix.
    get_matrix <- function() x 
    
    ## Here , the inverse of the matrix is being estimate, cached in the list (inv)
    set_inv <- function(inverse) inv <<- inverse  
    
    # This function returns the inverse matrix.
    get_inv <- function() inv        
    
    # this code define a list with all of the functions being used in the main function(makeCacheMatrix)
    list(set_matrix= set_matrix, get_matrix = get_matrix, set_inv = set_inv, get_inv = get_inv)  

}

## Write a short comment describing this function

## The 2nd function, is for finding the inverse of the function defined in above, 
#first it check inverse of the matrix is available or not,if not it calls solve() to calculate the inverse. 

cacheSolve <- function(x, ...) {
    inv <- x$get_inv()  # getting the inverse..
    # checking the inverse matrix in the cache...
    if(!is.null(inv)) {
        message("getting cached data !!")
        return(inv)
    }
    #if not available , the below code is for getting, caching the value...
    data <- x$get_matrix()
    inv <- solve(data)  #solve function is only works for square matrix (determinant != zero)
    x$set_inv(inv)
    inv
}
