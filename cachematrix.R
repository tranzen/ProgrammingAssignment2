## A set of two functions to compute the inverse of a matrix only when it has 
## not been computed before. The first time it is computed, it is stored 
## in memory

## makeCacheMatrix returns a special "matrix" that is able to store its inverse
## It is a list with four function objects. Two functions are devoted to get and
## set the matrix. Two more functions to set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y = matrix()) {
                if (is.numeric(y)){
                        x <<- y 
                }else{
                        message("Not a numeric matrix")
                        x <<- NULL                        
                }                
                inverse <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inv=matrix()){
                if (is.numeric(inv)){
                        if((nrow(inv)==ncol(inv))&&(nrow(inv)==nrow(x))&&
                        (nrow(x)==ncol(x))){
                                inverse <<- inv             
                        } else {
                                message(paste("Not invertible matrix or", 
                                        "dimensions not valid", sep=" "))
                        }
                         
                }else{
                        message("The inverse is not a numeric matrix")
                        inverse <<- NULL                        
                }  
        } 
        
        getInverse <- function() inverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve takes a makeCacheMatrix object 'x' and checks if the inverse 
## has already been calculated. If yes, the inverse is retrieved from memory 
## using the getInverse() fuction of the corresponding makeCacheMatrix object.
## If no, the inverse is computed and stored in the corresponding 
## makeCacheMatrix object using setInverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("Getting cached inverse")                
        }else{
                matrix <- x$get()                
                inverse <- solve(matrix)
                x$setInverse(inverse)
        }
        return(inverse)
}
