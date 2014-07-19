##
## File description: This file contains two functions whose purpose is to 
## cache the inverse of a matrix. 
## 
## Function makeCacheMatrix
## Input 	: An invertible matrix. 
##	          If input is not a matrix, e.g. it is an integer object, 
##		  then stop processing.
##		  If no input is supplied, an empty matrix is used.
## Output 	: Returns a special "matrix" which is a list of set/get  
##		  functions to manage an inverse matrix object stored in cache.
## Purpose	: To create a special matrix object that can cache its 
##		  inverse via set/get functions.  
##
makeCacheMatrix <- function(x = matrix()) {
	# Do not proceed if the input is not a matrix
        if( !is.matrix(x) ) stop( "Input must be a matrix" )
	# Initialize the inverse object and define all set/get functions
        the_inverse <- NULL
	# Set x to the supplied matrix and initialize the inverse. 
        set <- function( y ) {
		if( !is.matrix(y) ) stop( "Input must be a matrix" )
                x <<- y
		# Setting the_inverse to NULL here will indicate to later
		# processing in cacheSolve that the matrix has been changed. 		
                the_inverse <<- NULL	
        }
	# Get the supplied matrix. 
        get <- function() x	
	
	# Cache the inverse 
        setinverse <- function( inverse ) { 
		the_inverse <<- inverse
	}
	# Get the inverse that's been cached
        getinverse <- function() the_inverse

	# Return the list of get/set functions to be used on the_inverse 
	# or the matrix.
        list( set 	 = set,        get 	  = get,
              setinverse = setinverse, getinverse = getinverse )
}

##
## Function cacheSolve
## Input 	: The list of functions returned from makeCacheMatrix
## Output 	: Either a newly computed inverse 
##		  OR the inverse that has been stored in cache. 
## Purpose	: This function computes the inverse of the special "matrix" 
##		  returned by makeCacheMatrix. 
##		  If the inverse has already been calculated 
##		  AND the matrix has not changed, then the inverse is 
##		  retrieved from the cache and returned.
##		  Else the inverse is computed and returned.
##
cacheSolve <- function(x, ...) {
        ## Get the inverse of the user supplied matrix stored in cache, if any.
	the_inverse <- x$getinverse()
	## If the inverse is NOT NULL return the stored inverse.
	## Note - it will be NULL if it's not yet been calculated or 
	## it has changed via a call to the Set function.
        if( !is.null( the_inverse) ){
                message( "....getting cached data" )
                return( the_inverse )
        }
	## Either the inverse hasn't been computed yet or it has changed via a 
	## direct call to the Set function.
	## So, compute the new inverse.
        data <- x$get()
        the_inverse <- solve( data,...)   
	## Store it 
        x$setinverse( the_inverse )
	## Return the stored inverse.
        the_inverse
}
