## The makeCacheMatrix function is used to create a cache that can store the value of the inverse of a matrix. The makeCacheMatrix function returns a list which contains the return values of the functions defined within it. Each of these functions can be accessed by the cacheSolve function which uses them to compute the inverse of the object passed to it.

## makeCacheMatrix() - Creates the cache for storing the inverse value of a matrix. Returns a list that contains the results of its functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## set() - sets the value of the object passed to the makeCacheMatrix equal to its argument. It           also sets the inverse value in the cache to Null. It uses the <<- to set the values since the             variables are belong to a different environment than the function.
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        ## get() - gets the value of the object passed to makeCacheMatrix.
        get <- function() {
                x
        }
        
        ## setinv() - sets the value of the inverse in the cache equal to the value passed through its            argument. It also uses the <<- to set the value since variable is defined in the makeCacheMatrix          function i.e shares a different environment. 
        setinv <- function(xinv){
                inv <<- xinv
        }
        
        ## getinv() - gets the value of the inverse stored in the cache.
        getinv <- function(){
                inv
        }
        
        ## Returns a named list that contains the return values of the above functions.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        # Checking if the entered object is a matrix.
        stopifnot(!is.matrix(x))
        
        # Obtaining the inverse value from the cache. It does this by reading the value of the list               variable that has the name "getinv" (hence the $ sign). Since that value happens to be the return         value of the getinv function, we obtain the "inv" value as defined in the function "getinv".
        inv <- x$getinv()
        
        # If it is not null, it means it is already set and the matrix hasn't changed. In that case,              retrieve the inverse from the cache and return that as the result (skips the code following if            condition).
        if(!is.null(inv)){                      
                message("Returning inverse from cached data")
                return(inv)
        }
        
        # If the inverse is null, then get the value of the object into the "data" variable.
        data <- x$get()
        
        # Calculate inverse
        inv <- solve(data)
        
        # Set the inverse in the cache equal to the computed inverse (note the use of the <<- operator in         the setinv function).
        x$setinv(inv)
        
        # Return the inverse value
        inv
}
