## This function makeCacheMatrix, creates a special "matrix".A list containing a function to
## 1)   set the value of the matrix
## 2)   get the value of the matrix
## 3)   set the inverse value of the matrix
## 4)   get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL                         ## SETUP THE inv_mat TO NULL AS PLACE HOLDER FOR FUTURE USE
        
        set <- function(y) {
                x <<- y 
                inv_mat <<- NULL
        }                                       ## SET MATRIX y, TO A NEW MATRIX y. AND RESET THE inv_mat TO NULL
        
        get <- function() x                     
        set_inv <- function(solve) inv_mat <<- solve  
        get_inv <- function() inv_mat  
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        inv_mat <- x$get_inv()
        
        if(!is.null(inv_mat)) {
                print("getting cached inverse data")  ## FIRST CHECK IF MATRIX HAS BEEN INVERTED
                return(inv_mat)
                
        }
        data <- x$get()
        inv_mat <- solve(data, ...)             ## COMPUTING THE INVERSE OF SQUARE MATRIX
        x$set_inv(inv_mat)
        inv_mat                                 ## RETURN A MATRIX THAT IS THE INVERSE OF 'x'
}

