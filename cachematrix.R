## makeCashMatrix: create an object of type list
##cacheSolve calculates inverse of matrix en gives error messages if determinant = 0 or matrix != square

## makeCacheMatrix: Create an object of type list

makeCacheMatrix <- function(x = matrix()) {
        inv_matr <- matrix(NA_real_, 1, 1)  # initialise an empty matrix with NA_real_ to avoid coercion
        setfun <- function(y) {
                x <<- y
                inv_matr <<- matrix(NA_real_, n.row, n.col)  # initialise an empty matrix with NA_real_ to avoid coercion              
        }
                getfun <- function() x
                setsolvefun <- function(value) inv_matr <<- value
                getsolvefun <- function() inv_matr
                list(get = getfun,
                     setsolve = setsolvefun,
                     getsolve = getsolvefun)
        }



## CacheSolve: return a matrix that is the inverse of 'x'
## Inverse matrix exists if and only if matrix has determinant and determinant is not zero
## cacheSolve checks existence of inverse matrix before calculating.
## In case inverse matrix does not exist, cacheSolve returns a message

cacheSolve <- function(x, ...) {        
        
        inv_matr <- x$getsolve()              
        if(!any(is.na(inv_matr))) {             
                
                message("getting cached data")  
                return(inv_matr)                      
        }
        data <- x$get()      
        sq_matrix <- identical(nrow(data), ncol(data))
        if (sq_matrix==FALSE) {                         ## check matrix is square
                message("matrix is not square, the matrix does not have an inverse")
        } 
        else {
                det_data <- det(data)  # calculate determinant of data
                if(det_data == 0) {                     ## check determinant is 0
                        message("determinant is zero, the matrix is singular and does not have an inverse")
                }
                else {
                        inv_matr <- solve(data, ...)   
                        x$setsolve(inv_matr)           
                        inv_matr       
                }
                }       
}
