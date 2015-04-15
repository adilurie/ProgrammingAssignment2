## The functions below return the invoice of a matrix. 
##We assum ethat the matrix is inertable.
##The 1st time the inverse is returned the inverse is calculated and sorted in cache.
##Thereafter when the inverse matrix is retrived from chache, a message "getting cached data" is retuned together with the inverse of the matrix

##Example: Here is in example of how to use the functions
##x=makeCacheMatrix()
##data <- matrix ( c(2, -4, -3, 2) , 2, 2)
##x$set(data)
##cacheSolve(x)
##cacheSolve(x)

##Each time ##x$set(data) is used for a different matrix. 
##The cache is clearded to NULL and the  1st time the cacheSolve function is run it calculates the inverse matrix ... as explained above.





##makeCacheMatrix Function
--------------------------
## This function generates/retuns  a list.
## The list contains 4 elements (set, get, setsolve and getsolve)
## Each of these elemnts are functions in their own right.
## The set function sets x and y IN THE cacheSolve function!!!
## The get function retreives the matrix.
## The setsolve function stores the inverse matrix into m IN THE cacheSolve function!!!
## The get function retreives chached inverse matrix.
   

## It is important to note that the 2 different m's exist in the 2 different functions. 
## m in the calling enviroment (dynamic scoping) - The m in the set and setsolve functions exist in the cacheSolve function!!!
## M in the defining enviroment(lexical  scoping) - While the m at the begining of the makeCacheMatrix function and in the  getsolve function exist within the makeCacheMatrix function.

   

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) m <<- solve
   getsolve <- function() m
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

   


## cacheSolve function
## ---------------------
## cacheSolve function   uses the functions that have been generated within the makeCacheMatrix function.
##This function returns the inverse of he matrix. 
##If this is the first time we are doing the inverse then it is claculated in this function (cacheSolve) and stored in cache using the setsolve element of the list x. Then the inverse is returned) 
##If the inverse has already been claculated then a msg is printed "getting cached data" and the inverse is returned.


cacheSolve <- function(x, ...) {
   m <- x$getsolve()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setsolve(m)
   m
}
   ## Return a matrix that is the inverse of 'x'
}
