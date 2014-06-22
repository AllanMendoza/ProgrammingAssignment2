## Caches inverse o a matrix to use in solving

 makeCacheMatrix <- function(Mi_Matriz = matrix()) {
   cachedInversa <- NULL
   set <- function(y) {
     Mi_Matriz <<- y
     cachedInversa <<- NULL
   }
   get <- function() Mi_Matriz
   setInversa <- function(inversa) cachedInversa <<- inversa
   getInversa <- function() cachedInversa
   list(set = set, get = get,
        setInversa = setInversa,
        getInversa = getInversa)
 }
  
  
 ## Use inverse cached if exists or get the inver of a matriz
 ## Return the inverse of matriz
  
 cacheSolve <- function(Mi_Matriz, ...) {
   ## Return a matrix that is the inverse of 'Mi_Matriz'
   ## Return a matrix that is the inverse of 'Mi_Matriz'
   invFunc <- Mi_Matriz$getInversa()
   if(!is.null(invFunc)) {
     message("getting cached data")
     return(invFunc)
   }
   data <- Mi_Matriz$get()
   invFunc <- solve(data, ...)
   Mi_Matriz$setInversa(invFunc)
   invFunc
 }
