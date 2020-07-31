## Esta funcion permite guardar la matriz de manera que pueda ser utilizada 
## posteriormente en la siguiente funcion, la forma en la que funciona es que
## aloja el data.frame o la matriz de interes y le aplica la transformacion
## deseada, en este caso, la inversa de una matriz.

makeCacheMatrix <- function(x=matrix()){
  M <- NULL
  set <- function(y){
    x <<- y
    M <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) M <<- solve(x) 
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    
   
}

## Para el caso de la funcion cacheSolve, esta utiliza la funcion anterior, makeCacheMatrix
## para mostrar el resultado esperado de la operacion solicitada.

cacheSolve <- function(x,...) {
  M <- x$getinverse()
  if(!is.null(M)){
    message("obteniendo la matriz") # en esta parte se muestra cuando la matriz se ha guardado.
    return(M)
  }
  data <- x$get()
  M <- solve(data,...)
  x$setinverse(M)  
  M
}





