#first function makes a special matrix
makeCacheMatrix<- function(sp_mat = matrix()){
  inv <- NULL #null inverse initialization
  #set t he matrix
  set<-function(matrix){
    sp_mat<<- matrix
    inv <<- NULL
  }
  #get the matrix
  get<- function(){
    sp_mat #Return the matrix
  }
  
  #set the inverse of the matrix
  setInverse <- function(inverse){
    inv <<-inverse
  }
  
  #get the inverse of the matrix
  getInverse <- function(){
    inv 
  }
  
  #list of methods declared
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

#The second function computes special inverse 

cacheSolve<- function(x, ...){
   #return inverse of matrix x
  sp_mat<- x$getInverse()
  
  #if inverse is already set return
  if( !is.null(sp_mat) ) {
    message("getting cached data")
    return(sp_mat)
  }
  
  ##get the matrix from the object x
  
  data<- x$get()
  
  ## Calculate the inverse using usual matrix multiplication
  sp_mat <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(sp_mat)
  
  ## Return the matrix
  sp_mat
  
}
