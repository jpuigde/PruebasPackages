require(Matrix)


n=10
m=6
Matrix_ini <- matrix(seq(n*m),nrow = n,ncol = m)

# funcio_a_aplicar <- get('*')
i=1
get_cross_function <- function(Matrix_ini, function_to_aplly="I",...){
  function_to_aplly <- get(function_to_aplly)
  
  for(i in 1:(floor(ncol(Matrix_ini)/2)+1) ){
    if(i==1){
      result <-NULL
      permutation <- as(as.integer( c(ncol(Matrix_ini),1:(ncol(Matrix_ini)-1)  )  ),"pMatrix")
      permutation_matrix <- as(as.integer( c(1:ncol(Matrix_ini)  )  ),"pMatrix")
    }
    result <- cbind(result, function_to_aplly( Matrix_ini * (Matrix_ini %*% permutation_matrix),...=...))
    permutation_matrix <- permutation_matrix %*% permutation
  }
return (result)
}





get_cross_function(Matrix_ini)
get_cross_function(Matrix_ini,'^',2)
get_cross_function(Matrix_ini,'^',1/2)



