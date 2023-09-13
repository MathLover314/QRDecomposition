gram_shmidt <- function(A) {
  V = matrix(0, nrow = nrow(A), ncol=ncol(A))
  R = matrix(0, nrow = nrow(A), ncol=ncol(A))
  for (i in 1:nrow(A)) {
    V[,i] = A[,i]
    if (i > 1) {
      for (j in 1:(i-1)) {
        V[, i] =V[, i] - proj(V[, j], A[,i])
      }
    }
    
  }
  Q = matrix(0, nrow = nrow(A), ncol=ncol(A))
  for (i in 1:nrow(A)) {
    Q[, i] = V[, i]/norm(V[,i], type = "2")
    for (j in 1:i) {
      R[j, i] = as.numeric(Q[,j]%*%A[,i])
    }
  }
  return(list(Q, R))
}

#projection of v on u
proj <- function(u, v) {
  u_ = c(u)
  v_ = c(v)
  return(as.numeric((u_%*%v_)/norm(u_, type = "2")^2) * u_)
}
p = matrix(c(1,2,3,4), nrow=2)
gram_shmidt(p)


