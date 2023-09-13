# QRDecomposition
![alt text](gram.png)

In this project I used Gram-Shmidt process to calculate QR decomposition where Q is orthogonal and R is upper triangle matricies.
```R
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
```
