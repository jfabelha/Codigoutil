myfmisra <- function(nr) {
  con <- file(paste("dados.txt", sep = ","), open ="r")
  on.exit(close(con))
  contador<-matrix(0,2,nr)
  while(TRUE) {
    line <- scan(con, sep="", quiet =TRUE, nlines=1,fill=TRUE, what="raw",strip.white = TRUE)
    if (length(line) == 0)
      break;
    valor <- as.integer(line[1])
    pos <- which(contador[1,] == valor)
    if (length(pos) > 0)
      contador[2,pos[1]] <- 1 + contador[2,pos[1]] 
    else {
      pos <- which(contador[2,] == 0)
      if (length(pos) > 0) {
        contador[1,pos[1]] <- valor
        contador[2,pos[1]] <- 1
      }
      else
        contador[2,] <- contador[2,] - 1
    }
  }
  print(contador)
}

a<-myfmisra(40)
a[,order(a[2,], decreasing=T)]
