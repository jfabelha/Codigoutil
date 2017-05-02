Entropia <- function (x) {
		output <- 0
		sumx <- sum(x)
		for (i in x) {
			output1 = (i/sumx)*log2(i/sumx)
			if (is.nan(output1))  
			output1 <- 0
			output <- output - output1
			}
		output
		}
