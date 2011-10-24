# Function to standardize input values
zscore <- function(x, mean.val=NA) {
	if(is.matrix(x)) return(apply(x, 2, zscore, mean.val=mean.val))
	if(is.data.frame(x)) return(data.frame(apply(x, 2, zscore, mean.val=mean.val)))
	if(is.na(mean.val)) mean.val <- mean(x)
	sd.val <- sd(x)
	if(all(sd.val == 0)) return(x) # if all the values are the same
	(x - mean.val) / sd.val 
}

