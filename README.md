# Rarefaction Analysis Tutorial
#Step 1: Install vegan package.

install.packages(vegan)

#Step 2: Run necessary packages.

library(vegan)

#Step 3: Import data:


#Step 4: Set string.genera and temp.string as NULL 
#(will allow the use of these values in for-loop iterations).

string.genera = NULL
temp.string = NULL


#Step 5: Set up a for-loop with length equal to the length of the genera 
#column of the data.

for (i in 1:length(mw001_genera$genus))
  
#Step 6: add brackets under for-loop that designate the temporary strings
#as replicates for each iteration of genus and specimen count

  for (i in 1:length(mw001_genera$genus))
{
  temp.string = rep(mw001_genera$genus[i],mw001_genera$specimen_count[i])
  string.genera = c(string.genera, temp.string)
}


#Step 7: Set sample maximum as total amount of genera in sample, and name
#an object "resample" equal to 1000 (This will be the number of iterations or
#times that the for-loop will run). 


sample.maximum = sum(mw001_genera$specimen_count)
resample = 1000

#Step 8: Create a matrix for the rarefaction, where the number collected 
#is equal to the sample maximum, and the number of rows is equal to the 
#the object previously named "resample".

rarefaction.results = matrix(ncol = sample.maximum, nrow = resample)

#Step 9: Create a dataframe using the object rarefaction.results:

rarefaction.results = data.frame(rarefaction.results)

#Step 10: Create a for-loop for 1 through resample (resample = 1000), in which 
#temp.rarefaction results are set to equal NULL. Within this for-loop, 
#create another for-loop within whichfor each k sample in 1 through 
#sample.maximum, temp.sample is set to equal to the function 
#sample(string.genera, k, replace =  TRUE. 
#In this for-loop, also set temp.div equal to the function 
#length(unique(temp.sample)), and set temp.rarefactionresult equal to the 
#character string c(temp.rarefactionresult, temp.div)

for (l in 1:resample)
{
  
  
  temp.rarefactionresult = NULL
  
  for (k in 1:sample.maximum)
  {
    temp.sample = sample(string.genera, k, replace = TRUE)
    temp.div = length(unique(temp.sample))
    temp.rarefactionresult = c(temp.rarefactionresult, temp.div)
  }
  
  
  rarefaction.results[l,] = temp.rarefactionresult
  
}

#Step 11: Plot the results using base R plot function. labeling the
#x-axis as "Rarefaction Iterations" and the y-axis as "Number of Genera":

plot(x = c(1:sample.maximum), y = colMeans(rarefaction.results), 
     type = "l", main="Rarefaction Analysis",
     xlab="Iterations", ylab="Number Sampled")
