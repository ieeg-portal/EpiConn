library(vegan)

sink('StructFunc_Similarity_Analysis.txt')

# load distance matrix generated in python (metric: standardized euclidean)
# This reveals the pairwise distance between seizure time signals
mymatrix <- read.csv('../simmat_for_R.csv', header=FALSE)

# convert to R's dist format
mydistmat = as.dist(mymatrix)

# generate a dataframe corresponding to: column 1: seizure number; column 2: subject number
mydf = data.frame('sz_id'=c(1:45),'sub_id'=c(1,1,1,2,2,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9))

mydf <- within(mydf, {
  sub_id <- factor(sub_id)
  sz_id <- factor(sz_id)
})

# use adonis (from vegan) to do a permutational MANOVA
adonis(formula = mydistmat ~ sub_id, data = mydf)  

sink()

