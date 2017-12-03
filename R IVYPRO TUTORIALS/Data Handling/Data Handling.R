##### VECTORS
x <- c(2.1, 4.2, 3.3, 5.4)
print (x)

# Accessing vectors
x[c(3, 1)]

# Sort
x[order(x)]
x[order(-x)]

# Character vector
x_str <- c("Ele1","Mle","Ale")
?sort
sort(x_str)
sort(x_str,decreasing = TRUE)

# Omit specified elements
x[-c(3, 1)]

# Logical Vectors
x[c(TRUE, TRUE, FALSE, FALSE)]

x[x > 3]

# Recycled to be same length if logical vector is less
x[c(TRUE, FALSE)]

# Equivalent to
x[c(TRUE, FALSE, TRUE, FALSE)]

#A missing value in the index always yields a 
#missing value in the output
x[c(TRUE, TRUE, NA, FALSE)]

# Nothing returns full vector
x[]

# Zero returns 0 length vector
x[0]

# Character vectors
y <- setNames(x, letters[1:4])
print (y)

y[c("d", "c", "a")]

#### LISTS

# Matrices
a <- matrix(1:9, nrow = 3,byrow=TRUE)
colnames(a) <- c("A", "B", "C")
print (a)

a[1:2, ]
a[c(T,F,T), ]

# Combinations
a[c(T, F, T), c("B", "A")]

#### DATA FRAMES
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
class(df)
print (df)

# Subsetting data frames
df[df$x == 2, ]
df[c(1, 3), ]
df[c("x", "z")]

# Like a matrix
df[ , c("x", "z")]

### LOADING data
?read.csv
df <- read.csv("/Users/vivekkalyanarangan/Desktop/Forest Fires/forestfires.csv")
str(df)
summary(df)

# Accessing data frames
head(df)
df["month"]
df[c("month","day")]

# And Subsetting
df_sub <- subset(df, month=="aug" & day=="fri")

# OR Subsetting
df_sub <- subset(df, month=="aug" | day=="fri")

# More Subsetting
df_sub2 <- subset(df, month=="aug" & day=="fri", select = c("temp","wind"))

subset(df, month=="aug" | month=="jun")

df_sub3 <- df[df$month=="aug", ]

# Subsetting on more than 1 values
df_sub4 <- df[df$month %in% c("jun","aug"), ]
df_sub4

# Subsetting using column indices
df_sub5 <- df[, 1:2]
df_sub5

# Subsetting 1st, 3rd, 5th columns
df_sub6 <- df[, c(1, 3, 5)]
df_sub6

# Subsetting both rows and columns
df_sub8 <- df[c(1, 3), 3:6]
df_sub8

# Sort data
df_sorted <- df[with(df, order(month, day)), ]

# plyr package
# install.packages("plyr")
library(plyr)
df_arrange <- arrange(df,desc(month),day)

# take a vector from 1 to 10. access all the odd elements
# sort the odd numbers' vector descending
# Create a list with elements --> "Ale", "Zle", "Mle"
# Sort them in both ascending and descending order
# Print the 1st and 3rd elements of the above list
# Print all other elements of the list except the 2nd element
# Load forestfires.csv
# Output summary of the dataset
# Sort Hierarchically by month, day
# Retrieve records that have X greater than 7
# Retrieve records that have X greater than 7 and Y less than equal to 5
# Retrieve records that have FFMC greater than 60 and sort the results by "month"
# Retrieve the first 3 columns of the dataset
# Retrieve the first 100 rows and the 3rd,5th column of the dataset



#cr <- seq(1,10)
#len <- length(cr)
#cr[seq(1,len,by=2)]

##### APPLY FUNCTIONS
X<-matrix(rnorm(30), nrow=5, ncol=6)
print (X)
apply(X,2 ,sum)

?apply

# 1. It can be used for other objects like dataframes, lists or vectors.
# 2. The output returned is a list (thus the l in the function name) which has the same number of elements as the object passed to it.
A<-matrix(1:9, 3,3)
B<-matrix(4:15, 4,3)
C<-matrix(8:10, 3,2)
MyList<-list(A,B,C) # display the list

# extract the second column from the list of matrices, using the selection operator "["
lapply(MyList,"[", , 2)
MyList

cor(df$X,df$Y)
# Another example: we now  extract the first row from the list of matrices, using the selection operator "["
lapply(MyList,"[", 1, )

?sapply
# sapply works as lapply
# but it tries to simplify the output to the most elementary data structure 
# that is possible. In effect, as can be seen in the base manual, 
# sapply is a 'wrapper' function for lapply. 
# An example may help. Say we want to repeat 
#   the extraction operation of a single element as in the last example, 
# now taking the first element of the second row (indexes 2 and 1) for each matrix. 
# As we know, lapply would give us a list
lapply(MyList,"[", 2,1 )
sapply(MyList,"[", 2,1 )
# unless we tell simplify=FALSE as parameter to sapply, in which case a list will be returned
sapply(MyList,"[", 2,1, simplify=F)

?mapply
# mapply applies a Function to Multiple List or multiple Vector Arguments
Q=matrix(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)),4,4)
Q
Q=mapply(rep,1:4,4)
Q

##### MISSING VALUES

# find
is.na(df)
sapply(df,function(x) sum(is.na(x)))
colSums(is.na(df))

is.na(airquality)
sapply(airquality,function(x) sum(is.na(x)))
colSums(is.na(airquality))


#apply replace
f=function(x){
  if(class(x)=="numeric"){
    x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
    x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
    #x #display the column
  }else{
    #print (x)
  }
  return( x )
}
lapply(df,f)
df

a2 <- data.frame(apply(airquality,2,f))
sapply(a2,function(x) sum(is.na(x)))

# replace
df[is.na(df)] <- '.'

#### GROUPING AND PIVOTING
# install.packages("dplyr")
library(dplyr)
# Converting data to a table data frame ["Smarter" Version of data frame]
data_tdf <- tbl_df(df)

# filter(), arrange(), select(), mutate() and summarise()
summarise(group_by(df, month), sum_rain=sum(rain),sum_wind=sum(wind),mean_rain=mean(rain),mean_wind=mean(wind),count=n())

# df$Month_Full <-
#   sapply(df$month,function(x){
#   if(x=="aug"){
#     x <- as.factor("August")
#   }
#     return( x )
# })
