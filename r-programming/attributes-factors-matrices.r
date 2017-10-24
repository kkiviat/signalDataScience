attributes(mtcars)
names(mtcars) = NULL
str(mtcars)



# double names of a named list ----
double_names = function(l) {
  attr(l, "names") = sapply(attr(l, "names"), function (x) paste0(x, x))
  return(l)
}

l = double_names(list(a=1, b=2))
names(l)

# FACTORS -------------
f = factor(c("a", "b", "c")); levels(f)
f[1] = "b"
f[1] = "e" # error
levels(f)[length(levels(f)) + 1] = "e"; levels(f) # doing this additional times doesn't change anything
levels(f) = c(levels(f), "e") # same
f[1] = "e" # no error

# combining factors
g = factor(c("x", "y", "z")) # creates vector of integers corresponding to the levels of the elements in each factor
c(f, g)
factor(c(f,g)) # e.g. b and y are both mapped to 2

factor(c("a", "b", "a"))
as.vector(c("a", "b", "a"))

factor(letters)
rev(factor(letters))
factor(letters, levels = rev(letters))

# fruits ----
fruits = c("apple", "grapefruit", "NA", "apple", "apple", "-", "grapefruit", "durian")
table(factor(fruits, exclude=c("NA", "-")), useNA="always")

# convert character vector to factor including NAs ---------
char_to_factor = function(v) {
  factor(v, exclude=NULL)
}
char_to_factor(c("a", NA, "b", "c", NA))
char_to_factor(c("a", "b", "c"))

# convert half of columns to factors ----
cols_to_factor = function(df) {
  df[,1:(floor(ncol(df))/2)] = lapply(df[,1:(floor(ncol(df))/2)], factor)
  # use lapply since data frames are lists
  return(df)
}

df = data.frame(x=c("a","b","c"), y=c(1,2,3), z=c(4,5,6), w=c("x","y","z"), v=c(9,6,5))
df = cols_to_factor(df); df$x; df$y; df$z; df$w; df$v

# convert columns with <= 5 unique values into factors -----
simple_cols_to_factor = function(df) {
  indices = sapply(df, function (x) length(unique(x)) <= 5)
  df[, indices] = lapply(df[, indices], factor)
  return(df)
}
df = data.frame(x=c(1,2,3,4,5,6), y=c(1,2,3,1,2,3), z=c(5,5,2,5,5,5), w=c("x","y","z","w","y","z"))
df = simple_cols_to_factor(df)
str(df)

# replace NAs with most frequent -----
replace_NA_with_most_common = function(df) {
  factor_cols = colnames(df2)[sapply(df, is.factor)]
  for (col in factor_cols) {
    df[is.na(df[,col]), col] = names(which.max(table(df[,col])))
  }
  return(df)
}
df2 = data.frame(x=c(1,2,3,4,5,6), y=factor(c(1,2,NA,1,2,3)), z=c(5,5,5,5,5,5), w=c("x",NA,NA,"y","y","z"))
colnames(df2)[sapply(df2, is.factor)]
df2 = replace_NA_with_most_common(df2)
df2

# replace NAs with random column value w/ prob proportional to freq ----
replace_NA_with_random = function(df) {
  factor_cols = colnames(df)[sapply(df, is.factor)]
  for (col in factor_cols) {
    values = df[!is.na(df[, col]), col]
    df[is.na(df[,col]), col] = sample(values, 1)
  }
  return(df)
}
df3 = data.frame(x=c(1,2,3,4,5,6), y=factor(c(1,2,NA,1,2,3)), z=factor(c(NA,5,5,1,5,5)), w=c("x",NA,NA,"y","y","z")); df3
df3 = replace_NA_with_random(df3)
df3

# convert factors to indicator variables --------
convert_factors_to_indicators = function(df) {
  factor_cols = colnames(df)[sapply(df, is.factor)]
  for (col in factor_cols) {
    df_new = data.frame(matrix(nrow=nrow(df), ncol=0)) # initialize empty data frame
    
    for (level in tail(levels(df[,col]), -1)) { # create new column for all but first level
      new_col_name = paste(col, level, sep="_")
      df_new[new_col_name] = sapply(df[,col], function (x) ifelse(x == level, 1, 0))
    }
    
    df[col] = NULL
    df = cbind(df, df_new)
  }
  return(df)
}

df3 = data.frame(x=c(1,2,3,4,5,6), y=factor(c(1,2,3,1,2,3)), z=factor(c(5,5,5,5,5,5)), w=c("x",NA,NA,"y","y","z")); df3
str(df3)
df3 = convert_factors_to_indicators(df3)
str(df3)

df = mtcars[1:10,]; for (n in c("cyl", "am", "carb")) df[[n]] = factor(df[[n]])
str(convert_factors_to_indicators(df))


# ADOLESCENT HEALTH ---------
load("time/time.dat")
str(df)
levels(df$H2GH42) # has some invalid times indicating refused or don't know --- they will be converted to NAs

# H2GH42: During the school year, what time do you usually go to bed on week
# nights? Type in time in this format HH:MM A for AM or HH:MM P for
# PM. Please remember that midnight is 12:00 A and noon is 12:00 P!

# H2GH43: During the summer, what time do you usually go to bed on week nights?

# convert columns to number of hours past 8:00PM
# takes a string of the format "HH:MM{A|P}" and converts to hours past 8:00PM
# using strings
# Note: anything in (8PM, 12PM] is considered after 8PM, anything in (12PM, 8PM) is before 
convert_to_hours_past_8pm = function(t) {
  split_time = strsplit(t, ":")[[1]]
  hour = as.numeric(split_time[1])
  minute = as.numeric(substr(split_time[2], 1, 2))
  AM_PM = substr(t, nchar(t), nchar(t))
  if ((AM_PM == "A" && hour < 12) || (AM_PM == "P" && hour == 12)) {
    hour = hour + 12
  }
  hour = hour + minute/60
  if (is.na(hour)) print(t)
  return(hour - 8)
}

# using time class
convert_to_hours_past_8pm2 = function(t) {
  t = strptime(paste0(t, "M"), format="%I:%M%p")
  
  if (t$hour <= 12) t$hour = t$hour + 24
  t_comparison = strptime("08:00PM", format="%I:%M%p")
  return(difftime(t, t_comparison)[[1]])
}

convert_to_hours_past_8pm("01:45A")
convert_to_hours_past_8pm2("01:45A")
convert_to_hours_past_8pm("12:00A")
convert_to_hours_past_8pm2("12:00A")
convert_to_hours_past_8pm("12:00P")
convert_to_hours_past_8pm2("12:00P")
convert_to_hours_past_8pm("03:30P")
convert_to_hours_past_8pm2("03:30P")


# convert all times in df to hours past 8PM
df = data.frame(lapply(df, function(x) sapply(x, function (x) convert_to_hours_past_8pm(as.character(x)))))
str(df)

library(ggplot2)
ggplot(df) + geom_histogram(aes(H2GH42, fill="school year"), binwidth=1, alpha=0.5) +
  geom_histogram(aes(H2GH43, fill="summer"), binwidth=1, alpha=0.5) +
  xlab("Hours past 8PM") +
  scale_fill_manual(values = c("red", "green"), name="Time of Year") +
  #scale_fill_discrete(name="Time of Year") +
  ggtitle("Frequency of reported bedtimes")
  
# MATRICES --------------
# convert nxm matrix to mxn matrix s.t. as.numeric() result doesn't change --------
convert_matrix = function(m) {
  dim(m) = rev(dim(m))
  return(m)
}
m = matrix(1:10, nrow=5)
as.numeric(m)
as.numeric(convert_matrix(m))

# ----
df = data.frame(matrix(1:100, nrow=10)); df[5, 5] = NA; df[6, 6] = NA
df
df[is.na(df)]

# returns all entries from numeric data frame df that are divisible by k
find_multiples = function(df, k) {
  return(df[df %% k == 0])
}

find_multiples(df, 5)

# min_matrix -------------
# generate nxm matrix where value in row i, col j is min(i,j)
min_matrix = function(n, m) {
  mat = matrix(nrow=n, ncol=m)
  for (row in 1:n) {
    for (col in 1:m) {
      mat[row, col] = min(row, col)
    }
  }
  return(mat)
}
min_matrix(4, 5)

# determine if matrix is symmetric -------
test_symmetry = function(m) {
  return(all(m == t(m)))
}
m1 = matrix(c(1,2,3,2,1,3,3,3,2), nrow=3); m1
test_symmetry(m1)
m2 = matrix(c(1,2,1,2,1,3,3,3,2), nrow=3); m2
test_symmetry(m2)

# trace ------
trace = function(mat) {
  indices = seq(1, length(mat), sqrt(length(mat)) + 1)
  return(sum(mat[indices]))
  # or return(sum(diag(mat)))
}
m1 = matrix(c(1,2,3,2,1,3,3,3,2), nrow=3); m1
trace(m1)
m2 = matrix(c(5,2,1,2,4,3,3,3,2), nrow=3); m2
trace(m2)

trace(m1) + trace(m2)
trace(m1 + m2)

trace(5*m1)
trace(m1)

# matrix multiplication --------
# very slow
matrix_mult = function(A, B) {
  prod = matrix(nrow=nrow(A), ncol=ncol(B))
  for (row in 1:nrow(A)) {
    for (col in 1:ncol(B)) {
      prod[row, col] = sum(A[row, ] * B[, col])
    }
  }
  return(prod)
}
A = matrix(1:4, nrow=2)
matrix_mult(A, A)

matrix_mult2 = function(A, B) {
  prod = matrix(nrow=nrow(A), ncol=ncol(B))
  for (row in 1:nrow(A)) {
    for (col in 1:ncol(B)) {
      prod[row, col] = sum(A[row, ] * B[, col])
    }
  }
  return(prod)
}

A = matrix(1:10000, nrow=2000)
B = matrix(1:10000, ncol=2000)
all(A %*% B == matrix_mult(A, B))

system.time(matrix_mult(A, B))
system.time(A %*% B)

