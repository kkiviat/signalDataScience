# VECTORS ---------------------

# can set names of existing vectors
x = 1:3; names(x) = c("n1", "n2", "n3"); x

# vectors can have repeated names
x = c(a = 1, b = 2, a = 3); x

# assigning logical vectors as names convers to characters
x = 1:3; names(x) = c(TRUE, TRUE, FALSE); names(x)

# assigning name vector of wrong length
x = 1:3; names(x) = c("n1", "n2"); x # NA for extra name
x = 1:3; names(x) = c("n1", "n2", "n3", "n4"); x # error

# remove names
x = 1:3; names(x) = c("n1", "n2", "n3"); x = unname(x); x # unname doesn't modify x

# accessing by name
x = 1:3; names(x) = c("n1", "n2", "n3"); x["n1"]
x = 1:3; names(x) = c("n1", "n2", "n1"); x["n1"] # takes first when duplicate names


# LISTS --------------------
typeof(list("a", 1, TRUE))

# combines two lists into one
combine = function(a, b) {
  c(a, b)
}
combine(list(1,2), list(3,4))
list(1,2,3,4)

combine(list("a", 1), list(TRUE, 4))
list("a", 1, TRUE, 4)

# unlisting a nested list
unlist(list(1, list(TRUE, 3), "a"))

# why doesn't as.vector work on lists? because lists are already vectors, just not atomic ones (there is no as.atomic())
as.vector(list(1, list(TRUE, 3), "a"))

x = list(a=1, b=2, a=3)
x$a # accesses value 1
x["a"] # gives list(a=1)

# LISTS AND VECTORS -----------
# combine list and vector
c(c(1,2,3), list("a", 1, TRUE)) # creates a (collapsed) list

# list inside vector
c(list(1,2,"a"), list(TRUE, 0)) # creates (collapsed) list

# vector inside list
x = list(c(1,2,3), "a"); x # creates list where first element is vector
typeof(x[[1]])


# DATA FRAMES -----------
df = data.frame(matrix(1:100, nrow=10))
str(df)
typeof(df) # list
class(df) # data.frame

# names vs rownames and colnames
names(df) # gives colnames
rownames(df)
colnames(df)

# converting vector to data frame
as.data.frame(c(1,2,3))

# converting list to data frame
as.data.frame(list(c(1,2,3), c("a", "b", "c")))

# different length elements in list
as.data.frame(list(c(1,2,3), c("a", "b"))) # raises error

# COMBINE DATA FRAMES -----------
dim(rbind(df, df, df, df))
dim(cbind(df, df))

# create 10x100 df
dim(do.call(cbind, rep(df, 10)))

# create df with vector of characters
d = as.data.frame(c("a", "bc", "d")); str(d) # converts to factors by default
d = as.data.frame(c("a", "bc", "d"), stringsAsFactors=FALSE); str(d)

# SUBSETTING ---------------
x = 1:5
x[c(3,1)]
x[-c(3,1)]; x[c(-3, -1)] # equivalent
x[c(1, 1)]
x[1.4] # drops decimal point
x[1.6]
x[2.1]
x[-1.1]
x[-2.1]

x[c(1, -3)] # error
x[c(TRUE, FALSE)] # repeats pattern
x[TRUE]
x[c(1, NA, 2)] # gives NA in NA's position

names(x) = c("a", "b", "c", "d", "e")
x[c("a", "b")]
x[c("a", "x")] # gives NA for nonexistent name

# replace third element with 1:5
x = list(1,2,3,4,5); x[3] = list(1:5); x
list(1,2,1:5, 4, 5)

# comparing initializing list and iteratively filling values to iteratively extending list -----------
library(tictoc)
size = 1e7
tic()
x = list(integer(size))
for (i in 1:size) {
  x[i] = i
}
toc()

tic()
x = list()
for (i in 1:size) {
  x[i] = i
}
toc()

# subsetting data frames --------
df = data.frame(matrix(1:100, nrow=10, ncol=10))
df[2:4, 3:6]

# subset with single vector
df[2:4] # takes columns 2:4

df[2:4, c("X2", "X3", "X4")]

df[1]
typeof(df[1]) # returns data frame
is.data.frame(df[1])
df[,1]
typeof(df[,1]) # returns vector of values
is.data.frame(df[,1])


# N-DOMINOES ------------
# returns all pairs of integers in [0:n] (ignoring order)
ndominoes = function(n) {
  dominoes = vector("list", n*(n-1)/2)
  index = 1
  for (i in 1:n) {
    for (j in i:n) {
      dominoes[index] = list(c(i, j))
      index = index + 1
    }
  }
  return(dominoes)
}
ndominoes(3)

is_circle = function(L) {
  flatL = unlist(L)
  seconds = flatL[c(FALSE, TRUE)]
  #firsts = unlist(L)[c(TRUE, FALSE)]
  #shifted_firsts = c(firsts[2:length(firsts)], firsts[1])
  shifted_firsts = flatL[c(seq(3, length(flatL), 2), 1)]
  return(all(shifted_firsts == seconds))
}
is_circle(list(list(1, 2), list(2, 3), list(3, 1), list(1, 2), list(2, 1)))
is_circle(list(list(1, 2), list(2, 3), list(3, 4), list(4, 5)))


# MORE DATA FRAMES --------------
# turn 1:5 into c(10, 11, 3,4,5)
x = 1:5; x[1:2] = c(10, 11); x
x = 1:5; x[1:2] = x[1:2] + 9; x

# turn 1:10 into c(1, 100, 3, 100, 5, 100, 7, 100, 9, 10) with single subset assignment
x = 1:10; x[c(FALSE, TRUE)] = 100; x

x = 1:5; x[NA]; x[NA_real_] # NA is logical so it gets repeated?

mtcars[1:5]
mtcars[1:20] # only 11 columns
str(mtcars)

# replace NAs in data frame
df = data.frame(matrix(1:4), nrow=2); df[2,2] = NA; df[is.na(df)] = 0; df
is.na(df)

# given x, create fruits s.t. fruits[x] = c("apple", "banana", "apple", "apple", "banana", NA, "banana", "apple")
x = c("a", "b", "a", "a", "b", "x", "b", "a")
fruits = c(a="apple", b="banana"); fruits[x]

# alphabetize column names
mtcars[order(colnames(mtcars))]

# scramble_df -------------
scramble_df = function(df, rows=FALSE) {
  columns = sample(colnames(df), length(colnames(df)), replace=FALSE)
  if (rows) {
    rows = sample(rownames(df), length(rownames(df)), replace=FALSE)
  } else {
    rows = rownames(df)
  }
  return(df[rows, columns])
}
scramble_df(mtcars, TRUE)

# sample k columns with replacement -----------
sample_columns = function(df, k) {
  return(df[sample(colnames(df), k, replace=TRUE)])
}
head(sample_columns(mtcars, 3))

# m random contiguous rows --------
get_random_contiguous_rows = function(df, m) {
  i = sample(1:(nrow(df)-m+1), 1)
  return(df[i:(i+m-1),])
}
get_random_contiguous_rows(mtcars, 4)

# remove columns with given name -----------
remove_columns = function(df, colname) {
  while (colname %in% colnames(df)) {
    df[colname] = NULL
  }
  return(df)
}
head(remove_columns(mtcars, "mpg"))
head(mtcars)

# STRING MANIPULATION -------------
# count occurences of each letter in column names --------
count_letters_in_column_names = function(df) {
  all_names = paste(colnames(mtcars), collapse="") # convert to one contiguous string
  all_names = as.list(unlist(strsplit(all_names, "")))
  sapply(letters, function (x) length(grep(x, all_names)))
}
count_letters_in_column_names(mtcars)

# modify column names ---------------
# but columns already have spaces replaced with .?
mod_colnames = function(df) {
  colnames(df) = gsub(" ", ".", colnames(df))
  colnames(df) = paste0(colnames(df), "_mod")
  return(df)
}
df = data.frame("col 1" = 1:5, "col 2 " = 6:10)
mod_colnames(df)

# remove last 4 characters of colnames ----------
remove_tail_4 = function(df) {
  colnames(df) = sapply(colnames(df), function (x) substr(x, 1, nchar(x)-4))
  return(df)
}
df = data.frame("column1" = 1:5, "c2 " = 6:10)
remove_tail_4(df)

# join row names by underscores --------
join_row_names = function(df) {
  do.call(paste, c(as.list(rownames(df)), sep="_"))
}
join_row_names(mtcars)

# spiral order --------------
# interview question
# gets an edge along a spiral transversal given current bounding box corners and current direction
get_edge = function(df, direction, corners, clockwise) {
  min_x = corners['min_x']
  max_x = corners['max_x']
  min_y = corners['min_y']
  max_y = corners['max_y']
  values = switch(direction,
         right = if (clockwise) as.numeric(df[min_y, min_x:max_x]) else as.numeric(df[max_y, min_x:max_x]),
         left = if (clockwise) as.numeric(df[max_y, max_x:min_x]) else as.numeric(df[min_y, max_x:min_x]),
         up = if (clockwise) df[max_y:min_y, min_x] else df[max_y:min_y, max_x],
         down = if (clockwise) df[min_y:max_y, max_x] else df[min_y:max_y, min_x]
         )
  return(values)
}

# updates the boundary of the remaining matrix based on the last move
update_corners = function(corners, direction, clockwise) {
  val_to_update = switch(direction,
         right = ifelse(clockwise, 'min_y', 'max_y'),
         left = ifelse(clockwise, 'max_y', 'min_y'),
         up = ifelse(clockwise, 'min_x', 'max_x'),
         down = ifelse(clockwise, 'max_x', 'min_x')
  )
  if (val_to_update %in% c("min_x", "min_y")) {
    corners[val_to_update] = corners[val_to_update] + 1
  } else {
    corners[val_to_update] = corners[val_to_update] - 1
  }
  return(corners)
}

# traverses a 2d numeric data frame in an inward spiral starting from the top left
get_spiral_order = function(df, clockwise=FALSE) {
  corners = c(min_x=1, min_y=1, max_x=ncol(df), max_y=nrow(df))
  if (clockwise) {
    directions = c("right", "down", "left", "up")
  } else {
    directions = c("down", "right", "up", "left")
  }
  
  index = 0
  vals = c()
  while (length(vals) < prod(dim(df))) {
    direction = directions[(index %% length(directions)) + 1]
    index = index + 1
    vals = c(vals, get_edge(df, direction, corners, clockwise))
    corners = update_corners(corners, direction, clockwise)
    print(corners)
  }
  return(vals)
}
get_spiral_order(data.frame(matrix(1:9, nrow=3)))
get_spiral_order(data.frame(matrix(1:6, nrow=2)))


# test switch -----
x = "b"
y = c(0,1,2,3)
switch(x,
       a = (y[1] = 1),
       b = (y[1] = 2))
y

# Fibonacci --------
