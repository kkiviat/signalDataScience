################################################################################
### Part 1: R and Probability ##################################################

## Expected value estimate ----------------------------------------------
## returns a list of x and y, where x ~ Uniform[0,1] and y ~ Uniform[0,x]
get_pair = function() {
    x = runif(1)
    y = runif(1, min=0, max=x)
    return(c(x=x, y=y))
}

## simulate this
set.seed(10)
k = 10000
a = sapply(1:k, function(x) get_pair()) # each column is an x,y pair
x_vals = a[1,]
y_vals = a[2,]
library(ggplot2)
qplot(x=x_vals, y=y_vals)

## split x into bins
w = 0.01
endpoints = seq(0, 1, w)
num_bins = length(endpoints) - 1
x_means = vector(mode="numeric", length=num_bins)
for (i in 1:num_bins) {
    x_means[i] = mean(x_vals[y_vals >= endpoints[i] & y_vals < endpoints[i+1]])
}
## compute the means of each y bin
y_means = mapply(function(x,y) (x+y)/2, endpoints[-length(endpoints)], endpoints[-1])
qplot(y_means, x_means)

## compare simulated results to expected results ------------------------------
## plot the theoretical curve
qplot(x=y_means, y=(y_means-1)/log(y_means))

## combine them to see that they match up
df = data.frame(y=y_means, sim_x=x_means, theoretical_x=(y_means-1)/log(y_means))
ggplot(df) + geom_point(aes(y, sim_x)) + geom_line(aes(y, theoretical_x))




################################################################################
### Part 2: Data Analysis ######################################################

## Set up data -----------------------------------------------------------------
library(psych)
df=msq

## compute the fraction of missing values for each feature,
## sorted in descending order
sort(sapply(df, function(x) mean(is.na(x))), decreasing=TRUE)

## make a dataframe with columns "active" through "scornful" and Extraversion and Neuroticism
str(df)
chosen_indices = c(which(colnames(df) == 'active'):which(colnames(df) == 'scornful'),
                   which(colnames(df) == 'Extraversion'), which(colnames(df) == 'Neuroticism'))
df2 = df[chosen_indices]

## replace each missing value with that column's mean
df2 = data.frame(lapply(df2, function(x) ifelse(is.na(x), mean(unlist(x), na.rm=TRUE), x)))

## Create plots -----------------------------------------------------------------
## histograms for Extraversion and Neuroticism
library(Rmisc) # for multiplot
p1 = ggplot(df2) + geom_histogram(aes(x=Extraversion), binwidth=1)
p2 = ggplot(df2) + geom_histogram(aes(x=Neuroticism), binwidth=1)
multiplot(p1, p2)

## density plots for Extraversion and Neuroticism
p1 = ggplot(df2) + geom_density(aes(x=Extraversion))
p2 = ggplot(df2) + geom_density(aes(x=Neuroticism))
multiplot(p1, p2)

## scatter plot
ggplot(df2) + geom_point(aes(x=Neuroticism, y=Extraversion)) + geom_smooth(aes(x=Neuroticism, y=Extraversion))

## Linear Regression -----------------------------------------------------------------
## Extraversion against others (except Neuroticism)
extraversion_lm = lm(Extraversion ~ .-Neuroticism, data=df2)
## Neuroticism against others (except Extraversion)
neuroticism_lm = lm(Neuroticism ~ .-Extraversion, data=df2)

## print top 10 coefficients for each
## function that returns the names of the n largest (abs) columns in L
get_top_by_abs_value = function(L, n) {
    top_indices = names(head(
    sort(
        abs(L),
        decreasing=TRUE
    ) , n
    ))
    return(L[top_indices])
}

get_top_by_abs_value(extraversion_lm$coefficients, 10)
get_top_by_abs_value(neuroticism_lm$coefficients, 10)

## sociability is a strong predictor of extraversion,
## while guilt is a strong predictor of neuroticism
## and confidence is a very strong predictor of low
## neuroticism




################################################################################
### Part 3: SQL Queries ########################################################

## 1. What's the difference between WHERE and HAVING?
##
## WHERE is used before aggregating, while HAVING is used after.

## 2. Given a table Employees with a single column Salary of integers,
##    write two SQL queries to determine the second highest distinct salary.
##
## SELECT DISTINCT Salary
##   FROM Employees
## ORDER BY Salary DESC
## LIMIT 1 OFFSET 1
##
## SELECT MAX(Salary)
##   FROM Employees
##  WHERE Salary < (SELECT MAX(Salary) FROM Employees)

## 3. What's the difference between LEFT JOIN, RIGHT JOIN, and INNER JOIN?
##
## LEFT JOIN ensures that every row in the left table is included in the
## result, even if it doesn't match any rows from the right table (filling in with NULL values)
## Similarly, RIGHT JOIN ensures that every row in the right table is
## included, even if it has no matches in the left table.
## INNER JOIN will only include rows with matches
## For instance, given
## Table A         Table B
## x  y            y  z
## 1  2            2  3
## 2  3            4  5
## an inner join of A and B on their y columns would give only the row x=1, y=2, z=3
## a left join would have the rows (1, 2, 3) and (2, 3, NULL)
## a right join would have the rows (1, 2, 3) and (NULL, 4, 5)

## 4. Given a COURSES table with columns course_id and course_name, a FACULTY table
##    with columns faculty_id and faculty_name, and a COURSE_FACULTY table with columns
##    faculty_id and course_id, how would you return a list of the names of faculty members
##    who teach a course given the name of a course?
##
## To find the faculty teaching the course named 'CS 101':
## SELECT faculty_name
##   FROM FACULTY JOIN COURSE_FACULTY ON FACULTY.faculty_id=COURSE_FACULTY.faculty_id
##                JOIN COURSES ON COURSES.course_id=COURSE_FACULTY.course_id
##  WHERE COURSES.course_name = 'CS 101'
