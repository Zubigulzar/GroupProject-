---
title: "Analysis of Hr indicators as predictor of employeers left"
author: "Giusti Elena,Sarah Wong,Zohaib Gulzar, Tatsuya Nagata."
date: ""
output: html_document
---




###Human Resources Analytics
##Why are our best and most experienced employees leaving prematurely?

<br>This is a dataset that includes 14999 observation of single employees
<br>Fields in the dataset include:

<br/>Employee satisfaction level
<br/>Last evaluation
<br/>Number of projects
<br>Average monthly hours
<br>Time spent at the company
<br>Whether they have had a work accident
<br>Whether they have had a promotion in the last 5 years
<br>Department
<br>Salary
<br>Whether the employee has left

#The objectives of the project are the followings
<br>Which of those factors influence the employeers decison to left?
<br>Who will leave next?
<br>Is it possible to target with some specific strategy some department?

#The project is divided in the following step
<br>Step 1) Analyse actual state of the company HR indicators
- Clean the data
- Describe the data 
<br>Step 2) Analyse correlation and main factor definining HR indicators
-Correlationa analysis
-Factor reduction
<br>Step 3) Build a regression model to estimate who will be more likely to leave
-Simple regression model
-Randomtree model




##>Step 1)Analyse actual state of the company HR indicator

Looking at data we can make the following assumptions related the nature of the 10 variables

<br>satisfaction_level: A numeric indictor, presumably filled out by the employee.
<br>last_evaluation: A numeric indicator, presumably filled in by the employee’s manager.
<br>number_project: A integer - perhaps the number of projects the employee has worked on. 
<br>average_monthly_hours: The number of hours they work in the month
time_spend_company: An integer value, perhaps years of service.
<br>Work_accident: Looks like a boolean value, probably whether(1) or not(0) they had an accident.
<br>left: Looks like a boolean value, leave (1), not leave(0)
promoted_last_5years: Looks like a boolean value, promoted(1), not promoted(0)
<br>sales: Is a categorical variable assessing the department (sales,technical...)
<br>salary: Looks like a 3-level salary score (low, medium, high)

## Data quality report


```
## Error in summary(myData): object 'myData' not found
```
<br/>
<hr/>
<br/>

From basic statistical analysis we see that the overall satisfaction of the company is really low around 63% and that approximately 24% of the employees left. This brings us to the following step we would like therefore to understand bettwe who are the employees that are leaving

## First visualisations

<br/>

We will start our analysis looking more deeply to a set of all emplyees 


```r
Allemployees <- myData %>% select(satisfaction_level:promotion_last_5years)
```

```
## Error in eval(expr, envir, enclos): object 'myData' not found
```

```r
thecor <- cor(Allemployees)
```

```
## Error in is.data.frame(x): object 'Allemployees' not found
```

```r
cex.before <- par("cex")
corrplot(thecor, method = "circle" , type = "full", title = "Correlation Heat map of all data" , tl.cex = 1/par("cex"), cl.cex = 1/par("cex"), order = "original", number.font = 2 , tl.col = "red", tl.srt = 30)
```

```
## Error in corrplot(thecor, method = "circle", type = "full", title = "Correlation Heat map of all data", : object 'thecor' not found
```

```r
par(cex = cex.before)
```

## First visualisations of the employees who left

We will start our analysis looking more deeply to a subset composed of all emplyees who left


In the total of 15 000 employees that compose our database, here are the people that have left:


```
## Error in eval(expr, envir, enclos): object 'myData' not found
```

```
## Error in nrow(employeesleft): object 'employeesleft' not found
```

<br/>


```r
hr_valuable_people <- myData %>% filter(last_evaluation >= 0.50 | time_spend_company >= 4 | number_project > 5)
```

```
## Error in eval(expr, envir, enclos): object 'myData' not found
```

```r
hr_valuable_people2 <- hr_valuable_people %>% select(satisfaction_level, number_project: promotion_last_5years)
```

```
## Error in eval(expr, envir, enclos): object 'hr_valuable_people' not found
```

```r
M <- cor(hr_valuable_people2)
```

```
## Error in is.data.frame(x): object 'hr_valuable_people2' not found
```

```r
cex.before <- par("cex")
corrplot(M, method = "circle" , type = "full", title = "Valuable people" , tl.cex = 1/par("cex"), cl.cex = 1/par("cex"), order = "original", number.font = 2 , tl.col = "red", tl.srt = 30)
```

```
## Error in corrplot(M, method = "circle", type = "full", title = "Valuable people", : object 'M' not found
```

```r
par(cex = cex.before)
```


```r
thecor2 <- cor(employeesleft[c(1,2,3,4,5,6,8)])
```

```
## Error in is.data.frame(x): object 'employeesleft' not found
```

```r
corrplot(thecor2, method="circle" , title = "Correlation Heat map of employees who left" , tl.cex = 1/par("cex"), cl.cex = 1/par("cex"), order = "original", number.font = 2 , tl.col = "red", tl.srt = 30)
```

```
## Error in corrplot(thecor2, method = "circle", title = "Correlation Heat map of employees who left", : object 'thecor2' not found
```

## Who is leaving?

Number of people valuable to the company leaving. The screening criterea is that the employees have worked for company for more than 4 years and have good last evaluation results >0.50 


```r
hr_valuable_people <- employeesleft %>% filter(last_evaluation >= 0.50 | time_spend_company >= 4 | number_project > 5)
```

```
## Error in eval(expr, envir, enclos): object 'employeesleft' not found
```

```r
  nrow(hr_valuable_people)
```

```
## Error in nrow(hr_valuable_people): object 'hr_valuable_people' not found
```

<br/>


Let's create a data frame with only the people that have left the company, so we can visualise what is the distribution of each features:


```r
par(mfrow=c(1,3))
hist(employeesleft$satisfaction_level,col="#3090C7", main = "Satisfaction level") 
```

```
## Error in hist(employeesleft$satisfaction_level, col = "#3090C7", main = "Satisfaction level"): object 'employeesleft' not found
```

```r
hist(employeesleft$last_evaluation,col="#3090C7", main = "Last evaluation")
```

```
## Error in hist(employeesleft$last_evaluation, col = "#3090C7", main = "Last evaluation"): object 'employeesleft' not found
```

```r
hist(employeesleft$average_montly_hours,col="#3090C7", main = "Average montly hours")
```

```
## Error in hist(employeesleft$average_montly_hours, col = "#3090C7", main = "Average montly hours"): object 'employeesleft' not found
```

We can see why we don't want to retain everybody. Some people don't work well as we can see from their evaluation, but clearly there are also many good workers that leave.


```r
par(mfrow=c(1,2))
hist(employeesleft$Work_accident,col="#3090C7", main = "Work accident")
```

```
## Error in hist(employeesleft$Work_accident, col = "#3090C7", main = "Work accident"): object 'employeesleft' not found
```

```r
plot(employeesleft$salary,col="#3090C7", main = "Salary")
```

```
## Error in plot(employeesleft$salary, col = "#3090C7", main = "Salary"): object 'employeesleft' not found
```


From the previous histograms we can colcude that not only employers with low satisfaction level and bad evaluation are leaving.

Looking at data it seems that none of the distribution is normal but show strong polarization:

1) Employers that are leaving are highly polirize in 2 categories(really bad or really good)
2) The distribution of employers that are leaving is quite polirize on employers that are medium on satisfaction.
3)Employers that work less seems more willing to leave

<br/>
From a first analysis it  seems that the number of work accident and the salary are factors that might influence more directly the employers decision to leave


<br/>  Looking more deeply to relation between satisfaction level and decision to leaving:


##Step 2) Analyse correlation and main factor definining HR indicators

To perform correlation we need first of all to normalize those data that are not metric and to drop the value that we want to predict (left) from the ananlysis



```
## Error in eval(expr, envir, enclos): object 'myData' not found
```

```
## Error in is.data.frame(x): object 'myData2' not found
```

```
## Error in is.factor(x): object 'myData2' not found
```

```
## Error in is.factor(x): object 'myData2' not found
```

```
## Error in factor(x, ..., ordered = TRUE): object 'myData2' not found
```

```
## Error in is.data.frame(frame): object 'myData2' not found
```



```
## Error in is.data.frame(x): object 'ProjectData' not found
```

```
## Error in ncol(ProjectData): object 'ProjectData' not found
```

```
## Error in eval(expr, envir, enclos): object 'ProjectData' not found
```

```
## Error in is.data.frame(frame): object 'ProjectDataFactor' not found
```


After having done this adjustments we can now confirm that data are metric.
However we need to scale the data to have a more homogeneus dataset


```
## Error in apply(thedata, 2, function(r) c(min(r), quantile(r, 0.25), quantile(r, : object 'ProjectDataFactor' not found
```

```
## Error in apply(ProjectDataFactor, 2, function(r) {: object 'ProjectDataFactor' not found
```
Notice now the summary statistics of the scaled dataset:


```
## Error in apply(thedata, 2, function(r) c(min(r), quantile(r, 0.25), quantile(r, : object 'ProjectDatafactor_scaled' not found
```


We can therefore look to correlation level we can see that those are really low at few factor show correlation>3

We can then apply factor analysis, after some trial we understand that prediction through eingvalue gives the best assessment.



```
## Error in principal(ProjectDataFactor, nfactors = ncol(ProjectDataFactor), : object 'ProjectDataFactor' not found
```

```
## Error in eval(expr, envir, enclos): object 'UnRotated_Results' not found
```

```
## Error in as.data.frame(unclass(UnRotated_Factors)): object 'UnRotated_Factors' not found
```

```
## Error in ncol(UnRotated_Factors): object 'UnRotated_Factors' not found
```


```
## Error in as.data.frame(X): object 'ProjectDataFactor' not found
```

```
## Error in eval(expr, envir, enclos): object 'Variance_Explained_Table_results' not found
```

```
## Error in eval(expr, envir, enclos): object 'Variance_Explained_Table' not found
```

```
## Error in nrow(Variance_Explained_Table): object 'Variance_Explained_Table' not found
```

```
## Error in colnames(Variance_Explained_Table) <- c("Eigenvalue", "Pct of explained variance", : object 'Variance_Explained_Table' not found
```

```
## Error in inherits(x, "list"): object 'Variance_Explained_Table' not found
```

```r
eigenvalues  <- Variance_Explained_Table[, "Eigenvalue"]
```

```
## Error in eval(expr, envir, enclos): object 'Variance_Explained_Table' not found
```

```r
df           <- cbind(as.data.frame(eigenvalues), c(1:length(eigenvalues)), rep(1, length(eigenvalues)))
```

```
## Error in as.data.frame(eigenvalues): object 'eigenvalues' not found
```

```r
colnames(df) <- c("eigenvalues", "components", "abline")
```

```
## Error in `colnames<-`(`*tmp*`, value = c("eigenvalues", "components", : attempt to set 'colnames' on an object with less than two dimensions
```

```r
ggplot(melt(df, id="components"), aes(x=components, y=value, colour=variable)) + geom_line()
```

```
## Don't know how to automatically pick scale for object of type function. Defaulting to continuous.
```

```
## Error in eval(expr, envir, enclos): object 'components' not found
```

![plot of chunk figure01](figure/figure01-1.png)


```
## Error in principal(ProjectDatafactor_scaled, nfactors = max(3), rotate = rotation_used, : object 'ProjectDatafactor_scaled' not found
```

```
## Error in eval(expr, envir, enclos): object 'Rotated_Results' not found
```

```
## Error in as.data.frame(unclass(Rotated_Factors)): object 'Rotated_Factors' not found
```

```
## Error in ncol(Rotated_Factors): object 'Rotated_Factors' not found
```

```
## Error in sort(Rotated_Factors[, 1], decreasing = TRUE, index.return = TRUE): object 'Rotated_Factors' not found
```

```
## Error in eval(expr, envir, enclos): object 'Rotated_Factors' not found
```

```
## Error in inherits(x, "list"): object 'Rotated_Factors' not found
```


```
## Error in eval(expr, envir, enclos): object 'Rotated_Factors' not found
```

```
## Error in Rotated_Factors_thres[abs(Rotated_Factors_thres) < MIN_VALUE] <- NA: object 'Rotated_Factors_thres' not found
```

```
## Error in is.data.frame(x): object 'Rotated_Factors' not found
```

```
## Error in rownames(Rotated_Factors): object 'Rotated_Factors' not found
```

```
## Error in inherits(x, "list"): object 'Rotated_Factors_thres' not found
```




Notice now the summary statistics of the scaled dataset:

Based on eingvalue we can see thet the number of principal components are 3 

Component 1: number of project, montly hours, last evaluation we can call this component commitment
Component 2 : only satisfaction
Component 3: salary+promotion we can call this component remunaration


CONCLUSION: principal component analisys allow to easily have a lower number of factor to describe the dataset, however given the low correlation measure and the limited number of dependent variable we can conclude is not particularly useful in this case.

##Step 3) Build a regression model to estimate who will be more likely to leave
Now we will try to built a regression model to predict who will more possibly leave


```
## Error in nrow(myData): object 'myData' not found
```

# Make a few modications

```
## Error in eval(expr, envir, enclos): object 'myData' not found
```

```
## Error in eval(expr, envir, enclos): object 'd' not found
```

```
## Error in eval(expr, envir, enclos): object 'd' not found
```

```
## Error in terms.formula(formula, data = data): 'data' argument is of the wrong type
```

```
## Error in predict(output.forest, newdata = test): object 'output.forest' not found
```

Then we'll train a single decision tree using `rpart` to and evaluate to see how good our fit is.

https://www.kaggle.com/msjgriffiths/d/ludobenistant/hr-analytics/explore-explain-density-decision-trees
