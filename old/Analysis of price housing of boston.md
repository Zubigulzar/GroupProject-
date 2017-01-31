---
title: "Analysisi of price of housing Boston"
author: "Giusti Elena"
date: ""
output: html_document
---



##This doccument is the result of a Survey among 506 respondent
 This dataset was taken from the StatLib library which is
maintained at Carnegie Mellon University
<br>Number of Instances: 506                
<br>   Number of Attributes: 13 continuous attributes (including "class"
attribute "MEDV"), 1 binary-valued attribute 

<br1. CRIM      per capita crime rate by town
    2. ZN        proportion of residential land zoned for lots over 
                 25,000 sq.ft.
    3. INDUS     proportion of non-retail business acres per town
    4. CHAS      Charles River dummy variable (= 1 if tract bounds 
                 river; 0 otherwise)
    5. NOX       nitric oxides concentration (parts per 10 million)
    6. RM        average number of rooms per dwelling
    7. AGE       proportion of owner-occupied units built prior to 1940
    8. DIS       weighted distances to five Boston employment centres
    9. RAD       index of accessibility to radial highways
    10. TAX      full-value property-tax rate per $10,000
    11. PTRATIO  pupil-teacher ratio by town
    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
                 by town
    13. LSTAT    % lower status of the population
    14. MEDV     Median value of owner-occupied homes in $1000's
    
    
    Target variable to predict: pricing of house MEDIAN
    

```
## Error in View(myData, header = FALSE): unused argument (header = FALSE)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.73    6.95   11.36   12.65   16.96   37.97
```


####Questions

#<br>1.What are the main factors that influence the price of house in Boston?
#<br>2.Which are the main cluster of houses that can be identify?
#<br>3.What is the best predictions of the relative importance of those factors?( regression)


## Step 1: Confirm data is metric


```
##  [1] "ID"        "X1..CRIM"  "X2..ZN"    "X3..INDUS" "X4..CHAS" 
##  [6] "X5..NOX"   "X6..RM"    "X7..AGE"   "X8..DIS"   "X9..RAD"  
## [11] "X10..TAX"  "X11..PTRA" "X12..B"    "X13..LSTA" "X14..MEDV"
```

## Step 1: Confirm data is metric


```r
knitr::kable(round(my_summary(ProjectDataFactor), 2))
```



|          |    min| 25 percent| median|   mean| 75 percent|    max|    std|
|:---------|------:|----------:|------:|------:|----------:|------:|------:|
|X1..CRIM  |   1.00|     126.25| 252.50| 252.22|     377.75| 504.00| 145.81|
|X2..ZN    |   0.00|       0.00|   0.00|  11.36|      12.50| 100.00|  23.32|
|X3..INDUS |   0.46|       5.19|   9.69|  11.14|      18.10|  27.74|   6.86|
|X4..CHAS  |   0.00|       0.00|   0.00|   0.07|       0.00|   1.00|   0.25|
|X5..NOX   |   0.38|       0.45|   0.54|   0.55|       0.62|   0.87|   0.12|
|X6..RM    |  35.61|      58.86|  62.09|  62.85|      66.23|  87.80|   7.03|
|X7..AGE   |   2.90|      45.02|  77.50|  68.57|      94.07| 100.00|  28.15|
|X8..DIS   |  11.30|      21.00|  32.07|  37.95|      51.88| 121.27|  21.06|
|X9..RAD   |   1.00|       4.00|   5.00|   9.55|      24.00|  24.00|   8.71|
|X10..TAX  | 187.00|     279.00| 330.00| 408.24|     666.00| 711.00| 168.54|
|X11..PTRA |  12.60|      17.40|  19.05|  18.46|      20.20|  22.00|   2.16|
|X12..B    |   0.32|     375.38| 391.44| 356.67|     396.23| 396.90|  91.29|
|X13..LSTA |   1.73|       6.95|  11.36|  12.65|      16.96|  37.97|   7.14|
## Step 2: Scale the  data

```r
ProjectDatafactor_scaled = apply(ProjectDataFactor, 2, function(r) {
    if (sd(r) != 0) 
        res = (r - mean(r))/sd(r) else res = 0 * r
    res
})
```
Notice now the summary statistics of the scaled dataset:


```r
knitr::kable(round(my_summary(ProjectDatafactor_scaled), 2))
```



|          |   min| 25 percent| median| mean| 75 percent|  max| std|
|:---------|-----:|----------:|------:|----:|----------:|----:|---:|
|X1..CRIM  | -1.72|      -0.86|   0.00|    0|       0.86| 1.73|   1|
|X2..ZN    | -0.49|      -0.49|  -0.49|    0|       0.05| 3.80|   1|
|X3..INDUS | -1.56|      -0.87|  -0.21|    0|       1.01| 2.42|   1|
|X4..CHAS  | -0.27|      -0.27|  -0.27|    0|      -0.27| 3.66|   1|
|X5..NOX   | -1.46|      -0.91|  -0.14|    0|       0.60| 2.73|   1|
|X6..RM    | -3.88|      -0.57|  -0.11|    0|       0.48| 3.55|   1|
|X7..AGE   | -2.33|      -0.84|   0.32|    0|       0.91| 1.12|   1|
|X8..DIS   | -1.27|      -0.80|  -0.28|    0|       0.66| 3.96|   1|
|X9..RAD   | -0.98|      -0.64|  -0.52|    0|       1.66| 1.66|   1|
|X10..TAX  | -1.31|      -0.77|  -0.46|    0|       1.53| 1.80|   1|
|X11..PTRA | -2.70|      -0.49|   0.27|    0|       0.81| 1.64|   1|
|X12..B    | -3.90|       0.20|   0.38|    0|       0.43| 0.44|   1|
|X13..LSTA | -1.53|      -0.80|  -0.18|    0|       0.60| 3.55|   1|

## Step 3:  Check correlations 
Check for correlation


```r
thecor = round(cor(ProjectDataFactor),2)
colnames(thecor)<-colnames(ProjectDataFactor)
rownames(thecor)<-colnames(ProjectDataFactor)

knitr::kable(round(thecor,2))
```



|          | X1..CRIM| X2..ZN| X3..INDUS| X4..CHAS| X5..NOX| X6..RM| X7..AGE| X8..DIS| X9..RAD| X10..TAX| X11..PTRA| X12..B| X13..LSTA|
|:---------|--------:|------:|---------:|--------:|-------:|------:|-------:|-------:|-------:|--------:|---------:|------:|---------:|
|X1..CRIM  |     1.00|  -0.56|      0.72|     0.08|    0.79|  -0.27|    0.66|   -0.67|    0.74|     0.73|      0.34|  -0.38|      0.55|
|X2..ZN    |    -0.56|   1.00|     -0.53|    -0.04|   -0.52|   0.31|   -0.57|    0.66|   -0.31|    -0.31|     -0.39|   0.18|     -0.41|
|X3..INDUS |     0.72|  -0.53|      1.00|     0.06|    0.76|  -0.39|    0.64|   -0.71|    0.60|     0.72|      0.38|  -0.36|      0.60|
|X4..CHAS  |     0.08|  -0.04|      0.06|     1.00|    0.09|   0.09|    0.09|   -0.10|   -0.01|    -0.04|     -0.12|   0.05|     -0.05|
|X5..NOX   |     0.79|  -0.52|      0.76|     0.09|    1.00|  -0.30|    0.73|   -0.77|    0.61|     0.67|      0.19|  -0.38|      0.59|
|X6..RM    |    -0.27|   0.31|     -0.39|     0.09|   -0.30|   1.00|   -0.24|    0.21|   -0.21|    -0.29|     -0.36|   0.13|     -0.61|
|X7..AGE   |     0.66|  -0.57|      0.64|     0.09|    0.73|  -0.24|    1.00|   -0.75|    0.46|     0.51|      0.26|  -0.27|      0.60|
|X8..DIS   |    -0.67|   0.66|     -0.71|    -0.10|   -0.77|   0.21|   -0.75|    1.00|   -0.49|    -0.53|     -0.23|   0.29|     -0.50|
|X9..RAD   |     0.74|  -0.31|      0.60|    -0.01|    0.61|  -0.21|    0.46|   -0.49|    1.00|     0.91|      0.46|  -0.44|      0.49|
|X10..TAX  |     0.73|  -0.31|      0.72|    -0.04|    0.67|  -0.29|    0.51|   -0.53|    0.91|     1.00|      0.46|  -0.44|      0.54|
|X11..PTRA |     0.34|  -0.39|      0.38|    -0.12|    0.19|  -0.36|    0.26|   -0.23|    0.46|     0.46|      1.00|  -0.18|      0.37|
|X12..B    |    -0.38|   0.18|     -0.36|     0.05|   -0.38|   0.13|   -0.27|    0.29|   -0.44|    -0.44|     -0.18|   1.00|     -0.37|
|X13..LSTA |     0.55|  -0.41|      0.60|    -0.05|    0.59|  -0.61|    0.60|   -0.50|    0.49|     0.54|      0.37|  -0.37|      1.00|

Apply the facto analysis, set with eingvalue


```r
UnRotated_Results<-principal(ProjectDataFactor, nfactors=ncol(ProjectDataFactor), rotate="none",score=TRUE)
UnRotated_Factors<-round(UnRotated_Results$loadings,2)
UnRotated_Factors<-as.data.frame(unclass(UnRotated_Factors))
colnames(UnRotated_Factors)<-paste("Component",1:ncol(UnRotated_Factors),sep=" ")
```


```r
Variance_Explained_Table_results<-PCA(ProjectDataFactor, graph=FALSE)
Variance_Explained_Table<-Variance_Explained_Table_results$eig
Variance_Explained_Table_copy<-Variance_Explained_Table


rownames(Variance_Explained_Table) <- paste("Component", 1:nrow(Variance_Explained_Table))
colnames(Variance_Explained_Table) <- c("Eigenvalue", "Pct of explained variance", "Cumulative pct of explained variance")

knitr::kable(round(Variance_Explained_Table, 2))
```



|             | Eigenvalue| Pct of explained variance| Cumulative pct of explained variance|
|:------------|----------:|-------------------------:|------------------------------------:|
|Component 1  |       6.51|                     50.11|                                50.11|
|Component 2  |       1.38|                     10.59|                                60.70|
|Component 3  |       1.19|                      9.13|                                69.83|
|Component 4  |       0.86|                      6.58|                                76.41|
|Component 5  |       0.84|                      6.44|                                82.85|
|Component 6  |       0.65|                      5.04|                                87.89|
|Component 7  |       0.41|                      3.16|                                91.05|
|Component 8  |       0.33|                      2.52|                                93.56|
|Component 9  |       0.23|                      1.79|                                95.35|
|Component 10 |       0.22|                      1.69|                                97.04|
|Component 11 |       0.18|                      1.38|                                98.42|
|Component 12 |       0.14|                      1.09|                                99.52|
|Component 13 |       0.06|                      0.48|                               100.00|


```r
eigenvalues  <- Variance_Explained_Table[, "Eigenvalue"]
df           <- cbind(as.data.frame(eigenvalues), c(1:length(eigenvalues)), rep(1, length(eigenvalues)))
colnames(df) <- c("eigenvalues", "components", "abline")
ggplot(melt(df, id="components"), aes(x=components, y=value, colour=variable)) + geom_line()
```

![plot of chunk figure01](figure/figure01-1.png)




```r
Rotated_Results<-principal(ProjectDatafactor_scaled, nfactors=max(3), rotate=rotation_used,score=TRUE)
Rotated_Factors<-round(Rotated_Results$loadings,2)
Rotated_Factors<-as.data.frame(unclass(Rotated_Factors))
colnames(Rotated_Factors)<-paste("Component",1:ncol(Rotated_Factors),sep=" ")

sorted_rows <- sort(Rotated_Factors[,1], decreasing = TRUE, index.return = TRUE)$ix
Rotated_Factors <- Rotated_Factors[sorted_rows,]

knitr::kable(Rotated_Factors)
```



|          | Component 1| Component 2| Component 3|
|:---------|-----------:|-----------:|-----------:|
|X7..AGE   |        0.81|        0.27|        0.07|
|X5..NOX   |        0.76|        0.49|        0.03|
|X3..INDUS |        0.69|        0.48|        0.22|
|X1..CRIM  |        0.66|        0.60|        0.09|
|X13..LSTA |        0.55|        0.30|        0.54|
|X10..TAX  |        0.35|        0.84|        0.23|
|X4..CHAS  |        0.34|       -0.11|       -0.53|
|X9..RAD   |        0.29|        0.86|        0.17|
|X11..PTRA |        0.15|        0.31|        0.64|
|X12..B    |       -0.09|       -0.67|       -0.07|
|X6..RM    |       -0.34|        0.05|       -0.77|
|X2..ZN    |       -0.77|       -0.02|       -0.25|
|X8..DIS   |       -0.84|       -0.30|        0.00|

Here is the result of 3 principal factoor chosen 


|          | Component 1| Component 2| Component 3|
|:---------|-----------:|-----------:|-----------:|
|X7..AGE   |        0.81|          NA|          NA|
|X5..NOX   |        0.76|          NA|          NA|
|X3..INDUS |        0.69|          NA|          NA|
|X1..CRIM  |        0.66|        0.60|          NA|
|X13..LSTA |        0.55|          NA|        0.54|
|X10..TAX  |          NA|        0.84|          NA|
|X4..CHAS  |          NA|          NA|       -0.53|
|X9..RAD   |          NA|        0.86|          NA|
|X11..PTRA |          NA|          NA|        0.64|
|X12..B    |          NA|       -0.67|          NA|
|X6..RM    |          NA|          NA|       -0.77|
|X2..ZN    |       -0.77|          NA|          NA|
|X8..DIS   |       -0.84|          NA|          NA|


Component nr 1 low status houses:

1. CRIM      per capita crime rate by town
3. INDUS     proportion of non-retail business acres per town
7. AGE       proportion of owner-occupied units built prior to 1940
5. NOX       nitric oxides concentration (parts per 10 million)
13. LSTAT    % lower status of the population
 
 
 Component nr 2 : hight value


