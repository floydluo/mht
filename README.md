## Project Description

You can view this project report in: https://nbviewer.jupyter.org/github/floydluo/mht/blob/master/MulHypoTest.ipynb

**1. Project Description**

This is a group project in course **STA4010 Selected Topics in Statistics I** delivered by Prof. Anna Choi (Stanford) and Prof. Ka Wai Tsang (CUHKSZ) in 2017 fall semester at CUHKSZ.

This courses topics include: Multiple Hypotheses Testing, Family-wise Error rate (FWER), False Discovery Rate (FDR), Resampling methods, Bayesian Analysis, Time Series Analysis, Survival Analysis, etc.

*Project Question and Requirements*

This project uses the data set [**Project_data.csv**](https://github.com/floydluo/mht/blob/master/Project_data.csv) provided in class. The dataset contains the log returns of S&P500 and 100 stocks in US market from Sept. 7, 2016 to Sept 7, 2017. The goal is to identify which stocks have the Jensen index greater than 0. We assume that the returns of each particular stocks are independent and the interest rate is 0.016 (annual). Note that the returns among the stocks can be correlated.

​	(1) Identify the stocks with positive Jensen Index with $FWER \leq 0.05$

​	(2) Explain your methods and why they could control $FWER \leq 0.05$

**2. Methods and Tools**

**Statistical Methods**

1. p-values from Simple Linear Regression 
2. Closed Testing Procedure
3. Holm's Step-down Procedure
4. Hochberg's Step-up Procedure
5. Tests for Independence
6. [Jonathan Taylor's Conditional p-values for Hypothesis Test](https://pdfs.semanticscholar.org/f775/729a65da59b2e503cb2e7479f9a2a681fa51.pdf)
7. Tests for Normality
8. Hybrid Resampling Method

**Language and Platforms**

1. This Project was developed with R kernel in Jupyter Notebook. 
2. You can access the whole project source code and data via: https://github.com/floydluo/mht

**3. Group Members and Credits**

Among the five members, Zhen LIN and I (Junjie LUO) proposed the statistical ideas. Junjie LUO completed the R codes and made this project report.

