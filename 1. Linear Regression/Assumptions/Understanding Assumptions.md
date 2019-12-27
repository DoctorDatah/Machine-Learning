# **5 Assumptions:** 
- Linearity 
- No endogeneity of Regressors 
- Normality and homosecedasiticity of Errors
- No Auto-Correlation
- No Multi-colinearity

**Mistake:** 
Perform Regression which violates one of these Assumptions

## **1.  Linearity**<br>
Each Predictor should have linear relationship with the response variable. <br>

**How to Check?**<br> 
We can scatter plot plot each predictor against the response to check the Linear Structure

**Response Vs Predictor**
- Partial Regression Plot
- Partial Residual Plot<br>
_For more refer the these techniques here:_
https://github.com/DoctorDatah/Regression-Analysis-R/blob/master/Theory/Assumptions/Structure%20of%20the%20Model%20.md

**Fixes:**<br>
- Run a non Linear Regression
- Exponential Transformation 
- Log Transformation

## 2. No endogeneity of Regressors   ##
Correlation of Error and Predictors should be zero.<br>
**cov(error, predictor) = 0**

**How and Why?** <br>

Endogeneity can caused by **Omitted Variable Bias**. <br>
When we do not add a variable that is significant then it leads toward the endogeneity.

**Some Good Examples:**
https://towardsdatascience.com/endogeneity-the-reason-why-we-should-know-about-data-part-i-80ec33df66ae

**Fix:** Only Experience and Advance Knowledge on the Subject can Help

## 3. Normality and homosecedasiticity of Errors ##
**a. Normality**
Error for Normal Distributed. <br>
T-test and F-Test work because we have assumed normality. <br>

**What if Error are not Normally Distributed?**
CLI - Central Limit Theorem
For Large samples the CLI applies for error terms too. So we can Consider normality given for us. 

*For in case of non-normality we can consider permutation, refer:* ( will add link when upload related content 

Checking normality using qqnorm in R: https://github.com/DoctorDatah/Regression-Analysis-R/blob/master/Theory/Assumptions/Normality%20of%20Errors.md


