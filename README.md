# Insurance Economic Capital


This project intends to determine the Economic Capital in the insurance sector. This was done by computing value at risk (VaR) with the use of Normal Power (NP) approximation.

From a solvency point of view, VaR for a confidence level α is the value of loss such that the probability that the loss is greater than this value is at most 1- α.

As we may alreay know, VaR is a risk measure and here the risk is the underwriting risk; the total cost in a non-life insurance portfolio (S) arising from the sumation of each individual claim amount (iid - independent and identically distributed) from 1 to N (the total number of claims).

![image](https://user-images.githubusercontent.com/77916301/179453731-4c91bc5e-7588-4abc-baef-bd36e1eb4854.png)

The total claim amount (S) follows a compound distribution. Compound distributions do not have closed expressions of VaR and that's where the NP approximation comes to ease our way. Let X be a continuous random variable, its VaR at a confidence level α using the NP approximation can be shown by following formula.

![image](https://user-images.githubusercontent.com/77916301/179452523-7b8902e5-6090-4c65-b345-5f34321262db.png)


The difference between the VaR and the Expected Loss is quite important for the insurance companies. Expected Loss refers to the amount of loss that the insurance company can easily cover it with the issued premium. Hense, it can be compute by multiplying the average loss frequency and severity. Frequency is the number of claims during a defined period and it can be modeled using discrete distributions (here Poisson & Negative Binomial), however, for severity, the continuous distributions were applied. Note that the insurance loss and consequently the severity distribution are almost skewed so in this project to model the severity, Lognormal, Exponential, Weibull, Gamma, etc. were implemented. 

The difference between the compound distribution VaR and Expecte Loss is called Economic Capital.

![Pic Small](https://user-images.githubusercontent.com/77916301/179454748-e8b84645-b5b0-4d5f-a8e7-9cae31b836cf.jpeg)


#Insurance #Loss_Distribution #Distribution_fitting #VaR #Economic_Capital #Normal_Power_Approximation
