# Insurance-Economic-Capital
#Insurance #Loss_Distribution #Distribution_fitting #VaR #Economic_Capital

This project intends to determine the Economic Capital in the Insurance sector. This can be done by calculating the Expected Loss and the VaR of the compound distribution function.

Expected Loss refers to the amount of loss that the insurance company can easily cover it with the issued premium. Hense, it can be compute by multiplying the average loss frequency and severity. Frequency is the number of claims during a defined period while Severity is the average financial amount of each claim. As frequency refers to numbers, it can be moeled using discrete distributions (here "Poisson" & "Negative Binomial" was used), however, for severity, the continuous distributions were applied. Note that the insurance loss and consequently the severity distribution are almost skewed so in this project to model the severity, "Lognormal", "Exponential", "Weibull" and "Gamma" distributions were implemented.

