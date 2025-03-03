# pseScript4_N5.R and pseScript4_N60 analyze data from the n=5 and n=60 experiments
# using mixed-effects models.  

# pseScript5_N5.R and pseScript5_N60 perform the same analyses EXCEPT the value of
# the standard duration was centered on 400, not zero. The reason for doing this is that
# the intercepts estimated from these analyses will correspond to the value of the pse
# at a standard duration of 400 msec rather than zero msec, which may be of more interest.


# The mixed-effects analyses are done using the nlme package which is described here:

# Pinheiro JC, Bates DM (2000). _Mixed-Effects Models in S and S-PLUS_. 
# Springer, New York. doi:10.1007/b98882 <https://doi.org/10.1007/b98882>

# Most of the analyses can be performed with lme4 and lmerTest. However,
# the nlme package has several commands for graphing data/results (e.g., plot(augPred(model)))
# that are quite useful. Also, the nlme package makes it easy to create a mixed-effects
# regression model which assumes that the variance of the within-subject residuals is NOT constant
# across levels of the covariate (the "standard" duration, in this case). It turns out that this
# type of heteroscedatic model fits the data significantly better.

# The Pinheiro & Bates book is available on-line from the library.
# The book includes clear examples of fitting models to various data sets, some of
# which are similar to yours. (It also includes technical descriptions of the statistical
# theory that underpins the software, but I haven't found those sections to be very useful.)
# Chapters 1 & 4 illustrate how to fit mixed-effects models to several
# data sets. Sections 1.4 & 4.2 are particularly relevant to your data.
# Section 4.3 illustrates commands that can be used to evaluate the goodness of
# fit. Finally, Chapter 5 describes how the framework can be extended to handle
# cases where the within-participant residuals are not constant variance and/or
# independent. My analyses have assumed independence but I did fit models that allowed
# the variance of the residuals to vary according to a power-function of abs(standard).
# Section 5.2.2 shows how to create a heteroscedatic model like the one I used.

# The folder "on-line-tutorials" includes pointers to several web pages that provide tutorials on mixed-effects models

