Copyright@ Albert C. Bemmaor, ESSEC Business School, Cergy-Pontoise, France, and Li Zheng, Leeds U. Business School, University of Leeds, UK, 2018. 

On this site, you can find:

(i) a synthetic data set which has been created from the estimates of the Bass model for the US (plus random error): Xt = Nt/10^6 - Nt-1/10^6 + Std Normal deviate with mean 0 and SD = 0.1 where Nt is the number of unique and active MSN users in month t.

(ii) the code in R that was used in the manuscript to compute the measures of forecasting accuracy of the four diffusion models + model with naive trend and model with seasonality

(iii) the output of the code for the synthetic data set (.csv). There is a total of 18 time horizons (i.e., from one month ahead to 18-month ahead forecasts) with samples sizes that vary from 18 to 35 absolute percentage errors (ape's).

(iv) the nonlinear least square parameter estimates of the four models for the synthetic data set (.pdf).They are based on the 67 observations.

(v) myfunctions.R and utilities.R that are useful for running the code. They both need to be uploaded along with the .csv data file.

Possible extensions: The user can create another synthetic data set with a chasm in adoptions by using G/SG (alpha = 0.20 for example) and adding noise.

Reference: Bemmaor, Albert C. and Li Zheng (2018), "The diffusion of mobile social networking: Further study," Working Paper, ESSEC Business School, Cergy-Pontoise.