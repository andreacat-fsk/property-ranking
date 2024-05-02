# property-ranking
Ordinal Logistic Regression Model Fit for ranking properties on Booking.com.

In this repository, I try to find what are the features of a property that have the most significant effect on its ranking on a leading OTA platform like Booking.com.

To this purpose, I fitted an ordinal logistic regression model to a dataset of 1000 properties which popped up on a first-time search of a new user on a new browser. In this way, the results are actually those given as a cold start to a new user. Infact, my goal was to see how much visible regressors on the website affect ranking, compared to 'invisible' regressors like views, clicks and time spent on a specific page. 

Results are surprising, as much of variability may actually be explained by visible regressors.

To further test this assumption, it may be useful to test the fit on new datasets and check the predicting power of the model.
