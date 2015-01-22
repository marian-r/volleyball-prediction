Volleyball prediction
=====================

Usage of Machine Learning methods for prediction of volleyball games.

Classification
--------------

Prediction of winning teams.

Given the data set of **841 volleyball matches** between countries and various statistics I trained the models for predicting the winners of the matches. I used ~70:30 ratio of training and testing examples. Training part was further divided, again by 70:30, in order to test tune parameters of the models. I tried also cobination of the methods - via voting.

### Used models:

- Artificial Neural Networks
- Decision Trees
- Naive Bayes
- k-Nearest Neighbor algorithm
- Random Forests

### Best model: 
ANN, KNN, RF

- Classification accuracy: **68%**
- Brier Score: **0.43**

Artificail Neural Networks:

- Classification accuracy: **68%**
- Brier Score: **0.45**

Random Forests:

- Classification accuracy: **66%**
- Brier Score: **0.43**


Regression
----------

Predicting the duration of matches.

Given the same data set and statistics I trained a model to predict the duration of the matches. The division into training and testing examples, and parameter tunning was the same as in the classifiation part.

###Used models:

- Linear model
- Anrtificial Neural Networks
- k-Nearest Neighbor algorithm
- Random Forests

###Results

With the given data I was not able to train a useful model and the regression part hence was not successful. It turned out to be much harder task than the classification part. The values of RMAE (Relative Mean Absolute Error) were always around 1.
