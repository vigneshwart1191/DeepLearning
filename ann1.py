# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os
os.chdir('C:/Users/vigneshwar.thiyagara/Documents/DataScience/DeepLearning/Data')


#Import the dataset
dataset = pd.read_csv('Churn_Modelling.csv')

#Partition dependent and independent variable
X = dataset.iloc[:,3:13]
Y = dataset.iloc[:,13]

#Covert categorical data to numerical
from sklearn.preprocessing import LabelEncoder,OneHotEncoder
#Label Encoding
le = LabelEncoder()
X.iloc[:,1] = le.fit_transform(X.iloc[:,1])
X.iloc[:,2] = le.fit_transform(X.iloc[:,2])
#One hot encoding
Onehotencoder = OneHotEncoder(categorical_features = [1])
X = Onehotencoder.fit_transform(X).toarray()
X = X[:,1:]

#Partititon train and test on both x and y
from sklearn.model_selection import train_test_split
X_train,X_test,Y_train,Y_test = train_test_split(X,Y,test_size = 0.2,random_state = 0)

#Data Normalization
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.fit_transform(X_test)

#Import keras and tensorflow
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Dropout
#Sequential will create hidden layers
classifier = Sequential()
#Dense will be helpful for creation of activation function and parameters tuning
#Creating 1st hidden layer
classifier.add(Dense(units = 6,kernel_initializer = 'uniform',activation='relu',input_dim=11))
#TO deal with overfitting use drouput which will disable nurons
#Creating 2nd hidden layer
classifier.add(Dense(units = 6,kernel_initializer = 'uniform',activation='relu'))
classifier.add(Dropout(noise_shape = 0.1))
#creating output layer
classifier.add(Dense(units = 1,kernel_initializer = 'uniform',activation='sigmoid'))
classifier.add(Dropout(noise_shape = 0.1))
#Compiling theANn
classifier.compile(optimizer='adam',loss = 'binary_crossentropy',metrics = ['accuracy'])

#Fit the model to train the data
classifier.fit(X_train,Y_train,batch_size = 10,epochs=100)


#Predict with testr data
y_pred = classifier.predict(X_test)
y_pred = (y_pred > 0.5)

#confusion matrix 
from sklearn.metrics import confusion_matrix
cm  = confusion_matrix(Y_test,y_pred)

#######Evaluating model and improving perfomance
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import cross_val_score
from keras.models import Sequential
from keras.layers import Dense
def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(units = 6,kernel_initializer = 'uniform',activation='relu',input_dim=11))
    classifier.add(Dense(units = 6,kernel_initializer = 'uniform',activation='relu'))
    classifier.add(Dense(units = 1,kernel_initializer = 'uniform',activation='sigmoid'))
    classifier.compile(optimizer='adam',loss = 'binary_crossentropy',metrics = ['accuracy'])
    return classifier

#Creating K-Folds
classifier = KerasClassifier(build_fn = build_classifier,batch_size = 10,nb_epoch = 100)
accuracies = cross_val_score(estimator = classifier,X = X_train,y = Y_train,cv = 10,n_jobs = -1)

mean = accuracies.mean()
variance = accuracies.std()

#Parameter tuning in ANN
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV
from keras.models import Sequential
from keras.layers import Dense
def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(units = 6,kernel_initializer = 'uniform',activation='relu',input_dim=11))
    classifier.add(Dense(units = 6,kernel_initializer = 'uniform',activation='relu'))
    classifier.add(Dense(units = 1,kernel_initializer = 'uniform',activation='sigmoid'))
    classifier.compile(optimizer='adam',loss = 'binary_crossentropy',metrics = ['accuracy'])
    return classifier

classifier = KerasClassifier(build_fn = build_classifier)
parameters = {'batch_size':[25,32],
              'nb_epoch' : [100,500],
              'optimizer': ['adam','rmsprop']}
grid_search = GridSearchCV(estimator = classifier,
                           param_grid = parameters,
                           scoring = 'accuracy',
                           cv = 10)
grid_search = grid_search.fit(X_train,Y_train)
best_parameters = grid_search.best_params_
best_accuracy = grid_search.best_score_