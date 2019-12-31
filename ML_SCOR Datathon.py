#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Dec 14 11:27:17 2019

@author: Tim
"""

import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
import sklearn 
from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold
from sklearn import model_selection
from sklearn import metrics
import os
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC


os.getcwd()
os.chdir("/Users/zhangmeng/Desktop/study/scor/test/2007-2008/")
Dataset = pd.read_csv('merged_data.csv')
print(Dataset.describe())
Dataset = Dataset.dropna(axis=0, subset=['MCQ160E'])
print(Dataset.head())
Dataset = Dataset.fillna(method ='bfill')

#Dataset.to_csv("/Users/zhangmeng/Desktop/study/scor/test/2007-2008/merged_data.csv")

#Plot 
# Dataset.plot(kind='box',subplots=True,layout=(110,110),sharex=False,sharey=False)
# Dataset.hist()
# pd.scatter_matrix(Dataset)
# plt.show()

#creat your train and test set
array = Dataset.values
X = array[:,:]
Y = Dataset["MCQ160E"]
print(Dataset.describe())
X_train, X_validation, Y_train, Y_validation = train_test_split(X,Y,test_size=0.2,random_state=1)


#test options nad evaluation metric
kfold = KFold(n_splits=10, random_state=1)
#cv_results = cross_val_score(model, X_train, Y_train, cv=kfold, scoring='accuracy'))

#Build Model
# Spot Check Algorithms
models = []
models.append(('LR', LogisticRegression(solver='liblinear',multi_class='ovr')))
models.append(('LDA', LinearDiscriminantAnalysis()))
models.append(('KNN', KNeighborsClassifier()))
models.append(('CART', DecisionTreeClassifier()))
models.append(('NB', GaussianNB()))
models.append(('SVM', SVC(gamma='auto')))
# evaluate each model in turn
results = []
names = []
for name, model in models:
    kfold = KFold(n_splits=10, random_state=1)
    cv_results = model_selection.cross_val_score(model, X_train, Y_train, cv=kfold, scoring='accuracy')
    results.append(cv_results)
    names.append(name)
    msg = "%s: %f (%f)" % (name, cv_results.mean(), cv_results.std())
    print(msg)
#Predict & Evaluate
model = SVC(gamma = 'auto')
### OR model = GaussianNB()
model.fit(X_train, Y_train) #my training set
predicttion = model.predict(X_validation)

#Evaluate predictions
print(metrics.accuracy_score(Y_validation,predicttion))
print(metrics.confusion_matrix(Y_validation,predicttion))
print(metrics.classification_report(Y_validation,predicttion))

