# Imbalanced Classification in R

This project comprises of .R files dealing with classification of imbalanced data, both binary and multiclass. It is tested on Default of Credit Card Clients dataset available at
[UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients) and Glass dataset available at mlbench package. 

The project is organized as follows:

* feature_selection.R: in this file we split the data into train and test dataset and then perform data preprocessing and feature selection.
* resampling.R: in this file we define functions used in resampling methods.
* cost_sensitive.R this file contains implementations of weighted logistic regression and weighted kNN model.
* ensemble.R: in this file we define ensembles and their upgrades used in imbalanced classification. Current implementations consider only binary target.
* main.R: in this file we train and test models and write results into .xlsx file.
* visualizations.R: in this file we create some useful visual interpretations of scenarios encountered in imbalanced classification.
