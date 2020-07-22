# Capstone Project_Certificate in Data Analytics, Big Data, and Predictive Analytics
This repository contains source code and related documents for my capstone project at Ryerson University. 
Project Abstract - CKME136

Toronto Crime Analysis and Prediction through Machine Learning

The Problem:

Prevent crime before it happens

Crime prediction has always been subject to continued research in many parts of the world and has particularly garnered interest in Canada. Crime prediction is a law-enforcement technique that uses data, and machine learning for the identification of crimes most likely to occur. Toronto is one of the most populous, ethnically diverse, and multicultural urban cities in Canada. The capstone project proposes to analyze crime and build predictive models to predict crime in Toronto. 

Key research questions

Predict the category of crimes that occurred in Toronto
Explore the dataset and conduct an analysis to understand crime trends and answer key questions such as :
Top crimes committed by type - number and trends
Understand crime hotspots in Toronto
How has the number of various crimes changed over time in Toronto?
Are there any trends in the crimes being committed?
Which crimes are most frequently committed?
Which locations are these frequent crimes being committed to?
What are the safest neighbourhoods in Toronto?


The dataset

For this capstone project, Toronto crime data for the past 6 years i.e 2014-2019 is made available and open by the Toronto Police Service public safety data portal. Link to this dataset can be found here: http://data.torontopolice.on.ca/datasets/mci-2014-to-2019/data

The raw data is also saved as an Rdata file in this repository - see filename (capstonedataset.Rdata)

To load the object from the file use load("capstonedataset.Rdata") in R

The Major Crime Indicators categories are Assault, Break and Enter,  Auto Theft, Robbery and Theft Over (Excludes Homicides and Sexual Assaults).

The techniques proposed

A supervised prediction technique, namely, classification can be utilized to create a predictive model that can accurately predict the category of crimes in Toronto. The techniques will also cover to include pattern identification, prediction, and visualization. In addition, we shall also compare other algorithms such as KNN Classifier, Naive Bayes and Random Forest to understand how these perform when it comes to predicting crimes in Toronto.

Some notes about the files in this repository:
CKME136-Vidyasankar_Sundar_Capstone2020_Final_Report.PDF (Contains the Final Project Report)

Filename EDA_Crime.R (Contains source code to perform exploratory data analysis)

Filename EDA_Crime_Toronto.Rmd (Contains the RMarkDown - provides the code and the insights and can be used to generate the exploratory data analysis report in various formats - see 3 to 5)

Filename EDA_Crime_Toronto.docx (Contains the Exploratory data analysis report exported in word format)

Filename EDA_Crime_Toronto.html (Contains the Exploratory data analysis report exported as a html output)

Filename EDA_Crime_Toronto.PDF (Contains the Exploratory data analysis report exported in PDF format)

Filename Exploratory Analysis Results No Code.PDF(Contains the results of the exploratory work with no code embedded in the document)

Filename EDAdrop.Rdata - contains the dataset further to manually eliminating irrelevant variables from the dataset. Dropped the variables (14) that are not meaningful. This includes the following #Index, event unique id, occurrence date and reported date in unix formats,ucr_code, ucr_ext, offence, reported year, month, day, day of year, day of week, hour, and objectid.This file was subsequently used for featureselection to identify key variables for training models. 

Filename EDAfilter.Rdata - contains the crimedataset exported from the open data provided by the Toronto Police Service.

Filename MCI_Metadata.csv - details of various variables as provided by the Toronto Police Service

Filename MCI_Shooting_Glossary.pdf - description of crime categories as provided by the Toronto Police Service

Filename fsmethods.R - contains the R source code to run information gain - a filter based feature selection method to extract the variables that are important for model building

Filename modelsfinal.R - contains the R source code to train/test models to predict crime categories in Toronto


