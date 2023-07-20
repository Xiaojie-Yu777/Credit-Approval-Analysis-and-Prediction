# Team-12
 Team 12's group project GitHub repository for MGT 6203 (Edx) Summer of 2023 semester.
# Credit Approval Analysis and Prediction

## Project Structure


### [Code](Code/)
This folder contains the individual code contributions from the team

### [Data](Data/)
UCI Credit Approval dataset ([here](https://www.kaggle.com/datasets/samuelcortinhas/credit-card-approval-clean-data?select=clean_dataset.csv)):  The main dataset is the Credit Approval dataset taken from the archives of the machine learning repository of the University of California. It contains data from credit card applications in the US. The data is collected from the credit card application in the US. 
There are 15  independent variables in the 1st dataset (below). There is one  dependent variable: Approved. We think the most important factors in credit decisions are: credit score, prior default and employment status. The dataset is very clean so we don’t expect to do a lot of cleaning. However, there are several categorical variables, such as ethnicity, we might have to create dummy variables during our modeling process.

### [Final Report](Final%20Report)
Final report contains the collective analysis and prediction models built by the team. Following are some of the topics covered in the final report:

* Data Considerations
* Visual Data Analysis
* Exploratory Data Analysis
* Feature Selection
* PCA Transformation
* VIF Analysis
* Data Manipulations in preparation for building analytical models
* Logistic Regression Model
* Neural Network Model

## Setting up Machine to run Final Code

Install R from the official [website](https://cran.r-project.org/bin/windows/base/)

Install Anaconda distribution of python from [here](https://conda.io/projects/conda/en/latest/user-guide/install/index.html)

Launch terminal/command line in your machine and run the below command to install jupyter notebook

<pre>
conda install jupyter 
</pre>

Now Launch R interactive terminal and run the following code

<pre>
install.packages('IRkernel')
IRkernel::installspec() 
</pre>

The above code will make the R kernel available in jupyter notebook so we can execute R code in jupyter notebooks.

If the you prefer to watch a video about this setup process please follow the below links:

[MacOS](https://www.youtube.com/watch?v=2PhS5JMVpf0)

[Windows](https://www.youtube.com/watch?v=4hYdeMZX8zc)