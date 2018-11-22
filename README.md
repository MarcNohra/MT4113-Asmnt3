# MT4113-Assignment3
## ID: 180024947

### Running The Code
To run the code start by running the 1_DataPrep.R file chronologically.  
Then the different scenarios interface is in the 2_RunScenarios.R  

### Notice
I used the gridExtra, foreach, doParallel and truncnorm libraries.  
I included calls to the package install in the code.  
I sourced all the required files, so there is no need to run the functions beforehand.  

### Understanding the code
#### 1_DataPrep.R
I start by cleaning and preparing the dataset.  
I created 2 groups of countries. "R" and "P" for rich and poor countries.  
I chose the 3rd quantile as a baseline to assign the groups to the countries.  
I draw some histograms to understand the data distributions.  
I draw a boxplot to check for the differences in means between the groups.  
I summarise my data by calculating the mean, sd, min and max for every group.  This summary will be used as properties for the rest of the code therefore it has to be loaded to the general environment.  
I then check for 2 assumptions using different tests.  

#### 2_RunScenarios.R
This file is like an interface design pattern, for every scenario, start by defining the variables. The sampleSizes, variances, effectSizes... lists can take any number of integers. The scenarios.run function will call all the necessary functions to return a list of power/size for both the parametric and non-parametric tests. The returned variable can then be used as a parameter in the scenarios.plot. This function will draw 2 plots for each scenario.  
If for example you run scenario 1 and then scenario 2, if you decide to draw the plot again for scenario 1, please run the Initialise Parameters section for the specific scenario since I use the same variables for all the scenario (they will be overwritten).  
In the scenario 4 section, I added a ggplot code that shows the variation of power with the effect size.  

#### PerformanceTesting.R
I this file I added some profiling when I was testing for the parallel improvements. I saved the first profile in the profile folder.  

#### The other code files
The code files don't have to be run before running the code since I sourced them in the 1_DaraPrep.R

### Folders
In the FIGURES folder I added some images used in the report.  
In the PROFILING folder I added 1 profile of the first basic scenario function in order to check for possible performance improvements.  
In the REPORTS folder I added the report in a PDF verison as well as in .pages extension (original program used to write it). I also added a image of the Diagram.  
In the Documentation folder I added a documentation for one function. To see it in a proper way, click on the preview button.

### Possible Improvements
Due to a lack of time I did have time to write all of my input checks, so a possible improvement would be adding them to all the necessary functions.  
I could also add documentation files for all the created functions in the Documentation folder.  

