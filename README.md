# AIDA
This project consists of a <b>Statistical purpose analysis on the Italian firms</b>, it borns with an educational aim for the 
[Statistical Methods for Data Science](http://didawiki.cli.di.unipi.it/doku.php/mds/smd/start) course(A.Y. 2017-2018) 
at the Università di Pisa.

We've been analyzing Italian firms by trying to answer very common claims in the economical/statistical field, that are:
- what's the measure of size that best describes the firms size;
- quantifying the correlation between different measures of the firms size;
- how are the firms sizes distributed;
- how is the firms growth distributed;
- is the mean growth statistically different from zero;
- is the growth distribution symmetric or asymmetric.

Then we've also tried to distinguish the behaviours within distinct subsamples of the whole dataset, such as: distinct subsectors, distinct years, distinct firms sizes.



## Tools and Technologies used
All the analysis have been done with <b>R</b> (version 3.5.0). 

To perform useful operations on our data we've used the <b>dplyr</b> package; for power law distribution we've used <b>poweRlaw</b> library. For plotting we've mostly used <b>ggplot</b> package.

## Files description
A brief description of the distinct directories and files you may find in this repository:
- the <b>data</b> directory contains RData files that refer to our original data.
- the <b>files</b> directory contains:
  - <b>distrResults</b> which contains all the RData files for the results of fitted distributions on distinct (sub)samples;
  - <b>images</b> which contains all the images of plotting, CIs etc.
- <b>utils.R</b> is an R file for very general utilities(eg: loading needed packages, loading datasets into current workspace);
- <b>functions.R</b> is an R script that contains several useful functions for analysis purposes;
- <b>first_analysis.R</b> contains a very general analysis on the whole dataset, eg: basic statistics of the distinct features;
- <b>correlation.R</b> contains correlation analysis and linear regression for Employee and Revenue attributes;
- <b>test_distr.R</b> contains all the analysis done for Size distribution of the firms;
- <b>powerlaw.R</b> has been written to further analyze the power law hypothesis on the firms size by using Employee attribute;
- <b>growth_rate_dist.R</b> and all the remaining files which name starts by "growth"(one for each (sub)sample) contain analysis on the growth of the italian firms;
- <b>distributionResultsAnalysis</b> contains the results that we've obtained and thus analyzed from files contained in "files/distrResults" 
- <b>packages.txt</b> contains a list of the packages needed to perform the analysis.

For deeper and clearer explanations about the procedures and the results, please read our [final report](https://github.com/honestus/AIDA/blob/master/Results_Report.pdf).
