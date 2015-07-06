# PLRank: An R Package for Label Ranking

## PLRank overview

PLRank is a collection of general purpose functions that provide a flexible set of
tools for applying a wide range of label ranking methods based on
Plackett-Luce model. PLRank includes evaluation
methods, datasets and new algorithms I developed in my master thesis,
"[Generalized Plackett-Luce Model for Label Ranking](https://github.com/toppu/PLRank/blob/master/msThesis_SuttipongMungkala_20150120.pdf)", under the direction of [Dr.Giorgio Corani](http://people.idsia.ch/~giorgio/) and [Dr.Alessandro Antonucci](http://people.idsia.ch/~alessandro/)
from the [Swiss AI Lab IDSIA](http://ipg.idsia.ch/). Label
ranking datasets in PLRank consist of training examples of a target
function that has multiple binary target variables. In other words, each
item of a label ranking dataset is annotated by many labels (classes).
This is the nature of many real world problems such as web page
categorization, music categorization, direct marketing, and etc. The
typical usage scenario of PLRank would involve a machine learning
researcher performing an empirical evaluation of one or more label
ranking learning algorithms, based on one or more label ranking
datasets, and a machine learning practitioner building a label ranking
model using a training dataset and then applying it to a new (unlabeled)
dataset, in order to obtain predictions.

Currently PLRank includes algorithms for performing major
label ranking tasks and sample of datasets:

-   Label ranking datasets, [KEBI](http://www.uni-marburg.de/fb12/kebi/research/repository/labelrankingdata?language_sync=1) Data Repository hosted by the
    Philipps University of Marburg.

-   Artificial ranking datasets. The functions to generate ranking
    datasets.

-   Maximum likelihood estimation methods to estimate the parameters of
    Plackett-Luce model. Two [MLE](http://en.wikipedia.org/wiki/Maximum_likelihood) methods are available, [MM](http://en.wikipedia.org/wiki/MM_algorithm) and [NR](http://en.wikipedia.org/wiki/Newton%27s_method).

-   Evaluation methods for label ranking problems. Calculate a variety
    of evaluation measures through k-folds cross validation.

![R-PLRank_help](https://github.com/toppu/PLRank/blob/master/images/PLRank_help.png)

## Using PLRank

Many examples concerning optimization tasks are provided in this
section. In particular, we will present the optimization of well-known
benchmark mathematical functions and label ranking problems in general.
Hereafter, we assume that PLRank package is already installed and
loaded in the current R session, for example by entering the following
command:

    R> library("PLRank")

### Function Generating 1-Vase Artificial Dataset

We start by creating an artificail dataset from PL1 model. This is
useful for performing inference tasks for ranking problems. Figure
\ref{fig:R-genrank1V}, $\$rank$ is the rankings randomly drawn from the
known parameters $\$para$

Usage:

    genRank1v(nLabels, nObs)

Arguments:

    nLabels number of labels to rank
    nObs number of observations

Example: create artificial dataset with 4 labels to rank and 10 instances

    R> lables = 4
    R> observations = 10
    R> genRank1v(lables, observations)
    
![R-genrank2V](https://github.com/toppu/PLRank/blob/master/images/R-genrank2V.png)


### Function Generating 2-vase Artificial Dataset

Creating an artificial dataset from PL2 model.Figure
\ref{fig:R-genrank2V}, $\$rank$ is the rankings randomly drawn from the
known parameters $\$para1$ and $\$para2$, where the split point is
defined at the second position.

    #Usage
    genRank2v(nLabels, nObs, L)

    #Arguments
    nLabels number of labels to rank
    nObs number of observations
    L split position between the first vase and the second vase

    #Examples
    Create artificial dataset with 4 labels to rank and 10 instances
    R> lables = 4
    R> observations = 10
    R> L = 2
    R> genRank2v(lables, observations, L)
    
![R-genrank1V](https://github.com/toppu/PLRank/blob/master/images/R-genrank1V.png)

###  MM Method for Estimating One-Vase Plackett-Luce Model Parameters

This function uses the MM (minorization-maximization) algorithms to fit the Plackett-Luce model. The
method was rewritten from the original Matlab code (plackmm) provided by [Hunter, 2004].
The method was modified by adding a stop criteria to prevent the estimates from approaching
infinity. 

    #Usage
    est.PL.MM(dset)

    #Arguments
    dset ranking dataset

    #Examples
    Create 2 ranks and 3 l a bel s
    R> ranks = matrix (0 ,2 ,3) # rows : number of ranks, column : number of labels
    R> ranks [1 , ] = c (1 ,2 ,3) # L1>L2>L3
    R> ranks [2 , ] = c (3 ,1 ,2) # L3>L1>L2
    R> est.PL.MM(ranks)
    
![R-plmm](https://github.com/toppu/PLRank/blob/master/images/R-plmm.png)

### NR Method for Estimating Two-Vases Plackett-Luce Model Parameters

This function uses the NR algorithm to fit PL2 model. The function extends the original method
fitting the one-vase Plackett-Luce model from PMR package by adding the two-vases model
interpretation.

    #Usage
    est.PL2.NR( d se t , L )

    #Arguments
    dset ranking dataset
    L split point between the first vase and the second vase

    #Value
    Estimated parameters and log-likelihood
    Create 2 ranks and 3 l a bel s
    R> ranks = matrix (0 ,2 ,3) # rows : number o f ranks , column : number o f l a b el s
    R> ranks [1 , ] = c (1 ,2 ,3) # L1>L2>L3
    R> ranks [2 , ] = c (3 ,1 ,2) # L3>L1>L2
    R> est.PL2.NR( ranks , 1 )
    
![R-pl2](https://github.com/toppu/PLRank/blob/master/images/R-pl2.png)

### Evaluating Predictive Models On Label Ranking Dataset

This section illustrates the use of the cv.knn() function on two real datasets. The first one, Iris,
is a well-known dataset in ranking study, which consists of measurements in centimeters of the
variables sepal length and width and petal length and width, respectively, for each flowers from
each of 3 species (3 labels).

The second one, Eurovision, presented in Jacques and Biernacki (2012), consists of the votes
of European countries during the Euro- vision Song Contest. Both datasets are available in the
Rankcluster package, as well as the three other datasets analysed and described in Jacques and
Biernacki (2012): APA, quiz, sports.

Label ranking dataset consists of two main parts, the features and ranks. The latter forms
a set of preference labels. At the moment, each instance only contains a complete ranking.
Particularly, labels are assembled to a labek ranking by using the ">" sign. For example, L1 >
L2 means that label 1 is preferred over label 2. It is not allowed to state one single label inside
of an instance and thus there have to be at least two labels separated by the ">" character. The
label ranking dataset will be automatically avaialble in the R environment once the PLRank
package is loaded into the R session. For example, we can simply call the iris dataset by trying `R > data.iris` in the R console.

    

    #Usage
    cv.knn( dset , k = 10, nFolds = 10, nRuns = 10, alpha = 0.05)

    #Arguments
    dsetlabel ranking dataset
    k the number of nearest neighbors to search . The defaul k value is set to 10
    nFolds the number of folds for the cross validation
    nRuns the number of runs for the cross validation
    alpha the default value is set to 0.05

    #Examples
    Perform the 10 runs of 10 #folds cross validation on the Iris dataset
    dset = data.iris
    cv.knn(dset , k=15, nFolds=10, nRuns=10, alpha =0.05)
    
![R-cv](https://github.com/toppu/PLRank/blob/master/images/R-cv.png)


    
