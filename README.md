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
    
![R-genrank1V](https://github.com/toppu/PLRank/blob/master/images/R-genrank1V.png)


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
