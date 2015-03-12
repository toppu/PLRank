PLRank: An R Package for Label Ranking
======================================

PLRank overview
---------------

PLRank is a collection of general purpose functions that provide a flexible set of
tools for applying a wide range of label ranking methods based on
Plackett-Luce model. PLRank includes evaluation
methods, datasets and new algorithms I developed in my master thesis,
"Generalized Plackett-Luce Model for Label Ranking", under the direction of [Dr.Giorgio Corani](http://people.idsia.ch/~giorgio/) and [Dr.Alessandro Antonucci](http://people.idsia.ch/~alessandro/)
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

Using PLRank
------------

Many examples concerning optimization tasks are provided in this
section. In particular, we will present the optimization of well-known
benchmark mathematical functions and label ranking problems in general.
Hereafter, we assume that PLRank package is already installed and
loaded in the current R session, for example by entering the following
command:

R> library("PLRank")
        

Function Generating 1-Vase Artificial Dataset
---------------------------------------------

We start by creating an artificail dataset from PL1 model. This is
useful for performing inference tasks for ranking problems. Figure
\ref{fig:R-genrank1V}, $\$rank$ is the rankings randomly drawn from the
known parameters $\$para$

    #Usage
    genRank1v(nLabels, nObs)

    #Arguments
    nLabels number of labels to rank
    nObs number of observations

    #Examples
    Create artificial dataset with 4 labels to rank and 10 instances
    R> lables = 4
    R> observations = 10
    R> genRank1v(lables, observations)
        

\centering![Sample of 1-vase artificial dataset.<span
data-label="fig:R-genrank1V"></span>][]

[^1]: https://github.com/toppu/PLRank

[^2]: http://cran.r-project.org/web/packages/pmr/index.html

[^3]: https://www.uni-marburg.de/fb12/kebi/research/repository/labelrankingdata

  [Screenshot of PLRank.<span data-label="fig:ibplta"></span>]: ./figures/PLRank_help.png
    "fig:"
