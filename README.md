PLRank: An R Package for Label Ranking
======================================

PLRank overview
---------------

In the this chapter, we describe the R package PLRank[^1] which is a
collection of general purpose functions that provide a flexible set of
tools for applying a wide range of label ranking methods based on
Plackett-Luce model. We have extended an existing method (pl.r) of
fitting Plackett-Luce model from PMR[^2] package. The PLRank package
includes the new algorithms we developed in Chapter 4, the evaluation
methods and datasets used in Chapter 5 and 6 of this thesis. Label
ranking datasets in the PLRank consist of training examples of a target
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

Currently the PLRank package includes algorithms for performing major
label ranking tasks and sample of datasets:

-   Label ranking datasets, KEBI[^3] Data Repository hosted by the
    Philipps University of Marburg. The properties of the datasets are
    mentioned in Table \ref{tab:data_uci}.

-   Artificial ranking datasets. The functions to generate ranking
    datasets.

-   Maximum likelihood estimation methods to estimate the parameters of
    PL1 and PL2 model. Two MLE methods are available, MM and NR.

-   Evaluation methods for label ranking problems. Calculate a variety
    of evaluation measures through k-folds cross validation.

\centering![Screenshot of PLRank.<span
data-label="fig:ibplta"></span>][]

Using PLRank
------------

Many examples concerning optimization tasks are provided in this
section. In particular, we will present the optimization of well-known
benchmark mathematical functions and label ranking problems in general.
Hereafter, we assume that the PLRank package is already installed and
loaded in the current R session, for example by entering the following
command:

[^1]: https://github.com/toppu/PLRank

[^2]: http://cran.r-project.org/web/packages/pmr/index.html

[^3]: https://www.uni-marburg.de/fb12/kebi/research/repository/labelrankingdata

  [Screenshot of PLRank.<span data-label="fig:ibplta"></span>]: ./figures/PLRank_help.png
    "fig:"
