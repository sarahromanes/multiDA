multiDA [![Build Status](https://travis-ci.org/sarahromanes/multiDA.svg?branch=master)](https://travis-ci.org/sarahromanes/multiDA)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) 
  <img src="man/figures/test_logo.png" align="right"  height="250" width="250"/>
======================================================

High Dimensional Discriminant Analysis using Multiple Hypothesis Testing

Overview
--------

**multiDA** is a Discriminant Analysis (DA) algorithm capable for use in high dimensional datasets, providing feature selection through multiple hypothesis testing. This algorithm has minimal tuning parameters, is easy to use, and offers improvement in speed compared to existing DA classifiers.

**Publication to appear in JCGS. See our preprint - available on arXiv, [here](https://arxiv.org/pdf/1807.01422).**

This package is part of a suite of discriminant analysis packages we have authored for large-scale/complex datasets. See also our package [genDA](http://github.com/sarahromanes/genDA), a statistical ML method for Multi-distributional Discriminant Analysis using Generalised Linear Latent Variable Modelling.

Installation
--------

```r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("sarahromanes/multiDA")

```


Usage
-----

The following example trains the multiDA classifier using the SRBCT dataset, and finds the resubstitution error rate. 

```r
vy   <- SRBCT$vy
mX   <- SRBCT$mX
res  <- multiDA(mX, vy, penalty="EBIC", equal.var=TRUE, set.options="exhaustive")
vals <- predict(res, newdata=mX)$vy.pred          #vy.pred returns class labels
rser <- sum(vals!=vy)/length(vy)

```

A case study and overview of the statistical processes behind multiDA can be found [here](https://sarahromanes.github.io/multiDA/articles/multiDAvignette_caseStudy.html).

## Authors

* **Sarah Romanes**  - [@sarah_romanes](https://twitter.com/sarah_romanes)
* **John Ormerod**   - [@john_t_ormerod](https://twitter.com/john_t_ormerod)

## License

This project is licensed under the GPL-2 license.

## Acknowledgements

I am grateful to everyone who has provided thoughtful and helpful comments to support me building my first package - especially members of the Sydney University Statistical Bioinformatics group and also the NUMBATS group at Monash University. You guys rock!
