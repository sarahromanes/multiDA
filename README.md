multiDA <img src="man/figures/test_logo.png" align="right"  height="250" width="217"/>
======================================================

High Dimensional Discriminant Analysis using Multiple Hypothesis Testing

Overview
--------

**multiDA** is a Discriminant Analysis (DA) algorithm capable for use in high dimensional datasets, providing feature selection through multiple hypothesis testing. This algorithm has minimal tuning parameters, is easy to use, and offers improvement in speed compared to existing DA classifiers.



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
data(SRBCT)
vy   <- SRBCT$vy
mX   <- SRBCT$mX
res  <- multiDA(vy, mX, penalty="GIC-4", equal.var=TRUE, set.options="exhaustive")
vals <- predict(res, newdata=mX)$vy.pred          #vy.pred returns class labels
rser <- sum(vals!=vy)/length(vy)

```

## Authors

* **Sarah Romanes** - *Initial work* - [sarahromanes](https://github.com/sarahromanes)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone who's code was used
* Inspiration
* etc
