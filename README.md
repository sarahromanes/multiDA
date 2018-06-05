multiDA <img src="man/figures/test_logo.png" align="right"  height="250" width="250"/>
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
vy   <- SRBCT$vy
mX   <- SRBCT$mX
res  <- multiDA(mX, vy, penalty="EBIC", equal.var=TRUE, set.options="exhaustive")
vals <- predict(res, newdata=mX)$vy.pred          #vy.pred returns class labels
rser <- sum(vals!=vy)/length(vy)

```

## Authors

* **Sarah Romanes**  - [@sarah_romanes](https://twitter.com/sarah_romanes)
* **John Ormerod**   - [@john_t_ormerod](https://twitter.com/john_t_ormerod)

## License

This project is licensed under the GPL-2 license - see the [LICENSE.md](LICENSE.md) file for details


