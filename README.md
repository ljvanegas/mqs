# mqs (Multiscale quantile segmentation)

We introduce a new methodology for analyzing serial data by quantile regression assuming that the underlying quantile function consists of constant segments. The procedure does not rely on any distributional assumption besides serial independence. It is based on a multiscale statistic, which allows to control the (finite sample) probability for selecting the correct number of segments S at a given error level, which serves as a tuning parameter. For a proper choice of this parameter, this tends exponentially fast to the true S, as sample size increases. We further show that the location and size of segments are estimated at minimax optimal rate (compared to a Gaussian setting) up to a log-factor. Thereby, our approach leads to (asymptotically) uniform confidence bands for the entire quantile regression function in a fully nonparametric setup. The procedure is efficiently implemented using dynamic programming techniques with double heap structures, and software is provided. Simulations and data examples from genetic sequencing and ion channel recordings confirm the robustness of the proposed procedure, which at the same hand reliably detects changes in quantiles from arbitrary distributions with precise statistical guarantees. 

## Reference

L. Vanegas, M. Behr, and A. Munk, *Multiscale Quantile Segmentation*, [Journal of the American Statistical Association](https://doi.org/10.1080/01621459.2020.1859380), 117(539):1384–1397 (2022)


[See also our manuscript on ArXiv.](https://arxiv.org/abs/1902.09321)

## Using mqs as an R package

The R package can be instaled using devtools. To install devtools you can use `install.packages('devtools')`.
Once devtools is installed, you can install the mqs package as follows:
```R
library(devtools)
devtools::install_github("ljvanegas/mqs")
```
You can then load the mqs package with `library(mqs)`.

An example on how to use mqs on simulated data can be found in the jupyter notebook mqs.ipynb.

## Contact

For further information or suggestions you can contact us at ljulava(at)gwdg(dot)de or merle.behr(at)ur(dot)de.
