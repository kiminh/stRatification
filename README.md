# stRatification package

## about this package

stRatification is developed to analyze the Average Treatment Effect of subgroup that is build based on the prediction result.
The original idea is introduced in [this](http://www.nber.org/papers/w19742) paper.
Especially, this package is aim to reproduce the Table2 in the above paper.
This package is still in developing, though you are able to reproduce the table 2 some degree.

## install

You are able to install with install_github function in devtools package.
```
devtools::install_github("yasui-salmon/stRatification")
```

## STAR dataset

The original dataset is possible to download from [here](https://ideas.repec.org/p/boc/bocins/webstar.html)

Also you can find some definitions of the column in those data in [here](http://wise.xmu.edu.cn/course/gecon/star_Description.pdf)


## STAR Example
You are able to find the example code and result [here](https://github.com/yasui-salmon/stRatification/blob/master/STAR_example/STAR_EXAMPLE.md), please check this before you go for your analysis.
Also the Rmd file of the above example is putted [here](https://github.com/yasui-salmon/stRatification/blob/master/STAR_example/STAR_EXAMPLE.Rmd)


## Reference

Abadie, Alberto, Matthew M. Chingos, and Martin R. West. Endogenous stratification in randomized experiments.
No. w19742. National Bureau of Economic Research, 2013.