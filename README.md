# titanic.predictor


random-forest predictor of whether a given passenger died or lived on the titanic. Solution for the associated [Kaggle challenge](https://www.kaggle.com/c/titanic).

#### to run:

1) make sure Rscript is installed on your computer. 
2) navigate to the project's root. 
3) download [the required data sets from Kaggle](https://www.kaggle.com/c/titanic/data) to the current directory. 

To generate a sample model, type:

```sh
$ Rscript titanic.R
```

%o generate the csv suitable for [upload to Kaggle](https://www.kaggle.com/c/titanic/submissions/attach), type:

```sh
$ Rscript titanic.R production
```
