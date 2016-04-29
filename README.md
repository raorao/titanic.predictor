# titanic.predictor


random-forest predictor of whether a given census object has a salary more than 50k. 

#### to run:

1) make sure Rscript is installed on your computer. 
2) navigate to the project's root. 
3) download [the required data sets from Kaggle](https://www.kaggle.com/c/titanic/data) to the current director. 

To generate a sample model, type:

```sh
$ Rscript adult.R
```

%o generate the csv suitable for [upload to Kaggle](https://www.kaggle.com/c/titanic/submissions/attach), type:

```sh
$ Rscript adult.R production
```
