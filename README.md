# Workouts
Package collecting my functions around my workout analysis

![R-CMD-check](https://github.com/ulf85/Workouts/workflows/R-CMD-check/badge.svg)
![lint](https://github.com/ulf85/Workouts/workflows/lint/badge.svg)

I started to export my workouts recorded by my Wahoo app to Excel and make some analysis on it.
To export the data I store the workouts as `.fit`-files in my `fit`-directory and extract the important data with the programm [GPSBabel](https://www.gpsbabel.org/) 
to an Excel file in the `fixed`-directory.
This is done with the R-function `fit2csv()`, expecting the two mentioned folders to exist.

THe other functions are made for summaries and plots, which I used in my `.Rmd`.

So far I have functions for 
+ running
+ walking
+ cycling
+ indoor cycling

Remark: The documentation is in German, since it was just indended to be used by me.
