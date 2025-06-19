# Workouts

Package collecting my functions for workout analysis

![R-CMD-check](https://github.com/ulf85/Workouts/workflows/R-CMD-check/badge.svg)
![lint](https://github.com/ulf85/Workouts/workflows/lint/badge.svg)

## Overview
This package provides R functions to analyze workouts recorded by the Wahoo app. The workflow involves exporting workouts as `.fit` files to the `fit` directory, then extracting and converting the data using the program [GPSBabel](https://www.gpsbabel.org/) and the R function `fit2csv()`. The processed data is saved as Excel files in the `fixed` directory.

Other functions in this package generate summaries and plots, which can be used in R Markdown (`.Rmd`) reports.

## Features
- Running analysis
- Walking analysis
- Cycling analysis
- Indoor cycling analysis

## Installation
1. Clone this repository:
   ```sh
   git clone https://github.com/ulf85/Workouts.git
   ```
2. Install required R packages (see code for dependencies, e.g., `dplyr`, `ggplot2`, `readr`).

## Usage Example
```r
# Convert .fit files to .csv
fit2csv(path = "fit/", file = "workout.fit")

# See documentation in each R script for more details
```

## Notes
- The documentation and code comments are in German, as the package was originally intended for personal use.

## License
Specify your license here (e.g., MIT, GPL-3.0, etc.).

## Contributions
Contributions are welcome! Please open an issue or submit a pull request.
