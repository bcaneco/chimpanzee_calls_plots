

<!-- badges: start -->
<!-- badges: end -->

## Visualizations of acoustic data from chimpanzee calls

This repository contains code developed during the analysis underpinning
the manuscript *Generative vocal plasticity in chimpanzees* by Lameira
*et al* (2025, accepted under review). The code was designed to generate
clear and insightful graphical visualizations of acoustic data collected
from audio recordings of chimpanzee calls. Several of these
visualizations appear in the final manuscript, specifically in Figures
3-6.

To ensure reproducibility, this README provides a usage guide and
references the necessary scripts to generate these plots. Brief
descriptions of each visualization are also provided for clarity and
context.

### Data

Data used for this analysis is provided in the file
[calls_data.csv](data/calls_data.csv) and consists of acoustic metrics
and attributes of calls recorded. Please refer to the manuscript for
further details on the extraction of the metrics of interest.

### Software Requirements

- [R](https://www.r-project.org/) (\> v4.2.1)
- [RStudio Desktop](https://posit.co/download/rstudio-desktop/)

### Plot Generation

Briefly, we consider two types of plots - Directional plots (Fig. 4 in
manuscript):

To set-up plot reproduction, users can follow the next steps:

1.  Clone or fork the repository to your local machine
2.  Start an **R** session in Posit
3.  Select *File* \> Open Project double-clicking on the R project file
    *chimpanzee_calls_plots.Rproj*
4.  Run the command `renv::restore()`
5.  Open and run code contained in each of the scripts specified below

#### Directional plots

#### ‘Snake-and-Ladded’ plots

- Absolute change (Fig. 5)
- Relative change (Fig. 6)

<!-- Scripts to build graphical panels for each type of plot are available in -->
<!-- files: -->
<!-- - [panels_pizza.R](panels_pizza.R)  -->
<!-- - [panels_rays.R](panels_rays.R) -->
<!-- - [panels_snl.R](panels_snl.R) -->
<!-- - [panels_snl_spokes.R](panels_snl_spokes.R) -->

### Futher Details

A more detailed description and discussion the developed plots can be
found [here](.\outputs/readme.md).
