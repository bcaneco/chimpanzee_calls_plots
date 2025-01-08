

<!-- badges: start -->
<!-- badges: end -->

## Visualizations of acoustic data from chimpanzee calls

This repository contains **R** code developed for the manuscript
*Generative vocal plasticity in chimpanzees* by Lameira *et al* (2025,
accepted under review). In particular, the code presented here was
created to generate intuitive graphical visualizations of acoustic data
collected and analysed from audio recordings of chimpanzee calls. Some
of these graphs were selected for inclusion in the final draft of the
paper, specifically those presented in Figures 3-6.

Here we provide a brief description of each plot, pointing readers
interested in reproducing them to the underlying scripts.

### Data

Data used in this analysis is provided in the CSV file and consists of
acoustic metrics and attributes of calls recorded. Please refer to the
manuscript for further details on the extraction of the metrics of
interest.

### Software Requirements

- [R](https://www.r-project.org/) (\> v4.2.1)
- [RStudio Desktop](https://posit.co/download/rstudio-desktop/)

### Plot Reproducibility

To reproduce the plots, users can follow the next steps:

1.  Clone or fork the repository to your local machine
2.  Start an **R** session in Posit
3.  Select *File* \> Open Project double-clicking on the R project file
    *chimpanzee_calls_plots.Rproj*
4.  Run the command `renv::restore()`
5.  Open and run code contained in each of the scripts specified below

Briefly, we consider three types of plots - Directional plots - Absolute
change - Relative change

Data

<!-- Scripts to build graphical panels for each type of plot are available in -->
<!-- files: -->
<!-- - [panels_pizza.R](panels_pizza.R)  -->
<!-- - [panels_rays.R](panels_rays.R) -->
<!-- - [panels_snl.R](panels_snl.R) -->
<!-- - [panels_snl_spokes.R](panels_snl_spokes.R) -->

### Futher Details

A more detailed description and discussion the developed plots can be
found [here](.\outputs/readme.md).
