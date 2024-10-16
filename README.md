
<!-- README.md is generated from README.Rmd. Please edit that file -->

# :notebook: `alientraitgaps` – “Gaps in global non-native plant trait data and how to move forward”

<!-- badges: start -->
<!-- badges: end -->

## Summary

This repository holds the compendium for the manuscript entitled “Gaps
in global non-native plant trait data and how to move forward”.

The goal of the manuscript and the code in this compendium is to
leverage global plant trait data to depict how much we know about the
traits of naturalized plants around the world. This is a prerequisite to
perform any large-scale analyses about naturalized plants.

For this, we leverage data from four global plant trait databases:
[AusTraits](https://austraits.org/),
[BIEN](https://bien.nceas.ucsb.edu/bien/),
[GIFT](https://gift.uni-goettingen.de/home), and
[TRY](https://www.try-db.org/). We also use the
[GloNAF](https://glonaf.org) database to get lists of naturalized
species globally.

## Citation

If you want to reference this workflow you can cite it through the
following citation:

> Grenié M., Bruelheide H., Dawson W., Essl F., van Kleunen M., KÜhn I.,
> Kref H., Pyšek P., Weigelt P., and Winter M.. Workflow for *Gaps in
> global non-native plant trait data and how to move forward*.
> Submitted. Online at <https://doi.org/xxx/xxx> \[**Zenodo DOI to
> come**\]

Please also cite the manuscript:

> Grenié M., Bruelheide H., Dawson W., Essl F., van Kleunen M., KÜhn I.,
> Kref H., Pyšek P., Weigelt P., and Winter M.. *Gaps in global
> non-native plant trait data and how to move forward*. Submitted.
> \[**EcoEvoRxiv DOI to come**\]

## Installation

The repository contains the full workflow to reproduce our analyses and
figures. You can download it through GitHub with the following link:
<https://github.com/Rekyt/alientraitgaps/archive/main.zip>

From there, unzip it, and read the section below.

## How to run

The workflow leverages the `targets` package to get reproducible
analyses. To run it run the following command `targets::tar_make()`. The
workflow comes with an `renv` lockfile detailing all its dependencies.
Use `renv::restore()` to get all packages.

The workflow needs a reliable internet connection to run. On a standard
laptop with internet, it takes about **8 hours** to complete without
parallel execution.

## Needed environment variables

As the GloNAF data are not yet fully open, you need to have access to
the GloNAF database and put the id and password as environment variables
`GLONAF_USER` and `GLONAF_PASSWORD` respectively as provided by the
GloNAF. Similarly, these analyses rely on using the private access of
the GIFT database to gather all observations from GIFT. To reproduce it,
you need to store the private GIFT API URL as the `GIFT_RESTRICTED_API`.
If you don’t have access to the private API, you can use the public one
which gives very similar results.
