# Occupancy Detection

This repository includes the code used for this publication:

**Schaffer, M., Vera-Valdés, J. E., & Marzsal-Pomianowska, A. (2025). Non-intrusive hourly occupancy detection in residential buildings using remotely readable water meter data: Validation and large-scale analysis. Building and Environment, 113917. https://doi.org/10.1016/j.buildenv.2025.113917**

If you use this code, please cite the above-mentioned publication.

## Code Overview

This GitHub repository contains the code used in the analysis and generation of figures for our scientific publication. The code is organised into several files, each serving specific purposes:

1.  [**`01_validate_becker_kleiminger.R`**](01_validate_becker_kleiminger.R)**:** This script showcases the GeoMA algorithm developed by Becker and Kleiminger (2018) to detect occupants based on 30-minute electricity data. It uses the two accessible data sets used in their research. Additionally, it shows the performance of the algorithm with a 60-minute input.

2.  [**`02_daily_validation.R`**](02_daily_validation.R)**:** This script evaluates the daily occupant detection part of the proposed occupant detection algorithm based on water use data. It investigates the effect of the different thresholds for water use volume and usage events.

3.  [**`03_hourly_validation.R`**](03_hourly_validation.R)**:** This script evaluates the hourly occupant detection part of the proposed occupant detection algorithm based on water use data. It also investigates the individual steps of the algorithm. Additionally, it compares the algorithm against the electricity-based algorithm of Becker and Kleiminger (2018).

4.  [**`04_large_scale_processing.R`**](04_large_scale_processing.R)**:** This script prepares the large scale SWM data.

5.  [**`05_large_scale_analysis.R`**](05_large_scale_analysis.R)**:** This script uses the prepared large scale SWM data and establishes daily and hourly occupancy based on the proposed algorithm. Afterwards, his occupancy is aggregated and plotted

## Data

Most data used is publicly available and automatically downloaded via the scripts. Exceptions are:

1.  [**`01_validate_becker_kleiminger.R`**](01_validate_becker_kleiminger.R)**:**

    The Eco dataset could not be downloaded directly due to issues with the copyright consent. The data description is available at: <https://vs.inf.ethz.ch/res/show.html?what=eco-data> and can be downloaded from there. The scripts expect the unzipped folder in: `data/eco`.

4.  [**`04_large_scale_processing.R`**](04_large_scale_processing.R)**:**

    Uses the SWM data from Schaffer et al. (2024). The scripts expect an .fst file: `data/04_large_scale_swm.fst` with the following four columns:

    | water_meter_id | customer_id | time_rounded | water_colume_m3 |
    |:----------------|:-------------|:--------------|:-----------------|

## Bibtext

```
@article{SCHAFFER2025113917,
title = {Non-intrusive hourly occupancy detection in residential buildings using remotely readable water meter data: Validation and large-scale analysis},
journal = {Building and Environment},
pages = {113917},
year = {2025},
issn = {0360-1323},
doi = {https://doi.org/10.1016/j.buildenv.2025.113917},
url = {https://www.sciencedirect.com/science/article/pii/S0360132325013873},
author = {Markus Schaffer and J. Eduardo Vera-Valdés and Anna Marszal-Pomianowska},
keywords = {Non-intrusive occupant monitoring, Validation, Electricity meter, Smart meter, Occupant analysis}
}
```


## References

Becker, V., & Kleiminger, W. (2018). Exploring zero-training algorithms for occupancy detection based on smart meter measurements. Computer Science - Research and Development, 33(1–2), 25–36. <https://doi.org/10.1007/s00450-017-0344-9>

Schaffer, M., Veit, M., Marszal-Pomianowska, A., Frandsen, M., Pomianowski, M. Z., Dichmann, E., Sørensen, C. G., & Kragh, J. (2024). Dataset of smart heat and water meter data with accompanying building characteristics. Data in Brief, 52, 109964. <https://doi.org/10.1016/j.dib.2023.109964>
