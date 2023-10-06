# Examining the intersection between socioeconomic position, ethnicity, and age in relation to grip strength in men and women from the UK Biobank study

This repository contains the research code and data related to our study on the intersection between socioeconomic position, ethnicity, and age, particularly focusing on their relationship with grip strength. The study uses data from the UK Biobank. Note that while the code is available, the UK Biobank data itself is excluded to comply with data protection regulations.

# Abstract:

## Repository Structure

```
├── README.md
├── 00_input/
│   ├── 0a_source_code/
│   │   ├── cleaning_functions.R
│   │   ├── modelling_functions.R
│   │   └── visualisation_functions.R
│   └── 0b_data/
│       ├── cleaned/
│       └── raw/
├── 01_scripts/
│   ├── 1a_data_cleaning.R
│   ├── 1b_multiple_imputation.R
│   ├── 1c_produce_descriptive_tables.R
│   ├── 1d_produce_regression_tables.R
│   └── 1e_produce_forestplots.R
└── 02_outputs/
    ├── 2a_model_outputs/
    ├── 2b_table_outputs/
    └── 2c_visualisation_outputs/
```


## Installation

To clone this repository:

```git clone https://github.com/your_username/UKBiobank-SocioEthnicAge-GripStrength.git```

## Usage

- Start with the data cleaning and imputation code located in `01_scripts/1a_data_cleaning.R` and `01_scripts/1b_multiple_imputation.R`.
- Proceed to data modelling with the scripts in `01_scripts/`.
- Generate descriptive and regression tables using the code in `01_scripts/1c_produce_descriptive_tables.R` and `01_scripts/1d_produce_regression_tables.R`.
- Create visualisations with the code located in `01_scripts/1e_produce_forestplots.R`.
