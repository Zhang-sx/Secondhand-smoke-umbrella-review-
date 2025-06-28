# Secondhand-smoke-umbrella-review 
R Scripts for Umbrella Review on Second-hand Smoke Exposure
NOTE: This repository provides R scripts and sample data used to reproduce the figures and analyses presented in our umbrella review on second-hand smoke exposure and its health effects.

1. System Requirements
Software Dependencies and Operating Systems
Operating System:
Windows 11 (Home Edition, Version 24H2)
Software Requirements:
R version: 4.2.3
Required R packages:
- R version: 4.2.3
- Required R packages:
  - `meta`
  - `metafor`
  - `dplyr`
  - `tidyr`
  - `ggplot2`
  - `readxl`
  - `forcats`
  - `janitor`
Hardware Requirements:
Standard desktop/laptop computer
At least 8 GB RAM
No additional hardware required

2. Installation Guide
Instructions
Install R 4.2.3 from https://cran.r-project.org
Open RStudio and run the following command to install dependencies:
install.packages(c("meta", "metafor", "dplyr", "tidyr", "ggplot2", "readxl", "forcats", "janitor"))
Typical Installation Time: 2–5 minutes depending on your internet connection and system configuration

3. Demo
Instructions to Run on Data
1. Open the `main_meta_analysis.R` script in RStudio.
2. Run all lines sequentially, ensuring Excel files are placed in the same folder.
3. Figures will be saved to your working directory.
Expected Output
- Summary statistics and p-values from meta-analysis
- Forest plots and publication bias assessments
- Reproduced figures used in the manuscript
Expected Run Time
- Forest plot analyses: ~1–3 seconds each
- Figure generation: ~5–10 seconds per figure

4. Instructions for Use
Running the Software on Your Data
1. Replace the provided `.xlsx` files with your data (ensure same format).
2. Update column names in the script if needed.
3. Re-run the full script from the beginning to reproduce results.
Reproduction Instructions (Optional)
We provide pre-cleaned data used to generate each figure in the paper. To reproduce:
- Load the relevant script
- Use the dataset provided in `/data` folder

Citation
If you use this code, please cite our umbrella review paper once published.
Generated on 2025-06-28
