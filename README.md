# Secondhand-smoke-umbrella-review 
R Scripts for Umbrella Review on Second-hand Smoke Exposure
NOTE: This repository provides R scripts and sample data used to reproduce the figures and analyses presented in our umbrella review on second-hand smoke exposure and its health effects.

1. System Requirements
Software Dependencies and Operating Systems
- R version: 4.3.2
- Required R packages:
  - `meta`
  - `metafor`
  - `dplyr`
  - `tidyr`
  - `ggplot2`
  - `readxl`
  - `forcats`
  - `janitor`
- These scripts were tested on:
  - Windows 10
  - macOS Monterey 12.6 (via RStudio)
Non-standard Hardware
No non-standard hardware is required. The scripts were run on standard laptops/desktops with 8GB+ RAM.

2. Installation Guide
Instructions
1. Install R 4.3.2 from https://cran.r-project.org
2. Use the following commands to install required packages:
install.packages(c("meta", "metafor", "dplyr", "tidyr", "ggplot2", "readxl", "forcats", "janitor"))
Typical Install Time
Installation on a normal desktop (Windows 10 or macOS) typically takes 2–5 minutes, depending on internet speed.

3. Demo
Instructions to Run on Data
1. Open the `main_meta_analysis.R` script in RStudio.
2. Run all lines sequentially, ensuring Excel files are placed in the same folder.
3. Figures (Fig1–Fig9, FigS2, FigS3) will be saved to your working directory.
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
- Load the relevant script (e.g., `fig3to9.R`, `figS2.R`)
- Use the dataset provided in `/data` folder

Citation
If you use this code, please cite our umbrella review paper once published.
Generated on 2025-06-28
