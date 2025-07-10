# BART-bittern-modelling
Spatial and scenario modelling to help guide environmental water delivery for Australasian bittern conservation


# Bittern BART Modelling

This repository contains R code and example outputs from a spatial modelling analysis of Australasian bittern (Botaurus poiciloptilus) habitat suitability in the southern Murray–Darling Basin, Australia. The models were developed to support evidence-based environmental water planning.

## 🧠 Project Summary

We used **Bayesian Additive Regression Trees (BART)** to model bittern detection data collected via autonomous acoustic recorders across managed wetlands. The models incorporate short- and long-term hydrological variables, vegetation structure, and spatial covariates to predict bittern use under current and hypothetical watering regimes.

---

## 📁 Repository Structure
```
bittern-bart-modelling/
├── scripts/
│ ├── 01_BART_model_FINAL.R # Data prep, scaling, and BART model fitting
│ └── 02_prediction_mapping_CLEANED.R # Spatial prediction, PDPs, and scenario maps
├── figures/
│ ├── PDPthresholds_fullModel.png # Partial dependence plots with scenario thresholds
│ └── bittern_prediction_and_WMAs_aiSmall.png # Predicted habitat suitability map
├── BART_model_summary_FINAL.docx # 1-page code summary (optional)
```
---

## 📈 Key Outputs

- **Partial dependence plots (PDPs)** showing key hydrological thresholds
- **Spatial maps** of bittern suitability across wetlands under different flow scenarios
- **Scenario predictions** using buffer- and WMA-scale inundation + river flow inputs

See `figures/` folder for visual examples.

---

## 🔧 Requirements

- R version ≥ 4.2
- Packages used: `sf`, `terra`, `dplyr`, `dbarts`, `ggplot2`, `patchwork`, `tmap`, `zoo`, `scales`, etc.
- Input data not included due to file size and access restrictions

---

## 🧑‍💻 Author

This code was developed by **Gavin Bonsen** as part of a postdoctoral project under the NSW Saving our Species program.

---

## 📬 Contact

Questions? Contact: `gavin.bonsen [at] [yourinstitution] [dot] edu [dot] au`
