This repository contains data, R scripts and outputs for the analyses of GCRMN reef monitoring data in Sint Maarten, from 2016 to 2023.

In the Data folder:
- 1-Metadata: contains surveys and sites information, and folders of sea surface temperature and degree heating weeks data
- 2-Raw_data: contains GCRMN data for each taxa, percent cover from CoralNet (2018 to 2023), and a folder with percent cover from CPCe (2016,2017)
- 3-Clean_data: contains the same data but cleaned and formated, ready for analyses

In the R_scripts folder:
- 01_Cleaning_data: contains scripts to clean and format raw metadata and data, and calculate diversity metrics
- 02_Analyses_Plots: contains all analyses scripts
	- Boxplots, Barplots, Lineplots and Anovas for each taxa
	- Generalized Addiditive Models for some fish and cover metrics
	- Principal Coordinates Analyses for fish communities and benthic communities
	- Permanova analyses for fish communities and benthic communities

The Outputs folder contains all graphs and tables sorted by taxa.
