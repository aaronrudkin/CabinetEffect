1. To replicate the paper and regenerate the results, run COMPILE from the Terminal (Mac OS X / Linux). In order, this does the following:
	i) 	Assembles the panel data using the raw data files
		and pre-saved replication scripts. (5+ minutes Canada)
	ii)	Generates synthetic observations for Heckman correction models
	iii)	Runs data analysis and creates figures/tables. (5-10 minutes) 
	iv)	Runs scripts which modify the LaTeX output to correct
		for some scaling issues from stargazer.
	v)	Removes intermediary RData files.

	If you plan to manually run code, the assumed working directory 
	is this folder; many file loads use relative paths. Please
	ensure you have correctly set your path in R.

	Code run in Rscript 3.3.1; Python 2.7.12.
	

2. Location and contents of scripts:
	Paper and analysis:

	/Prep_1.R
		Data cleaning / prep for Heckman script
	/Prep_2.R
		Final data cleaning / merging external data
	/CanadaDataAnalysis.r
		Main analysis, tables, and figures
	/Supplementary/Canada Electorate Histogram.r
		Generates unused histogram graph for Canada
	/Supplementary/ENP Calculator.r
		Function to calculate ENP scores for Case Selection
		section
	/Includes/shrinkTex.py
		Modifies outputted tables and figures to fit the
		screen properly.

	Constructing Dataset:

	Canada Data/Cabinet/canCabinet.py
		Scrapes cabinet appointments from Library of
		Parliament
	Canada Data/Election Results/canadaResultsPull.py
		Scrapes Canadian election data and birthdates
	Canada Data/Merge/processResult.py
		Assembles scraped Canadian data into panel
	Canada Data/Party Names/canadaPartyNames.py
		Scrapes all parties so I can assemble a list
		for fuzzy matching of minor party name changes.
	Canada Data/SyntheticObs/processResults.py
		Adds cabinet metadata and ID values to the
		synthetic observations for Heckman correction.

3. Datasets for final analysis:
	Canada Data/Merge/output.csv
		Main panel dataset.
	
	Canada Data/SyntheticObs/synthOut.csv
		Synthetic observations
