# Annotations to the provided files

Data files are structured in the following way:
- Data on the TVL of the leading DeFi services is combined in a single file and contained in the sub-folder “defi”;
- Collected articles from every media are stored in the subfolder “news”.

Code is separated into 5 key parts corresponding to:
- Data collection (news_data.py) — Python scraper;
- General cleaning, preprocessing and data visualisation (clean_vis.R) — R script;
- Naive and modified DeFi EPU index construction (epu_index.R) — R script;
- Structural VAR model for the analysis of the uncertainty shocks (var_base.R) — R script;
- Functions, required for active learner and querying (uncertainty_sampling.R) — R script.

The analysis can be replicated by using the provided data files and running the provided R scripts. Active learning model would require iterating querying and model selection based on the AUC. 
