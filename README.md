# fish-paste
Routine preprocessing to create analysis ready data from REA SPC surveys

# Folder structure
* lib - local functions
* data - files used (raw data ALL_REA_FISH_RAW, Sectors-Strata_Areas_2015 - needs to be updated after every new survey round, and SITEMASTER 2015 - also to be updated every new survey round)
* munge - working scripts to get data analysis ready, whether working site or pooled up to island across years (work through scripts in order, each depends on the output of the preceding script).

Edit these preprocessing files depending on your purpose.

01_gitFishREACleanData.R housekeeping to create a clean raw working data set (wd) (rows as individual fish observations)

02_gitFISHREACalcWD.R loads wd, filter what you want in terms of location, years, methods etc and select summary metrics e.g. biomass by trophic group, species richness etc. Creates working site data (wsd) (averages from the two diver replicates)

03_gitFishREAPooledData loads wed, check for replication across strata, select analysis scheme (depends on region of interest - this will require a couple of iterative runs depending on the scheme, select how you want the data aggregate, by island, reef zone per year, by island across all years, by region etc.

04_gitMRbinding - is this specific to the monitoring report - perhaps shift it out of here?


