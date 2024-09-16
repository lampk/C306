This folder contains all final data and codes for interim analysis.

Process for an interim analysis:
- Copy value-added data from folder Data/VAD (at top level) to subfolder Data/interimData.
- Copy Dummy-randomization list from folder Data/RandomizationList (at top level) to subfolder Data/RandomizationList.
- Run interimAnalysis.Rmd to create interim analysis report.
- Zip the whole folder to send to DSMB statistician.

DSMB statistician needs to:
- Put the true randomization list in the Data/RandomizationList subfolder (and remove the dummy list)
- change lines 5,6 and 14 of the code in interimAnalysis.Rmd