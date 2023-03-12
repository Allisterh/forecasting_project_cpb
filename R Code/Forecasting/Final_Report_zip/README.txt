### ---- READ ME ----

Dear Reader,

We, group A of the CPB case, have put together a zip-file containing all data and codes used in our study. This contains for the data the files:
- "data-eur2023.csv" given by the CPB, 
- "data_core.csv" which we use for the small dataset (V=9), 
- "data_additional.csv" which we use together with "data_core.csv" for the big dataset (V=21).

For our study we have run RF and XGB algorithms to forecast, calculate RMSEs, calculate Marginal Contributions and Variable Importance Measures. These codes are found in the following files:
- ...
- "XGB_Predictions_FinalReport.R" for forecasting and calculating RMSEs for the XGB method
- "SSA_FinalReport_RFXGB.R" for forecasting feature combinations including SSA
- "MC_FinalReport.R" To calculate and plot the marginal contributions
- "VIM_RF_FinalReport.R" To calculate the VIMs and plot the results (RF)
- ...

Furthermore, we have used function files "Data_file.R" and "Function_File.R" to set up our data and feature matrices. 

Kind regards,

Group A (CPB)