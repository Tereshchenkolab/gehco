***STATA version 16
***STATA code written by Larisa Tereshchenko 
***October 2020
***preparation for multiple imputations and MI execution.

use "your_datafile_wit_missing_data.dta"


***step 1 explore missingness type
misstable summarize
misstable patterns
misstable tree, frequency
misstable nested
mi set flong


***step 2 Register variables
mi register regular study_id ecg_fileno alive place eligible dead_beforeVT VTVF inapShock MMVT PVTVF apShock apATP compev

mi register imputed ecg_filepacing icd_replacement sex race cm mi revascularization CABG PTCA htn diabetes af cva aa_usage Amiodarone Dronedarone Propafenone Sotalol OtherAAD bb_usage noRAASmed ACEI ARB ARNI AldosteroneAnt ccbs bun cr nyha exact_lvef inclusion exclusion indication icd_type LVleadapexbase LVleadantpost manufacturer vt_zone_option atp vt_zone_bpm vf_zone_bpm vt_zone_ms vf_zone_ms initial_nid_vt redect_nid_vt nid_numerator nid_denominator initial_time_vt initial_time_vf peakQRSTAngle_deg - SAIQRST age AP_VTVF - NshockAp ttcompev mbeat ttinapSh


***step 3 Dry run to test models

mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef peakQRSTAngle_deg - SAIQRST vt_zone_bpm - nid_numerator (logit) ACEI ARB AldosteroneAnt  (ologit) nyha   (mlogit) race mbeat3 icd_replacement cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp   = i.compev ttcompev logtime i.sex i.alive i.MMVT i.PVTVF i.apShock i.apATP i.inapShock, dryrun


***Step 4 Test run to check convergence

mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef peakQRSTAngle_deg - SAIQRST vt_zone_bpm - nid_numerator (logit) ACEI ARB AldosteroneAnt  (ologit) nyha   (mlogit) race mbeat3 icd_replacement cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp   = i.compev ttcompev logtime i.sex i.alive  i.MMVT i.PVTVF i.apShock i.apATP i.inapShock, augment chainonly noisily burnin(20) savetrace(impstats, replace) rseed(9999999)





***Step 5 real imputations

mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef peakQRSTAngle_deg - SAIQRST vt_zone_bpm - nid_numerator (logit) ACEI ARB AldosteroneAnt  (ologit) nyha   (mlogit) race mbeat3 icd_replacement cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp   = i.compev ttcompev logtime i.sex i.alive  i.MMVT i.PVTVF i.apShock i.apATP i.inapShock, augment burnin(20) add(20) rseed(9999999)

***check how many imputations is enough
help how_many_imputations



