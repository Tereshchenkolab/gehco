***STATA version 16
***STATA code written by Larisa Tereshchenko 
***October-December 2020
***preparation for multiple imputations and MI execution.

use "your_datafile_wit_missing_data.dta"


***step 1 explore missingness type
misstable summarize
misstable patterns
misstable tree, frequency
misstable nested
mi set flong


***step 2 Register variables
mi register regular study_id ecg_fileno alive sex place dead_beforeVT VTVF inapShock MMVT PVTVF apShock apATP compev ttcompev logtime VTtype
mi register imputed  icd_replacement race cm mi revascularization htn diabetes af cva aa_usage bb_usage noRAASmed ACEI ARB ARNI AldosteroneAnt ccbs bun cr nyha exact_lvef icd_type  manufacturer vt_zone_option atp vt_zone_bpm vf_zone_bpm peakQRSTAngle_deg - SAIQRST RRms - AAD3 pQRSaz - progr 




***step 3 Dry run to test models

***main imputed dataset
mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef  vt_zone_bpm vf_zone_bpm peakQRSTAngle_deg - SAIQRST RRms QTch QTcb QTcf pQRSaz - HRbpm (logit)  ACEI ARB AldosteroneAnt  PVCdistort PVCany (ologit) nyha (mlogit) race cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp progr icd_progr AAD3 mbeat3 icd_replacement = i.compev ttcompev logtime i.sex i.alive i.MMVT i.PVTVF i.apShock i.apATP i.inapShock i.place , dryrun

***sensitivity analysis imputed dataset (no missing ECG data)
mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef  vt_zone_bpm vf_zone_bpm  (logit)  ACEI ARB AldosteroneAnt  (ologit) nyha (mlogit) race cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp progr icd_progr AAD3  icd_replacement = i.compev ttcompev logtime i.sex i.alive i.MMVT i.PVTVF i.apShock i.apATP i.inapShock i.place peakQRSTAngle_deg - SAIQRST RRms QTch QTcb QTcf pQRSaz - HRbpm PVCdistort PVCany mbeat3, dryrun




***Step 4 Test run to check convergence
****main imputed dataset
mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef  vt_zone_bpm vf_zone_bpm peakQRSTAngle_deg - SAIQRST RRms QTch QTcb QTcf pQRSaz - HRbpm (logit) ACEI ARB AldosteroneAnt  PVCdistort PVCany (ologit) nyha (mlogit) race cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp progr AAD3 mbeat3 icd_replacement = i.compev ttcompev logtime i.sex i.alive i.MMVT i.PVTVF i.apShock i.apATP i.inapShock i.place , augment chainonly noisily burnin(66) savetrace(impstats, replace) rseed(9999999)

***sensitivity analysis imputed dataset (no missing ECG data)
mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef  vt_zone_bpm vf_zone_bpm  (logit)  ACEI ARB AldosteroneAnt  (ologit) nyha (mlogit) race cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp progr AAD3  icd_replacement = i.compev ttcompev logtime i.sex i.alive i.MMVT i.PVTVF i.apShock i.apATP i.inapShock i.place peakQRSTAngle_deg - SAIQRST RRms QTch QTcb QTcf pQRSaz - HRbpm PVCdistort PVCany mbeat3, augment chainonly noisily burnin(30) savetrace(impstats, replace) rseed(9999999)





***Step 5 real imputations
**main imputed dataset
mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef  vt_zone_bpm vf_zone_bpm peakQRSTAngle_deg - SAIQRST RRms QTch QTcb QTcf pQRSaz - HRbpm (logit) ACEI ARB AldosteroneAnt  PVCdistort PVCany (ologit) nyha (mlogit) race cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp progr AAD3 mbeat3 icd_replacement = i.compev ttcompev logtime i.sex i.alive i.MMVT i.PVTVF i.apShock i.apATP i.inapShock i.place, augment burnin(20) add(66) rseed(9999999)

**sensitivity analysis imputed dataset (no missing ECG data)
mi impute chained (pmm, knn(20))  agey cr bun egfr exact_lvef  vt_zone_bpm vf_zone_bpm  (logit)  ACEI ARB AldosteroneAnt  (ologit) nyha (mlogit) race cm mi revascularization htn diabetes af cva aa_usage  bb_usage ccbs icd_type manufacturer vt_zone_option atp progr AAD3  icd_replacement = i.compev ttcompev logtime i.sex i.alive i.MMVT i.PVTVF i.apShock i.apATP i.inapShock i.place peakQRSTAngle_deg - SAIQRST RRms QTch QTcb QTcf pQRSaz - HRbpm PVCdistort PVCany mbeat3, augment burnin(20) add(30) rseed(9999999)



***check how many imputations is enough
help how_many_imputations



