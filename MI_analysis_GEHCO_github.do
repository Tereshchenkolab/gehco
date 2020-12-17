***STATA version 16
***STATA code written by Larisa Tereshchenko 
***October-December 2020
***analysis of GEHCO study data

use "your_datafile_imputed20.dta"

***descriptive stats
mi estimate, vartable: mean agey
mi estimate, vartable : proportion race cm
mi estimate, vartable: mean agey
mi estimate, vartable : proportion race cm
mi estimate, vartable : proportion mi
mi estimate, vartable : proportion revascularization
mi estimate, vartable : proportion htn
mi estimate, vartable : proportion diabetes
mi estimate, vartable : proportion af
mi estimate, vartable : proportion cva
mi estimate, vartable : proportion aa_usage
mi estimate, vartable : proportion bb_usage
mi estimate, vartable : proportion ccbs
mi estimate, vartable : proportion icd_type
mi estimate, vartable : proportion manufacturer
mi estimate, vartable: mean exact_lvef
mi estimate, vartable : proportion nyha
mi estimate, vartable: mean bun
mi estimate, vartable: mean areaQRSTAngle_deg SAIQRST egfr
mi estimate, vartable : proportion ACEI
mi estimate, vartable : proportion ARB
mi estimate, vartable : proportion AldosteroneAnt
mi estimate, vartable : proportion icd_replacement
mi estimate, vartable : proportion vt_zone_option
mi estimate, vartable : proportion atp
mi estimate, vartable: mean vt_zone_bpm vf_zone_bpm nid_numerator

***to assess if there is a sufficient number of imputations
how_many_imputations

mi stset ttcompev, id(study_id) failure(compev==1)

***generate standardized variables
mi passive: egen zaQRSTa = std( areaQRSTAngle_deg )
mi passive: egen zpQRSTa = std( peakQRSTAngle_deg )
mi passive: egen zSAI = std( SAIQRST )
mi passive: egen zpSVGmag = std( peakSVGMagnitude_uV)
mi passive: egen zSVGmag = std( WilsonSVG_uVms )
mi passive: egen zVMQTi = std( AUCofQTVM_mVms )
mi passive: egen zaSVGaz = std( areaSVGAzimuth_deg )
mi passive: egen zpSVGaz = std( peakSVGAzimuth_deg )
mi passive: egen zaSVGel = std( areaSVGElevation_deg )
mi passive: egen zpSVGel = std( peakSVGElevation_deg )
mi passive: egen zQT = std( QTInterval_ms )
mi passive: egen zRR = std( RRinterval_ms )
mi passive: egen zQRS = std( QRSduration_ms )
mi passive: egen zage = std( agey)
mi passive: egen zlvef = std( exact_lvef )
mi passive: egen zbun = std( bun )
mi passive: egen zegfr = std( egfr )
mi passive: egen zvtzone = std( vt_zone_bpm )
mi passive: egen zvfzone = std( vf_zone_bpm )
mi passive: egen znidnum = std( nid_numerator )
mi passive: egen zTpTe = std( TpeaktoTend_ms )


***survival analysis  model 1
mi stset ttcompev, id(study_id) failure(compev==1)
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.cm
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.mi
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.revascularization
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.htn
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.diabetes
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.af
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.cva
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.aa_usage
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.bb_usage
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.ccbs
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place i.icd_type
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place i.manufacturer
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.atp
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place ib2.vt_zone_option
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place i.ACEI
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place i.ARB
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place i.AldosteroneAnt
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place i.nyha
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place zlvef
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place zbun
mi estimate, eform("Haz. Ratio") : stcox age ib2.sex i.race i.mbeat3 i.place zegfr
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zvtzone
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zvfzone
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.progr
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zHR
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zRR
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zQTch i.PVCany i.PVCdistort zRR
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zQTcb i.PVCany i.PVCdistort zRR
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zQTcf i.PVCany i.PVCdistort zRR
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zQRS
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place  i.PVCany i.PVCdistort zRR zaQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place  i.PVCany i.PVCdistort zRR zpQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zSAI
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zVMQTi
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGaz 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGaz 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib1.qaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib1.q4aSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGel 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGel 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGmag 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGmag 
how_many_imputations

***model 2
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.mi
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.revascularization
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.ccbs
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.AldosteroneAnt
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zbun
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.icd_type
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.atp
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.vt_zone_option
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zvtzone
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zvfzone
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.progr
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB  zHR
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.PVCany
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zQTch
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zQRS
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zaQRSTa
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zpQRSTa
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zSAI
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zVMQTi
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zaSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zpSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany ib1.qaSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany ib1.q4aSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zaSVGel
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zpSVGel
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zaSVGmag
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zpSVGmag
how_many_imputations


***model 3
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.mi
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.revascularization
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.ccbs
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr i.AldosteroneAnt
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zbun
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.atp
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.vt_zone_option
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zHR
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr i.PVCany
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zRR i.PVCdistort i.PVCany zQRS
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zRR i.PVCdistort i.PVCany zQTch
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaQRSTa
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpQRSTa
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGel
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGel
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib1.qaSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib1.q4aSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zSAI
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zVMQTi
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGmag
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGmag
how_many_imputations


***model 4 in addition add QRS, QTch, RR 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch 
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.mi
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.revascularization
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.ccbs
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch i.AldosteroneAnt
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch zbun
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.atp
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.vt_zone_option
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zaQRSTa
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zpQRSTa
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zaSVGel
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zpSVGel
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zaSVGmag
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zpSVGmag
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zSAI
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zVMQTi
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zaSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zpSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch ib1.qaSVGaz
how_many_imputations
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch ib1.q4aSVGaz
how_many_imputations

***difference in point estimates test 
***difference in model1
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place, nohr
di ( -.1264623  - ( .4776647 ))/(sqrt((.0490502)^2+(.0574782 )^2))
di (.4898189 - (.1530541 ))/(sqrt((.1310314)^2+(.1268795)^2))
di (.3706412 - ( .1201702 ))/(sqrt(( .1508992)^2+(.1848231)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cm, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cm, nohr
di (.2181661  - (.3565288 ))/(sqrt(( .1161431)^2+(.1137744)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.diabetes, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.diabetes, nohr
di ( -.0124301 - ( .594895))/(sqrt((.0918996)^2+( .0943145)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.af, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.af, nohr
di (.2217683 - ( .3939796 ))/(sqrt((.1084438)^2+(.1087951)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cva, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cva, nohr
di ( .1791597  - (.1962261))/(sqrt((.1356221)^2+( .1461018 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.aa_usage , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.aa_usage , nohr
di (.4484519  - ( .3678537))/(sqrt((.1252471)^2+( .1273875)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.bb_usage , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.bb_usage , nohr
di (.1703995  - ( -.3267419 ))/(sqrt(( .1414729  )^2+( .1229645 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef , nohr
di (-.1892308 - ( -.2951747))/(sqrt(( .0550386  )^2+( .0636054)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.nyha , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.nyha , nohr
di (.1941509 - (  .4840713))/(sqrt(( .139848)^2+(.1692239)^2))
di (.3054747 - ( .9202167 ))/(sqrt((.1409748)^2+(.1652322)^2))
di ( .2432532 - (  1.469343 ))/(sqrt((.3243171)^2+(.2416546 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zegfr , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zegfr , nohr
di (  -.1797066 - ( -.4379407 ))/(sqrt(( .0535174 )^2+( .0637136 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.ACEI , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.ACEI , nohr
di ( .1126737 - ( -.3562873))/(sqrt(( .0986905)^2+( .1039638)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.ARB , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.ARB , nohr
di (  -.1881015  - ( -.0600245))/(sqrt((.1237869)^2+(.1297993)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.icd_type , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.icd_type , nohr
di (  -.2677608  - (.0185297))/(sqrt((.1124283)^2+(.1074809 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zvtzone , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zvtzone , nohr
di ( -.2831322 - (.0195767))/(sqrt(( .0595454)^2+( .0637827)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zvfzone , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zvfzone , nohr
di ( -.1287059  - ( .0726438  ))/(sqrt((.067616 )^2+( .0705023)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.progr , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.progr , nohr
di (.4639906- (   .0451561 ))/(sqrt(( .142192)^2+(.1348939)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.mi , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.mi , nohr
di (.2903407 - (  .2440839 ))/(sqrt((.0962791)^2+(.1032216)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.revascularization , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.revascularization , nohr
di (  .2492734 - ( .2192051 ))/(sqrt((.0950692)^2+(.100238)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.ccbs , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.ccbs , nohr
di ( -.2740368 - ( .0444144 ))/(sqrt(( .1806208)^2+( .1721293)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.AldosteroneAnt , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.AldosteroneAnt , nohr
di (.1214244- (  -.0570493 ))/(sqrt((.0998127)^2+( .1178864 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zbun , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zbun , nohr
di ( .1082354- ( .3572516 ))/(sqrt((  .0450671)^2+( .0390053 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.atp , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.atp , nohr
di (-.0718657- ( -.2486378))/(sqrt((.1522698)^2+(.1305217)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.vt_zone_option , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.vt_zone_option , nohr
di ( -.0636392- (  -.2414695 ))/(sqrt((.1743752)^2+( .1545653  )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zHR , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zHR , nohr
di ( .0931646 - ( .2662303 ))/(sqrt((.0477813)^2+(.049105 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany , nohr
di ( .3264949  - (-.0059427 ))/(sqrt((.1098499)^2+(.122386)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zQTch, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zQTch, nohr
di (  -.0779742 - ( .0650906  ))/(sqrt(( .049529)^2+(.0514384)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zQRS, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zQRS, nohr
di (  -.0192575 - (-.0148572))/(sqrt((.0418891)^2+(.048689)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaQRSTa, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaQRSTa, nohr
di ( .1655097 - (.15817 ))/(sqrt((.0533446)^2+(.0588672)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpQRSTa, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpQRSTa, nohr
di (.1041897 - (.1519898 ))/(sqrt(( .0490638)^2+(.0564743)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zSAI, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zSAI, nohr
di ( -.1149993 - (-.0351797 ))/(sqrt((.0513099)^2+(.0514828 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zVMQTi, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zVMQTi, nohr
di ( -.1235559 - (-.0416539))/(sqrt((.0514166)^2+(.0512185 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGel, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGel, nohr
di (.1230777 - ( .0343785))/(sqrt((.0458954)^2+( .0508879 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGel, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGel, nohr
di ( .0932389 - ( .0544262))/(sqrt(( .0481985 )^2+(  .0541027 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGmag, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGmag, nohr
di ( -.331357 - (  -.2094642))/(sqrt((  .0594112 )^2+(.0614822 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGmag, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGmag, nohr
di ( -.0875239 - (  -.0934896))/(sqrt((  .0526778 )^2+( .0619311  )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGaz, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGaz, nohr
di ( .111844  - ( -.0560679))/(sqrt((  .0523209 )^2+(  .0532735)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib1.qaSVGaz, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib1.qaSVGaz, nohr
di ( -.1754239  - (.155554))/(sqrt(( .1164011)^2+(.1272645)^2))
di (.0501646  - (.2773898))/(sqrt((.1121421)^2+(.1277821)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib1.q4aSVGaz, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib1.q4aSVGaz, nohr
di (-.1897753  - (.0785203))/(sqrt((.1323555)^2+(.1501698)^2))
di (-.0767691  - (.2305638))/(sqrt((.1294069)^2+( .146507)^2))
di (-.0186419  - (.325938))/(sqrt((.129994)^2+(.1470045)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib2.q4aSVGaz, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib2.q4aSVGaz, nohr
di (.1897753  - ( -.0785203))/(sqrt(( .1323555)^2+(.1501698)^2))
di (.1130062  - (.1520435))/(sqrt(( .1334879)^2+(.1444428)^2))
di (.1711335  - (.2474176 ))/(sqrt((.1367663)^2+(.1480284)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib2.qaSVGaz, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib2.qaSVGaz, nohr
di ( .1754239  - ( -.155554))/(sqrt((.1164011)^2+( .1272645)^2))
di (  .2255885  - ( .1218358 ))/(sqrt((.118211 )^2+( .1260855 )^2))

***difference in model 3
***model 3
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr , nohr
di (  -.219258  - (  .274563 ))/(sqrt((.0586002 )^2+( .066486 )^2))
di ( .4749006  - ( .1452189 ))/(sqrt((.1368216 )^2+( .1343807 )^2))
di ( .3111573  - ( -.0135594 ))/(sqrt(( .1616148 )^2+( .1978612 )^2))
di ( .1252017   - (.3709558 ))/(sqrt(( .1253739 )^2+( .1207167 )^2))
di ( -.0423439   - ( -.0300141 ))/(sqrt((  .1083084  )^2+( .1241238 )^2))
di (  -.1102638   - (  .3739846 ))/(sqrt((   .0971542  )^2+(  .1018255 )^2))
di (  .0653167   - (  .2815901 ))/(sqrt((  .1189895  )^2+(  .119573  )^2))
di (  .169051   - ( .1564477 ))/(sqrt((  .1401823  )^2+(  .1502423   )^2))
di (   .3350839   - ( .0733938 ))/(sqrt(( .1393444  )^2+(  .1407533   )^2))
di (   .1504919  - (  -.2946667))/(sqrt(( .1460766  )^2+(  .127627   )^2))
di (   .1504919  - (  -.2946667))/(sqrt(( .1460766  )^2+(  .127627   )^2))
di (   -.1384101  - ( -.1983762))/(sqrt(( .0597278  )^2+(  .0683076   )^2))
di (   .1799148  - (  .4800025))/(sqrt((.1468353  )^2+( .1743806 )^2))
di (  .3127618   - (   .794271))/(sqrt((.1583285  )^2+(  .1814753 )^2))
di (   .1233228   - (  1.248717))/(sqrt(( .3385304  )^2+(  .2562088 )^2))
di (   -.1287153   - ( -.2967007))/(sqrt(( .0566095  )^2+( .0648168 )^2))
di ( .0138481 - ( -.4291395))/(sqrt((.1209012 )^2+( .1258226 )^2))
di ( -.091358 - (  -.2334048))/(sqrt((.1524743 )^2+( .1591186 )^2))
di (  -.3930927 - ( -.2642995))/(sqrt((.1261527 )^2+( .1207717 )^2))
di ( -.2849423 - (  .0462514))/(sqrt(( .0696065 )^2+( .0764098 )^2))
di (  .0697077 - (  .0446652))/(sqrt(( .0832708 )^2+(.0872622 )^2))
di (  .4475445 - (  .0272988))/(sqrt(( .1534841 )^2+(.1437664 )^2))


mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.mi , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.mi , nohr
di ( .1862358 - (  -.0332618))/(sqrt((  .1580238 )^2+( .1579805 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.revascularization , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.revascularization , nohr
di (  .2071459 - (  -.0638406))/(sqrt((  .1303022 )^2+( .1288841  )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.ccbs, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.ccbs, nohr
di (  -.2127329 - ( .0727201))/(sqrt(( .1842713 )^2+(  .176767  )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr i.AldosteroneAnt , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr i.AldosteroneAnt, nohr
di (  .06932 - (-.2383088))/(sqrt((  .1056659 )^2+(  .1221559 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zbun , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zbun , nohr
di ( .0122987 - ( .2050029))/(sqrt(( .0662586 )^2+(  .0594425 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.atp , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.atp , nohr
di (.0833832 - (-.2477423))/(sqrt((.1682274 )^2+(  .1403229 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox  zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.vt_zone_option , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox  zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.vt_zone_option , nohr
di (.0276375 - ( -.4252888))/(sqrt(( .2378376 )^2+(   .208915 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zHR, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zHR, nohr
di (.0929172 - (  .1426214))/(sqrt(( .0522402  )^2+(   .0539421 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr i.PVCany , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr i.PVCany , nohr
di (.3906081 - (  .0403305 ))/(sqrt(( .1137384  )^2+(  .1273707 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zRR i.PVCdistort i.PVCany zQRS , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zRR i.PVCdistort i.PVCany zQRS , nohr
di ( .0217773 - (-.0119112))/(sqrt((  .0478125 )^2+( .0549478 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zRR i.PVCdistort i.PVCany zQTch , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zRR i.PVCdistort i.PVCany zQTch, nohr
di ( -.0774873 - (.0071599))/(sqrt(( .0534968 )^2+( .0559594 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaQRSTa , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaQRSTa , nohr
di ( .190698  - ( .1674827))/(sqrt(( .0575444 )^2+( .0656195 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpQRSTa , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpQRSTa , nohr
di ( .1296931  - ( .1497209))/(sqrt(( .0533799 )^2+( .061981 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGel , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGel , nohr
di ( .1365434  - ( .0300067))/(sqrt((.0457961 )^2+( .0519536 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGel , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGel , nohr
di ( .0990088  - ( .0202934))/(sqrt((.0485743 )^2+( .0549944 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zSAI , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zSAI , nohr
di (  -.0515902  - ( .0155274))/(sqrt((.0579721 )^2+( .0582115 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zVMQTi, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zVMQTi, nohr
di (  -.0622273  - ( .0099567))/(sqrt((.0589909 )^2+( .0588794 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGmag, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGmag, nohr
di ( -.2700643  - ( -.1382812))/(sqrt((.0629746 )^2+(  .0648234 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGmag, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGmag, nohr
di (  -.047343  - ( .0013157 ))/(sqrt(( .0540143 )^2+( .0626172 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGaz, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGaz, nohr
di (  .1148979  - (-.0900317 ))/(sqrt((.0533033 )^2+( .0537329 )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib1.qaSVGaz, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib1.qaSVGaz, nohr
di ( -.1394709  - (.1658044 ))/(sqrt(( .1242763 )^2+(.1366097  )^2))
di (  .0406304   - (.1952812 ))/(sqrt(( .1176575 )^2+( .1339413  )^2))

mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib1.q4aSVGaz, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib1.q4aSVGaz, nohr
di (  -.1308446   - (.1054632 ))/(sqrt(( .13983 )^2+( .158385  )^2))
di ( -.0290665   - (.1972884 ))/(sqrt((  .1395713 )^2+( .155859  )^2))
di ( -.0221028   - (.2455506 ))/(sqrt(( .1368771 )^2+( .155158  )^2))

****score replication
mi passive: gen sqrtSVGel=sqrt( areaSVGElevation_deg)
mi passive: gen sqrtSVGaz=sqrt(abs((areaSVGAzimuth_deg-360)/2))

mi passive: gen bestSAI_WM =1 if race==1 & sex==1 & SAIQRST >=174.05397
mi passive: gen bestSAI_WF =1 if race==1 & sex==2 & SAIQRST >=178.32982
mi passive: gen bestSAI_BM =1 if race!=1 & sex==1 & SAIQRST >=166.08623
mi passive: gen bestSAI_BF =1 if race!=1 & sex==2 & SAIQRST >=181.52505

mi passive: gen bestQRST_WM =1 if race==1 & sex==1 & areaQRSTAngle_deg >=89.600204
mi passive: gen bestQRST_WF =1 if race==1 & sex==2 & areaQRSTAngle_deg >=70.313507
mi passive: gen bestQRST_BM =1 if race!=1 & sex==1 & areaQRSTAngle_deg >=89.294922
mi passive: gen bestQRST_BF =1 if race!=1 & sex==2 & areaQRSTAngle_deg >=57.724033

mi passive: gen bestSVGmag_WM =1 if race==1 & sex==1 & peakSVGMagnitude_uV >=858.53711
mi passive: gen bestSVGmag_WF =1 if race==1 & sex==2 & peakSVGMagnitude_uV >=2022.6794
mi passive: gen bestSVGmag_BM =1 if race!=1 & sex==1 & peakSVGMagnitude_uV >=1725.0658
mi passive: gen bestSVGmag_BF =1 if race!=1 & sex==2 & peakSVGMagnitude_uV >=2335.7021

mi passive: gen bestSVGel_WM =1 if race==1 & sex==1 & sqrtSVGel >=8.6278677
mi passive: gen bestSVGel_WF =1 if race==1 & sex==2 & sqrtSVGel >=8.5964823
mi passive: gen bestSVGel_BM =1 if race!=1 & sex==1 & sqrtSVGel >=8.2215099
mi passive: gen bestSVGel_BF =1 if race!=1 & sex==2 & sqrtSVGel >=8.5089712

mi passive: gen bestSVGaz_WM =1 if race==1 & sex==1 & sqrtSVGaz >=6.8436542
mi passive: gen bestSVGaz_WF =1 if race==1 & sex==2 & sqrtSVGaz >=6.1420283
mi passive: gen bestSVGaz_BM =1 if race!=1 & sex==1 & sqrtSVGaz >=5.7895684
mi passive: gen bestSVGaz_BF =1 if race!=1 & sex==2 & sqrtSVGaz >=6.3935628


mi passive: gen abnSAI = 1 if bestSAI_WM ==1 | bestSAI_WF==1 | bestSAI_BM==1 | bestSAI_BF==1 
recode abnSAI (.=0)

mi passive: gen abnQRS_T = 1 if bestQRST_WM==1 | bestQRST_WF==1 | bestQRST_BM==1 | bestQRST_BF==1 
recode abnQRS_T (.=0)

mi passive: gen abnSVGmag = 1 if bestSVGmag_WM==1 | bestSVGmag_WF==1 | bestSVGmag_BM==1 | bestSVGmag_BF==1 
recode abnSVGmag (.=0)

mi passive: gen abnSVGel =1 if bestSVGel_WM==1 | bestSVGel_WF==1 | bestSVGel_BM==1 | bestSVGel_BF==1 
recode abnSVGel (.=0)

mi passive: gen abnSVGaz =1 if bestSVGaz_WM==1 | bestSVGaz_WF==1 | bestSVGaz_BM==1 | bestSVGaz_BF==1 
recode abnSVGaz (.=0)

mi passive: gen score1 = 1 if abnSVGaz==1 | abnSVGel==1 | abnQRS_T==1 | abnSAI==1 | abnSVGmag==1
recode score1 (.=0)

mi passive: gen score21 = 1 if abnSVGaz==1 & abnSVGel==1 & abnQRS_T==0 & abnSAI==0 & abnSVGmag==0
mi passive: gen score22 = 1 if abnSVGaz==1 & abnSVGel==0 & abnQRS_T==1 & abnSAI==0 & abnSVGmag==0
mi passive: gen score23 = 1 if abnSVGaz==1 & abnSVGel==0 & abnQRS_T==0 & abnSAI==1 & abnSVGmag==0
mi passive: gen score24 = 1 if abnSVGaz==1 & abnSVGel==0 & abnQRS_T==0 & abnSAI==0 & abnSVGmag==1
mi passive: gen score25 = 1 if abnSVGaz==0 & abnSVGel==1 & abnQRS_T==1 & abnSAI==0 & abnSVGmag==0
mi passive: gen score26 = 1 if abnSVGaz==0 & abnSVGel==1 & abnQRS_T==0 & abnSAI==1 & abnSVGmag==0
mi passive: gen score27 = 1 if abnSVGaz==0 & abnSVGel==1 & abnQRS_T==0 & abnSAI==0 & abnSVGmag==1
mi passive: gen score28 = 1 if abnSVGaz==0 & abnSVGel==0 & abnQRS_T==1 & abnSAI==1 & abnSVGmag==0
mi passive: gen score29 = 1 if abnSVGaz==0 & abnSVGel==0 & abnQRS_T==1 & abnSAI==0 & abnSVGmag==1
mi passive: gen score20 = 1 if abnSVGaz==0 & abnSVGel==0 & abnQRS_T==0 & abnSAI==1 & abnSVGmag==1

mi passive: gen score31 =1 if abnSAI==1 & abnQRS_T==1 & abnSVGel==1 & abnSVGaz==0 & abnSVGmag==0
mi passive: gen score32 =1 if abnSAI==1 & abnQRS_T==1 & abnSVGel==0 & abnSVGaz==1 & abnSVGmag==0
mi passive: gen score33 =1 if abnSAI==1 & abnQRS_T==1 & abnSVGel==0 & abnSVGaz==0 & abnSVGmag==1
mi passive: gen score34 =1 if abnSAI==1 & abnQRS_T==0 & abnSVGel==1 & abnSVGaz==1 & abnSVGmag==0
mi passive: gen score35 =1 if abnSAI==1 & abnQRS_T==0 & abnSVGel==0 & abnSVGaz==1 & abnSVGmag==1
mi passive: gen score36 =1 if abnSAI==0 & abnQRS_T==1 & abnSVGel==1 & abnSVGaz==1 & abnSVGmag==0
mi passive: gen score37 =1 if abnSAI==0 & abnQRS_T==1 & abnSVGel==1 & abnSVGaz==0 & abnSVGmag==1
mi passive: gen score38 =1 if abnSAI==0 & abnQRS_T==0 & abnSVGel==1 & abnSVGaz==1 & abnSVGmag==1
mi passive: gen score39 =1 if abnSAI==0 & abnQRS_T==1 & abnSVGel==0 & abnSVGaz==1 & abnSVGmag==1

mi passive: gen score41 =1 if abnSAI==1 & abnQRS_T==1 & abnSVGel==1 & abnSVGaz==1 & abnSVGmag==0
mi passive: gen score42 =1 if abnSAI==1 & abnQRS_T==1 & abnSVGel==1 & abnSVGaz==0 & abnSVGmag==1
mi passive: gen score43 =1 if abnSAI==1 & abnQRS_T==1 & abnSVGel==0 & abnSVGaz==1 & abnSVGmag==1
mi passive: gen score44 =1 if abnSAI==1 & abnQRS_T==0 & abnSVGel==1 & abnSVGaz==1 & abnSVGmag==1
mi passive: gen score45 =1 if abnSAI==0 & abnQRS_T==1 & abnSVGel==1 & abnSVGaz==1 & abnSVGmag==1

mi passive: gen score3 = 1 if score31==1 | score32==1 | score33==1 | score34==1 | score35==1 | score36==1 | score37==1 | score38==1 |  score39==1
 recode score3 (.=0)

mi passive: gen score2 = 1 if score21==1 | score22==1 | score23==1 | score24==1 | score25==1 | score26==1 | score27==1 | score28==1 | score29==1 | score20==1
 recode score2 (.=0)

mi passive: gen score4 = 1 if score41==1 | score42==1 | score43==1 | score44==1 | score45==1 
 recode score4 (.=0)

mi passive: gen score5 = 1 if abnSVGaz==1 & abnSVGel==1 & abnQRS_T==1 & abnSAI==1 & abnSVGmag==1
 recode score5 (.=0)

mi passive: gen score = score1
 recode score (1=2) if score2==1
 recode score (1=3) if score3==1
 recode score (1=4) if score4==1
 recode score (1=5) if score5==1
 
mi passive: egen white =  anymatch(race), values(1)

mi passive: gen beta = (0.7066*age/10) - (0.9851*sex) - (1.0180*white) + (1.1734*diabetes) + (0.8452*htn) + (1.1298*chd) + (0.6525*cva) + (0.9126*SAIQRST/100) + (0.3923*areaQRSTAngle_deg/10) + (0.1199*sqrtSVGel) - (0.0566*areaQRSTAngle_deg*age/10) + (0.0908*areaQRSTAngle_deg*white/10) - (0.0534*areaQRSTAngle_deg*diabetes/10) - (0.0481*areaQRSTAngle_deg*htn/10) + (0.0882*sqrtSVGaz*sex) - (0.1030*SAIQRST*age/100)

mi passive: gen GEHscore = 1 - (0. 999966^exp(beta))

mi estimate : proportion qGEHscore
mi estimate : proportion score


