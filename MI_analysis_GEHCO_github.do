***STATA version 16
***STATA code written by Larisa Tereshchenko 
***October 2020
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


***survival analysis with VTVF outcome model 1
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
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place znidnum
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zaQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zSAI
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zaSVGel
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place 

***model 2
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.mi
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.revascularization
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.ccbs
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.AldosteroneAnt
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zbun
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.icd_type
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.atp
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB ib2.vt_zone_option
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zvtzone
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zvfzone
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB znidnum
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zaQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zSAI
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zaSVGel
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zSVGmag

***model 3
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.mi
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.revascularization
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.ccbs
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.AldosteroneAnt
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zbun
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.atp
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.vt_zone_option
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone znidnum
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaSVGel
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zSAI
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zSVGmag

***difference in point estimates test 
***difference in model1
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place, nohr
di (-.1257723 - (.4787754 ))/(sqrt((.0489931)^2+(.0573566 )^2))
di (.4868499 - (.1569927 ))/(sqrt((.1310206)^2+(.1269013 )^2))
di ( .3525988- (.1431337 ))/(sqrt(( .1510923)^2+(.1876593 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cm, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cm, nohr
di ( .2445938  - (.3790418 ))/(sqrt((.1074786)^2+(.1105025 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.diabetes, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.diabetes, nohr
di ( -.0094346  - ( .5913449 ))/(sqrt(( .0919144)^2+( .0942881  )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.af, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.af, nohr
di (.2245353  - (  .3861749 ))/(sqrt((.1085306)^2+( .1085454  )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cva, nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cva, nohr
di ( .2016675  - ( .1642933 ))/(sqrt((.1383535)^2+(.1508146  )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.aa_usage , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.aa_usage , nohr
di ( .4531523  - ( .3710136 ))/(sqrt((.1253254)^2+( .127052  )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.bb_usage , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.bb_usage , nohr
di (.1692148  - ( -.3293573 ))/(sqrt(( .1416137 )^2+(  .1231425 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef , nohr
di (.1692148  - ( -.1943237))/(sqrt(( .0549633 )^2+( .0636763)^2))
di ( -.1943237  - ( -.3013776))/(sqrt(( .0549633 )^2+( .0636763)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.nyha , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.nyha , nohr
di (  .213436 - ( .4818944 ))/(sqrt((.1377188)^2+( .1698902)^2))
di ( .3259127  - (.9178136))/(sqrt((.1386501)^2+(.1657768)^2))
di ( .2485975   - ( 1.463442))/(sqrt(( .3252674 )^2+(.2436959)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zegfr , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zegfr , nohr
di ( -.1766593  - (  -.4409069))/(sqrt((.0534025)^2+(.0653462)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.ACEI , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.ACEI , nohr
di ( .129345  - ( -.30513))/(sqrt(( .1007814)^2+(.1021729)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.ARB , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.ARB , nohr
di (   -.1994261  - ( -.0678804))/(sqrt((  .124116)^2+( .1271094)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.icd_type , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.icd_type , nohr
di (  -.028832  - (  -.2012047))/(sqrt((  .120622)^2+(.1352665)^2))
di (  -.2714174 - (-.0894969))/(sqrt((  .1249708 )^2+(.1289659)^2))
di ( .8628325 - ( -.0761297))/(sqrt(( .4409793 )^2+(.9951813)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zvtzone , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zvtzone , nohr
di (  -.2230387 - ( .0316657))/(sqrt((  .0661154 )^2+(.0619399)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zvfzone , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zvfzone , nohr
di (  .0567697 - (  .0248766 ))/(sqrt((.0252814 )^2+(.0505902)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.mi , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.mi , nohr
di ( .2889083 - (  .2456935 ))/(sqrt((.0963455)^2+(.10343)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.revascularization , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.revascularization , nohr
di ( .2496289 - ( .2221941 ))/(sqrt((.0949224)^2+(.1004114)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.ccbs , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.ccbs , nohr
di ( -.2323825 - ( .0815354 ))/(sqrt((.1795314)^2+( .1696953)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.AldosteroneAnt , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place i.AldosteroneAnt , nohr
di ( .1358 - (  -.0720608 ))/(sqrt(( .1001189)^2+(.113187 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zbun , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zbun , nohr
di ( .1147518- ( .3595239 ))/(sqrt((  .0447831)^2+( .0378802 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.atp , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.atp , nohr
di (  -.1654739- ( -.2709682 ))/(sqrt(( .1488495)^2+(.1295142 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.vt_zone_option , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place ib2.vt_zone_option , nohr
di (  -.0266696- ( -.3245812 ))/(sqrt((.1732226)^2+( .1473253 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place znidnum , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place znidnum , nohr
di (-.1066679- ( -.0904314))/(sqrt(( .0721655)^2+( .0746446 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zaQRSTa , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zaQRSTa , nohr
di ( .179888- ( .1916271 ))/(sqrt((.0534164)^2+(  .059497 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zSAI , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zSAI , nohr
di (  -.120238- (  -.0273951 ))/(sqrt(( .0505618)^2+(.0520563 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zaSVGel , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zaSVGel , nohr
di (   .1404696- (  .0496008 ))/(sqrt(( .0451047)^2+( .0503488)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zaSVGaz , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zaSVGaz , nohr
di (   -.010938- (  .0788198 ))/(sqrt(( .0466766)^2+(  .050372)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zSVGmag , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zSVGmag , nohr
di ( -.345104- (  -.2353622 ))/(sqrt(( .0576111)^2+(  .06156)^2))

***difference in model 3
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaSVGel , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaSVGel , nohr
di (  .1539695- ( .0394742 ))/(sqrt((.0450053)^2+( .0520884)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaQRSTa , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaQRSTa , nohr
di (  .198785- ( .1891057 ))/(sqrt((.0579494)^2+(.0669751)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zSAI , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zSAI , nohr
di (  -.0644255- ( .0261313 ))/(sqrt(( .0576675)^2+( .056695)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zSVGmag , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zSVGmag , nohr
di ( -.2944214- (-.1535932))/(sqrt(( .060613)^2+( .0658771)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaSVGaz , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zaSVGaz , nohr
di (  -.0106959- ( .0422862))/(sqrt(( .0470357)^2+(.0513282)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone znidnum , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone znidnum , nohr
di (  -.0877531- ( -.1034895))/(sqrt(( .0774905)^2+( .0892584)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.vt_zone_option , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.vt_zone_option , nohr
di ( .056014- (  -.3602711))/(sqrt((  .1898865)^2+(  .1640725)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.atp , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.atp , nohr
di ( -.0532501- (  -.27973))/(sqrt(( .1517522)^2+( .1386747)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zbun , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone zbun , nohr
di ( -.0026547- (.2020891 ))/(sqrt(( .0661319)^2+(.0586275)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.AldosteroneAnt , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.AldosteroneAnt , nohr
di (  .0661527- ( -.2462684 ))/(sqrt((  .1064951)^2+( .1167129)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.ccbs , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.ccbs , nohr
di ( -.1975724- ( .1136879))/(sqrt((  .1878621)^2+( .1748677)^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.revascularization , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.revascularization , nohr
di (  .1691533 - (-.0888844))/(sqrt(( .1327428)^2+( .1303727 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.mi , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone ib2.mi , nohr
di (  .1637547 - ( -.1063144))/(sqrt(( .1830822)^2+( .1642024 )^2))
mi stset ttcompev, id(study_id) failure(compev==1)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone , nohr
mi stset ttcompev, id(study_id) failure(compev==2)
mi estimate: stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone , nohr

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


