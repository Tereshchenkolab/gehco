***STATA version 18
***STATA code written by Larisa Tereshchenko <tereshl@ccf.org>
***September-October 2023
***analysis of GEHCO study data

use "your_datafile_imputed66.dta"

***descriptive statistics

mi estimate : mean areaQRSTAngle_deg, over(qaQRSTa)
mi estimate : total areaQRSTAngle_deg, over(qaQRSTa)
mi estimate : regress areaQRSTAngle_deg i.qaQRSTa
mi estimate : regress areaQRSTAngle_deg i.qaQRSTa ib(none).qaQRSTa
mi estimate : mean agey , over(qaQRSTa)
mi estimate : proportion sex , over( qaQRSTa )
mi estimate : proportion race , over( qaQRSTa )
mi estimate : proportion cm , over( qaQRSTa )
mi estimate : mean exact_lvef , over(qaQRSTa)
mi estimate : proportion nyha , over( qaQRSTa )
mi estimate : proportion mi , over( qaQRSTa )
mi estimate : proportion revascularization , over( qaQRSTa )
mi estimate : proportion htn , over( qaQRSTa )
mi estimate : proportion diabetes , over( qaQRSTa )
mi estimate : proportion af , over( qaQRSTa )
mi estimate : proportion aa_usage , over( qaQRSTa )
mi estimate : proportion cva , over( qaQRSTa )
mi estimate : proportion ACEI , over( qaQRSTa )
mi estimate : proportion bb_usage , over( qaQRSTa )
mi estimate : mean egfr , over(qaQRSTa)
mi estimate : mean bun , over(qaQRSTa)
mi estimate : proportion icd_type , over( qaQRSTa )
mi estimate : proportion icd_replacement , over( qaQRSTa )
mi estimate : proportion vt_zone_option , over( qaQRSTa )
mi estimate : proportion atp , over( qaQRSTa )
mi estimate : mean vt_zone_bpm , over(qaQRSTa)
mi estimate : mean vf_zone_bpm , over(qaQRSTa)
mi estimate : proportion icd_progr , over( qaQRSTa )
mi estimate : proportion manufacturer , over( qaQRSTa )
mi estimate : mean HRbpm , over(qaQRSTa)
mi estimate : mean QRSduration_ms , over(qaQRSTa)
mi estimate : mean zQTch , over(qaQRSTa)
mi estimate : mean QTch , over(qaQRSTa)
mi estimate : mean areaQRSTAngle_deg , over(qaQRSTa)
mi estimate : mean areaSVGElevation_deg , over(qaQRSTa)
mi estimate : mean aSVGaz , over(qaQRSTa)
mi estimate : mean areaSVG_mVms , over(qaQRSTa)
mi estimate : mean SAIQRST , over(qaQRSTa)
mi estimate : mean agey , over( qaSVGel )
mi estimate : proportion sex , over( qaSVGel )
mi estimate : proportion race , over( qaSVGel )
mi estimate : proportion cm , over( qaSVGel )
mi estimate : proportion nyha , over( qaSVGel )
mi estimate : mean exact_lvef , over(qaQRSTa)
mi estimate : mean exact_lvef , over( qaSVGel )
mi estimate : mean egfr , over( qaSVGel )
mi estimate : proportion htn , over( qaSVGel )
mi estimate : proportion diabetes , over( qaSVGel )
mi estimate : proportion af , over( qaSVGel )
mi estimate : proportion cva , over( qaSVGel )
mi estimate : proportion aa_usage , over( qaSVGel )
mi estimate : proportion bb_usage , over( qaSVGel )
mi estimate : proportion revascularization , over( qaSVGel )
mi estimate : mean bun , over( qaSVGel )
mi estimate : proportion icd_type , over( qaSVGel )
mi estimate : proportion icd_replacement , over( qaSVGel )
mi estimate : proportion vt_zone_option , over( qaSVGel )
mi estimate : proportion atp , over( qaSVGel )
mi estimate : mean vt_zone_bpm , over( qaSVGel )
mi estimate : mean vf_zone_bpm , over( qaSVGel )
mi estimate : proportion icd_progr , over( qaSVGel )
mi estimate : mean HRbpm , over( qaSVGel )
mi estimate : mean QRSduration_ms , over( qaSVGel )
mi estimate : mean zQTch , over( qaSVGel )
mi estimate : mean QTch , over( qaSVGel )
mi estimate : mean areaQRSTAngle_deg , over( qaSVGel )
mi estimate : mean areaSVGElevation_deg , over( qaSVGel )
mi estimate : mean aSVGaz , over( qaSVGel )
mi estimate : mean areaSVG_mVms , over( qaSVGel )
mi estimate : mean SAIQRST , over( qaSVGel )
mi estimate : mean agey , over( qaSVGaz )
mi estimate : proportion sex , over( qaSVGaz )
mi estimate : proportion race , over( qaSVGaz )
mi estimate : proportion cm , over( qaSVGaz )
mi estimate : mean exact_lvef , over( qaSVGaz )
mi estimate : proportion nyha , over( qaSVGaz )
mi estimate : proportion revascularization , over( qaSVGaz )
mi estimate : proportion htn , over( qaSVGaz )
mi estimate : proportion diabetes , over( qaSVGaz )
mi estimate : proportion af , over( qaSVGaz )
mi estimate : proportion cva , over( qaSVGaz )
mi estimate : proportion aa_usage , over( qaSVGaz )
mi estimate : proportion bb_usage , over( qaSVGaz )
mi estimate : mean egfr , over( qaSVGaz )
mi estimate : mean bun , over( qaSVGaz )
mi estimate : proportion icd_type , over( qaSVGaz )
mi estimate : proportion icd_replacement , over( qaSVGaz )
mi estimate : proportion vt_zone_option , over( qaSVGaz )
mi estimate : proportion atp , over( qaSVGaz )
mi estimate : mean vt_zone_bpm , over( qaSVGaz )
mi estimate : mean vf_zone_bpm , over( qaSVGaz )
mi estimate : proportion icd_progr , over( qaSVGaz )
mi estimate : mean HRbpm , over( qaSVGaz )
mi estimate : mean QRSduration_ms , over( qaSVGaz )
mi estimate : mean QTch , over( qaSVGaz )
mi estimate : mean areaQRSTAngle_deg , over( qaSVGaz )
mi estimate : mean areaSVGElevation_deg , over( qaSVGaz )
mi estimate : mean aSVGaz , over( qaSVGaz )
mi estimate : mean areaSVG_mVms , over( qaSVGaz )
mi estimate : mean SAIQRST , over( qaSVGaz )
mi estimate : mean agey , over( qaSVGmag )
mi estimate : proportion sex , over( qaSVGmag )
mi estimate : proportion race , over( qaSVGmag )
mi estimate : proportion cm , over( qaSVGmag )
mi estimate : mean exact_lvef , over( qaSVGmag )
mi estimate : proportion nyha , over( qaSVGmag )
mi estimate : proportion revascularization , over( qaSVGmag )
mi estimate : proportion htn, over( qaSVGmag )
mi estimate : proportion diabetes , over( qaSVGmag )
mi estimate : proportion af , over( qaSVGmag )
mi estimate : proportion cva , over( qaSVGmag )
mi estimate : proportion aa_usage , over( qaSVGmag )
mi estimate : proportion bb_usage , over( qaSVGmag )
mi estimate : mean egfr , over( qaSVGmag )
mi estimate : mean bun , over( qaSVGmag )
mi estimate : proportion icd_type , over( qaSVGmag )
mi estimate : proportion icd_replacement , over( qaSVGmag )
mi estimate : proportion vt_zone_option , over( qaSVGmag )
mi estimate : proportion atp , over( qaSVGmag )
mi estimate : mean vt_zone_bpm , over( qaSVGmag )
mi estimate : mean vf_zone_bpm , over( qaSVGmag )
mi estimate : proportion icd_progr , over( qaSVGmag )
mi estimate : mean HRbpm , over( qaSVGmag )
mi estimate : mean QRSduration_ms , over( qaSVGmag )
mi estimate : mean QTch , over( qaSVGmag )
mi estimate : mean areaQRSTAngle_deg , over( qaSVGmag )
mi estimate : mean areaSVGElevation_deg , over( qaSVGmag )
mi estimate : mean aSVGaz , over( qaSVGmag )
mi estimate : mean areaSVG_mVms , over( qaSVGmag )
mi estimate : mean SAIQRST  , over( qaSVGmag )
mi estimate : mean agey  , over( qSAI )
mi estimate : proportion sex , over( qSAI  )
mi estimate : proportion race, over( qSAI  )
mi estimate : proportion cm, over( qSAI  )
mi estimate : mean exact_lvef  , over( qSAI )
mi estimate : proportion nyha, over( qSAI  )
mi estimate : proportion revascularization , over( qSAI  )
mi estimate : proportion htn , over( qSAI  )
mi estimate : proportion diabetes , over( qSAI  )
mi estimate : proportion af , over( qSAI  )
mi estimate : proportion cva , over( qSAI  )
mi estimate : proportion aa_usage , over( qSAI  )
mi estimate : proportion bb_usage , over( qSAI  )
mi estimate : mean egfr  , over( qSAI )
mi estimate : mean bun , over( qSAI )
mi estimate : proportion icd_type , over( qSAI  )
mi estimate : proportion icd_replacement , over( qSAI  )
mi estimate : proportion vt_zone_option , over( qSAI  )
mi estimate : mean vt_zone_bpm , over( qSAI )
mi estimate : mean vf_zone_bpm , over( qSAI )
mi estimate : proportion atp , over( qSAI  )
mi estimate : proportion icd_progr , over( qSAI  )
mi estimate : mean HRbpm , over( qSAI )
mi estimate : mean QRSduration_ms , over( qSAI )
mi estimate : mean QTch , over( qSAI )
mi estimate : mean areaQRSTAngle_deg , over( qSAI )
mi estimate : mean areaSVGElevation_deg , over( qSAI )
mi estimate : mean aSVGaz , over( qSAI )
mi estimate : mean areaSVG_mVms , over( qSAI )
mi estimate : mean SAIQRST  , over( qSAI )



***survival analysis (cause-specigic Cox models) with MMVT (VTtype==1) or PVT/VF outcome (VTtype==2)

**setup outcome of interest
mi stset ttcompev, id(study_id) failure(VTtype==2)

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cm
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.mi
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.revascularization
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.htn
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.diabetes
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.af
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.cva
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.aa_usage
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.bb_usage
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.ccbs
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.icd_type
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.manufacturer
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.atp
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place ib2.vt_zone_option
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.ACEI
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.ARB
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.AldosteroneAnt
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.nyha
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zbun
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zegfr
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zvtzone
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zvfzone
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.progr
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zHR
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zRR
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zQTch i.PVCany i.PVCdistort zRR
***how_many_imputations
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

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib2.qaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib2.q4aSVGaz

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGel 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGel 

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zaSVGmag 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR zpSVGmag 

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib2.qpSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place i.PVCany i.PVCdistort zRR ib2.qpSVGaz



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
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.progr

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB  zHR
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.PVCany
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zQTch
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zQRS

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zaQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zpQRSTa

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zSAI
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zVMQTi

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zpSVGaz

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany ib2.qaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany ib2.q4aSVGaz

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zaSVGel
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zpSVGel

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zaSVGmag
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB zRR i.PVCdistort i.PVCany zpSVGmag



***model 3
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.mi
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.revascularization
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.ccbs
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr i.AldosteroneAnt
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zbun
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.atp
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr ib2.vt_zone_option

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zHR
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr i.PVCany

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zRR i.PVCdistort i.PVCany zQRS

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr  zRR i.PVCdistort i.PVCany zQTch

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpQRSTa

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGel
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGel

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGaz

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib2.qaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib2.q4aSVGaz

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zSAI
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zVMQTi

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zaSVGmag
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zpSVGmag

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib2.qpSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany ib2.q4pSVGaz



***model 4 in addition add QRS, QTch, RR 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch 
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.mi
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.revascularization
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.ccbs
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch i.AldosteroneAnt
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch zbun
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.atp
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zHR i.PVCdistort i.PVCany zQRS zQTch ib2.vt_zone_option

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zaQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zpQRSTa
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zaSVGel
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zpSVGel
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zaSVGmag
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zpSVGmag
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zSAI
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zVMQTi

mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch zpSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch ib2.qaSVGaz
mi estimate, eform("Haz. Ratio") : stcox zage ib2.sex i.race i.mbeat3 i.place zlvef i.nyha ib2.cm ib2.htn ib2.diabetes ib2.cva ib2.af zegfr ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type zvtzone zvfzone i.progr zRR i.PVCdistort i.PVCany zQRS zQTch ib2.q4aSVGaz

**lasso Cox example on M0 dataset
stset ttcompevy, id(study_id) failure(VTtype==1) scale(1)
lasso cox c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, selection(cv, alllambdas) grid(100)
estimates store cv
cvplot
lassoinfo
coefpath
help coefpath
coefpath selection(cv)
predict y_cv
lassoinfo
lassoknots
lassocoef cv, display(coef)
cvplot

***Fine-Gray competing risk models using imputed dataset (M66)
mi stset ttcompev, id(study_id) failure(VTtype==1)
mi estimate, saving(miest_mmvt): stcrreg c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm ib2.progr  i.PVCany QTch ib2.revascularization ib2.mi i.ccbs i.AldosteroneAnt c.bun ib2.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef  QRSduration_ms aSVGaz egfr, compete(VTtype==2 3 4)

mi stset ttcompev, id(study_id) failure(VTtype==2)
mi estimate, saving(miest_pvtvf): stcrreg c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm ib2.progr  i.PVCany QTch ib2.revascularization ib2.mi i.ccbs i.AldosteroneAnt c.bun ib2.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, compete(VTtype==1 3 4)

***Competing outcome: VTtype
***VTtype is: 1=MMVT 2=PVTVF 3=undefined VTVF 4=death w/o VTVF 5=censored ***time to competing event: ttcompev

***35 input variables:c.agey ib2.sex i.race i.mbeat3 i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm ib2.progr i.PVCany QTch ib2.revascularization ib2.mi i.ccbs i.AldosteroneAnt c.bun ib2.atp c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr

mi stset ttcompev, id(study_id) failure(VTtype==1)
mi estimate, saving(miest_mmvt_full): stcox c.agey ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization  ib2.htn ib2.diabetes  ib2.af ib2.cva egfr  c.bun  ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt  i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr  i.manufacturer HRbpm  i.mbeat3  i.PVCany  QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg aSVGaz c.areaSVG_mVms c.SAIQRST  
mi predict cox_mmvt_full using "D:\Lora\my papers_new\2023\2023_GEHCO_MMVT_PVTVF\stata_data\miest_mmvt_full.ster", xb
mi passive: gen prMMVT_coxfull =exp( cox_mmvt_full )/(1+exp( cox_mmvt_full ))
brier MMVT prMMVT_coxfull , group(10)

mi estimate, saving(miest_mmvt_age): stcox ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization  ib2.htn ib2.diabetes  ib2.af ib2.cva egfr  c.bun  ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt  i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr  i.manufacturer HRbpm  i.mbeat3  i.PVCany  QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg aSVGaz c.areaSVG_mVms c.SAIQRST
mi predict cox_mmvt_age using "D:\Lora\my papers_new\2023\2023_GEHCO_MMVT_PVTVF\stata_data\miest_mmvt_age.ster", xb
mi passive: gen prMMVT_cox_age =exp( cox_mmvt_age )/(1+exp( cox_mmvt_age ))
brier MMVT prMMVT_cox_age , group(10)

mi estimate, saving(miest_mmvt_sex): stcox agey i.race exact_lvef i.nyha i.mi i.revascularization  ib2.htn ib2.diabetes  ib2.af ib2.cva egfr  c.bun  ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt  i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr  i.manufacturer HRbpm  i.mbeat3  i.PVCany  QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg aSVGaz c.areaSVG_mVms c.SAIQRST
mi predict cox_mmvt_sex using "D:\Lora\my papers_new\2023\2023_GEHCO_MMVT_PVTVF\stata_data\miest_mmvt_sex.ster", xb
mi passive: gen prMMVT_cox_sex =exp( cox_mmvt_sex )/(1+exp( cox_mmvt_sex ))
brier MMVT prMMVT_cox_sex , group(10)

mi estimate, saving(miest_mmvt_race): stcox agey ib2.sex exact_lvef i.nyha i.mi i.revascularization  ib2.htn ib2.diabetes  ib2.af ib2.cva egfr  c.bun  ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt  i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr  i.manufacturer HRbpm  i.mbeat3  i.PVCany  QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg aSVGaz c.areaSVG_mVms c.SAIQRST



****to calculate IPA

***setup outcome of interest before the analysis
stset ttcompevy, id(study_id) failure(VTtype==1) scale(1)
stset ttcompevy, id(study_id) failure(VTtype==2) scale(1)

***null competing risk model 3y 
stbrier , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)

***full competing risk model 
stbrier c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)

***IPA = 1 − (model Brier score/null model Brier score)
di 1-(8.41/9.21)

***remove age
stbrier ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.49/9.21)
di 8.69-7.82

***remove sex
stbrier agey  i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.407/9.21)
di 8.69-8.719

**remove race
stbrier agey  ib2.sex i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.35/9.21)
di 8.69-9.338

***remove LVEF
stbrier agey  ib2.sex i.race  i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.47/9.21)
di 8.69-8.03

***remove NYHA
stbrier agey  ib2.sex i.race exact_lvef i.mbeat3  ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.42/9.21)
di 8.69-8.58

***remove MI
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mbeat3  ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.41/9.21)
di 8.69-8.686

***remove revascularization
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.mbeat3  ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.449/9.21)
di 8.69-8.263

***remove hypertension
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization i.mbeat3 ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.482/9.21)
di 8.69-7.90

***remove diabetes
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.mbeat3 ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.387/9.21)
di 8.69-8.936

***remove AF Hx
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes i.mbeat3 ib2.cva  ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.437/9.21)
di 8.69-8.393

***remove stroke Hx
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af i.mbeat3   ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.40/9.21)
di 8.69-8.795

***remove eGFR
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva i.mbeat3   ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.487/9.21)
di 8.69-7.85

***remove BUN
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr  i.mbeat3   ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.446/9.21)
di 8.69-8.295

***remove BB use
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun i.mbeat3  ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.42/9.21)
di 8.69-8.5776

***remove AAD use
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage  i.mbeat3 i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.412/9.21)
di 8.69-8.664

***remove ACEI
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.mbeat3 i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.469/9.21)
di 8.69-8.046

***remove ARBs
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.mbeat3 i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.491/9.21)
di 8.69-7.807

***remove CCB use
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.mbeat3 i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(9.033/9.21)
di 8.69-1.92

***remove aldosterone antagonists
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.mbeat3 i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.458/9.21)
di 8.69-8.165

****remove device type ICD vs CRT-D
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.mbeat3 i.manufacturer vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.458/9.21)
di 8.69-8.165

***remove ATP 
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type  i.mbeat3 i.manufacturer vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.412/9.21)
di 8.69-8.664

***remove VT zone bpm
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 i.manufacturer vf_zone_bpm HRbpm i.progr  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.374/9.21)
di 8.69-9.077

***remove VF zone bpm
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 i.manufacturer vt_zone_bpm HRbpm i.progr  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.414/9.21)
di 8.69-8.643

***remove delay ICD therapy programming
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 i.manufacturer vt_zone_bpm vf_zone_bpm HRbpm  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(9.033/9.21)
di 8.69-1.92

***remove device manufacturer
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 vt_zone_bpm vf_zone_bpm i.progr HRbpm  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.586/9.21)
di 8.69-6.775

***remove heart rate
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 vt_zone_bpm vf_zone_bpm i.progr i.manufacturer  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.404/9.21)
di 8.69-8.751

***remove median beat type
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.419/9.21)
di 8.69-8.588

***remove PVC on ECG
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.546/9.21)
di 8.69-7.21

***remove QRS duration
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.424/9.21)
di 8.69-8.534

***remove QTch
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.459/9.21)
di 8.69-8.154

***remove area QRS-T angle
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch  c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.4136/9.21)
di 8.69-8.647

***remove area SVG elevation
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVG_mVms c.SAIQRST aSVGaz , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.378/9.21)
di 8.69-9.034

***remove area SVG azimuth
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.419/9.21)
di 8.69-8.588

***remove area SVG mag
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg aSVGaz c.SAIQRST , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.5/9.21)
di 8.69-7.709

***remove SAI QRST
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg aSVGaz c.areaSVG_mVms  , bt(3.0) comp(VTtype==2 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(8.41/9.21)
di 8.69-8.686

****for PVT VF outcome
stset ttcompevy, id(study_id) failure(VTtype==2) scale(1)
***null model
stbrier if y_PVTVF_noGEH!=., bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
***full model
stbrier c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.216/2.941)

***remove agey
stbrier  ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.328/2.941)
di 24.651-20.843

**remove sex
stbrier agey  i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.212/2.941)
di 24.651-24.787

**remove race
stbrier agey ib2.sex  i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.308/2.941)
di 24.651-21.523

***remove LVEF
stbrier agey  ib2.sex i.race  i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.248/2.941)
di 24.651-23.563

**remove NYHA
stbrier agey  ib2.sex i.race exact_lvef i.mbeat3  ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.248/2.941)
di 24.651-23.563

**remove MI
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mbeat3  ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.245/2.941)
di 24.651-23.665

***remove revascularization
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.mbeat3  ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.283/2.941)
di 24.651-22.373

**remove HTN
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization i.mbeat3 ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.223/2.941)
di 24.651-24.413

***remove diabetes
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.mbeat3 ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.232/2.941)
di 24.651-24.107

**remove AF Hx
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes i.mbeat3 ib2.cva  ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.256/2.941)
di 24.651-23.291

***remove stroke Hx
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af i.mbeat3   ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz egfr, bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.223/2.941)
di 24.651-24.413

**remove eGFR
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva i.mbeat3   ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.26/2.941)
di 24.651-23.155

***remove BUN
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr  i.mbeat3   ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.231/2.941)
di 24.651-24.141

**remove BB use
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun i.mbeat3  ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.217/2.941)
di 24.651-24.614

**remove AADs
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage  i.mbeat3 i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.383/2.941)
di 24.651-18.973

**remove ACEI
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.mbeat3 i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.198/2.941)
di 24.651-25.26

***remove ARB Use
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.mbeat3 i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.ccbs i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.203/2.941)
di 24.651-25.09

***remove CCB use
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.mbeat3 i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  i.AldosteroneAnt i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.218/2.941)
di 24.651-24.583

***remove aldosterone antagonists
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.mbeat3 i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.204/2.941)
di 24.651-25.06

***remove devce type
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.mbeat3 i.manufacturer vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.216/2.941)
di 24.651-24.651

***remove ATP ON
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type  i.mbeat3 i.manufacturer vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.217/2.941)
di 24.651-24.617

**remove VT zone bpm
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 i.manufacturer vf_zone_bpm HRbpm i.progr  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.211/2.941)
di 24.651-24.821

***remove VF zone
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 i.manufacturer vt_zone_bpm HRbpm i.progr  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.229/2.941)
di 24.651-24.209

***remove delay ICD therapy programming
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 i.manufacturer vt_zone_bpm vf_zone_bpm HRbpm  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.269/2.941)
di 24.651-22.849

***remove device manufacturer
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 vt_zone_bpm vf_zone_bpm i.progr HRbpm  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.358/2.941)
di 24.651-19.82

***remove heart rate now
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp  i.mbeat3 vt_zone_bpm vf_zone_bpm i.progr i.manufacturer  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.313/2.941)
di 24.651-21.353

***remove median beat type
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm  i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.32/2.941)
di 24.651-21.115

***remove PVF on current ECG
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST QRSduration_ms aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.242/2.941)
di 24.651-23.767

***remove QRS duration
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QTch  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.228/2.941)
di 24.651-24.243

**remove QTch
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.208/2.941)
di 24.651-24.923

***remove QRS-T angle
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch  c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.254/2.941)
di 24.651-23.359

***remove SVG elevation
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVG_mVms c.SAIQRST aSVGaz , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.213/2.941)
di 24.651-24.753

***remove SVG azimuth
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.199/2.941)
di 24.651-25.23

***remove SVG mag
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg aSVGaz c.SAIQRST , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.222/2.941)
di 24.651-24.447

***remove SAIQRST
stbrier agey  ib2.sex i.race exact_lvef i.nyha i.mi i.revascularization ib2.htn i.diabetes ib2.af ib2.cva egfr bun ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.ccbs i.AldosteroneAnt i.icd_type i.atp vt_zone_bpm vf_zone_bpm i.progr i.manufacturer HRbpm i.mbeat3 i.PVCany QRSduration_ms QTch c.areaQRSTAngle_deg c.areaSVGElevation_deg aSVGaz c.areaSVG_mVms  , bt(3.0) comp(VTtype==1 3 4) ipcw(c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm i.progr  i.PVCany QTch i.revascularization i.mi i.ccbs i.AldosteroneAnt c.bun i.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr)
di 1-(2.19/2.941)
di 24.651-25.536



***to determine baseline CIF for calculator
**for MMVT
stset ttcompevy, id(study_id) failure(VTtype==1) scale(1)
stcrreg c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm ib2.progr  i.PVCany QTch ib2.revascularization ib2.mi i.ccbs i.AldosteroneAnt c.bun ib2.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, comp(VTtype==2 3 4)
stcrreg, noshr
predict cif_base_MMVT, basecif
predict xbeta_cif_MMVT , xb
gen cifbase_MMVT = 1 - cif_base_MMVT 
gen comp_MMVT = 1 - cifbase_MMVT^exp( xbeta_cif_MMVT )
sum cifbase_MMVT if _t<3
scalar cifbase_MMVT3y=r(min)
gen comp_MMVT3y = 1 - cifbase_MMVT3y^exp( xbeta_cif_MMVT )
sum comp_MMVT3y
egen q10compMMVTT3y =  xtile( comp_MMVT3y ), n(10)

**for PVT/VF
stset ttcompevy, id(study_id) failure(VTtype==2) scale(1)
stcrreg c.agey ib2.sex i.race i.mbeat3  i.nyha ib2.htn ib2.diabetes ib2.cva ib2.af ib2.bb_usage ib2.aa_usage i.ACEI i.ARB i.manufacturer i.icd_type vt_zone_bpm vf_zone_bpm HRbpm ib2.progr  i.PVCany QTch ib2.revascularization ib2.mi i.ccbs i.AldosteroneAnt c.bun ib2.atp  c.areaQRSTAngle_deg c.areaSVGElevation_deg c.areaSVG_mVms c.SAIQRST exact_lvef QRSduration_ms aSVGaz egfr, comp(VTtype==1 3 4)
stcrreg, noshr
predict cif_base_PVTVF, basecif
predict xbeta_cif_PVTVF , xb
gen cifbase_PVTVF = 1 - cif_base_PVTVF
gen comp_PVTVF = 1 - cifbase_PVTVF^exp( xbeta_cif_PVTVF )
sum cifbase_PVTVF if _t<3
scalar cifbase_PVTVF3y=r(min)
gen comp_PVTVF3y = 1 - cifbase_PVTVF3y^exp( xbeta_cif_PVTVF )
egen q10compPVTVF3y =  xtile( comp_PVTVF3y ), n(10)

***to calculate fime-updated ROC(t)AUC
***make sure to setup survival outcome of interest before running ROC(t)AUC. 
bootstrap e(AUC), reps(1000) seed(4567): stroccurve comp_MMVT3y , time(3)
bootstrap e(AUC), reps(1000) seed(4567): stroccurve comp_PVTVF3y , time(3)

