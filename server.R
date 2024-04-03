#version tereshl@ccf.org dated 3 Apr 2024
#calling all the libraries that would be required to run the app
library(shiny)
library(DT)
library(shinyWidgets)
library(shinyjs)

#following code connects the ui to the server
shinyServer(function(input, output){
  data <- eventReactive(input$goButton, {
    #event reactive is used for reactive expressions
    #displays error message when age is not within the placeholder mentioned in the ui side
    #age should be a numeric value else shows it is invalid
    validate(need(!is.na(as.numeric(input$agey))& as.numeric(input$agey)>=18& as.numeric(input$agey)<=100, 'Please input a valid Age.'))
    agey= as.numeric(input$agey)
    
    sex = factor(input$sex, levels = c('Female', 'Male'))
    race = factor(input$race, levels = c('Non-Hispanic White','Hispanic White', 'Black','Asian'))
    htn = factor(input$htn, levels = c('No', 'Yes'))
    diabetes = factor(input$diabetes, levels = c('No', 'Yes'))
    cva=factor(input$cva, levels = c('No', 'Yes'))
    af= factor(input$af, levels = c('No', 'Yes'))
    mi = factor(input$mi, levels = c('No', 'Yes'))
    revascularization = factor(input$revascularization, levels = c('No', 'Yes'))
    nyha = factor(input$nyha, levels = c('Class I','Class II', 'Class III','Class IV'))
    
    validate(need(!is.na(as.numeric(input$exact_lvef))& as.numeric(input$exact_lvef)>=7 & as.numeric(input$exact_lvef)<=77, 'Please input a valid Left Ventricular Ejection Fraction (%).'))
    exact_lvef= as.numeric(input$exact_lvef)
    
    validate(need(!is.na(as.numeric(input$bun))& as.numeric(input$bun)>=2 & as.numeric(input$bun)<=143, 'Please input a valid BUN (mg/dL).'))
    bun= as.numeric(input$bun)
    
    validate(need(!is.na(as.numeric(input$egfr))& as.numeric(input$egfr)>=0.5 & as.numeric(input$egfr)<=291, 'Please input a valid eGFR CKD-EPI (mL/min/1.73 m^2.'))
    egfr= as.numeric(input$egfr)
    
    bb_usage = factor(input$bb_usage, levels = c('No', 'Yes'))
    aa_usage = factor(input$aa_usage, levels = c('No', 'Yes'))
    ACEI = factor(input$ACEI, levels = c('No', 'Yes'))
    ARB = factor(input$ARB, levels = c('No', 'Yes'))
    ccbs = factor(input$ccbs, levels = c('No', 'Yes'))
    aldosteroneAnt=factor(input$aldosteroneAnt, levels = c('No', 'Yes'))
    manufacturer=factor(input$manufacturer, levels =  c('Medtronic', 'Guidant/Boston Scientific','St. Jude/Abbott',"Biotronic"))
    icd_type=factor(input$icd_type, levels =  c('ICD/S-ICD', 'CRT-D'))
    atp = factor(input$atp, levels = c('OFF', 'ON'))
    progr = factor(input$progr, levels = c('Historic Convention', 'Delayed Therapy'))
    
    validate(need(!is.na(as.numeric(input$vt_zone_bpm))& as.numeric(input$vt_zone_bpm)>=105 & as.numeric(input$vt_zone_bpm)<=320, 'Please input a valid VT zone cut-off (bpm).'))
    vt_zone_bpm= as.numeric(input$vt_zone_bpm)
    
    validate(need(!is.na(as.numeric(input$vf_zone_bpm))& as.numeric(input$vf_zone_bpm)>=150 & as.numeric(input$vf_zone_bpm)<=320, 'Please input a valid VF zone cut-off (bpm).'))
    vf_zone_bpm= as.numeric(input$vf_zone_bpm)
    
    mbeat3=factor(input$mbeat3, levels = c('Normal sinus','Atrial fibrillation or flutter','Ventricular pacing'))
    
    validate(need(!is.na(as.numeric(input$HRbpm))& as.numeric(input$HRbpm)>=38 & as.numeric(input$HRbpm)<=148, 'Please input a valid Heart rate (bpm).'))
    HRbpm= as.numeric(input$HRbpm)
    
    PVCany = factor(input$PVCany, levels = c('No', 'Yes'))
    
    validate(need(!is.na(as.numeric(input$QTch))& as.numeric(input$QTch)>=260 & as.numeric(input$QTch)<=600, 'Please input a valid Hodges-corrected QT interval (ms).'))
    QTch= as.numeric(input$QTch)
    
    validate(need(!is.na(as.numeric(input$QRSduration_ms))& as.numeric(input$QRSduration_ms)>=50 & as.numeric(input$QRSduration_ms)<=330, 'Please input a valid QRS duration (ms).'))
    QRSduration_ms= as.numeric(input$QRSduration_ms)
    
    validate(need(!is.na(as.numeric(input$areaQRSTAngle_deg))& as.numeric(input$areaQRSTAngle_deg)>=2 & as.numeric(input$areaQRSTAngle_deg)<=179, 'Please input a valid Spatial QRS-T angle (degrees).'))
    areaQRSTAngle_deg= as.numeric(input$areaQRSTAngle_deg)
    
    validate(need(!is.na(as.numeric(input$areaSVGElevation_deg))& as.numeric(input$areaSVGElevation_deg)>=3 & as.numeric(input$areaSVGElevation_deg)<=177, 'Please input a valid Spatial Ventricular Gradient Elevation (degrees).'))
    areaSVGElevation_deg=as.numeric(input$areaSVGElevation_deg)
    
    validate(need(!is.na(as.numeric(input$aSVGaz))& as.numeric(input$aSVGaz)>=(-179) & as.numeric(input$aSVGaz)<=(+179), 'Please input a valid Spatial Ventricular Gradient Azimuth (degrees).'))
    aSVGaz=as.numeric(input$aSVGaz)
    
    validate(need(!is.na(as.numeric(input$areaSVG_mVms))& as.numeric(input$areaSVG_mVms)>=1 & as.numeric(input$areaSVG_mVms)<=312, 'Please input a valid Spatial Ventricular Gradient magnitude (mVms).'))
    areaSVG_mVms=as.numeric(input$areaSVG_mVms)
    
    validate(need(!is.na(as.numeric(input$SAIQRST))& as.numeric(input$SAIQRST)>=34 & as.numeric(input$SAIQRST)<=824, 'Please input a valid Sum Absolute QRST integral (mVms).'))
    SAIQRST=as.numeric(input$SAIQRST)
    
    
    #creating a data frame in which we store various inputs
    data <- data.frame(agey = agey,
                       sex = sex,
                       race = race,
                       htn = htn,
                       diabetes = diabetes,
                       cva=cva,
                       af= af,
                       mi = mi,
                       revascularization =revascularization,
                       nyha = nyha,
                       exact_lvef= exact_lvef,
                       bun = bun,
                       egfr= egfr,
                       bb_usage = bb_usage,
                       aa_usage = aa_usage,
                       ACEI = ACEI,
                       ARB = ARB,
                       ccbs = ccbs,
                       aldosteroneAnt=aldosteroneAnt,
                       manufacturer=manufacturer,
                       icd_type=icd_type,
                       atp = atp,
                       progr = progr,
                       vt_zone_bpm= vt_zone_bpm,
                       vf_zone_bpm= vf_zone_bpm,
                       mbeat3=mbeat3,
                       HRbpm=HRbpm,
                       PVCany =PVCany,
                       QTch= QTch,
                       QRSduration_ms=QRSduration_ms,
                       areaQRSTAngle_deg= areaQRSTAngle_deg,
                       areaSVGElevation_deg= areaSVGElevation_deg,
                       aSVGaz=aSVGaz,
                       areaSVG_mVms=areaSVG_mVms,
                       SAIQRST=SAIQRST)
    data})
  

  #DT::renderDataTable makes a reactive version of the function that returns the data frame to be rendered
  output$result <- DT::renderDataTable({
    data_df = data()
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp<- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class II' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class II' & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp<- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class II' & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) ++
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class II' & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class III' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class III' & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class III' & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class III' & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class IV' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class IV' & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class IV' & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Normal sinus' & input$nyha=='Class IV' & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class I' & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class I' & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class I' & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class II' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & (input$nyha=='Class II') & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & (input$nyha=='Class II') & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class II' & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class III'  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class III'  & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class III'  & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class III'  & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class IV'  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class IV'  & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & input$mbeat3=='Atrial fibrillation or flutter' & input$nyha=='Class IV'  & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV')  & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      }) 
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Non-Hispanic White'  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=='St. Jude/Abbott')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
        
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=="Biotronic")
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Black'  & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Black'  & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if (input$race=='Black'  & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I' & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I' & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I' & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })  
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Black' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.3888513)*((input$race=='Black' )-0.0967) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (0.8912569)*((input$race=='Black')-0.1029) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp  <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV')  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV')  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV')  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV')  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I'  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I'  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I'  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I'  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II')  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II')  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II')  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II')  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III')  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III')  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III')  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III')  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV')  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV')  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV')  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV')  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I'  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I'  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I'  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I'  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II')  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II')  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II')  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II')  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III')  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III')  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III')  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III')  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV')  & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV')  & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV')  & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Asian' ) & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV')  & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (0.5327577)*((input$race=='Asian' )-0.0449) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-15.87776)*(((input$race=='Asian'))-0.025) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & input$nyha=='Class I' & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class II') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
      
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class III') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*(((input$race=='Hispanic White')) -0.0747) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*(((input$race=='Hispanic White'))-0.0403) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & input$mbeat3=='Normal sinus' & (input$nyha=='Class IV') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I' & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I' & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & input$nyha=='Class I' & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
        
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class II') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class III') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Atrial fibrillation or flutter') & (input$nyha=='Class IV') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.2482811)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (1.926322)*((input$mbeat3=='Atrial fibrillation or flutter')-0.1293) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & input$manufacturer=='Guidant/Boston Scientific')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*(((input$race=='Hispanic White') )-0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }  
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
      
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & input$nyha=='Class I' & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
      
      
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class II') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (0.0438932)*((input$nyha=='Class II')-0.3964) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.8890844)*((input$nyha=='Class II')-0.3825) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class III') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.0445497)*((input$nyha=='Class III')-0.4316) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp<- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (0.788116)*((input$nyha=='Class III')-0.4033) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & input$manufacturer=='Medtronic')
    {
      predict.risk.MMVT <- expression({
        lp  <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & ((input$manufacturer=='Guidant/Boston Scientific')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.5884312)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2430) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-1.648756)*(((input$manufacturer=='Guidant/Boston Scientific'))-0.2615) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & ((input$manufacturer=='St. Jude/Abbott')))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.0390527)*(((input$manufacturer=='St. Jude/Abbott'))-0.1337) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (1.019888)*(((input$manufacturer=='St. Jude/Abbott'))-0.0779)+
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    if ((input$race=='Hispanic White')  & (input$mbeat3=='Ventricular pacing') & (input$nyha=='Class IV') & ((input$manufacturer=="Biotronic")))
    {
      predict.risk.MMVT <- expression({
        lp <- 1-0.8292047^exp(
          (-0.0219907)*(data_df$agey-63.13125) +
            (0.325852)*((input$sex=='Male')-0.7661) +
            (-0.7704543)*((input$race=='Hispanic White') -0.0747) +
            (-0.1087567)*((input$mbeat3=='Ventricular pacing')-0.1288) +
            (-0.1619379)*((input$nyha=='Class IV')-0.0273) +
            (0.2283616)*(((input$manufacturer=="Biotronic"))-0.018) +
            (0.4276305)*((input$htn=='Yes')-0.6935) +
            (-0.1804765)*((input$diabetes=='Yes')-0.3621) +
            (0.1593888)*((input$cva=='Yes')-0.1105) +
            (0.4687373)*((input$af=='Yes')-0.339) +
            (0.5464825)*((input$bb_usage=='Yes')-0.8512) +
            (0.0620628)*((input$aa_usage=='Yes')-0.1440) +
            (-0.2058735)*((input$ACEI=='Yes')-0.5227) +
            (-0.641419)*((input$ARB=='Yes')-0.2039) +
            (-0.5173946)*((input$icd_type=='CRT-D')-0.3627) +
            (-0.0185014)*(data_df$vt_zone_bpm-177.8278) +
            (-0.0000623)*(data_df$vf_zone_bpm-224.5021) +
            (-0.0057464)*(data_df$HRbpm-71.55304) +
            (-1.246902)*((input$progr=='Delayed Therapy')-0.3270) +
            (0.6952856)*((input$PVCany=='Yes')-0.1897) +
            (-0.0021566)*(data_df$QTch-448.5279) +
            (-0.2024031)*((input$revascularization=='Yes')-0.4308) +
            (-0.0209583)*((input$mi=='Yes')-0.4913) +
            (-1.28567)*((input$ccbs=='Yes')-0.0798) +
            (-0.5852881)*((input$aldosteroneAnt=='Yes')-0.2991) +
            (-0.0175218)*(data_df$bun-24.68193) +
            (0.0417666)*((input$atp=='ON')-0.8579) +
            (0.0001828)*(data_df$areaQRSTAngle_deg-141.8829) +
            (0.0035389)*(data_df$areaSVGElevation_deg-87.59344) +
            (-0.019313)*(data_df$areaSVG_mVms-41.09404) +
            (-0.0004595)*(data_df$SAIQRST-194.9203) +
            (-0.0127859)*(data_df$exact_lvef-28.24665)+
            (0.0048715)*(data_df$QRSduration_ms-117.0333) +
            (-0.000486)*(data_df$aSVGaz-42.42782) +
            (-0.0086773)*(data_df$egfr-70.35064)
        )
      })
      predict.risk.PVT <- expression({
        lp <- 1-0.948173^exp(
          (-0.0660511)*(data_df$agey-64.36876) +
            (0.3467898)*((input$sex=='Male')-0.7608) +
            (-1.320991)*((input$race=='Hispanic White')-0.0403) +
            (-0.1683076)*((input$mbeat3=='Ventricular pacing')-0.1113) +
            (-14.86976)*((input$nyha=='Class IV')-0.0348) +
            (-16.48323)*((input$manufacturer=="Biotronic")-0.0083) +
            (0.2984812)*((input$htn=='Yes')-0.6898) +
            (-0.537758)*((input$diabetes=='Yes')-0.3505)+
            (-0.1233915)*((input$cva=='Yes')-.0904) +
            (-0.8228998)*((input$af=='Yes')-0.3129) +
            (-1.151641)*((input$bb_usage=='Yes')-0.8734) +
            (-16.47702)*((input$aa_usage=='Yes')-.1001) +
            (1.087283)*((input$ACEI=='Yes')-0.5841) +
            (1.262122)*((input$ARB=='Yes')-0.1780) +
            (-0.0188273)*((input$icd_type=='CRT-D')-0.3783)+
            (0.002684)*(data_df$vt_zone_bpm-183.491) +
            (-0.0085447)*(data_df$vf_zone_bpm-240.0807) +
            (0.0383147)*(data_df$HRbpm-70.94591) +
            (-0.6453979)*((input$progr=='Delayed Therapy')-0.338) +
            (0.4405619)*((input$PVCany=='Yes')-0.1864) +
            (-0.0018592)*(data_df$QTch-451.5698) +
            (2.020529)*((input$revascularization=='Yes')-0.4993) +
            (-0.7344092)*((input$mi=='Yes')-0.5257) +
            (-1.223912)*((input$ccbs=='Yes')-0.0834)+
            (0.4746157)*((input$aldosteroneAnt=='Yes')-0.267) +
            (-0.0546977)*(data_df$bun-25.32823)+
            (-0.1398414)*((input$atp=='ON')-0.7761) +
            (0.0109526)*(data_df$areaQRSTAngle_deg-140.2143) +
            (-0.0069049)*(data_df$areaSVGElevation_deg-87.21858)+
            (-0.0184298)*(data_df$areaSVG_mVms-41.59897) +
            (0.0041371)*(data_df$SAIQRST-195.8314) +
            (0.0535254)*(data_df$exact_lvef-26.57524) +
            (0.0072088)*(data_df$QRSduration_ms-112.2003)  +
            (-0.0025816)*(data_df$aSVGaz-40.40908)+
            (-0.021884)*(data_df$egfr-70.64261)
        )
      })
    }
    
    #we multiply the predicted value by 100 to get a percentage value
    pred1 = eval(predict.risk.MMVT, envir = data_df)
    pred1 = paste(round(pred1 * 100, 1), "%", sep='')
    
    pred2 = eval(predict.risk.PVT, envir = data_df)
    pred2 = paste(round(pred2 * 100, 1), "%", sep='')
    
    #the final result is concatenated within one data frame which is then displayed when the function in called in the ui side
    res = data.frame(Result = c("Probability of MMVT","Probability of PVT/VF"),
                     Probability = c(pred1,pred2))
    
    res}, options = list(
      pageLength = 10,
      lengthMenu = 0,
      
      searching = 0,
      info = 0,
      paging = 0,
      processing = 0,
      initComplete = I(
        "function(settings, json) {
                    $(this.api().table().header()).css({'background-color': '#606060', 'color': '#fff'});
            }")))
})

