require(shinythemes)
library(shinyBS)

fluidPage(theme = shinytheme("flatly"),
          
          titlePanel('Predicting 3-year competing risk of monomorphic ventricular tachycardia (MMVT) and polymorphic
                     ventricular tachycardia/ventricular fibrillation (PVT/VF)'),
          hr(),
          sidebarLayout(
            sidebarPanel(id = "sidebar",
                         h5("Demographic Characteristics"),
                         textInput('agey', 'Age (Years)'),
                         selectInput('sex', 'Gender', choices = c('Female', 'Male')),
                         selectInput('race', 'Race and Ethnicity', choices = c('Non-Hispanic White','Hispanic White', 'Black','Asian')),
                         h5("Past Medical History"),
                         selectInput("htn", "History of Hypertension", choices = c('No', 'Yes')),
                         selectInput("diabetes", "History of Diabetes", choices = c('No', 'Yes')),
                         selectInput("cva", "History of stroke", choices = c('No', 'Yes')),
                         selectInput("af", "History of atrial fibrillation", choices = c('No', 'Yes')),
                         selectInput("mi", "History of myocardial infarction", choices = c('No', 'Yes')),
                         selectInput("revascularization", "History of revascularization (CABG or PCI)", choices = c('No', 'Yes')),
                         h5("Clinical Characteristics"),
                         selectInput('nyha', 'NYHA Class', choices = c('Class I','Class II', 'Class III','Class IV')),
                         textInput('exact_lvef', 'Left Ventricular Ejection Fraction (%)'),
                         textInput('bun', 'BUN (mg/dL)'),
                         textInput('egfr', 'eGFR CKD-EPI (mL/min/1.73 m^2)'),
                         h5("Use of Medications"),
                         selectInput("bb_usage", "Beta-blocker use", choices = c('No', 'Yes')),
                         selectInput("aa_usage", "Class I or III antiarrhythmic drug use", choices = c('No', 'Yes')),
                         selectInput("ACEI", "Angiotensin-converting-enzyme inhibitor use", choices = c('No', 'Yes')),
                         selectInput("ARB", "Angiotensin II receptor blocker use", choices = c('No', 'Yes')),
                         selectInput("ccbs", "Calcium channel blocker use", choices = c('No', 'Yes')),
                         selectInput("aldosteroneAnt", "Aldosterone Antagonist use", choices = c('No', 'Yes')),
                         h5("Device Characteristics"),
                         selectInput("manufacturer", "Device Manufacturer", choices = c('Medtronic', 'Guidant/Boston Scientific','St. Jude/Abbott',"Biotronic")),
                         selectInput("icd_type", "Device Type", choices = c('ICD/S-ICD', 'CRT-D')),
                         selectInput("atp", "Anti-tachycardia pacing", choices = c('OFF', 'ON')),
                         selectInput("progr", "ICD therapy programming type", choices = c('Historic Convention', 'Delayed Therapy')),
                         # tableOutput()-need to push in a non-interactive data table
                         textInput('vt_zone_bpm', 'VT zone cut-off (bpm)'),
                         textInput('vf_zone_bpm', 'VF zone cut-off (bpm)'),
                         h5("ECG Characteristics"),
                         selectInput('mbeat3', 'Rhythm on the ECG', choices = c('Normal sinus (N)','Atrial fibrillation or flutter (S)','Ventricular pacing (V)')),
                         textInput('HRbpm', 'Heart rate (bpm)'),
                         selectInput("PVCany", "Premature ventricular complex (PVC) on ECG", choices = c('No', 'Yes')),
                         textInput('QTch', 'Hodges-corrected QT interval (ms)'),
                         textInput('QRSduration_ms', 'QRS duration (ms)'),
                         textInput('areaQRSTAngle_deg', 'Spatial QRS-T angle (degrees) '),
                         textInput('areaSVGElevation_deg', 'Spatial Ventricular Gradient Elevation (degrees) '),
                         textInput('aSVGaz', 'Spatial Ventricular Gradient Azimuth (degrees) '),
                         textInput('areaSVG_mVms', 'Spatial Ventricular Gradient magnitude (mVms) '),
                         textInput('SAIQRST', 'Sum Absolute QRST integral (mVms) '),
            ),
            mainPanel(
              actionButton("goButton", "Run Calculator"),
              br(),
              hr(),
              tags$head(
                tags$style(type="text/css", "tfoot {display:none;}")
              ),
              DT::dataTableOutput('result'),
              br(),
              wellPanel(h3('Disclaimer'),
                        p("No Medical Advice. ALTHOUGH SOME CONTENT MAY BE PROVIDED BY INDIVIDUALS IN THE MEDICAL PROFESSION, 
                          YOU ACKNOWLEDGE THAT PROVISION OF SUCH CONTENT DOES NOT CREATE A MEDICAL PROFESSIONAL-PATIENT 
                          RELATIONSHIP AND DOES NOT CONSTITUTE AN OPINION, MEDICAL ADVICE, PROFESSIONAL DIAGNOSIS, SERVICE 
                          OR TREATMENT OF ANY CONDITION. Access to general information is provided for educational purposes only, 
                          through this site and links to other sites. Content is not recommended or endorsed by any doctor or 
                          healthcare provider. The information and Content provided are not substitutes for medical or professional 
                          care, and you should not use the information in place of a visit, call, consultation or the advice of your 
                          physician or other healthcare provider. You are liable or responsible for any advice, course of treatment, 
                          diagnosis or any other information, services or product obtained through this site.")),
              p(a("Homepage",
                  href = "../", style = "font-family: 'Lato','Helvetica Neue',Helvetica,Arial,sans-serif;
                      font-size: 15px;color: #2c3e50;font-weight: bold;text-align: center;text-decoration: underline;"), 
                " | ",
                a("Contact Us",
                  href = "mailto:rcalcsupport@ccf.org?body=Diabetes%20-%20Predicting%206-Year%20Mortality%20Risk", style = "font-family: 'Lato','Helvetica Neue',Helvetica,Arial,sans-serif;
                      font-size: 15px;color: #2c3e50;font-weight: bold;text-align: center;text-decoration: underline;"), 
                style = "text-align: center;"),
              img(src='../logo-ccf.png', style = "max-width:50%; max-height:100%;"))
            ))