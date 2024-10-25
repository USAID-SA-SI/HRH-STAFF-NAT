# Title:HRH_STAFF_NAT MER Reporting Script
# Author: C. Trapence
# Date:2024-10-23
# Updated:2024:10:23 by Rosaline & C.Trapence @ 13:00pm
#Load Required libraries
# Red text symbolizes comments

#######################################################################################################################
#  sources files/Inputs used in the code include:                                                                            #
#              1) Data  collected by IP's                                                                          #
#              2) Host Country Results SUB NAT (USG) from DATIM Support                                                #
#              3) Data Exchange Organisation Units from DATIM Support                                                 #
#              4) Mechanisms from DATIM support
#              5) Master Facility List
#######################################################################################################################


#glamr::folder_setup()
#glamr::load_secrets()

#'[GLOBAL VARIABLES --------------------------------------------------------

current_quarter<-"FY24Q4"
Filename<-paste0(current_quarter, "_Final_HRH_STAFF_NAT_importV2",Sys.Date(),".CSV")

# packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, patchwork, googledrive,googlesheets4,openxlsx,lubridate,janitor,readr, esquisse, flextable,stringr,sqldf)

#'[Load Data from Individual Partners google sheets for Level one review

#'[RTC]
RTC_range <- "A4:AO497"
RTC<-read_sheet(as_sheets_id(" https://docs.google.com/spreadsheets/d/1qQS8i-WIh0eo7rYpcooyCSLnt_NplfSCbL7pWWXM434/edit?gid=1970442414#gid=1970442414"), sheet = "COP23 HRH_STAFF_NAT Reporting", range =RTC_range) %>%
select(OrgUnit  ,contains("Count")) %>% 
mutate(AttributeOptionCombo="R6zwVobwi58",mech_name="RTC") %>%  
mutate(OrgUnit=if_else(OrgUnit=="fs OR Tambo Clinic","fs OR Tambo (Senekal) Clinic",OrgUnit)) %>% mutate_at(2:20, as.numeric)

#'[ANOVA]
ANOVA_range <- "A4:AO959"
ANOVA<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1B3N7phs5c07cSUjxWtI_sZK5mOiv5BdYhtNGEQJ6RcE/edit?gid=1970442414#gid=1970442414"), sheet = "COP23 HRH_ STAFF_NAT Reporting", range= ANOVA_range) %>% 
select(OrgUnit  ,contains("Count")) %>% mutate_at(2:20, as.numeric) %>%  mutate(AttributeOptionCombo="LbZtY0khSQw",mech_name="ANOVA")

#'[MATCH]
MatCH_range <- "A4:AO391"
MatCH<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1SKMv5ahX8ik5N_kfVhEjYXXCT7djocQkJWcBvavGOes/edit?gid=1970442414#gid=1970442414"), sheet = "COP23 HRH_STAFF_NAT Reporting", range = MatCH_range) %>% 
select(OrgUnit  ,contains("Count")) %>% 
mutate(AttributeOptionCombo="Sm6Y3REDZ42",mech_name="MatCH")%>% mutate_at(2:20, as.numeric)

#'[WRHI]
WRHI_range <- "A4:AO96"
WRHI<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18kVVsNIo2m6DjbLiHl9eFvEjHuDlR1d4NKCnWPhk2aM/edit?gid=1970442414#gid=1970442414"), sheet = "COP23 HRH_STAFF_NAT Reporting", range = WRHI_range) %>% 
select(OrgUnit  ,contains("Count")) %>% 
mutate(AttributeOptionCombo="Rv3LaFFxBCY",mech_name="WRHI")%>% mutate_at(2:20, as.numeric)

#'[BRCH]
BRCH_range <- "A4:AO412"
BRCH<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Wd0YcIDTLCa6n-fEkK18oamBWGxfxe2dcCjpJ1y0jmk/edit?gid=804516909#gid=804516909'), sheet = "COP23 HRH_STAFF_NAT Reporting", range = BRCH_range ) %>%
select(OrgUnit  ,contains("Count")) %>% filter(!(OrgUnit == "mp Balfour Clinic")) %>% filter(!(OrgUnit == "mp Siyathemba CHC")) %>% 
mutate(AttributeOptionCombo="koVrJ0HjBxy",mech_name="BRCH")  %>% mutate_at(2:20, as.numeric)

#'[Use sqldf package to execute an SQL query by grouping data from the BRCH table by OrgUnit.
#'[ and summing various position counts.]

BRCH<-sqldf("
SELECT 
mech_name,
OrgUnit,
AttributeOptionCombo,
sum(`Medical Officer - Position Count`) , 
sum(`Professional Nurse - Position Count`),
sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),
sum(`Other - Clinical - Position Count`) , 
sum(`Lab Employee - Position Count`) , 
sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),
sum(`Management - Other - Position Count` ), 
sum( `Social Service - Position Count`),
sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`),
sum(`Linkage Officer - Position Count`)  ,
sum(`WBOT CHW - Position Count`)  , 
sum(`Other CHW (non-WBOT) - Position Count`),
sum(`Other - Lay - Position Count`), 
sum(`Data Capturer - Position Count`),
sum(`Data Clerk - Position Count`)  , 
sum(`Other - HCW\n- Position Count` )    
            
FROM BRCH 
    group by OrgUnit")

MatCH<-sqldf("SELECT 
mech_name,
OrgUnit,
AttributeOptionCombo,
sum(`Medical Officer - Position Count`) ,
sum(`Professional Nurse - Position Count`),
sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),
sum(`Other - Clinical - Position Count`) ,
sum(`Lab Employee - Position Count`) , 
sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),
sum(`Management - Other - Position Count` ), 
sum( `Social Service - Position Count`), 
sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`),
sum(`Linkage Officer - Position Count` ),
sum(`WBOT CHW - Position Count`)  , 
sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),
sum(`Data Capturer - Position Count`),
sum(`Data Clerk - Position Count`)  ,
sum(`Other - HCW\n- Position Count` )    

FROM MatCH 
group by OrgUnit")


WRHI<-sqldf("SELECT 

mech_name,
OrgUnit,
AttributeOptionCombo,

sum(`Medical Officer - Position Count`) ,
sum(`Professional Nurse - Position Count`),
sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),
sum(`Other - Clinical - Position Count`) ,
sum(`Lab Employee - Position Count`) ,
sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),
sum(`Management - Other - Position Count` ),
sum( `Social Service - Position Count`), 
sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`),
sum(`Linkage Officer - Position Count` )  ,
sum(`WBOT CHW - Position Count`)  , 
sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`),
sum(`Data Capturer - Position Count`),
sum(`Data Clerk - Position Count`)  , 
sum(`Other - HCW\n- Position Count` )    

FROM WRHI
group by OrgUnit")


RTC<-sqldf("SELECT

mech_name, 
OrgUnit,
AttributeOptionCombo,

sum(`Medical Officer - Position Count`) ,
sum(`Professional Nurse - Position Count`),
sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`),
sum(`Other - Clinical - Position Count`) , 
sum(`Lab Employee - Position Count`) ,
sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),
sum(`Management - Other - Position Count` ), 
sum( `Social Service - Position Count`), 
sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`),
sum(`Linkage Officer - Position Count` )  ,
sum(`WBOT CHW - Position Count`)  , 
sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`), 
sum(`Data Capturer - Position Count`), 
sum(`Data Clerk - Position Count`)  , 
sum(`Other - HCW\n- Position Count` )   

FROM RTC 
group by OrgUnit")

ANOVA_2<-  sqldf("SELECT
mech_name,
OrgUnit, 
AttributeOptionCombo, 
sum(`Medical Officer - Position Count`) ,
sum(`Professional Nurse - Position Count`), 
sum(`Enrolled Nurse - Position Count`),
sum(`WBOT OTL - Position Count`), 
sum(`Other - Clinical - Position Count`) , 
sum(`Lab Employee - Position Count`) , 
sum(`Pharmacist / Pharmacist Assistant - Position Count`),
sum(`Other - Pharmacy - Position Count`),
sum(`Management - Other - Position Count` ), 
sum( `Social Service - Position Count`), 
sum(`Other - Social Service - Position Count`),
sum(`Lay Counselor - Position Count`), 
sum(`Linkage Officer - Position Count` ),
sum(`WBOT CHW - Position Count`)  , 
sum(`Other CHW (non-WBOT) - Position Count` ),
sum(`Other - Lay - Position Count`), 
sum(`Data Capturer - Position Count`), 
sum(`Data Clerk - Position Count`)  , 
sum(`Other - HCW\n- Position Count` )   
FROM ANOVA 
group by OrgUnit") %>%
mutate(OrgUnit=case_when(OrgUnit=="lp Sophia Clinic/Sekwai Clinic"~"lp Sophia/Sekwai Clinic",
OrgUnit=="wc Gordons Bay CDC"~"wc Gordon's Bay CDC",
OrgUnit=="wc Pelican Park Clinic"~"wc Pelican Park Satellite Clinic",
OrgUnit=="wc Somerset West Clinic"~"wc Somerset West CDC",
OrgUnit =="gp North West University Clinic"~"gp North West University Occupational Health Centre",
OrgUnit =="gp Slovoville clinic"~"gp Slovoville Clinic",
OrgUnit =="gp Sonder Water Health Post"~"gp Sonder Water Health post",
OrgUnit =="lp Hlokomela Non-medical Site"~"lp Hlokomela Non-Medical Site",
OrgUnit =="lp Matsotsosela clinic"~"lp Matsotsosela Clinic",
OrgUnit =="lp Sophia Clinic/Sekwai Clinic"~"lp Sophia/Sekwai Clinic",
OrgUnit =="wc Gordons Bay CDC"~"wc Gordon's Bay CDC",
OrgUnit =="wc Pelican Park Clinic"~"wc Pelican Park Satellite Clinic",
OrgUnit =="wc Somerset West Clinic"~"wc Somerset West CDC", TRUE ~OrgUnit ))


#write.xlsx(ANOVA_2,"Clean.xlsx")
partners_data <- bind_rows(BRCH,WRHI,MatCH,ANOVA_2,RTC) %>% 
relocate(mech_name,AttributeOptionCombo) %>% 
rename(facility=OrgUnit) %>% 
mutate(facility= case_when(facility=="mp Amsterdam Mobile"~"mp Amsterdam Mobile 1",
facility=="kz King Dinizulu Clinic" ~"kz King Dinuzulu Clinic",
facility=="kz Morrisons Post Clinic"~"kz Morrison's Post Clinic",
facility=="mp Bettysgoed Clinic"~"mp Betty'sgoed Clinic",
facility=="mp Klarinet Clinic" ~"mp Klarinet CHC",
facility=="mp Lillian Mambakazi CHC"~"mp Lilian Mambakazi CHC",
facility=="mp MS Msimanga Clinic"~"mp MS Msimango Clinic",
facility=="mp Mbhejeka CHC"~"mp Mbhejeka Clinic",
facility=="kz Rietvlei Mobile 1"~"Kz Rietvlei Mobile 1",
facility=="fs Dinkweng clinic"~"fs Dinkweng Clinic",
facility=="fs Khosatsana Masetjhaba Clinic"~"fs Khosatsana Masetjhaba clinic",
facility=="fs Makeneng clinic"~"fs Makeneng Clinic",
facility=="fs Monontsha clinic"~"fs Monontsha Clinic",
facility=="fs Mphatlalatsane clinic"~"fs Mphatlalatsane Clinic",
facility=="fs Phuthaditjhaba clinic"~"fs Phuthaditjhaba Clinic",
facility=="fs Tebang clinic"~"fs Tebang Clinic",
facility=="fs Thabang clinic"~"fs Thabang Clinic",
facility=="fs Tina moloi clinic"~"fs Tina Moloi Clinic",
TRUE~facility)) %>% filter(!facility %in% c("kz Lower Umfolozi War Memorial Hospital",
"kz Turton CHC","kz Port Shepstone Mobile 1",
"kz Port Shepstone Mobile 3",
"kz Port Shepstone Mobile 6",
"kz Port Shepstone Mobile 7"))

#partners_data<-ANOVA_2%>% relocate(mech_name,AttributeOptionCombo) %>% rename(facility=OrgUnit)

#'[Creat new aggregated columns (Clinical, Laboratory, Pharmacist, Management, Social_Services, Lay, and Other_HCW) 
#'[by summing up several existing columns from the partners_data dataframe.]

partners_datav1 <- partners_data %>%  
mutate(
  
Clinical =         `sum(\`Medical Officer - Position Count\`)`+ 
                   `sum(\`Professional Nurse - Position Count\`)`+ 
                   `sum(\`Enrolled Nurse - Position Count\`)`+ 
                   `sum(\`WBOT OTL - Position Count\`)` +
                   `sum(\`Other - Clinical - Position Count\`)`, 

Laboratory =       `sum(\`Lab Employee - Position Count\`)`,

Pharmacist=        `sum(\`Pharmacist / Pharmacist Assistant - Position Count\`)` +
                   `sum(\`Other - Pharmacy - Position Count\`)` ,
Management=        `sum(\`Management - Other - Position Count\` )`,

Social_Services =  `sum( \`Social Service - Position Count\`)` +
                   `sum(\`Other - Social Service - Position Count\`)` ,

Lay =              `sum(\`Lay Counselor - Position Count\`)`+
                   `sum(\`Linkage Officer - Position Count\` )` +
                   `sum(\`WBOT CHW - Position Count\`)` +
                   `sum(\`Other CHW (non-WBOT) - Position Count\` )` +
                   `sum(\`Other - Lay - Position Count\`)`,

Other_HCW =        `sum(\`Data Capturer - Position Count\`)` +
                   `sum(\`Data Clerk - Position Count\`)`  +
                  `sum(\`Other - HCW\n- Position Count\` )`  )

#'[partners_data2 now contains only the relevant columns, focusing on aggregated counts for various positions (Clinical, Laboratory, Pharmacist, etc.)
#'[along with mech_name, AttributeOptionCombo, and facility. ]

partners_data2 <- partners_datav1%>% 
select(
       mech_name,
       AttributeOptionCombo,
       facility,
       Clinical,
       Laboratory,
       Pharmacist,
       Management,
       Lay, 
       Other_HCW ,
       Social_Services)

#'[Transforms partners_data2 into a longer format and add some additional columns like period
#']

partners_data_v3 <- partners_data2 %>%   
pivot_longer(
  cols = Clinical:Social_Services,
  names_to = "code",
  values_to = "Value"
  ) %>% 
mutate(
  Dataelement ="Kk4CdspETNQ",
  CategoryOptionCombo  = case_when(
    str_detect(code,"Clin")~"mkOfrTuz7tS",
    str_detect(code,"Lab")~"T1jZtIrfVkq",
    str_detect(code,"Lay")~"a9N5X73zhET",
    str_detect(code,"Mana")~"oaRfTQD4RLG",
    str_detect(code,"Other")~"wKH5X6oHquw",
    str_detect(code,"Pharm")~"VYMJrOJU5rQ",
    TRUE~"itxIkeWqiE9"
    ),
  Period="2024Q3"
  )

#'[Checking for all cadres's code
print(distinct(partners_data_v3,code))

#'[Using Org Units
orgunits<-list.files(here("data"),pattern = "Exchange")
orgunits<-read_csv(here("data", orgunits)) %>% mutate(Orgunit = orgunit_internal_id) %>% filter(orgunit_internal_id != "KJ6EYihrWdp") 

#MFL_FY24_ID<-"1UuDYK4X-Lr8avq4IUsKGWChr17AE4Bc21MlHO3OWB-Y"
#mfl_fy23_id <- "1QWVeT2wXA428WLYBy6-X5vP9A6sXjLjdtmkvLTw3mJA"
#mfl_new_df <- googlesheets4::read_sheet(mfl_fy23_id, sheet = "MFL_FY23")

# #get mech info from MFL
# mech_mfl <- mfl_new_df %>%
#   dplyr::filter(!is.na(OU2name)) %>%
#   janitor::clean_names() %>%
#   dplyr::select(ou5name, datim_uid,partner, mechanism_i_d, mechanism_uid) %>%
#   rename(sitename = ou5name,
#          facilityuid = datim_uid,
#          prime_partner_name = partner,
#          mech_code = mechanism_i_d,
#          mech_uid = mechanism_uid) %>% distinct()
#

#Final_Df<-left_join(partners_data_v1 ,mech_mfl,by=c("facility"="sitename","AttributeOptionCombo"="mech_uid"))%>%  rename(OrgUnit=facilityuid)%>% mutate(OrgUnit=if_else(OrgUnit=="ekRXPVQUGw4","eHGSLAgEbWP",OrgUnit))

#'[Final Import data]
Final_Df<-left_join(partners_data_v3 ,orgunits,by=c("facility"="orgunit_name")) %>% 
rename(OrgUnit=orgunit_internal_id  ) %>% mutate(AttributeOptionCombo="HllvX50cXC0")

Final_Df2<-Final_Df %>% filter(is.na(OrgUnit))

#write.xlsx(Final_Df2,"HRH_check6.xlsx")
#%>% mutate(AttributeOptionCombo="HllvX50cXC0")
#In case of error use this code AttributeOptionCombo="HllvX50cXC0"
#Data Review

#Checking mismatches between MFL & reported data
#Check1<-Final_Df %>% filter(is.na(OrgUnit))

#print(distinct(Final_Df,code))
#write.xlsx(Check1,"HRH_Site_clean.xlsx")


Final_Df_Import<-Final_Df %>% select(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo,Value) %>%
filter(!is.na(Value) ,Value>0)


###Appending data from the HRID Dashboard
#Mech Reference list
# mechanismID	PrimePartner
# 70310	Anova Health Institute
# 70287	BroadReach Healthcare
# 80007	Wits Reproductive Health and HIV Institute (Wits RHI)
# 70306	Wits Reproductive Health and HIV Institute (Wits RHI)
# 80008	NACOSA
# 70301	Wits Reproductive Health and HIV Institute (Wits RHI)
# 70307	HIV SA (NPC)
# 87576	Maternal, Adolescent and Child Health Institute NPC (MatCH Institute NPC)
# 160611	Education Development Center
# 80004	mothers2mothers (M2M)
# 70311	Children in Distress (CINDI)
# 87575	Maternal, Adolescent and Child Health Institute NPC (MatCH Institute NPC)
# 86131	OUT LGBT Well-being
# 14295	Family Health International South Africa (FHI 360)
# 81705	Department of Basic Education G2G
# 81904	Department of Social Development G2G
# 82199	Family Health International South Africa (FHI 360)
# 70290	Right to Care

HRID<-list.files(here("Data"),pattern="02_FY2024_Q2COP23_USAID_2")
HRID_USAID<-read.xlsx(here("Data",HRID),sheet = "Refined_Factview_20240614")

HRID_Count<-HRID_USAID %>% 
  filter(!is.na(facilityuid) ,str_detect(indicator,"Count"),
  mechanismID %in% c("70287","70290","70301","70306","70307","70310","70311","80007","80004","80008",
                    "82199","87576","87575","87577","81904","14631","14295","86131","160611","81705")) %>%
mutate(
  mechname=case_when(mechanismID==70310~"ANOVA",mechanismID==87577~"ANOVA-LP",
  mechanismID==70287~"BRCH",mechanismID==87575~"MaTCH KZN",mechanismID==80007~"Wits Prevention",
  mechanismID==70306~"WRHI FSW/TG",mechanismID==70307~"HIV SA (NPC)",mechanismID==70301~"WRHI-USAID/APACE",
  mechanismID==82199~"FHI360",mechanismID==87576~"Match EC",mechanismID==70290~"RTC",
  mechanismID==70311~"CINDI",mechanismID==160611~"EDC",mechanismID==81705~"DBE-G2G",mechanismID==86131~"Endanger",
  mechanismID==14295~"FHI360",mechanismID=="81904"~"G2G DSD",mechanismID==70307~"HIVSA",mechanismID==80004~"M2M",
  mechanismID==80008~"NACOSA GBV",mechanismID==14631~"Pact_inc",TRUE~""))

HRID_CountP2<-HRID_Count %>% 
mutate(Facility=case_when(Facility=="kz Mondlo No 1 Clinic"~"kz Mondlo 2 Clinic",
Facility=="Mfundo Arnold Lushaba"~"kz Mfundo Arnold Lushaba CHC", 
Facility=="ec Cecilia Makiwane Haem"~"ec Cecilia Makiwane Hospital",
Facility=="fs Senorita Ntlabathi Hospital"~"fs Senorita Ntlabathi Hospital",
Facility=="gp Mpumelelo Clinic"~"gp Mpumelelo Clinic" ,
Facility=="lp Phelang NGO Clinic"~"lp Phelang Community Centre", 
Facility=="mp Rockdale CHC"~"mp Rockdale CHC",
Facility=="mp Tweefontein G CHC"~"mp Tweefontein M Clinic",
Facility=="mp Tweefontein G Clinic"~"mp Tweefontein G CHC",
Facility=="mp Waterval Boven Clinic"~"mp Waterval Boven Gateway Clinic",
Facility=="wc Alphen Satellite Clinic"~"wc Alphen Clinic",
TRUE~Facility)) %>% 
  
select(psnu,PrimePartner,mechname, mechanismID,Facility, facilityuid,Cadre ,FY2024Q2 )%>% 
rename(Value=FY2024Q2) %>%
mutate(facilityuid=case_when(
  
Facility=="ec Cecilia Makiwane Hospital"~	"Vd9jdCAwC0d",
Facility=="fs Senorita Ntlabathi Hospital"~"FJJA8Rdd2YC",
Facility== "gp Mpumelelo Clinic" ~ "jShhMB84PRa",
Facility== "kz Mfundo Arnold Lushaba CHC" ~  "ESLvoqIBQlH",
Facility== "kz Mondlo 2 Clinic" ~ "Sd9nCkCLsiz",
Facility== "lp Phelang Community Centre" ~ "fNwbKuVabSH",
Facility== "mp Kempville CHC" ~ "WVgSBCk9tRa",
Facility== "mp Rockdale CHC" ~ "rMjDtGXNyDV",
Facility== "mp Tweefontein G CHC" ~ "Wg4yViRpIyi",
Facility== "mp Tweefontein M Clinic" ~ "yVPZPpbyPMI",
Facility== "mp Waterval Boven Gateway Clinic" ~ "YHgT1ahFKAf",
Facility==  "wc Alphen Clinic" ~ "HWz6wRLgUvg",
Facility==  "mp Ubuhle Bempilo CHC" ~ "yVPZPpbyPMI",
TRUE~ facilityuid))

#write.xlsx(HRID_CountP2,"Check.xlsx")
#print(distinct(HRID_CountP2,mechname ,mechanismID))
HRID_CountP3<-HRID_CountP2 %>%left_join(orgunits,by=c("facilityuid"="orgunit_internal_id"))%>% 
select(psnu,PrimePartner,mechname, mechanismID,Facility,Orgunit,Cadre ,Value )


HRID_CountP4<-HRID_CountP3%>% mutate(Dataelement="Kk4CdspETNQ",
CategoryOptionCombo=case_when(str_detect(Cadre,"Clin")~"mkOfrTuz7tS",
str_detect(Cadre,"Lab")~"T1jZtIrfVkq",str_detect(Cadre,"Lay")~"a9N5X73zhET",str_detect(Cadre,"Mana")~"oaRfTQD4RLG",
str_detect(Cadre,"Other")~"wKH5X6oHquw",str_detect(Cadre,"Pharm")~"VYMJrOJU5rQ",TRUE~"itxIkeWqiE9"),Period="2024Q3") %>% 
mutate(AttributeOptionCombo="HllvX50cXC0") %>% 
filter(Value>0) %>% 
rename(OrgUnit=Orgunit)

HRID_CountP4_Final<-HRID_CountP4%>% select(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo,Value) %>% 
filter(!is.na(Value) ,Value>0) %>%
group_by(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo) %>% 
summarise(Value=sum(Value))

#'[Append with the reported HRH results by NDOH

Final_PEPFAR_DOH<-rbind(Final_Df_Import,HRID_CountP4_Final) %>% 
group_by(Dataelement,Period,OrgUnit,CategoryOptionCombo,AttributeOptionCombo) %>% 
summarise(Value=sum(Value))

write_csv(Final_PEPFAR_DOH,here("dataout",Filename))