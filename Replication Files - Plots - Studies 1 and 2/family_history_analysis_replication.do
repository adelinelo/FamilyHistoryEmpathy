
***HEADER**********************************************************

clear
clear matrix
set more off
cap log close

* Set Working directory
cd ""

********************************************************************
*********************** Wave 1 *************************************
********************************************************************

***CLEANING*********************************************************

* Load data
use family_history_wave1_data, clear

* Generating treatment variable for whether family history question was asked first.
gen treatment = SHW300_rand - 1

* Outcome measure: Do you agree or disagree that the U.S. should limit the number of immigrants entering the country?
* 1 = strongly agree, 7 = strongly disagree
rename SHW301 not_restrict_immigrant
tabulate not_restrict_immigrant
summarize not_restrict_immigrant

* Answers to family history question about when family came to US
rename SHW302 immigrant_history
tabulate immigrant_history
summarize immigrant_history

* Dummy variable for family arriving during or after grandparents' generation
generate my_grandparent_generation = 1 if immigrant_history==1|immigrant_history==2|immigrant_history==3
replace my_grandparent_generation = 0 if immigrant_history==4|immigrant_history==5
summarize my_grandparent_generation

* Partisanship variables
generate republican = 1 if pid3==2
replace republican = 0 if pid3!=2
summarize republican

generate democrat = 1 if pid3==1
replace democrat = 0 if pid3!=1
summarize democrat

generate independent = 1 if pid3!=1&pid3!=2
replace independent = 0 if independent!=1

generate strong_democrat = 1 if pid7==1
replace strong_democrat = 0 if strong_democrat!=1

generate medium_democrat = 1 if pid7==2
replace medium_democrat = 0 if medium_democrat!=1

generate lean_democrat = 1 if pid7==3
replace lean_democrat = 0 if lean_democrat!=1

generate strong_republican = 1 if pid7==7
replace strong_republican = 0 if strong_republican!=1

generate medium_republican = 1 if pid7==6
replace medium_republican = 0 if medium_republican!=1

generate lean_republican = 1 if pid7==5
replace lean_republican = 0 if lean_republican!=1

* Control variables
generate male = 1 if gender==1
replace male = 0 if gender==2
summarize male

generate white = 1 if race==1
replace white = 0 if race!=1
summarize white

generate black = 1 if race==2
replace black = 0 if race!=2

generate hispanic = 1 if race==3
replace hispanic = 0 if race!=3

generate college = 1 if educ==3|educ==4|educ==5|educ==6
replace college = 0 if educ==1|educ==2
summarize college

generate age = 2017-birthyr
summarize age

generate employed = 1 if employ==1|employ==2
replace employed = 0 if employed!=1
summarize employed

egen state_id = group(inputstate)

***Balance**********************************************************

logit treatment republican democrat male white college age employed

***Analysis*********************************************************

summarize not_restrict_immigrant
ttest not_restrict_immigrant, by(treatment)
reg not_restrict_immigrant treatment, vce(robust)

reg not_restrict_immigrant treatment republican democrat male white college age employed i.state_id, vce(robust)

summarize not_restrict_immigrant if democrat==1
summarize not_restrict_immigrant if republican==1
summarize not_restrict_immigrant if independent==1
* Magnitude of treatment effect is one-third of difference between reps and indeps,
* and one-fifth of difference between reps and dems.

***Subgroup Effects*********************************************************

* Partisanship 

reg not_restrict_immigrant treatment male white college age employed i.state_id if republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.state_id if democrat==1, vce(robust)

reg not_restrict_immigrant treatment male white college age employed i.state_id if strong_republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.state_id if medium_republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.state_id if lean_republican==1, vce(robust)

reg not_restrict_immigrant treatment male white college age employed i.state_id if independent==1, vce(robust)

reg not_restrict_immigrant treatment male white college age employed i.state_id if strong_democrat==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.state_id if medium_democrat==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.state_id if lean_democrat==1, vce(robust)

* Immigration Generation

reg not_restrict_immigrant treatment male white college age employed i.state_id if my_grandparent_generation==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.state_id if my_grandparent_generation==0, vce(robust)

* Dataset for plotting and pooled analysis
*generate wave = 1
*keep not_restrict_immigrant treatment republican strong_republican medium_republican lean_republican independent lean_democrat medium_democrat strong_democrat democrat male white college age employed state_id wave
*sa wave1_analysis_data

********************************************************************
*********************** Wave 2 *************************************
********************************************************************

***CLEANING*********************************************************

use family_history_wave2_data, clear

keep if finished==1

* Generating treatment variable for whether family history question was asked first.
rename immigration_treatment treatment
tabulate treatment

* Outcome measure 1: Do you agree or disagree that the U.S. should limit the number of immigrants entering the country?
* 1 = strongly agree, 7 = strongly disagree
tabulate immigrant_restrict
rename immigrant_restrict not_restrict_immigrant
summarize not_restrict_immigrant

* Outcome measure 2: On a scale from 1 to 100, how do you feel about immigrants in the United States?
*1 = I view immigrants extremely unfavorably 
*100 = I view immigrants extremely favorably
tabulate immigrant_therm
summarize immigrant_therm

* Answers to family history question about when family came to US
tabulate immigrant_history
summarize immigrant_history

* Dummy variable for family arriving during or after grandparents' generation
generate my_grandparent_generation = 1 if immigrant_history==1|immigrant_history==2|immigrant_history==3
replace my_grandparent = 0 if immigrant_history==4|immigrant_history==5

* Partisanship variables
generate republican = 1 if partisanship==2
replace republican = 0 if partisanship!=2
summarize republican

generate democrat = 1 if partisanship==1
replace democrat = 0 if partisanship!=1
summarize democrat

generate independent = 1 if partisanship==3|partisanship==4|partisanship==5
replace independent = 0 if independent!=1
summarize independent

generate strong_democrat = 1 if strong_dem==1
replace strong_democrat = 0 if strong_democrat!=1
summarize strong_democrat

generate medium_democrat = 1 if strong_dem==2
replace medium_democrat = 0 if medium_democrat!=1
summarize medium_democrat

generate lean_democrat = 1 if partisan_lean==1
replace lean_democrat = 0 if lean_democrat!=1
summarize lean_democrat

generate strong_republican = 1 if strong_repub==1
replace strong_republican = 0 if strong_republican!=1
summarize strong_republican

generate medium_republican = 1 if strong_repub==2
replace medium_republican = 0 if medium_republican!=1
summarize medium_republican

generate lean_republican = 1 if partisan_lean==2
replace lean_republican = 0 if lean_republican!=1
summarize lean_republican

generate trump_approve = 1 if approvetr==1|approvetr==2|approvetr==3
replace trump_approve = 0 if trump_approve!=1
summarize trump_approve

* Control variables
generate male = 1 if gender==1
replace male = 0 if gender==2
summarize male

generate college = 0 if education==1|education==2
replace college = 1 if college!=0
summarize college

generate white = 1 if v20==1
replace white = 0 if white!=1
summarize white

generate black = 1 if v20==2
replace black = 0 if v20!=2

generate employed = 1 if employment==1|employment==2
replace employed = 0 if employed!=1
summarize employed

summarize age

tabulate region
summarize region

***Balance**********************************************************

logit treatment republican democrat male white college age employed

***Analysis*********************************************************

* No Restrictions Outcome

summarize not_restrict_immigrant
ttest not_restrict_immigrant, by(treatment)
reg not_restrict_immigrant treatment, vce(robust)

reg not_restrict_immigrant treatment republican democrat male white college age employed i.region, vce(robust)

* Magnitude is one-fourth of difference between reps and indeps, and one-sixth
* of difference between reps and dems.
summarize not_restrict_immigrant if democrat==1
summarize not_restrict_immigrant if independent==1
summarize not_restrict_immigrant if republican==1

* Feeling Thermometer Outcome

summarize immigrant_therm
ttest immigrant_therm, by(treatment)
reg immigrant_therm treatment, vce(robust)

reg immigrant_therm treatment republican democrat male white college age employed i.region, vce(robust)

* Magnitude is over one-third of difference between reps and indeps, and 
* over one-sixth of difference between reps and dems.
summarize immigrant_therm if democrat==1
summarize immigrant_therm if independent==1
summarize immigrant_therm if republican==1

***Subgroup Analysis*********************************************************

*** No Restrictions Outcome

* Partisanship
reg not_restrict_immigrant treatment male white college age employed i.region if republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.region if democrat==1, vce(robust)

reg not_restrict_immigrant treatment male white college age employed i.region if strong_republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.region if medium_republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.region if lean_republican==1, vce(robust)

reg not_restrict_immigrant treatment male white college age employed i.region if independent==1, vce(robust)

reg not_restrict_immigrant treatment male white college age employed i.region if strong_democrat==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.region if medium_democrat==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.region if lean_democrat==1, vce(robust)

* Trump Approval
reg not_restrict_immigrant treatment male white college age employed i.region if trump==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.region if trump==0, vce(robust)

* Immigration Generation
reg not_restrict_immigrant treatment male white college age employed i.region if my_grandparent_generation==1, vce(robust)
reg not_restrict_immigrant treatment male white college age employed i.region if my_grandparent_generation==0, vce(robust)

*** Feeling Thermometer Outcome

* Partisanship 

reg immigrant_therm treatment male white college age employed i.region if republican==1, vce(robust)
reg immigrant_therm treatment male white college age employed i.region if democrat==1, vce(robust)

reg immigrant_therm treatment male white college age employed i.region if strong_republican==1, vce(robust)
reg immigrant_therm treatment male white college age employed i.region if medium_republican==1, vce(robust)
reg immigrant_therm treatment male white college age employed i.region if lean_republican==1, vce(robust)

reg immigrant_therm treatment male white college age employed i.region if independent==1, vce(robust)

reg immigrant_therm treatment male white college age employed i.region if strong_democrat==1, vce(robust)
reg immigrant_therm treatment male white college age employed i.region if medium_democrat==1, vce(robust)
reg immigrant_therm treatment male white college age employed i.region if lean_democrat==1, vce(robust)

* Trump Approval 

reg immigrant_therm treatment male white college age employed i.region if trump==1, vce(robust)
reg immigrant_therm treatment male white college age employed i.region if trump==0, vce(robust)

* Immigration Generation 

reg immigrant_therm treatment male white college age employed i.region if my_grandparent_generation==1, vce(robust)
reg immigrant_therm treatment male white college age employed i.region if my_grandparent_generation==0, vce(robust)

* Saving dataset for plotting and pooled analysis
*generate wave = 2
*keep not_restrict_immigrant immigrant_therm treatment republican strong_republican medium_republican lean_republican independent lean_democrat medium_democrat strong_democrat democrat male white college age employed region wave
*rename immigrant_therm_1 immigrant_therm
*sa wave2_analysis_data


********************************************************************
*********************** Wave 3 *************************************
********************************************************************

***CLEANING*********************************************************

use family_history_wave3_data, clear

* Generating treatment variable for whether family history question was asked first.
rename family_history_treatment treatment
tabulate treatment

* Outcome measure 1: Do you agree or disagree that the U.S. should limit the number of immigrants entering the country?
rename immigrationlimitint not_restrict_immigrant
replace not_restrict_immigrant = 8-not_restrict_immigrant
*1 = strongly agree, 7 = strongly disagree
tabulate not_restrict_immigrant
summarize not_restrict_immigrant

* Outcome measure 2: On a scale from 0 to 100, how do you feel about immigrants in the United States?
*0 = I view immigrants extremely unfavorably 
*100 = I view immigrants extremely favorably
rename immigrationtherm_1 immigrant_therm
tabulate immigrant_therm
summarize immigrant_therm

* Answers to family history question about when family came to US
rename immigrationtreat immigrant_history
tabulate immigrant_history
summarize immigrant_history

generate my_grandparent_generation = 1 if immigrant_history=="My generation"|immigrant_history=="My parents' generation"|immigrant_history=="My grandparents' generation"
replace my_grandparent_generation = 0 if my_grandparent_gen!=1

* Partisanship

generate republican = 1 if partyid=="Lean Republican"|partyid=="Republican"|partyid=="Strong Republican"
replace republican = 0 if republican!=1
summarize republican

generate democrat = 1 if partyid=="Lean Democrat"|partyid=="Democrat"|partyid=="Strong Democrat"
replace democrat = 0 if democrat!=1
summarize democrat

generate strong_democrat = 1 if partyid=="Strong Democrat"
replace strong_democrat = 0 if strong_democrat!=1

generate medium_democrat = 1 if partyid=="Democrat"
replace medium_democrat = 0 if medium_democrat!=1

generate lean_democrat = 1 if partyid=="Lean Democrat"
replace lean_democrat = 0 if lean_democrat!=1

generate strong_republican = 1 if partyid=="Strong Republican"
replace strong_republican = 0 if strong_republican!=1

generate medium_republican = 1 if partyid=="Republican"
replace medium_republican = 0 if medium_republican!=1

generate lean_republican = 1 if partyid=="Lean Republican"
replace lean_republican = 0 if lean_republican!=1

generate independent = 1 if partyid=="Independent"
replace independent = 0 if independent!=1

rename trumpapproval2 trump_approve

* Control Variables

generate male = 1 if gender=="Male"
replace male = 0 if male!=1
summarize male

generate college = 0 if educ=="Did not graduate high school"|educ=="High school graduate"
replace college = 1 if college!=0
summarize college

generate white = 1 if race=="White"&latino=="No"
replace white = 0 if white!=1
summarize white

summarize age

egen state_id = group(birthstate)
replace state_id = 54 if birthstate==""

***Balance**********************************************************

logit treatment male republican democrat college white age

***Analysis*********************************************************

* No Restrictions Outcome

summarize not_restrict_immigrant
ttest not_restrict_immigrant, by(treatment)
reg not_restrict_immigrant treatment, vce(robust)

reg not_restrict_immigrant treatment republican democrat male white college age i.state_id, vce(robust)

* Feeling Thermometer Outcome

summarize immigrant_therm
ttest immigrant_therm, by(treatment)
reg immigrant_therm treatment, vce(robust)

reg immigrant_therm treatment republican democrat male white college age i.state_id, vce(robust)

* Magnitude is more than two-thirds of difference between reps and indeps, and
* over one-tenth of difference between reps and dems.
summarize immigrant_therm if republican==1
summarize immigrant_therm if democrat==1
summarize immigrant_therm if independent==1

***Subgroup Analysis*********************************************************

*** No Restrictions Outcome

* Partisanship
reg not_restrict_immigrant treatment male white college age i.state_id if republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age i.state_id if democrat==1, vce(robust)

reg not_restrict_immigrant treatment male white college age i.state_id if strong_republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age i.state_id if medium_republican==1, vce(robust)
reg not_restrict_immigrant treatment male white college age i.state_id if lean_republican==1, vce(robust)

reg not_restrict_immigrant treatment male white college age i.state_id if independent==1, vce(robust)

reg not_restrict_immigrant treatment male white college age i.state_id if strong_democrat==1, vce(robust)
reg not_restrict_immigrant treatment male white college age i.state_id if medium_democrat==1, vce(robust)
reg not_restrict_immigrant treatment male white college age i.state_id if lean_democrat==1, vce(robust)

* Trump Approval
reg not_restrict_immigrant treatment male white college age i.state_id if trump_ap==1, vce(robust)
reg not_restrict_immigrant treatment male white college age i.state_id if trump_ap==0, vce(robust)

* Generation of Immigration
reg not_restrict_immigrant treatment male white college age i.state_id if my_grandparent_gen==1, vce(robust)
reg not_restrict_immigrant treatment male white college age i.state_id if my_grandparent_gen==0, vce(robust)

*** Feeling Thermometer Outcome

* Partisanship
reg immigrant_therm treatment male white college age i.state_id if republican==1, vce(robust)
reg immigrant_therm treatment male white college age i.state_id if democrat==1, vce(robust)

reg immigrant_therm treatment male white college age i.state_id if strong_republican==1, vce(robust)
reg immigrant_therm treatment male white college age i.state_id if medium_republican==1, vce(robust)
reg immigrant_therm treatment male white college age i.state_id if lean_republican==1, vce(robust)

reg immigrant_therm treatment male white college age i.state_id if independent==1, vce(robust)

reg immigrant_therm treatment male white college age i.state_id if strong_democrat==1, vce(robust)
reg immigrant_therm treatment male white college age i.state_id if medium_democrat==1, vce(robust)
reg immigrant_therm treatment male white college age i.state_id if lean_democrat==1, vce(robust)

* Trump Approval
reg immigrant_therm treatment male white college age i.state_id if trump_ap==1, vce(robust)
reg immigrant_therm treatment male white college age i.state_id if trump_ap==0, vce(robust)

* Generation of Immigration
reg immigrant_therm treatment male white college age i.state_id if my_grandparent_gen==1, vce(robust)
reg immigrant_therm treatment male white college age i.state_id if my_grandparent_gen==0, vce(robust)

***Empathy Analysis*********************************************************

*** Mediation

generate immigrant_emp = 7 if immigrationempathy=="Strongly agree"
replace immigrant_emp = 6 if immigrationempathy=="Agree"
replace immigrant_emp = 5 if immigrationempathy=="Somewhat agree"
replace immigrant_emp = 4 if immigrationempathy=="Neither agree nor disagree"
replace immigrant_emp = 3 if immigrationempathy=="Somewhat disagree"
replace immigrant_emp = 2 if immigrationempathy=="Disagree"
replace immigrant_emp = 1 if immigrationempathy=="Strongly disagree"

summarize immigrant_emp

reg immigrant_emp treatment,vce(robust)
reg immigrant_therm treatment,vce(robust)
reg immigrant_therm immigrant_emp treatment,vce(robust)

reg immigrant_emp treatment republican democrat male white college age i.state_id,vce(robust)
reg immigrant_therm treatment republican democrat male white college age i.state_id,vce(robust)
reg immigrant_therm immigrant_emp treatment republican democrat male white college age i.state_id,vce(robust)

* Saving dataset for plotting and pooled analysis
*generate wave = 3
*keep not_restrict_immigrant immigrant_therm treatment republican strong_republican medium_republican lean_republican independent lean_democrat medium_democrat strong_democrat democrat male white college age state_id wave
*sa wave3_analysis_data

********************************************************************
*********************** Combined ***********************************
********************************************************************

*** Making pooled dataset for analysis

use "wave3_analysis_data", clear

append using "wave2_analysis_data"

append using "wave1_analysis_data"

* sa "pooled_analysis_data"

* Analysis

summarize not_restrict_immigrant
ttest not_restrict_immigrant, by(treatment)
reg not_restrict_immigrant treatment, vce(robust)

summarize immigrant_therm
ttest immigrant_therm, by(treatment)
reg immigrant_therm treatment, vce(robust)
