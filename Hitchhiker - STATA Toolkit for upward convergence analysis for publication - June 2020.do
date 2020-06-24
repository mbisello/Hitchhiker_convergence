
***********************************************************************************************************************
***********************************************************************************************************************
*****                          HITCHHIKER - VERSION 2.0 JUN 2020                                                  *****
*****                       A TOOLKIT FOR UPWARD CONVERGENCE ANALYSIS IN EUROPE                                   ***** 
*****              AN APPLICATION TO THE EMPLOYMENT RATE (AGE 20-64) FROM 2002 TO 2016                            *****
***********************************************************************************************************************                                                                                                             
***********************************************************************************************************************
*****                                                                                                             ***** 
*****                                                                                                             ***** 
*****          Hitchhiker@Eurofound is a set of scripts developed in STATA software and created by:               *****
*****                Martina Bisello, Research Officer (Eurofound) - mbs@eurofound.europa.eu;                     *****
*****               Massimiliano Mascherini, Senior Research Manager (Eurofound) - mam@eurofound.europa.eu;       *****
*****                                                                                                             *****
*****  The scripts allow the analysis of upward/downward convergence trends, building on the methodology          *****
*****  developed in  "Eurofound (2018), Upward convergence in the EU: Concepts, measurements and indicators,      *****
*****  Publications Office of the European Union, Luxembourg".                                                    *****
*****  The report can be downloaded here: https://goo.gl/ttS8wY   												  *****
*****  The above report should be cited in any publication whose output/data analysis has been generated          *****
*****  with this script.                                                                                          *****
*****                                                                                                             *****
*****  The scripts allow the computation of Beta, Sigma and Delta Convergence                                     *****
*****  as well as the investigation of Member States dynamics (comparison of MS trends against the EU average     *****
*****  and analysis of the magnitude of MS convergence/divergence trends).                                        *****
*****                                                                                                             ***** 
*****  The scripts are set to work with the attached data set (DB.dta)                                            *****
*****  By way of example, the employment rate is chosen as a variable for the analysis over the period 2000-2016  *****
*****  The script can be adapted to work with any other indicator in the database. However, missing data will     *****
*****  need to be checked since the scripts is set to work with non missing values for the indicator of interest. *****
*****  If the policy objective is the minimisation, rather than maximisation, of the value of indicator of        *****
*****  interest the script needs to be adjusted accordingly.                                                      *****
*****  																											  *****
*****  STATA packages grc1leg and gr0034 should be installed for the script to work correctly 		              *****
*****  The script comes with no warranty and its use is regulated by the attached licence.                        *****
***********************************************************************************************************************
***********************************************************************************************************************


**********************************************************************************************************************
**********************************************************************************************************************
*****                                           PART 1                                                           *****
*****                                     DATA PREPARATION                                                       ***** 
**********************************************************************************************************************                                                                                                             
**********************************************************************************************************************




***************************************************************
** SETTING FOLDERS AND CLEANING TEMPORARY GRAPHICAL OUTPUTS ***
***************************************************************

clear all
set more off

cd "xxx"
local path = "xxx"
use "`path'\DB.dta", clear // open DB file 


local list : dir . files "*.gph" /*drop temporary graphical outputs */
foreach f of local list {
    erase "`f'"
}

**************************************
** SELECTING COUNTRIES OF INTEREST ***
**************************************

*** keep only EU27 Member States

 keep if geo=="AT" ///
| geo=="BE" ///
| geo=="BG" ///
| geo=="HR" ///
| geo=="CY" ///
| geo=="CZ" ///
| geo=="DK" ///
| geo=="EE" ///
| geo=="FI" ///
| geo=="FR" ///
| geo=="DE" ///
| geo=="EL" ///
| geo=="HU" ///
| geo=="IE" ///
| geo=="IT" ///
| geo=="LV" ///
| geo=="LT" ///
| geo=="LU" ///
| geo=="MT" ///
| geo=="NL" ///
| geo=="PL" ///
| geo=="PT" ///
| geo=="RO" ///
| geo=="SK" ///
| geo=="SI" ///
| geo=="ES" ///
| geo=="SE" 

**** create numerical variable country for further analysis

encode geo, gen(country)
numlabel, add
tab country 



*************************************************************
** SELECTING VARIABLE OF ANALYSIS                         ***
*************************************************************

*keep only variables of interest (in this case the employment rate) and rename the indicator

keep emp_20_64_p country geo time sex 
rename emp_20_64_p indicator

*select Total/Female/Males (some indicators are disaggregated by gender; not considered at this stage) 
keep if sex=="Total"


*************************************************************
** PERFORM ANALYSIS OF MISSING VALUES                     ***
*************************************************************


************This part can be disregarded if you are already working on data without missing values**************

/*let's have a look at the distribution of missing values by year and country*/
egen mis=rowmiss(indicator)
replace mis=. if mis==0
table  country time , c(mean mis)


/*we keep only those years where we have a non missing value of the employment rate for each MS*/
keep if mis==.
bysort time: egen N = count(indicator)  
tab N

keep if N==27 
cap drop N mis
tab time country /*so now we have a full dataset for all EU countries from 2002 to 2016*/

**********************************************************************************************************************




**********************************************************************************************************************
**********************************************************************************************************************


**********************************************************************************************************************
**********************************************************************************************************************
*****                                           PART 2                                                           *****
*****                             COMPUTATION OF BETA, SIGMA AND DELTA CONVERGENCE                               ***** 
**********************************************************************************************************************                                                                                                             
**********************************************************************************************************************



************************************************************
* COMPUTATION OF UNWEIGHTED AVERAGES AND SIGMA CONVERGENCE *
************************************************************



** generate the unweighted average at the EU27 level, the standard deviation, and the coefficient of variation by year **

egen EUmean=mean(indicator) , by(time) // generate the (unweighted) average at EU level for each year
egen EUsd=sd(indicator), by(time) // generate the standard deviation at EU27 level for each year
bysort time: gen N=_N
replace EUsd  = EUsd*sqrt((N-1)/N) // replace with the population standard deviation, where N=27 
drop N
gen EUcv=EUsd/EUmean*100 // generate the coefficient of variation multiplied by 100

*** A GRAPHICAL REPRESENTATION OF SIGMA CONVERGENCE FOR THE EU27 

tw line EUmean time if country==1, ytitle("Average") xtitle("Years")   xlabel(2002(2)2016, angle(vertical))  graphregion(fcolor(white) lcolor(white) color(white) icolor(white) ) saving(mean) 
graph export "UNWEIGHTED AVERAGE_EU27.pdf", replace
tw line EUsd time if country==1, ytitle("Standard deviation") xtitle("Years")   xlabel(2002(2)2016, angle(vertical))  graphregion(fcolor(white) lcolor(white) color(white) icolor(white) ) saving(sd)   
graph export "STANDARD DEVIATION_EU27.pdf", replace
tw line EUcv time if country==1 , ytitle("Coefficient of variation") xtitle("Years")  xlabel(2002(2)2016, angle(vertical))    graphregion(fcolor(white) lcolor(white) color(white) icolor(white) ) saving(cv)
graph export "COEFFICIENT OF VARIATION_EU27.pdf", replace
graph combine  "mean.gph" "sd.gph" "cv.gph" , rows(1) graphregion(fcolor(white) lcolor(white) color(white) icolor(white)) 
graph export "SIGMA_convergence_EU27.pdf", replace

** now do the same for the Euro Area only **

gen ea=0
replace ea=1 if country==1 | country==2 | country==4 | country==6| country==8 | country==9 | country==10  /// 
 | country==11 | country==12  | country==15 | country==16 | country==17 | country==18 | country==19 ///
  | country==20 | country==21 | country==23 | country==26   | country==27 
tab country ea 

egen EAmean=mean(indicator) if ea==1 , by(time) 
egen EAsd=sd(indicator) if ea==1 , by(time)
bysort time ea: gen N=_N
replace N=. if ea==0
replace EAsd  = EAsd*sqrt((N-1)/N) // replace with the population standard deviation, where N=27 
drop N
gen EAcv=EAsd/EAmean*100 


** and the Non-Euro area**

egen NEAmean=mean(indicator)  if ea==0 , by(time) 
egen NEAsd=sd(indicator) if ea==0 , by(time) 
bysort time ea: gen N=_N
replace N=. if ea==1
replace NEAsd  = NEAsd*sqrt((N-1)/N) // replace with the population standard deviation, where N=27 
drop N

gen NEAcv=NEAsd/NEAmean*100 


*** A GRAPHICAL REPRESENTATION OF SIGMA CONVERGENCE FOR THE EURO/NONEURO AREA

tw line EAsd time if country==1 || line NEAsd time if country==3 ,  ytitle("Standard deviation") xtitle("Years")   xlabel(2002(2)2016, angle(vertical))   legend(lab(1 "Euro area") lab(2 "Non Euro area")) graphregion(fcolor(white) lcolor(white) color(white) icolor(white) )  saving(sd2) /*country==1 is Austria (Euro area); country==3 is Bulgaria ( non EA)*/
tw line EAcv time if country==1 || line NEAcv time if country==3 ,  ytitle("Coefficient of variation") xtitle("Years") xlabel(2002(2)2016, angle(vertical))     graphregion(fcolor(white) lcolor(white) color(white) icolor(white) )  saving(cv2)
tw line EAmean time if country==1 || line NEAmean time if country==3 ,  ytitle("Average") xtitle("Years") xlabel(2002(2)2016, angle(vertical))   graphregion(fcolor(white) lcolor(white) color(white) icolor(white) )  saving(mean2)
grc1leg  "mean2.gph" "sd2.gph" "cv2.gph"    , rows(1) graphregion(fcolor(white) lcolor(white) color(white) icolor(white) )  legendfrom("sd2.gph")
graph export "SIGMA_convergence_EAvsNEA.pdf", replace


*************************************************************************************




***************************************************************************************************
* COMPUTATION OF DELTA CONVERGENCE                                                                *
* Note that this script is adapted for the EMPLOYMENT RATE, hence best performer = highest value  *
* If best performer = lowest value (as per UNEMPLOYMENT RATE), minimisation is required instead   *
***************************************************************************************************

sort country time

gen diff=(indicator-EUmean) // simple deviation from EU mean


**  IDENTIFYING BEST PERFORMER  

egen maxdiff=max(diff), by(time) // greatest positive deviation from the EU mean
egen mindiff=min(diff), by(time) // greatest negative deviation from the EU mean
gen maxcountry=1  if maxdiff==diff
gen mincountry=1  if  mindiff==diff

table country time, c(mean maxcountry) //best performing country in each year takes value 1
table country time, c(mean mincountry) //worst performing country in each year takes value 1

egen bestperf=max(indicator), by(time) // if the indicator requires minimisation, use the following code: egen bestperf=min(indicator), by(time) 
egen worstperf=min(indicator), by(time) // if the indicator requires minimisation, use the following code: egen worstperf=max(indicator), by(time) 


**  PLOTTING BEST PERFORMER VS EACH MEMBER STATES IF MAXIMIZATION IS REQUIRED (AS EMPLOYMENT RATE FOR EXAMPLE)

tw line indicator time , by(country) || line bestperf time , xlabel(2002(2)2016, angle(vertical))  legend(lab(1 "Employment rate") lab(2 "Best performance"))  ///
 title("" , size(large)) xtitle("") graphregion(fcolor(white) lcolor(white) color(white) icolor(white) margin(zero)) note("") 
gr export "DELTA-MS against best performance_maximization.pdf" , replace  

*******************************************************************************************************
** COMPUTATION OF DELTA CONVERGENCE AS SUM OF DISTANCES FROM BEST PERFORMER if MAXIMIZATION APPLIES   *
*******************************************************************************************************

gen deltabest=bestperf-indicator //distance from best performance
gen deltaworst=indicator-worstperf //distance from worst performance

sort time country  
by time: egen deltasum=sum(deltabest) // sum of distances from best performance


** GRAPHICAL REPRESENTATION OF DELTA CONVERGENCE
tw line deltasum time, xlabel(2002(2)2016, angle(vertical))  ytitle("")  ///
 title("" , size(large)) xtitle("") graphregion(fcolor(white) lcolor(white) color(white) icolor(white) margin(zero)) note("") 
gr export "DELTA_convergence_maximization.pdf" , replace  
*************************************************************************************



*************************************************************************************
* COMPUTATION OF BETA CONVERGENCE                                                   *  
*                                                                                   *
* Initial year is set to 2002 and final year to 2016                                * 
* These can be changed in the script and adapted to a different periodisation       *
*************************************************************************************


*************************************************************************************
* BETA CONVERGENCE                                                                  *
*************************************************************************************


preserve
keep if time==2002|time==2016   // adjust to the needed time interval
gsort  country -time

gen t0=2002 // adjust to the selected time interval
gen t1=2016 // adjust to the selected time interval

gen Tcapital = t1-t0 // total number of years in the selected time interval

gen logindicator=log(indicator) 
by country: gen change=(indicator[_n-1]/indicator) //ratio of employment rate in 2016 (final year) and 2002 (initial year)
gen logchange=(1/Tcapital)*log(change) //average rate of growth of indicator

**** beta convergence log growth
graph twoway (lfit logchange logindicator ) (scatter logchange logindicator, mlabel(geo))
  // graphical representation
gr export "BETA_convergence_log.pdf" , replace

regress logchange logindicator //regress the average rate of growth of the indicator against its initial value
 
restore






*************************************************************************************



**********************************************************************************************************************
**********************************************************************************************************************

**********************************************************************************************************************
**********************************************************************************************************************
*****                                           PART 3                                                           *****
*****                             COMPUTATION OF MEMBER STATES DYNAMICS                                          ***** 
**********************************************************************************************************************                                                                                                             
**********************************************************************************************************************






*************************************************************************************
* CLASSIFICATION OF CONVERGENCE PATTERNS                                            *
*************************************************************************************

/* The main variables that are necessary for the analysis of Member States dynamics and for classifying the different patterns of convergence are created below. These are: 
- dEUmean: the yearly change in the average employment rate at the EU level
- dMSmean: the yearly change in the average employment rate for each MS
- dsqdiff: the yearly change in the (squared) differences from the EU mean */

sort country time
by country: gen dEUmean=EUmean - EUmean[_n-1] // yearly change in the average employment rate at the EU level (by construction the firt year is always missing)
by country: gen dMSmean=indicator - indicator[_n-1] // yearly change in the average employment rate for each MS
gen sqdiff=diff^2 // the squared deviation from the EU mean
by country: gen dsqdiff= sqdiff - sqdiff[_n-1]  // yearly change in the squared differences from the EU mean

/* From a purely mathematical point of view, a total of 20 patterns (plus a residual category with other remaining cases) can be identified: 
- 3 patterns of upward convergence; 
- 3 patterns of upward divergence;
- 3 patterns of downward divergence;
- 3 patterns of downward convergence;
- 6 cases in which the trends in the average of the indicators are parallel at the EU and MS level, 
- 2 cases in which the trend lines cross each other instead
- a residual category */

gen value=. //by default, this will always be missing in the first year of analysis 

//Upward convergence:(1) Catching up; (2) Flattening; (3) Inversion
replace value=1 if  (dEUmean>=0) & (dEUmean<dMSmean) & (dMSmean>0) & (dsqdiff<0) 
replace value=2 if  (dEUmean>0) & (dEUmean>dMSmean) & (dMSmean>0) & (dsqdiff<0) 
replace value=3 if  (dEUmean>0) & (dEUmean>dMSmean) & (dMSmean<0) & (dsqdiff<0) 

//Upward divergence: (4) Outperforming; (5) Slower pace; (6) Diving
replace value=4 if  (dEUmean>0) & (dEUmean<dMSmean) & (dMSmean>0) & (dsqdiff>0) 
replace value=5 if  (dEUmean>0) & (dEUmean>dMSmean) & (dMSmean>0) & (dsqdiff>0) 
replace value=6 if  (dEUmean>0) & (dEUmean>dMSmean) & (dMSmean<0) & (dsqdiff>0) 

//Downward divergence: (7) Defending better; (8) Escaping; (9) Falling away
replace value=7 if  (dEUmean<0) & (dEUmean<dMSmean) & (dMSmean<0) & (dsqdiff>0) 
replace value=8 if  (dEUmean<=0) & (dEUmean<dMSmean) & (dMSmean>0) & (dsqdiff>0) 
replace value=9 if  (dEUmean<=0) & (dEUmean>dMSmean) & (dMSmean<0) & (dsqdiff>0) 

//Downward convergence: (10) Underperforming; (11) Recovering; (12) Reacting better
replace value=10 if  (dEUmean<=0) & (dEUmean>dMSmean) & (dMSmean<0) & (dsqdiff<0) 
replace value=11 if  (dEUmean<0) & (dEUmean<dMSmean) & (dMSmean>0) & (dsqdiff<0) 
replace value=12 if  (dEUmean<0) & (dEUmean<dMSmean) & (dMSmean<0) & (dsqdiff<0) 

//Parallel: (13) Parallel-better-over; (14) Parallel-equal-over; (15) Parallel-worse-over;(16) Parallel-worse-under;(17) Parallel-equal-under;(18) Parallel-better-under  
replace value=13 if  (dEUmean>0) & (dMSmean>0) & (dsqdiff==0) & (EUmean<indicator)
replace value=14 if  (dEUmean==0) & (dMSmean==0)  & (dsqdiff==0) & (EUmean<indicator)
replace value=15 if  (dEUmean<0) & (dMSmean<0)  & (dsqdiff==0) & (EUmean<indicator)
replace value=16 if  (dEUmean<0) & (dMSmean<0)  & (dsqdiff==0) & (EUmean>indicator)
replace value=17 if  (dEUmean==0) & (dMSmean==0)  & (dsqdiff==0) & (EUmean>indicator)
replace value=18 if  (dEUmean>0) & (dMSmean>0)  & (dsqdiff==0) & (EUmean>indicator)


//(19) Crossing and (20) Crossing reversed 
by country: gen cross=.
 replace cross=1 if diff[_n-1]>0&diff<0
 replace cross=2 if diff[_n-1]<0&diff>0
 replace cross=. if time==2002
 
replace value=19 if  (cross==1)
replace value=20 if (cross==2)

//Other remaining cases (21)
replace value=21 if value==. & time!=2002

table country time, c(mean value)  // patterns of convergence/diverge by country (note that the column 2003 indicates the convergence/divergence patterns from 2002 to 2003, and so on) 
// missing values in the table corresponds to those cases mentioned above of parallel or crossing trend lines. 

//Note that the combinations identified above rely on the assumption that upward convergence is reached through the maximisation of the indicator
//If the policy objective is a minimisation of the indicator (e.g. unemployment rate), please consult the conversion table available online in the tutorial. 
//For instance, the situation in which a Member State catches up with the EU average in terms of unemployment rate corresponds to case 10 and not 1

*************************************************************************************
* CLASSIFICATION OF MAGNITUDE OF CONVERGENCE                                        *
*************************************************************************************


// GRAPHICAL REPRESENTATION OF employment rate average by MS against EU average
tw line indicator time , by(country) || line EUmean time, xlabel(2002(2)2016, angle(vertical))  legend(lab(1 "Employment rate") lab(2 "Unweigthed average EU27"))  ///
 title("" , size(large)) xtitle("") graphregion(fcolor(white) lcolor(white) color(white) icolor(white) margin(zero)) note("") 
gr export "MS DYNAMICS_MS against EU average.pdf" , replace  


// GRAPHICAL REPRESENTATION OF deviation from the EU mean (so that the distribution is centered around the mean)
tw li diff time, by(country)  xlabel(, angle(vertical)) xlabel(2002(2)2016) yline(0, lcolor(green)) ytitle("Deviations from  the EU mean") graphregion(fcolor(white) ///
 lcolor(white) color(white) icolor(white) margin(zero))
gr export "MS DYNAMICS_Deviations from the mean.pdf" , replace


// TAKING INSPIRATION FROM THE JAF METHODOLOGY generate upper and lower bands defined on the basis of the EU standard deviation (-1/+1 std.)
gen upper = EUsd *(1) 
gen lower = EUsd *(-1)  

// TAKING INSPIRATION FROM JAF METHODOLOGY: GRAPHICAL REPRESENTATION OF deviations from the EU mean with respects to these bands
tw li diff time || line upper time , lcolor(magenta) || line lower time, by(country, note("" ) legend(off)) lcolor(magenta) ///
 xlabel(, angle(vertical)) xlabel(2002(2)2016)   ylabel(-20(10)20, angle(horizontal)) xtitle("") ///
 ytitle("") graphregion(fcolor(white) lcolor(white) color(white) icolor(white) margin(zero)) 
 gr export "MS DYNAMICS_JAF TYPE Deviations from the mean with bands.pdf" , replace
 
 
// TAKING INSPIRATION FROM THE JAF METHODOLOGY: generate an alert indicator for entrance/exit into/from bands (-1;+1 std)
gen inoutband=0
replace inoutband=-1 if diff<lower // this equals the condition: emp rate in the MS < (EU mean - 1*EUsd) 
replace inoutband=+1 if diff>upper
label var inoutband "Position in the band"
label define inoutband_lab 0 "Inside the bands" -1 "Below lower band"  +1 "Above the upper band"  
label values inoutband inoutband_lab 


// TAKING INSPIRATION FROM THE JAF METHODOLOGY: GRAPHICAL REPRESENTATION BY EACH MEMBER STATES OF CROSSING THESE BANDS
tw li inoutband time, by(country, note("Note: the alert indicator takes value 0 when employment rate in a give country is inside the bands" "+1 when is above the upper band, and -1 when is below the lower band" )) ///
lcolor(blue) xlabel(, angle(vertical)) xlabel(2002(2)2016)   ylabel(-1(1)1) ytitle("") xtitle("") ///
 ytitle("") graphregion(fcolor(white) lcolor(white) color(white) icolor(white) margin(zero))
gr export "MS DYNAMICS_JAF TYPE Alert indicators.pdf" , replace


// generate a categorical variables to identify countries always inside the bands, always above the upper band, always below the lower band and crossing the bands at least once
by country (inoutband), sort: gen crossbands = inoutband[1] != inoutband[_N] 
replace crossbands=2 if inoutband==+1 & crossbands==0
replace crossbands=3 if inoutband==-1 & crossbands==0
label var crossbands "Crossing bands"
label define crossbands_lab  0 "Always inside the bands" 1 "Crossing the bands" 2 "Always above the upper band" 3 "Always below the lower band" 
label values crossbands crossbands_lab
tab crossbands inoutband


// GRAPHICAL REPRESENTATION OF SUM OF YEARLY CHANGES IN THE ABSOLUTE DIFFERENCES FROM THE EU MEAN 

sort country time
by country: gen ddiff= abs(diff) - abs(diff[_n-1])  // yearly change in the absolute differences from the EU mean
 
egen positive=sum(ddiff) if ddiff>0 , by(country) // sum of positive yearly changes in the absolute differences from the EU mean, by country (i.e. the MS performance is deviating from the EU mean)
egen negative=sum(ddiff) if ddiff<0 , by(country) // sum of negative yearly changes in the absolute differences from the EU mean, by country (i.e. the MS performance is getting closer the EU mean)

table country, c(mean positive)
table country, c(mean negative)

 graph hbar positive, over(country)  title("Total increase in the gap" "with the EU mean") ytitle("") ///
  legend(cols(5)) graphregion(fcolor(white) lcolor(white) color(white) icolor(white)) saving(temp1)

   graph hbar negative , over(country)  title("Total decrease in the gap" "with the EU mean") ytitle("") ///
  legend(cols(5)) graphregion(fcolor(white) lcolor(white) color(white) icolor(white)) saving(temp2)

graph combine  "temp1" "temp2" , rows(1) graphregion(fcolor(white) lcolor(white) color(white) icolor(white) ) 

graph export "Sum of yearly changes in absolute differences from EU mean.pdf", replace





*************************************************************************************
* COMPUTATION OF STRICT/WEAK UPWARD CONVERGENCE                                     *
*************************************************************************************


sort country time
by country: gen totaldiff= indicator - indicator[_n-14]   //  overall change in employment rate per country; it gives us an indication of the performance over the entire period 2002-2016//
replace totaldiff=round(totaldiff, 0.1)

 
preserve
keep if totaldiff!=.
keep country totaldiff
egen min_totaldiff=min(totaldiff) //this identifies the country with the largest overall change
gen strict=1
replace strict=0 if min_totaldiff <0 //this code needs to be adjusted in case of an indicator for which we want a decrease, rather than an increase, over time (e.g. unemployment rate)
label define strict_lab 0 "No" 1 "Yes"  
label values strict strict_lab
export excel using "strict or weak convergence.xlsx", firstrow(variables)  replace
restore


**********************************************************************************************************************
**********************************************************************************************************************
*****                                          END                                                               ***** 
**********************************************************************************************************************                                                                                                             
**********************************************************************************************************************
