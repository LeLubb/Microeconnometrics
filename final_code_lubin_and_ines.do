use "HRS_cognitive.dta", clear

gen age2 = age^2
gen score = cognitive + recall

* Define instruments with the (+1) year honeymoon lag
gen z62 = (age >= 63)
gen z65 = (age >= 66)

xtset id year

*------------------------------------------------------------------------------*
* 1. First stage, joint significance and reduced form
*------------------------------------------------------------------------------*

* first stage
xtreg ret1 z62 z65 age age2, fe
eststo first_stage
// esttab first_stage using "C:\Users\lubin\Documents\STATA\results.tex", replace ///
//     b(3) t(3) ///
//     label title("First Stage Regression") ///
//     booktabs alignment(c)
* test of joint significance 
test z62 z65
local first_f = r(F)
display "First stage F-stat: " `first_f'
estadd scalar first_f = `first_f' : first_stage
// esttab first_stage using "C:/Users/sandrine/Documents/STATA/F_test.tex", replace ///
//     b(3) t(3) ///
//     label title("First Stage Regression") ///
//     booktabs alignment(c) ///
//     stats(first_f N, labels("First-stage F-stat" "Observations") fmt(3 0))

* reduced form
xtreg score z62 z65 age age2, fe
eststo reduced_form
// esttab reduced_form using "C:\Users\sandrine\Documents\STATA\results_reduced_form.tex", replace ///
//     b(3) t(3) ///
//     label title("Reduced Form Regression") ///
//     booktabs alignment(c)
*------------------------------------------------------------------------------*
* 2. second stage
*------------------------------------------------------------------------------*
* 
xtivreg score (ret1 = z62 z65) age age2, fe
eststo second_stage
// esttab second_stage using "C:\Users\sandrine\Documents\STATA\results_second_stage.tex", replace ///
//     b(3) t(3) ///
//     label title("Second Stage Regression") ///
// 	booktabs alignment(c)
	
	
* Overidentification Test 

xtivreg score (ret1 = z62) age age2, fe
estimates store iv_z62

xtivreg score (ret1 = z65) age age2, fe
estimates store iv_z65

hausman iv_z62 iv_z65
local chi2 = r(chi2)
local pval = r(p)

xtivreg score (ret1 = z62 z65) age age2, fe
eststo full_model

estadd scalar haus_chi2 = `chi2' : full_model
estadd scalar haus_p = `pval' : full_model
// esttab full_model using "C:/Users/sandrine/Documents/STATA/overid_results.tex", replace ///
//     b(3) t(3) label booktabs alignment(c) ///
//     title("Second Stage with Overidentification Test") ///
//     stats(haus_chi2 haus_p N, ///
//           labels("Hausman $\chi^2$" "Hausman p-value" "Observations") ///
//           fmt(3 3 0))

*------------------------------------------------------------------------------*
* 3. Refinement part: heterogeneous effects by education
*------------------------------------------------------------------------------*

summarize year_edu, meanonly
gen edu_centered = year_edu - r(mean)

gen ret_x_edu = ret1 * edu_centered

gen z62_x_edu = z62 * edu_centered
gen z65_x_edu = z65 * edu_centered

xtivreg score (ret1 ret_x_edu = z62 z65 z62_x_edu z65_x_edu) age age2, fe
eststo model_interaction
// esttab model_interaction using "C:/Users/sandrine/Documents/STATA/interaction_results.tex", replace ///
//     b(3) t(3) ///
//     label title("Structural Equation with Heterogeneous Effects") ///
//     booktabs alignment(c) ///
//     rename(ret1 "Retirement" ret_x_edu "Retirement x Education") ///
//     addnotes("Education is centered at its sample mean." "Instruments include Z62, Z65, and their interactions with education.")

* Storage with all the resutls

// esttab first_stage reduced_form second_stage model_interaction ///
//     using "C:/Users/sandrine/Documents/STATA/Main_Results_Table.tex", replace ///
//     b(3) t(3) label booktabs alignment(c) ///
//     mtitles("First Stage" "Reduced Form" "Second Stage" "Interaction") ///
//     rename(ret1 "Retirement" ret_x_edu "Retirement x Edu" z62 "Z62 (62+1)" z65 "Z65 (65+1)") ///
//     stats(first_f N, labels("First-stage F-stat" "Observations") fmt(3 0)) ///
//     title("Impact of Retirement on Cognition: IV and Heterogeneous Effects") ///
//     addnotes("Note: First stage and reduced form are OLS Fixed Effects. Columns 3 and 4 are 2SLS Fixed Effects." ///
//              "Education is centered at its sample mean for the interaction model.")

*------------------------------------------------------------------------------*
* 4. ploting varaibles
*------------------------------------------------------------------------------*


use "HRS_cognitive.dta", clear

local numeric_vars cognitive recall year_edu age


foreach var of local numeric_vars {
    twoway (kdensity `var', recast(area) ///  
        fcolor("76 114 176%40") ///           
        lcolor("76 114 176")), ///            
        title("Distribution of `var'") ///
        xtitle("`var'") ///
        ytitle("Density") ///
        legend(off) ///
        name(gr_`var', replace)
}


graph combine gr_cognitive gr_recall gr_year_edu gr_age, ///
    cols(3) ///
    ysize(12) xsize(18) ///
    iscale(*0.8)

histogram cognitive, ///
    bin(20) ///
    frequency ///
    title("Distribution of cognitive test scores") ///
    xtitle("Cognitive score") ///
    ytitle("Frequency")
	
graph box cognitive, ///
    horizontal ///
    title("Boxplot of cognitive test scores") ///
    ytitle("Cognitive score")
