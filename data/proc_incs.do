set more off
clear all

*===============================================================================
// This do-file processes poverty for numerous income measures, it also produces
// inequality values for these incomes. 
// The file is inefficient and is meant to test how effectively people can work 
// with Stata.
// With a 30,000 observation database, the program below takes roughly 360  
// seconds to run...can you do it faster?
*===============================================================================

timer on 1

global data "C:\Users\WB378870\OneDrive - WBG\0.GPWG misc\Stata Functions\Tests\"

use "$data\Incomes.dta", clear

local peso weight
local lines 3000 2000 5500

//Get poverty headcount rate, gap, and gap square for each region, for all lines
//for all income variables

foreach x of varlist income*{
local nm=subinstr("`x'", "income_","",.)
	foreach l of local lines{
		forval a=0/2{
			gen pov_i`nm'_fgt`a'_`l' = (`x'<`l')*(1-(`x'/`l'))^`a'	
		}
	}
}

preserve

collapse (mean) pov* [aw=`peso'] , by(region)

export excel using "$data\Test.xlsx", sheet(Pov_reg) sheetreplace firstrow(variables) 
restore

preserve
collapse (mean) pov* [aw=`peso']

export excel using "$data\Test.xlsx", sheet(Pov_nat) sheetreplace firstrow(variables) 
restore



putexcel set  "$data\Test.xlsx", sheet(gini_theil, replace) modify

//Gini and Theil index of all incomes
foreach x of varlist income*{
	ainequal `x' [aw=weight]
	mat a = r(b)
	mat coln a = `x'
	mat total = nullmat(total),a	
}

putexcel A1= matrix(total), rownames


timer off 1
timer list
