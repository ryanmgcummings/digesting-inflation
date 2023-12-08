clear all

import excel "~/Consumer sentiment dataset.xlsx", first	

drop if pce == . 

*** Panel A: Proportional Decay
* Model 1
nlsur (ics_all= {a}*cpi_log + {a}*(1-{decay})*cpi_yy_logs_lag_12 + {a}*(1-{decay})^2*cpi_yy_logs_lag_24 ///
		+ {b0}), robust
estimates store A1
* Model 2	
nlsur (ics_all= {a}*cpi_log + {a}*(1-{decay})*cpi_yy_logs_lag_12 + {a}*(1-{decay})^2*cpi_yy_logs_lag_24 ///
		+ {b1}*unemployment_rate + {b2}*ffr + {b0}), robust	
estimates store A2
* Model 3
nlsur (ics_all= {a}*cpi_log + {a}*(1-{decay})*cpi_yy_logs_lag_12 + {a}*(1-{decay})^2*cpi_yy_logs_lag_24 ///
		+ {b1}*unemployment_rate + {b3}*SP500_quarterly_return + {b4}*pce + {b0}), robust
estimates store A3
* Model 4		
nlsur (ics_all= {a}*cpi_log + {a}*(1-{decay})*cpi_yy_logs_lag_12 + {a}*(1-{decay})^2*cpi_yy_logs_lag_24 ///
		+ {b1}*unemployment_rate + {b2}*ffr + {b3}*SP500_quarterly_return + {b4}*pce + {b0}), robust
estimates store A4

*** Panel B: Year by Year
* Model 1
regress ics_all cpi_log cpi_yy_logs_lag_12 cpi_yy_logs_lag_24, robust
estimates store B1
* Model 2
regress ics_all cpi_log cpi_yy_logs_lag_12 cpi_yy_logs_lag_24 ///
	unemployment_rate ffr, robust
estimates store B2
* Model 3
regress ics_all cpi_log cpi_yy_logs_lag_12 cpi_yy_logs_lag_24 ///
	unemployment_rate SP500_quarterly_return pce, robust	
estimates store B3
* Model 4
regress ics_all cpi_log cpi_yy_logs_lag_12 cpi_yy_logs_lag_24 ///
	unemployment_rate ffr SP500_quarterly_return pce, robust
estimates store B4


*** Export in table.csv with one column per model
esttab A1 A2 A3 A4, /// PANEL A
      keep(a:_cons) nostar ///
      b(%9.0f) se(%9.0f) ///
	  mtitle("Model 1" "Model 2" "Model 3" "Model 4") ///
      varlabels(a:_cons "Base Effect") ///
      nolabel noobs nogaps nonotes eqlabel(none) nolines nonumber replace
esttab A1 A2 A3 A4, ///
      keep(decay:_cons) nostar ///
	  b(%9.2f) se(%9.2f) ///
      varlabels(decay:_cons "Decay Rate") ///
      nolabel noobs nogaps nonotes eqlabel(none) nolines nomtitles nonumber append
esttab B1 B2 B3 B4, /// PANEL B
      keep(cpi_log cpi_yy_logs_lag_12 cpi_yy_logs_lag_24) nostar ///
      b(%9.0f) se(%9.0f) ///
      varlabels(cpi_log "Current CPI (y/y)" cpi_yy_logs_lag_12 "Lagged CPI" /// 
      cpi_yy_logs_lag_24 "Twice Lagged CPI") ///
      nolabel noobs nogaps nonotes eqlabel(none) nolines nomtitles nonumber append ///
      addnote("Controls" "Model 1: Constant" ///
	  "Model 2: Constant; Unemployment Rate (U3); FFR" ///
	  "Model 3: Constant; Unemployment Rate (U3); S&P Quaretrly Retun; PCE" ///
	  "Model 4: Constant; Unemployment Rate (U3); FFR; S&P Quaretrly Retun; PCE")
	  
* Scale CPI by 10 for coefficients to refer to 10% impact
replace cpi_log = cpi_log*10 // to get effect on 10%
replace cpi_yy_logs_lag_12 = cpi_yy_logs_lag_12*10
replace cpi_yy_logs_lag_24 = cpi_yy_logs_lag_24*10

* Construct matrix with coefficient and CI for model 4 (both A and B versions)
matrix define CI = J(6,4, .)

* Year by Year version
regress ics_all cpi_log cpi_yy_logs_lag_12 cpi_yy_logs_lag_24 ///
	unemployment_rate ffr SP500_quarterly_return pce, robust

forvalues i = 1/3 {
    matrix CI[`i',1] = r(table)[1,`i'] // Coefficient
    matrix CI[`i',2] = r(table)[5,`i'] // Lower CI
    matrix CI[`i',3] = r(table)[6,`i'] // Upper CI
    matrix CI[`i',4] = 1
}
	
* Proportional Decay version
nlsur (ics_all= {a}*cpi_log + {a}*(1-{decay})*cpi_yy_logs_lag_12 + {a}*(1-{decay})^2*cpi_yy_logs_lag_24 ///
		+ {b1}*unemployment_rate + {b2}*ffr + {b3}*SP500_quarterly_return + {b4}*pce + {b0}), robust

forvalues i = 1/3 {
    if `i' == 1 {
        nlcom _b[a:_cons], level(95) // alpha
    }
    else if `i' == 2 {
        nlcom (_b[a:_cons] * (1 - _b[decay:_cons])), level(95) // alpha * (1- decay)
    }
    else if `i' == 3 {
        nlcom (_b[a:_cons] * ((1- _b[decay:_cons])^2)), level(95) // alpha * (1- decay)^2
    }

    matrix CI[`i'+3,1] = r(table)[1,1] // Coefficient
    matrix CI[`i'+3,2] = r(table)[5,1] // Lower CI
    matrix CI[`i'+3,3] = r(table)[6,1] // Upper CI
	matrix CI[`i'+3,4] = 2
}

* Save Coefficients and CI's
clear
svmat CI

* Plot Coefficients and CI
gen x_axis = _n - 3 if CI4 == 2 // B-4 appears first from left
replace x_axis = _n + 0.3 if CI4 == 1 // move to side if model A-4
sort x_axis
twoway (bar CI1 x_axis if CI4 == 2, barwidth(0.2) color(navy)) /// B-4
       (bar CI1 x_axis if CI4 == 1, barwidth(0.2) color(maroon)) /// A-4
       (rcap CI2 CI3 x_axis, lcolor(black)), ///
       ytitle("") xtitle("") ///
	   xlabel(1.15 "1st Year" 2.15 "2nd Year" 3.15 "3rd Year", nogrid) ///
	   xscale(range(0.8 3.5)) ///
	   ylabel(, valuelabel glcolor(gs14) glstyle(solid)) ///
	   xscale(alt) title("Impact on sentiment of 10% inflation") ///
	   legend(order(1 "Proportional Decay" 2 "Year by Year") cols(2) position(6))
