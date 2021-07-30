** THIS FILE CONTAINS PROGRAMS CHARACTERIZING CA ADJUSTMENT AFTER THE TRAVEL SHOCK **
capture log close
 
drop _all

set virtual on 
use "\\tsclient\c\Users\GMilesiFerretti\OneDrive - The Brookings Institution\Documents\FOLDERS\tourism\tourism_CA_dataset.dta"

** define changes between 2020 and previous years for CA variables **
replace ca_dif=ca_20-ca_1519
replace bg_dif=bg_20-bg_1519
replace bs_dif=bs_20-bs_1519
replace btv_dif=btv_20-btv_1519
replace btr_dif=btr_20-btr_1519
replace bprin_dif=bprin_20-bprin_1519
replace bsecin_dif=bsecin_20-bsecin_1519
replace bsoth_dif=bs_dif-btv_dif
replace btt_dif=btv_dif+btr_dif


** summarize stylized facts for countries with large travel surpluses ** 

*** regressions ****
local set1 lgdppc lpop cases1000 deaths1000 stringency agr_1419 manuf_1419 tour_tot 
local set2 lgdppc lpop cases deaths stringency tour_dir agr_1419 manuf_1419
local set3 lgdppc lpop cases deaths stringency tour_tot btv_1519 agr_1419 manuf_1419

eststo clear
eststo: reg grow_dif `set1' if sample==0, robust
eststo: reg grow_dif `set1' if sample=0  & ae==1, robust
eststo: reg grow_dif `set1' if sample=0  & ae==0, robust
eststo: reg grow_dif `set2' if sample==0, robust
eststo: reg grow_dif `set3' if sample==0, robust

esttab using growth2.csv, replace r2 label title (growth regressions) ///
mtitles("All" "AE" "EM" "All" "All") starlevels( * 0.10 ** 0.05 *** 0.010)

************************************************************************

cd "\\tsclient\c\Users\GMilesiFerretti\OneDrive - The Brookings Institution\Documents\FOLDERS\tourism"

eststo clear
eststo: reg ca_dif ca_1519 boil_1519 btv_1519 if sample==0, robust
eststo: reg ca_dif ca_1519 boil_1519 btv_1519 if sample==0 & ifs_code~=443, robust
eststo: reg ca_dif ca_1519 boil_1519 btv_1519 if sample==0 & ae==1 & ifs_code~=443, robust
eststo: reg ca_dif ca_1519 boil_1519 btv_1519 if sample==0 & ae==0 & ifs_code~=443, robust
eststo: reg ca_dif ca_1519 boil_1519 btv_1519 if sample==0 & ifs_code~=443 & btv_1519>0, robust
eststo: reg ca_dif ca_1519 boil_1519 btv_1519 if sample==0 & ifs_code~=443 & btv_1519<0, robust


esttab using ca.csv, replace r2 label title (growth regressions) ///
mtitles("All" "All ex KWT" "Adv Ec" "EM ex KUW" "BTV>0" "BTV<0") starlevels( * 0.10 ** 0.05 *** 0.010)


eststo clear
eststo: reg grow_dif cases1000 deaths1000 stringency if sample==0, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot if sample==0, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot lgdppc lpop if sample==0, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot lgdppc lpop agr_1419 manuf_1419 if sample==0, robust

esttab using growth1.csv, replace r2 label title (growth regressions) ///
mtitles("All" "All" "All" "All") starlevels( * 0.10 ** 0.05 *** 0.010)


eststo clear
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot lgdppc lpop if sample==0, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot lgdppc lpop if sample==0  & ae==1, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot lgdppc lpop if sample==0  & ae==0, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot lgdppc lpop agr_1419 manuf_1419 if sample==0, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot lgdppc lpop agr_1419 manuf_1419 if sample==0  & ae==1, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_tot lgdppc lpop agr_1419 manuf_1419 if sample==0  & ae==0, robust
eststo: reg grow_dif cases1000 deaths1000 stringency tour_dir lgdppc lpop agr_1419 manuf_1419 if sample==0, robust
eststo: reg grow_dif cases1000 deaths1000 stringency btv_1519 lgdppc lpop agr_1419 manuf_1419 if sample==0, robust

esttab using growth2.csv, replace r2 label title (growth regressions) ///
mtitles("All" "AE" "EM" "All" "AE" "EM" "All" "All") starlevels( * 0.10 ** 0.05 *** 0.010)
