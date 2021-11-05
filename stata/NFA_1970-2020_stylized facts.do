*** THIS FILE CONSTRUCTS DATA AND STATS FOR THE NEW EXTERNAL WEALTH DATASET (February 2019) ****
use "/Users/malcalakovalski/Documents/Projects/tourism/data/nfa.dta"
cd "/Users/malcalakovalski/Documents/Projects/tourism"
capture log close
 
* drop _all

set virtual on   

set more 1

set matsize 1600

sort ifs_code date

# delimit; 

**** DROP BELUX, Liechtenstein, Far Oer, ECCU **;
drop if ifs_code==126;
drop if ifs_code==147; 
drop if ifs_code==309 | ifs_code==816;


* DEFINE COUNTRY GROUP DUMMIES * ;

* 1) DUMMY FOR SMALL OFFSHORE CENTERS * ;
g off=0;
replace off=1 if ifs_code==113 | ifs_code==117 | ifs_code==118 | ifs_code==135 | ifs_code==171 | ifs_code==312 | ifs_code==313 | ifs_code==316 | ifs_code==319 | ifs_code==353 | ifs_code==354 | ifs_code==377 | ifs_code==379 | ifs_code==381  | ifs_code==718 | ifs_code==823;

******* DUMMY FOR FINANCIAL CENTERS NOT INCLUDED IN PAPER ON OFFSHORE CENTERS ****;
g off_lg=0;
replace off_lg=1 if ifs_code==112 | ifs_code==124 | ifs_code==137 | ifs_code==138 | ifs_code==146 | ifs_code==178 | ifs_code==181 | ifs_code==283 | ifs_code==419 | ifs_code==423 | ifs_code==532 | ifs_code==546 | ifs_code==576 | ifs_code==684;

*** DEFINE ADVANCED ECONOMIES EXCLUDING SMALL OFFSHORE CENTERS *****;
** note: the AE definition is the IMF one and thus includes Malta, Cyprus, Czech and Slovak republic, Slovenia, the Baltics, Israel, HK, Korea, Macao, SGP, Taiwan **;

g ind=1;
replace ind=0 if ifs_code>196 | ifs_code==186;
replace ind=0 if off==1; 
replace ind=0 if ifs_code==163;
replace ind=1 if ifs_code==423 | ifs_code==436 | ifs_code==528 | ifs_code==532 | ifs_code==546 | ifs_code==542 | ifs_code==576 | ifs_code==935 | ifs_code==936 | ifs_code==939 | ifs_code==941 | ifs_code==946 | ifs_code==961;

******** DEFINE AEs excluding FINANCIAL CENTERS **** ;
g ind_nfc=ind;
replace ind_nfc=0 if off_lg==1; 

******** DEFINE EMDEs excluding FINANCIAL AND OFFSHORE CENTERS ****; 

*** note: larger offshore centers such as Bahrain and Panama are thus excluded from EMDEs ***;

g dev=1;
replace dev=0 if ind==1 | off==1 | ifs_code==163 | off_lg==1;
g dev_c=dev;
replace dev_c=0 if ifs_code==924;

g euro=0;
replace euro=1 if ifs_code==122 | ifs_code==124 | ifs_code==132 | ifs_code==134 | ifs_code==136 | ifs_code==137 | ifs_code==138 | ifs_code==172 | ifs_code==174 | ifs_code==178 | ifs_code==181 | ifs_code==182 | ifs_code==184 | ifs_code==423 | ifs_code==936 | ifs_code==961 | ifs_code==939 | ifs_code==941 | ifs_code==946;

g ind_euro=ind;
replace ind_euro=1 if ifs_code==163;
replace ind_euro=0 if euro==1;

g othadv=0;

replace othadv=1 if (ifs_code==156 | ifs_code==158 | ifs_code==193 | ifs_code==196);

g otheur=0;
replace otheur=1 if (ifs_code==112 | ifs_code==128 | ifs_code==142 | ifs_code==144 | ifs_code==146 | ifs_code==176);

g asia=1;
replace asia=0 if (ifs_code<500 | (ifs_code>600&ifs_code<800) | ifs_code>900) & ifs_code~=924;
replace asia=0 if off==1;
replace asia=0 if ind==1;

g pacific=0;
replace pacific=1 if ifs_code>800 & ifs_code<900;
replace pacific=1 if ifs_code==565;
replace pacific=0 if ifs_code==823;
replace pacific=0 if ifs_code==853;
g asia1=asia-pacific;


g em_asia=0;
replace em_asia=1 if (ifs_code==528 | ifs_code==532 | ifs_code==536 | ifs_code==542 | ifs_code==548 | ifs_code==566 | ifs_code==576 | ifs_code==578 | ifs_code==924);

g em_asia1=em_asia;

replace em_asia1=0 if ifs_code==924;
replace em_asia=0 if off==1;

g weshem=1;
replace weshem=0 if ifs_code<200 | ifs_code>399;
replace weshem=0 if off==1 | off_lg==1;

g weshem2=weshem;

replace weshem2=0 if (ifs_code==248 | ifs_code==299 | ifs_code==369);

g latam=weshem;

replace latam=0 if ifs_code>300;

g latam2=latam;

replace latam2=0 if ifs_code==263 | ifs_code==278 | ifs_code==268;

g carib=1;
replace carib=0 if ifs_code<300 | ifs_code>399;
replace carib=0 if off==1;

g mideast=0;
replace mideast=1 if ifs_code>400 & ifs_code<500;
replace mideast=1 if ifs_code==672 | ifs_code==612 | ifs_code==686 | ifs_code==744;
replace mideast=0 if ifs_code==423;
replace mideast=0 if off==1 | off_lg==1;

g mideast1=mideast;
replace mideast1=0 if ifs_code==436;

g oil=mideast1;
replace oil=0 if ifs_code==439 | ifs_code==446 | ifs_code==463 | ifs_code==469 | ifs_code==487 | ifs_code==686 | ifs_code==744;
replace oil=1 if ifs_code==248 | ifs_code==299 | ifs_code==369 | ifs_code==516 | ifs_code==537 | ifs_code==614 | ifs_code==634 | ifs_code==642 
| ifs_code==646 | ifs_code==694 | ifs_code==628 | ifs_code==912 | ifs_code==916 | ifs_code==922 | ifs_code==925;

g afr=0;
replace afr=1 if ifs_code==199 | (ifs_code>600 & ifs_code<800);
replace afr=0  if ifs_code==672 | ifs_code==612 | ifs_code==686 | ifs_code==744;
replace afr=0 if off==1 | off_lg==1;

g trans=0;
replace trans=1 if ifs_code>900 & ifs_code~=924;

g em_eur_2=0;
replace em_eur_2=1  if ifs_code>934 & ifs_code~=961 & ifs_code~=936;
replace em_eur_2=1 if ifs_code==186 | ifs_code==914 | ifs_code==918 | ifs_code==926;
replace em_eur_2=0 if ifs_code==948;

g em_eur=em_eur_2;
replace em_eur=1 if ifs_code==922 | ifs_code==913 | ifs_code==921;
replace em_eur=0 if ind==1;


g fsu=0;
replace fsu=1 if trans==1 & em_eur==0;
replace fsu=0 if ind==1;

replace mideast1=1 if fsu==1;

g europe=1 if ifs_code<185;
replace europe=0 if ifs_code==111 | ifs_code==156 | ifs_code==158;
replace europe=1 if em_eur==1;
replace europe=0 if ifs_code==962 | ifs_code==963 | ifs_code==914 | ifs_code==965;

g dev_small=0;
replace dev_small=1 if ifs_code==186 | ifs_code==199 | ifs_code==213 | ifs_code==223 | ifs_code==228 | ifs_code==233 | ifs_code==273 | ifs_code==299 | ifs_code==534 | ifs_code==536 | ifs_code==542 | ifs_code==548 | ifs_code==564 | ifs_code==566 | ifs_code==578 | ifs_code==924;

g em_eur1=em_eur;
replace em_eur1=0 if oil==1;

g eur_def=em_eur1;
replace eur_def=1 if ifs_code==112 | ifs_code==136 | ifs_code==174 | ifs_code==176 | ifs_code==178 | ifs_code==182 | ifs_code==184 | ifs_code==936 | ifs_code==961;

gen eur_sur=0;
replace eur_sur=1 if ifs_code==122 | ifs_code==124 | ifs_code==128 | ifs_code==134 | ifs_code==137 | ifs_code==138 | ifs_code==144 | ifs_code==146 | ifs_code==172;


*********CREATE LIDC GROUP ***************;
g lidc=0;
replace lidc=1 if ifs_code==218 | ifs_code==263 | ifs_code==268 | ifs_code==278 | ifs_code==474 | ifs_code==512 | ifs_code==513 | ifs_code==514 | ifs_code==518 | ifs_code==522 | ifs_code==544 | ifs_code==558 | ifs_code==582;
replace lidc=1 if ifs_code==813 | ifs_code==826 | ifs_code==853 | ifs_code==917 | ifs_code==921 | ifs_code==923 | ifs_code==927 | ifs_code==948;
replace lidc=1 if ifs_code>600 & ifs_code<800;
replace lidc=0 if ifs_code==612 | ifs_code==614 | ifs_code==616 | ifs_code==624 | ifs_code==642 | ifs_code==646 | ifs_code==672 | ifs_code==684 | ifs_code==686 | ifs_code==718 | ifs_code==726 | ifs_code==728 | ifs_code==728 | ifs_code==734 | ifs_code==744;

g emg=dev-lidc;


************ GENERATE ALL FINANCIAL CENTER GROUP *****;
g fc=0;
replace fc=1 if off==1 | off_lg==1;



*** SET K-INFLOWS TO 0 IF DATA UNAVAILABLE (FOR WORLD SUMS) ***
replace fdil=0 if fdil==.;
replace ol=0 if ol==.;
replace pdl=0 if pdl==.;
replace eql=0 if eql==.;
replace fdia=0 if fdia==.;
replace oa=0 if oa==.;
replace pda=0 if pda==.;
replace eqa=0 if eqa==.;
replace fx=0 if fx==.;
replace fdia6=0 if fdia6==.;
replace fdil6=0 if fdil6==.;
replace pa=0 if pa==.;
replace pl=0 if pl==.;

*** make flows consistent with BOP6 sign ***;
replace fdia=-fdia;
replace fdia6=-fdia6;
replace eqa=-eqa;
replace fx=-fx;
replace dera=-dera;

*** replace FDI6 for FDI5 when latter unavailable ***;
replace fdia=fdia6 if (fdia==0 | fdia==.) & fdia6~=0;
replace fdil=fdil6 if (fdil==0 | fdil==.) & fdil6~=0;

** replace FDI5 for FDI6 when latter unavailable ***;
replace fdia6=fdia if (fdia6==0 | fdia6==.) & fdia~=0;
replace fdil6=fdil if (fdil6==0 | fdil6==.) & fdil~=0;

*** set derivatives=0 if unavailable *** ;
replace dera=0 if dera==.;
replace derl=0 if derl==.;
g der_net=dera-derl;

*** generate total K-inflows and outflows *** ; 
g k_in=fdil6+pl+ol-der_net;
g k_out=(fdia6+pa+oa+fx);

replace ipol=ipdebtl-ippdl;

replace gdpd=gdpd[_n-1] if gdpd==. & date==2018;
replace gdpd=gdpd[_n-1] if gdpd==. & date==2019;

** DEFINE RATIOS TO GDP ***;
g cay=ca/gdpd*100;
g ca_abs=abs(ca);
g fa=ca+eo;

g ipfay=ipfa/gdpd*100*e_eop/e_avg;
g ipfly=ipfl/gdpd*100*e_eop/e_avg;
g ipeqay=ipeqa/gdpd*100*e_eop/e_avg;
g ipeqly=ipeql/gdpd*100*e_eop/e_avg;
g ipfdiay=ipfdia/gdpd*100*e_eop/e_avg;
g ipfdily=ipfdil/gdpd*100*e_eop/e_avg;
g ipdebtay=ipdebta/gdpd*100*e_eop/e_avg;
g ipdebtly=ipdebtl/gdpd*100*e_eop/e_avg;
g ipfxy=ipfx/gdpd*100*e_eop/e_avg;
g ipderay=ipdera/gdpd*100*e_eop/e_avg;
g ipderly=ipderl/gdpd*100*e_eop/e_avg;
g ipnfay=ipnfa/gdpd*100*e_eop/e_avg;
g ipteqay=(ipeqay+ipfdiay)*e_eop/e_avg;
g ipteqly=(ipeqly+ipfdily)*e_eop/e_avg;
g neqy=(ipteqay-ipteqly)*e_eop/e_avg;
g ndebty=(ipnfay-neqy)*e_eop/e_avg;
g iipy=iip/gdpd*100*e_eop/e_avg;

g ippday=ippda/gdpd*100*e_eop/e_avg;
g ippdly=ippdl/gdpd*100*e_eop/e_avg;
g ipoay=ipoa/gdpd*100*e_eop/e_avg;
g ipoly=ipol/gdpd*100*e_eop/e_avg;

g nfay=ipnfa/gdpd;
g ipgfay=ipfay+ipfly;
g ipteqy=ipteqay+ipteqly;
g nfa_abs=abs(ipnfa);
g nfay_abs=nfa_abs/gdpd;
g gfa=(ipfa+ipfl)/1000;
g ca_absy=ca_abs/gdpd;
g ipporteqy=ipeqay-ipeqly;

label var ipporteqy "net portfolio equity position (pct of GDP)";

replace ipfay=ipfa/gdpd*100 if (e_eop==. | e_avg==.);
replace ipfly=ipfl/gdpd*100 if (e_eop==. | e_avg==.);
replace ipeqay=ipeqa/gdpd*100 if (e_eop==. | e_avg==.);
replace ipeqly=ipeql/gdpd*100 if (e_eop==. | e_avg==.);
replace ipfdiay=ipfdia/gdpd*100 if (e_eop==. | e_avg==.);
replace ipfdily=ipfdil/gdpd*100 if (e_eop==. | e_avg==.);
replace ipdebtay=ipdebta/gdpd*100 if (e_eop==. | e_avg==.);
replace ipdebtly=ipdebtl/gdpd*100 if (e_eop==. | e_avg==.);
replace ipfxy=ipfx/gdpd*100 if (e_eop==. | e_avg==.);
replace ipderay=ipdera/gdpd*100 if (e_eop==. | e_avg==.);
replace ipderly=ipderl/gdpd*100 if (e_eop==. | e_avg==.);
replace ipnfay=ipnfa/gdpd*100 if (e_eop==. | e_avg==.);
replace ipteqay=ipeqay+ipfdiay if (e_eop==. | e_avg==.);
replace ipteqly=ipeqly+ipfdily if (e_eop==. | e_avg==.);
replace neqy=ipteqay-ipteqly if (e_eop==. | e_avg==.);
replace ndebty=ipnfay-neqy if (e_eop==. | e_avg==.);
replace iipy=iip/gdpd*100 if (e_eop==. | e_avg==.);

replace ippday=ippda/gdpd*100 if (e_eop==. | e_avg==.);
replace ippdly=ippdl/gdpd*100 if(e_eop==. | e_avg==.);
replace ipoay=ipoa/gdpd*100 if (e_eop==. | e_avg==.);
replace ipoly=ipol/gdpd*100 if (e_eop==. | e_avg==.);

replace ipgfay=ipfay+ipfly;
replace ipteqy=ipteqay+ipteqly;



***** AVERAGES OF TOURISM-RELATED VARIABLES ******* ;
egen btv_1519=mean(btvy) if date>2014 & date<2020,by(ifs_code);
label var btv_1519 "Net intl travel balance, 2015-19 avg (pct of GDP)";
egen tour_dir_1519=mean(tour_dir) if date>2014 & date<2020,by(ifs_code);
label var tour_dir_1519 "direct share of tourism in GDP, 2015-19 avg";
egen tour_tot_1519=mean(tour_tot) if date>2014 & date<2020,by(ifs_code);
label var tour_tot_1519 "total share of tourism in GDP, 2015-19 avg";

**** generate 1-year lead in the change of IIP to GDP *** ;

g dnfay_l=dnfay[_n+1] if ifs_code[_n]==ifs_code[_n+1];
label var dnfay_l "Change in IIP net of gold to GDP, 2019-20";
** Note: label is correct for the 2019 observation only (capturing the change 2019-20)***;

*** Now the change of the IIP to GDP ratio for 2020 can be compared with the 3 variables summarizing tourism dependence in 2019 ;

scatter dnfay_l tour_tot_1519 if tour_tot_1519>16 & tour_tot!=. & date==2019, mlabel(ccode);
 * save graph tour_tot, replace;
scatter dnfay_l tour_dir_1519  if tour_dir_1519>6 & tour_dir!=. & date==2019, mlabel(ccode);
* save graph tour_dir, replace;
scatter dnfay_l btv_1519 if btv_1519>5 & btv_1519!=. & date==2019, mlabel(ccode);
* save graph btv, replace;

label var neqy "Net equity position (FDI + portfolio) in pct of GDP";
scatter dnfay_l neqy if neqy>20 & fc==0 & date==2019, mlabel(ccode);
* save graph neqy_cr, replace;
scatter dnfay_l neqy if neqy<-20 & fc==0 & date==2019, mlabel(ccode);
* save graph neqy_deb, replace;

