* import xlsx;
proc import file = "/home/u63542639/cdc2023/HealthSciences_Dataset.xlsx"
	out = work.healthdata
	dbms = xlsx
	replace;
	sheet = data;
run;

* gives stats? lol?;
proc univariate data = work.healthdata;
	hist time_in_hospital / endpoints = 0 to 100 by 10;
run;

* plots things by gender;
proc sgpanel data = work.healthdata;
	panelby gender;
	vbox time_in_hospital;
run;

proc sgpanel data = work.healthdata;
	panelby gender;
	vbox num_lab_procedures;
run;

proc sgpanel data = work.healthdata;
	panelby gender;
	vbox num_procedures;
run;

proc sgpanel data = work.healthdata;
	panelby gender;
	vbox num_medications;
run;

proc sgpanel data = work.healthdata;
	panelby gender;
	vbox number_outpatient;
run;

proc sgpanel data = work.healthdata;
	panelby gender;
	vbox number_emergency;
run;

proc sgpanel data = work.healthdata;
	panelby gender;
	vbox number_inpatient;
run;

proc sgpanel data = work.healthdata;
	panelby gender;
	vbox number_diagnoses;
run;

* sort data by gender;
proc sort data = work.healthdata out = work.sorteddata;
	by gender;
run;

proc means data = work.sorteddata;
	by gender;
	var number_diagnoses;
run;

* sort data by race, run means, plot num diag by race;
proc sort data = work.healthdata out = work.sortedracedata;
	by race;
run;

proc means data = work.sortedracedata;
	by race;
	var number_diagnoses;
run;

proc sgpanel data = work.sortedracedata;
	panelby race;
	vbox number_diagnoses;
run;

* proc freq for age and race;
proc freq data = work.healthdata;
	tables age;
run;

proc freq data = work.healthdata;
	tables race;
run;

* ods graphics on;
* admission type, sort, format, means and plot against certain vars;
proc sort data = work.healthdata out = work.sortedadmittype;
	by admission_type_id;
run;
	
proc format;
	value admission_type_idf 	  
	1 = "Emergency"
	2 = "Urgent"
	3 = "Elective"
	4 = "Newborn"
	5 = "Not Available"
	6 = "NULL"
	7 = "Trauma Center"
	8 = "Not Mapped";
run;
	
/*	
	value discharge_disposition_idf
	1 = "Discharged to home"
	2 = "Discharged/transferred to another short term hospital"
	3 = "Discharged/transferred to SNF"
	4 = "Discharged/transferred to ICF"
	5 = "Discharged/transferred to another type of inpatient care institution"
	6 = "Discharged/transferred to home with home health service"
	7 = "Left AMA"
	8 = "Discharged/transferred to home under care of Home IV provider"
	9 = "Admitted as an inpatient to this hospital"
	10 = "Neonate discharged to another hospital for neonatal aftercare"
	11 = "Expired"
	12 = "Still patient or expected to return for outpatient services"
	13 = "Hospice / home"
	14 = "Hospice / medical facility"
	15 = "Discharged/transferred within this institution to Medicare approved swing bed"
	16	Discharged/transferred/referred another institution for outpatient services
	17	Discharged/transferred/referred to this institution for outpatient services
	18	NULL
	19	Expired at home. Medicaid only, hospice.
	20	Expired in a medical facility. Medicaid only, hospice.
	21	Expired, place unknown. Medicaid only, hospice.
	22	Discharged/transferred to another rehab fac including rehab units of a hospital .
	23	Discharged/transferred to a long term care hospital.
	24	Discharged/transferred to a nursing facility certified under Medicaid but not certified under Medicare.
	25	Not Mapped
	26	Unknown/Invalid
	30	Discharged/transferred to another Type of Health Care Institution not Defined Elsewhere
	27	Discharged/transferred to a federal health care facility.
	28	Discharged/transferred/referred to a psychiatric hospital of psychiatric distinct part unit of a hospital
	29	Discharged/transferred to a Critical Access Hospital (CAH).;

run;
*/



proc means data = work.sortedadmittype;
	by admission_type_id;
	var time_in_hospital num_lab_procedures num_procedures num_medications number_diagnoses;
	format admission_type_id admission_type_idf.;
run;

proc sgplot data = work.sortedadmittype;
	vbox time_in_hospital / category = admission_type_id;
	format admission_type_id admission_type_idf.;
run;

proc sgplot data = work.sortedadmittype;
	vbox num_lab_procedures / category = admission_type_id;;
	format admission_type_id admission_type_idf.;
run;

proc sgplot data = work.sortedadmittype;
	vbox num_procedures / category = admission_type_id;;
	format admission_type_id admission_type_idf.;
run;

proc sgplot data = work.sortedadmittype;
	vbox num_medications / category = admission_type_id;;
	format admission_type_id admission_type_idf.;
run;

proc sgplot data = work.sortedadmittype;
	vbox number_diagnoses/ category = admission_type_id;;
	format admission_type_id admission_type_idf.;
run;

* corr matrix;
proc corr data = work.healthdata;
run;

* correlation? via boxplot;
proc sgplot data = work.healthdata;
	vbox time_in_hospital / category = number_diagnoses;
run;

* freq cross diag and time;
proc freq data = work.healthdata;
	tables number_diagnoses * time_in_hospital;
run;

proc freq data = work.healthdata;
	tables number_diagnoses * num_lab_procedures;
run;

proc freq data = work.healthdata;
	tables number_diagnoses * num_procedures;
run;

proc freq data = work.healthdata;
	tables number_diagnoses * num_medications;
run;

proc freq data = work.healthdata;
	tables num_medications;
run;

* fail;
proc corr data = work.healthdata plots=scatter(nvar=all);
run;
