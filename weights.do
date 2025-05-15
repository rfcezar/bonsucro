import excel "bonsucro_data.xlsx", firstrow clear

psmatch2 RED_TREATED (GDP), kernel logit 

export excel using "bonsucro_data.xlsx", firstrow(variables) replace
