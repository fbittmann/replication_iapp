

*Felix Bittmann, 2024
*felix.bittmann@lifbi.de

clear all
version 16.1
cap cd ""
set scheme plotplain        //requires Blindschemes from SSC




*** Required Ados ***
ssc install fre, replace
ssc install estout, replace
ssc install blindschemes, replace
net install plnepsmiss, from("https://raw.github.com/fbittmann/plnepsmiss/stable") replace

"https://www.stata.com/users/ymarchenko/mibeta/mibeta.ado"		//MIBETA, install manually



*** Folder Structure ***
/*All NEPS Data files must be in a subfolder "Data" relative to the working path
*/
