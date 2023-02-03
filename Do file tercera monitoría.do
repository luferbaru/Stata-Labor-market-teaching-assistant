**************************
***  Clase Práctica 2  ***
***  Economía Laboral  ***
************************** 

	clear all
	cap log close
	cd "C:\Users\sme02\Dropbox\GEIH 2014 (1)"
	log using "Clase 2.smcl", replace
	use "II trim 2014.dta", clear
	set more off
	
	
******************************
*1.SALARIO MENSUAL Y POR HORA*
******************************

des P6500 P6430 P6750 P6760 

*A. SALARIO MENSUAL*		
	gen wage=0
		
	/* /!\ NOTA: Corregir el salario si la persona es empleada o independiente. Así mismo,  
				 corregir por la frecuencia en la que el individuo percibe su ingreso */
				 
	*Persona empleada
		tab P6430
		replace wage=P6500 if (P6430==1|P6430==2|P6430==3|P6430==8)
				
	*Frecuencia del ingreso*
		tab P6760
		replace wage=P6750/P6760 if(P6430==4|P6430==5|P6430==9 & P6760!=. & P6760<=12)
			
*B. SALARIO POR HORA TRABAJADA*
	des P6800
	gen wh = wage/(P6800*(4.285714))
	
	*Logaritmo natural del salario*	
		gen lwh = ln(wh)
	
****************************
*2.DEFINICIÓN DE LA MUESTRA*
****************************

des P6240 P6430 P6800 P6040

	/* /!\ NOTA: La muestra estará definida por los trabajadores asalariados de entidades 
				 privadas, que trabajen de 20 a 85 horas semanales, entre 15 y 60 años. */

	*Muestra*
		gen sample=0
		replace sample=1 if P6240==1 & P6430==1 & (P6800>19 & P6800<86) & (P6040>=15 & P6040<=60)
	
************
*3.OUTLIERS*
************

	/* /!\ NOTA: Crear una variable que identifique los percentiles de la distribución de 
				 ingresos y eliminar los datos atípicos de ambas colas (percentiles 1 y 100). */

	*Percentiles*
		xtile percentil=wage if sample==1, nq(100)
		
	*Eliminar outliers*
		replace wh=. if percentil==1|percentil==100

*************
*4.OCUPACIÓN*
*************

	des OFICIO

	/* /!\ NOTA: Generar una variable que clasifique la ocupación del trabajo del individuo
	de acuerdo a la Clasificación Internacional Uniforme de Ocupaciones (CIUO)*/
				 
	gen ocupac=.
		
	*1. Reemplazar la variable segpun la codificación de las ocupaciones
		#delimit ;
		replace ocupac=0 if OFICIO=="00";
		replace ocupac=1 if OFICIO=="1"|OFICIO=="2"|OFICIO=="3"|OFICIO=="4"|OFICIO=="5"|OFICIO=="6"|OFICIO=="7"| OFICIO=="8"|OFICIO=="9"|OFICIO=="11"|OFICIO=="12";
		replace ocupac=2 if OFICIO=="13"|OFICIO=="14"|OFICIO=="15"|OFICIO=="16"|OFICIO=="17"|OFICIO=="18"|OFICIO=="19";
		replace ocupac=3 if OFICIO=="20"|OFICIO=="21";
		replace ocupac=4 if OFICIO=="30"|OFICIO=="31"|OFICIO=="32"|OFICIO=="33"|OFICIO=="34"|OFICIO=="39"|OFICIO=="35"|OFICIO=="36"|OFICIO=="37"|OFICIO=="38";
		replace ocupac=5 if OFICIO=="40"|OFICIO=="41"|OFICIO=="42"|OFICIO=="43"|OFICIO=="44"|OFICIO=="45"|OFICIO=="49";
		replace ocupac=6 if OFICIO=="50"|OFICIO=="51"|OFICIO=="52"|OFICIO=="53"|OFICIO=="54"|OFICIO=="55"|OFICIO=="56"|OFICIO=="57"|OFICIO=="58"|OFICIO=="59";
		replace ocupac=7 if OFICIO=="60"|OFICIO=="61"|OFICIO=="62"|OFICIO=="63"|OFICIO=="64";
		replace ocupac=8 if OFICIO=="70"|OFICIO=="71"|OFICIO=="72"|OFICIO=="73"|OFICIO=="74"|OFICIO=="76"|OFICIO=="75"|OFICIO=="77"|OFICIO=="78"|OFICIO=="79"|OFICIO=="80"|OFICIO=="81"|OFICIO=="82"|OFICIO=="83"|OFICIO=="84"|OFICIO=="85"|OFICIO=="86"|OFICIO=="87"|OFICIO=="88"|OFICIO=="89"|OFICIO=="90"|OFICIO=="91"|OFICIO=="92"|OFICIO=="93"|OFICIO=="94"|OFICIO=="95"|OFICIO=="96"|OFICIO=="97"|OFICIO=="98"|OFICIO=="99";	
							
	*2. Asignar labels de las ocupaciones;	
		label define ocupaclbl 0"INCODIFICABLE"1"PROFESIONALES Y TEC. 1"2"PROFESIONALES Y TEC. 2"3"DIRECTIVOS Y FUNCIONARIOS"4"PERSONAL ADMIN."5"COMERCIANTE O VENDEDOR"6"TRABAJADOR DE SERVICIOS"7"TRABAJADOR AGRICOLA Y FORESTALES"8"OPERARIO, NO AGRI."
		label values ocupac ocupaclbl		
		#delimit cr
		
********************
*5.SEGURIDAD SOCIAL*
********************

des P6920 P6090 P6110 P6870
	
	/* /!\ NOTA: Crear una variable que identifique los percentiles de la 
				 distribución de ingresos y eliminar los datos atípicos. */

*A. Cotiza en fondo de pensiones			
	gen informal_pension=.
		replace informal_pension=1 if P6920==2
		replace informal_pension=0 if P6920==1

*B. Beneficiario de salud
	gen informal_health = .
		replace informal_health=1 if P6090==2
		replace informal_health=0 if P6090==1
		*Corregir si la persona no paga pero es beneficiaria
		replace informal_health=1 if P6110==5
		*Corregir si la persona no sabe
		replace informal_health=. if P6110==9
		*Corregir si la persona esta afiliada al régimen subsidiado
		replace informal_health=1 if P6100==3

*C. Tamaño del lugar de trabajo
	gen informal_size=.
		replace informal_size=1 if P6870<4
		replace informal_size=0 if P6870>=4

*D. Variables de formalidad e informalidad	
	gen informal=1
		replace informal=0 if informal_pension==0 & informal_health==0
	gen formal=1
		replace formal=0 if informal==1

***********
*6.EJEMPLO*
***********

	/* /!\ NOTA: Al estimar una regresión con una variable categórica como explicativa, puede ser necesario
			     escoger la categoría base. Esto se puede hacer con los comandos "char" o "fvset". */
			 
	char ocupac[omit] 5
		xi: reg lwh P6040 i.ocupac

	foreach i of numlist 1/8{
		char ocupac[omit] `i'
		eststo ols`i': xi: reg lwh P6040 i.ocupac
		}
			
		
		esttab ols1 ols2 ols3 ols4, cells(b(star fmt(%9.4f)) se(par)) stats(F p) margin legend

log close

	
