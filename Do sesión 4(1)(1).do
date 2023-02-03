**************************
***  Clase Práctica 3  ***
***  Economía Laboral  ***
************************** 

/***************CONTENT*********************************
1. CREACIÓN Y LIMPIEZA DE VARIABLES
	- Variables a nivel individual
	- Variables a nivel hogares
2. DESEMPLEO
	- Estadísticas descriptivas
	- Definición de la muestra
	- Estimación modelo de desempleo
	- Corrección de autoselección de Heckman	
3. DESCOMPOSICIÓN DE OAXACA
	- Outliers
	- Estimación de Oaxaca
********************************************************/

	clear all
	set more off
		
************************************
*1.CREACIÓN Y LIMPIEZA DE VARIABLES*
************************************

*1A. VARIABLES A NIVEL INDIVIDUAL
********

	*VARIABLE: GENERO 
		*Crear una variable dicótoma para las mujeres de la base		
			gen genero=P6020
			tab genero
			recode genero (1=0)(2=1), gen(mujer)
			label var mujer "mujer==1"
			
	*VARIABLE: EDAD
		*Crear una variable de edad y ver su distribución	
			gen edad=P6040
			sum edad
			label var edad "edad de la persona"
			
	*VARIABLE: ESTADO CIVIL
		*Crear una dicótoma para las personas que esten casadas o en unión libre de la base		
			tab P6070
			gen casado=.
			replace casado = 1 if P6070==1 | P6070==2| P6070==3
			replace casado = 0 if P6070==4 | P6070==5| P6070==6
			label var casado "casado o en unión libre==1" 
			
	*VARIABLE: POBLACION EN EDAD DE TRABAJAR
		*Crear una dicótoma para las personas en edad de trabajar
			tab P6240
			gen pet=(P6240==1|P6240==2|P6240==3|P6240==4|P6240==5|P6240==6)
			label variable pet "Poblacion en edad de trabajar==1"
			
	*VARIABLE: EMPLEADOS
		*Crear una dicótoma para las personas empleadas de la muestra
			gen e =  pet
			replace e = 0 if P6240==2|P6240==3|P6240==4|P6240==5|P6240==6
			replace e = 1 if P6250==1
			replace e = 1 if P6260==1
			replace e = 1 if P6270==1 
			replace e = . if pet==0
			label variable e "empleados==1"
			
	*VARIABLE: DESEMPLEADOS
		*Crear una dicótoma para las personas desempleadas de la muestra
			gen u = (P7250>0 & P7250!=.)
			replace u = . if pet==0
			label variable u "desempleados==1"
			
	*VARIABLE: POBLACIÓN ECONÓMICAMENTE ACTIVA
		*Crear una dicótoma para las personas que pertenecen a la Población Economicamente Activa (PEA)
			gen pea = (e==1|u==1)
			replace pea = . if pet==0
			label var pea "Población Economicamente activa==1"	
			
	*VARIABLE: AÑOS DE EDUCACIÓN
		gen yedu=.			
		*Igual a cero si es no tiene estudios o preescolar
			replace yedu=0 if P6210==1| P6210==2
		*Igual a los grados aprobados para los que tienen básica primaria, básica secundaria o secundaria
			replace yedu=P6210S1 if P6210==3|P6210==4|P6210==5
		*Igual a cinco si están cursando el primer año de básica secundaria pero ya completaron primaria
			replace yedu=5 if P6210S1==0 & P6210==4
		*Se homogeniza los que tienen 12 y 13 a 11 años
			replace yedu=11 if yedu==12 | yedu==13
			replace yedu=11+P6210S1 if P6210==6
			label var yedu "Años de educacion"
			
	*VARIABLE: SALARIO MENSUAL
		gen wage=0
		*Si la persona es empleada
			tab P6430
			replace wage=P6500 if (P6430==1|P6430==2|P6430==3|P6430==8)
		*Frecuencia del ingreso para personas no empleadas
			tab P6760
			replace wage=P6750/P6760 if(P6430==4|P6430==5|P6430==9 & P6760!=. & P6760<=12)
			
	*VARIABLE: SALARIO POR HORA
		des P6800
		gen wh = wage/(P6800*(4.285714))
		
	*VARIABLE: LOGARITMO DEL SALARIO	
		gen lwh = ln(wh)
		
	*VARIABLE: ANTIGUEDAD LABORAL
		des P6426
		gen ant=P6426
		
	*VAIRABLE: EXPERIENCIA LABORAL
		gen exp = edad-5-yedu
		gen exp2 = exp*exp
		
	*VARIABLE: JEFE DE HOGAR
		*Crear una dicótoma para las personas que son jefe de hogar
			gen jhogar = 0
			replace jhogar = 1 if P6050==1
				
*1B. VARIABLES A NIVEL HOGARES
********

	*VARIABLE: IDENTIFICADOR DEL HOGAR
		*Crear una variable que identifique el hogar dentro de la vivienda
			egen idh = concat(DIRECTORIO HOGAR)  
			order idh, after (HOGAR)
		*Conviene algunas veces organizar la base por la variable (en este caso el hogar)
			sort idh
			
	*VARIABLE: NIÑOS MENORES DE 7 AÑOS
		gen nmenor=1 if edad<7
		*Si se quiere contar los niños menores a 7 años que tienen una relacion con el jefe de hogar y que viven en el hogar
			bys idh: egen nmenorhog=total(nmenor) if P6050<6
		*Si solo se esta interesado en la presencia o no de niños menores a 7 años
			bys idh: egen pmenorhog=max(edad<7) if P6050<6
			list idh DIRECTORIO HOGAR edad nmenor nmenorhog pmenorhog P6050 mujer if nmenorhog>1 in 1800/2000
			
	*VARIABLE: NIÑOS MAYORES DE 7 AÑOS ESTUDIANDO
		gen nmayor=1 if edad>6 & edad<18 & P6170==1
		*Si se quiere contar los niños mayores de 7 años y estudiando, que tienen una relación con el jefe de hogar y que viven en el hogar
			by idh: egen nmayorhog=total(nmayor) if P6050<6
		*Si solo se está interesado en la presencia o no de niños mayores de 7 años y estudiando
			bys idh: egen pmayorhog=max(nmayor==1) if P6050<6
			
	*VARIABLE: PRESENCIA DE SERVICIO DOMESTICO
		bys DIRECTORIO: egen domserv=max(P6050==6)
		
	*VARIABLE: ÁREA METROPOLITANA
		tab AREA
		encode AREA, gen(area)
		*Crear label para los valores del área metropolitana
		label define area_valabel 5"MEDELLIN" 8"BARRANQUILLA" 11"SANTAFE DE BOGOTA" 13"CARTAGENA" 17"MANIZALES" 23"MONTERIA" 50"VILLAVICENCIO" 52"PASTO" 54"CUCUTA" 66"PEREIRA" 68"BUCARAMANGA" 73"IBAGUE" 76"CALI"
		label values area area_valabel
		label var area "Ãrea metropolitana"
		tab area
	
*************
*2.DESEMPLEO*
*************

*2A. ESTADÍSTICAS DESCRIPTIVAS
********

	*Tasa de desempleo global
		tabstat u if pea==1 [fweight=int(factor_expansion)], by(area)
		
	*Tasa de desempleo por género
		tabstat u if pea==1 & mujer==1 [fweight=int(factor_expansion)], by(area) 
		tabstat u if pea==1 & mujer==0 [fweight=int(factor_expansion)], by(area) 
		
	*Tasa de desempleo por nivel educativo
		*Basica Secundaria o menos*
			tabstat u if pea==1 & P6210<=4 [fweight=int(factor_expansion)], by(area) 
		*Educacion media o menos
			tabstat u if pea==1 & P6210<=5 [fweight=int(factor_expansion)], by(area) 
		*Superior o universitaria*
			tabstat u if pea==1 & P6210==6 [fweight=int(factor_expansion)], by(area) 

			
*2B.DEFINICION DE LA MUESTRA
********
	
	*Muestra*
		gen sample=0
		replace sample=1 if edad>=15 & edad<=60 & pea==1
	

*2C.ESTIMACION MODELO DE DESEMPLEO
********

	*ESTIMACION PARA EL TOTAL DE LA MUESTRA
		char area [omit] 11
		xi: probit u c.yedu##c.yedu c.edad##c.edad i.mujer i.area if sample==1
			margins, dydx(_all) post
			eststo des_total_nacional
	
	*ESTIMACIÓN POR GENERO

		*Mujeres*
		char area [omit] 11
		xi: probit u c.yedu##c.yedu c.edad##c.edad i.mujer i.area if sample==1 & mujer==1
			margins, dydx(_all) post
			eststo des_total_mujer
	
		*Hombres*	
		char AREA [omit] 11
		xi: probit u c.yedu##c.yedu c.edad##c.edad i.mujer i.area if sample==1 & mujer==0
			margins, dydx(_all) post
			eststo des_total_hombre
	
	*CONSOLIDACION DE ESTIMACIONES EN TABLA DE REGRESION
	ssc install outreg2
	
		est restore des_total_nacional
			outreg2 using "Resultadosdes.xls", replace ctitle(margins) bdec(4) tdec(3) stats(coef se) addstat(Log L, logl_t) label word excel tex
		est restore des_total_mujer
			outreg2 using "Resultadosdes.xls", replace ctitle(margins) bdec(4) tdec(3) stats(coef se) addstat(Log L, logl_m) label word excel tex
		est restore des_total_hombre
			outreg2 using "Resultadosdes.xls", replace ctitle(margins) bdec(4) tdec(3) stats(coef se) addstat(Log L, logl_h) label word excel tex


*2D.CORRECCIÓN DE AUTOSELECCIÓN DE HECKMAN
********

	/* /!\ NOTA: El comando heckprobit estima un modelo probit corrigiendo por el sesgo de selección.
				 Para esto se debe contar con las variables que pueden explicar dicha autoselección.
				  *** http://www.stata.com/manuals13/rheckprobit.pdf ***/

	*Suponiendo que no podemos cuantificar la tasa de participacion (pea):
		gen sample2=(edad>=15 & edad<=60)


	*ESTIMACION PARA EL TOTAL DE LA MUESTRA
		char area [omit] 11
		xi: heckprob u c.yedu##c.yedu c.edad##c.edad i.mujer i.area if sample2==1, select(pea = c.yedu##c.yedu c.edad##c.edad i.area i.mujer domserv pmayorhog pmenorhog) tech(nr bhhh 20)
			
			margins if e(sample), predict(pcond) dydx(*) post
			est store des_total_corr
		
	*ESTIMACION POR GENERO

		*Mujeres*
		char area [omit] 11
		xi: heckprob u c.yedu##c.yedu c.edad##c.edad i.area if sample2==1 & mujer==1, select(pea = c.yedu##c.yedu c.edad##c.edad i.area domserv pmayorhog pmenorhog) tech(nr bhhh 20)
			
			margins if e(sample), predict(pcond) dydx(*) post
			est store des_mujer_corr
			
		*Hombres*
		char area [omit] 11
		xi: heckprob u c.yedu##c.yedu c.edad##c.edad i.area if sample2==1 & mujer==0, select(pea = c.yedu##c.yedu c.edad##c.edad i.area domserv pmayorhog pmenorhog) tech(nr bhhh 20)
			
			margins if e(sample), predict(pcond) dydx(*) post
			est store des_hombre_corr
		
	*CONSOLIDACIÓN DE LAS ESTIMACIONES EN TABLA DE REGRESIÓN
		est restore des_total_corr
			outreg2 using "Resultadoscorr.xls", replace ctitle(margins) bdec(4) tdec(3) stats(coef se) addstat(N, obs_t, Rho, rho, se, rho_es, Chi2(1), rho_chi2, Prob>chi2, rho_pval, Log L, ll_t) label word excel tex
		est restore des_mujer_corr
			outreg2 using "Resultadoscorr.xls", replace ctitle(margins) bdec(4) tdec(3) stats(coef se) addstat(N, obs_t, Rho, rho, se, rho_es, Chi2(1), rho_chi2, Prob>chi2, rho_pval, Log L, ll_t) label word excel tex
		est restore des_hombre_corr
			outreg2 using "Resultadoscorr.xls", replace ctitle(margins) bdec(4) tdec(3) stats(coef se) addstat(N, obs_t, Rho, rho, se, rho_es, Chi2(1), rho_chi2, Prob>chi2, rho_pval, Log L, ll_t) label word excel tex	
		
		
****************************
*3.DESCOMPOSICIÓN DE OAXACA*
****************************

*3A.OUTLIERS
********

	/* /!\ NOTA: Crear una variable que identifique los percentiles de la distribución de 
				 ingresos y eliminar los datos atípicos de ambas colas (percentiles 1 y 100). */

	*Percentiles*
		xtile percentil=wage if sample==1, nq(100)
	*Eliminar outliers*
		replace wh=. if percentil==1|percentil==100

		
*3B.ESTIMACIÓN DE OAXACA
********

ssc install oaxaca
help oaxaca

reg lnwh mujer yedu exp exp2 ant casado
oaxaca lnwh yedu exp exp2 ant casado, by (mujer) vce(robust) detail(Educacion: yedu, Experiencia: exp exp_2, Tenure: ant, Caracteristicas: casado)
outreg2 using regresiones.xls, replace dec(3) see label title("Tabla 1. Resultados") ctitle("Blinder-Oaxaca") addnote("Source: GEIH 2018 Tercer Trimestre") 

oaxaca lnwh yedu exp exp2 ant casado, by (mujer) vce(robust) detail(Educacion: yedu, Experiencia: exp exp_2, Tenure: ant, Caracteristicas: casado) pooled
outreg2 using regresiones.xls, append dec(3) see label title("Tabla 1. Resultados") ctitle("Blinder-Oaxaca Pooled") addnote("Source: GEIH 2018 Tercer Trimestre")

	

heckprob u $x_desem i.mujer i.ciudad, select(pea = $x_part i.mujer i.ciudad) tech(nr bhhh 20) vce(robust)

	
	
	
	
	
	
