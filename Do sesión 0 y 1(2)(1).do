*******************************************************
***ECONOMIA LABORAL - I TALLER: INTRODUCCION A STATA***
*******************************************************

/* Sesión 0: INTRODUCCIÓN GENERAL */
clear
set more off, permanently


cd "C:\Users\aerdn\Documents\Universidad\Séptimo semestre\Monitoria Laboral\GEIH20182\CD Stata" 

**Importar y unir datos datos a Stata (GEIH 2018): La base de datos esta por meses y modulos. Para este caso importaremos y uniremos abril, mayor y junio de todos los módulos de cabecera.

	*Nota:Para la descripcipn de variables, entrar a: https://formularios.dane.gov.co/Anda_4_1/index.php/catalog/427/data_dictionary
	*Importar datos a Stata:

*Abril

		use  "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - CaracterÃ­sticas generales (Personas).txt"
		save Caracteristicas_Generales_Abril, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Desocupados.txt"
		save Desocupados_Abril, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Fuerza de trabajo.txt"
		save FuerzaTrabajo_Abril, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Inactivos.txt"
		save Inactivos_Abril, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Ocupados.txt"
		save Ocupados_Abril, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Otras actividades y ayudas en la semana.txt"
		save OtrasActividades_Abril, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Otros ingresos.txt"
		save OtrosIngresos_Abril, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Vivienda y Hogares.txt"
		save Vivienda_Hogares_Abril, replace
		clear

	*Mayo
		use  "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - CaracterÃ­sticas generales (Personas).txt"
		save Caracteristicas_Generales_Mayo, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Desocupados.txt"
		save Desocupados_Mayo, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Fuerza de trabajo.txt"
		save FuerzaTrabajo_Mayo, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Inactivos.txt"
		save Inactivos_Mayo, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Ocupados.txt"
		save Ocupados_Mayo, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Otras actividades y ayudas en la semana.txt"
		save OtrasActividades_Mayo, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Otros ingresos.txt"
		save OtrosIngresos_Mayo, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Vivienda y Hogares.txt"
		save Vivienda_Hogares_Mayo, replace
		clear
	*Junio
		use  "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - CaracterÃ­sticas generales (Personas).txt"
		save Caracteristicas_Generales_Junio, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Desocupados.txt"
		save Desocupados_Junio, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Fuerza de trabajo.txt"
		save FuerzaTrabajo_Junio, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Inactivos.txt"
		save Inactivos_Junio, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Ocupados.txt"
		save Ocupados_Junio, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Otras actividades y ayudas en la semana.txt"
		save OtrasActividades_Junio, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Otros ingresos.txt"
		save OtrosIngresos_Junio, replace
		clear

		use "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Vivienda y Hogares.txt"
		save Vivienda_Hogares_Junio, replace
		clear

	*Nota: para importar datos en otros formtaos (ejemplo Excel) escribir help import o "cacharrear" en la comamnd window dandole click a file/import.

*Juntando bases de datos que tienen una variable "llave"

*Juntando todo Abirl: Merge
	use "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - CaracterÃ­sticas generales (Personas)", clear 
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Desocupados"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Fuerza de trabajo"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Inactivos"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Ocupados"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Otras actividades y ayudas en la semana"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Otros ingresos.dta"
	drop _merge
	merge m:1 directorio secuencia_p using "C:\Users\sala801\Downloads\SESION1\Abril.txt\Abril.txt\Ãrea - Vivienda y Hogares.dta"
	drop _merge

	save GEIH_Abril.dta, replace

*Juntando todo Mayo: Merge
	use "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - CaracterÃ­sticas generales (Personas)", clear 
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Desocupados"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Fuerza de trabajo"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Inactivos"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Ocupados"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Otras actividades y ayudas en la semana"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Otros ingresos.dta"
	drop _merge
	merge m:1 directorio secuencia_p using "C:\Users\sala801\Downloads\SESION1\Mayo.txt\Mayo.txt\Ãrea - Vivienda y Hogares.dta"
	drop _merge

	save GEIH_Mayo.dta, replace

*Juntando todo Junio: Merge
	use "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - CaracterÃ­sticas generales (Personas)", clear 
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Desocupados"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Fuerza de trabajo"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Inactivos"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Ocupados"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Otras actividades y ayudas en la semana"
	drop _merge
	merge 1:1 directorio secuencia_p orden hogar using "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Otros ingresos.dta"
	drop _merge
	merge m:1 directorio secuencia_p using "C:\Users\sala801\Downloads\SESION1\Junio.txt\Junio.txt\Ãrea - Vivienda y Hogares.dta"
	drop _merge

	save GEIH_Junio.dta, replace



	*Nota: Para mayor informacion de merge, ver: https://www.princeton.edu/~otorres/Merge101.pdf

	
*Uniendo todos los meses
	clear
	use GEIH_Abril.dta, clear
	append using GEIH_Mayo.dta, force
	append using GEIH_Junio.dta, force
	compress

	gen factor_expansion= (fex_c_2011 / 3) /*OJO: Muy importante dividir el factor de expansion entre el numero de meses*/

	save GEIH_II_trimestre2018.dta, replace

***Generalidades: Estadísticas descriptivas, condicionales, macros, entre otras.

use GEIH_II_trimestre2018.dta, clear

**Estadísticas descriptivas**
tab DPTO
tab P6020
rename P6020 genero
tab DPTO genero

sum P6040
rename P6040 edad
count if edad > 60
count if edad >=22 & edad <=26
count if edad >=22 & edad <=26 & genero==2
count if genero != 2
count if genero != .

rename P6220 nivel_educativo
tab nivel_educativo
sum nivel_educativo /*OJO: ESTO NO TIENE SENTIDO PUES nivel_educativo es una variable categórica. Nunca hacer esto para variables categóricas para mostrar descriptivos o meterlas en regresiones.*/
tab nivel_educativo, gen(nivel_educativo)
sum nivel_educativo1- nivel_educativo5 /*Esto si*/
tab DPTO, gen(DPTO) /*Lo mismo para los departamentos y cualquier otra variable categórica*/

rename P6090 cotiza_eps

table DPTO if cotiza_eps==1 , contents(count cotiza_eps)
table DPTO if cotiza_eps==1 [pweight = factor_expansion], contents( count cotiza_eps)



gen soltero=1
replace soltero=0 if P6070==1 | P6070==2 | P6070==3
sum soltero

replace genero=0 if genero==1
replace genero=1 if genero==2
tab genero, gen(genero)


rename P6440 tiene_contrato
replace tiene_contrato=0 if tiene_contrato==2


rename P6426 tiempo_en_trabajo /*(en meses)*/

sum tiempo_en_trabajo tiene_contrato edad genero soltero nivel_educativo1- nivel_educativo5 dpto1- dpto13

ssc install outreg2

set matsize 2000

outreg2 using Descriptivos.xls, replace sum(log) keep(tiempo_en_trabajo tiene_contrato edad genero soltero nivel_educativo1 nivel_educativo2 nivel_educativo3 - nivel_educativo5 dpto1- dpto13)

reg tiempo_en_trabajo tiene_contrato edad genero soltero nivel_educativo1 nivel_educativo3-nivel_educativo5 DPTO2- DPTO13, r

global controles1 tiene_contrato edad genero soltero nivel_educativo1 nivel_educativo3-nivel_educativo5 DPTO2- DPTO13
global controles2 tiene_contrato edad genero soltero nivel_educativo1 nivel_educativo3-nivel_educativo5

reg tiempo_en_trabajo tiene_contrato edad genero soltero nivel_educativo1 nivel_educativo3-nivel_educativo5 DPTO2- DPTO13, r
reg tiempo_en_trabajo $controles1 ,r
reg tiempo_en_trabajo $controles2 ,r 

	*Nota: para eliminar un global, usar el comando macro drop NombreDelGlobal





/* Sesión 1: PARTICIPACIÓN LABORAL */

*1) descripcion de la base

describe
	  
*2) Creacion y limpieza de variables

*Variables de individuos (gente)

	*1. Genero: Crear una variable dicótoma para las mujeres de la base		
		gen genero=P6020
			tab genero
			recode genero (1=0)(2=1), gen(mujer)
			label var mujer "mujer==1"
		
	*B. Edad: Crear una variable de edad y ver su distribución	
		gen edad=P6040
			sum edad
			histogram edad, bin(106) xtitle("Edad en años") ytitle("Densidad") title("Histograma") subtitle("Edad")
			save graph, replace
			kdensity edad, xtitle("Edad en años") ytitle("Densidad") title("Kernel") subtitle("Edad")
			label var edad "edad de la persona"
		
	*C. Estado civil: Crear una binaria para las personas que esten casadas o en unión libre de la base		
		tab P6070
			gen casado=.
			replace casado = 1 if P6070==1 | P6070==2| P6070==3
			replace casado = 0 if P6070==4 | P6070==5| P6070==6
			label var casado "casado o en unión libre==1" 

			*Otra forma de calcular esta variable en Stata podría ser: 
			gen casado2 = (P6070==1 | P6070==2| P6070==3)
	
			*¿Es igual casado a casado2? 
			tab casado 
			tab casado2
	
			/* NOTA: Las variables NO son iguales. la diferencia está en los missing values (valores no 
				         reportados = valores con .) en la variable y QUE NO DEBEN CONTABILIZARSE COMO CEROS!!!!! */
			
			tab casado, missing
			tab casado2, missing
			
			*Las personas que no tienen respuesta son niños porque no tendría sentido preguntar esto a niños
			
			sum edad if casado == .
			
			/*MORALEJA: TENER MUCHO CUIDADO CON LOS MISSING, NO HAY QUE CONTARLOS COMO CEROS*/
	
	*D. Población en Edad de Trabajar (PET)*	
		tab P6240
			gen pet = (P6240==1|P6240==2|P6240==3|P6240==4|P6240==5|P6240==6)
			label variable pet "Poblacion en edad de trabajar==1"
			
			*Otra forma de calcular esta variable podría ser como las personas con 12 años o más
			gen pet2 = (edad>=12)
			
			*¿Es igual pet a pet2? Analice mediante un tab ambas variables
			tab pet 
			tab pet2
			
			/* NOTA: En este caso SI son iguales porque la variable "edad" no tiene missing values.*/
			
	*E. Empleados (E)*	
		gen e =  pet
			replace e = 0 if P6240==2|P6240==3|P6240==4|P6240==5|P6240==6
			replace e = 1 if P6250==1
			replace e = 1 if P6260==1
			replace e = 1 if P6270==1 
			replace e = . if pet==0
			label variable e "empleados==1"
			
	*F. Desempleados (U)*
		gen u = (P7250>0 & P7250!=.)
			replace u = . if pet==0
			label variable u "desempleados==1"

	*G. Población Economicamente Activa (PEA)*
		gen pea = (e==1 |u==1)
			replace pea = . if pet==0
			label var pea "Población Economicamente activa==1"	
			
	*F. Años de educación*
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
			
			sum yedu

*VARIABLES A NIVEL HOGARES*

	*A. Identificador del Hogar: Crear una variable que identifique el hogar dentro de la vivienda
			egen idh = concat(DIRECTORIO HOGAR)  
			order idh, after (HOGAR)

			*Conviene algunas veces organizar la base por la variable (en este caso el hogar)
			sort idh
			
	*B. Niños menores de 7 años
		gen nmenor=1 if edad<7
		
			*Si se quiere contar los niños menores a 7 años que tienen una relacion con el jefe de hogar y que viven en el hogar
			bysort idh: egen nmenorhog=total(nmenor) if P6050<6
			
			*Si solo se esta interesado en la presencia o no de niños menores a 7 años
			bys idh: egen pmenorhog=max(edad<7) if P6050<6
			list idh DIRECTORIO HOGAR edad nmenor nmenorhog pmenorhog P6050 mujer if nmenorhog>1 in 1800/2000
					
			*Notese que en ambas variables un 63.35% de los hogares no tiene presencia de niños
			tab nmenorhog
			tab pmenorhog

	*C. Niños mayores de 7 años y estudiando
		gen nmayor=1 if edad>6 & edad<18 & P6170==1
		
			*Si se quiere contar los niños mayores de 7 años y estudiando, que tienen una relación con el jefe de hogar y que viven en el hogar
			by idh: egen nmayorhog=total(nmayor) if P6050<6
			
			*Si solo se está interesado en la presencia o no de niños mayores de 7 años y estudiando
			bysort idh: egen pmayorhog=max(nmayor==1) if P6050<6

	*D. Presencia de servicio domestico
			bysort DIRECTORIO: egen domserv=max(P6050==6)



*3)ESTADÍSTICAS DESCRIPTIVAS


	*A. Media de la Tasa de Participación por género*
		tabstat pea, by(mujer)
		
			*Usando el factor de expansión para obtener descriptivos que describan la poblacion que representa la muestra
			tabstat pea [fweight=int(factor_expansion)], by(mujer)

	*B. Diferencias en la Tasa de Participacion*

			
			ttest pea, by(mujer)
			*Los resultados guardados de un comando se pueden ver con el comando
			return list

					 
	*C. Diferencias en características que pueden afectar la participación entre ambos grupos*
		global descriptivas edad casado yedu
		
		putexcel set "T-test.xlsx", sheet("t_test_1") replace 
		putexcel					B1=("Estadísticas Descriptivas-T-test")																						
		putexcel A2=("Variable") 	B2=("Hombres")                   	E2=("Mujeres") 											
		putexcel             	 	B3=("Obs.") C3=("Media") D3=("D.E.")  E3=("Obs.") F3=("Media") G3=("D.E.") H3=("Diferencia de medias")	

		local row=4
								 
		foreach var of varlist  $descriptivas {

		   qui ttest `var'  , by (genero)
		   
		putexcel A`row' = ("`var'")
		putexcel B`row'	= (r(N_1))
		putexcel C`row' = (r(mu_1))
		putexcel D`row' = (r(sd_1))
		putexcel E`row' = (r(N_2))
		putexcel F`row' = (r(mu_2))
		putexcel G`row' = (r(sd_2))
		putexcel H`row' =  (r(mu_1)-r(mu_2))

		local p= r(p)
		local s ""
			if `p'<0.01 {
				local s "***"
			}
			else if `p'<0.05 {
				local s "**"
			}
			else if `p'<0.1 {
				local s "*"
			}
			else {
				local s ""
			}
			putexcel I`row' = ("`s'")
			
			local ++row
			
			}


*4.ESTIMACIONES*

	*Haciendo gráficos de las horas trabajadas por grupo de género
		gen horas_trab = P6800
		
			*Horas trabajadas si la persona es mujer
			twoway histogram horas_trab if mujer==1, xtitle("Horas de Trabajo") ytitle("") title("Mujer") scheme(s2mono)
			graph save mujer.gph, replace
			
			*Horas trabajadas si la persona es hombre
			twoway histogram horas_trab if mujer==0, xtitle("Horas de Trabajo") ytitle("") title("Hombre") scheme(s2mono)
			graph save hombre.gph, replace
			
			gr combine mujer.gph hombre.gph 
			
	*Probando estadísticamente si las horas trabajadas difieren entre hombres y mujeres
			ttest horas_trab, by(mujer)

	*Una regresión MCO de la PEA sobre el género debe dar el mismo resultado que el ttest
			
			reg pea mujer casado edad,r
			
		*Para mostrar tablas de regresión tipo publicacion
			reg pea mujer edad, r
			outreg2 using Regresion.xls, replace ctitle(MCO - probabilidad lineal) onecol
			
	*Corrigiendo los problemas del modelo de probabilidad lineal con un probit de participacion
	
			probit pea mujer, r
			reg pea mujer, r
			probit pea mujer, r
			mfx
			outreg2 using Regresion.xls, append ctitle(Probit) onecol
	
			dprobit pea mujer, r
			
			
			
			
			
/**NOTA de operadores
> 			mayor a
< 			menor a 
== 			igual a
>= 			mayor o igual
<= 			menor o igual
!= 			diferente a
&			Y
|			o
!			no


**Modificar la base de datos**
label data	 	darle un nombre a la base de datos
order	 		ordenar las variables de la base de datos
label variable	darle un nombre a una variable
label define	
label values	aplicar los nombres a una variables 
rename		    renombrar una variable
recode		    recodificar los valores de una variable
notes			poenr notas a la base de datos
generate		crear una variable nueva
egen			tiene funciones especiales
replace		    reemplazar un valor 
by				ejecutar el comando basado en valores de varlist
if				condición al final del comando
in 			    condición al final del comando**/

use hh_98, clear
drop dmmfd dfmfd
save hh_98_1.dta, replace

use hh_98, clear
keep nh dmmfd dfmfd
save hh_98_2.dta, replace
