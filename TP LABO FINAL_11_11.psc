//Imprime el tablero actual en pantalla
Funcion ImprimirTablero (tablero,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
	Definir a,j,f,fila Como Entero
	a<-0
	j<-0
	f<-Trunc(cantPixelesFila/2) +1
	fila<-cantCuadradosFilaYColum + 1
	Escribir "|   A   ||   B   ||   C   ||   D   ||   E   ||   F   ||   G   ||   H   |"
	Para i<- 1 Hasta cantPixelesColum*cantPixelesFila*(cantCuadradosFilaYColum*cantCuadradosFilaYColum) Con Paso 1 Hacer
		a<-a+1
		Escribir Sin Saltar tablero[i]
		Si a = cantPixelesColum*cantCuadradosFilaYColum Entonces
			a<-0
			j<-j+1
			Si j = f Entonces
				fila <- fila - 1
				Escribir fila
				f<-cantPixelesFila
				j<-0
			SiNo
				Si f = cantPixelesFila Entonces
					Si j = 2 O j = 3 Entonces
						Escribir "-"
					SiNo
						Escribir ""
					FinSi
				SiNo
					Si j = 1 O j = 3 Entonces
						Escribir "-"
					SiNo
						Escribir ""
					FinSi
					
				FinSi
			FinSi
		FinSi
	FinPara
Fin Funcion

//Carga el tablero con los valores iniciales
Funcion TableroInicial (tablero Por Referencia,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
	Definir cambiaRelleno Como Logico
	cambiaRelleno <- Falso
	
	Definir relleno Como Caracter
	
	j<-0
	Para i<-1 Hasta cantPixelesColum*cantPixelesFila*(cantCuadradosFilaYColum*cantCuadradosFilaYColum) Con Paso 1 Hacer
		j<-j+1
		Si cambiaRelleno = Falso Entonces
			relleno <- " "
		SiNo
			relleno <- "*"
		FinSi
		
		tablero[i] <- relleno
		
		Si i%cantPixelesColum = 0 Entonces
			cambiaRelleno<-!cambiaRelleno
		FinSi
		Si i%(cantPixelesColum*cantPixelesFila*cantCuadradosFilaYColum) = 0 Entonces
			j<-0
			cambiaRelleno<-!cambiaRelleno
		FinSi
	FinPara
Fin Funcion

//Carga inicial del tablero logico
Funcion CargarTableroLogico(tablero Por Referencia,cantCuadradosFilaYColum)
	
	Definir cantidadDePiezasColor Como Entero
	cantidadDePiezasColor <-12
	Dimension piezasNegras[cantidadDePiezasColor]
	Dimension piezasBlancas[cantidadDePiezasColor]
	
	piezasNegras[1] <- 2;piezasNegras[2] <- 4;piezasNegras[3] <- 6;piezasNegras[4] <- 8;piezasNegras[5] <- 9;piezasNegras[6] <- 11;piezasNegras[7] <- 13;piezasNegras[8] <- 15;piezasNegras[9] <- 18;piezasNegras[10] <- 20;piezasNegras[11] <- 22;piezasNegras[12] <- 24
	piezasBlancas[1] <- 41;piezasBlancas[2] <- 43;piezasBlancas[3] <- 45;piezasBlancas[4] <- 47;piezasBlancas[5] <- 50;piezasBlancas[6] <- 52;piezasBlancas[7] <- 54;piezasBlancas[8] <- 56;piezasBlancas[9] <- 57;piezasBlancas[10] <- 59;piezasBlancas[11] <- 61;piezasBlancas[12] <- 63
	
	
	//Carga las fichas en el tablero logico
	Para i <- 1 Hasta cantCuadradosFilaYColum*cantCuadradosFilaYColum Con Paso 1 Hacer
		tablero[i] <- 0
	FinPara
	
	//Carga las fichas en el tablero logico
	Para i <- 1 Hasta cantidadDePiezasColor Con Paso 1 Hacer
		tablero[piezasnegras[i]] <- 2
		tablero[piezasBlancas[i]] <- 1
	FinPara
FinFuncion

Funcion ImprimirTableroLogico(tablero,cantCuadradosFilaYColum)
	Para i <- 1 Hasta cantCuadradosFilaYColum*cantCuadradosFilaYColum Con Paso 1 Hacer
		Escribir sin saltar tablero[i]
		Si i%cantCuadradosFilaYColum = 0 Entonces
			Escribir ""
		FinSi
	FinPara
FinFuncion

Funcion GraficarFichasDesdeLogicoAGrafico(tableroGrafico Por Referencia,tableroLogico,cantCuadradosFilaYColum)
	//Evalua cada posicion del tablero logico
	
	Definir filaPixelInf,filaPixelSup,columPixelIzq,columPixelDer,columna,fila Como Entero
	Definir relleno Como Caracter
	
	Dimension posEnTableroTexto[3]
	Para posLogica <-1 Hasta cantCuadradosFilaYColum*cantCuadradosFilaYColum Con Paso 1 Hacer
		//Si detecta una ficha OK
		Si tableroLogico[posLogica] > 0 Entonces
			//Escribir "Cuadrado con relleno: ", posLogica
			Segun tableroLogico[posLogica]
				1: 
					relleno <- "_"
				2: 
					relleno <- "B"
				3: 
					relleno <- "Q"
				4: 
					relleno <- "?"
				5: 
					relleno <- "?"
				6,7,8,9: 
					relleno <- "X"
			FinSegun
			restoDiv <- posLogica % cantCuadradosFilaYColum
			Si (restoDiv) > 0 Entonces
				fila <- Trunc(posLogica/cantCuadradosFilaYColum)+1
			SiNo
				fila <- posLogica / cantCuadradosFilaYColum
			FinSi
			//AMBOS DETERMINAN EL TAMAÑO DEL RELLENO
			filaPixelInf <- fila * 5 - 2 
			filaPixelSup <- fila * 5 - 4
			
//			Escribir "Fila: ", fila
//			Escribir "Fila pixel sup: ", filaPixelSup
//			Escribir "Fila pixel inf: ", filaPixelInf
			
			Si (restoDiv) = 0 Entonces
				columna <- cantCuadradosFilaYColum
			SiNo
				columna <- restoDiv
			FinSi
			
			//AMBOS DETERMINAN EL TAMAÑO DEL RELLENO
			columPixelIzq <- columna * 9 - 6
			columPixelDer <- columna * 9 - 2
			
//			Escribir "Columna: ", columna
//			Escribir "Columna pixel izq: ", columPixelIzq
//			Escribir "Columna pixel der: ", columPixelDer
			
			//Escribir " "
			CalcularPosicionEnTablero(posEnTableroTexto,posLogica,cantCuadradosFilaYColum)
			Para h <- filaPixelSup Hasta filaPixelInf Con Paso 1 Hacer
				Si filaPixelInf-h <> 1 Entonces //El numero al que debe ser desigual aclara en que linea se deja el espacio (en el caso de 1 en una ficha de 3 de altura, deja el centro
					Para f <- columPixelIzq Hasta columPixelDer Con Paso 1 Hacer
						//Escribir "Pixeles con relleno: ", f+72*h
						tableroGrafico[f+72*h] <- relleno
					FinPara
				SiNo
					tableroGrafico[columPixelIzq+1+72*h]<-posEnTableroTexto[1]
					tableroGrafico[columPixelIzq+2+72*h]<-posEnTableroTexto[2]
					tableroGrafico[columPixelIzq+3+72*h]<-posEnTableroTexto[3]
					Para f <- columPixelIzq Hasta columPixelDer Con Paso 4 Hacer //El paso fija la separacion entre paredes
						//Escribir "Pixeles con relleno: ", f+72*h
						tableroGrafico[f+72*h] <- relleno
					FinPara
				FinSi
			FinPara
		FinSi
	FinPara
FinFuncion

Funcion CalcularPosicionEnTablero (res Por Referencia, posLogica, cantCuadradosFilaYColum)
	Dimension posEnTablero[3]
	Definir fila,columna Como Caracter
	
	restoDiv <- posLogica % cantCuadradosFilaYColum
	
	Si (restoDiv) > 0 Entonces
		fila <- ConvertirATexto(8 - Trunc(posLogica/cantCuadradosFilaYColum))
	SiNo
		fila <- ConvertirATexto(9 - posLogica / cantCuadradosFilaYColum)
	FinSi
	
	Si (restoDiv) = 0 Entonces
		columna <- ConvertirATexto(cantCuadradosFilaYColum)
	SiNo
		columna <- ConvertirATexto(restoDiv)
	FinSi
	
	Segun ConvertirANumero(columna) Hacer
		1:
			columna<-"A"
		2:
			columna<-"B"
		3:
			columna<-"C"
		4:
			columna<-"D"
		5:
			columna<-"E"
		6:
			columna<-"F"
		7:
			columna<-"G"
		8:
			columna<-"H"
	Fin Segun
	//Escribir "Posicion en tablero de ",posLogica,": ",columna, "-",fila
	res[1] <- columna
	res[2] <- "-"
	res[3] <- fila
FinFuncion


Funcion res <- ConvertirPosicionGraficaALogica(posCalculable)
	Dimension posColumnasValidas[8]
	posColumnasValidas[1] <- "A";posColumnasValidas[2] <- "B";posColumnasValidas[3] <- "C";posColumnasValidas[4] <- "D";posColumnasValidas[5] <- "E";posColumnasValidas[6] <- "F";posColumnasValidas[7] <- "G";posColumnasValidas[8] <- "H";
	Si Longitud(posCalculable) = 3 Entonces
		Definir posColumnas Como Caracter
		Definir posFilas, posColumnasInterpretada Como Entero
		Definir valida Como Logico
		valida <- Falso
		posFilas <- ConvertirANumero(Subcadena(posCalculable,3,3))
		
		Para i <- 1 Hasta 8 Con Paso 1 Hacer
			Si posFilas = i Entonces
				valida <- Verdadero
				i <- 11 //PARA SALIR DEL BUCLE!
			FinSi
		FinPara
		Si valida = Verdadero Entonces
			
			posColumnas <- Subcadena(posCalculable,1,1)
			valida <- Falso
			Para i <- 1 Hasta 8 Con Paso 1 Hacer
				Si posColumnas = posColumnasValidas[i] Entonces
					valida <- Verdadero
					posColumnasInterpretada <- i
					i <- 11 //PARA SALIR DEL BUCLE!
				FinSi
			FinPara
			Si valida = Verdadero Entonces
				res <- (8-posFilas)*8+posColumnasInterpretada
			SiNo
				res <- -20
			FinSi
		SiNo
			res <- -10
		FinSi
		
	SiNo
		Escribir "Error interno nº ", err
	FinSi
FinFuncion

Funcion turno <- CambiaTurno(turnoActual)
	Si turnoActual = "negras" Entonces
		turno <- "blancas"
	SiNo
		turno <- "negras"
	FinSi
FinFuncion

//Verifica si la ficha se convierte en dama
Funcion VerificaSiHayCoronacion(tableroLogico Por Referencia, posLogicaInicial, posLogicaFinal)
	Segun tableroLogico[posLogicaInicial] Hacer
		1:
			Si posLogicaFinal <= 8 Entonces
				tableroLogico[posLogicaInicial] <- 3
			FinSi
		2:
			Si posLogicaFinal >= 57 Entonces
				tableroLogico[posLogicaInicial] <- 4
			FinSi
	FinSegun
FinFuncion
//Valida la posicion a la que se dirige la nueva ficha y devuelve si se comio una ficha para poder repetir el turno 
Funcion res <- EjecutaMovimientoDeFicha(tableroLogico Por Referencia, posLogicaInicial, posLogicaFinal)
	Definir resultado Como Logico
	Definir posicionIntermedia Como Entero
	resultado <- Falso
	//Si se elimino una pieza la borra del tablero logico SOLO APLICA A FICHAS COMUNES NO DAMAS
	Si Abs(posLogicaFinal - PosLogicaInicial) > 10 Entonces
		posicionIntermedia <- (posLogicaFinal+posLogicaInicial)/2
		tableroLogico[posicionIntermedia] <- 0
		resultado <- Verdadero
	FinSi
	tableroLogico[posLogicaFinal] <- tableroLogico[posLogicaInicial]
	tableroLogico[posLogicaInicial] <- 0 
	res<- resultado
FinFuncion

//Limpia los movimientos disponibles (y las posibles piezas a comer)
Funcion BorrarMovimientos(tableroLogico Por Referencia, cantCuadradosFilaYColum)
	Para pos <- 1 Hasta cantCuadradosFilaYColum * cantCuadradosFilaYColum Con Paso 1 Hacer
		Segun tableroLogico[pos] Hacer
			5:
				tableroLogico[pos] <- 0 
			6: 
				tableroLogico[pos] <- 2
			7: 
				tableroLogico[pos] <- 1
			8: 
				tableroLogico[pos] <- 4
			9: 
				tableroLogico[pos] <- 3
				
		FinSegun
	FinPara
FinFuncion

//FUNCION CRITICA, SI LA CAGAS SONAMOS
Funcion CargaOpcionesDeMovimiento(tableroGrafico,tableroLogico Por Referencia, posLogica)
	Dimension posiblesMovimientos[15]
	Dimension posicionCalculada[3]
	Dimension posicionCalculadaPosible[3]
	Definir tipoFicha, fila Como Entero
	tipoFicha <- tableroLogico[posLogica]
	
	Segun tipoFicha Hacer
		//FICHA BLANCA
		1:
			//Primer condicion para no hacer overflow en el vector del tablero logico, si corresponde coloca un 5 en el lugar donde van los movimientos permitidos
			Si posLogica - 7 > 0 Entonces
				CalcularPosicionEnTablero(posicionCalculada, posLogica,8)
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-7,8)
				Si ConvertirANumero(posicionCalculada[3]) <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica-7]
						0:
							tableroLogico[posLogica-7] <- 5
						1,3:
							
						2,4:
							Si posLogica-14 > 0 Entonces
								Si tableroLogico[posLogica-14] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-14,8)
									Si ConvertirANumero(posicionCalculada[3])+1 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica-7] = 2 Entonces
											tableroLogico[posLogica-7] <- 6 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica-7] <- 8 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica-14] <- 5
									FinSi
								FinSi
							FinSi
					FinSegun
				FinSi
			FinSi
			Si posLogica - 9 > 0 Entonces
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-9,8)
				Si ConvertirANumero(posicionCalculada[3])+2 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica-9]
						0:
							tableroLogico[posLogica-9] <- 5
						1,3:
							
						2,4:
							Si posLogica-18 > 0 Entonces
								Si tableroLogico[posLogica-18] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-18,8)
									Si ConvertirANumero(posicionCalculada[3])+3 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica-9] = 2 Entonces
											tableroLogico[posLogica-9] <- 6 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica-9] <- 8 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica-18] <- 5
									FinSi
								FinSi
							FinSi
					FinSegun
				FinSi
			FinSi
		//FICHA NEGRA
		2: 
			//Primer condicion para no hacer overflow en el vector del tablero logico, si corresponde coloca un 5 en el lugar donde van los movimientos permitidos
			Si posLogica + 7 < 65 Entonces
				CalcularPosicionEnTablero(posicionCalculada, posLogica,8)
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+7,8)
				Si ConvertirANumero(posicionCalculada[3]) <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica+7]
						0:
							tableroLogico[posLogica+7] <- 5
						1,3:
							Si posLogica+14 < 65 Entonces
								Si tableroLogico[posLogica+14] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+14,8)
									Si ConvertirANumero(posicionCalculada[3])-1 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica+7] = 1 Entonces
											tableroLogico[posLogica+7] <- 7 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica+7] <- 9 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica+14] <- 5
									FinSi
								FinSi
							FinSi
						2,4:
					FinSegun
				FinSi
			FinSi
			Si posLogica + 9 < 65 Entonces
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+9,8)
				Si ConvertirANumero(posicionCalculada[3])-2 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica+9]
						0:
							tableroLogico[posLogica+9] <- 5
						1,3:
							Si posLogica+18 < 65 Entonces
								Si tableroLogico[posLogica+18] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+18,8)
									Si ConvertirANumero(posicionCalculada[3])-3 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica+9] = 1 Entonces
											tableroLogico[posLogica+9] <- 7 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica+9] <- 9 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica+18] <- 5
									FinSi
								FinSi
							FinSi
						2,4:
					FinSegun
				FinSi
			FinSi
		//DAMA BLANCA
		3:
			//Primer condicion para no hacer overflow en el vector del tablero logico, si corresponde coloca un 5 en el lugar donde van los movimientos permitidos
			Si posLogica - 7 > 0 Entonces
				CalcularPosicionEnTablero(posicionCalculada, posLogica,8)
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-7,8)
				Si ConvertirANumero(posicionCalculada[3]) <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica-7]
						0:
							tableroLogico[posLogica-7] <- 5
						1,3:
							
						2,4:
							Si posLogica-14 > 0 Entonces
								Si tableroLogico[posLogica-14] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-14,8)
									Si ConvertirANumero(posicionCalculada[3])+1 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica-7] = 2 Entonces
											tableroLogico[posLogica-7] <- 6 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica-7] <- 8 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica-14] <- 5
									FinSi
								FinSi
							FinSi
					FinSegun
				FinSi
			FinSi
			Si posLogica - 9 > 0 Entonces
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-9,8)
				Si ConvertirANumero(posicionCalculada[3])+2 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica-9]
						0:
							tableroLogico[posLogica-9] <- 5
						1,3:
							
						2,4:
							Si posLogica-18 > 0 Entonces
								Si tableroLogico[posLogica-18] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-18,8)
									Si ConvertirANumero(posicionCalculada[3])+3 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica-9] = 2 Entonces
											tableroLogico[posLogica-9] <- 6 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica-9] <- 8 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica-18] <- 5
									FinSi
								FinSi
							FinSi
					FinSegun
				FinSi
			FinSi
			//Primer condicion para no hacer overflow en el vector del tablero logico, si corresponde coloca un 5 en el lugar donde van los movimientos permitidos
			Si posLogica + 7 < 65 Entonces
				CalcularPosicionEnTablero(posicionCalculada, posLogica,8)
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+7,8)
				Si ConvertirANumero(posicionCalculada[3]) <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica+7]
						0:
							tableroLogico[posLogica+7] <- 5
						1,3:
							Si posLogica+14 < 65 Entonces
								Si tableroLogico[posLogica+14] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+14,8)
									Si ConvertirANumero(posicionCalculada[3])-1 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica+7] = 2 Entonces
											tableroLogico[posLogica+7] <- 6 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica+7] <- 8 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica+14] <- 5
									FinSi
								FinSi
							FinSi
						2,4:
					FinSegun
				FinSi
			FinSi
			Si posLogica + 9 < 65 Entonces
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+9,8)
				Si ConvertirANumero(posicionCalculada[3])-2 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica+9]
						0:
							tableroLogico[posLogica+9] <- 5
						1,3:
							Si posLogica+18 < 65 Entonces
								Si tableroLogico[posLogica+18] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+18,8)
									Si ConvertirANumero(posicionCalculada[3])-3 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica+9] = 2 Entonces
											tableroLogico[posLogica+9] <- 6 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica+9] <- 8 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica+18] <- 5
									FinSi
								FinSi
							FinSi
						2,4:
					FinSegun
				FinSi
			FinSi
		//DAMA NEGRA
		4:
			//Primer condicion para no hacer overflow en el vector del tablero logico, si corresponde coloca un 5 en el lugar donde van los movimientos permitidos
			Si posLogica - 7 > 0 Entonces
				CalcularPosicionEnTablero(posicionCalculada, posLogica,8)
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-7,8)
				Si ConvertirANumero(posicionCalculada[3]) <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica-7]
						0:
							tableroLogico[posLogica-7] <- 5
						1,3:
							Si posLogica-14 > 0 Entonces
								Si tableroLogico[posLogica-14] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-14,8)
									Si ConvertirANumero(posicionCalculada[3])+1 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica-7] = 1 Entonces
											tableroLogico[posLogica-7] <- 7 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica-7] <- 9 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica-14] <- 5
									FinSi
								FinSi
							FinSi
							
						2,4:
					FinSegun
				FinSi
			FinSi
			Si posLogica - 9 > 0 Entonces
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-9,8)
				Si ConvertirANumero(posicionCalculada[3])+2 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica-9]
						0:
							tableroLogico[posLogica-9] <- 5
						1,3:
							Si posLogica-18 > 0 Entonces
								Si tableroLogico[posLogica-18] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica-18,8)
									Si ConvertirANumero(posicionCalculada[3])+3 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica-9] = 1 Entonces
											tableroLogico[posLogica-9] <- 7 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica-9] <- 9 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica-18] <- 5
									FinSi
								FinSi
							FinSi
						2,4:
					FinSegun
				FinSi
			FinSi
			//Primer condicion para no hacer overflow en el vector del tablero logico, si corresponde coloca un 5 en el lugar donde van los movimientos permitidos
			Si posLogica + 7 < 65 Entonces
				CalcularPosicionEnTablero(posicionCalculada, posLogica,8)
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+7,8)
				Si ConvertirANumero(posicionCalculada[3]) <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica+7]
						0:
							tableroLogico[posLogica+7] <- 5
						1,3:
							Si posLogica+14 < 65 Entonces
								Si tableroLogico[posLogica+14] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+14,8)
									Si ConvertirANumero(posicionCalculada[3])-1 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica+7] = 1 Entonces
											tableroLogico[posLogica+7] <- 7 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica+7] <- 9 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica+14] <- 5
									FinSi
								FinSi
							FinSi
						2,4:
					FinSegun
				FinSi
			FinSi
			Si posLogica + 9 < 65 Entonces
				CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+9,8)
				Si ConvertirANumero(posicionCalculada[3])-2 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
					Segun tableroLogico[posLogica+9]
						0:
							tableroLogico[posLogica+9] <- 5
						1,3:
							Si posLogica+18 < 65 Entonces
								Si tableroLogico[posLogica+18] = 0 Entonces
									CalcularPosicionEnTablero(posicionCalculadaPosible, posLogica+18,8)
									Si ConvertirANumero(posicionCalculada[3])-3 <> ConvertirANumero(posicionCalculadaPosible[3]) Entonces
										Si tableroLogico[posLogica+9] = 1 Entonces
											tableroLogico[posLogica+9] <- 7 //MARCA LA FICHA QUE SE VA A ELIMINAR
										SiNo
											tableroLogico[posLogica+9] <- 9 //MARCA LA FICHA QUE SE VA A ELIMINAR
										FinSi
										tableroLogico[posLogica+18] <- 5
									FinSi
								FinSi
							FinSi
						2,4:
					FinSegun
				FinSi
			FinSi
		De Otro Modo:
	Fin Segun
FinFuncion

Funcion res <- ValidaSiPosicionCorrespondeATurno (tableroLogico,posLogica,turno)
	
	//Valida que la ficha ingresada sea una ficha que corresponde al turno
	res <- -30
	Segun tableroLogico[posLogica] Hacer
		1,3:
			Si turno = "blancas" Entonces
				res <- posLogica
			SiNo
				Escribir "La posición ingresada no corresponde a una ficha ", turno 
			FinSi
		2,4:
			Si turno = "negras" Entonces
				res <- posLogica
			SiNo
				Escribir "La posición ingresada no corresponde a una ficha ", turno 
			FinSi
		5:
			Si turno = "movimientoPermitido" Entonces
				res <- posLogica
			SiNo
				Escribir "La posición ingresada no corresponde a una ficha, sino que corresponde a un movimiento permitido"
			FinSi
		De Otro Modo:
			Si turno = "movimientoPermitido" Entonces
				Escribir "La posición ingresada no corresponde a un movimiento permitido"
			SiNo
				Escribir "La ficha ingresada no existe"
			FinSi
	Fin Segun
FinFuncion
//Valida la entrada del usuario y la descompone en 2 posiciones (inicial -> final)
Funcion res <- ControlDeIngresoUsuario(tableroGrafico,tableroLogico,entradaUsuario,turno)
	
	Definir posLogica Como Entero
	Si Longitud(entradaUsuario) = 3 Entonces
		//Escribir "Posicion : ", entradaUsuario
		posLogica <- ConvertirPosicionGraficaALogica(Mayusculas(entradaUsuario))
		Si posLogica > 0 Entonces
			//Escribir "posLogica de controldeingresodeusuario: ", posLogica
			res <- ValidaSiPosicionCorrespondeATurno(tableroLogico,posLogica,turno)
			//Escribir "res de controldeingresodeusuario: ", res
		SiNo
			Escribir "Posición inexistente"
			res <- -40
		FinSi
	SiNo
		Escribir "Las posición no ha sido ingresada correctamente"
		res <- -50
	FinSi
FinFuncion

//Valida ganador de la partida
Funcion ganador <- VerificaGanador (tableroLogico Por Referencia, cantCuadradosFilaYColum, turnoActual)
	Definir gana Como Logico
	gana <- Verdadero
	//Recorre el tablero logico verificando si las piezas del contrario fueron consumidas totalmente
	Para i <- 1 Hasta cantCuadradosFilaYColum*cantCuadradosFilaYColum Con Paso 1 Hacer
		Si turnoActual = "negras" Entonces
			Si tableroLogico[i] = 1 O tableroLogico[i] = 3 Entonces
				gana <- Falso
			FinSi
		SiNo
			Si tableroLogico[i] = 2 O tableroLogico[i] = 4 Entonces
				gana <- Falso
			FinSi
		Fin si
		//Imprimir "Valor ", i,"- ",tableroLogico[i]
	FinPara
	ganador <- gana
FinFuncion

Funcion CorrerJuegoD (tableroGrafico Por Referencia,tableroLogico Por Referencia,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
	Definir salir, volver,repiteTurno Como Logico
	
	Definir entrada Como Caracter
	Definir turno Como Caracter
	Definir posLogicaInicial, posLogicaFinal Como Entero
	
	salir <- Falso
	volver <- Falso
	repiteTurno <- Falso
	//Empieza en negras para respetar el inicio de blancas
	turno <- "negras"
	Mientras salir <> Verdadero Hacer
		
		Si volver <> Verdadero Y repiteTurno <> Verdadero Entonces
			turno <- CambiaTurno(turno)
		SiNo //Si es una vuelta atras
			
			volver <- Falso
			repiteTurno <- Falso
			
			TableroInicial(tableroGrafico,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
			
			BorrarMovimientos(tableroLogico,cantCuadradosFilaYColum)
			//Mete las fichas en el tablero grafico
			GraficarFichasDesdeLogicoAGrafico(tableroGrafico,tableroLogico,cantCuadradosFilaYColum)
			
		FinSi
		Limpiar Pantalla
		ImprimirTablero(tableroGrafico,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
		
		Escribir "Juegan ", turno,", ingrese la posición de la ficha que desea mover o -1 para salir"
		Leer entrada
		
		Si entrada = "-1" Entonces
			salir <- Verdadero
		SiNo
			posLogicaInicial <- ControlDeIngresoUsuario(tableroGrafico,tableroLogico,entrada,turno)
			Si posLogicaInicial > 0 Entonces
				CargaOpcionesDeMovimiento(tableroGrafico,tableroLogico,posLogicaInicial)
				
				TableroInicial(tableroGrafico,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
				
				//Mete las fichas en el tablero grafico
				GraficarFichasDesdeLogicoAGrafico(tableroGrafico,tableroLogico,cantCuadradosFilaYColum)
				
				Limpiar Pantalla
				ImprimirTablero(tableroGrafico,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
				//Para debug descomentar la siguiente linea
				//ImprimirTableroLogico(tableroLogico,cantCuadradosFilaYColum)
				Escribir "Ingrese la posición donde desea mover la ficha o -1 para volver a seleccionar"
				Leer entrada
				
				Si entrada = "-1" Entonces
					volver <- Verdadero
				SiNo
					posLogicaFinal <- ControlDeIngresoUsuario(tableroGrafico,tableroLogico,entrada,"movimientoPermitido")
					Si posLogicaFinal > 0 Entonces
						VerificaSiHayCoronacion(tableroLogico,posLogicaInicial, posLogicaFinal)
						repiteTurno <- EjecutaMovimientoDeFicha(tableroLogico,posLogicaInicial, posLogicaFinal)
						
						BorrarMovimientos(tableroLogico,cantCuadradosFilaYColum)
						
						TableroInicial(tableroGrafico,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
						
						//Mete las fichas en el tablero grafico
						GraficarFichasDesdeLogicoAGrafico(tableroGrafico,tableroLogico,cantCuadradosFilaYColum)
					SiNo
						volver <- Verdadero
						Escribir "Presione cualquier tecla para repetir el turno"
						Esperar Tecla
					FinSi
				FinSi
			SiNo
				volver <- Verdadero
				Escribir "Presione cualquier tecla para repetir el turno"
				Esperar Tecla
			FinSi
			
		FinSi
		
		Si VerificaGanador(tableroLogico,cantCuadradosFilaYColum,turno) = Verdadero Entonces
			
			Escribir "     # # #    # # # #     # # # #    # # #    # # # #    # # # # #   "
			Escribir "   #       #  #      #   #         #       #  #      #   #           "
			Escribir "   #       #  #      #  #          #       #  #       #  # # #       "
			Escribir "   # # # # #  # # # #   #          # # # # #  #       #  #           "
			Escribir "   #       #  #    #     #         #       #  #      #   #           "
			Escribir "   #       #  #      #    # # # #  #       #  # # # #    # # # # #   "
			Escribir "                                                                     "
			Escribir "  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  "
			Escribir "#                                                                   #"
			Escribir "#     _________________________________________________________     #"
			Escribir "#   /                                                           \   #"
			Escribir "#  |   * - - - - - - - - - - - - - - - - - - - - - - - - - - *   |  #"
			Escribir "#  |  |                :::::::::::::::::::::::                |  |  #"
			Escribir "#  |  |                ::   GANADOR DAMAS   ::                |  |  #"
			Escribir "#  |  |                :::::::::::::::::::::::                |  |  #"
			Escribir "#  |  |                                                       |  |  #"
			Escribir "#  |  |                                                       |  |  #"
			si turno = "negras" Entonces
				Escribir "#  |  |                       NEGRAS                          |  |  #"
				
			SiNo
				Escribir "#  |  |                       BLANCAS                         |  |  #"
				
			FinSi
			Escribir "#  |  |                                                       |  |  #"
			Escribir "#  |  |     (PRESIONE CUALQUIER TECLA PARA VOLVER AL MENÚ)    |  |  #"
			Escribir "#  |  |                                                       |  |  #"
			Escribir "#  |  |                                                       |  |  #"
			Escribir "#  |  |                                                       |  |  #"
			Escribir "#  |   * - - - - - - - - - - - - - - - - - - - - - - - - - - *   |  #"
			Escribir "#  |                                                             |  #"
			Escribir "#  |                                                             |  #"
			Escribir "#  |        :: :: :: ::                                          |  #"
			Escribir "#  |      :: . * - * . ::                        :::::           |  #"
			Escribir "#  |    ::   |  /W\  |   ::                    ::: _ :::         |  #"
			Escribir "#  |   :: *- *   |   * -* ::                  :: | E | ::        |  #"
			Escribir "#  |   :: | <A- ( ) -D> | ::                   ::: - :::         |  #"
			Escribir "#  |   :: *- *   |   * -* ::             :::::   :::::           |  #"
			Escribir "#  |    ::   |  \S/  |   ::            ::: _ :::                 |  #"
			Escribir "#  |      :: . * - * . ::             :: | F | ::                |  #"
			Escribir "#  |        :: :: :: ::                ::: - :::                 |  #"
			Escribir "#  |                                     :::::                   |  #"
			Escribir "#  |                                                             |  #"
			Escribir "#   \ _________________________________________________________ /   #"
			Escribir "#                                                                   #"
			Escribir "  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  "
			Esperar Tecla
			salir <- Verdadero
		FinSi
		
	Fin Mientras
FinFuncion


//Principal damas
Funcion JuegoDamas
	//Definiciones y asignaciones iniciales
	Definir cantPixeles,cantPixelesColumna,cantPixelesFila,cantCuadradosFilaYColum Como Entero
	cantPixelesColum <- 9
	cantPixelesFila <- 5
	cantCuadradosFilaYColum <- 8
	cantPixeles <- cantPixelesColum*cantPixelesFila*(cantCuadradosFilaYColum*cantCuadradosFilaYColum)
	Dimension tableroGrafico[cantPixeles]
	
	//Definiciones tablero logico
	Dimension tableroLogico[cantCuadradosFilaYColum*cantCuadradosFilaYColum]
	
	//Llamado a tablero logico
	CargarTableroLogico(tableroLogico,cantCuadradosFilaYColum)
	//ImprimirTableroLogico(tableroLogico,cantCuadradosFilaYColum)
	
	//Llamado a funciones para precarga y muestra
	TableroInicial(tableroGrafico,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
	
	//Mete las fichas en el tablero grafico
	GraficarFichasDesdeLogicoAGrafico(tableroGrafico,tableroLogico,cantCuadradosFilaYColum)
	
	CorrerJuegoD(tableroGrafico,tableroLogico,cantPixelesColum,cantPixelesFila,cantCuadradosFilaYColum)
	Limpiar Pantalla
FinFuncion






//
//
//				2048!!!
//
//


//Muestra el tablero del sistema
Funcion ImprimirElTablero(sistema)
	Escribir "| " sistema[1,1], " | ", sistema[1,2], " | ", sistema[1,3], " | ", sistema[1,4]
    Escribir  "-----------------"
    Escribir "| " sistema[2,1], " | ", sistema[2,2], " | ", sistema[2,3], " | ", sistema[2,4]
    Escribir  "-----------------"
    Escribir "| " sistema[3,1], " | ", sistema[3,2], " | ", sistema[3,3], " | ", sistema[3,4]
    Escribir  "-----------------"
    Escribir "| " sistema[4,1], " | ", sistema[4,2], " | ", sistema[4,3], " | ", sistema[4,4]
	
FinFuncion

//Agrega los dos valores iniciales al juego
Funcion InicioDelTablero(sistema Por Referencia,tablero Por Referencia)
	definir fila Como Entero
	definir columna Como Entero
    Para fila <- 1 Hasta 4 con paso 1 Hacer            
        Para columna <- 1 Hasta 4 con paso 1 Hacer     
            sistema[fila,columna] <- 0                    
            tablero[fila,columna] <- " "
        Fin Para
    Fin Para
Fin Funcion

// Busca un lugar vacio dentro de la matriz (esto es unicamente para el inicio del juego)
funcion valoresinicialestablero(sistema Por Referencia)
	Para k Desde 1 Hasta 2 Con Paso 1 Hacer
        Repetir
            fila <- Aleatorio(1,4)
            columna <- Aleatorio(1,4)
        Hasta Que sistema[fila,columna] = 0
        Si Aleatorio(0, 1) = 0 Entonces 
            sistema[fila, columna] <- 2
        Sino
            sistema[fila, columna] <- 4
        Fin Si
    Fin Para
FinFuncion

//Imprime la visual del juego sin los ceros, con espacios vacios
Funcion imprimirvisual(tablero,sistema)
	
	definir fila Como Entero
	definir columna Como Entero
	
    Para fila <- 1 Hasta 4 con paso 1 Hacer            
        Para columna <- 1 Hasta 4 con paso 1 Hacer     
			segun sistema[fila,columna] hacer                    
			    0: 
					tablero[fila,columna] <- "  0 "
			    2:
					tablero[fila,columna] <- "  2 "
			    4:
					tablero[fila,columna] <- "  4 "
			    8:
					tablero[fila,columna] <- "  8 "
				16:
					tablero[fila,columna] <- " 16 "
				32:
					tablero[fila,columna] <- " 32 "
				64:
					tablero[fila,columna] <- " 64 "
				128:
					tablero[fila,columna] <- " 128"
				256:
					tablero[fila,columna] <- " 256"
				512:
					tablero[fila,columna] <- " 512"
				1024:
					tablero[fila,columna] <- "1024"
				2048:
					tablero[fila,columna] <- "2048"
			FinSegun
		FinPara
	FinPara
	
	
	Escribir "                   #####     #####        ###    #####                "
	Escribir "                  #     #   #     #      #  #   #     #               "
	Escribir "                       #    #     #     #   #   #     #               "
	Escribir "                      #     #     #    #    #     ###                 "
	Escribir "                     #      #     #   # # # #   #     #               "
	Escribir "                    #       #     #         #   #     #               "
	Escribir "                  #######    #####          #    #####                "
	Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
	Escribir " #                #                #                #                #"
	Escribir " #                #                #                #                #"
	Escribir " #      " tablero[1,1], "      #      " tablero[1,2], "      #      " tablero[1,3], "      #      " tablero[1,4], "      #"
	Escribir " #                #                #                #                #"
	Escribir " #                #                #                #                #"
	Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
	Escribir " #                #                #                #                #"
	Escribir " #                #                #                #                #"
	Escribir " #      " tablero[2,1], "      #      " tablero[2,2], "      #      " tablero[2,3], "      #      " tablero[2,4], "      #"
	Escribir " #                #                #                #                #"
	Escribir " #                #                #                #                #"
	Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
	Escribir " #                #                #                #                #"
	Escribir " #                #                #                #                #"
	Escribir " #      " tablero[3,1], "      #      " tablero[3,2], "      #      " tablero[3,3], "      #      " tablero[3,4], "      #"
	Escribir " #                #                #                #                #"
	Escribir " #                #                #                #                #"
	Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
	Escribir " #                #                #                #                #"
	Escribir " #                #                #                #                #"
	Escribir " #      " tablero[4,1], "      #      " tablero[4,2], "      #      " tablero[4,3], "      #      " tablero[4,4], "      #"
	Escribir " #                #                #                #                #"
	Escribir " #                #                #                #                #"
	Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
	Escribir "                                                                      "
	Escribir "    #  #  #                      #####                                "
	Escribir "    #  F  # --> EXIT            #  W  #--> UP                         "
	Escribir "    #  #  #                ##### ##### #####                          "
	Escribir "                  LEFT <--#  A  #  S  #  D  #--> RIGHT                "
	Escribir "                           ##### ##### #####                          "
	Escribir "                                 DOWN                                 "
	
FinFuncion

// agrega un valor entre 2 o 4 en algun espacio vacio
funcion agregarnumeroeneljuego(sistema Por Referencia) 
	definir validacion Como entero
	validacion <- 0
	para fila <- 1 Hasta 4 Con Paso 1 hacer
		para columna <- 1 Hasta 4 Con Paso 1 Hacer
			si sistema[fila, columna] = 0 Entonces 
				validacion <- validacion + 1
			FinSi
		FinPara
    FinPara
	
	si validacion > 0 Entonces
		Repetir
            fila <- Aleatorio(1,4)
            columna <- Aleatorio(1,4)
        Hasta Que sistema[fila,columna] = 0
		Si Aleatorio(0, 1) = 0 Entonces
			sistema[fila, columna] <- 2
		Sino
			sistema[fila, columna] <- 4
		Fin Si
	FinSi
FinFuncion

//Movimiento hacia la izquierda
Funcion MoverIzquierda(sistema Por Referencia)
    Para fila <- 1 Hasta 4 con paso 1 Hacer
        aux <- 1
        Para columna <- 1 Hasta 4  con paso 1 Hacer
            Si sistema[fila, columna] <> 0 Entonces
                Si aux > 1 Y sistema[fila, aux - 1] = sistema[fila, columna] Entonces
                    sistema[fila, aux - 1] <- sistema[fila, aux - 1] * 2
                    sistema[fila, columna] <- 0
                Sino
                    sistema[fila, aux] <- sistema[fila, columna]
                    Si columna <> aux Entonces
                        sistema[fila, columna] <- 0
                    Fin Si
                    aux <- aux + 1
                Fin Si
            Fin Si
        Fin Para
    Fin Para
Finfuncion
//Movimiento hacia la derecha
funcion MoverDerecha(sistema Por Referencia)
    Para fila <- 1 Hasta 4 con paso 1 Hacer
        aux <- 4
        Para columna <- 4 Hasta 1 Con Paso -1 Hacer
            Si sistema[fila, columna] <> 0 Entonces
                Si aux < 4 Y sistema[fila, aux + 1] = sistema[fila, columna] Entonces
                    sistema[fila, aux + 1] <- sistema[fila, aux + 1] * 2
                    sistema[fila, columna] <- 0
                Sino
                    sistema[fila, aux] <- sistema[fila, columna]
                    Si columna <> aux Entonces
                        sistema[fila, columna] <- 0
                    Fin Si
                    aux <- aux - 1
                Fin Si
            Fin Si
        Fin Para
    Fin Para
Finfuncion
//Movimiento para abajo
funcion MoverAbajo(sistema Por Referencia)
    Para columna Desde 1 Hasta 4 Hacer
		aux <- 4
        Para fila <- 4 Hasta 1 con Paso -1 Hacer
            Si sistema[fila, columna] <> 0 Entonces
                Si aux < 4 Y sistema[aux + 1, columna] = sistema[fila, columna] Entonces
                    sistema[aux + 1, columna] <- sistema[aux + 1, columna] * 2
                    sistema[fila, columna] <- 0
                Sino
                    sistema[aux, columna] <- sistema[fila, columna]
                    Si fila <> aux Entonces
                        sistema[fila, columna] <- 0
                    Fin Si
                    aux <- aux - 1
                Fin Si
            Fin Si
        Fin Para
    Fin Para
Finfuncion
//Movimiento para arriba
Funcion  MoverArriba(sistema Por Referencia)
    Para columna <- 1 Hasta 4 con paso 1 Hacer
		aux <- 1
        Para fila <- 1 Hasta 4 con paso 1 hacer
            Si sistema[fila, columna] <> 0 Entonces
                Si aux > 1 Y sistema[aux - 1, columna] = sistema[fila, columna] Entonces
                    sistema[aux - 1, columna] <- sistema[aux - 1, columna] * 2
                    sistema[fila, columna] <- 0
                Sino
                    sistema[aux, columna] <- sistema[fila, columna]
                    Si fila <> aux Entonces
                        sistema[fila, columna] <- 0
                    Fin Si
                    aux <- aux + 1
                Fin Si
            Fin Si
        Fin Para
    Fin Para
FinFuncion

funcion perdiste(sistema, perder Por referencia)
	perder <- 1
	Para columna <- 1 Hasta 4 con paso 1 Hacer
		Para fila <- 1 Hasta 4 con paso 1 hacer
			Si sistema[fila, columna] = 0 Entonces
				perder <- 0 
			FinSi
			Si columna < 4 Y sistema[fila, columna] = sistema[fila, columna + 1] Entonces
				perder <- 0
			FinSi
			Si fila < 4 Y sistema[fila, columna] = sistema[fila + 1, columna] Entonces
				perder <- 0
			FinSi
		FinPara
	FinPara
FinFuncion

funcion ganar(sistema, ganaste Por Referencia)
	Para fila Desde 1 Hasta 4 Hacer
        Para columna Desde 1 Hasta 4 Hacer
            Si sistema[fila, columna] = 2048 Entonces
                ganaste <- 1
			SiNo
				ganaste <- 0
            Fin Si
        Fin Para
    Fin Para
FinFuncion

Funcion Juego2048
	definir ingreso Como Caracter
	definir perder Como Entero
	definir ganaste Como entero
	
    Dimension sistema[4,4]
    Dimension tablero[4,4]
	
	InicioDelTablero(sistema,tablero)
	valoresinicialestablero(sistema)
	imprimirvisual(tablero,sistema)
	Repetir
		perdiste(sistema,perder)
		si perder = 0 Entonces
			Leer ingreso
			Segun Minusculas(ingreso) Hacer
				"a":
					MoverIzquierda(sistema)
					agregarnumeroeneljuego(sistema)
					Borrar Pantalla
					imprimirvisual(tablero,sistema)
				"s":
					MoverAbajo(sistema)
					agregarnumeroeneljuego(sistema)
					Borrar Pantalla
					imprimirvisual(tablero,sistema)
				"d":
					MoverDerecha(sistema)
					agregarnumeroeneljuego(sistema)
					Borrar Pantalla
					imprimirvisual(tablero,sistema)
				"w":
					MoverArriba(sistema)
					agregarnumeroeneljuego(sistema)
					Borrar Pantalla
					imprimirvisual(tablero,sistema)
				"f":
				De Otro Modo:
					Borrar Pantalla
					imprimirvisual(tablero,sistema)
			Fin Segun
		FinSi
		ganar(sistema, ganaste)
		si ganaste = 1 Entonces
			Borrar Pantalla
			Escribir "                   #####     #####        ###    #####                "
			Escribir "                  #     #   #     #      #  #   #     #               "
			Escribir "                       #    #     #     #   #   #     #               "
			Escribir "                      #     #     #    #    #     ###                 "
			Escribir "                     #      #     #   # # # #   #     #               "
			Escribir "                    #       #     #         #   #     #               "
			Escribir "                  #######    #####          #    #####                "
			Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
			Escribir " #                                                                   #"
			Escribir " #    :)  :)  :)  :)  :)  :)  :)  :)  :)  :)  :)  :)  :)  :)  :)     #"
			Escribir " #                                                                   #"
			Escribir " #     ##    ##                                                      #"
			Escribir " #      ##  ##   # ## #  ##    ##    ##    :)    ## ## ##     ##     #"
			Escribir " #       ####   ##    ## ## :) ##     ##        ##  ## ####   ##     #"
			Escribir " #        ##    ##    ## ##    ##      ##  ##  ##   ## ## ##  ##     #"
			Escribir " #   :)   ##    ##    ## ##    ##       ## ## ##    ## ##  ## ##     #"
			Escribir " #        ##     # ## #   # ## #         ##  ##     ## ##   ####     #"
			Escribir " #          :)                     :)                                #"
			Escribir " #                                                      :)           #"
			Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
			Escribir "                                                                      "
			Escribir "    #  #  #                      #####                                "
			Escribir "    #  F  # --> EXIT            #  W  #--> UP                         "
			Escribir "    #  #  #                ##### ##### #####                          "
			Escribir "                  LEFT <--#  A  #  S  #  D  #--> RIGHT                "
			Escribir "                           ##### ##### #####                          "
			Escribir "                                 DOWN                                 "
			perder <- 1 
			esperar 3 Segundos 
		FinSi
		si perder = 1 Entonces
			Borrar Pantalla
			Escribir "                   #####     #####        ###    #####                "
			Escribir "                  #     #   #     #      #  #   #     #               "
			Escribir "                       #    #     #     #   #   #     #               "
			Escribir "                      #     #     #    #    #     ###                 "
			Escribir "                     #      #     #   # # # #   #     #               "
			Escribir "                    #       #     #         #   #     #               "
			Escribir "                  #######    #####          #    #####                "
			Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
			Escribir " #        :(                    :(                                   #"
			Escribir " #                                               :(           :(     #"
			Escribir " #        ##        # ## ## #     ## ## #  ## ## ##  # ## ##         #"
			Escribir " #  :(    ##       ##       ##  ##         ##       ##     ##        #"
			Escribir " #        ##       ##       ##  ##         ## ##    ##     ##   :(   #"
			Escribir " #        ##       ##  :(   ##    ## ##    ##       ##   ##          #"
			Escribir " #  :(    ##       ##       ##         ##  ##       ##    ##    :(   #"
			Escribir " #        ## ## ##  # ## ## #   ## ## ##   ## ## ## ##     ##        #"
			Escribir " #                                                                   #"
			Escribir " #        :(  :(  :(  :(  :(  :(  :(  :(  :(  :(  :(  :(  :(         #"
			Escribir " #                                                                   #"
			Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " #                #                #                #                #"
			Escribir " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
			Escribir "                                                                      "
			Escribir "    #  #  #                      #####                                "
			Escribir "    #  F  # --> EXIT            #  W  #--> UP                         "
			Escribir "    #  #  #                ##### ##### #####                          "
			Escribir "                  LEFT <--#  A  #  S  #  D  #--> RIGHT                "
			Escribir "                           ##### ##### #####                          "
			Escribir "                                 DOWN                                 "
			esperar 3 Segundos 
		FinSi
	Hasta Que Minusculas(ingreso) = "f" o perder = 1
	Escribir "Fin del juego"
FinFuncion

Funcion Menu
	
	Escribir "      # # #    # # # #     # # # #    # # #    # # # #    # # # # #   "
	Escribir "    #       #  #      #   #         #       #  #      #   #           "
	Escribir "    #       #  #      #  #          #       #  #       #  # # #       "
	Escribir "    # # # # #  # # # #   #          # # # # #  #       #  #           "
	Escribir "    #       #  #    #     #         #       #  #      #   #           "
	Escribir "    #       #  #      #    # # # #  #       #  # # # #    # # # # #   "
	Escribir "                                                                      "
	Escribir "   # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  "
	Escribir " #                                                                   #"
	Escribir " #     _________________________________________________________     #"
	Escribir " #   /                                                           \   #"
	Escribir " #  |   * - - - - - - - - - - - - - - - - - - - - - - - - - - *   |  #"
	Escribir " #  |  |                :::::::::::::::::::::::                |  |  #"
	Escribir " #  |  |                ::SELECCIONE EL JUEGO::                |  |  #"
	Escribir " #  |  |                :::::::::::::::::::::::                |  |  #"
	Escribir " #  |  |                     :::::::::::::                     |  |  #"
	Escribir " #  |  |                     ::1 - Damas::                     |  |  #"
	Escribir " #  |  |                     :::::::::::::                     |  |  #"
	Escribir " #  |  |                     ::2 - 2048 ::                     |  |  #"
	Escribir " #  |  |                     :::::::::::::                     |  |  #"
	Escribir " #  |  |                     ::0 - Salir::                     |  |  #"
	Escribir " #  |  |                     :::::::::::::                     |  |  #"
	Escribir " #  |   * - - - - - - - - - - - - - - - - - - - - - - - - - - *   |  #"
	Escribir " #  |                                                             |  #"
	Escribir " #  |                                                             |  #"
	Escribir " #  |        :: :: :: ::                                          |  #"
	Escribir " #  |      :: . * - * . ::                        :::::           |  #"
	Escribir " #  |    ::   |  /W\  |   ::                    ::: _ :::         |  #"
	Escribir " #  |   :: *- *   |   * -* ::                  :: | E | ::        |  #"
	Escribir " #  |   :: | <A- ( ) -D> | ::                   ::: - :::         |  #"
	Escribir " #  |   :: *- *   |   * -* ::             :::::   :::::           |  #"
	Escribir " #  |    ::   |  \S/  |   ::            ::: _ :::                 |  #"
	Escribir " #  |      :: . * - * . ::             :: | F | ::                |  #"
	Escribir " #  |        :: :: :: ::                ::: - :::                 |  #"
	Escribir " #  |                                     :::::                   |  #"
	Escribir " #  |                                                             |  #"
	Escribir " #   \ _________________________________________________________ /   #"
	Escribir " #                                                                   #"
	Escribir "   # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  "
FinFuncion



Funcion Arcade
	Definir salir Como Entero
	salir <- 9999
	Mientras salir <> 0 
		Limpiar Pantalla
		Menu()
		Leer salir
		Segun salir
			1:
				Limpiar Pantalla
				JuegoDamas
			2:
				Limpiar Pantalla
				Juego2048
		FinSegun
	FinMientras
FinFuncion


Algoritmo Main
	Arcade
FinAlgoritmo




