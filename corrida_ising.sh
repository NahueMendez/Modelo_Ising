#!/bin/bash

# Valores de temperatura
temperaturas=(0.5 0.65 0.8 1 1.5 1.6 1.7 1.8 1.9 2 2.1 2.125 2.15 2.175 2.2 2.225 2.25 2.275 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3 4 5)
# Nombre del ejecutable
ejecutable="./ising"

# Itera sobre las temperaturas y ejecuta el programa
for temp in "${temperaturas[@]}"
do
	#.Hago cuatro corridas (tres de producción y una de termalizacion)
	for i in {1..4}
	do
    		echo "Modificando T.dat con temperatura: $temp"
    		echo "$temp" > temp.dat
    		echo "Ejecutando $ejecutable con temperatura: $temp"
    		#.Elimino matriz.dat asi inicializa aleatoriamente
		#[ -e ./matriz.dat ] && rm ./matriz.dat
		$ejecutable

     	# Renombra el archivo de energía media
    		mv ./E.dat "./resultados/$i-E-$temp.dat"

     	# Renombra el archivo magnetizacion media
    		mv ./Mag.dat "./resultados/$i-Mag_$temp.dat"

     	# Renombra el archivo de energia cuadratica media
    		mv ./E2.dat "./resultados/$i-E2_$temp.dat"

     	# Renombra el archivo de magnetizacion cuadratica media
    		mv ./Mag2.dat "./resultados/$i-Mag2_$temp.dat"

     	# Renombra el archivo calor especifico
    		mv ./CV.dat "./resultados/$i-CV_$temp.dat"

     	# Renombra el archivo susceptibilidad
    		mv ./chi.dat "./resultados/$i-chi_$temp.dat"
	done
done
