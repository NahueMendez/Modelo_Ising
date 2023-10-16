#!/bin/bash

# Valores de temperatura
temperaturas=(0.5 0.8 1 1.5 2 2.1 2.2 2.3 2.4 2.5 3 4 5)

# Nombre del ejecutable
ejecutable="./ising"

# Itera sobre las temperaturas y ejecuta el programa
for temp in "${temperaturas[@]}"
do
	#.Hago cuatro corridas (tres de producción y una de termalizacion)
	for i in {1..4}
	do
    		echo "Modificando T.dat con temperatura: $temp"
    		echo "$temp" > T.dat
    		echo "Ejecutando $ejecutable con temperatura: $temp"
    		$ejecutable

     	# Renombra el archivo energy.dat a energy_$temp.dat
    		mv ./energy.dat "./resultados/$i-energy-$temp.dat"
     	# Renombra el archivo magnetizacion.dat a magnetizacion_$temp.dat
    		mv ./magnetizacion.dat "./resultados/$i-magnetizacion_$temp.dat"

     	# Renombra el archivo calor_especifico.dat a calor_especifico_$temp.dat
    		mv ./calor_especifico.dat "./resultados/$i-calor_especifico_$temp.dat"

     	# Renombra el archivo susceptibilidad.dat a susceptibilidad_$temp.dat
    		mv ./susceptibilidad.dat "./resultados/$i-susceptibilidad_$temp.dat"
	done
done
