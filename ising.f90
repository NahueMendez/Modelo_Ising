program ising_mc 
    use ziggurat
    implicit none
    logical :: es1,es2,es3
    integer :: seed,steps,i,x,y,ii,jj,j
    integer,parameter :: n=20,m=20
    real(kind=8),parameter :: kb = 1.0
    real(kind=8) :: sistema_mu(n,m)
    real(kind=8) :: b,T,deltaE,deltaM, Emed, E, Mag, Jis,r, Mmed, Emed_2, Mmed_2, cv, sus
![NO TOCAR] Inicializa generador de número random
!------------------------------------------------------------------------------------------------------------

    inquire(file='input.dat',exist=es1)
    if(es1) then
        open(unit=10,file='input.dat',status='old')
        read(10,*) seed
        read(10,*) steps 
        close(10)
        print *,"  * Leyendo semilla y N° pasos de archivo input.dat"
    else
        print *, " * Archivo de input no encontrado. Valores por defecto.."
        seed = 24583490
        steps = 10000
    end if

    call zigset(seed)
!FIN [NO TOCAR]
!............................................................................................................

!------------------------------------------------------------------------------------------------------------
! Leo el número de pasos de MC de un archivo externo
!------------------------------------------------------------------------------------------------------------
!..........................................................................................................   
   
    inquire(file='temp.dat',exist=es2)
    if(es2) then
        open(unit=30,file='temp.dat',status='old')
        read(30,*) T
        close(30)
        print *,"  * Leyendo temperatura de archivo temp.dat"
    else
        T = 1.0
    end if
! -------------------------------------------------------------------------------------------------------
! Doy valor a Jis, constante J para el cálculo de la energía

Jis = 1.0

!.........................................................................................................
!------------------------------------------------------------------------------------------------------------
! 1°paso: Inicializo una matriz de nxm que representa nuestro sistema
!------------------------------------------------------------------------------------------------------------
! Llamo a la subrutina para inicializar my_system
    call init_system(n, m, sistema_mu)
    
    inquire(file='matriz.dat',exist=es3)
    if(es3) then
        open(unit=40,file='matriz.dat',status='old')
        ! Lee la matriz desde el archivo
        do i = 1, n
                read(40, *) (sistema_mu(i, j), j = 1, m)
        end do
        close(40)
        print *,"  * Buscando matriz inicial de archivo"
    else
        call init_system(n, m, sistema_mu)
        print *, "  * Archivo matriz.dat no encontrado. Inicializando.."
    end if
    print *, "  * Sistema de 20x20 inicializado "

 !-----------------------------------------------------------------------------------------------------------
 ! 2° paso: Hago el loop de MonteCarlo
 !-----------------------------------------------------------------------------------------------------------

!. Calculo la energía inicial del sistema    
    call initial_energy(sistema_mu,Jis,n,m,E)    
    print *, "  * Energía inicial:", E

!. Calculo la magnetización inicial del sistema
    call initial_magnetization(sistema_mu, n, m, Mag)
    print *, "  * Magnetización incial:", Mag
    

!. Inicializo en cero los acumuladores de energía y magnetización y sus cuadráticas medias
    Emed = 0
    Mmed = 0
    Emed_2 = 0
    Mmed_2 = 0


!.Archivo para ir guardando la energía
    open(unit=50, file='E.dat',status='unknown')
!. Archivo para ir guardando la magnetización
    open(unit=70, file='Mag.dat', status='unknown')
!. Archivo para ir guardando la energia cuadratica media
    open(unit=80, file='E2.dat',status='unknown')
!. Archivo para ir guardando la magnetización cuadratica media
    open(unit=90, file='Mag2.dat', status='unknown')
!. Archivo para ir guardando el calor específico
    open(unit=100, file='CV.dat', status='unknown')
!. Archivo para ir guardando la susceptibilidad magnética
    open(unit=110, file='chi.dat', status='unknown')
    !.Loop de montecarlo
    do i = 1, steps
    !.Selecciono una fila y una columna al azar
        x = int(uni()*(n-1))+1
        y = int(uni()*(m-1))+1
       !.Planteo el spinflip       
       !.Acá viene el calculo de diferencia de energias (me falta hacer la subrutina)
        call delta_energy(sistema_mu,x,y,n,m,Jis,deltaE)
        !.Evaluo si acepto o no el nuevo estado
        !.Si la energía del nuevo estado es menor, la acepto:
        if (deltaE <=0) then
                ! Hago efectivo el spin flip
                sistema_mu(x,y)=-1*sistema_mu(x,y)
                ! Calculo la diferencia de magnetización
                deltaM = sistema_mu(x,y)*2
                !.Calculo energía y magnetización
                E = E+deltaE
                Mag = Mag +deltaM
        else
                r = uni()
                if (r<exp(-deltaE/(kb*T))) then
                        sistema_mu(x,y) = -1*sistema_mu(x,y)
                        deltaM=sistema_mu(x,y)*2
                        !.Calculo energía y magnetizacion del sistema
                        E = E + deltaE
                        Mag = Mag + deltaM        
                end if
        end if
        !. Calculo la energía del sistema
        Emed = Emed + E

        !. Calculo la energía cuadrática media del sistema
        Emed_2 = Emed_2 + E**2
        
        !. Calculo la magnetización del sistema
        Mmed = Mmed + Mag

        !. Calculo la magnetización cuadrática media del sistema
        Mmed_2 = Mmed_2 + Mag**2

        !. Calculo el calor específico
        cv = 1/(kb**2*T**2*m*n)*(Emed_2/real(i) - (Emed/real(i))**2)

        !. Calculo la susceptibilidad
        sus = 1/(kb*T)*(Mmed_2/real(i) - (Mmed/real(i)**2))
        !.Escribo a archivo la energia y la magnetización cada 1000 pasos
        if (MOD(steps,1000) == 0) then
                write(50,*) i,",",Emed/real(i)
                write(70,*) i, ", ", Mmed/real(i)
                write(80, *) i, ", ", Emed_2/real(i)
                write(90, *) i, ", ", Mmed_2/real(i)
                write(100,*) i, ", ", cv
                write(110,*) i, ", ", sus
        end if


     end do
     print *, "  * Fin de MC Loop"
     print *, "  * Cerrando archivo energia.dat, magnetizacion.dat, calor_especifico.dat, susceptibilidad.dat"
      !.Cierro archivos de energia, magnetización, calor específico y susceptibilidad  
      close(50)
      close(70)
      close(80)
      close(90)

      
      !.Guardo la matriz final
      print *, "  * Escribiendo matriz final del sistema"
      open(unit=60,file='matriz.dat',status='unknown')
      ! Escribe la matriz en el archivo
        do i = 1, n
                write(60, '(4F12.4)') (sistema_mu(i, j), j = 1, m)
        end do

      close(60)
      print *, "  * Corrida finalizada"
!! 
!! EDITAR AQUI 
!! 


!! 
!! FIN FIN edicion
!! 
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
         write(10,*) seed
        close(10)
![FIN no Tocar]        


end program ising_mc

