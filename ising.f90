program ising_mc 
    use ziggurat
    implicit none
    logical :: es1,es2,es3,es4
    integer :: seed,steps,i,x,y
    integer,parameter :: n=20,m=20
    real(kind=8),parameter :: kb = 8.6173303**(-5)
    real(kind=8) :: sistema_mu(n,m),sistema_v(n,m)
    real(kind=8) :: b,T,deltaE
![NO TOCAR] Inicializa generador de número random
!------------------------------------------------------------------------------------------------------------

    inquire(file='seed.dat',exist=es1)
    if(es1) then
        open(unit=10,file='seed.dat',status='old')
        read(10,*) seed
        close(10)
        print *,"  * Leyendo semilla de archivo seed.dat"
    else
        seed = 24583490
    end if

    call zigset(seed)
!FIN [NO TOCAR]
!............................................................................................................

!------------------------------------------------------------------------------------------------------------
! Leo el número de pasos de MC de un archivo externo
!------------------------------------------------------------------------------------------------------------

    inquire(file='steps.dat',exist=es2)
    if(es2) then
        open(unit=20,file='steps.dat',status='old')
        read(20,*) steps
        close(20)
        print *,"  * Leyendo número de pasos de steps.dat"
    else
        steps = 2000
        print *,"  * Archivo steps.dat no encontrado. Por defecto se toman 2000 pasos."
    end if
    
    print *,"  * Número de pasos:", steps
!..........................................................................................................   
   
    inquire(file='temp.dat',exist=es3)
    if(es3) then
        open(unit=30,file='temp.dat',status='old')
        read(30,*) T
        close(30)
        print *,"  * Leyendo temperatura de archivo temp.dat"
    else
        T = 1
    end if
!------------------------------------------------------------------------------------------------------------
! 1°paso: Inicializo una matriz de nxm que representa nuestro sistema
!------------------------------------------------------------------------------------------------------------
! Llamo a la subrutina para inicializar my_system
    call init_system(n, m, sistema_mu)
    
    inquire(file='matriz.dat',exist=es4)
    if(es4) then
        open(unit=40,file='matriz.dat',status='old')
        read(40,*) sistema_mu
        close(40)
        print *,"  * Leyendo matriz inicial de archivo matriz.dat"
    else
        call init_system(n, m, sistema_mu)
        print *, "  * Archivo matriz.dat no encontrado. Inicializando.."
    end if
    print *, "  * Sistema de 20x20 inicializado "
    
    sistema_v=sistema_mu
 !-----------------------------------------------------------------------------------------------------------
 ! 2° paso: Hago el loop de MonteCarlo
 !----------------------------------------------------------------------------------------------------------- 
    do i = 1, steps
    !.Selecciono una fila y una columna al azar
       x = nint(uni()*m)
       y = nint(uni()*n)
       !.Planteo el spinflip       
       sistema_v(x,y)=-1*sistema_mu(x,y) 
       !.Acá viene el calculo de diferencia de energias (me falta hacer la subrutina)
        call delta_energy(sistema_mu,x,y,n,m,1,deltaE)
        print *, deltaE
    end do



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

