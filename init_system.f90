subroutine init_system(n,m,initial_system)
        use ziggurat
        implicit none
        integer :: i,j
        real(kind=8) :: x,y
        !. n es el numero de filas y "m" el de columnas de la matriz
        integer, intent(in) :: n
        integer,intent(in) :: m
        !. Las variables que obtendré
        real(kind=8), intent(out) :: initial_system(n,m)
        !. Inicializo la matriz como nula
        initial_system = 0
        do i=1,n
           do j=1,m
                 if (uni()>0.5) then
                         initial_system(i,j)=1.0
                 else
                         initial_system(i,j)=-1.0
                end if
           end do
       end do
end subroutine init_system
