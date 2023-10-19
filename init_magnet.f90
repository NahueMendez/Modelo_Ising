subroutine initial_magnetization(sistema,n,m,Magnet)
    implicit none
    integer, intent(in) :: n, m
    real(kind=8), intent(in) :: sistema(n,m)
    integer :: i,j
    real(kind=8),intent(out) :: Magnet

    !.Inicializo la variable Magnet
    Magnet = 0 
    !.Sumo a lo largo de la matriz considerando condiciones periodicas de contorno
    do i=1,n
       do j=1, m
                Magnet = Magnet + sistema(i,j)
       end do  
    end do
end subroutine initial_magnetization
