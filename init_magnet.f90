subroutine initial_magnetization(sistema,n,m,Magnet)
    implicit none
    integer, intent(in) :: n, m
    real(kind=8), intent(in) :: sistema(n,m)
    integer :: sigx, sigy, antx, anty, ii,jj
    real(kind=8),intent(out) :: Magnet
    !.Inicializo la variable Magnet
    Magnet = 0 
    !.Sumo a lo largo de la matriz considerando condiciones periodicas de contorno
    do ii=1,m
       do jj=1, n

       if (ii ==1) then
               antx=n
       else if (jj==1) then
               anty=m
       else if (ii==n) then
               sigx=1
       else if (jj==m) then
               sigy=1
       else
               sigx = ii+1
               antx = ii-1
               sigy = jj+1
               anty = jj-1  
        end if
        Magnet = Magnet + sistema(ii,jj)
       end do  
    end do
end subroutine initial_magnetization
