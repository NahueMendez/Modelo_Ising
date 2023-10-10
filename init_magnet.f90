subroutine initial_energy(sistema,Jis,n,m,E)
    implicit none
    integer, intent(in) :: n, m
    real(kind=8), intent(in) :: Jis, sistema(n,m)
    integer :: sigx, sigy, antx, anty, ii,jj
    real(kind=8),intent(out) :: E
    !.Inicializo la variable E
    E = 0 
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
        E = E + (-Jis*sistema(ii,jj)*(sistema(antx,jj)+sistema(sigx,jj)+sistema(ii,anty)+sistema(ii,sigy)))
       end do  
    end do
end subroutine initial_energy
