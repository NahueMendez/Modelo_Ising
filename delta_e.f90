subroutine delta_energy(sistema, x, y, n, m, Jis,delta)
    implicit none
    real(kind=8), intent(in) :: Jis   ! Constante
    integer, intent(in) :: x, y   ! Fila y columna a calcular
    integer, intent(in) :: n, m   ! Tamaño de la matriz 
    real(kind=8), intent(out) :: delta  ! Salida de la rutina Delta Energia
    real(kind=8), intent(in) :: sistema(n, m) !Entrada: matriz del sistema
    integer :: sigx,sigy,antx,anty  !. Enteros a utilizar

    !.Introduzco las condiciones periódicas de contorno
    if (x==1) then
            antx = n
    else if (y==1) then
            anty = m
    else if (x==n) then
            sigx = 1
    else if (y==m) then
            sigy = 1
    else
           sigx = x+1
           antx = x-1
           sigy = y+1
           anty = y-1
    end if

    ! Calcular deltaE
    delta=2.0*Jis*sistema(x,y)*(sistema(antx,y)+sistema(sigx,y)+sistema(x,anty)+sistema(x,sigy))

end subroutine delta_energy

