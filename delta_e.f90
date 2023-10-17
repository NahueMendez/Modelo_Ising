subroutine delta_energy(sistema, x, y, n, m, Jis,delta)
    implicit none
    real(kind=8), intent(in) :: Jis   ! Constante
    integer, intent(in) :: x, y   ! Fila y columna a calcular
    integer, intent(in) :: n, m   ! Tamaño de la matriz 
    real(kind=8), intent(out) :: delta  ! Salida de la rutina Delta Energia
    real(kind=8), intent(in) :: sistema(n, m) !Entrada: matriz del sistema
    real(kind=8),allocatable :: sistema_aug(:,:) !. Matriz aumentada con PVC
    
    !.Aloco una matriz de n+1 y m+1
    allocate(sistema_aug(0:n+1,0:m+1))
    sistema_aug(1:n,1:m)=sistema
    !.Repito filas y columnas (condiciones periodicas de contorno PVC)
    sistema_aug(0,:)=sistema(n,:)
    sistema_aug(n+1,:)=sistema(1,:)
    sistema_aug(:,0)=sistema(:,m)
    sistema_aug(:,m+1)=sistema(:,1)

    ! Calcular deltaE
    delta=2.0*Jis*sistema_aug(x,y)*(sistema_aug(x-1,y)+sistema_aug(x+1,y)+sistema_aug(x,y-1)+sistema_aug(x,y+1))

end subroutine delta_energy

