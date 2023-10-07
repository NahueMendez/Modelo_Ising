subroutine delta_energy(sistema, x, y, n, m, Jis,delta)
    implicit none
    real(kind=8), intent(in) :: Jis   ! Constante
    integer, intent(in) :: x, y   ! Fila y columna a calcular
    integer, intent(in) :: n, m   ! Tamaño de la matriz original
    integer :: nf, mf   ! Tamaño de la matriz final
    real(kind=8), intent(out) :: delta
    real(kind=8), intent(in) :: sistema(n, m)
    real(kind=8), dimension(:, :), allocatable :: matriz_final
    integer :: i, j, row, col

    ! Inicializar matriz_final con ceros
    matriz_final = 0.0d0
    !.Asigno las longitudes para la matriz final (n+1 fila inicio+ 1 fila final)
    nf = 2+n
    mf = 2+m
    allocate(matriz_final(nf, mf))
    matriz_final(2:nf-1,2:mf-1)=sistema
    matriz_final(1,2:nf-1)=sistema(1,:)
    matriz_final(2:mf-1,1)=sistema(:,1)
    matriz_final(nf,2:nf-1)=sistema(nf,:)
    matriz_final(2:mf-1,mf)=sistema(:,mf)

    ! Calcular deltaE
    delta=2.0d0*Jis*matriz_final(x+1, y+1)*(matriz_final(x+2, y+1)+matriz_final(x, y+1)+matriz_final(x+1, y+2)+matriz_final(x+1, y))
    !Libero memoria
    deallocate(matriz_final)
end subroutine delta_energy

