subroutine initial_energy(sistema,Jis,n,m,E)
    implicit none
    integer, intent(in) :: n, m
    real(kind=8), intent(in) :: Jis, sistema(n,m)
    real(kind=8), allocatable :: sistema_aug(:,:)
    real(kind=8),intent(out) :: E
    integer :: ii,jj
    ! Aloco la matriz aumentada
    allocate(sistema_aug(0:n+1,0:m+1))
    !Repito filas y columnas (PVC)
    sistema_aug(1:n,1:m)=sistema
    sistema_aug(0,:)=sistema(n,:)
    sistema_aug(n+1,:)=sistema(1,:)
    sistema_aug(:,0)=sistema(:,m)
    sistema_aug(:,m+1)=sistema(:,1)

    !.Inicializo la variable E
    E = 0 
    !.Sumo a lo largo de la matriz considerando condiciones periodicas de contorno
    do ii=1,m
       do jj=1, n

        E = E + (-Jis*sistema_aug(ii,jj)*(sistema_aug(ii-1,jj)+sistema_aug(ii+1,jj)+sistema_aug(ii,jj-1)+sistema_aug(ii,jj+1)))
       end do  
    end do
end subroutine initial_energy
