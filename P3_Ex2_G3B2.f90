Program Matrice
    implicit none
    integer :: n, m, k, i, j
    integer , dimension(:,:), allocatable :: A

    write (*,*) "Entrez les valeurs de n et m"
    read (*,*) n, m
    allocate (A(n,m))

    !Lignes impaires
    k = 1
    do i = 1, n, 2
        do j = 1, m
            A(i,j) = k
            k = k +1
        end do
    end do

    !Lignes paires
    do i = 2, n, 2
        do j = 1, m
            A(i,j) = 1
        end do
    end do

    !Affichage
    do i = 1, n
        write(*,*) A(i,:)
    end do
end program