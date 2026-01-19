Program Algebre_Combinatoire
    implicit none
    integer :: n = 10
    integer ::  k, i, C, val
    real :: a, b, Pascal
    !integer, dimension (10,10) :: triangle

    interface
    subroutine Factoriel(N, res)
        implicit none
        !real :: res
        integer :: i, N, res
    end subroutine

    subroutine Combinaison(n, p, res)
        implicit none
        integer :: n, p, valn, valp, valnp, res
        !real :: res
        !external :: Factoriel
    end subroutine
    end interface

    !Test avec les sous programmes
    a = 2 ; b = 5
    Pascal = 0.
    do i = 1, n
        !Pascal = Pascal + a**n
        call Combinaison(n, i, C)
        Pascal = Pascal + C * (b**i) * (a**(n-i))
        Pascal = Pascal + a**n
    end do
    write (*,*) a, '+', b, 'a la puissance', n, 'est:', Pascal

    do n = 0, 10
        !write(*, '(A,I0,A)', advance='no') "Rang ", n, " : "
        do k = 0, n
            call Combinaison(n, k, val)
            write(*, '(I0, A)', advance='no') val, " "
            
        end do
        print *
    end do

    
end program

subroutine Factoriel(N, res)
    implicit none
    integer :: res
    integer :: i, N

    res = 1
    if (N == 0) then
        res = 1
    else
        do i = 1, N
            res = res * i
        end do
    end if
end subroutine

subroutine Combinaison(n, p, res)
    implicit none
    integer :: n, p, valn, valp, valnp
    integer :: res
    external :: Factoriel

    if (p == 1 .and. n .ne.1 ) then
        res = n
    else if ( p == n .or. p == 0 ) then
        res = 1
    else if (P > n) then
        write(*,*) "Error n doit etre > p"
    else
        call Factoriel(n, valn)
        call Factoriel(p, valp)
        call Factoriel((n-p), valnp)
        res = valn / (valp * valnp)
    end if
end subroutine