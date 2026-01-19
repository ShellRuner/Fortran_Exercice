module Mod_FP
    implicit none
    real(8) :: eps, a, b, x
    real(8), external :: f, fausse_position, Log_10
    integer :: nmax
end module

Program vitesse_FP
    use Mod_FP
    implicit none
    real(8), dimension(100) :: T
    real(8) :: yf, Val_racn, x1, x2, y1, y2, y, yn
    integer :: i

    eps = 10E-5
    !**************************************************************************
    !Cas pour x appartenant a [0,1]
    a = 0 ; b = 1
    ! Verification de l'intervalle
    if (f(a) * f(b) > 0) then
        write(*,*) 'L intervalle est mal choisi'
        write (*,*) 'Le programme va s arreter'
        stop
    end if

    !Initialisation
    x1 = a ; y1 = f(x1); x2 = b; y2 = f(x2)
    nmax = 1
    do while (nmax .ne. 0)
        x = (x1*y2 - x2*y1)/(y2 - y1)
        T(nmax) = x
        y = f(x)
        if (abs(f(x)) < eps) exit

        if (y1 * y < 0) then
            x2 = x ; y2 = f(x2) ! la racine appartient a l'intervalle [x1,x]
        else
            x1 = x; y1 = f(x1) ! la racine appartient a l'intervalle [x,x2]
        end if
        nmax = nmax + 1
    end do
    Val_racn = fausse_position(f, a, b, eps, nmax) 
    open(1, file = "Donnees_Racine1.dat")
                write(1,*) 'Iterations:', 'xn:', 'yn:'
    do i = 1, nmax
            yf = f(T(i))
            if (T(i) <= Val_racn) then 
                yn = Log_10(yf)   
                write(1,*) i, T(i), yn
            end if
    end do
    close(1)
    !***********************************************************************
    !cas pour x appartenant a [2,4]
    a = 2 ; b = 4
    ! Verification de l'intervalle
    if (f(a) * f(b) > 0) then
        write(*,*) 'L intervalle est mal choisi'
        write (*,*) 'Le programme va s arreter'
        stop
    end if

    !Initialisation
    x1 = a ; y1 = f(x1); x2 = b; y2 = f(x2)
    nmax = 1
    do while (nmax .ne. 0)
        x = (x1*y2 - x2*y1)/(y2 - y1)
        T(nmax) = x
        y = f(x)
        if (abs(f(x)) < eps) exit

        if (y1 * y < 0) then
            x2 = x ; y2 = f(x2) ! la racine appartient a l'intervalle [x1,x]
        else
            x1 = x; y1 = f(x1) ! la racine appartient a l'intervalle [x,x2]
        end if
        nmax = nmax + 1
    end do
    Val_racn = fausse_position(f, a, b, eps, nmax) 
    open(2, file = "Donnees_Racine2.dat")
    do i = 1, nmax
            yf = f(T(i))
            if (T(i) <= Val_racn) then 
                yn = Log_10(yf)   
                write(2,*) i, T(i), yn
            end if
    end do
    close(2)
    !****************************************************************************
    !Cas pour x appartenant a [5,7]
    a = 5 ; b = 7
    ! Verification de l'intervalle
    if (f(a) * f(b) > 0) then
        write(*,*) 'L intervalle est mal choisi'
        write (*,*) 'Le programme va s arreter'
        stop
    end if

    !Initialisation
    x1 = a ; y1 = f(x1); x2 = b; y2 = f(x2)
    nmax = 1
    do while (nmax .ne. 0)
        x = (x1*y2 - x2*y1)/(y2 - y1)
        T(nmax) = x
        y = f(x)
        if (abs(f(x)) < eps) exit

        if (y1 * y < 0) then
            x2 = x ; y2 = f(x2) ! la racine appartient a l'intervalle [x1,x]
        else
            x1 = x; y1 = f(x1) ! la racine appartient a l'intervalle [x,x2]
        end if
        nmax = nmax + 1
    end do
    !Val_racn = fausse_position(f, a, b, eps, nmax) 
    open(3, file = "Donnees_Racine3.dat")
    do i = 1, nmax
        yf = f(T(i))
        !if (Ti(i) <= Val_racn) then 
            yn = Log_10(yf)   
            write(3,*) i, T(i), yn
        !end if
    end do
    close(3)
    !**************************************************************************

    
end program

!******Fonction fausse_position
function fausse_position(func, a, b, eps, nmax)
    implicit none
    real(8) :: fausse_position, func
    real(8) :: a, b, eps, x, x1, x2, y1, y2, y
    integer :: nmax, i

    ! Verification de l'intervalle
    if (func(a) * func(b) > 0) then
        write(*,*) 'L intervalle est mal choisi'
        write (*,*) 'Le programme va s arreter'
        stop
    end if

    !Initialisation
    x1 = a ; y1 = func(x1); x2 = b; y2 = func(x2)
    do i = 1, nmax
        x = (x1*y2 - x2*y1)/(y2 - y1)
        y = func(x)
        if (abs(func(x)) < eps) exit

        if (y1 * y < 0) then
            x2 = x ; y2 = func(x2) ! la racine appartient a l'intervalle [x1,x]
        else
            x1 = x; y1 = func(x1) ! la racine appartient a l'intervalle [x,x2]
        end if
    end do
    fausse_position = x
end function

!**************Fonction f(x) = xtan(x) - 1
function f(x)
    implicit none
    real(8) :: x
    real(8) :: f

    f = x * (dsin(x) / dcos(x)) - 1
end function

!*************Fonction Log_10(x)
function Log_10(x)
    implicit none
    real(8) :: Log_10, x

    x = abs(x)
    Log_10 = log10(x)
end function