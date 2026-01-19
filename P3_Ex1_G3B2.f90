Program Ordre_Naturel_De_Tab
    implicit none
    integer, Parameter :: n = 1000
    real, dimension(n,n) :: A, B, Produit
    integer :: i, j
    real :: start_time, end_time, elapse_time

    !Initialisation des tableaux A et B de mainiere aleatoire
    call random_number(A)
    call random_number(B)
    !Calcul du produit terme a terme de A par B avec une boucle externe sur les lignes

    call cpu_time(start_time) ! permet d'enregistrer le temps de debut de la boucle
    do i = 1, n
        do j =1, n
            Produit(i,j) = A(i,j) * B(i,j)
        end do
    end do
    call cpu_time(end_time) ! permet d'enregistrer le temps de fin de la boucle

    elapse_time = end_time - start_time ! On a donc le temps de calcule de la boucle

    write(*,*) 'Le temps de calcul du produit avec une boucle externe sur les&
            & ligne est :', elapse_time

    !Calcul du produit terme a terme de A par B avec une boucle externe sur les colones
    
    call cpu_time(start_time)
    do j = 1, n
        do i =1, n
            Produit(i,j) = A(i,j) * B(i,j)
        end do
    end do
    call cpu_time(end_time)

    elapse_time = end_time - start_time
    write(*,*) 'Le temps de calcul du produit avec une boucle externe sur les&
            & colone est :', elapse_time

    write(*,*) ''        
    !Explications        
    write(*,*) 'Si les temps de calcul sont diferrents pour les deux methodes: &
                &cela peut etre du a certain facteurs comme l acces a la me&
                &moire , l efficacite de l acces a la memoire peut varier en&
                 & fonction de comment les boucles sont agencees, la complexite du cal&
                 &cul sur laquel la boucle est execute influe sur les performances&
                  & de calcul'
    write(*,*) "Si les temps de calcul sont egales cela peut etre du a la nature&
                  & du calcul, si les calcules sont plutot simples les temps de calculs peuvent&
                   & etre identique"

end program