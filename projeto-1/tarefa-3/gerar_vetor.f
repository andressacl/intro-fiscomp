       PROGRAM gera_vetor_aleatorio

       dimension vetor(1:1000)

       open(10, FILE='entrada.dat')

       do i = 1, 1000
            call random_number(x)
            write(10, *) x*100
       end do

       end program
