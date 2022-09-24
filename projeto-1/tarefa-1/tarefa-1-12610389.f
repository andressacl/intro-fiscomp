       PROGRAM torus
c      Esse programa recebe os raios interno (r1) e externo (r2) de um 
c      torus e retorna sua área superficial e seu volume.

       parameter (pi = acos(-1.e0))

       write(*,*) 'Insira os raios interno (r1) e externo (r2):'
       read(*,*) r1, r2

       sarea = 4 * pi**2 * r1 * r2
       vol = 2*(pi**2)*r2*(r1**2)

       write(*,*) 'A área superficial é: ', sarea
       write(*,*) 'O volume é: ', vol

       end program torus

