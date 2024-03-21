       PROGRAM integracao_numerica

       implicit real*8 (a-h, o-z)
       parameter(a = 0d0)
       parameter(b = 1d0)

       open(10, FILE='saida.dat')
       
       N = 12
       val_exato = antideriv(b)-antideriv(a)

       do i = 1, 10
            h = 1d0/N
            area_trap = 0d0
            area_simp = 0d0
            area_boole = 0d0

            do l = 0, N
                x0 = a+l*h

                f0 = f(x0, 0, h)
                f1 = f(x0, 1, h)

                area_trap = area_trap + (h/2d0)*(f0+f1)

            end do

            do j = 1, N, 2
                x0 = a+j*h

                f0 = f(x0, 0, h)
                f1 = f(x0, 1, h)
                fm1 = f(x0, -1, h)

                area_simp = area_simp + (h/3d0)*(f1+4*f0+fm1)

            end do

            do k = 0, N, 4
                x0 = a+k*h

                f0 = f(x0, 0, h)
                f1 = f(x0, 1, h)
                f2 = f(x0, 2, h)
                f3 = f(x0, 3, h)
                f4 = f(x0, 4, h)

                area_boole = area_boole +((2*h)/45d0)*(7*f0+32*f1+
     $          12*f2+32*f3+7*f4)

            end do

            write(*,*) 'N =', N, 'Trapézio:', abs(area_trap-val_exato), 
     $      'Simpson:', abs(area_simp-val_exato), 
     $      'Boole: ', abs(area_boole-val_exato)

            N = N*2
       end do

       write(*,*) 'Valor exato: ', val_exato

       close(10)
       
       end program


       function f(x, n, h)
           implicit real*8 (a-h, o-z)
           parameter(pi = dacos(-1d0))

           x_n = x + n * h
           f = dexp(x_n/4d0)*dsin(pi*x)

       return
       end function

       function antideriv(x)
           implicit real*8 (a-h, o-z)
           parameter(pi = dacos(-1d0))

           antideriv = (4*dexp(x/4d0)*(dsin(pi*x)-4*pi*dcos(pi*x)))/
     $                 ((16*pi**2)+1)

       return
       end function
