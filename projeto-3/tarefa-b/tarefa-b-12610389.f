       PROGRAM integracao_numerica

       implicit real*8 (a-h, o-z)
       parameter(a = 0d0)
       parameter(b = 1d0)

 11    format(A6, A10, 3A20, /) 
 12    format(I8, f12.8, 3e20.11)     
 13    format(/, A30, e22.11)

       open(10, FILE='saida-b-12610389.dat')
       
       N = 12
       val_exato = antideriv(b)-antideriv(a)

       write(10, 11) 'N', 'h', 'Trapézio', 'Simpson', 'Boole'

       do i = 1, 10
            h = 1d0/N
            area_trap = 0d0
            area_simp = 0d0
            area_boole = 0d0

            do j = 1, N-1, 2
                x0 = a+j*h

                f0 = f(x0, 0, h)
                f1 = f(x0, 1, h)
                fm1 = f(x0, -1, h)

                area_trap = area_trap + (h/2d0)*(fm1+2*f0+f1)
                area_simp = area_simp + (h/3d0)*(fm1+4*f0+f1)

            end do

            do k = 0, N-4, 4
                x0 = a+k*h

                f0 = f(x0, 0, h)
                f1 = f(x0, 1, h)
                f2 = f(x0, 2, h)
                f3 = f(x0, 3, h)
                f4 = f(x0, 4, h)

                area_boole = area_boole +((2*h)/45d0)*(7d0*f0+32d0*f1+
     $          12d0*f2+32d0*f3+7d0*f4) 

            end do

            write(10, 12) N, h, abs(val_exato-area_trap), 
     $      abs(val_exato-area_simp), abs(val_exato-area_boole)

            N = N*2
       end do

       write(10, 13) 'Valor exato: ', val_exato

       close(10)
       
       end program


       function f(x, n, h)
           implicit real*8 (a-h, o-z)
           parameter(pi = dacos(-1d0))

           x_n = x + n * h
           f = dexp(x_n/4d0)*dsin(pi*x_n)

       return
       end function

       function antideriv(x)
           implicit real*8 (a-h, o-z)
           parameter(pi = dacos(-1d0))

           antideriv = (4*dexp(x/4d0)*(dsin(pi*x)-4*pi*dcos(pi*x)))/
     $                 ((16*pi**2)+1)

       return
       end function
