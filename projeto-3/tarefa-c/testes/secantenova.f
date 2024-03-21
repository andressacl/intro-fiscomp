       PROGRAM secante
       implicit real*8(a-h, o-z)

       parameter(eprec = 1e-6)
       parameter (a = -10)

       open(10, FILE='saida.dat')
 11    format(A20, f20.11, /)

c      Método da Secante
       write(*, *) 'Método da Secante'

       x = a
       xnm1 = -9
       h_n = 0.40
       ult_raiz = 0d0
       n_raizes = 0
       h = 0.1d0

30     if (n_raizes < 3) then
           i = 0
           x_ant = x

40         if (abs(f(x)) > eprec) then
                if (f(x)-f(xmn1) .ne. 0) then
                    xnp1 = x - (f(x)*(x-xnm1)/(f(x)-f(xnm1)))
                    write(*,*) i, xnp1
                    xnm1 = x
                    x = xnp1
                else
                    xnm1 = x
                    x = x+h
                end if 

                i = i +1
                goto 40
           end if

           if (abs(x-ult_raiz) > eprec .or. n_raizes .eq. 0) then
               write(*, *) 'Raiz encontrada: ', x

               n_raizes = n_raizes+1
               ult_raiz = x
               x_ant = ult_raiz
           end if

           x = x_ant + 2*h_n
           xnm1 = x - h_n
           write(*,*) 'n raizes', n_raizes
           goto 30
        end if

       end program


       function f(x)
           implicit real*8(a-h, o-z)
           f = x**3-(1.5d0)*x**2-(1.5d0)*x+1
       return
       end function

       function df(x)
           implicit real*8(a-h,o-z)
           df = 3*x**2-3*x-(1.5d0)
       return
       end function

