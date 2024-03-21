       PROGRAM secante
       implicit real*8(a-h, o-z)

       parameter(eprec = 1e-6)
       parameter (a = -10)

c      Método da Secante
       write(*, *) 'Método da Secante'

       x = a
       xnm1 = -9d0
       h = 0.1d0

       i = 0

40     if (abs(f(x)) > eprec) then
            if ((f(x)-f(xnm1)) .ne. 0) then
                xnp1 = x - (f(x)*(x-xnm1)/(f(x)-f(xnm1)))
            
            write(*,*) i, x, xnp1, f(x), f(xnm1)
                xnm1 = x
                x = xnp1
            else
                xnm1 = x
                x = x+h
            end if

            i = i +1
            goto 40
       end if
    
       write(*, *) 'Raiz encontrada: ', x, 'Iterações: ', i

       end program


       function f(x)
           implicit real*8(a-h, o-z)
           f = x**3-(1.5d0)*x**2-(1.5d0)*x+1
       return
       end function
