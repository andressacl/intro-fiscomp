       PROGRAM raizes_polinomio
       implicit real*8(a-h, o-z)

       parameter(eprec = 1e-6)
       parameter (a = -10)
       parameter(b = 10)
       parameter(h = 0.1) 
       parameter(h_n = 0.3d0)
       
       open(10, FILE='saida-c-12610389.dat')
 11    format(/, A24, /)
 12    format(I4, f20.11)
 13    format(A20, f20.11, A20, I2)

c      Método de busca direta

       write(10, 11) 'Busca direta'
       write(*, *) 'Busca direta'

       N = (b-a)/h

       do n = 0, N-1

            xe = a + n*h
            xd = xe + h

            fe = f(xe)
            fd = f(xd)

            if (fe * fd .lt. 0) then
                i = 0

 20            if (abs(f(xe)-f(xd)) .gt. eprec) then
                    xm = (xe+xd)/2.d0

                    if (f(xd)*f(xm) .gt. 0) then
                        xd = xm
                    else 
                        xe = xm
                    end if

                    i = i+1
                    write(10, 12) i, xm

                    goto 20
                end if

            write(*, 13) 'Raiz encontrada: ', xm, 'Iterações: ', i
            write(10, 13) 'Raiz encontrada: ', xm

            end if
       end do

c      Método de Newton Raphsen
       write(10, 11) 'Método de Newton-Raphson'
       write(*, *) 'Método de Newton-Raphson'

       x = a
       ult_raiz = 0d0
       n_raizes = 0

30     if (n_raizes < 3) then
            i = 0
            x_ant = x
                
40         if (abs(f(x)) > eprec) then
               if (df(x) .ne. 0) then
                    x = x - (f(x)/df(x))
               else
                    x = x+h
               end if

               i = i+1
               write(10, 12) i, x
               goto 40
           end if

           if (abs(x-ult_raiz) > eprec .or. n_raizes .eq. 0) then
               write(*, 13) 'Raiz encontrada: ', x, 'Iterações: ', i
               write(10, 13) 'Raiz encontrada: ', x

               n_raizes = n_raizes+1
               ult_raiz = x
               x_ant = ult_raiz
           end if

           x = x_ant + h_n
           goto 30
        end if

c      Método da Secante
       write(10, 11) 'Método da Secante'
       write(*, *) 'Método da Secante'

       x = a
       xnm1 = x + h_n
       ult_raiz = 0d0
       n_raizes = 0

50     if (n_raizes < 3) then
           i = 0
           x_ant = x

60         if (abs(f(x)) > eprec) then
                if ((f(x)-f(xnm1)) .ne. 0) then
                    xnp1 = x - (f(x)*(x-xnm1)/(f(x)-f(xnm1)))
                
                    xnm1 = x
                    x = xnp1
                else
                    xnm1 = x
                    x = x+h
                end if

                i = i +1

                write(10, 12) i, x
                goto 60
           end if

           if (abs(x-ult_raiz) > eprec .or. n_raizes .eq. 0) then
               write(*, 13) 'Raiz encontrada: ', x, 'Iterações: ', i
               write(10, 13) 'Raiz encontrada: ', x

               n_raizes = n_raizes+1
               ult_raiz = x
               x_ant = ult_raiz
           end if

           x = x_ant + 2*h_n
           xnm1 = x - h_n
           goto 50
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
