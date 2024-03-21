       PROGRAM raizes_polinomio

       eprec = 1e-6
       a = -10
       b = 10
       h = 0.1e0
       N = (b-a)/h

c      Método de Newton-Raphson

       do n = 0, N-1

            xe = a + n*h
            xd = xe + h

            fe = f(xe)
            fd = f(xd)

            if (fe * fd .lt. 0) then
                i = 0
                guess = xe
                
 10             if (abs(f(guess)) > eprec) then
                    guess = guess - (f(guess)/df(guess))
                    write(*,*) i, guess
                    i = i+1
                    goto 10
                end if

            write(*,*) '------------------'
            write(*,*) 'Raiz encontrada: ', guess, 
     $                 'Nº de iterações:', i

            end if

       end do

       end program


       function f(x)

           f = x**3-(1.5e0)*x**2-(1.5e0)*x+1

       return
       end function

       function df(x)

           df = 3*x**2-3*x-(1.5e0)

       return
       end function



