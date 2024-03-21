       PROGRAM raizes_polinomio
       !implicit real*8 (a-h, o-z)

       erro = 1e-6
       a = -10
       b = 10
       h = 0.1e0
       N = (b-a)/h

c      Método de busca direta

       do n = 0, N-1

            xe = a + n*h
            xd = xe + h
            desvio = 2*erro

            fe = f(xe)
            fd = f(xd)

            if (fe * fd .lt. 0) then
                i = 0

   20           if (desvio .gt. erro) then
                    xm = (xe+xd)/2.d0

                    if (fd*f(xm) .gt. 0) then
                        xd = xm
                    else 
                        xe = xm
                    end if

                    desvio = abs(f(xe)-f(xd))
                    i = i+1
                    write(*,*) i, xm

                    goto 20
                end if


            write(*,*) '------------------'
            write(*,*) 'Raiz encontrada: ', xm, 'Nº de iterações:', i

            end if

            !if (fe .eq. 0) then
            !    write(*,*) '0', xe
            !end if

            !if (fd .eq. 0) then
            !    write(*,*) '0', xd
            !end if

       end do



       end program


       function f(x)
           !implicit real*8 (a-h, o-z)

           f = x**3-(1.5e0)*x**2-(1.5e0)*x+1

       return
       end function

       function df(x)

           df = 3*x**2-3*x-(1.5e0)

       return
       end function
