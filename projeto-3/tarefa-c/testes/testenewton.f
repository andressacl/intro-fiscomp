       PROGRAM raizes

       implicit real*8 (a-h, o-z)

       eprec = 1e-6
       !read(*,*) guess
       !write(*,*) 'Valor inicial da função', f(guess)

       a = -10
       b = 10
       h = 0.5e0
       !N = (b-a)/h

       guess = a
       ult_raiz = 0
       i = 0

20     if (i < 3) then
            guess_ant = guess
                
10         if (abs(f(guess)) > eprec) then
                guess = guess - (f(guess)/df(guess))
                write(*,*) guess
                goto 10
           end if

           write(*,*) '----------'

           if (abs(guess-ult_raiz) > eprec .or. i .eq. 0) then
               write(*,*) 'Raiz encontrada: ', guess
               i = i+1
               ult_raiz = guess
           end if

           guess = guess_ant + h
           !write(*,*) 'Next guess: ', guess

           goto 20
        end if


       end program

       function f(x)
           implicit real*8(a-h,o-z)

           f = x**3-(1.5e0)*x**2-(1.5e0)*x+1

       return
       end function

       function df(x)
           implicit real*8(a-h,o-z)

           df = 3*x**2-3*x-(1.5e0)

       return
       end function



