       PROGRAM calculo_derivadas

       implicit real*8 (a-h, o-z)
       parameter(x = 0.5d0)

       dimension h(1:14)
       parameter(h = (/0.5d0, 0.2d0, 1d-1, 5d-2, 1d-2, 5d-3, 1d-3, 5d-4, 
     $ 1d-4, 5d-5, 1d-5, 1d-6, 1d-7, 1d-8/))

 11    format(A8, 2A24, A20, A16, 2A22, /) 
 12    format(e16.8, 6e20.11)     
 13    format(/, A16, 6e20.11)

       open(10, FILE='saida-a-12610389.dat')

c      Expressoes exatas
       d1 = 3*dsinh(3*x)*dsin(x/2d0)+0.5d0*dcosh(3*x)*dcos(x/2) 
       d2 = (12*dcos(x/2d0)*dsinh(3*x)+35*dsin(x/2)*dcosh(3*x))*0.25d0
       d3 = (198*dsin(x/2)*dsinh(3*x)+ 107*dcos(x/2)*dcosh(3*x))*0.125d0

       write(10, 11) 'h', 'D. 3 pts', 'D. p/ frente 2 pts', 
     $ 'D. p/ trás 2 pts', 'D. 5 pts', 'D. 2ª 5 pts', 'D. 3ª 5 pts'

       do i = 1, 14

            f0 = f(x, 0, h(i))
            f1 = f(x, 1, h(i))
            fm1 = f(x, -1, h(i))
            f2 = f(x, 2, h(i))
            fm2 = f(x, -2, h(i))

            ds3 = (f1 - fm1)/(2d0*h(i))
            df2 = (f1 - f0)/h(i)
            dt2 = (f0 - fm1)/h(i)
            ds5 = (fm2 - 8*fm1 + 8*f1 - f2)/(12d0*h(i))
            d2s5 = (-fm2 + 16*fm1 - 30*f0 + 16*f1 - f2)/(12d0*(h(i))**2)
            d3as5 = (-fm2 + 2*fm1 - 2*f1 + f2)/(2d0*(h(i))**3)

            eds3 = abs(d1-ds3)
            edf2 = abs(d1-df2)
            edt2 = abs(d1-dt2)
            eds5 = abs(d1-ds5)
            ed2s5 = abs(d2-d2s5)
            ed3as5 = abs(d3-d3as5)
        
            write(10, 12) h(i), eds3, edf2, edt2, eds5, ed2s5, ed3as5
       end do

       write(10, 13) 'Exatas:', d1, d1, d1, d1, d2, d3

       end program 

       function f(x, n, h)
          implicit real*8(a-h, o-z)
       
          x_novo = x + n*h
          f = dcosh(3.0d0*x_novo)*dsin(x_novo/2.0d0)
       return
       end function
