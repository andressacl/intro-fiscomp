       PROGRAM dimensoes_esferas
       parameter(pi=acos(-1.0e0))
             
       open(10, FILE='dimensoes-esferas-12610389.dat')

       do i = 2, 20
            write(10,*) i, volume(i)
       enddo

       close(10)

       end program dimensoes_esferas
 
       function volume(idim)
            pi = acos(-1.0e0)
            aux = 1
            fgamma = 1
            arg = (idim/2.0e0)+1

 99         if (arg .gt. 0) then
                call gamma_aux(aux, arg)
                if (arg .gt. 1) then
                    fgamma = fgamma*(arg-1)
                endif
            
                arg = arg - 1
                fgamma = fgamma*aux
           goto 99
           endif

           volume = (pi**(idim/2.0e0))/fgamma
       return
       end

       subroutine gamma_aux(aux, arg)
           pi = acos(-1.0e0)

           if (arg == 1) then 
                aux = 1
           else if (arg == 0.5) then
                aux = pi**0.5
           endif
       return
       end
           


