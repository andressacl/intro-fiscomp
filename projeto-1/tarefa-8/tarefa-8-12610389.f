       PROGRAM vol_hipersfera
       dimension r(100), M(3), idim(3)
        
       write(*,*) '         DIM         M      VOLUME          ERRO'
       do i = 1, 3
            idim(i) = 1+i

            volume_exato = vol_exato(idim(i))

            do j=1, 3
                M(j) = 100**j
                volume_aprox = volume_mc(idim(i), M(j))
                erro = abs(volume_exato-volume_aprox)
                write(*,*) idim(i), M(j), volume_aprox, erro
            enddo
       enddo

       end program vol_hipersfera


       function volume_mc(idim, M)
           dimension r(100)
            
           hit = 0.0
           do i= 1, M

                do j=1, idim
                    r(j) = rand()
                enddo

                raio = 0 

                do j =1, idim
                    raio = raio + r(j)**2
                enddo

                raio = raio**0.5

                if (raio <= 1) then
                    hit = hit + 1
                endif
           enddo

           volume_mc = 2**idim*(hit/M)
       return 
       end

       function vol_exato(idim)
            pi = acos(-1.0e0)
            aux = 1
            fgamma = 1
            arg = (idim/2.0e0)+1

99          if (arg .gt. 0) then
                call gamma_aux(aux, arg)
                if (arg .gt. 1) then
                    fgamma = fgamma*(arg-1)
                end if
            
                arg = arg - 1
                fgamma = fgamma*aux
           goto 99
           endif

           vol_exato = (pi**(idim/2.0e0))/fgamma
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
            
