       PROGRAM volume

       read(*,*) idim

       v = vol(idim)
       write(*,*) 'Aqui o volume', v

       end

       function vol_exato(idim)
           pi = acos(-1.0e0)

           d_n = idim/2.0e0
           arg_gamma = d_n+1
           val_gamma = 1.0e0

           call fgamma(arg_gamma, val_gamma, fgamma)

           vol_exato = (pi**d_n)/val_gamma

       return
       end
       
       subroutine fgamma(ent, saida, aux_fgamma)
           pi = acos(-1.0e0)
           if (ent == 1) then 
               return
           else if (ent==0.5) then
               saida = saida*pi**(0.5)
               return
           else
               saida = saida*(ent-1)
               ent = ent-1
               call aux_fgamma(ent, saida, aux_fgamma)
            end if

            return
        end