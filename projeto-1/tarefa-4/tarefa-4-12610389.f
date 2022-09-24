       PROGRAM aproximacao_cos

       real*8 :: res_dupla, deprec, dx, dsoma, dtermo_novo, derro
       integer*8 :: ifat, idfat
       
       eprec = 1e-5
       deprec = 1e-16

       write(*,*) 'Insira o valor de x:'
       read(*,*) x

c      Cálculo direto
       res_simples = cos(x)
       write(*,*) 'Valor da função intrínseca:', res_simples

c      Cálculo por aproximação
       i = 2
       ifat = 1
       soma = 0e0
       termo_novo =1e0

  10   if(abs(termo_novo)>eprec) then
            soma = soma + termo_novo
            ifat = ifat*(i-1)*i
            termo_novo = (-1.0)**(i/2) * (x**i/ifat)
            i = i+2
            goto 10
        endif
        
        erro = abs(res_simples-soma)
        write(*,*) 'Valor da aproximação:', soma
        write(*,*) 'Valor do erro:', erro

c       Dupla precisão -------------------------------
        
        if (abs(x) >= 1.4) then
            write(*,*) 'Os valores precisam ser mais próximos de 0 para
     $  o cálculo com dupla precisão.'

        else

            dx = x

c           Cálculo direto
            res_dupla = dcos(dx)
            write(*,*) 'Valor intrínseco (dupla precisão):', res_dupla

c           Cálculo por aproximação

            j = 2
            idfat = 1
            dsoma = 0d0
            dtermo_novo =1d0

  20        if(abs(dtermo_novo)>deprec) then
                dsoma = dsoma + dtermo_novo
                idfat = idfat*(j-1)*j
                dtermo_novo = (-1.0)**(j/2) * (dx**j/idfat)
                j = j+2
                goto 20
            endif
        
        derro = abs(res_dupla-dsoma)
        write(*,*) 'Valor da aproximação (dupla precisão):', dsoma
        write(*,*) 'Valor do erro (dupla precisão):', derro

        endif
        end program aproximacao_cos
