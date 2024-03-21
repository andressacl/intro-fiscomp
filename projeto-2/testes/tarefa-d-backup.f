       PROGRAM entropy
       parameter(p = 1.0/4.0)
       parameter(N=100)
       parameter(M=10)
       parameter(iwidth = 10)

       parameter(idim_m = 2*N/iwidth)
       dimension matriz_count(idim_m, idim_m)
    
       parameter(ip_dim = 1/p)
       dimension ix(M), iy(M) !vetor de posições dos andarilhos

       dimension ipx(ip_dim), ipy(ip_dim) 
       parameter(ipx=(/1 , 0, -1, 0/))
       parameter(ipy=(/0, 1, 0, -1/))

       write(*,*) 'Vetor de possibilidades x: ', (ipx(i), i=1, ip_dim)
       write(*,*) 'Vetor de possibilidades y: ', (ipy(i), i=1, ip_dim)

       open(10, file = 'saida-entropia.dat')
       open(99, file='posicoes.dat')

       write(*,*) 'Insira o número de passos N:'
       read(*,*) N

       do i = 1, M
            ix(i) = 0
            iy(i) = 0
       end do

       do i = 1, N !Cada passo corresponde a um microestado
            
            do j = 1, M
                ir = (rand()/p)+1
                ix(j) = ix(j) + ipx(ir)
                iy(j) = iy(j) + ipy(ir)

                write(99,*) i, j, ix(j), iy(j)
            end do

            S_total = 0.0

            do k= -ispace, ispace-iside, iside
                do l = -ispace, ispace-iside, iside
                    icount = 0

                    do j = 1, M

                        if ((k .lt. ix(j)) .and. (ix(j) .lt. k + iside)
     &                  .and. (l .lt. iy(j)) .and. 
     &                   (iy(j) .lt. l + iside)) then
                            icount = icount + 1
                        end if

                    end do

                    if (icount .ne. 0) then
                        Pi = (1.0)*icount/M
                        S = Pi * log(Pi)
                        S_total = S_total - S
                    end if
                end do
            end do

            write(10,*) i, S_total
                        
       end do

       close(10)
       close(99)

       end program
