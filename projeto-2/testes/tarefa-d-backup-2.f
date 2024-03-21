       PROGRAM entropy
       parameter(p = 1.0/4.0)
       parameter(N=1000)
       parameter(M=1000)
       parameter(iwidth = 10)

       parameter(idim_m = 2*N/iwidth)
       dimension m_count(idim_m, idim_m)

       dimension ix(M), iy(M) !vetor de posições dos andarilhos
    
       parameter(ip_dim = 1/p)
       dimension ipx(ip_dim), ipy(ip_dim) 
       parameter(ipx=(/1 , 0, -1, 0/))
       parameter(ipy=(/0, 1, 0, -1/))

       open(10, file = 'saida-entropia.dat')
       open(99, file='posicoes.dat')

c      Inicializando os vetores em 0
       do i = 1, idim_m
            do j = 1, idim_m
                m_count(i,j) = 0
            end do
       end do

       do j = 1, M
            ix(j) = 0
            iy(j) = 0
       end do

       min_mat = -(idim_m/2)

       do i = 1, N !Cada passo corresponde a um microestado
            
            do j = 1, M
                ir = (rand()/p)+1
                ix(j) = ix(j) + ipx(ir)
                iy(j) = iy(j) + ipy(ir)

                write(99,*) i, j, ix(j), iy(j)

                ipos_x = ix(j)/iwidth
                ixlocal = ipos_x-min_mat+1
                
                ipos_y = iy(j)/iwidth
                iylocal = ipos_y-min_mat+1

                m_count(ixlocal,iylocal) = m_count(ixlocal,iylocal) + 1
            end do   

            S_total = 0.0

            do k = 1, idim_m
                do l = 1, idim_m
                    if (m_count(k,l) .ne. 0) then
                        Pi = m_count(k,l)*(1.0)/M
                        S = Pi*log(Pi)
                        S_total = S_total - S
                        m_count(k,l) = 0
                    end if
                end do
            end do

            write(10,*) i, S_total
       end do

       close(10)
       close(99)

       end program
