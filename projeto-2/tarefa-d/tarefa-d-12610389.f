       PROGRAM entropy
       parameter(p = 1.0/4.0)
       parameter(Nmax=10000)
       parameter(M=10000)
       parameter(iwidth = 10)

       parameter(idim_m = (2*Nmax/iwidth)+1)
       dimension m_count(idim_m, idim_m)
    
       parameter(ip_dim = 1/p)
       dimension ipx(ip_dim), ipy(ip_dim) 
       parameter(ipx=(/1 , 0, -1, 0/))
       parameter(ipy=(/0, 1, 0, -1/))

       open(10, file = 'saida-d-12610389.dat')

c      Inicializando os vetores em 0
       do i = 1, idim_m
            do j = 1, idim_m
                m_count(i,j) = 0
            end do
       end do

       min_mat = -((idim_m-1)/2)

       write(10, *) 0, 0 

       do Np = 100, Nmax, 100
            S_total = 0.0

            do i = 1, M

                ix = 0
                iy = 0
                
                do j=1, Np
                    ir = (rand()/p)+1
                    ix = ix + ipx(ir)
                    iy = iy + ipy(ir)
                end do

                ix = ix/iwidth
                ixlocal = ix-min_mat+1
                
                iy = iy/iwidth
                iylocal = iy-min_mat+1

                m_count(ixlocal,iylocal) = m_count(ixlocal,iylocal) + 1
            end do

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

            write(10,*) Np, S_total
       end do

       close(10)

       end program
