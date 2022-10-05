        PROGRAM bidimensional_random_walk
        parameter(p = 1.0/4.0)
        parameter(ip_dim = 1/p)
        dimension ipx(1:ip_dim), ipy(1:ip_dim) !vetores de controle de possibilidades
        parameter(ipx=(/1 , 0, -1, 0/))
        parameter(ipy=(/0, 1, 0, -1/))

        write(*,*) 'Vetor de possibilidades x: ', (ipx(i), i=1, ip_dim)
        write(*,*) 'Vetor de possibilidades y: ', (ipy(i), i=1, ip_dim)

        open(10, FILE = 'info-andarilhos.dat')

        write(*,*) 'Insira o número N de passos:'
        read(*,*) N

        write(*,*) 'Insira o número M de andarilhos:'
        read(*,*) M

        xsum = 0.0
        ysum = 0.0
        xsum2 = 0.0
        ysum2 = 0.0

        do i = 1, M
            ipos_x = 0 !posicao do andarilho
            ipos_y = 0

            do j = 1, N
                ir = (rand()/p)+1
                ipos_x = ipos_x + ipx(ir)
                ipos_y = ipos_y + ipy(ir)
            end do

            write(10,*) ipos_x, ipos_y
            
            xsum = xsum + ipos_x
            ysum = ysum + ipos_y

            xsum2 = xsum2 + ipos_x**2
            ysum2 = xsum2 + ipos_y**2
        end do

c       Média das posições
        r_norma = sqrt((xsum/M)**2+(ysum/M)**2)

c       Δ**2
        pescalar = xsum**2+ysum**2
        disp = (xsum2/M) + (ysum2/M) - pescalar

        write(*,*) '<r> =', r_norma 
        write(*,*) '<Δ²> =', disp

        close(10)

        stop
        end
