        PROGRAM bidimensional_random_walk
        parameter(p = 1.0/4.0)
        parameter(M=10000)
        dimension ipx(1:4), ipy(1:4) !vetores de controle de possibilidades
        parameter(ipx=(/1 , 0, -1, 0/))
        parameter(ipy=(/0, 1, 0, -1/))

        open(10, FILE = 'saida-c-1-12610389.dat')
        open(20, FILE = 'saida-c-2-12610389.dat')
        open(30, FILE = 'saida-c-3-12610389.dat')
        open(40, FILE = 'saida-c-4-12610389.dat')
        open(50, FILE = 'saida-c-5-12610389.dat')
        open(60, FILE = 'saida-c-6-12610389.dat')

        write(*,*) 'Número de andarilhos: ', M

        do k = 1, 6
            N = 10**k

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

                io = k*10
                write(io,*) ipos_x, ipos_y
                
                xsum = xsum + ipos_x
                ysum = ysum + ipos_y

                xsum2 = xsum2 + ipos_x**2
                ysum2 = xsum2 + ipos_y**2
            end do

c       Média das posições
            r_norma = sqrt((xsum/M)**2+(ysum/M)**2)

c       Δ**2
            pescalar = (xsum/M)**2+(ysum/M)**2
            disp = (xsum2/M)+ (ysum2/M) - pescalar

            write(*,*) 'N = ', N
            write(*,*) '<r> =', r_norma 
            write(*,*) '<Δ²> =', disp
            write(*,*) '-----------------'

        end do
        close(10)
        close(20)
        close(30)
        close(40)
        close(50)
        close(60)

        stop
        end
