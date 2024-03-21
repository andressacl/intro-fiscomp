        PROGRAM walkers_path 
        parameter(p = 1.0/4.0)
        parameter(ip_dim = 1/p)
        dimension ipx(1:ip_dim), ipy(1:ip_dim) !vetores de controle de possibilidades
        parameter(ipx=(/1 , 0, -1, 0/))
        parameter(ipy=(/0, 1, 0, -1/))

        M = 10
        N = 1000

        write(*,*) 'Vetor de possibilidades x: ', (ipx(i), i=1, ip_dim)
        write(*,*) 'Vetor de possibilidades y: ', (ipy(i), i=1, ip_dim)

        open(10, FILE = 'posicao1.dat')
        open(11, FILE = 'posicao2.dat')
        open(12, FILE = 'posicao3.dat')
        open(13, FILE = 'posicao4.dat')
        open(14, FILE = 'posicao5.dat')
        open(15, FILE = 'posicao6.dat')
        open(16, FILE = 'posicao7.dat')
        open(17, FILE = 'posicao8.dat')
        open(18, FILE = 'posicao9.dat')
        open(19, FILE = 'posicao10.dat')

            do i = 1, M
                ipos_x = 0 !posicao do andarilho
                ipos_y = 0

                io = i+9
                write(io,*) ipos_x, ipos_y

                do j = 1, N
                    ir = (rand()/p)+1
                    ipos_x = ipos_x + ipx(ir)
                    ipos_y = ipos_y + ipy(ir)
                    
                    write(io,*) ipos_x, ipos_y
                end do
            end do

        close(10)
        close(11)
        close(12)
        close(13)
        close(14)
        close(15)
        close(16)
        close(17)
        close(18)
        close(19)

        stop
        end
