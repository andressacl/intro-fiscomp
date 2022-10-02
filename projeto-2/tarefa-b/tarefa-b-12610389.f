        PROGRAM unidimensional_random_walk
        parameter(p = 1.0/5.0)
        parameter(N = 1000)
        parameter(ip_dim = 1/p)
        parameter(iwidth=10)
        parameter(idim_h = 2*N/iwidth)
        dimension ip(1:ip_dim) !vetor1 de controle de possibilidades
        dimension ihist(1:idim_h) !vetor do histograma

        ip(1) = 1

        do k = 2, ip_dim
            ip(k) = -1
        enddo

        write(*,*) 'Vetor de possibilidades: ', (ip(i), i=1, ip_dim)
        

        min_hist = -(idim_h)/2

        do i = 1, idim_h
            ihist(i) = 0
        end do

        open(10, FILE = 'info-andarilhos.dat')
        open(20, FILE = 'histograma-andarilhos.dat')

        write(*,*) 'Insira o número M de andarilhos:'
        read(*,*) M

        sum1 = 0 ! <x>
        sum2 = 0 ! <x²>

        do i = 1, M
            ipos = 0 !posicao do andarilho

            do j = 1, N
                ix = rand()/p
                ipos = ipos+ip(ix+1) !ix+1 ajusta índice do vetor de
                                     !controle
            end do

            sum1 = sum1 + ipos
            sum2 = sum2 + ipos**2

            write(10,*) i, ipos

            ipos_hist = ipos/iwidth
            local = ipos_hist-min_hist+1
            ihist(local) = ihist(local)+1
        end do

c       Contar andarilhos para cada número de passos
        
        do i = 1, idim_h
            ipos_hist = i+min_hist-1
            write(20,*) ipos_hist, ihist(i)
        end do

c       Média das posições

        sum1 = sum1/N
        sum2 = sum2/N

c       forma estatistica
        write(*,*) 'Resultado estatístico: '
        write(*,*) '<x> =', sum1
        write(*,*) '<x²> =', sum2

c       forma analitica
        q = 1 - p
        sum_an1 = N*(p-q)
        sum_an2 = (N*(p-q))**2 + 4*N*p*q

        write(*,*) 'Resultado analítico:'
        write(*,*) '<x>=', sum_an1
        write(*,*) '<x^2>=', sum_an2

        close(10)
        close(20)

        stop
        end

