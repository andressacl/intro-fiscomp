        PROGRAM unidimensional_random_walk
        parameter(p = 1.0/2.0)
        parameter(N = 1000)
        parameter(ip_dim = 1/p)
        parameter(iwidth=10)
        parameter(idim_h = (2*N/iwidth)+1)
        dimension ip(1:ip_dim) !vetor de controle de possibilidades
        dimension ihist(1:idim_h) !vetor do histograma

        ip(1) = 1

        do k = 2, ip_dim
            ip(k) = -1
        enddo 

        min_hist = -(idim_h-1)/2

        do i = 1, idim_h
            ihist(i) = 0
        end do

        open(10, FILE = 'saida-b-histograma1-12610389.dat')

        write(*,*) 'Insira o número M de andarilhos:'
        read(*,*) M

        sum1 = 0 ! <x>
        sum2 = 0 ! <x²>

        do i = 1, M
            ipos = 0 !posicao do andarilho

            do j = 1, N
                ix = (rand()/p) + 1
                ipos = ipos+ip(ix) 
            end do

            sum1 = sum1 + ipos
            sum2 = sum2 + ipos**2

            ipos_hist = ipos/iwidth
            local = ipos_hist-min_hist+1
            ihist(local) = ihist(local)+1
        end do

c       Passar informações do histograma para um arquivo
        
        do i = 1, idim_h
            ipos_hist = i+min_hist-1
            write(10,*) ipos_hist*iwidth, ihist(i)
        end do

c       Média das posições

        sum1 = sum1/M
        sum2 = sum2/M

c       Forma estatistica
        write(*,*) 'Resultado estatístico: '
        write(*,*) '<x> =', sum1
        write(*,*) '<x²> =', sum2

c       Forma analitica
        q = 1 - p
        sum_an1 = N*(p-q)
        sum_an2 = (N*(p-q))**2 + 4*N*p*q

        write(*,*) 'Resultado analítico:'
        write(*,*) '<x>=', sum_an1
        write(*,*) '<x^2>=', sum_an2

        close(10)

        stop
        end

