        PROGRAM random_walk
        parameter(N=1000)
        parameter(iwidth=10)
        parameter(idim_h = 2*N/iwidth)
        dimension ip(1:2)
        dimension ihist(1:idim_h)

        !N = 1000 !numero de passos

        ip(1) = 1 !Vetor de controle de possibilidades
        ip(2) = -1
        write(*,*) idim_h

        !iwidth = 10 !Largura do histograma
        !idim_h = 2*N/iwidth
        min_hist = -(idim_h)/2
        do i=1, idim_h
            ihist(i)=0
        end do

        open(10, FILE = 'info-andarilhos.dat')
        open(20, FILE='histograma-andarilhos.dat')


        write(*,*) 'Insira o número M de andarilhos:'
        read(*,*) M

        sum1 = 0 ! <x>
        sum2 = 0 ! <x²>

        do i = 1, M
            ipos = 0 !posicao do andarilho

            do j = 1, N
                ix = 2*rand()
                ipos = ipos+ip(ix+1) 
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

c        Média das posições

        sum1 = sum1/N
        sum2 = sum2/N

c       forma estatistica
        write(*,*) '<x> =', sum1
        write(*,*) '<x²> =', sum2

c       forma analitica




        close(10)
        close(20)

        stop
        end

