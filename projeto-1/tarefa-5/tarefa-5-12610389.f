       PROGRAM paridade_permutacao

c      Esse programa recebe as permutações permitidas para n elementos e
c      suas respectivas paridades e retorna as permutações e as
c      paridades de um conjunto com n+1 elementos. A dimensão
c      das matrizes M e M_aux deve ser ajustada manualmente alterando os
c      parâmetros.

       parameter (N=3)
       parameter(Nfat = 6)
       dimension M(Nfat,N+1), M_aux(N+1,Nfat), MN(1000,1000)

       m_lin = Nfat
       m_col = N

       mn_lin = Nfat*(N+1)
       mn_col = N+1 !Matriz de permutações SEM indicador de paridade

       open(10, FILE = 'entrada-5-N3-12610389.dat')
       open(20, FILE = 'saida-5-N3-12610389.dat')

       read(10, *, end = 99) M_aux
       M = transpose(M_aux)
 99    continue

       do i = 0, N
            isinal = i
            ipos = mn_col - i
            i_mn = m_lin*i

            do j = 1, ipos-1
                do i_aux = 1, m_lin
                    MN(i_mn+i_aux, j) = M(i_aux, j)
                enddo
            enddo

            do i_aux = 1, m_lin
                MN(i_mn+i_aux, ipos) = N+1
            enddo

            do j = ipos+1, mn_col
                do i_aux = 1, m_lin
                    MN(i_mn+i_aux, j) = M(i_aux, j-1)
                enddo
            enddo

            do i_aux = 1, m_lin
                MN(i_mn+i_aux, mn_col+1) = M(i_aux, mn_col)*(-1)**isinal
            enddo

       enddo

       do i=1, mn_lin
            write(20,*) (MN(i,j), j=1, mn_col+1)
       enddo

       close(10)
       close(20)

       end program paridade_permutacao

