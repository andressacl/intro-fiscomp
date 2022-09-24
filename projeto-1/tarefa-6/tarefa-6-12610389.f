       PROGRAM determinante_matriz
c      Este programa utiliza uma matriz de permutações de N para
c      calcular o determinante. No diretório estão contidos os arquivos 
c      gerados para N=3, N=4, N=5 e N=6, pegos das saidas da tarefa 5. 
c      Para a utilização de N=4 ou outro N
c      qualquer é necessário ajustar os parâmetros e fornecer o arquivo
c      que pode ser gerado com o outro código.
           
       parameter(N=4)
       parameter(Nfat=24)
       dimension RM(N,N), MQ(Nfat, N+1), MQ_aux(N+1, Nfat)

       mq_lin = Nfat
       mq_col = N
       indice_par = N+1

       open(10, FILE='entrada-6-12610389.dat')

       read(10, *, end = 99) MQ_aux
       MQ = transpose(MQ_aux)
 99    continue

       write(*,*) 'Forneça a matriz: '
       do i = 1, N
            read(*,*) (RM(i, j), j=1, N)
       end do

       det = 0

       do i = 1, mq_lin
            soma = 1
            do j = 1, mq_col
                soma = soma * RM(j, MQ(i,j))
            end do
            det = det + MQ(i, indice_par) * soma
       end do

       write(*,*) 'O determinante vale: ', det

       close(10)

       end program determinante_matriz

