       PROGRAM sistema_linear

c      É necessário alterar o arquivo de entrada para o arquivo com as
c      permutações de N ao utilizar.

       parameter(N=5)
       dimension RM_coef(N, N), vet(N), det_coord(N), aux(N), sol(N)
       
       write(*,*) 'Insira a matriz de coeficientes:'

       do i=1, N
            read(*,*) (RM_coef(i,j), j=1, N)
       enddo

       det_rm = 0
       Nfat = ifat(N)
       call calcular_det(N, Nfat, RM_coef, det_rm)

       write(*,*) 'Insira o vetor de resultados'
       read(*,*) (vet(i), i=1, N) 

       do k=1, N
            do i=1, N
                aux(i) = RM_coef(i, k) 
                RM_coef(i,k) = vet(i)
            enddo

            call calcular_det(N, Nfat, RM_coef, det_coord(k))

            do i=1, N                
                RM_coef(i,k) = aux(i)
            enddo
            
            sol(k) = det_coord(k)/det_rm
       enddo

    
       write(*,*) 'Conjunto solução:', (sol(k), k=1, N)
      
       end program sistema_linear

       subroutine calcular_det(N, Nfat, RM, det)
       dimension RM(N, N), MQ(Nfat, N+1), MQ_aux(N+1,Nfat)
       mq_lin = ifat(N)
       mq_col = N
       indice_par = N+1

       open(10, FILE='entrada-7-12610389.dat')

       read(10, *, end = 99) MQ_aux
       MQ = transpose(MQ_aux)
 99    continue

       det = 0e0

       do i = 1, mq_lin
            soma = 1e0
            do j = 1, mq_col
                soma = soma * RM(j, MQ(i,j))
            end do
            det = det + MQ(i, indice_par) * soma
       end do

       
       close(10)
       return
       end 
       
       integer function ifat(n)
       ifat = 1

       do i = 1, n
            ifat = ifat * i
       end do

       return
       end
