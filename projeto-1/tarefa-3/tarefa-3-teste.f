       PROGRAM lista_num

       dimension vetor(1:1000)

       write(*,*) 'Digite m: '
       read(*,*) m

       open(10, FILE= 'entrada-3-12610389.dat')
       open(20, FILE='saida-3-12610389.dat')

       do i=1, 1000
            read(10, *, end = 89) vetor(i)
       end do
  89   continue

       n = i - 1 !i faz mais uma iteração até achar o EOF

       call ordenar(m, n, vetor)

       write(20,*) m
       do k=1, m
            write(20,*) vetor(k)
       enddo

       close(10)
       close(20)

       end program lista_num


       subroutine ordenar(m, itam, vetor)
       dimension vetor(1:1000)

       do i = m, 1, -1
            do j = itam, 2, -1
                if(vetor(j-1) .gt. vetor(j)) then
                    aux = vetor(j-1)
                    vetor(j-1) = vetor(j)
                    vetor(j) = aux
                endif
            enddo
       enddo

       return
       end
