        PROGRAM media_aleat
        dimension sum(1:4)
        
        write(*,*) 'Insira N:'
        read(*,*) N

        do i = 1, 4
            sum(i) = 0.0
        end do

        do j = 1, N
            x = rand()
            do i = 1, 4
                sum(i) = sum(i) + x**i
            end do
        end do

       do i = 1, 4
            sum(i) = sum(i)/N
       end do

       write(*,*) '<x> = ', sum(1) 
       write(*,*) '<x²> =', sum(2)
       write(*,*) '<x³> =', sum(3) 
       write(*,*) '<x⁴> =', sum(4)

       stop 
       end
