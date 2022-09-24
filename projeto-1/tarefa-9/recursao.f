       PROGRAM recursion_test
       
       n = 4
       max = 10
       ival = 1

       !call recursion(n, recursion)
       call factorial(n, ival, factorial)
       write(*,*) 'Fatorial de 4: ', ival

       end program recursion_test

       subroutine recursion(i, tmp_recursion)
           
           if(i .lt. 10) then 
               i = i+1
               write(*,*) 'n=', i
               call tmp_recursion(i, tmp_recursion)
            end if

        end


        subroutine factorial(n, ifact, aux_factorial)

            if (n == 1) then
                return
            else
                ifact = ifact*n
                      n = n-1
                write(*,*) n, ifact
                call aux_factorial(n, ifact, aux_factorial)
            end if

            return
        end 
