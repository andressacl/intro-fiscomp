       PROGRAM pendulo_aprox

       implicit real*8(a-h,o-z)

       parameter(pi = dacos(-1d0))
       parameter(theta_0 = (60*pi/180d0))
       parameter(g = 9.8d0)
       parameter(p_l = 9.8d0) !l
       parameter(p_mass = 1d0)
       parameter(delta_t = 0.03d0)
      
       dimension f_0(1:3)
       dimension omega(1:3), theta(1:3)
       dimension omega_np1(1:3), theta_np1(1:3)  
       parameter(f_0 = (/0d0, 0.5d0, 1.2d0/))

 11    format(A4, 3A20) 
 12    format(f6.2, 3e20.11)

       open(10, FILE='saida-b4-theta-12610389.dat')
       open(20, FILE='saida-b4-omega-12610389.dat')

       gamma = 0.5d0
       freq = 2d0/3d0
       ratio_gl = (g/p_l)

       t_f = 102d0  
       N = t_f/(delta_t)
       t = 0d0

       do i = 1, 3
            theta(i) = theta_0
            omega(i) = 0d0
       end do

       write(10, 11) 't', 'Theta F_0 0', 'Theta F_0 0.5','Theta F_0 1.2'
       write(20, 11) 't', 'Omega F_0 0', 'Omega F_0 0.5','Omega F_0 1.2'

       write(10, 12) t, theta(1), theta(2), theta(3)
       write(20, 12) t, omega(1), omega(2), omega(3)
       
       do i = 1, N 
            t = t + delta_t

            do j = 1, 3                
                omega_np1(j) = omega(j) 
     $                         - ratio_gl*dsin(theta(j))*delta_t
     $                         - gamma*omega(j)*delta_t
     $                         + f_0(j)*dsin(freq*t)*delta_t
                    
                theta_np1(j) = theta(j) + omega_np1(j)*delta_t

                omega(j) = omega_np1(j)
                theta(j) = theta_np1(j)
            end do

            write(10, 12) t, mod(theta(1)+100*pi, -2*pi), 
     $      mod(theta(2)+100*pi, -2*pi),
     $      mod(theta(3)+100*pi, -2*pi)
            write(20, 12) t, omega(1), omega(2), omega(3)
       end do

       close(10)
       end program
