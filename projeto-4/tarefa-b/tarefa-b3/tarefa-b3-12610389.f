       PROGRAM pendulo_amortecido

       implicit real*8(a-h,o-z)

       parameter(pi = dacos(-1d0))
       parameter(theta_0 = (60*pi/180d0))
       parameter(g = 9.8d0)
       parameter(p_l = 9.8d0) !l
       parameter(p_mass = 1d0)
       parameter(delta_t = 0.01d0)

 11    format(A4, A16) 
 12    format(f6.2, e20.11)

       open(10, FILE='saida-b3-12610389.dat')

       gamma = 0.5d0

       t_f = 100d0
       N = t_f/(delta_t)
       t = 0d0

       theta = theta_0
       omega = 0d0
       ratio_gl = (g/p_l)

       write(10, 11) 't', 'theta' 
       write(10, 12) t, theta

       do i = 1, N 
            t = t + delta_t
            
            omega_np1 = omega - ratio_gl*dsin(theta)*delta_t
     $      - gamma*omega*delta_t
            
            theta_np1 = theta + omega_np1*delta_t

            write(10, 12) t, theta_np1

            omega = omega_np1
            theta = theta_np1
       end do

       close(10)
       end program
