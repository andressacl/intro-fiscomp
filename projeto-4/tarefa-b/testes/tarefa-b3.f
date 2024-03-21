       PROGRAM pendulo_aprox

       implicit real*8(a-h,o-z)

       parameter(pi = dacos(-1d0))
       parameter(theta_0 = (7*pi/18d0))
       parameter(g = 9.8d0)
       parameter(p_l = 9.8d0) !l
       parameter(p_mass = 1d0)
       parameter(delta_t = 0.01d0)

 11    format(A4, f6.2, A24, e20.11) 
 12    format(f6.2, e20.11)
 13    format(f6.2, 2e30.11)

       open(10, FILE='saida-b3.dat')

       gamma = 0.5d0

       t_f = 50d0
       N = t_f/(delta_t)
       t = 0d0

       theta = theta_0
       omega = 0d0
       ratio_gl = (g/p_l)

       sum = 0d0
       icount = 0
       t_ant = 0d0

       write(10, *) t, theta

       do i = 1, N 
            t = t + delta_t

            theta = mod(theta, 2d0*pi)
            
            omega_np1 = omega - ratio_gl*dsin(theta)*delta_t
     $      - gamma*omega*delta_t
            
            theta_np1 = theta + omega_np1*delta_t

            write(10, *) t, theta_np1

            omega = omega_np1
            theta = theta_np1
       end do

       close(10)

       end program
