       PROGRAM pendulo_comparacao

       implicit real*8(a-h,o-z)

       parameter(pi = dacos(-1d0))
       parameter(theta_0 = (6*pi/18d0))
       parameter(g = 9.8d0)
       parameter(p_l = 9.8d0) !l
       parameter(p_mass = 1d0)
       parameter(delta_t = 0.03d0)

 11    format(f6.2, e30.11)
 12    format(A4, A30)

       open(10, FILE='saida-c-12610389.dat')

       gamma = 0.5d0
       f_0 = 1.2d0
       freq = 2d0/3d0

       t_f = 102d0
       N = t_f/(delta_t)
       t = 0d0

       theta1 = theta_0
       theta2 = theta1 + 0.001d0
       omega1 = 0d0
       omega2 = omega1
       ratio_gl = (g/p_l)

       write(10, *) 'F_0 = ', f_0 
       write(10, 12) 't', 'log(delta_t)'

       do i = 1, N 
            t = t + delta_t
                
            omega1_np1 = omega1 - ratio_gl*dsin(theta1)*delta_t
     $      - gamma*omega1*delta_t +f_0*dsin(freq*t)*delta_t
                
            theta1_np1 = theta1 + omega1_np1*delta_t

            omega2_np1 = omega2 - ratio_gl*dsin(theta2)*delta_t
     $      - gamma*omega2*delta_t +f_0*dsin(freq*t)*delta_t
                
            theta2_np1 = theta2 + omega2_np1*delta_t

            write(10, 11) t, dlog(abs(theta2_np1 - theta1_np1))

            omega1 = omega1_np1
            theta1 = theta1_np1

            omega2 = omega2_np1
            theta2 = theta2_np1
       end do
  
       close(10)
       end program
