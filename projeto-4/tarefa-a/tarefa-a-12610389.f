       PROGRAM pendulo_aprox

       implicit real*8(a-h,o-z)

       parameter(pi = dacos(-1d0))
       parameter(g = 9.8d0)
       parameter(p_l = 9.8d0) !l
       parameter(p_mass = 1d0)
       parameter(delta_t = 0.1d0)

 11    format(A4, A30, A34)
 12    format(f6.2, 2e30.11)

       open(10, FILE='saida-a-solucoes-12610389.dat')
       open(20, FILE='saida-a-energia-12610389.dat')

       t_f = 100
       N = t_f/delta_t
       t = 0

       theta_1 = pi/36d0
       omega_1 = 0d0

       theta_2 = theta_1
       omega_2 = 0d0

       ratio_gl = (g/p_l)

       write(10, 11) 't', 'theta (Euler)', 'theta (Euler-Cromer)'
       write(20, 11) 't', 'Energia (Euler)', 'Energia (Euler-Cromer)'

       do i = 1, N
            t = delta_t + t

            !Método Euler
            omega1_np1 = omega_1 - ratio_gl*theta_1*(delta_t)
            theta1_np1 = theta_1 + omega_1*(delta_t)
            
            !Método Euler-Cromer
            omega2_np1 = omega_2 - ratio_gl*theta_2*delta_t
            theta2_np1 = theta_2 + omega2_np1*delta_t

            !Cálculo da energia
            e1 = energia(g, p_mass, p_l, omega1_np1, theta1_np1)
            e2 = energia(g, p_mass, p_l, omega2_np1, theta2_np1)

            write(10, 12) t, theta1_np1, theta2_np1
            write(20, 12) t, e1, e2

            omega_1 = omega1_np1
            theta_1 = theta1_np1

            omega_2 = omega2_np1
            theta_2 = theta2_np1
       end do

       close(10)
       close(20)

       end program

       function energia(g, p_mass, p_l, omega, theta)
       implicit real *8(a-h, o-z)

       energia = 0.5d0*p_mass*omega**2+(1d0/2d0*p_l)*p_mass*g*theta**2
       
       return
       end function
