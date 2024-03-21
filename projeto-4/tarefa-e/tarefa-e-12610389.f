       PROGRAM pendulo_poincare

       implicit real*8(a-h,o-z)

       parameter(pi = dacos(-1d0))
       parameter(theta_0 = (6*pi/18d0))
       parameter(g = 9.8d0)
       parameter(p_l = 9.8d0)
       parameter(p_mass = 1d0)
       parameter(delta_t = 0.03d0)

       dimension omega(1:3), theta(1:3)
       dimension omega_np1(1:3), theta_np1(1:3)  

 11    format(2e30.11)

       open(10, FILE='saida-e-p1-12610389.dat')
       open(20, FILE='saida-e-p2-12610389.dat')
       open(30, FILE='saida-e-p3-12610389.dat')

       gamma = 0.5d0
       f_0 = 1.2d0      !variar parâmetro
       freq = 2d0/3d0

       t_f = 30000d0
       N = t_f/(delta_t)
       t = 0d0

       theta(1) = theta_0 - 0.001d0
       theta(2) = theta_0
       theta(3) = theta_0 + 0.001d0

       do i = 1, 3
            omega(i) = 0d0

            index = i*10
            write(index, 11) theta(i), omega(i) 
       end do

       ratio_gl = (g/p_l)

       do i = 1, N 
            t = t + delta_t

            do j = 1, 3
                
            omega_np1(j) = omega(j) - ratio_gl*dsin(theta(j))*delta_t
     $      - gamma*omega(j)*delta_t + f_0*dsin(freq*t)*delta_t
                
            theta_np1(j) = theta(j) + omega_np1(j)*delta_t

            omega(j) = omega_np1(j)
            theta(j) = theta_np1(j)

            end do

            if(abs(mod(freq*t, pi)) .lt. delta_t/2) then 
                write(10, 11) mod(theta(1)+100d0*pi, -2d0*pi), omega(1)
                write(20, 11) mod(theta(2)+100d0*pi, -2d0*pi), omega(2)
                write(30, 11) mod(theta(3)+100d0*pi, -2d0*pi), omega(3)
            end if
       end do
  
       close(10)
       close(20)
       close(30)

       end program
