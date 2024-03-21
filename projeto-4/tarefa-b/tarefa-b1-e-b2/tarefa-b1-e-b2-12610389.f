       PROGRAM pendulo_aprox

       implicit real*8(a-h,o-z)

       parameter(pi = dacos(-1d0))
       parameter(theta_0 = (10*pi/180d0))
       parameter(g = 9.8d0)
       parameter(p_l = 9.8d0) !l
       parameter(p_mass = 1d0)
       parameter(delta_t = 0.01d0)
       
       t_f = 100d0
       N = t_f/(delta_t)
       t = 0d0

       theta = theta_0
       omega = 0d0
       ratio_gl = (g/p_l)

       sum = 0d0
       icount = 0
       t_ant = 0d0

       write(*,*) 'θ inicial = ', theta_0
       write(*,*) 'Tempo simulado:', t_f

       do i = 1, N 
            t = t + delta_t

            omega_np1 = omega - ratio_gl*dsin(theta)*delta_t
            theta_np1 = theta + omega_np1*delta_t

            if (omega_np1*omega .lt. 0) then
                sum = sum + (t-t_ant) !Medindo o meio período 
                t_ant = t
                icount = icount + 1
            end if

            omega = omega_np1
            theta = theta_np1
       end do

       write(*, *) 'Tn médio =', 2d0*(sum/icount*1d0)

       !Integral elíptica
       epsilon = 0.001d0
       a = -abs(theta_0) + epsilon
       b = abs(theta_0) - epsilon
       h = (b-a)/N
       area = 0d0

       do i=0, N-4, 4
            x0 = a+i*h

            f0 = f(x0, 0, h, theta_0)
            f1 = f(x0, 1, h, theta_0)
            f2 = f(x0, 2, h, theta_0)
            f3 = f(x0, 3, h, theta_0)
            f4 = f(x0, 4, h, theta_0)

            area = area +((2*h)/45d0)*(7d0*f0+32d0*f1+
     $             12d0*f2+32d0*f3+7d0*f4) 
       end do

       area = sqrt(2*p_l/g)*(area+4d0*sqrt(epsilon/dsin(theta_0)))
       write(*,*) 'Tn (integral) =', area

       !Aprox. para ângulos pequenos
       per_aprox = 2d0*pi*sqrt(p_l/g)*(1+((theta_0**2)/16d0))
       write(*,*) 'Tn (aproximação) =', per_aprox 

       close(10)

       end program

       function f(x, n, h, theta_0)
            implicit real*8(a-h,o-z)

            x_n = x+n*h
            f = 1/sqrt(dcos(x_n)-dcos(theta_0))

       return
       end function
