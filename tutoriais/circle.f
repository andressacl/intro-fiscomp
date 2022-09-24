       program circle
               
       real r, area, pi 
       parameter (pi = 3.14159)

c      Esse programa lê um número real r e retorna a área de um
c      círculo de raio r.

       write (*,*) 'Give radius r:'
       read (*,*) r
       area = pi*(r**2)
       write (*,*) 'Area = ', area


       stop
       end


