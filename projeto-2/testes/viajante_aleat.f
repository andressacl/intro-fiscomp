       PROGRAM random_walk
       parameter (p=0.5e0)
       parameter (q=0.5e0)

       write(*,*) 'Insira o num. de passos:'
       read(*,*) N

       nd = 0
       ne = 0

       do i = 1, N
            ipp = 2*rand()
            nd = nd+ipd(ipp)
            ne = ne+ipe(ipp)
       enddo

                !nd = nd+ip(ipp)i
                !ip(o)=o (vazio), ip(1)=1
                !ipx(1)=1 ipx(2)=0
                !ipy(1)=0 ipy(2)=1
                !bidim: 4*rand()+1
                !ix = ix + ipx(ipp)
                !iy=iy+ipy(ipp)
                !ii = ix/delta x (para o histograma

       enddo
   
       stop
       end

       function ipd(ipp)

           if (ipp == 

       return
       end
