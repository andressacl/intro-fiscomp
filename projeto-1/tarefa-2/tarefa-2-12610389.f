       PROGRAM prisma

       dimension v1(1:3), v2(1:3), v3(1:3), v4(1:3)
       dimension pvet12(1:3), pvet14(1:3), pvet24(1:3)

       write(*,*) 'Insira o primeiro vetor:'
       read(*,*) v1(1), v1(2), v1(3)

       write(*,*) 'Insira o segundo vetor:'
       read(*,*) v2(1), v2(2), v2(3)

       write(*,*) 'Insira o terceiro vetor:'
       read(*,*) v3(1), v3(2), v3(3)

       do 10 i = 1, 3
            v4(i) = v2(i)-v3(i)
 10    continue

       !write(*,*) 'Vetor resultante:', (v4(i), i = 1, 3)
       !area da base:

       call calc_produto_vet(v1, v2, pvet12)
       !write(*,*) 'Produto vetorial 12:', (pvet12(i), i = 1, 3)

       area_base = sqrt(pvet12(1)**2 + pvet12(2)**2 + pvet12(3)**2)
       alt = abs(pvet12(1)*v4(1) + pvet12(2)*v4(2) + pvet12(3)*v4(3))
       volume = area_base*alt

       !calculando a área lateral:
       call calc_produto_vet(v1, v4, pvet14)
       call calc_produto_vet(v2, v4, pvet24)

       area_lat1 = sqrt(pvet14(1)**2 + pvet14(2)**2 + pvet14(3)**2)
       area_lat2 = sqrt(pvet24(1)**2 + pvet24(2)**2 + pvet24(3)**2)

       area_superficial = 2*area_base + 2*area_lat1 + 2*area_lat2

       write(*,*) 'Área lateral total:', area_superficial
       write(*,*) 'Volume: ', volume

       end program


       subroutine calc_produto_vet(a, b, res)
       dimension a(1:3), b(1:3), res(1:3)

       res(1) = a(2)*b(3) - a(3)*b(2)
       res(2) = a(3)*b(1) - a(1)*b(3)
       res(3) = a(1)*b(2) - a(2)*b(1)

       return
       end
