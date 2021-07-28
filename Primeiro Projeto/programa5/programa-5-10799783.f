      program main
      
          real x
          real*8 xd
          real xn
          real*8 xnd
          real ln
          real*8 lnd
          integer n
          integer m
          real eprec
          real*8 eprecd
          integer counter
          integer counterd
          real*8 ten
          
          write(*,*) "Deseja calcular o ln de qual número?"
          read(*,*) x
          
          xn = x
          ln = 0
          n = 1
          eprec = 0.00001
          counter = 0
          
          xd = x
          xnd = xd
          lnd = 0.0
          m = 1
          eprecd = 1D-15
          counterd = 0
          ten = 10
C        O máximo que consegue-se extrair de precisao é de até 1D-16, para 
C        |1 -xn| tendendo a 1, o máximo que a máquina consegue computar nessa 
C        precisão, com a mesma diminuindo com |~| tendendo a 0. Abaixo de 1D-16,
C        o while que calcula o Ln entra em um loop eterno pois a condição de 
C        parada nunca será atingida, afinal com os arredondamentos da precisão 
C        dupla, a condição sempre acusará que os valores de dlog e Ln são  
C        equivalentes. Para valores mais precisos, seriam necessários valores 
C        de precisão quádrupla. 
          
          
          
          if (x .GE. 1) then
              do while (xn .GE. 1)
                  xn = xn/10
                  counter = counter + 1
              end do
          end if    
          do while (abs(log(xn) - ln) .GT. eprec)
              ln = ln - ((1 - xn)**n)/n
              n = n + 1                
          end do
          ln = ln + counter*log(10.0)
          write(*,*) "Calculado manualmente: ", ln
          write(*,*) "Valor tabelado:        ", log(x)
           
          
          write(*,*) " "
          write(*,*) "Agora em dupla precisão"
          
          
          if (xd .GE. 1) then
              do while (xnd .GE. 1)
                  xnd = xnd/10
                  counterd = counterd + 1
              end do
          end if    
          do while (abs(dlog(xnd) - lnd) .GT. eprecd)
              lnd = lnd - ((1 - xnd)**m)/m
              m = m + 1                
          end do
          write(*,*) counterd
          lnd = lnd + counterd*dlog(ten)
          write(*,*) "Calculado manualmente: ", lnd
          write(*,*) "Valor tabelado:        ", dlog(xd)
          
          
          
      end program main