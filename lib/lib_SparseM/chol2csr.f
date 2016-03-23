      subroutine chol2csr(nrow,nnzlindx,nsuper,lindx,xlindx,nnzl,lnz,
     &                    xlnz,dim,ra,ia,ja)
      integer nrow, nnzlindx, nsuper, lindx(nnzlindx), xlindx(nrow+1),
     &        nnzl, xlnz(nrow+1), dim(2), ia(nrow+1), ja(nnzl),
     &        place, i, j, k, lindx2(nnzlindx+1)
      double precision lnz(nnzl), ra(nnzl) 

c  first, the easy ones
      dim(1) = nrow
      dim(2) = nrow

      do 100 i = 1, nnzl
         ra(i) = lnz(i)
 100  continue

      do 150 i = 1, (nnzlindx)
         lindx2(i) = lindx(i)
 150     continue
         
      lindx2(nnzlindx+1) = nrow+1

c lindx2 is a copy of lindx, but with an extra entry that allows the 
c loop below not to over-run the array

      do 200 i = 1, (nrow+1)
         ia(i) = xlnz(i)
 200  continue
      
c  initialization
      place = 1

c  fill out the ja slot
      do 300 i = 1, nsuper
         do 250 j = 0, (lindx2(xlindx(i+1)) - lindx2(xlindx(i))-1)
            do 225 k = (xlindx(i)+j), (xlindx(i+1)-1)
               ja(place) = lindx2(k)
c               write(*,*) 'i =', i, ' j =', j,' k =', k,' place =',place
               place = place + 1
 225            continue
 250         continue
 300     continue

      END subroutine



