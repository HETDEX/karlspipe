      
      integer it(10000)
      character a1*15,fname*5,a3*3

      open(unit=1,file='inlist',status='old')
      nt=0
      do i=1,10000
         read(1,*,end=666) a1,x2,i3
         if(i.eq.1) then
            nt=nt+1
            it(nt)=i3
            goto 667
         else
            do j=1,nt
               if(i3.eq.it(j)) goto 667
            enddo
         endif
         nt=nt+1
         it(nt)=i3
 667     continue
      enddo
 666  continue
      rewind(1)

      open(unit=3,file='olist',status='unknown')
      fname="list0"
      do j=1,nt
         a3="000"
         iounit=10+j
         write(fname(5:5),1001) j
         if(it(j).lt.10) write(a3(3:3),2001) it(j)
         if(it(j).ge.10.and.it(j).lt.100) write(a3(2:3),2002) it(j)
         if(it(j).ge.100) write(a3(1:3),2003) it(j)
         write(3,1003) fname,a3
         open(unit=iounit,file=fname,status='unknown')
         do i=1,10000
            read(1,*,end=668) a1,x2,i3
            if(i3.eq.it(j)) then
               write(iounit,*) a1,x2,i3
            endif
         enddo
 668     continue
         rewind(1)
      enddo

      close(3)
      close(1)

 1001 format(i1)
 2001 format(i1)
 2002 format(i2)
 2003 format(i3)
 1003 format(a5,1x,a3)
      end

         
