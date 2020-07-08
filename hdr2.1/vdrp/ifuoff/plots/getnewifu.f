
      real xa(100),ya(100),xs(100),xpn(100),ypn(100)
      integer ia(100)
      character a1*3,a4*3,a5*3,a6*6,aifu(100)*3,aifun(100)*3

      open(unit=1,file='offall',status='old')
      n=0
      do i=1,100
         read(1,*,end=666) a1,x2,x3,i4,x5,x6
         n=n+1
         aifu(n)=a1
         xa(n)=x2
         ya(n)=x3
         ia(n)=i4
         xs(n)=sqrt((x5*x5+x6*x6)/2.)
      enddo
 666  continue
      close(1)

      open(unit=1,file='old',status='old')
      open(unit=11,file='out',status='unknown')
      open(unit=12,file='out3',status='unknown')
      ngood=10
      siggood=0.65
      do i=1,100
         read(1,*,end=667) a1,x2,x3,a4,a5,a6,x7,x8
         do j=1,n
            if(a1.eq.aifu(j)) then
               if(ia(j).gt.ngood.and.xs(j).lt.siggood) then
                  write(11,1101) a1,x2-ya(j),x3-xa(j),a4,a5,a6,x7,x8
                  write(12,1103) a1,x2-ya(j),x3-xa(j),a4,a5,a6,x7,x8,
     $                 xs(j),ia(j)
                  xpn(i)=x2-ya(j)
                  ypn(i)=x3-xa(j)
                  aifun(i)=a1
               else
                  write(11,1101) a1,x2,x3,a4,a5,a6,x7,x8
                  write(12,1103) a1,x2,x3,a4,a5,a6,x7,x8,xs(j),-ia(j)
               endif
               goto 555
            endif
         enddo
         xpn(i)=x2
         ypn(i)=x3
         aifun(i)=a1
 555     continue
      enddo
 667  continue
      close(1)
      close(11)
      close(12)

      open(unit=1,file='nominal',status='old')
      open(unit=11,file='out2',status='unknown')
      ngood=10
      sigggod=0.65
      do i=1,100
         read(1,*,end=668) a1,x2,x3
         do j=1,100
            if(a1.eq.aifun(j)) then
               write(11,1102) a1,x2-xpn(j),x3-ypn(j)
            endif
         enddo
      enddo
 668  continue
      close(1)
      close(11)

 1101 format(a3,2(2x,f8.3),3(2x,a3),2(2x,f7.3))
 1102 format(a3,2(2x,f8.3),1x,f6.2,1x,i5)
 1103 format(a3,2(2x,f8.3),3(2x,a3),2(2x,f7.3),1x,f6.2,1x,i5)

      end
