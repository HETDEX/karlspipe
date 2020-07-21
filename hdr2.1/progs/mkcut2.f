
      real xin(14),xin2(4)
      integer is(10000),ie(10000)
      character amp(10000)*20,a1*20,adum2*50
      character file1*120,afield*12,aspec*32,aexp*5,adum*50,an1*6

      open(unit=1,file='listbad',status='old')
      read(1,*)
      nb=0
      do i=1,1000
         read(1,*,end=666) idum,adum,a1,i1,i2
         nb=nb+1
         amp(nb)=a1
         is(nb)=i1
         ie(nb)=i2
      enddo
 666  continue
      close(1)

      open(unit=1,file='in',status='old')
      open(unit=11,file='out',status='unknown')
      open(unit=12,file='out2',status='unknown')

      do iall=1,100000
         read(1,*,end=668) adum,x2,x3,i4,i5,i6,i7,afield
         id=i7
         call getaname(id,an1,in1)
         file1="spec/"//afield//"_"//an1(1:in1)//".list"
         open(unit=2,file=file1,status='old',err=877)
         do i=1,100
            read(2,*,end=877) x1,x2,x3,x4,aspec,adum,x7,x8,
     $        adum2,idate,ishot,idum,idum2,xw
            if(xw.gt.0.09) then
               do j=1,nb
                  if(idate.ge.is(j).and.idate.le.ie(j).and.
     $                 aspec(1:20).eq.amp(j)) then
                     write(12,1104) id
                     goto 888
                  endif
               enddo
            endif
         enddo
 877     continue
         write(11,1104) id
 888     continue
         close(2)
      enddo
 668  continue
      close(1)
      close(11)
      close(12)

 1001 format(i8)
 1104 format(i6)

      end

      subroutine getaname(i1,an1,in1)
      character an1*6
      if(i1.lt.10) then
         write(an1(1:1),1001) i1
         in1=1
      endif
      if(i1.ge.10.and.i1.lt.100) then
         write(an1(1:2),1002) i1
         in1=2
      endif
      if(i1.ge.100.and.i1.lt.1000) then
         write(an1(1:3),1003) i1
         in1=3
      endif
      if(i1.ge.1000.and.i1.lt.10000) then
         write(an1(1:4),1004) i1
         in1=4
      endif
      if(i1.ge.10000.and.i1.lt.100000) then
         write(an1(1:5),1005) i1
         in1=5
      endif
      if(i1.ge.100000.and.i1.lt.1000000) then
         write(an1(1:6),1006) i1
         in1=6
      endif
 1001 format(i1)
 1002 format(i2)
 1003 format(i3)
 1004 format(i4)
 1005 format(i5)
 1006 format(i6)
      return
      end
