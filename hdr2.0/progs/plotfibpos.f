
      real xin(1000),yin(1000),yin2(1000)
      character file1*30,c3*30,cdate*6,af(50)*6

      af(1)='201701'
      af(2)='201702'
      af(3)='201703'
      af(4)='201704'
      af(5)='201705'
      af(6)='201706'
      af(7)='201707'
      af(8)='201708'
      af(9)='201709'
      af(10)='201710'
      af(11)='201711'
      af(12)='201712'
      af(13)='201801'
      af(14)='201802'
      af(15)='201803'
      af(16)='201804'
      af(17)='201805'
      af(18)='201806'
      af(19)='201807'
      af(20)='201808'
      af(21)='201809'
      af(22)='201810'
      af(23)='201811'
      af(24)='201812'
      af(25)='201901'
      af(26)='201902'
      af(27)='201903'
      af(28)='201904'
      af(29)='201905'
      af(30)='201906'
      af(31)='201907'
      af(32)='201908'
      af(33)='201909'
      af(34)='201910'
      af(35)='201911'
      af(36)='201912'

      call pgbegin(0,'?',1,1)
      call pgpap(0.,1.)
      call pgscf(2)
      call pgsch(1.5)
      call pgslw(2)

c      call pgenv(0.,37.,980.,1001.,0,0)
c      call pgenv(0.,37.,988.,996.,0,0)
c      call pglabel("months since 201701","delta pixel","")
      call pgenv(0.,37.,15.,27.,0,0)
      call pglabel("months since 201701","delta wavelength","")

      open(unit=1,file='list2',status='old')
      ntot=0
      do i=1,1000
         read(1,*,end=777)
         ntot=ntot+1
      enddo
 777  continue
      rewind(1)

      ic=0
      do i=1,1000
         read(1,*,end=666) file1
         open(unit=2,file=file1,status='old')
         nin=0
         ymin=1e10
         ymax=-ymin
         do j=1,100
            read(2,*,end=667) x1,x2,c3
            cdate=c3(1:6)
             do k=1,36
               if(cdate.eq.af(k)) then
                  nin=nin+1
                  xin(nin)=float(k)
c                  yin(nin)=x2
c                  yin2(nin)=x2
                  yin(nin)=abs(x2)
                  yin2(nin)=yin(nin)
                  ymin=min(ymin,x2)
                  ymax=max(ymax,x2)
               endif
            enddo
         enddo
 667     continue
         close(2)
         call moment(yin2,nin,ave,adev,sdevf,var,skew,curt)
         call biwgt(yin2,nin,xb,xs)
         if(ntot.le.4) then
            ic=ic+1
            if(ic.eq.15) ic=1
            call pgsci(ic)
            call pgpt(nin,xin,yin,17)
            if(ic.eq.1) call pgmtxt('T',-1.5,0.5,0.5,file1(2:4))
         else
            if(ymax-ymin.lt.1.5.and.adev.lt.0.4) then
               ic=ic+1
               if(ic.eq.15) ic=1
               call pgsci(ic)
               call pgline(nin,xin,yin)
            endif
         endif
         print *,xs,adev,sdevf,file1
      enddo
 666  continue
      close(1)
      call pgend

      end
