
      parameter(nmax=10000,nmax2=100000)
      real xin(nmax),yin(nmax),yin2(nmax),xp(15),xpos(15)
      real xl(2),yl(2),xswap(nmax)
      real xplot(nmax),yplot(nmax)
      character ima(nmax2)*6
      character c3*30,cdate*6,af(50)*6,cifu*3
      character a3*6,a4*3,a5*3,a6*2,aold*3,file1*30
      character adate(nmax)*6,aspec(nmax)*3,acon(nmax)*3,aamp(nmax)*2
      character aswap(nmax)*3
      character cfa(nmax2)*3,csa(nmax2)*3

      read *,cifu
      call gfplane(cifu,nt,ima,cfa,csa)
c- order is:
c listback2.dat
c listprof2.dat
c listfib2.dat
c listwave2.dat
c listf2f2.dat
c lista2a2.dat

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
      af(37)='202001'
      af(38)='202002'
      af(39)='202003'
      af(40)='202004'
      af(41)='202005'
      nmth=41
      open(unit=1,file='swapdate',status='old')
      nswap=0
      do i=1,nmax
         read(1,*,end=667) a3,x2
         nswap=nswap+1
         aswap(nswap)=a3
         xswap(nswap)=x2
      enddo
 667  continue
      close(1)

      call pgbegin(0,'?',1,1)
c      call pgpap(0.,1.)
      call pgscf(2)
      call pgsch(1.5)
      call pgslw(2)

      xmin=0.
      xmax=42.

      file1='listback2.dat'
      ymin=0.
      ymax=4.
      call get2(file1,cifu,nmth,af,nplot,xplot,yplot,aamp)
      call pgsch(0.8)
      call pgvport(0.10,0.49,0.65,0.95)
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('bcst',0.,0,'bcnst',0.,0)
      call pgsch(1.8)
      call pgmtxt('T',0.1,1.03,0.5,cifu)
      call pgsch(0.8)
      call pgmtxt('T',-1.5,0.5,0.5,"background Counts")
c      call pgenv(0.,38.,ymin,ymax,0,0)
      call plotswaps(nswap,aswap,xswap,cifu,ymin,ymax)
      do k=1,nplot
         call getic(aamp(k),ic)
         call pgsci(ic)
         yplot(k)=min(ymax,yplot(k))
         yplot(k)=max(ymin,yplot(k))
         call pgpt1(xplot(k),yplot(k),17)
      enddo
      call pgsci(1)

      file1='listprof2.dat'
      ymin=6.
      ymax=8.5
      call getprof(file1,cifu,nmth,af,nplot,xplot,yplot,aamp)
      call pgvport(0.51,0.90,0.65,0.95)
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('bcst',0.,0,'bcmst',0.,0)
      call pgmtxt('T',-1.5,0.5,0.5,"Fiber FWHM")
c      call pgenv(0.,38.,ymin,ymax,0,0)
      call plotswaps(nswap,aswap,xswap,cifu,ymin,ymax)
      do k=1,nplot
         call getic(aamp(k),ic)
         call pgsci(ic)
         yplot(k)=min(ymax,yplot(k))
         yplot(k)=max(ymin,yplot(k))
         call pgpt1(xplot(k),yplot(k),17)
      enddo
      call pgsci(1)

      file1='listfib2.dat'
      ymin=971.
      ymax=988.
      call get2(file1,cifu,nmth,af,nplot,xplot,yplot,aamp)
      call pgvport(0.10,0.49,0.35,0.63)
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('bcst',0.,0,'bcnst',0.,0)
      call pgmtxt('T',-1.5,0.5,0.5,"\gDFiberPos")
c      call pgenv(0.,38.,ymin,ymax,0,0)
      call plotswaps(nswap,aswap,xswap,cifu,ymin,ymax)
      do k=1,nplot
         call getic(aamp(k),ic)
         call pgsci(ic)
         yplot(k)=min(ymax,yplot(k))
         yplot(k)=max(ymin,yplot(k))
         call pgpt1(xplot(k),yplot(k),17)
      enddo
      call pgsci(1)

      file1='listwave2.dat'
      ymin=12.
      ymax=30.
      call get2(file1,cifu,nmth,af,nplot,xplot,yplot,aamp)
      call pgvport(0.51,0.90,0.35,0.63)
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('bcst',0.,0,'bcmst',0.,0)
      call pgmtxt('T',-1.5,0.5,0.5,"\gDWave")
c      call pgenv(0.,38.,ymin,ymax,0,0)
      call plotswaps(nswap,aswap,xswap,cifu,ymin,ymax)
      do k=1,nplot
         call getic(aamp(k),ic)
         call pgsci(ic)
         yplot(k)=min(ymax,yplot(k))
         yplot(k)=max(ymin,yplot(k))
         call pgpt1(xplot(k),yplot(k),17)
      enddo
      call pgsci(1)

      file1='listf2f2.dat'
      ymin=0.5
      ymax=1.5
      call get2(file1,cifu,nmth,af,nplot,xplot,yplot,aamp)
      call pgvport(0.10,0.49,0.10,0.34)
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('bcnst',0.,0,'bcnst',0.,0)
      call pgmtxt('T',-1.5,0.5,0.5,"F2F norm")
      call pgmtxt('B',2.1,0.5,0.5,"Months since 201701")
c      call pgenv(0.,38.,ymin,ymax,0,0)
      call plotswaps(nswap,aswap,xswap,cifu,ymin,ymax)
      do k=1,nplot
         call getic(aamp(k),ic)
         call pgsci(ic)
         yplot(k)=min(ymax,yplot(k))
         yplot(k)=max(ymin,yplot(k))
         call pgpt1(xplot(k),yplot(k),17)
         if(aamp(k).eq."LL") then
            do im=1,nt
               if(ima(im).eq.af(nint(xplot(k)))) then
                  yp=ymin+(ymax-ymin)/10.
                  if(k.eq.1) then
                     call pgptxt(2.,yp,0.,0.,csa(im))
                     aold=csa(im)
                  else
                     if(csa(im).ne.aold) then
                        call pgptxt(xplot(k)+1.,yp,0.,0.,csa(im))
                        aold=csa(im)
                     endif
                  endif
                  goto 669
               endif
            enddo
 669        continue
         endif
      enddo
      call pgsci(1)

      file1='lista2a2.dat'
      ymin=0.6
      ymax=1.5
      call get2(file1,cifu,nmth,af,nplot,xplot,yplot,aamp)
      call pgvport(0.51,0.90,0.10,0.34)
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('bcnst',0.,0,'bcmst',0.,0)
      call pgmtxt('T',-1.5,0.5,0.5,"A2A norm")
      call pgmtxt('B',2.1,0.5,0.5,"Months since 201701")
c      call pgenv(0.,38.,ymin,ymax,0,0)
      call plotswaps(nswap,aswap,xswap,cifu,ymin,ymax)
      do k=1,nplot
         call getic(aamp(k),ic)
         call pgsci(ic)
         yplot(k)=min(ymax,yplot(k))
         yplot(k)=max(ymin,yplot(k))
         call pgpt1(xplot(k),yplot(k),17)
      enddo
      call pgsci(1)

      call pgend

      end

      subroutine plotswaps(nswap,aswap,xswap,cifu,ymin,ymax)
      real xswap(nswap),xl(2),yl(2)
      character aswap(nswap)*3,cifu*3
      call pgsci(1)
      yl(1)=ymin
      yl(2)=ymax
      do i=1,nswap
         if(aswap(i).eq.cifu) then
            xl(1)=xswap(i)
            xl(2)=xl(1)
            call pgline(2,xl,yl)
         endif
      enddo
      return
      end

      subroutine getic(aamp,ic)
      character aamp*2
      if(aamp.eq."LL") ic=1
      if(aamp.eq."LU") ic=2
      if(aamp.eq."RL") ic=3
      if(aamp.eq."RU") ic=4
      return
      end

      subroutine get2(file1,cifu,nmth,af,nplot,xplot,yplot,aamp)
      parameter(nmax=10000)
      real xplot(nmax),yplot(nmax),xpos(15)
      character file1*30,c3*30,cdate*6,af(50)*6,a3*6
      character aamp(nmax)*2,a6*2,cifu*3,a4*3,a5*3

      open(unit=1,file=file1,status='old')
      nplot=0
      do i=1,10000
         read(1,*,end=777) x1,x2,a3,a4,a5,a6
         if(cifu.eq.a4) then
            nplot=nplot+1
            do k=1,nmth
               if(af(k).eq.a3) then
                  xplot(nplot)=float(k)
               endif
            enddo
            yplot(nplot)=x1
            aamp(nplot)=a6
         endif
      enddo
 777  continue
      close(1)

      return
      end

      subroutine getprof(file1,cifu,nmth,af,nplot,xplot,yplot,aamp)
      parameter(nmax=10000)
      real xplot(nmax),yplot(nmax),xp(15),xpos(15)
      character file1*30,c3*30,cdate*6,af(50)*6,a3*6
      character aamp(nmax)*2,a6*2,cifu*3,a4*3

      np=15
      do i=1,np
         xpos(i)=-7.+float(i-1)
      enddo

      open(unit=1,file=file1,status='old')
      nplot=0
      do i=1,10000
         read(1,*,end=777) x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
     $        x11,x12,x13,x14,x15,a3,a4,a6
         if(cifu.eq.a4) then
            xp(1)=x1
            xp(2)=x2
            xp(3)=x3
            xp(4)=x4
            xp(5)=x5
            xp(6)=x6
            xp(7)=x7
            xp(8)=x8
            xp(9)=x9
            xp(10)=x10
            xp(11)=x11
            xp(12)=x12
            xp(13)=x13
            xp(14)=x14
            xp(15)=x15
            call getfwhm(15,xpos,xp,0.5,xfwhm,pmax,ymax,v1,v2)
            nplot=nplot+1
            do k=1,nmth
               if(af(k).eq.a3) then
                  xplot(nplot)=float(k)
               endif
            enddo
            yplot(nplot)=xfwhm
            aamp(nplot)=a6
         endif
      enddo
 777  continue
      close(1)

      return
      end

      subroutine getfwhm(n,x,y,frac,fwhm,xmax2,ymax2,v1,v2)
      real x(n),y(n),y2(10000)

      data big /1.e20/

c      call spline(x,y,n,0.,0.,y2)

      ymax=-big
      ymax2=-big
      do i=1,n-1
         do ia=1,9
            xp=x(i)+float(ia-1)/9.*(x(i+1)-x(i))
c            call splint(x,y,y2,n,xp,yp)
            if(yp.gt.ymax2) then
               ymax2=yp
               xmax2=xp
            endif
         enddo
         if(y(i).gt.ymax) then
            ymax=y(i)
            imax=i
         endif
      enddo

      ymax2=ymax
      xmax2=x(imax)
      yhalf=ymax2*frac

      diff=big
      x1=x(1)
      do i=1,imax-1
         if(yhalf.ge.y(i).and.yhalf.lt.y(i+1)) then
            x1=x(i)+(yhalf-y(i))/(y(i+1)-y(i))*(x(i+1)-x(i))
         endif
      enddo

      diff=big
      x2=x(n)
      do i=imax,n-1
         if(yhalf.ge.y(i+1).and.yhalf.lt.y(i)) then
            x2=x(i+1)+(yhalf-y(i+1))/(y(i)-y(i+1))*(x(i)-x(i+1))
         endif
      enddo

      fwhm=x2-x1

c - get 1st and second moment:

      sum1=0.
      sum2=0.
      do i=1,n
         sum1=sum1+x(i)*y(i)
         sum2=sum2+y(i)
      enddo
      v1=sum1/sum2
      sum1=0.
      sum2=0.
      do i=1,n
         sum1=sum1+y(i)*(x(i)-v1)**2
         sum2=sum2+y(i)
      enddo
      v2=sqrt(sum1/sum2)

      return
      end

      subroutine gfplane(cifu0,nt,ima,cfa,csa)
      parameter(nmax=100000)
      character ima(nmax)*6,cifu0*3
      character file1*100,cifu*3,cspec*3,cnum*3,camp*5
      character cfa(nmax)*3,csa(nmax)*3

      open(unit=1,file='listall',status='old')

      nt=0
      do i=1,500
         read(1,*,end=666) file1
         open(unit=2,file=file1,status='old')
         do j=1,6
            read(2,*)
         enddo
         do j=1,100
            read(2,*,end=667) cifu,x2,x3,cspec
            if(cifu.eq.cifu0) then
               nt=nt+1
               ima(nt)=file1(40:47)
               cfa(nt)=cifu
               csa(nt)=cspec
            endif
         enddo
 667     continue
         close(2)
      enddo
 666  continue
      close(1)

      return
      end
