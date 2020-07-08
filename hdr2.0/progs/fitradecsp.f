
      parameter(nmax=1500,nfmax=1500,nmaxs=70000)
c- nmax is spectral; nfmax is fiber spectra; nmaxs is positions
      real ara(nmaxs),adec(nmaxs),awave(nmaxs)
      real sflux(nfmax),sfluxe(nfmax),wa(nfmax,nmax)
      real fa(nfmax,nmax),fea(nfmax,nmax),xw(nfmax),az(nfmax)
      real fadcw(nfmax,5),fweight(nfmax,nmax),sumrata(nmax)
      real fa2(nfmax,nmax),fea2(nfmax,nmax),weight(nfmax)
      real fweight2(nfmax,nmax),fitout(3000,6)
      real raf(nfmax),decf(nfmax),specexp(nmax,3)
      real fitcon(nmax),fitw(nmax),spec(nmax*10,9),fitchi(nmax)
      real fitsig(nmax),fitamp(nmax),fitsn(nmax)
      real rat(nmaxs*5),dect(nmaxs*5),fitwt(nmaxs*5),fitsnt(nmaxs*5)
      real fitp(nmaxs*5,8),fitchit(nmaxs*5),fitampt(nmaxs*5)
      real fitcont(nmaxs*5),fitsigt(nmaxs*5),flima(1036),xin(nmax)
      real tarray(2),xoutv(17),xouts(17),flimw(1036),flimv(1036)
      real xin1(nmax),xin2(nmax),xin3(nmax)
      real raem(10000),decem(10000),waveem(10000)
      integer iflag(nfmax),intuse(nfmax),imatchd(nmaxs),iexp(nfmax)
      integer nwa(nfmax)
      character cfield*12
      common/cfwhm/ ifwf,fwin

c- ifit1=  1 fit around the input line only, fit sigma, s/n>0
c          2 fit full spectrum, force sigma, keep s/n>3.5, write out binned spectrum
c          3 fit full spectrum, force sigma, and keep s/n>3.5
c          0 continuum?
c         -1 best spatial position by minimizing chi^2
c         -2 best fwhm by minimizing chi^2
c        101 spectra within and then ifit=1 at input wavelength
c        102 spectra within, then ifit=1, then run mc at input wavelength, then do mc on best
c        103 spectra within and then ifit=3
c        104 spectra within, and then fit at every fiber and wavelength in IFU, then do mc on best
c        105 flux limit at each fiber center (enter with .... stepsize 1 105)
c        106 just generate calib and calibe fits files for all exp and amps, and then exit
c        107 fit spatial (like -1 before)
c        108 fit fwhm (like -2 before)
c        109 same as 2
c        110 simulation at individual input and extract at just that input
c        111 simualtion at input list and then run as 104, full IFU search
c        2?? same as 1??, but check for high linewidths
c       -100 or less means use native spectral binning

      rd=3.
      wd=4.
      sncut0=4.0
      ifwf=0
      call getfwhm(rfw)

      read *,ra,dec,step,nstep,wcen,wrange,ifit1

c- inative=0 means rectify, inative=1 means native
      inative=0
      if(ifit1.le.-100) then
         ifit1=-ifit1
         inative=1
      endif

      isighi=0
      if(ifit1.gt.199) then
         ifit1=ifit1-100
         isighi=1
      endif

c- get the individual spectra; ntf is number of spectra
      ifit1o=ifit1
      call getspec(ifit1,ra,dec,wa,fa,fea,fa2,fea2,fweight,fweight2,
     $     nwa,ntf,raf,decf,az,cfield,rad0,iexp,inative,ratzero)

      if(ifit1o.eq.100) ifit1=2
      if(ifit1o.eq.106) goto 767
      if(ifit1o.eq.107) ifit1=-1
      if(ifit1o.eq.108) ifit1=-2
      if(ifit1o.eq.109) ifit1=2
      radmaxfit=3.5
      if(ifit1.eq.-1.or.ifit1.eq.-2) radmaxfit=7.

c- negative sigin means fix, positive means float
      sig3=3.0
      sig2p5=2.5
      sig8=8.0
      sigin=-sig3
      if(ifit1o.eq.102) ifit1=1
      if(ifit1.eq.1) sigin=-sig2p5

c- if doing source simulations, add sources into fibers (naf=Nwave, ntf=Nfib)
      ier=0
      if(ifit1o.eq.110) then
         call addem(rfw,nwa,ntf,raf,decf,wa,fa2,fea2,fea,
     $        nem,raem,decem,waveem,isighi,ier)
         ifit1=1
         sigin=-sig2p5
         if(ier.eq.1) goto 767
      endif
      if(ifit1o.eq.111) then
         call addem(rfw,nwa,ntf,raf,decf,wa,fa2,fea2,fea,
     $        nem,raem,decem,waveem,isighi,ier)
c         ifit1=1
         sigin=-sig2p5
         ifit1o=104
         if(ier.eq.1) goto 767
      endif
      
c- make the rastered search of RA,DEC: na is number of positions
      call mkraster(ra,dec,step,nstep,ara,adec,awave,rad0,
     $     na,ntf,raf,decf,nem,raem,decem,waveem,ifit1o)
c- integrate spectrum to get value for fitting PSF weights
      call getfl2d(wa,fa,fea,fweight2,nwa,ntf,wcen,wrange,
     $     sflux,sfluxe,xw)

      if(ifit1.ge.0) wrange=50.
      if(isighi.eq.1) then
         wrange=80.
         sigin=-sig8
      endif
      sig0=abs(sigin)

      nt=0
      print *,"Number of samplings: ",na,ntf
      t1=0.
      t2=0.
      ncheck=nint(float(na)/10.)
      if(ifit1o.eq.105) open(unit=13,file='flim.out',status='unknown')
      do i=1,na
c- set intuse to 1 if a specified radius
         time1=etime(tarray)
         call getint(ara(i),adec(i),ntf,raf,decf,radmaxfit,intuse)
c- fit the PSF and get weights for each spectrum
         call fit2d(ara(i),adec(i),rfw,ntf,sflux,sfluxe,raf,decf,intuse,
     $        xw,ntf2,weight,iflag,fadcw,az,chi,amps,sumrata)

         if(ifit1.ge.0) then
            if(inative.eq.0) then
c- sum weighted by the PSF weights
               call sumspec(nwa,ntf,wa,fa,fea,fa2,fea2,ntf2,intuse,
     $              weight,iflag,iexp,fadcw,az,spec,sumrata,
     $              fweight2,specexp)
               nspec=nwa(1)
            else
               call combspec(nwa,ntf,wa,fa,fea,fa2,fea2,ntf2,intuse,
     $              weight,iflag,iexp,fadcw,az,nspec,spec,sumrata)
               if(nspec.eq.0) goto 765
            endif

            if(ifit1.eq.2) goto 801
c- check if spectrum is any good (not all zeros)
            call checkspec(nspec,spec,ibad)
            if(ibad.eq.1) goto 765
c- fit the spectrum
            nsim=1
c            if(ifit1o.eq.102) nsim=100
            time2=etime(tarray)
            if(ifit1o.eq.110) wcen=awave(i)
            call fitspec(nspec,spec,sigin,wcen,wrange,ifit1,ifit1o,
     $           nsim,cfield,ra,dec,nout,fitout,xoutv,xouts,
     $           nflim,flimw,flimv,flima,ratnoise,i)
            time3=etime(tarray)
            t1=t1+time2-time1
            t2=t2+time3-time2
            if((float(i)/float(ncheck)-int(i/ncheck)).eq.0) 
     $           write(*,*) "Status ",t1,t2,float(i)/float(na),nt
            if(ifit1o.eq.105) then
               do iflim=1,nflim
                  if(flimv(iflim).ge.0.and.flimv(iflim).lt.1000.) then
                  else
                     flimv(iflim)=0.
                  endif
                  write(13,1201) ara(i),adec(i),
     $                 flimw(iflim),flimv(iflim),flima(iflim)
               enddo
               goto 765
            endif
c- combine over spatial and spectral pairs
            call getbestw(nout,fitout,wd,nw,fitw,fitsn,fitchi,fitcon,
     $           fitamp,fitsig)
            do j=1,nw
               nt=nt+1
               fitwt(nt)=fitw(j)
               fitsnt(nt)=fitsn(j)
               fitchit(nt)=fitchi(j)
               fitampt(nt)=fitamp(j)
               fitsigt(nt)=fitsig(j)
               fitcont(nt)=fitcon(j)
               rat(nt)=ara(i)
               dect(nt)=adec(i)
               if(nt.eq.nmaxs*5) then
                  print *,"Hit nt max ",nt
                  goto 766
               endif
            enddo
         else
            if(ifit1.eq.-2) then
               nfw=100
               fws=1.2
               fwe=3.2
               ifwf=1
               call getint(ara(i),adec(i),ntf,raf,decf,radmaxfit,intuse)
               open(unit=11,file='fwhm.out',status='unknown')
               do j=1,nfw
                  fwin=fws+float(j-1)/float(nfw-1)*(fwe-fws)
                  call fit2d(ara(i),adec(i),fwin,ntf,sflux,sfluxe,
     $                 raf,decf,intuse,
     $                 xw,ntf2,weight,iflag,fadcw,az,chif,ampsf,sumrata)
                  write(11,1103) fwin,chif,ampsf
               enddo
               close(11)
               ifwf=0
            endif
            nt=nt+1
            fitchit(nt)=chi
            fitcont(nt)=amps
            rat(nt)=ara(i)
            dect(nt)=adec(i)
         endif
 765     continue
      enddo
 766  continue
      if(ifit1o.eq.105) then 
         close(13)
         goto 767
      endif
      if(nt.eq.0) then
         print *,"Nothing fit"
         goto 767
      endif
 801  continue

      if(ifit1.ge.0) then
         open(unit=11,file='outbest',status='unknown')
         call getbestp(nt,rat,dect,fitwt,fitsnt,fitchit,fitampt,fitsigt,
     $        fitcont,rd,wd,np,fitp)
         do i=1,np
            write(11,1101) fitp(i,1),fitp(i,2),fitp(i,3),
     $           fitp(i,4),fitp(i,5),fitp(i,6),fitp(i,7),fitp(i,8)
         enddo
         if(ifit1o.eq.102) then
            ra=fitp(1,1)
            dec=fitp(1,2)
            call getint(ra,dec,ntf,raf,decf,radmaxfit,intuse)
            call fit2d(ra,dec,rfw,ntf,sflux,sfluxe,raf,decf,
     $           intuse,xw,ntf2,weight,iflag,fadcw,az,chi,amps,sumrata)
            open(unit=12,file='l2',status='unknown')
            do i=1,ntf
               write(12,1202) weight(i),iflag(i)
            enddo
            close(12)
 1202       format(1x,f7.4,1x,i1)
            if(inative.eq.0) then
c- sum weighted by the PSF weights
               call sumspec(nwa,ntf,wa,fa,fea,fa2,fea2,ntf2,intuse,
     $              weight,iflag,iexp,fadcw,az,spec,sumrata,
     $              fweight2,specexp)
               nspec=nwa(1)
            else
               call combspec(nwa,ntf,wa,fa,fea,fa2,fea2,ntf2,intuse,
     $              weight,iflag,iexp,fadcw,az,nspec,spec,sumrata)
            endif
c- get change in chi^2 if floating the linewidth
            nsim=1
            sigin=-sig0
            call fitspec(nspec,spec,sigin,wcen,wrange,ifit1,ifit1o,
     $           nsim,cfield,ra,dec,nout,fitout,xoutv,xouts,
     $           nflim,flimw,flimv,flima,ratnoise,1)
            fsig1=fitout(1,3)
            fchi1=fitout(1,6)
            sigin=sig0
            call fitspec(nspec,spec,sigin,wcen,wrange,ifit1,ifit1o,
     $           nsim,cfield,ra,dec,nout,fitout,xoutv,xouts,
     $           nflim,flimw,flimv,flima,ratnoise,1)
            fsig2=fitout(1,3)
            fchi2=fitout(1,6)
c- now get the full monte carlo
            nsim=100
            sigin=sig0
            call fitspec(nspec,spec,sigin,wcen,wrange,ifit1,ifit1o,
     $           nsim,cfield,ra,dec,nout,fitout,xoutv,xouts,
     $           nflim,flimw,flimv,flima,ratnoise,1)
            open(unit=11,file='mc2.out',status='unknown')
            write(11,1104) xoutv(1),xouts(1),xoutv(2),xouts(2),
     $           xoutv(3),xouts(3),xoutv(4),xouts(4),
     $           xoutv(5),xouts(5),xoutv(6),xouts(6),ra,dec,cfield,
     $           ratnoise,fsig2,fchi2
            close(11)
         endif
c- for 104, re-run to get the monte carlo for S/N>4.0
         if(ifit1o.eq.104.or.ifit1o.eq.110) then
c- first get those that match in radius and wavelength
            radiusd=3.5
            waved=2.5
            call getmatch(np,fitp,radiusd,waved,imatchd)
            open(unit=11,file='mc2.out',status='unknown')
            nga=0
            do i=1,np
               if(fitp(i,4).gt.sncut0.and.imatchd(i).eq.1) then
                  nga=nga+1
                  ra=fitp(i,1)
                  dec=fitp(i,2)
                  wcen=fitp(i,3)
                  wrange=50.
c                  sigin=2.5
                  sigin=-sig0
                  ifit1=1
                  call getint(ra,dec,ntf,raf,decf,radmaxfit,intuse)
                  call fit2d(ra,dec,rfw,ntf,sflux,sfluxe,raf,decf,
     $                 intuse,xw,ntf2,weight,iflag,fadcw,az,chi,amps,
     $                 sumrata)
                  if(inative.eq.0) then
c- sum weighted by the PSF weights
                     call sumspec(nwa,ntf,wa,fa,fea,fa2,fea2,ntf2,
     $                    intuse,
     $                    weight,iflag,iexp,fadcw,az,spec,sumrata,
     $                    fweight2,specexp)
                     nspec=nwa(1)
                  else
                     call combspec(nwa,ntf,wa,fa,fea,fa2,fea2,ntf2,
     $                    intuse,weight,iflag,iexp,fadcw,az,
     $                    nspec,spec,sumrata)
                  endif
c- get change in chi^2 if floating the linewidth
                  nsim=1
                  call fitspec(nspec,spec,-2.5,wcen,wrange,ifit1,ifit1o,
     $                 nsim,cfield,ra,dec,nout,fitout,xoutv,xouts,
     $                 nflim,flimw,flimv,flima,ratnoise,1)
                  fsig1=fitout(1,3)
                  fchi1=fitout(1,6)
                  call fitspec(nspec,spec,2.5,wcen,wrange,ifit1,ifit1o,
     $                 nsim,cfield,ra,dec,nout,fitout,xoutv,xouts,
     $                 nflim,flimw,flimv,flima,ratnoise,1)
                  fsig2=fitout(1,3)
                  fchi2=fitout(1,6)
c- now get the full monte carlo
                  nsim=100
                  sigin=sig0
                  call fitspec(nspec,spec,sigin,wcen,wrange,ifit1,
     $                 ifit1o,
     $                 nsim,cfield,ra,dec,nout,fitout,xoutv,xouts,
     $                 nflim,flimw,flimv,flima,ratnoise,1)
                  if(nout.gt.0) write(11,1104) xoutv(1),xouts(1),
     $                 xoutv(2),xouts(2),
     $                 xoutv(3),xouts(3),xoutv(4),xouts(4),
     $                 xoutv(5),xouts(5),xoutv(6),xouts(6),ra,dec,
     $                 cfield,ratnoise,fsig2,fchi2
               endif
            enddo
            print *,"S/N>4.0= ",nga
            close(11)
         endif
         if(ifit1o.eq.100) then
            ra=fitp(1,1)
            dec=fitp(1,2)
            call getint(ra,dec,ntf,raf,decf,radmaxfit,intuse)
            call fit2d(ra,dec,rfw,ntf,sflux,sfluxe,raf,decf,
     $           intuse,xw,ntf2,weight,iflag,fadcw,az,chi,amps,sumrata)
            open(unit=12,file='l2',status='unknown')
            do i=1,ntf
               write(12,1202) weight(i),iflag(i)
            enddo
            close(12)
         endif
      else
         open(unit=11,file='outbestc',status='unknown')
         do i=1,nt
            write(11,1102) rat(i),dect(i),fitchit(i),fitcont(i)
         enddo
      endif
      close(11)

      if(ifit1.ge.1) call writespec(nspec,spec,ratzero)
      if(ifit1.eq.2) then
         nin=0
         do i=1,naf
            if(spec(i,1).gt.3800.and.spec(i,1).lt.4800.) then
               nin=nin+1
               xin(nin)=spec(i,2)
            endif
         enddo
         call biwgt(xin,nin,xb,xs)
         if(xb.gt.1000.) then
            nin1=0
            nin2=0
            nin3=0
            do i=1,naf
               if(spec(i,1).gt.3600.and.spec(i,1).lt.3900.
     $              .and.spec(i,2).gt.0) then
                  if(specexp(i,1).gt.0) then
                     nin1=nin1+1
                     xin1(nin1)=specexp(i,1)/spec(i,2)
                  endif
                  if(specexp(i,2).gt.0) then
                     nin2=nin2+1
                     xin2(nin2)=specexp(i,2)/spec(i,2)
                  endif
                  if(specexp(i,3).gt.0) then
                     nin3=nin3+1
                     xin3(nin3)=specexp(i,3)/spec(i,2)
                  endif
               endif
            enddo
            call biwgt(xin1,nin1,xbb1,xs1)
            call biwgt(xin2,nin2,xbb2,xs1)
            call biwgt(xin3,nin3,xbb3,xs1)
            nin1=0
            nin2=0
            nin3=0
            do i=1,naf
               if(spec(i,1).gt.4200.and.spec(i,1).lt.4600.
     $              .and.spec(i,2).gt.0) then
                  if(specexp(i,1).gt.0) then
                     nin1=nin1+1
                     xin1(nin1)=specexp(i,1)/spec(i,2)
                  endif
                  if(specexp(i,2).gt.0) then
                     nin2=nin2+1
                     xin2(nin2)=specexp(i,2)/spec(i,2)
                  endif
                  if(specexp(i,3).gt.0) then
                     nin3=nin3+1
                     xin3(nin3)=specexp(i,3)/spec(i,2)
                  endif
               endif
            enddo
            call biwgt(xin1,nin1,xbg1,xs1)
            call biwgt(xin2,nin2,xbg2,xs1)
            call biwgt(xin3,nin3,xbg3,xs1)
            nin1=0
            nin2=0
            nin3=0
            do i=1,naf
               if(spec(i,1).gt.4900.and.spec(i,1).lt.5400.
     $              .and.spec(i,2).gt.0) then
                  if(specexp(i,1).gt.0) then
                     nin1=nin1+1
                     xin1(nin1)=specexp(i,1)/spec(i,2)
                  endif
                  if(specexp(i,2).gt.0) then
                     nin2=nin2+1
                     xin2(nin2)=specexp(i,2)/spec(i,2)
                  endif
                  if(specexp(i,3).gt.0) then
                     nin3=nin3+1
                     xin3(nin3)=specexp(i,3)/spec(i,2)
                  endif
               endif
            enddo
            call biwgt(xin1,nin1,xbr1,xs1)
            call biwgt(xin2,nin2,xbr2,xs1)
            call biwgt(xin3,nin3,xbr3,xs1)
            open(unit=13,file='exp.out',status='unknown')
            write(13,1301) xbb1,xbg1,xbr1
            write(13,1301) xbb2,xbg2,xbr2
            write(13,1301) xbb3,xbg3,xbr3
            close(13)
         endif
         call sumspec100(nspec,spec)
         call writespec100(nspec,spec)
      endif
 767  continue

 1101 format(2(1x,f10.6),3(1x,f7.2),1x,f9.1,1x,f7.2,1x,f10.2)
 1102 format(2(1x,f10.6),2(1x,f13.3))
 1103 format(f6.3,2(1x,f11.3))
 1104 format(12(1x,f8.2)2(1x,f10.6),1x,a12,3(1x,f6.2))
c 1201 format(f10.6,1x,f11.6,1x,f6.1,1x,f8.3,2(1x,f7.2),1x,f6.3)
 1201 format(f10.6,1x,f11.6,1x,f6.1,1x,f8.3,1x,f6.3)
 1301 format(3(1x,f10.3))
      end

      subroutine writespec(n,spec,ratzero)
      parameter(nmax=1500)
      real spec(nmax*10,9)
      
      open(unit=13,file='spec.out',status='unknown')
      do i=1,n
         write(13,1301) spec(i,1),spec(i,3),spec(i,5),
     $        spec(i,2),spec(i,4),spec(i,6),spec(i,7),
     $        spec(i,8),spec(i,9),ratzero
      enddo
 1301 format(1x,f7.2,9(1x,f11.3))

      return
      end

      subroutine writespec100(n,spec)
      parameter(nmax=1500)
      real spec(nmax*10,9)
      
      open(unit=13,file='spec100.out',status='unknown')
      do i=1,n
         write(13,1301) spec(i,1),spec(i,2),spec(i,3)
      enddo
 1301 format(1x,f7.2,1(1x,f13.3),1x,f6.3)

      return
      end

      subroutine checkspec(nspec,spec,ibad)
      parameter(nmax=1500)
      real spec(nmax*10,9)

      ibad=1
      do i=1,nspec
         if(spec(i,2).ne.0) ibad=0
      enddo

      return
      end

      subroutine sumspec100(n,spec)
      parameter(nmax=1500)
      real spec(nmax*10,9),w(nmax),w1(nmax),w2(nmax),sumsp(nmax)
      real sumgeom(nmax)
      integer ntb(nmax)
      
      ws=3490.
      we=5510.
      wbin=100.
      nbin=0
      do i=1,nmax
         sumsp(i)=0.
         sumgeom(i)=0.
         wnew=ws+float(i-1)*wbin
         if(wnew.lt.we) then
            nbin=nbin+1
            w(nbin)=wnew+wbin/2.
            w1(nbin)=wnew
            w2(nbin)=wnew+wbin
         else
            goto 766
         endif
      enddo
 766  continue
      
      do j=1,nbin
         ntb(j)=0
         do k=1,n
            wave=spec(k,1)
            cts=spec(k,2)
            geom=spec(k,9)
            if(wave.gt.w1(j).and.wave.le.w2(j)) then
               sumsp(j)=sumsp(j)+cts
               sumgeom(j)=sumgeom(j)+geom
               ntb(j)=ntb(j)+1
            endif
         enddo
      enddo

      nta=ntb(2)
      n=nbin
      do i=1,nbin
         spec(i,1)=w(i)
         if(ntb(i).gt.0) then
            spec(i,2)=sumsp(i)*float(nta)/float(ntb(i))
            spec(i,3)=sumgeom(i)/float(ntb(i))
         else
            spec(i,2)=0.
            spec(i,3)=0.
         endif
      enddo

      return
      end

      subroutine addem(rfw,nwa,ntf,raf,decf,wa,fa,fea,feac,
     $     nem,raem,decem,waveem,isighi,ier)
      parameter(nmax=1500,nfmax=1500)
      real raf(nfmax),decf(nfmax),xfa(nfmax),wvadd(nmax)
      real wa(nfmax,nmax),fa(nfmax,nmax),fea(nfmax,nmax)
      real raem(10000),decem(10000),waveem(10000),feac(nfmax,nmax)
      integer nwa(nfmax)
      parameter(pi=3.141593e0,radtodeg=57.29578)

      if(isighi.eq.1) then
         wsig=8.0
      else
         wsig=2.3
      endif
      dwave=2.0

      open(unit=3,file='sources.in',status='old',err=666)
      nem=0
      do iall=1,5000
         read(3,*,end=676) x,y,w,xf
         call getfluxsim(rfw,x,y,ntf,raf,decf,xf,xfa,frach,fracout)
         nem=nem+1
         raem(nem)=x
         decem(nem)=y
         waveem(nem)=w
         do j=1,ntf
            xfrac=xfa(j)
            do i=1,nwa(j)
               wvadd(i)=0.
            enddo
            if(xfrac.gt.0) then
               sum=0.
               do i=1,nwa(j)
                  xp=wa(j,i)
                  wg=(xp-w)/wsig
                  gaus=exp(-wg*wg/2.)/sqrt(2.*wsig*wsig*pi)*dwave
                  wvadd(i)=xfrac*gaus
                  sum=sum+wvadd(i)
               enddo
               do i=1,nwa(j)
                  fa(j,i)=fa(j,i)+wvadd(i)
               enddo
               do i=1,nwa(j)
                  if(fa(j,i).gt.3.*fea(j,i)) then
                     if(fea(j,i).gt.0) then
                        rat=feac(j,i)/fea(j,i)
                        xct=rat*fa(j,i)
                        xcte=sqrt(xct)
                        fnewe=sqrt((xcte/rat)**2+fea(j,i)**2)
                        fea(j,i)=fnewe
                     endif
c                     if(iall.eq.1095) print *,wa(j,i),xct,
c     $                    fa(j,i),fea(j,i),feac(j,i),fnewe
                  endif
               enddo
            endif
         enddo
      enddo
      goto 676
 666  continue
      print *,"No sources.in file"
      ier=1
 676  continue
      close(3)

      return
      end      

      subroutine getfluxsim(rfw,x,y,n,xpos,ypos,xf,xfa,frach,sum)
      real xpos(n),ypos(n),xfa(n)
      real*8 dxy,dyp,drad
      parameter(pi=3.141593e0,radtodeg=57.29578)

      imoff=1
      rsig=rfw/2.35
      fmof=rfw
      bmof=3.9

      cosd=cos(y/radtodeg)

      rfib=1.55/2.
c      ngrid=400
      ngrid=50
      xsig=7.
      xs=-xsig*rsig
      xe=+xsig*rsig
      ys=-xsig*rsig
      ye=+xsig*rsig
      deltx=(xe-xs)/float(ngrid)

      area=xf*deltx*deltx/(2.*rsig*rsig*pi)
      areamoff=4.*(2.**(1./bmof)-1.)*(bmof-1.)/pi/fmof/fmof
      areamoff=xf*deltx*deltx*areamoff

      xstep=(xe-xs)/float(ngrid-1)
      ystep=(ye-ys)/float(ngrid-1)
      do i=1,n
         xfa(i)=0.
      enddo
      ntot=0
      nhit=0
      do ix=1,ngrid
         xp=xs+float(ix-1)*xstep
         dxp=dble(x)+dble(xp/cosd)/3600.d0
         do iy=1,ngrid
            yp=ys+float(iy-1)*ystep
            dyp=dble(y)+dble(yp)/3600.d0
            ntot=ntot+1
            do i=1,n
               drad=3600.d0*dsqrt( (dble(cosd)*(dxp-dble(xpos(i))))**2
     $              +                          (dyp-dble(ypos(i)))**2)
               dist=sngl(drad)
               if(dist.lt.rfib) then
                  dist2=sqrt(xp**2+yp**2)
                  g=dist2/rsig
                  gaus=exp(-g*g/2.)*area
                  xmoff=areamoff*((1.+4.*(2.**(1./bmof)-1.)*
     $                 (dist2/fmof)**2)**(-bmof))
                  if(imoff.eq.1) gaus=xmoff
                  xfa(i)=xfa(i)+gaus
                  nhit=nhit+1
                  goto 666
               endif
            enddo
 666        continue
         enddo
      enddo
      fracm=float(ntot-nhit)/float(ntot)
      frach=float(nhit)/float(ntot)
      sum=0.
      do i=1,n
         sum=sum+xfa(i)
      enddo

      return
      end

      subroutine getmatch(np,fitp,rad,wc,imatchd)
      parameter(nmaxs=70000)
      real fitp(nmaxs*5,8)
      real w(nmaxs),xf(nmaxs),sn(nmaxs)
      real*8 dra(nmaxs),ddec(nmaxs),drad
      integer iok(nmaxs),iok2(nmaxs)
      integer imatchd(nmaxs)
      parameter(radtodeg=57.29578)

      do i=1,np
         dra(i)=dble(fitp(i,1))
         ddec(i)=dble(fitp(i,2))
         w(i)=fitp(i,3)
         sn(i)=fitp(i,4)
         iok(i)=1
         iok2(i)=1
         imatchd(i)=0
      enddo

      cosd=cos(sngl(ddec(1))/radtodeg)

      do i=1,np-1
         if(iok(i).eq.1) then
            imax=i
            snmax=sn(i)
            do j=i+1,np
               drad=dble(cosd)*(dra(i)-dra(j))**2+(ddec(i)-ddec(j))**2
               drad=3600.d0*dsqrt(drad)
               radc=sngl(drad)
               if(radc.lt.rad.and.abs(w(i)-w(j)).lt.wc) then
                  iok(j)=0
                  iok2(j)=0
                  iok2(i)=0
                  if(sn(j).ge.snmax) then
                     imax=j
                     snmax=sn(j)
                     iok(i)=0
                  endif
               endif
            enddo
            iok2(imax)=1
         endif
      enddo

      do i=1,np
         if(iok2(i).eq.1) imatchd(i)=1
      enddo

      return
      end

      subroutine getbestp(nt,rat,dect,fitwt,fitsnt,fitchit,
     $     fitampt,fitsigt,fitcont,rd,wd,nw,fitp)
      parameter(nmaxs=70000)
      real rat(nt),dect(nt),fitwt(nt),fitsnt(nt),fitchit(nt)
      real fitp(nmaxs*5,8),fitampt(nt),fitsigt(nt),fitcont(nt)
      integer icheck(nmaxs*5)
      parameter(radtodeg=57.29578)

      cosd=cos(dect(1)/radtodeg)
      nw=0
      do j=1,nt
         icheck(j)=0
      enddo
      do k=1,nt
         if(icheck(k).eq.0) then
            snmax=0.
            do j=1,nt
               if(icheck(j).eq.0) then
                  if(fitsnt(j).gt.snmax) then
                     snmax=fitsnt(j)
                     imax=j
                  endif
               endif
            enddo
            icheck(imax)=1
            nw=nw+1
            fitp(nw,1)=rat(imax)
            fitp(nw,2)=dect(imax)
            fitp(nw,3)=fitwt(imax)
            fitp(nw,4)=fitsnt(imax)
            fitp(nw,5)=fitchit(imax)
            fitp(nw,6)=fitampt(imax)
            fitp(nw,7)=fitsigt(imax)
            fitp(nw,8)=fitcont(imax)
            do j=1,nt
               wd0=abs(fitp(nw,3)-fitwt(j))
               rd0=cosd*(rat(imax)-rat(j))**2+(dect(imax)-dect(j))**2
               rd0=3600.d0*sqrt(rd0)
               if(wd0.lt.wd.and.rd0.lt.rd) icheck(j)=1
            enddo
         endif
      enddo

      return
      end

      subroutine getbestw(nout,fitout,wd,nw,fitw,fitsn,fitchi,
     $     fitcon,fitamp,fitsig)
      parameter(nmax=1500,nmaxs=70000)
      real fitout(3000,6),fitw(nmax),fitsn(nmax),fitchi(nmax)
      real fitcon(nmax),fitamp(nmax),fitsig(nmax)
      integer icheck(nmaxs*5)

c - this routine finds the wavelength with the highest S/N within wd
      nw=0
      if(nout.eq.0) return
      do j=1,nout
         icheck(j)=0
      enddo
      do k=1,nout
         if(icheck(k).eq.0) then
            snmax=0.
            do j=1,nout
               if(icheck(j).eq.0) then
                  if(fitout(j,4).gt.snmax) then
                     snmax=fitout(j,4)
                     imax=j
                  endif
               endif
            enddo
            icheck(imax)=1
            nw=nw+1
            fitw(nw)=fitout(imax,1)
            fitamp(nw)=fitout(imax,2)
            fitsig(nw)=fitout(imax,3)
            fitsn(nw)=fitout(imax,4)
            fitcon(nw)=fitout(imax,5)
            fitchi(nw)=fitout(imax,6)
            do j=1,nout
               if(abs(fitout(j,1)-fitout(imax,1)).lt.wd) icheck(j)=1
            enddo
         endif
      enddo

      return
      end
      
      subroutine getint(ra,dec,ntf,raf,decf,radmax,intuse)
      parameter(nfmax=1500)
      real raf(ntf),decf(ntf)
      integer intuse(nfmax)

      cosd=cos(dec/57.3)
      do i=1,ntf
         rad=3600.*sqrt((cosd*(ra-raf(i)))**2+(dec-decf(i))**2)
         if(rad.lt.radmax) then
            intuse(i)=1
         else
            intuse(i)=0
         endif
      enddo

      return
      end

      subroutine getfl2d(wa,fa,fea,fweight,nwa,ntf,wcen,wrange,
     $     sflux,sfluxe,fw)
      parameter(nmax=1500,nfmax=1500)
      real wa(nfmax,nmax),fa(nfmax,nmax),fea(nfmax,nmax)
      real fweight(nfmax,nmax),xin3(nmax),fw(nfmax)
      real sflux(nfmax),sfluxe(nfmax),xin(nmax),xin2(nmax)
      integer nwa(nfmax)
      w1=wcen-wrange
      w2=wcen+wrange
      do i=1,ntf
         nin=0
         do j=1,nwa(i)
            if(wa(i,j).gt.w1.and.wa(i,j).lt.w2.and.fa(i,j).ne.0) then
c            if(wa(i,j).gt.w1.and.wa(i,j).lt.w2.and.fa(i,j).ne.0
c     $           .and.fweight(i,j).ne.0) then
               nin=nin+1
               xin(nin)=fa(i,j)
               xin2(nin)=fea(i,j)*fea(i,j)
               xin3(nin)=fweight(i,j)
            endif
         enddo
         if(nin.ge.1) then
            call biwgt(xin,nin,xb,xs)
            call biwgt(xin2,nin,xb2,xs2)
            call biwgt(xin3,nin,xb3,xs3)
         else
            xb=0.
            xb2=0.
            xb3=0.
         endif
         sflux(i)=xb
         fw(i)=xb3
         if(xb2.ge.0) then
            sfluxe(i)=sqrt(xb2)
         else
            sfluxe(i)=0
         endif   
      enddo

      return
      end

      subroutine getspec(ifit1,ra0,dec0,wa,fa,fea,fa2,fea2,
     $     fweight,fweight2,nwa,ntf,ra,dec,az,cfield,rad0,iexp,inative,
     $     ratzero)
      parameter(nmax=1500,nfmax=1500,namax=1036,numax=12)
      real*8 dx1,dx2,drad,dra,ddec
      real wa(nfmax,nmax),fa(nfmax,nmax),fea(nfmax,nmax)
      real fa2(nfmax,nmax),fea2(nfmax,nmax),fweight(nfmax,nmax)
      real ra(nfmax),dec(nfmax),az(nfmax),fweight2(nfmax,nmax)
      real tp(1036),wtp(1036),xrelna(nfmax),xreluniq(nfmax)
      real wa1(namax),fa1(namax),fea1(namax),fa21(namax)
      real fea21(namax),fweight1(namax),fweight21(namax)
      real was(namax),fas(namax),az0a(namax)
      real wasa(numax,namax),fasa(numax,namax)
      real waveda(numax,namax,112),waved(namax,112)
      real skysuba(numax,namax,112),skysub(namax,112)
      real ftfa(numax,namax,112),ftf(namax,112)
      real skymoda(numax,namax,112),skymod(namax,112)
      real tracea(numax,namax,112),trace(namax,112)
      real chi2a(numax,namax,112),xchi2(namax,112)
      real chimapa(numax,namax,112),chimap(namax,112)
      real resmapa(numax,namax,112),resmap(namax,112)
      real ratmapa(numax,namax,112),ratmap(namax,112)
      real xflaga(numax,namax,1032),xflag(namax,1032)
      real f1out(nmax,nfmax),f2out(nmax,nfmax),fin(6,namax)
      real f3out(nmax,nfmax),f4out(nmax,nfmax),wavein(1036),xin(1036)
      real x4out(nfmax),x5out(nfmax),radout(nfmax)
      integer naa(namax),iexp(nfmax),nwa(nfmax)
      integer ifiba(nfmax),imatch(nfmax)
      character file2*80,file3*120,cfield*12,cdate*8,cshot*3
      character a1*6,a2*28,a3*20,a4*5,file1*120
      character file1a(nfmax)*120,file2a(nfmax)*120,cexp*2,camp*2
      character file3a(nfmax)*120,file4a(nfmax)*120,file5a(nfmax)*120
      character f1uniq(nfmax)*120,f2uniq(nfmax)*120,ctmp*120
      character f3uniq(nfmax)*120,f4uniq(nfmax)*120,f5uniq(nfmax)*120
      character a2out(nfmax)*28,a4out(nfmax)*5,a3out(nfmax)*20

      if(ifit1.lt.100) then
         open(unit=1,file='list',status='old')
         ntf=0
         do i=1,nfmax
            read(1,*,end=666) file2,x1,x2,x3
            ntf=ntf+1
            ra(ntf)=x1
            dec(ntf)=x2
            az(ntf)=x3
            open(unit=2,file=file2,status='old')
            na=0
            do j=1,nmax
               read(2,*,end=667) x1,x2,x3,x4,x5,x6,x7,x8,x9
               na=na+1
               wa(i,na)=x1
               fa(i,na)=x2
               fea(i,na)=x8
               fa2(i,na)=x3
               fea2(i,na)=x9
               fweight(i,na)=x4*x5
               fweight2(i,na)=1.
            enddo
 667        continue
            close(2)
         enddo
 666     continue
         close(1)
      else
         read *,cfield,rad0,w0,ww
         cdate=cfield(1:8)
         cshot=cfield(10:12)

c- get the throughput curve
         ntp=0
         file1="/data/00115/gebhardt/detect/tp/"//
     $        cdate//"v"//cshot//"sedtp_f.dat"
         open(unit=1,file=file1,status='old',err=676)
         do i=1,1036
            read(1,*,end=676) x1,x2
            ntp=ntp+1
            wtp(ntp)=x1
            tp(ntp)=x2
         enddo
 676     continue
         close(1)
         if(ntp.le.1) goto 576
         goto 578
 576     continue
         close(1)
         open(unit=1,file=
     $        "/work/00115/gebhardt/maverick/detect/tp/tpavg.dat",
     $        status='old')
         ntp=0
         do i=1,1036
            read(1,*,end=577) x1,x2
            ntp=ntp+1
            wtp(ntp)=x1
            tp(ntp)=x2
         enddo
 577     continue
         close(1)
 578     continue

c- get the relative dither normalization
         xn1=1.
         xn2=1.
         xn3=1.
         open(unit=1,file="norm.dat",status='old')
         read(1,*) xn1,xn2,xn3
         close(1)

c- get the dither file and find the fibers
         file1="/data/00115/gebhardt/detect/"//
     $        cdate//"v"//cshot//"/dithall.use"
         open(unit=1,file=file1,status='old')
         dra=dble(ra0)
         ddec=dble(dec0)
         ntf=0
         nuniq=0
         nzero=0
         nzero2=0
         ntotall=0
         do i=1,200000
            read(1,*,end=765) dx1,dx2,a1,x4,x5,x6,x7,a2,a3,a4
            drad=3600.d0*dsqrt(
     $           (dcos(ddec/57.3d0)*(dx1-dra))**2+(dx2-ddec)**2)
            rads=sngl(drad)
            if(rads.lt.rad0) then
               ntf=ntf+1
               ra(ntf)=sngl(dx1)
               dec(ntf)=sngl(dx2)
c- get the multifits               
               file1a(ntf)="/data/00115/gebhardt/red1/reductions/"
     $              //cdate//"/virus/virus0000"//cshot//"/"
     $              //a4//"/virus/"//a2(1:20)//".fits"
c- get the amplifier normalization
               file2a(ntf)="/data/00115/gebhardt/lib_calib/"
     $              //cdate(1:6)//"/i"//a2(11:13)//"a"//a2(19:20)
     $              //"ata.dat"
c- get the chi^2 map
               file3a(ntf)="/data/00115/gebhardt/lib_calib/reschi"
     $              //"/chi"//a2(7:9)//a2(19:20)//".fits"
c- get the residual map
               file4a(ntf)="/data/00115/gebhardt/lib_calib/reschi"
     $              //"/res"//a2(7:9)//a2(19:20)//".fits"
c- get the error ratio map
               file5a(ntf)="/data/00115/gebhardt/lib_calib/reschi"
     $              //"/rres"//a2(7:9)//a2(19:20)//".fits"
               if(a4(1:5).eq."exp01") xrelna(ntf)=xn1
               if(a4(1:5).eq."exp02") xrelna(ntf)=xn2
               if(a4(1:5).eq."exp03") xrelna(ntf)=xn3
               if(a4(1:5).eq."exp01") iexp(ntf)=1
               if(a4(1:5).eq."exp02") iexp(ntf)=2
               if(a4(1:5).eq."exp03") iexp(ntf)=3
               read(a2(22:24),1003) ifib
               ifiba(ntf)=ifib
c               write(12,2012) ra(ntf),dec(ntf),x4,x5,a2(1:28),a4,rads,
c     $              w0,a3,cdate,cshot

               x4out(ntf)=x4
               x5out(ntf)=x5
               a2out(ntf)=a2(1:28)
               a4out(ntf)=a4
               radout(ntf)=rads
               a3out(ntf)=a3

               do ic=1,120
                  if(file1a(ntf)(ic:ic).eq." ") then
                     nch=ic-1
                     goto 456
                  endif
               enddo
 456           continue
               if(ntf.eq.1) then
                  nuniq=nuniq+1
                  f1uniq(nuniq)=file1a(ntf)
                  f2uniq(nuniq)=file2a(ntf)
                  f3uniq(nuniq)=file3a(ntf)
                  f4uniq(nuniq)=file4a(ntf)
                  f5uniq(nuniq)=file5a(ntf)
                  xreluniq(nuniq)=xrelna(ntf)
               else
                  do iu=1,nuniq
                     idup=0
                     if(f1uniq(iu)(1:nch).eq.file1a(ntf)(1:nch)) idup=1
                  enddo
                  if(idup.eq.0) then
                     nuniq=nuniq+1
                     f1uniq(nuniq)=file1a(ntf)
                     f2uniq(nuniq)=file2a(ntf)
                     f3uniq(nuniq)=file3a(ntf)
                     f4uniq(nuniq)=file4a(ntf)
                     f5uniq(nuniq)=file5a(ntf)
                     xreluniq(nuniq)=xrelna(ntf)
                  endif
               endif
               imatch(ntf)=nuniq
            endif
         enddo
 765     continue
         close(1)
 1003    format(i3)
         do i=1,nuniq
c- get the multifits
            call getmulti(f1uniq(i),f2uniq(i),f3uniq(i),f4uniq(i),
     $           f5uniq(i),nas,was,fas,az0,waved,skysub,ftf,skymod,
     $           trace,xflag,xchi2,chimap,resmap,ratmap)
            naa(i)=nas
            do j=1,nas
               wasa(i,j)=was(j)
               fasa(i,j)=fas(j)
            enddo
            az0a(i)=az0
            do j=1,1036
               do k=1,112
                  waveda(i,j,k)=waved(j,k)
                  skysuba(i,j,k)=skysub(j,k)
                  ftfa(i,j,k)=ftf(j,k)
                  skymoda(i,j,k)=skymod(j,k)
                  tracea(i,j,k)=trace(j,k)
                  chi2a(i,j,k)=xchi2(j,k)
                  chimapa(i,j,k)=chimap(j,k)
                  resmapa(i,j,k)=resmap(j,k)
                  ratmapa(i,j,k)=ratmap(j,k)
               enddo
               do k=1,1032
                  xflaga(i,j,k)=xflag(j,k)
               enddo
            enddo
         enddo

         il1=0
         if(ifit1.eq.100) il1=1
         if(ifit1.eq.102) il1=1
         if(ifit1.eq.109) il1=1
         if(il1.eq.1) open(unit=12,file='l1',status='unknown')
         w0in=w0
c- now loop over all input fibers
         do i=1,ntf
            iu=imatch(i)
            xrelnorm=xrelna(i)
            ifib=ifiba(i)

            nas=naa(iu)
            do j=1,nas
               was(j)=wasa(iu,j)
               fas(j)=fasa(iu,j)
            enddo
            az(i)=az0a(iu)
            do j=1,1036
               do k=1,112
                  waved(j,k)=waveda(iu,j,k)
                  skysub(j,k)=skysuba(iu,j,k)
                  ftf(j,k)=ftfa(iu,j,k)
                  skymod(j,k)=skymoda(iu,j,k)
                  trace(j,k)=tracea(iu,j,k)
                  xchi2(j,k)=chi2a(iu,j,k)
                  chimap(j,k)=chimapa(iu,j,k)
                  resmap(j,k)=resmapa(iu,j,k)
                  ratmap(j,k)=ratmapa(iu,j,k)
               enddo
               do k=1,1032
                  xflag(j,k)=xflaga(iu,j,k)
               enddo
            enddo

c- get X,Y on detector
            do j=1,1036
               xin(j)=float(j)
               wavein(j)=waved(j,ifib)
            enddo
            call xlinint(w0in,1036,wavein,xin,xin0)
            ix=nint(xin0)
            iy=nint(trace(ix,ifib))
            if(il1.eq.1) write(12,2012) ra(i),dec(i),x4out(i),
     $           x5out(i),a2out(i),a4out(i),radout(i),w0in,
     $           a3out(i),cdate,cshot,ix,iy

            w0=4505.
            ww=1035.
            call getspec2(nas,was,fas,waved,skysub,ftf,
     $           skymod,trace,xflag,xchi2,chimap,resmap,ratmap,
     $           ntp,wtp,tp,xrelnorm,w0,ww,
     $           ifib,na1,wa1,fa1,fea1,fa21,fea21,fweight1,fweight21,
     $           fin,inative)

            do ich=1,120
               ctmp=f1uniq(iu)
               if(ctmp(ich:ich+2).eq."exp") then
                  cexp=ctmp(ich+3:ich+4)
                  goto 333
               endif
            enddo
 333        continue
            do ich=1,120
               ctmp=f1uniq(iu)
               if(ctmp(ich:ich+4).eq.".fits") then
                  camp=ctmp(ich-2:ich-1)
                  goto 334
               endif
            enddo
 334        continue
            if(cexp.eq."01".and.camp.eq."LL") is0=1
            if(cexp.eq."01".and.camp.eq."LU") is0=113
            if(cexp.eq."01".and.camp.eq."RL") is0=225
            if(cexp.eq."01".and.camp.eq."RU") is0=337
            if(cexp.eq."02".and.camp.eq."LL") is0=449
            if(cexp.eq."02".and.camp.eq."LU") is0=561
            if(cexp.eq."02".and.camp.eq."RL") is0=673
            if(cexp.eq."02".and.camp.eq."RU") is0=785
            if(cexp.eq."03".and.camp.eq."LL") is0=897
            if(cexp.eq."03".and.camp.eq."LU") is0=1009
            if(cexp.eq."03".and.camp.eq."RL") is0=1121
            if(cexp.eq."03".and.camp.eq."RU") is0=1233
            ifibamp=is0+ifib-1

 3001       format(i2)
            nwa(i)=na1
            do j=1,na1
               wa(i,j)=wa1(j)
               fa(i,j)=fa1(j)
               fea(i,j)=fea1(j)
               fa2(i,j)=fa21(j)
               fea2(i,j)=fea21(j)
               fweight(i,j)=fweight1(j)
               fweight2(i,j)=fweight21(j)
               f1out(j,ifibamp)=fin(1,j)
               f2out(j,ifibamp)=fin(2,j)
               f3out(j,ifibamp)=fin(3,j)*1.e17
               f4out(j,ifibamp)=fin(4,j)*1.e17
               if(fa(i,j).eq.0.) nzero=nzero+1
               if(fin(1,j).eq.0.) nzero2=nzero2+1
               ntotall=ntotall+1
            enddo
            na=na1
c            print *,cexp,camp
c            print *,ifibamp,is0
c            print *,iu,f1uniq(iu)
c            print *,iu,f2uniq(iu)
         enddo
 766     continue
         close(1)
         if(il1.eq.1) close(12)
         if(ifit1.eq.106) call writefits(ntf,f1out,f2out,f3out,f4out)
         ifit1=ifit1-100
      endif

      if(ntotall.gt.0) then
c         ratzero=100.*float(nzero)/float(ntotall)
         ratzero=100.*float(nzero2)/float(ntotall)
      else
         ratzero=0.
      endif

 2012 format(2(f11.7,1x),2(1x,f8.3),1x,a28,1x,a5,
     $     1x,f6.3,1x,f6.1,1x,a20,1x,a8,1x,a3,2(1x,i4))
      return
      end

      subroutine writefits(ntf,fa,fea,fa2,fea2)
      parameter(nmax=1500,nfmax=1500)
      real fa(nfmax,nmax),fea(nfmax,nmax)
      real fa2(nfmax,nmax),fea2(nfmax,nmax)
      integer naxes(2)

      naxis=2
      naxes(1)=1036
      naxes(2)=ntf
      iblock=1
      igc=0
      ier=0

      call ftinit(50,'out.fits',iblock,ier)
      call ftphps(50,-32,naxis,naxes,ier)
      call ftp2de(50,igc,nfmax,naxes(1),naxes(2),fa2,ier)
      call ftpkls(50,'EXTNAME','calib',"Label",ier)

      call ftiimg(50,-32,naxis,naxes,ier)
      call ftp2de(50,igc,nfmax,naxes(1),naxes(2),fea2,ier)
      call ftpkls(50,'EXTNAME','calibe',"Label",ier)

      call ftiimg(50,-32,naxis,naxes,ier)
      call ftp2de(50,igc,nfmax,naxes(1),naxes(2),fa,ier)
      call ftpkls(50,'EXTNAME','calib_c',"Label",ier)

      call ftiimg(50,-32,naxis,naxes,ier)
      call ftp2de(50,igc,nfmax,naxes(1),naxes(2),fea,ier)
      call ftpkls(50,'EXTNAME','calibe_c',"Label",ier)
      
      call ftclos(50,ier)

      return
      end

      subroutine getmulti(file1,file2,file3,file4,file5,
     $     na,wa,fa,az0,waved,skysub,ftf,skymod,trace,xflag,xchi2,
     $     chimap,resmap,ratmap)
      parameter(namax=1036)
      real wa(namax),fa(namax)
      real waved(namax,112),skymod(namax,112),ftf(namax,112)
      real skysub(namax,112),trace(namax,112),xflag(namax,1032)
      real xchi2(namax,112),xin(namax*112)
      real chimap(namax,112),resmap(namax,112),ratmap(namax,112)
      integer naxes(2)
      character file1*120,file2*120,filet*80
      character file3*120,file4*120,file5*120
      logical simple,extend,anyf

c- getting the amp2amp                                                                                                               
      open(unit=2,file=file2,status='old',err=678)
      na=0
      do i=1,namax
         read(2,*,end=677) x1,x2
         na=na+1
         wa(na)=x1
         fa(na)=x2
      enddo
 677  continue
      close(2)
      goto 679
 678  continue
      close(2)
      na=3
      wa(1)=3500.
      fa(1)=1.
      wa(2)=4500.
      fa(2)=1.
      wa(3)=5500.
      fa(3)=1.
      write(*,*) "Amp Norm does not exist: ",file2
 679  continue

      im1=0
      ier=0
      iread=0
c      call ftgiou(im1,ier)
      im1=51
      call ftopen(im1,file1,iread,iblock,ier)
      call ftgkye(im1,'STRUCTAZ',az0,filet,ier)
      if(az0.lt.-1000) az0=150.

c - this is the wavelength                                                                                                           
      iext=12
      call ftmahd(im1,iext,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      ncol=naxes(1)
      nrow=max(1,naxes(2))
      call ftg2de(im1,igc,0.,namax,ncol,nrow,waved,anyf,ier)

c - this is the sky-subtracted spectrum                                                                                              
      iext=16
      call ftmahd(im1,iext,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      ncol=naxes(1)
      nrow=max(1,naxes(2))
      call ftg2de(im1,igc,0.,namax,ncol,nrow,skysub,anyf,ier)

c - this is the F2F                                                                                                  
      iext=14
      call ftmahd(im1,iext,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      ncol=naxes(1)
      nrow=max(1,naxes(2))
      call ftg2de(im1,igc,0.,namax,ncol,nrow,ftf,anyf,ier)

c - this is the Skymod
      iext=17
      call ftmahd(im1,iext,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      ncol=naxes(1)
      nrow=max(1,naxes(2))
      call ftg2de(im1,igc,0.,namax,ncol,nrow,skymod,anyf,ier)

c - this is the trace
      iext=13
      call ftmahd(im1,iext,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      ncol=naxes(1)
      nrow=max(1,naxes(2))
      call ftg2de(im1,igc,0.,namax,ncol,nrow,trace,anyf,ier)

c - this is the chi^2 from fiber profile
      iext=8
      call ftmahd(im1,iext,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      ncol=naxes(1)
      nrow=max(1,naxes(2))
      call ftg2de(im1,igc,0.,namax,ncol,nrow,xchi2,anyf,ier)
c - get the chi relative to full frame
      nin=0
      do i=1,ncol
         do j=1,nrow
            if(xchi2(i,j).gt.0.) then
               nin=nin+1
               xin(nin)=xchi2(i,j)
            endif
         enddo
      enddo
      call biwgt(xin,nin,xb,xs)
      if(xb.gt.0.15) then
         do i=1,ncol
            do j=1,nrow
               xchi2(i,j)=xchi2(i,j)/xb
            enddo
         enddo
      else
c - set data to all zero since something is wrong
         do i=1,ncol
            do j=1,nrow
               skysub(i,j)=0.
            enddo
         enddo
      endif

c - this is the flagged frame
      iext=2
      call ftmahd(im1,iext,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      ncol=naxes(1)
      nrow=max(1,naxes(2))
      call ftg2de(im1,igc,0.,namax,ncol,nrow,xflag,anyf,ier)

      call ftclos(im1,ier)

c - this is the chi^2 map
      ier=0
      call ftopen(im1,file3,iread,iblock,ier)
      if(ier.eq.0) then
         iext=1
         call ftmahd(im1,iext,ihd,ier)
         call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
         ncol=naxes(1)
         nrow=max(1,naxes(2))
         call ftg2de(im1,igc,0.,namax,ncol,nrow,chimap,anyf,ier)
         call ftclos(im1,ier)
      else
         print *,"Not here: ",file3
         ier=0
         do j=1,112
            do i=1,1032
               chimap(i,j)=1.
            enddo
         enddo
      endif

c - this is the residual map
      ier=0
      call ftopen(im1,file4,iread,iblock,ier)
      if(ier.eq.0) then
         iext=1
         call ftmahd(im1,iext,ihd,ier)
         call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
         ncol=naxes(1)
         nrow=max(1,naxes(2))
         call ftg2de(im1,igc,0.,namax,ncol,nrow,resmap,anyf,ier)
         call ftclos(im1,ier)
      else
         print *,"Not here: ",file4
         ier=0
         do j=1,112
            do i=1,1032
               resmap(i,j)=0.
            enddo
         enddo
      endif

c - this is the error ratio map
      ier=0
      call ftopen(im1,file5,iread,iblock,ier)
      if(ier.eq.0) then
         iext=1
         call ftmahd(im1,iext,ihd,ier)
         call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
         ncol=naxes(1)
         nrow=max(1,naxes(2))
         call ftg2de(im1,igc,0.,namax,ncol,nrow,ratmap,anyf,ier)
         call ftclos(im1,ier)
      else
         print *,"Not here: ",file5
         ier=0
         do j=1,112
            do i=1,1032
               ratmap(i,j)=1.
            enddo
         enddo
      endif
         
      return
      end

      subroutine getspec2(nas,was,fas,wavedi,skysub,ftf,
     $           skymod,xtrace,xflag,xchi2,chimap,resmap,ratmap,
     $           ntp,wtp,tp,xrelnorm,w0,ww,
     $           ifib,nw,wave,fa1,fea1,fa21,fea21,fweight1,fweight21,
     $           fin,inative)

      parameter(namax=1036)
      real tp(namax),wtp(namax),was(nas),fas(nas),wavedi(namax,112)
      real wave(namax),fa1(namax),fea1(namax),fa21(namax),chi2s(namax)
      real fea21(namax),fweight1(namax),fweight21(namax),chi2(namax)
      real waved(namax),xin(100000),xerr(namax),ftf(namax,112)
      real skysub(namax,112),xtrace(namax,112),xflag(namax,1032)
      real xsp(namax),f2f(namax),sky(namax),trace(namax),flag(namax)
      real fin(6,namax),fout(6,namax),ypa(namax),yerra(namax)
      real yweight(namax),chimap(namax,112),resmap(namax,112)
      real ratmap(namax,112),raterr(namax),raterrs(namax)
      real residual(namax),residuals(namax),ftfs(namax)
      real skymod(namax,112),yerrb(namax),skys(namax),xchi2(namax,112)
      integer iwave(namax),naxes(2)
      character file1*120,file2*80,filet*80
      logical simple,extend,anyf

      convfac=(6.626e-27)*(3.e18)/360./5.e5

      n=0
      wmin=w0-ww
      wmax=w0+ww
      wsp=2.
      nw=nint((wmax-wmin)/wsp)+1
      do i=1,nw
         wave(i)=wmin+float(i-1)*wsp
      enddo

      do i=1,1032
         w=wavedi(i,ifib)
         if(w.gt.wmin.and.w.lt.wmax) then
            n=n+1
            waved(n)=w
            iwave(n)=i
            if(inative.eq.1) wave(i)=w
         endif
      enddo

      if(inative.eq.1) nw=n

      do i=1,n
         xres=skymod(iwave(i),ifib)*resmap(iwave(i),ifib)
         xsp(i)=skysub(iwave(i),ifib)-xres
c         ilo=max(1,i-3)
c         ihi=min(n,i+3)
c         sum=0.
c         ns=0
c         do ix=ilo,ihi
c            sum=sum+abs(resmap(iwave(ix),ifib))
c            ns=ns+1
c         enddo
c         sum=sum/float(ns)
c         if(ifib.eq.41) print *,i,wave(i),sum,resmap(iwave(i),ifib)
c         if(abs(resmap(iwave(i),ifib)).gt.0.1) ftf(iwave(i),ifib)=0.
      enddo

c - normalize by f2f
      do i=1,n
         xftf=ftf(iwave(i),ifib)
         if(xftf.gt.0.1) then
            xfnorm=ftf(iwave(i),ifib)
            xsp(i)=xsp(i)/xfnorm
            f2f(i)=xfnorm
            sky(i)=skymod(iwave(i),ifib)
            trace(i)=xtrace(iwave(i),ifib)
c            chi2(i)=xchi2(iwave(i),ifib)
            chi2(i)=chimap(iwave(i),ifib)
            raterr(i)=ratmap(iwave(i),ifib)
            residual(i)=resmap(iwave(i),ifib)
            flag(i)=xflag(iwave(i),nint(trace(i)))
         else
            xftf=1.
            xsp(i)=xsp(i)
            f2f(i)=xftf
            sky(i)=0.
            chi2(i)=0.
            flag(i)=-1
         endif
      enddo

c- get weight for error estimate
      jin=1
      do i=1,nw
         call xlininte(wave(i),n,waved,xsp,yweight(i),jin,jout)
         jin=jout
c         print *,i,ifib,wave(i),waved(jout),waved(jout+1),yweight(i)
      enddo
      jin=1
      do i=1,nw
         call xlinint2(wave(i),n,waved,xsp,ypa(i),jin,jout)
         jin=jout
      enddo
      jin=1
      do i=1,nw
         call xlinint2(wave(i),n,waved,sky,skys(i),jin,jout)
         jin=jout
      enddo
c- for chi2, get the nearest col
      jin=1
      do i=1,nw
         call xlinint2(wave(i),n,waved,chi2,chi2s(i),jin,jout)
         jin=jout
      enddo
c- for rat error
      jin=1
      do i=1,nw
         call xlinint2(wave(i),n,waved,raterr,raterrs(i),jin,jout)
         jin=jout
      enddo
c- for residual
      jin=1
      do i=1,nw
         call xlinint2(wave(i),n,waved,residual,residuals(i),jin,jout)
         jin=jout
      enddo
c- for ftf
      jin=1
      do i=1,nw
         call xlinint2(wave(i),n,waved,f2f,ftfs(i),jin,jout)
         jin=jout
      enddo

c- get the sqrt(n) errors. First smooth skysub and add to skymod.
c  Second, take sqrt and add in rnoise*3pixels (just make rn 3 for all)
      rnoise=3.
      nh1=1
      nh2=5
      do i=1,nw
         i1min=max(1,i-nh1)
         i1max=min(nw,i+nh1)
         i2min=max(1,i-nh2)
         i2max=min(nw,i+nh2)
         nin=0
         do j=i1min,i1max
            if(ypa(j).ne.0) then
               nin=nin+1
               xin(nin)=ypa(j)
            endif
         enddo
         call biwgt(xin,nin,xb1,xs1)
         nin=0
         do j=i2min,i2max
            if(ypa(j).ne.0) then
               nin=nin+1
               xin(nin)=ypa(j)
            endif
         enddo
         call biwgt(xin,nin,xb2,xs2)
         if(xb1.gt.(xb2+1.2*xs2)) then
            s1=xb1
         else
            s1=xb2
         endif
         stot=skys(i)+max(0.,s1)
c- add in read noise and then some fraction of sky (for poor subtraction)
         stots=sqrt(stot+3.*rnoise*rnoise)
         if(abs(residuals(i)).lt.0.06) then
            skyerrf=0.03
         else
            skyerrf=0.06
         endif
         yerrb(i)=sqrt(stots*stots+(skyerrf*stot)**2)
         ratmult=1.
         if(raterrs(i).gt.1.03) then
            ratmult=1.+(raterrs(i)-1.)/(1.5-1.)*3.
            yerrb(i)=yerrb(i)*ratmult
         endif
         if(ftfs(i).gt.0) yerrb(i)=yerrb(i)/ftfs(i)

c         if(ifib.eq.89) print *,wave(i),yerrb(i),raterrs(i),ftfs(i)
c- make is smaller by yweight since we are interpolating for a uniform grid
c         yerrb(i)=yerrb(i)/yweight(i)
c         yerrb(i)=yerrb(i)*yweight(i)

c         if(chi2s(i).gt.1.3) yerrb(i)=yerrb(i)*chi2s(i)
c         if(wave(i).gt.3680.and.wave(i).lt.3740.)
c     $        print *,wave(i),yerrb(i),chi2s(i)
c         print *,wave(i),yerrb(i),chi2s(i)
      enddo

c - parameters for empirical error estimate
c SKIP for now
      goto 888
      nerrsp=51
      nerrsp2=3
      nerrf=11
      nerrh=nint(float(nerrsp)/2.)
      nerrh2=nint(float(nerrsp2)/2.)
      nerrfh=nint(float(nerrf)/2.)
      do i=1,nw
c - get the errors both spectrally and across fibers
c - spectrally:
         iemin=max(1,i-nerrh)
         iemax=min(nw,iemin+nerrsp)
         nin=0
         nspece=iemax-iemin
         do ie=iemin,iemax
            nin=nin+1
            xin(nin)=ypa(ie)
         enddo
         nin1=nin
         call biwgt(xin,nin,xb,xerrsp)
         call moment(xin,nin,ave,adev,sdevs,var,skew,curt)
c - across fibers:
         iemin=max(1,i-nerrh2)
         iemax=min(nw,iemin+nerrsp2)
         ifmin=max(1,ifib-nerrfh)
         ifmax=min(112,ifmin+nerrf)
         nin=0
         nspecs1=iemax-iemin
         nspecs2=ifmax-ifmin
         do ie=iemin,iemax
            do ifc=ifmin,ifmax
               nin=nin+1
               xin(nin)=ypa(ie)
            enddo
         enddo
         call biwgt(xin,nin,xb,xerrf) 
         call moment(xin,nin,ave,adev,sdevf,var,skew,curt)
         yerra(i)=max(sdevs,sdevf)
      enddo
 888  continue

      jin1=1
      jin2=1
      jin3=1
      do i=1,nw
         yp=ypa(i)
         yerr=yerrb(i)
         call xlinint2(wave(i),n,waved,flag,yflag,jin1,jout1)
         call xlinint2(wave(i),ntp,wtp,tp,ytp,jin2,jout2)
         call xlinint2(wave(i),nas,was,fas,yfp,jin3,jout3)
         jin1=jout1
         jin2=jout2
         jin3=jout3
c - include the relative dither normalization first:
         if(xrelnorm.gt.0) yp=yp/xrelnorm
         if(xrelnorm.gt.0) yerr=yerr/xrelnorm
         yfrac=yfp*ytp
         if(yerr.le.0) yflag=0
         if(yerr.gt.1.e5) yflag=0
         if(wave(i).lt.waved(3).or.wave(i).gt.waved(n-3)) yflag=0
         if(yfp.le.0.1) yflag=0
         if(ytp.eq.0) yflag=0
         if(yflag.gt.0.and.yp.lt.1e7.and.yp.gt.-1e3) then
            fa1(i)=yp/yfp
            fea1(i)=yerr/yfp
            fa21(i)=yp*convfac/wave(i)/ytp/yfp
            fea21(i)=yerr*convfac/wave(i)/ytp/yfp
            fweight1(i)=yfp*ytp
            fweight21(i)=1.
         else
c            fa1(i)=yp/yfp
            fa1(i)=0.
            fea1(i)=0.
            fa21(i)=0.
            fea21(i)=0.
            fweight1(i)=yfp*ytp
            fweight21(i)=0.
         endif
      enddo

      do i=1,nw
         fin(1,i)=fa1(i)
         fin(2,i)=fea1(i)
         fin(3,i)=fa21(i)
         fin(4,i)=fea21(i)
         fin(5,i)=fweight1(i)
         fin(6,i)=fweight21(i)
      enddo
      call sclean(nw,wave,fin,fout)
      do i=1,nw
         fa1(i)=fout(1,i)
         fea1(i)=fout(2,i)
         fa21(i)=fout(3,i)
         fea21(i)=fout(4,i)
         fweight1(i)=fout(5,i)
         fweight21(i)=fout(6,i)
      enddo
            
      return
      end

      subroutine sclean(n,w,f,fout)
      real w(n),f(6,1036),fout(6,1036)

c- this routine replaces zeros with nearest good values
c  1 cts
c  2 err_cts
c  3 flux
c  4 err_flux
c  5 weight1
c  6 weight2

      do i=1,n
         do j=1,6
            fout(j,i)=f(j,i)
         enddo
      enddo

      jnear=1
      do i=1,n
         if(f(1,i).eq.0.or.f(2,i).eq.0) then
            diff=1e10
            do j=1,n
               if(f(1,j).ne.0.and.f(2,j).ne.0) then
                  d=abs(w(i)-w(j))
                  if(d.lt.diff) then
                     diff=d
                     jnear=j
                  endif
               endif
            enddo
c - search around nearest and average 3 together
            avg1=0.
            avg2=0.
            navg=0.
            js=max(1,jnear-5)
            je=min(n,jnear+5)
            do j=js,je
               if(f(1,j).ne.0) then
                  avg1=avg1+f(1,j)
                  avg2=avg2+f(3,j)
                  navg=navg+1
               endif
            enddo
            if(navg.gt.0) then
               avg1=avg1/float(navg)
               avg2=avg2/float(navg)
            else
               avg1=0.
               avg2=0.
            endif
            fout(1,i)=avg1
            fout(3,i)=avg2
            fout(2,i)=f(2,jnear)
            fout(4,i)=f(4,jnear)
            if(f(1,i).eq.0.or.f(2,i).eq.0) fout(2,i)=4.*f(2,jnear)
            if(f(1,i).eq.0.or.f(2,i).eq.0) fout(4,i)=4.*f(4,jnear)
         endif
      enddo
      
      return
      end


      SUBROUTINE moment(data,n,ave,adev,sdev,var,skew,curt)
      INTEGER n
      REAL adev,ave,curt,sdev,skew,var,data(n)
      INTEGER j
      REAL p,s,ep
c      if(n.le.1)pause 'n must be at least 2 in moment'
      s=0.
      do 11 j=1,n
        s=s+data(j)
11    continue
      ave=s/n
      adev=0.
      var=0.
      skew=0.
      curt=0.
      ep=0.
      do 12 j=1,n
        s=data(j)-ave
        ep=ep+s
        adev=adev+abs(s)
        p=s*s
        var=var+p
        p=p*s
        skew=skew+p
        p=p*s
        curt=curt+p
12    continue
      adev=adev/n
      var=(var-ep**2/n)/(n-1)
      sdev=sqrt(var)
      if(var.ne.0.)then
        skew=skew/(n*sdev**3)
        curt=curt/(n*var**2)-3.
      else
c        pause 'no skew or kurtosis when zero variance in moment'
      endif
      return
      END

      subroutine mkraster(ra,dec,step,nstep,ara,adec,awave,rad0,ntot,
     $     ntf,raf,decf,nem,raem,decem,waveem,ifit1)
      parameter(nmaxs=70000,nfmax=1500)
      real ara(nmaxs),adec(nmaxs),raf(ntf),decf(ntf),awave(nmaxs)
      real raem(10000),decem(10000),waveem(10000)

      cdec=cos(dec/57.29)

      if(ifit1.eq.110) then
c- 110 is for input positions
         ia=0
         if(nstep.gt.1) then
            atot=step*float(nstep-1)
            ahalf=atot/2.
         else
            ahalf=0.
         endif
         do iall=1,nem
            ras=raem(iall)-ahalf/cdec/3600.
            decs=decem(iall)-ahalf/3600.
            do i=1,nstep
               r=ras+float(i-1)*step/cdec/3600.
               do j=1,nstep
                  d=decs+float(j-1)*step/3600.
                  ia=ia+1
                  ara(ia)=r
                  adec(ia)=d
                  awave(ia)=waveem(iall)
               enddo
            enddo
         enddo
         ntot=ia
         return
      endif

      if(ifit1.eq.104) then
c- 104 selects on fiber centers with a raster around each
         ia=0
         atot=step*float(nstep-1)
         ahalf=atot/2.
         do iall=1,ntf
            ras=raf(iall)-ahalf/cdec/3600.
            decs=decf(iall)-ahalf/3600.
            do i=1,nstep
               r=ras+float(i-1)*step/cdec/3600.
               do j=1,nstep
                  d=decs+float(j-1)*step/3600.
                  ia=ia+1
                  ara(ia)=r
                  adec(ia)=d
               enddo
            enddo
         enddo
         ntot=ia
         return
      endif

      if(ifit1.eq.105) then
c- 105 selects on the specified raster but then only saves if any fibers are near
         radmin=2.5
         nstep=nint(2.*rad0/step)
         atot=step*float(nstep-1)
         ahalf=atot/2.
      
         ras=ra-ahalf/cdec/3600.
         decs=dec-ahalf/3600.
      
         ia=0
         do i=1,nstep
            r=ras+float(i-1)*step/cdec/3600.
            do j=1,nstep
               d=decs+float(j-1)*step/3600.
               do iall=1,ntf
                  rafib=raf(iall)
                  decfib=decf(iall)
                  rad=3600.*sqrt((cdec*(rafib-r))**2+(decfib-d)**2)
                  if(rad.lt.radmin) then
                     ia=ia+1
                     ara(ia)=r
                     adec(ia)=d
                     goto 888
                  endif
               enddo
 888           continue
            enddo
         enddo
         ntot=ia
         return
      endif

c- these are for the input raster positions
      if(nstep.lt.2) then
         ara(1)=ra
         adec(1)=dec
         ntot=1
         return
      endif

      atot=step*float(nstep-1)
      ahalf=atot/2.
      
      ras=ra-ahalf/cdec/3600.
      decs=dec-ahalf/3600.
      
      ia=0
      do i=1,nstep
         r=ras+float(i-1)*step/cdec/3600.
         do j=1,nstep
            d=decs+float(j-1)*step/3600.
            ia=ia+1
            ara(ia)=r
            adec(ia)=d
         enddo
      enddo
      ntot=ia

      return
      end

      subroutine getfwhm(rfw)
      common/cfwhm/ ifwf,fwin

c- we are assuming it is FWHM measured at 4550AA
      open(unit=1,file='fwhm.use',status='old',err=955)
      read(1,*) rfw
      close(1)
      goto 956
 955  continue
      close(1)
      rfw=1.55
 956  continue
      open(unit=1,file='fwhm.fix',status='old',err=957)
      read(1,*) rfw0
      close(1)
      if(rfw0.lt.0) rfw=-rfw0
      goto 958
 957  continue
      close(1)
 958  continue
      if(ifwf.eq.1) rfw=fwin
      if(rfw.le.0) rfw=1.55
      return
      end

      subroutine fit2d(ra,dec,rfw,ntf,sflux,sfluxe,raf,decf,intuse,
     $     xw,ntf2,gausa,iflag,fadcw,az,chi,amps,sumrata)

      parameter(nmax=1500,nfmax=1500)
      real raf(ntf),decf(ntf),sflux(ntf),sfluxe(ntf),gausa(nfmax)
      real xr(nmax),xd(nmax),wadc(5),adc(5),fadcw(nfmax,5),az(ntf)
      real da(nfmax),xw(nfmax),fadc(nfmax,5),sumrata(nmax)
      real relnorm(3),xrel(nfmax)
      real sfluxn(nfmax),sfluxen(nfmax),xwn(nfmax)
      integer iflag(nfmax),intuse(nfmax)
      parameter(pi=3.141593e0)      
      common/csigma/ rsig,fmof,bmof,imoff
      common/cfwhm/ ifwf,fwin

      imoff=1
      
      rsig=rfw/2.35
      fmof=rfw
      bmof=3.9

      ntf2=0
      cosd=cos(dec/57.3)
      do i=1,ntf
         if(intuse(i).eq.1) then
            ntf2=ntf2+1
            sfluxn(ntf2)=sflux(i)
            sfluxen(ntf2)=sfluxe(i)
            xwn(ntf2)=xw(i)
            iflag(ntf2)=0
            if(sflux(i).eq.0) iflag(ntf2)=1
            xr(ntf2)=raf(i)-ra
            xd(ntf2)=decf(i)-dec
            xr(ntf2)=xr(ntf2)*3600.*cosd
            xd(ntf2)=xd(ntf2)*3600.
         endif
      enddo

      amps=1.
      call getchifib2(0.,0.,amps,ntf2,xr,xd,sfluxn,xwn,sfluxen,
     $     iflag,da,gausa,chi,1,sumrat)

c- now get the atmospheric distortion correction to each fiber

      nw=5
      wadc(1)=3500.
      wadc(2)=4000.
      wadc(3)=4500.
      wadc(4)=5000.
      wadc(5)=5500.
      adc(1)=-0.74
      adc(2)=-0.4
      adc(3)=0.0
      adc(4)=0.08
      adc(5)=0.20
      call adcor(nw,wadc,adc,fadc,0.,0.,ntf2,xr,xd,az,sumrata)
      do i=1,ntf2
         do j=1,nw
            fadcw(i,j)=fadc(i,j)/fadc(i,3)
         enddo
      enddo

      return
      end

      subroutine getchifib(xrs,xds,amps,n,xr,xd,xf,xw,fe,iflag,da,
     $     gausa,chi,ip,sumrat)
      real xr(n),xd(n),xf(n),xw(n),da(n),fe(n)
      real gausa(n)
      integer iflag(n)
      parameter(pi=3.141593e0)      
      common/csigma/ rsig,fmof,bmof,imoff

c      if(ip.eq.1) write(*,*) "Ifib     Counts      Fit",
c     $     "       Distance        C-F           Chi"

      rfib=0.75
c      nstep=50
      nstep=20
      xstep=2.*rfib/float(nstep-1)
      deltx=pi*rfib*rfib
      area=amps*deltx/(2.*rsig*rsig*pi)
      areamoff=4.*(2.**(1./bmof)-1.)*(bmof-1.)/pi/fmof/fmof
      areamoff=amps*deltx*areamoff
      chi=0.
      do i=1,n
         xs=xr(i)-rfib
         ys=xd(i)-rfib
         gaus=0.
         xmoff=0.
         nsum=0
         do ix=1,nstep
            xp=xs+xstep*float(ix-1)
            do iy=1,nstep
               yp=ys+xstep*float(iy-1)
               dist0=sqrt((xp-xr(i))**2+(yp-xd(i))**2)
               if(dist0.lt.rfib) then
                  dist=sqrt((xp-xrs)**2+(yp-xds)**2)
                  g=dist/rsig
                  gaus=gaus+exp(-g*g/2.)*area
                  xmoff=xmoff+areamoff*((1.+4.*(2.**(1./bmof)-1.)*
     $                 (dist/fmof)**2)**(-bmof))
                  nsum=nsum+1
               endif
            enddo
         enddo
         gaus=gaus/float(nsum)
         xmoff=xmoff/float(nsum)
         if(imoff.eq.1) gaus=xmoff
         gausa(i)=gaus
         if(fe(i).gt.0) then
            chi1=xw(i)*(gaus-xf(i))**2/(fe(i))**2
         else
            chi1=0.
         endif
         da(i)=xf(i)-gaus
c         chia(i)=da(i)
         if(iflag(i).eq.0) chi=chi+chi1
         rad=sqrt((xr(i)-xrs)**2+(xd(i)-xds)**2)
c         if(ip.eq.1) write(*,1001) i,xf(i),gaus,rad,da(i),chi1,
c     $        an(i,1),an(i,2),an(i,3),an(i,4),iflag(i)
      enddo

 1001 format(i3,1x,f12.2,2(2x,f9.2),2(2x,f12.2),
     $     2x,a17,1x,a8,1x,a3,1x,a5,1x,i1)
      return
      end

      subroutine getchifib2(xrs,xds,amps,n,xr,xd,xf,xw,fe,iflag,da,
     $     gausa,chi,ip,sumrat)
      real xr(n),xd(n),xf(n),xw(n),da(n),fe(n)
      real gausa(n)
      integer iflag(n)
      parameter(pi=3.141593e0)      
      common/csigma/ rsig,fmof,bmof,imoff

      radmax=3.*fmof

      rfib=0.75
c      nstep=20
      nstep=10
      xstep=2.*rfib/float(nstep-1)
      deltx=pi*rfib*rfib
      area=amps*deltx/(2.*rsig*rsig*pi)
      areamoff=4.*(2.**(1./bmof)-1.)*(bmof-1.)/pi/fmof/fmof
      areamoff=amps*deltx*areamoff
      chi=0.
      do i=1,n
c         radcheck=sqrt((xr(i)-xrs)**2+(xd(i)-xds)**2)
c         if(radcheck.gt.radmax) goto 888
         xs=xr(i)-rfib
         ys=xd(i)-rfib
         gaus=0.
         xmoff=0.
         nsum=0
         do ix=1,nstep
            xp=xs+xstep*float(ix-1)
            do iy=1,nstep
               yp=ys+xstep*float(iy-1)
               dist0=sqrt((xp-xr(i))**2+(yp-xd(i))**2)
               if(dist0.lt.rfib) then
                  dist=sqrt((xp-xrs)**2+(yp-xds)**2)
                  g=dist/rsig
                  gaus=gaus+exp(-g*g/2.)*area
                  xmoff=xmoff+areamoff*((1.+4.*(2.**(1./bmof)-1.)*
     $                 (dist/fmof)**2)**(-bmof))
                  nsum=nsum+1
               endif
            enddo
         enddo
         gaus=gaus/float(nsum)
         xmoff=xmoff/float(nsum)
         if(imoff.eq.1) gaus=xmoff
         gausa(i)=gaus
 888     continue
      enddo

      ae=2.e5
 444  continue
      na=1000
      as=1.
      chimin=1e10
      do ia=1,na
         chi=0
         at=as+float(ia-1)/float(na-1)*(ae-as)
         do i=1,n
            gaus=gausa(i)*at
            if(fe(i).gt.0) then
               chi1=xw(i)*(gaus-xf(i))**2/(fe(i))**2
            else
               chi1=0.
            endif
            if(iflag(i).eq.0) chi=chi+chi1
         enddo
         if(chi.lt.chimin) then
            chimin=chi
            amps=at
         endif
      enddo

      if(amps.ge.ae) then
         ae=10.*ae
         goto 444
      endif

      na=1000
      as=amps*0.6
      ae=amps*1.4
      chimin=1e10
      do ia=1,na
         chi=0
         at=as+float(ia-1)/float(na-1)*(ae-as)
         do i=1,n
            gaus=gausa(i)*at
            if(fe(i).gt.0) then
               chi1=xw(i)*(gaus-xf(i))**2/(fe(i))**2
            else
               chi1=0.
            endif
            if(iflag(i).eq.0) chi=chi+chi1
         enddo
         if(chi.lt.chimin) then
            chimin=chi
            amps=at
         endif
      enddo

      do i=1,n
         gaus=gausa(i)*amps
         da(i)=xf(i)-gaus
      enddo
      chi=chimin

c- now get it for each exposure
c      chie(1)=0.
c      chie(2)=0.
c      chie(3)=0.
c      do i=1,n
c         gaus=gausa(i)*amps
c         if(fe(i).gt.0) then
c            chi1=xw(i)*(gaus-xf(i))**2/(fe(i))**2
c         else
c            chi1=0.
c         endif
c         if(iflag(i).eq.0) then
c            if(iexp(i).eq.1) chie(1)=chie(1)+chi1
c            if(iexp(i).eq.2) chie(2)=chie(2)+chi1
c            if(iexp(i).eq.3) chie(3)=chie(3)+chi1
c         endif
c      enddo

 1001 format(i3,1x,f12.2,2(2x,f9.2),2(2x,f12.2),
     $     2x,a17,1x,a8,1x,a3,1x,a5,1x,i1)
      return
      end

      subroutine adcor(nw,wadc,adc,fadc,xrs0,xds0,n,xr,xd,az,sumrata)
      parameter(nfmax=1500)
      real wadc(nw),adc(nw),fadc(nfmax,5),xr(n),xd(n),az(n),sumrata(nw)
      parameter(pi=3.141593e0)      
      common/csigma/ rsig,fmof,bmof,imoff

      dtr=180./pi
      xrs=xrs0
      xds=xds0
      rfib=0.75
c      nstep=50
c      nstep=20
      nstep=10
      xstep=2.*rfib/float(nstep-1)
      deltx=pi*rfib*rfib
      do ia=1,nw
c         rsig0=rsig
c         fmof0=fmof
         rsig0=rsig/((wadc(ia)/4550.)**0.2)
         fmof0=fmof/((wadc(ia)/4550.)**0.2)
         area=1.*deltx/(2.*rsig0*rsig0*pi)
         areamoff=4.*(2.**(1./bmof)-1.)*(bmof-1.)/pi/fmof0/fmof0
         areamoff=deltx*areamoff
         xaoff=adc(ia)*sin(az(1)/dtr)
         yaoff=adc(ia)*cos(az(1)/dtr)
         sumgw=0.
         do i=1,n
            gaus=0.
            xmoff=0.
            nsum=0
            xaoff=adc(ia)*sin(az(i)/dtr)
            yaoff=adc(ia)*cos(az(i)/dtr)
            xs=xr(i)-rfib+xaoff
            ys=xd(i)-rfib+yaoff
            do ix=1,nstep
               xp=xs+xstep*float(ix-1)
               do iy=1,nstep
                  yp=ys+xstep*float(iy-1)
                  dist0=sqrt((xp-xr(i))**2+(yp-xd(i))**2)
                  if(dist0.lt.rfib) then
                     dist=sqrt((xp-xrs)**2+(yp-xds)**2)
                     g=dist/rsig0
                     gaus=gaus+exp(-g*g/2.)*area
                     xmoff=xmoff+areamoff*((1.+4.*(2.**(1./bmof)-1.)*
     $                    (dist/fmof0)**2)**(-bmof))
                     nsum=nsum+1
                  endif
               enddo
            enddo
            gaus=gaus/float(nsum)
            xmoff=xmoff/float(nsum)
            if(imoff.eq.1) gaus=xmoff
            fadc(i,ia)=gaus
            sumgw=sumgw+gaus
         enddo

         sumrata(ia)=sumgw
c         do i=1,n
c            fadc(i,ia)=fadc(i,ia)/sumrat
c         enddo
      enddo

      return
      end

      subroutine combspec(nwa,ntf,wa,fa,fea,fa2,fea2,ntf2,
     $     intuse,weight,iflag,iexp,fadcw,az,nspec,spec,sumrata)
      parameter(nmax=1500,nfmax=1500)
      real wa(nfmax,nmax),fa(nfmax,nmax),fea(nfmax,nmax)
      real fa2(nfmax,nmax),fea2(nfmax,nmax),wv(5),wn(5)
      real wan(nfmax,nmax),fan(nfmax,nmax),fean(nfmax,nmax)
      real fa2n(nfmax,nmax),fea2n(nfmax,nmax),az(ntf),sumrata(nmax)
      real spec(nmax*10,9),weight(nfmax),fadcw(nfmax,5)
      integer intuse(nfmax),nwa(nfmax),iflag(nfmax),iexp(nfmax)
      integer nwan(nfmax),iflag2(nfmax)

      wcut=0.01
      nspec=0
      sum=0.
      nn=0
      do i=1,ntf
         if(intuse(i).eq.1) then
            nn=nn+1
            nwan(nn)=nwa(i)
            iflag2(nn)=iflag(i)
            do j=1,nwa(i)
               wan(nn,j)=wa(i,j)
               fan(nn,j)=fa(i,j)
               fa2n(nn,j)=fa2(i,j)
               fean(nn,j)=fea(i,j)
               fea2n(nn,j)=fea2(i,j)
            enddo
         endif
      enddo

      nw=5
      wv(1)=3500.
      wv(2)=4000.
      wv(3)=4500.
      wv(4)=5000.
      wv(5)=5500.

      do i=1,ntf2
         if(weight(i).gt.wcut) then
            sum=sum+weight(i)
         endif
      enddo
      if(sum.le.0.05) return

      do i=1,ntf2
         if(weight(i).gt.wcut) then
            xnorm0=weight(i)
            do j=1,nw
               wn(j)=fadcw(i,j)
            enddo
            do j=1,nwan(i)
               nspec=nspec+1
               x1=wan(i,j)
               call xlinint(x1,nw,wv,wn,fadc)
               xnorm=xnorm0*fadc
c               print *,i,j,fadc,xnorm
               spec(nspec,1)=wan(i,j)
               spec(nspec,2)=fan(i,j)/xnorm
               spec(nspec,3)=fa2n(i,j)*1.0e17/xnorm
               spec(nspec,4)=fean(i,j)/xnorm
               spec(nspec,5)=fea2n(i,j)*1.0e17/xnorm
               spec(nspec,6)=0.
               spec(nspec,7)=0.
               spec(nspec,8)=xnorm
               call xlinint(wan(i,j),nw,wv,sumrata,wgeom)
               spec(nspec,9)=wgeom
            enddo
         endif
      enddo

      return
      end

      subroutine sumspec(nwa,ntf,wa,fa,fea,fa2,fea2,ntf2,intuse,gna0,
     $     iflag,iexp,fadcw,az,spec,sumrata,fweight2,specexp)
      parameter(nmax=1500,nfmax=1500)
      real wa(nfmax,nmax),fa(nfmax,nmax),fea(nfmax,nmax)
      real fa2(nfmax,nmax),fea2(nfmax,nmax),gnw(nmax)
      real gna(nfmax),gna0(nfmax),az(ntf),spec(nmax*10,9),wn(5)
      real x(nmax),y(nmax),y2(nmax),ye(nmax),ye2(nmax)
      real wv(5),fadcw(nfmax,5),ysum3(nmax),ysum3e(nmax),gsum(nmax)
      real ysum(nmax),ysum2(nmax),ysume(nmax),ysum2e(nmax),fw2(nmax)
      real yout1(nmax),yout2(nmax),yout3(nmax),yout4(nmax),yout5(nmax)
      real yout6(nmax),yout7(nmax),sumrata(nmax),fweight2(nfmax,nmax)
      real wan(nfmax,nmax),fan(nfmax,nmax),fean(nfmax,nmax)
      real xin(nmax),xin2(nmax),xerro(nmax)
      real ysumexp(nmax,3),gsumexp(nmax,3),specexp(nmax,3)
      real fa2n(nfmax,nmax),fea2n(nfmax,nmax),fweight2n(nfmax,nmax)
      integer iflag(ntf),intuse(nfmax),iexp(ntf),nwa(nfmax)
      integer iflag2(nfmax),iexp2(nfmax)

c      fcut=0.03
c      fcut=0.01
      fcut=0.0

      naf=nwa(1)
      nn=0
      do i=1,ntf
         if(intuse(i).eq.1) then
            nn=nn+1
            nin=0
            iflag2(nn)=iflag(i)
            iexp2(nn)=iexp(i)
            do j=1,naf
               wan(nn,j)=wa(i,j)
               fan(nn,j)=fa(i,j)
               fean(nn,j)=fea(i,j)
               fa2n(nn,j)=fa2(i,j)
               fea2n(nn,j)=fea2(i,j)
               fweight2n(nn,j)=fweight2(i,j)
c               print *,i,j,fweight2n(nn,j)
c               if(wan(nn,j).gt.5055.and.wan(nn,j).lt.5155) then
c                  nin=nin+1
c                  xin(nin)=fan(nn,j)
c                  xin2(nin)=fean(nn,j)
c               endif
            enddo
c            call biwgt(xin,nin,xb1,xs1)
c            call biwgt(xin2,nin,xb2,xs2)
c            print *,i,nin,xs1,xb2,(xb2-xs1)/xs1
         endif
      enddo

      do i=1,nmax
         ysum(i)=0.
         ysum2(i)=0.
         ysume(i)=0.
         ysum2e(i)=0.
         ysum3(i)=0.
         ysum3e(i)=0.
         fw2(i)=0.
         gsum(i)=0.
         gnw(i)=0.
         do j=1,3
            ysumexp(i,j)=0.
            gsumexp(i,j)=0.
         enddo
      enddo
      do i=1,ntf2
         gna(i)=0.
      enddo
      do i=1,naf
         spec(i,1)=wan(1,i)
      enddo

      nw=5
      wv(1)=3500.
      wv(2)=4000.
      wv(3)=4500.
      wv(4)=5000.
      wv(5)=5500.

c - sumgall is total counts (amplitude of fit)
cc   gna is normalized counts for each                                 
      sumg=0.
      sumgall=0.
      ng=0
      do i=1,ntf2
         sumgall=sumgall+gna0(i)
         if(iflag2(i).eq.0) then
            ng=i
            gna(ng)=gna0(i)
            sumg=sumg+gna0(i)
         endif
      enddo
      if(ng.eq.0) goto 866
      if(sumg.eq.0) goto 866
      do i=1,ng
c         gna(i)=gna(i)/sumg
         gna(i)=gna(i)/sumgall
      enddo

c - first get normalization for each wavelength
      do il=1,ntf2
         if(gna(il).lt.fcut) iflag2(il)=1
         if(iflag2(il).eq.0) then
            do i=1,nw
               wn(i)=fadcw(il,i)
            enddo
            do i=1,naf
               x1=wan(il,i)
               call xlinint(x1,nw,wv,wn,fadc)
               gnw(i)=gnw(i)+gna0(il)*fadc
c               gnw(i,il)=gnw(i,il)+gna0(il)*fadc
c               print *,il,i,x1,gnw(i)
            enddo
         endif
      enddo

c - get the weighted sum
      do il=1,ntf2
         gn0=gna0(il)
         if(gna(il).lt.fcut) iflag2(il)=1
         if(iflag2(il).eq.0) then
            n=0
            do i=1,nw
               wn(i)=fadcw(il,i)
            enddo
            do i=1,naf
               x1=wa(il,i)
               call xlinint(x1,nw,wv,wn,fadc)
               n=n+1
c               gn=gn0/fadc/gnw(n,il)
               gn=gn0/fadc/gnw(n)
               x(n)=x1
               y(n)=fan(il,i)
               y2(n)=fa2n(il,i)
               ye(n)=fean(il,i)
               ye2(n)=fea2n(il,i)
               ysum(n)=ysum(n)+y(n)*gn
               ysum2(n)=ysum2(n)+y2(n)*gn
               ysume(n)=ysume(n)+ye(n)*ye(n)*gn
               ysum2e(n)=ysum2e(n)+ye2(n)*ye2(n)*gn
               fw2(n)=fw2(n)+fweight2n(il,i)*gn
               gsum(n)=gsum(n)+gn*gn
               iexp1=iexp2(il)
               ysumexp(n,iexp1)=ysumexp(n,iexp1)+y(n)*gn
               gsumexp(n,iexp1)=gsumexp(n,iexp1)+gn*gn
            enddo
         endif
      enddo

c - get the straight sum
      do il=1,ntf2
         if(iflag2(il).eq.0) then
            n=0
            do i=1,naf
               n=n+1
               ysum3(n)=ysum3(n)+fan(il,i)
               ysum3e(n)=ysum3e(n)+fean(il,i)*fean(il,i)
            enddo
         endif
      enddo

      nin=0
      do i=1,naf
         if(gsum(i).gt.0) then
            facu=1./gsum(i)
c            facu=1.
         else
            facu=0.
         endif
         xs1=sqrt(ysume(i)*facu)
         xs2=sqrt(ysum2e(i)*facu)
         xs3=sqrt(ysum3e(i))
         spec(i,1)=wan(1,i)
         spec(i,2)=ysum(i)*facu
         spec(i,3)=ysum2(i)*facu*1.0e17
         spec(i,4)=xs1
         spec(i,5)=xs2*1.0e17
         spec(i,6)=ysum3(i)
         spec(i,7)=xs3
c         spec(i,8)=fac2
         xdum=0.
         if(gsum(i).gt.0) xdum=fw2(i)/gsum(i)
         if(xdum.gt.0) then
            spec(i,8)=1./xdum
c            spec(i,8)=1.
         else
            spec(i,8)=0.
         endif
         call xlinint(wa(1,i),nw,wv,sumrata,wgeom)
         spec(i,9)=wgeom
         xin(i)=spec(i,2)
         xin2(i)=spec(i,3)
c- get the individual exposure spectra
         do j=1,3
            if(gsumexp(i,j).gt.0) then
               specexp(i,j)=ysumexp(i,j)/gsumexp(i,j)
            else
               specexp(i,j)=0.
            endif
         enddo
      enddo
 866  continue

c- modify the summed errors to be empirical (I don't like this, so skip)
      goto 888
      frace=2.5
      call geterre(naf,xin,xerro)
      do i=1,naf
         sp1=spec(i,4)
         if(sp1.gt.0.5*xerro(i).and.sp1.lt.frace*xerro(i)) then
            spec(i,4)=xerro(i)
         endif
      enddo
      call geterre(naf,xin2,xerro)
      do i=1,naf
         sp1=spec(i,5)
         if(sp1.gt.0.5*xerro(i).and.sp1.lt.frace*xerro(i)) then
            spec(i,5)=xerro(i)
         endif
      enddo
 888  continue

      return
      end

      subroutine geterre(n,x,xo)
      real x(n),xo(n),xin(1036)
      ne=51
      nh=(ne-1)/2
      do i=1,n
         is=max(1,i-nh)
         ie=min(n,is+ne-1)
         nin=0
         do j=is,ie
            nin=nin+1
            xin(nin)=x(j)
         enddo
         call biwgt(xin,nin,xb,xs)
         xo(i)=xs
      enddo

      return
      end

      subroutine xlinint(xp,n,x,y,yp)
      real x(n),y(n)
      do j=1,n-1
         if(xp.ge.x(j).and.xp.lt.x(j+1)) then
            yp=y(j)+(y(j+1)-y(j))*(xp-x(j))/(x(j+1)-x(j))
            return
         endif
      enddo
      if(xp.lt.x(1)) yp=y(1)
      if(xp.gt.x(n)) yp=y(n)
      return
      end

      subroutine xlinint2(xp,n,x,y,yp,jin,jout)
      real x(n),y(n)
      do j=jin,n-1
         if(xp.ge.x(j).and.xp.le.x(j+1)) then
            yp=y(j)+(y(j+1)-y(j))*(xp-x(j))/(x(j+1)-x(j))
            jout=j
            return
         endif
      enddo
      if(xp.lt.x(1)) yp=y(1)
      if(xp.gt.x(n)) yp=y(n)
      jout=1
      return
      end

      subroutine xlininte(xp,n,x,y,yp,jin,jout)
      real x(n),y(n)
      do j=jin,n-1
         if(xp.ge.x(j).and.xp.le.x(j+1)) then
c            yp=y(j)+(y(j+1)-y(j))*(xp-x(j))/(x(j+1)-x(j))
            yp=(xp-x(j))/(x(j+1)-x(j))
            yp=1.414-0.414*abs(0.5-yp)*2.
            jout=j
            return
         endif
      enddo
      if(xp.lt.x(1)) yp=1.
      if(xp.gt.x(n)) yp=1.
      jout=1
      return
      end

      subroutine fitspec(n0,spec,signsl0,wcen,wrange,ifit1,ifit1o,
     $     nsim,cfield,ra,dec,nout,fitout,xoutv,xouts,
     $     nflim,flimw,flimv,flima,ratnoise,icheck)

      parameter(nmax=1500,nca=17,nsimmax=5000,nmaxs=nmax*10)
      real spec(nmaxs,9),xin(nmaxs)
      real wave(nmaxs),flux(nmaxs),x(nmaxs),y(nmaxs),alpha(nca,nca)
      real a(nca),yin(nmax),covar(nca,nca),aout(10,nsimmax)
      real fluxe(nmaxs),ye(nmaxs),fitout(3000,6),xoutv(nca),xouts(nca)
      real yo(nmaxs),yeo(nmaxs),yfito(nmaxs),ao(nca)
      real flimw(1036),flimv(1036),flima(1036)
      character alist(10)*10,cfield*12
      parameter(pi=3.141592e0,cee=2.99792458e5)
      data big/1.e20/
      common/aval/ np,ifitsig

      n=n0
      ifit=0
      nout=0
      sncut=4.0
      chicut=5.
      if(ifit1.eq.1) sncut=0.
      sncuthi=900.
      ampcut=1.e30
      sigcut=50.
      wavecut=20.
      pixsize=2.

      wavec=50.

      if(signsl0.lt.0) then
         signsl=-signsl0
         ifitsig=0
      else
         signsl=signsl0
         ifitsig=1
      endif

      ws=spec(1,1)
      we=spec(n,1)
      dwave=0.5
      dwave=1.0
      dwave=8.0
      nw=nint((we-ws)/dwave)+1

c- get flux limits for 105 and then return
      if(ifit1o.eq.105) then
         dwave=2.0
         nw=nint((we-ws)/dwave)+1
         nflim=nw
         sigg=signsl
c         xnp=4.*sigg
c         xnp=xnp/pixsize
         xnp=4.*sigg
         xnp=xnp/pixsize
         xnp=max(3.,xnp)
         nflimh=3
         do iw=1,nw
            wfit=spec(iw,1)
c - get noise from the errors
            w1=wfit-xnp
            w2=wfit+xnp
            xnoise2=0.
            xnoise3=0.
            nerr=0
            ilo=max(1,iw-nflimh)
            ihi=min(nw,iw+nflimh)
            do i=ilo,ihi
               xnoise2=xnoise2+spec(i,5)**2
               if(spec(i,5).gt.0) xnoise3=xnoise3+1./spec(i,5)**2
               nerr=nerr+1
            enddo
c            do i=1,nw
c               xwf=spec(i,1)
c               if(xwf.ge.w1.and.xwf.le.w2) then
c                  xnoise2=xnoise2+spec(i,5)**2
c                  if(spec(i,5).gt.0) xnoise3=xnoise3+1./spec(i,5)**2
c                  nerr=nerr+1
c               endif
c            enddo
            xnoise2=sqrt(xnoise2)
c            xnoise3=sqrt(xnoise3/float(nerr))
c            xnoise3=1./xnoise3
c            print *,spec(iw,1),nerr,xnoise2,xnoise3,spec(iw,5)
            flimw(iw)=wfit
            flimv(iw)=xnoise2
            flima(iw)=spec(iw,9)
         enddo
         return
      endif

      if(ifit1.eq.1) nw=1

      nadd=5

      do i=1,n
         wave(i)=spec(i,1)
         flux(i)=spec(i,3)/spec(i,9)
         fluxe(i)=spec(i,5)/spec(i,9)
         if(flux(i).eq.0) flux(i)=-666
      enddo

      do iall=1,nw
         wave0=ws+float(iall-1)*(we-ws)/float(nw-1)
         if(ifit1.eq.1) then
            wave0=wcen
            wavec=wrange
         endif
         wlo=wave0-wavec
         wup=wave0+wavec
         ymax=-big
         nt=0

         do i=1,n
            if(wave(i).gt.wlo.and.wave(i).lt.wup.and.
     $           nint(flux(i)).ne.-666) then
               nt=nt+1
               x(nt)=wave(i)
               yo(nt)=flux(i)
               yin(nt)=flux(i)
               yeo(nt)=fluxe(i)
               ymax=max(ymax,y(nt))
            endif
         enddo
         call biwgt(yin,nt,xb,xs)
         call biwgt(yin,12,xb,xs)

         amp=(ymax-xb)*3.5
         ao(1)=signsl
         ao(2)=0.0
         ao(3)=0.
         ao(4)=xb
         ao(5)=0.
         ao(6)=wave0
         ao(7)=amp
         ao(8)=signsl
         ao(9)=0.
         na=8
         np=1

         idum=-1
         ngood=0
         do isim=1,nsim
            do i=1,9
               a(i)=ao(i)
            enddo
            do i=1,nca
               do j=1,nca
                  covar(i,j)=0.
                  alpha(i,j)=0.
               enddo
            enddo

            do i=1,nt
               ye(i)=yeo(i)
               if(isim.eq.1) then
                  y(i)=yo(i)
               else
                  y(i)=yfito(i)+yeo(i)*gasdev(idum)
               endif
            enddo

            call fitherms(nt,x,y,ye,a,na,covar,alpha,nca)

            h3=a(2)
            h4=a(3)
            con=a(4)
            xoff=a(5)
            rms=0.
            chi=0.
            chi0=0.
            sume=0.
            do ia=1,nt
               yfit=con
               do i=1,np
                  sigg=a(i+nadd+np+np)
                  amp=a(i+nadd+np)
                  vel=a(i+nadd)+xoff
                  w=(x(ia)-vel)/sigg
                  gaus=exp(-w*w/2.)/sqrt(2.*pi*sigg**2)
                  yfit=yfit+amp*gaus*(1.+h3*fh3(w)+h4*fh4(w))
               enddo
               rms=rms+(y(ia)-yfit)**2
               if(ye(ia).gt.0) chi=chi+((y(ia)-yfit)/ye(ia))**2
               chi0=chi0+((y(ia)-con)/ye(ia))**2
               if(isim.eq.1) yfito(ia)=yfit
               sume=sume+ye(ia)
            enddo
            rms=sqrt(rms/float(nt))
            sume=sume/float(nt)
            chi=chi/float(nt)
            chi0=chi0/float(nt)

            wfit=a(6)+a(5)

c - check if enough data under the fit
            wfit0=wfit
            nfit=0
            do ii=1,nt
               if(x(ii).gt.(wfit0-6).and.x(ii).lt.(wfit0+6)) nfit=nfit+1
            enddo

            if(nfit.le.5) goto 766
            if(sigg.gt.sigcut) goto 766
            if(chi.gt.chicut) goto 766
            if(con.gt.1000) goto 766
            if(amp.le.0) goto 766
            if(abs(wfit-wave0).gt.wavecut) goto 766
            if((wfit-x(1)).lt.6) goto 766
            if((x(nt)-wfit).lt.6) goto 766

            sigg=sqrt(sigg*sigg)
            xnp=4.*sigg
            xnp=xnp/pixsize
            xnp=max(3.,xnp)

c- get noise from the rms
            xnoise=rms*sqrt(xnp)
            ston=0.95*amp/xnoise/pixsize

c- get noise from the rms (empirical) and the errors
            w1=wfit-xnp
            w2=wfit+xnp
            nerr=0
            xn1=0.
            xn2=0.
            do i=1,nt
               if(x(i).ge.w1.and.x(i).le.w2.and.ye(i).gt.0) then
                  nerr=nerr+1
c                  xn1=xn1+1./rms/rms
c                  xn2=xn2+1./ye(i)/ye(i)
                  xn1=xn1+rms*rms
                  xn2=xn2+ye(i)*ye(i)
               endif
            enddo
            if(nerr.gt.0) then
c               xn1=sqrt(float(nerr)/xn1)
c               xn2=sqrt(float(nerr)/xn2)
               xn1=sqrt(xn1/float(nerr))
               xn2=sqrt(xn2)
c               ston=0.95*amp/pixsize/xn1
c               ston2=0.95*amp/pixsize/xn2
               ston=0.95*amp/xn1/pixsize
               ston2=0.95*amp/xn2/pixsize
            else
               xn1=0.
               xn2=0.
               ston=0.
               ston2=0.
            endif
c            xnoise=xn1
            xnoise2=xn2
c            if(isim.eq.1) print *,nerr,ston,ston2,xn2,xn1
c     $           xnoise,xnoise2,0.95*amp/2.
c - check this next setting carefully!
c            if(xnoise2.gt.2.0*xnoise) ston=ston2
            ston=ston2
            frat=1.
            if(sume.gt.0.and.isim.eq.1) then
               frat=rms/sume
               if(rms.le.0.2*sume) frat=1.
               chin=chi/frat/frat
               chi=max(chi,chin)
            endif

            if(isim.eq.1) then
               ratnoise=0.
               if(xnoise.gt.0) ratnoise=xnoise2/xnoise
               ratnoise=min(9.9,ratnoise)
               frat1=frat
               do i=1,nt
                  yeo(i)=yeo(i)*frat1
               enddo
            endif

            ngood=ngood+1
            aout(1,ngood)=wfit
            aout(2,ngood)=amp/pixsize
            aout(3,ngood)=sigg
            aout(4,ngood)=con
            aout(5,ngood)=ston
            aout(6,ngood)=chi

            if(ston.lt.sncut) goto 767
            if(ston.gt.sncuthi) goto 767

            ifit=ifit+1
            nout=ifit
            fitout(ifit,1)=wfit
            fitout(ifit,2)=amp/pixsize
            fitout(ifit,3)=abs(a(8))
            fitout(ifit,4)=ston
            fitout(ifit,5)=con
            fitout(ifit,6)=chi
            goto 767
 766        continue
            if(isim.eq.1.and.nsim.ne.1) goto 966
 767        continue
            if(ifit1.eq.1.and.nsim.eq.1) goto 966
         enddo
      enddo
 966  continue

      if(nsim.ge.10) then
c         print *,"Number of simulations: ",ngood,frat1
         n16=nint(0.16*float(ngood))
         n84=nint(0.84*float(ngood))
         alist(1)="Wave"
         alist(2)="Amp"
         alist(3)="Sig"
         alist(4)="Con"
         alist(5)="ston"
         alist(6)="chi"

         do ia=1,6
            do isim=1,ngood
               xin(isim)=aout(ia,isim)
            enddo
            call biwgt(xin,ngood,xb,xs)
            bias=aout(ia,1)-xb
            xoutv(ia)=aout(ia,1)
            xouts(ia)=xs
         enddo

      endif

 1001 format(1x,a6,4(1x,f8.2))
 1102 format(12(1x,f8.2)2(1x,f10.6),1x,a12)

      return
      end

      subroutine fitherms(n,x,y,sig,a,na,covar,alpha,nca)

      parameter(ncaf=17)

      real x(n),y(n),a(na),covar(nca,nca)
      real alpha(nca,nca),sig(n)
      integer ia(ncaf)
      common/aval/ np,ifitsig

c      data tol,itermax/1.e-4,1000/
      data tol,itermax/1.e-3,500/

      nadd=5
      do i=1,na
         ia(i)=1
      enddo
      do i=nadd+1,np+nadd
         ia(i)=0
      enddo
      ia(1)=0
      ia(2)=0
      ia(3)=0
      ia(4)=1
c     this is wavelength: 0 fix, 1 fit
c      ia(5)=0
c      ia(6)=0
c     this is sigma: 0 fix, 1 fit
      ia(8)=0
      if(ifitsig.eq.1) ia(8)=1

      alamda=-1
      alamo=1.e10
      cold=1.e10
      do iter=1,itermax
         call mrqminb(x,y,sig,n,a,ia,na,covar,alpha,nca,
     $        chisq,alamda)
         chirel=abs(cold-chisq)/chisq
         cold=chisq
         if(alamda.lt.alamo.and.chirel.lt.tol) goto 666
         if(alamda.gt.1.e9) goto 666
         alamo=alamda
      enddo
 666  continue

      call mrqminb(x,y,sig,n,a,ia,na,covar,alpha,nca,
     $     chisq,0.)

      return
      end

      subroutine funcs(x,a,yfit,dyda,na)
      real a(na),dyda(na)
      parameter(pi=3.141593e0)
      common/aval/ np,ifitsig

      h3=a(2)
      h4=a(3)
      con=a(4)
      xoff=a(5)
      nadd=5
      yfit=con
      do i=1,na
         dyda(i)=0.
      enddo
      dyda(4)=1.
      do i=1,np
         sig=a(i+nadd+np+np)
         amp=a(i+nadd+np)
         vel=a(i+nadd)+xoff
         w=(x-vel)/sig
         gaus=exp(-w*w/2.)/sqrt(2.*pi*sig*sig)
         yadd=amp*gaus*(1.+h3*fh3(w)+h4*fh4(w))
         yfit=yfit+yadd
         dyda(1)=dyda(1)+
     $        (-yadd)/sig+yadd*w*(x-vel)/sig/sig+amp*gaus*(
     $        -h3*dfh3(w)*(x-vel)/sig/sig-h4*dfh4(w)*(x-vel)/sig/sig)
         dyda(2)=dyda(2)+amp*gaus*fh3(w)
         dyda(3)=dyda(3)+amp*gaus*fh4(w)
         dyda(5)=dyda(5)+
     $        yadd*w/sig+amp*gaus*(-h3*dfh3(w)/sig-h4*dfh4(w)/sig)
         dyda(i+nadd+np)=yadd/amp
         dyda(i+nadd)=1.
         dyda(i+nadd+np+np)=
     $        (-yadd)/sig+yadd*w*(x-vel)/sig/sig+amp*gaus*(
     $        -h3*dfh3(w)*(x-vel)/sig/sig-h4*dfh4(w)*(x-vel)/sig/sig)
      enddo

      return
      end

      function fh3(x)
      fh3=1./sqrt(6.)*(2.*sqrt(2.)*x*x*x-3.*sqrt(2.)*x)
      return
      end
      function fh4(x)
      fh4=1./sqrt(24.)*(4.*x*x*x*x-12.*x*x+3.)
      return
      end
      function dfh3(x)
      dfh3=1./sqrt(6.)*(6.*sqrt(2.)*x*x-3.*sqrt(2.))
      return
      end
      function dfh4(x)
      dfh4=1./sqrt(24.)*(16.*x*x*x-24.*x)
      return
      end

      SUBROUTINE mrqminb(x,y,sig,ndata,a,ia,ma,covar,alpha,nca,chisq,
     *alamda)
      INTEGER ma,nca,ndata,ia(ma),MMAX
      REAL alamda,chisq,a(ma),alpha(nca,nca),covar(nca,nca),
     *sig(ndata),x(ndata),y(ndata)
      PARAMETER (MMAX=100)
CU    USES covsrt,gaussj,mrqcof
      INTEGER j,k,l,m,mfit
      REAL ochisq,atry(MMAX),beta(MMAX),da(MMAX)
      SAVE ochisq,atry,beta,da,mfit
      if(alamda.lt.0.)then
        mfit=0
        do 11 j=1,ma
          if (ia(j).ne.0) mfit=mfit+1
11      continue
        alamda=0.001
        call mrqcofb(x,y,sig,ndata,a,ia,ma,alpha,beta,nca,chisq)
        ochisq=chisq
        do 12 j=1,ma
          atry(j)=a(j)
12      continue
      endif
      j=0
      do 14 l=1,ma
        if(ia(l).ne.0) then
          j=j+1
          k=0
          do 13 m=1,ma
            if(ia(m).ne.0) then
              k=k+1
              covar(j,k)=alpha(j,k)
            endif
13        continue
          covar(j,j)=alpha(j,j)*(1.+alamda)
          da(j)=beta(j)
        endif
14    continue
      call gaussjb(covar,mfit,nca,da,1,1)
      if(alamda.eq.0.)then
        call covsrt(covar,nca,ma,ia,mfit)
        return
      endif
      j=0
      do 15 l=1,ma
        if(ia(l).ne.0) then
          j=j+1
          atry(l)=a(l)+da(j)
        endif
15    continue
      call mrqcofb(x,y,sig,ndata,atry,ia,ma,covar,da,nca,chisq)
      if(chisq.lt.ochisq)then
        alamda=0.1*alamda
        ochisq=chisq
        j=0
        do 17 l=1,ma
          if(ia(l).ne.0) then
            j=j+1
            k=0
            do 16 m=1,ma
              if(ia(m).ne.0) then
                k=k+1
                alpha(j,k)=covar(j,k)
              endif
16          continue
            beta(j)=da(j)
            a(l)=atry(l)
          endif
17      continue
      else
        alamda=10.*alamda
        chisq=ochisq
      endif
      return
      END
      SUBROUTINE mrqcofb(x,y,sig,ndata,a,ia,ma,alpha,beta,nalp,chisq)
      INTEGER ma,nalp,ndata,ia(ma),MMAX
      REAL chisq,a(ma),alpha(nalp,nalp),beta(ma),sig(ndata),x(ndata),
     *y(ndata)
      PARAMETER (MMAX=100)
      INTEGER mfit,i,j,k,l,m
      REAL dy,sig2i,wt,ymod,dyda(MMAX)
      mfit=0
      do 11 j=1,ma
        if (ia(j).ne.0) mfit=mfit+1
11    continue
      do 13 j=1,mfit
        do 12 k=1,j
          alpha(j,k)=0.
12      continue
        beta(j)=0.
13    continue
      chisq=0.
      do 16 i=1,ndata
        call funcs(x(i),a,ymod,dyda,ma)
        sig2i=1./(sig(i)*sig(i))
        dy=y(i)-ymod
        j=0
        do 15 l=1,ma
          if(ia(l).ne.0) then
            j=j+1
            wt=dyda(l)*sig2i
            k=0
            do 14 m=1,l
              if(ia(m).ne.0) then
                k=k+1
                alpha(j,k)=alpha(j,k)+wt*dyda(m)
              endif
14          continue
            beta(j)=beta(j)+dy*wt
          endif
15      continue
        chisq=chisq+dy*dy*sig2i
16    continue
      do 18 j=2,mfit
        do 17 k=1,j-1
          alpha(k,j)=alpha(j,k)
17      continue
18    continue
      return
      END
      SUBROUTINE gaussjb(a,n,np,b,m,mp)
      INTEGER m,mp,n,np,NMAX
      REAL a(np,np),b(np,mp)
      PARAMETER (NMAX=100)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      REAL big,dum,pivinv
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
              
c                pause 'singular matrix in gaussj'
c                print *,'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
c        if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj'
c        if (a(icol,icol).eq.0.) print *,'singular matrix in gaussj'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END
      SUBROUTINE covsrt(covar,npc,ma,ia,mfit)
      INTEGER ma,mfit,npc,ia(ma)
      REAL covar(npc,npc)
      INTEGER i,j,k
      REAL swap
      do 12 i=mfit+1,ma
        do 11 j=1,i
          covar(i,j)=0.
          covar(j,i)=0.
11      continue
12    continue
      k=mfit
      do 15 j=ma,1,-1
        if(ia(j).ne.0)then
          do 13 i=1,ma
            swap=covar(i,k)
            covar(i,k)=covar(i,j)
            covar(i,j)=swap
13        continue
          do 14 i=1,ma
            swap=covar(k,i)
            covar(k,i)=covar(j,i)
            covar(j,i)=swap
14        continue
          k=k-1
        endif
15    continue
      return
      END
      FUNCTION gasdev(idum)
      INTEGER idum
      REAL gasdev
CU    USES ran1
      INTEGER iset
      REAL fac,gset,rsq,v1,v2,ran1
      SAVE iset,gset
      DATA iset/0/
      if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
      else
        gasdev=gset
        iset=0
      endif
      return
      END
      FUNCTION ran1(idum)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
      return
      END
