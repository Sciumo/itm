      
C FTANGLE v1.30, created with IBM-PC/DOS on "Thursday, June 17, 1993 at 10:46." 
C COMMAND LINE: "C:\FOR5\PMOD\XITM\WITM\FTANGLE.EXE itm"
C RUN TIME: "Friday, March 21, 1997 at 11:27."
C WEB FILE:    "itm.web"
C CHANGE FILE: (none)
      subroutine lrprop(d)
C  Version 1.2.2 (Aug 71/Mar 77/Aug 84) of the Irregular Terrain Model
C    by Longley and Rice (1968)
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      common/propa/dlsa,dx,ael,ak1,ak2,aed,emd,aes,ems,dls(2),dla,tha
      
      save wlos,wscat,dmin,xae
      logical wlos,wscat,wq
      parameter(third=1./3.)
      if(mdp.NE.0)then
      do 114 j=1,2
      dls(j)=sqrt(2.*he(j)/gme)
      
114   CONTINUE
      dlsa=dls(1)+dls(2)
      
      dla=dl(1)+dl(2)
      
      tha=max(the(1)+the(2),-dla*gme)
      
      wlos=.FALSE.
      wscat=.FALSE.
      
      if(wn.LT.0.838.OR.wn.GT.210.)kwx=max(kwx,1)
      do 115 j=1,2
      if(hg(j).LT.1..OR.hg(j).GT.1000.)kwx=max(kwx,1)
115   CONTINUE
      do 116 j=1,2
      if(abs(the(j)).GT.200e-3.OR.dl(j).LT.0.1*dls(j).OR.dl(j).GT.3.*dls
     &(j))kwx=max(kwx,3)
116   CONTINUE
      if(ens.LT.250..OR.ens.GT.400..OR.gme.LT.75e-9.OR.gme.GT.250e-9.OR.
     &Real(zgnd).LE.abs(Imag(zgnd)).OR.wn.LT.0.419.OR.wn.GT.420.)kwx=4
      do 117 j=1,2
      if(hg(j).LT.0.5.OR.hg(j).GT.3000.)kwx=4
117   CONTINUE
      dmin=abs(he(1)-he(2))/200e-3
      
      q=adiff(0.)
      xae=(wn*gme**2)**(-third)
      
      d3=max(dlsa,1.3787*xae+dla)
      
      d4=d3+2.7574*xae
      
      a3=adiff(d3)
      
      a4=adiff(d4)
      
      emd=(a4-a3)/(d4-d3)
      
      aed=a3-emd*d3
      
      
      endif
      if(mdp.GE.0)then
      mdp=0
      dist=d
      endif
      if(dist.GT.0.)then
      if(dist.GT.1000e3)kwx=max(kwx,1)
      if(dist.LT.dmin)kwx=max(kwx,3)
      if(dist.LT.1e3.OR.dist.GT.2000e3)kwx=4
      
      endif
      if(dist.LT.dlsa)then
      if(.NOT.wlos)then
      q=alos(0.)
      d2=dlsa
      a2=aed+d2*emd
      d0=1.908*wn*he(1)*he(2)
      
      if(aed.GE.0.)then
      d0=min(d0,0.5*dla)
      
      d1=d0+0.25*(dla-d0)
      
      else
      d1=max(-aed/emd,0.25*dla)
      
      endif
      a1=alos(d1)
      
      wq=.FALSE.
      if(d0.LT.d1)then
      a0=alos(d0)
      
      q=log(d2/d0)
      ak2=max(0.,((d2-d0)*(a1-a0)-(d1-d0)*(a2-a0))/((d2-d0)*log(d1/d0)-(
     &d1-d0)*q))
      
      wq=aed.GT.0..OR.ak2.GT.0.
      if(wq)then
      ak1=(a2-a0-ak2*q)/(d2-d0)
      
      if(ak1.LT.0.)then
      ak1=0.
      
      ak2=dim(a2,a0)/q
      
      if(ak2.EQ.0.)ak1=emd
      
      endif
      endif
      endif
      if(.NOT.wq)then
      ak1=dim(a2,a1)/(d2-d1)
      
      ak2=0.
      
      if(ak1.EQ.0.)ak1=emd
      
      endif
      ael=a2-ak1*d2-ak2*log(d2)
      
      
      wlos=.TRUE.
      endif
      if(dist.GT.0.)aref=ael+ak1*dist+ak2*log(dist)
      
      
      endif
      if(dist.LE.0..OR.dist.GE.dlsa)then
      if(.NOT.wscat)then
      q=ascat(0.)
      d5=dla+200e3
      
      d6=d5+200e3
      
      a6=ascat(d6)
      
      a5=ascat(d5)
      
      if(a5.LT.1000.)then
      ems=(a6-a5)/200e3
      
      dx=max(dlsa,dla+0.3*xae*log(47.7*wn),(a5-aed-ems*d5)/(emd-ems))
      
      aes=(emd-ems)*dx+aed
      
      else
      ems=emd
      aes=aed
      dx=10e6
      
      endif
      
      wscat=.TRUE.
      endif
      if(dist.GT.dx)then
      aref=aes+ems*dist
      else
      aref=aed+emd*dist
      
      endif
      
      endif
      
      aref=max(aref,0.)
      return
      end
      function adiff(d)
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      common/propa/dlsa,dx,ael,ak1,ak2,aed,emd,aes,ems,dls(2),dla,tha
      
      save wd1,xd1,afo,qk,aht,xht
      parameter(third=1./3.)
      if(d.EQ.0.)then
      q=hg(1)*hg(2)
      qk=he(1)*he(2)-q
      if(mdp.LT.0)q=q+10.
      wd1=sqrt(1.+qk/q)
      xd1=dla+tha/gme
      
      q=(1.-0.8*exp(-dlsa/50e3))*dh
      q=0.78*q*exp(-(q/16.)**0.25)
      
      afo=min(15.,2.171*log(1.+4.77e-4*hg(1)*hg(2)*wn*q))
      
      qk=1./cabs(zgnd)
      aht=20.
      
      xht=0.
      do 118 j=1,2
      a=0.5*dl(j)**2/he(j)
      
      wa=(a*wn)**third
      
      pk=qk/wa
      
      q=(1.607-pk)*151.0*wa*dl(j)/a
      
      xht=xht+q
      
      aht=aht+fht(q,pk)
      
118   CONTINUE
      adiff=0.
      
      else
      th=tha+d*gme
      
      ds=d-dla
      q=0.0795775*wn*ds*th**2
      adiff=aknfe(q*dl(1)/(ds+dl(1)))+aknfe(q*dl(2)/(ds+dl(2)))
      
      a=ds/th
      wa=(a*wn)**third
      
      pk=qk/wa
      
      q=(1.607-pk)*151.0*wa*th+xht
      
      ar=0.05751*q-4.343*log(q)-aht
      
      q=(wd1+xd1/d)*min(((1.-0.8*exp(-d/50e3))*dh*wn),6283.2)
      wd=25.1/(25.1+sqrt(q))
      
      adiff=ar*wd+(1.-wd)*adiff+afo
      
      
      endif
      return
      end
      function aknfe(v2)
      if(v2.LT.5.76)then
      aknfe=6.02+9.11*sqrt(v2)-1.27*v2
      else
      aknfe=12.953+4.343*log(v2)
      endif
      return
      end
      function fht(x,pk)
      if(x.LT.200.)then
      w=-log(pk)
      if(pk.LT.1e-5.OR.x*w**3.GT.5495.)then
      fht=-117.
      if(x.GT.1.)fht=17.372*log(x)+fht
      
      else
      fht=2.5e-5*x**2/pk-8.686*w-15.
      
      endif
      else
      fht=0.05751*x-4.343*log(x)
      
      if(x.LT.2000)then
      w=0.0134*x*exp(-0.005*x)
      fht=(1.-w)*fht+w*(17.372*log(x)-117.)
      
      endif
      endif
      return
      end
      function alos(d)
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      common/propa/dlsa,dx,ael,ak1,ak2,aed,emd,aes,ems,dls(2),dla,tha
      
      save wls
      complex r
      abq(r)=Real(r)**2+Imag(r)**2
      if(d.EQ.0.)then
      wls=0.021/(0.021+wn*dh/max(10e3,dlsa))
      
      alos=0.
      
      else
      q=(1.-0.8*exp(-d/50e3))*dh
      
      s=0.78*q*exp(-(q/16.)**0.25)
      
      q=he(1)+he(2)
      sps=q/sqrt(d**2+q**2)
      
      r=(sps-zgnd)/(sps+zgnd)*exp(-min(10.,wn*s*sps))
      
      q=abq(r)
      if(q.LT.0.25.OR.q.LT.sps)r=r*sqrt(sps/q)
      
      alos=emd*d+aed
      
      q=wn*he(1)*he(2)*2./d
      
      if(q.GT.1.57)q=3.14-2.4649/q
      
      alos=(-4.343*log(abq(cmplx(cos(q),-sin(q))+r))-alos)*wls+alos
      
      
      endif
      return
      end
      function ascat(d)
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      common/propa/dlsa,dx,ael,ak1,ak2,aed,emd,aes,ems,dls(2),dla,tha
      
      save ad,rr,etq,h0s
      if(d.EQ.0.)then
      ad=dl(1)-dl(2)
      rr=he(2)/he(1)
      if(ad.LT.0.)then
      ad=-ad
      rr=1./rr
      endif
      etq=(5.67e-6*ens-2.32e-3)*ens+0.031
      
      h0s=-15.
      ascat=0.
      
      else
      if(h0s.GT.15.)then
      h0=h0s
      else
      th=the(1)+the(2)+d*gme
      
      r2=2.*wn*th
      r1=r2*he(1)
      r2=r2*he(2)
      
      if(r1.LT.0.2.AND.r2.LT.0.2)then
      ascat=1001.
      
      go to 101
      endif
      ss=(d-ad)/(d+ad)
      
      q=rr/ss
      ss=max(0.1,ss)
      q=min(max(0.1,q),10.)
      z0=(d-ad)*(d+ad)*th*0.25/d
      
      et=(etq*exp(-min(1.7,z0/8.0e3)**6)+1.)*z0/1.7556e3
      
      ett=max(et,1.)
      h0=(h0f(r1,ett)+h0f(r2,ett))*0.5
      
      h0=h0+min(h0,(1.38-log(ett))*log(ss)*log(q)*0.49)
      
      h0=dim(h0,0.)
      if(et.LT.1.)h0=et*h0+(1.-et)*4.343*log(((1.+1.4142/r1)*(1.+1.4142/
     &r2))**2*(r1+r2)/(r1+r2+2.8284))
      
      if(h0.GT.15..AND.h0s.GE.0.)h0=h0s
      endif
      h0s=h0
      th=tha+d*gme
      
      ascat=ahd(th*d)+4.343*log(47.7*wn*th**4)-0.1*(ens-301.)*exp(-th*d/
     &40e3)+h0
      
101   continue
      
      endif
      return
      end
      function h0f(r,et)
      dimension a(5),b(5)
      data a(1),a(2),a(3),a(4),a(5)/25.,80.,177.,395.,705./
      data b(1),b(2),b(3),b(4),b(5)/24.,45.,68.,80.,105./
      it=et
      if(it.LE.0)then
      it=1
      q=0.
      else if(it.GE.5)then
      it=5
      q=0.
      else
      q=et-it
      endif
      x=(1./r)**2
      h0f=4.343*log((a(it)*x+b(it))*x+1.)
      
      if(q.NE.0.)h0f=(1.-q)*h0f+q*4.343*log((a(it+1)*x+b(it+1))*x+1.)
      return
      end
      function ahd(td)
      dimension a(3),b(3),c(3)
      data a(1),a(2),a(3)/133.4,104.6,71.8/
      data b(1),b(2),b(3)/0.332e-3,0.212e-3,0.157e-3/
      data c(1),c(2),c(3)/-4.343,-1.086,2.171/
      if(td.LE.10e3)then
      i=1
      else if(td.LE.70e3)then
      i=2
      else
      i=3
      endif
      ahd=a(i)+b(i)*td+c(i)*log(td)
      
      return
      end
      function avar(zzt,zzl,zzc)
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      common/propv/lvar,sgc,mdvar,klim
      
      save kdv,wl,ws,dexa,de,vmd,vs0,sgl,sgtm,sgtp,sgtd,tgtd,gm,gp,cv1,c
     &v2,yv1,yv2,yv3,csm1,csm2,ysm1,ysm2,ysm3,csp1,csp2,ysp1,ysp2,ysp3,c
     &sd1,zd,cfm1,cfm2,cfm3,cfp1,cfp2,cfp3
      dimension bv1(7),bv2(7),xv1(7),xv2(7),xv3(7)
      dimension bsm1(7),bsm2(7),xsm1(7),xsm2(7),xsm3(7)
      dimension bsp1(7),bsp2(7),xsp1(7),xsp2(7),xsp3(7)
      dimension bsd1(7),bzd1(7)
      dimension bfm1(7),bfm2(7),bfm3(7),bfp1(7),bfp2(7),bfp3(7)
      logical ws,wl
      parameter(third=1./3.)
      
      
      data bv1/-9.67,-0.62,1.26,-9.21,-0.62,-0.39,3.15/
      data bv2/12.7,9.19,15.5,9.05,9.19,2.86,857.9/
      data xv1/144.9e3,228.9e3,262.6e3,84.1e3,228.9e3,141.7e3,2222.e3/
      data xv2/190.3e3,205.2e3,185.2e3,101.1e3,205.2e3,315.9e3,164.8e3/
      data xv3/133.8e3,143.6e3,99.8e3,98.6e3,143.6e3,167.4e3,116.3e3/
      data bsm1/2.13,2.66,6.11,1.98,2.68,6.86,8.51/
      data bsm2/159.5,7.67,6.65,13.11,7.16,10.38,169.8/
      data xsm1/762.2e3,100.4e3,138.2e3,139.1e3,93.7e3,187.8e3,609.8e3/
      data xsm2/123.6e3,172.5e3,242.2e3,132.7e3,186.8e3,169.6e3,119.9e3/
      data xsm3/94.5e3,136.4e3,178.6e3,193.5e3,133.5e3,108.9e3,106.6e3/
      data bsp1/2.11,6.87,10.08,3.68,4.75,8.58,8.43/
      data bsp2/102.3,15.53,9.60,159.3,8.12,13.97,8.19/
      data xsp1/636.9e3,138.7e3,165.3e3,464.4e3,93.2e3,216.0e3,136.2e3/
      data xsp2/134.8e3,143.7e3,225.7e3,93.1e3,135.9e3,152.0e3,188.5e3/
      data xsp3/95.6e3,98.6e3,129.7e3,94.2e3,113.4e3,122.7e3,122.9e3/
      data bsd1/1.224,0.801,1.380,1.000,1.224,1.518,1.518/
      data bzd1/1.282,2.161,1.282,20.,1.282,1.282,1.282/
      data bfm1/1.,1.,1.,1.,0.92,1.,1./
      data bfm2/0.,0.,0.,0.,0.25,0.,0./
      data bfm3/0.,0.,0.,0.,1.77,0.,0./
      data bfp1/1.,0.93,1.,0.93,0.93,1.,1./
      data bfp2/0.,0.31,0.,0.19,0.31,0.,0./
      data bfp3/0.,2.00,0.,1.79,2.00,0.,0./
      
      data rt,rl/7.8,24./
      
      
      curv(c1,c2,x1,x2,x3)=(c1+c2/(1.+((de-x2)/x3)**2))*((de/x1)**2)/(1.
     &+((de/x1)**2))
      
      
      if(lvar.GT.0)then
      if(lvar.LT.5)go to(106,105,104,103),lvar
102   continue
      if(klim.LE.0.OR.klim.GT.7)then
      klim=5
      kwx=max(kwx,2)
      endif
      cv1=bv1(klim)
      cv2=bv2(klim)
      yv1=xv1(klim)
      yv2=xv2(klim)
      yv3=xv3(klim)
      csm1=bsm1(klim)
      csm2=bsm2(klim)
      ysm1=xsm1(klim)
      ysm2=xsm2(klim)
      ysm3=xsm3(klim)
      csp1=bsp1(klim)
      csp2=bsp2(klim)
      ysp1=xsp1(klim)
      ysp2=xsp2(klim)
      ysp3=xsp3(klim)
      csd1=bsd1(klim)
      zd=bzd1(klim)
      cfm1=bfm1(klim)
      cfm2=bfm2(klim)
      cfm3=bfm3(klim)
      cfp1=bfp1(klim)
      cfp2=bfp2(klim)
      cfp3=bfp3(klim)
      
103   continue
      kdv=mdvar
      ws=kdv.GE.20
      if(ws)kdv=kdv-20
      wl=kdv.GE.10
      if(wl)kdv=kdv-10
      if(kdv.LT.0.OR.kdv.GT.3)then
      kdv=0
      kwx=max(kwx,2)
      endif
      
104   continue
      q=log(0.133*wn)
      gm=cfm1+cfm2/((cfm3*q)**2+1.)
      gp=cfp1+cfp2/((cfp3*q)**2+1.)
      
105   continue
      dexa=sqrt(18e6*he(1))+sqrt(18e6*he(2))+(575.7e12/wn)**third
      
      
106   continue
      if(dist.LT.dexa)then
      de=130e3*dist/dexa
      else
      de=130e3+dist-dexa
      
      endif
      vmd=curv(cv1,cv2,yv1,yv2,yv3)
      
      sgtm=curv(csm1,csm2,ysm1,ysm2,ysm3)*gm
      sgtp=curv(csp1,csp2,ysp1,ysp2,ysp3)*gp
      
      sgtd=sgtp*csd1
      
      tgtd=(sgtp-sgtd)*zd
      if(wl)then
      sgl=0.
      else
      q=(1.-0.8*exp(-dist/50e3))*dh*wn
      sgl=10.*q/(q+13.)
      
      endif
      if(ws)then
      vs0=0.
      else
      vs0=(5.+3.*exp(-de/100e3))**2
      
      endif
      
      
      lvar=0
      endif
      zt=zzt
      zl=zzl
      zc=zzc
      if(kdv.EQ.0)then
      zt=zc
      zl=zc
      else if(kdv.EQ.1)then
      zl=zc
      else if(kdv.EQ.2)then
      zl=zt
      endif
      if(abs(zt).GT.3.10.OR.abs(zl).GT.3.10.OR.abs(zc).GT.3.10)kwx=max(k
     &wx,1)
      
      if(zt.LT.0.)then
      sgt=sgtm
      else if(zt.LE.zd)then
      sgt=sgtp
      else
      sgt=sgtd+tgtd/zt
      
      endif
      vs=vs0+(sgt*zt)**2/(rt+zc**2)+(sgl*zl)**2/(rl+zc**2)
      
      
      if(kdv.EQ.0)then
      yr=0.
      sgc=sqrt(sgt**2+sgl**2+vs)
      else if(kdv.EQ.1)then
      yr=sgt*zt
      sgc=sqrt(sgl**2+vs)
      else if(kdv.EQ.2)then
      yr=sqrt(sgt**2+sgl**2)*zt
      sgc=sqrt(vs)
      else
      yr=sgt*zt+sgl*zl
      sgc=sqrt(vs)
      endif
      
      avar=aref-vmd-yr-sgc*zc
      
      if(avar.LT.0.)avar=avar*(29.-avar)/(29.-10.*avar)
      
      return
      end
      subroutine qlrps(fmhz,zsys,en0,ipol,eps,sgm)
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      complex zq
      
      data gma/157e-9/
      
      wn=fmhz/47.7
      
      ens=en0
      if(zsys.NE.0.)ens=ens*exp(-zsys/9460.)
      
      gme=gma*(1.-0.04665*exp(ens/179.3))
      
      zq=cmplx(eps,376.62*sgm/wn)
      
      zgnd=csqrt(zq-1.)
      if(ipol.NE.0)zgnd=zgnd/zq
      
      return
      end
      subroutine qlra(kst,klimx,mdvarx)
      dimension kst(2)
      
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      common/propv/lvar,sgc,mdvar,klim
      
      
      do 119 j=1,2
      if(kst(j).LE.0)then
      he(j)=hg(j)
      else
      q=4.
      if(kst(j).NE.1)q=9.
      if(hg(j).LT.5.)q=q*sin(0.3141593*hg(j))
      he(j)=hg(j)+(1.+q)*exp(-min(20.,2.*hg(j)/max(1e-3,dh)))
      endif
      q=sqrt(2.*he(j)/gme)
      dl(j)=q*exp(-0.07*sqrt(dh/max(he(j),5.)))
      the(j)=(0.65*dh*(q/dl(j)-1.)-2.*he(j))/q
119   CONTINUE
      
      mdp=1
      lvar=max(lvar,3)
      if(mdvarx.GE.0)then
      mdvar=mdvarx
      lvar=max(lvar,4)
      endif
      if(klimx.GT.0)then
      klim=klimx
      lvar=5
      endif
      return
      end
      subroutine qlrpfl(pfl,klimx,mdvarx)
      dimension pfl(*)
      
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      common/propv/lvar,sgc,mdvar,klim
      
      
      dimension xl(2)
      
      dist=pfl(1)*pfl(2)
      np=pfl(1)
      call hzns(pfl)
      do 120 j=1,2
      xl(j)=min(15.*hg(j),0.1*dl(j))
120   CONTINUE
      xl(2)=dist-xl(2)
      dh=dlthx(pfl,xl(1),xl(2))
      
      if(dl(1)+dl(2).GT.1.5*dist)then
      call zlsq1(pfl,xl(1),xl(2),za,zb)
      he(1)=hg(1)+dim(pfl(3),za)
      he(2)=hg(2)+dim(pfl(np+3),zb)
      do 121 j=1,2
      dl(j)=sqrt(2.*he(j)/gme)*exp(-0.07*sqrt(dh/max(he(j),5.)))
121   CONTINUE
      q=dl(1)+dl(2)
      if(q.LE.dist)then
      q=(dist/q)**2
      do 122 j=1,2
      he(j)=he(j)*q
      dl(j)=sqrt(2.*he(j)/gme)*exp(-0.07*sqrt(dh/max(he(j),5.)))
122   CONTINUE
      endif
      do 123 j=1,2
      q=sqrt(2.*he(j)/gme)
      the(j)=(0.65*dh*(q/dl(j)-1.)-2.*he(j))/q
123   CONTINUE
      
      else
      call zlsq1(pfl,xl(1),0.9*dl(1),za,q)
      call zlsq1(pfl,dist-0.9*dl(2),xl(2),q,zb)
      he(1)=hg(1)+dim(pfl(3),za)
      he(2)=hg(2)+dim(pfl(np+3),zb)
      
      endif
      
      mdp=-1
      lvar=max(lvar,3)
      if(mdvarx.GE.0)then
      mdvar=mdvarx
      lvar=max(lvar,4)
      endif
      if(klimx.GT.0)then
      klim=klimx
      lvar=5
      endif
      
      call lrprop(0.)
      
      return
      end
      subroutine hzns(pfl)
      dimension pfl(*)
      
      common/prop/kwx,aref,mdp,dist,hg(2),wn,dh,ens,gme,zgnd,he(2),dl(2)
     &,the(2)
      complex zgnd
      
      
      logical wq
      
      np=pfl(1)
      xi=pfl(2)
      za=pfl(3)+hg(1)
      zb=pfl(np+3)+hg(2)
      qc=0.5*gme
      q=qc*dist
      the(2)=(zb-za)/dist
      the(1)=the(2)-q
      the(2)=-the(2)-q
      dl(1)=dist
      dl(2)=dist
      if(np.LT.2)go to 107
      sa=0.
      sb=dist
      wq=.TRUE.
      do 124 i=2,np
      sa=sa+xi
      sb=sb-xi
      q=pfl(i+2)-(qc*sa+the(1))*sa-za
      if(q.GT.0.)then
      the(1)=the(1)+q/sa
      dl(1)=sa
      wq=.FALSE.
      endif
      if(.NOT.wq)then
      q=pfl(i+2)-(qc*sb+the(2))*sb-zb
      if(q.GT.0.)then
      the(2)=the(2)+q/sb
      dl(2)=sb
      endif
      endif
124   CONTINUE
      
107   return
      end
      function dlthx(pfl,x1,x2)
      dimension pfl(*)
      
      dimension s(247)
      
      np=pfl(1)
      xa=x1/pfl(2)
      xb=x2/pfl(2)
      dlthx=0.
      if(xb-xa.LT.2.)go to 108
      ka=0.1*(xb-xa+8.)
      ka=min0(max0(4,ka),25)
      n=10*ka-5
      kb=n-ka+1
      sn=n-1
      s(1)=sn
      s(2)=1.
      xb=(xb-xa)/sn
      k=xa+1.
      xa=xa-float(k)
      do 125 j=1,n
109   if(xa.GT.0..AND.k.LT.np)then
      xa=xa-1.
      k=k+1
      go to 109
      endif
      s(j+2)=pfl(k+3)+(pfl(k+3)-pfl(k+2))*xa
      xa=xa+xb
125   CONTINUE
      call zlsq1(s,0.,sn,xa,xb)
      xb=(xb-xa)/sn
      do 126 j=1,n
      s(j+2)=s(j+2)-xa
      xa=xa+xb
126   CONTINUE
      
      dlthx=qtile(n,s(3),ka)-qtile(n,s(3),kb)
      dlthx=dlthx/(1.-0.8*exp(-(x2-x1)/50e3))
      
108   return
      end
      function qerf(z)
      
      data b1,b2,b3,b4,b5/0.319381530,-0.356563782,1.781477937,-1.821255
     &987,1.330274429/
      data rp,rrt2pi/4.317008,0.398942280/
      
      x=z
      t=abs(x)
      if(t.LT.10.)go to 1
      qerf=0.
      go to 2
1     t=rp/(t+rp)
      qerf=exp(-0.5*x**2)*rrt2pi*((((b5*t+b4)*t+b3)*t+b2)*t+b1)*t
2     if(x.LT.0.)qerf=1.-qerf
      return
      end
      function qerfi(q)
      
      data c0,c1,c2/2.515516698,0.802853,0.010328/
      data d1,d2,d3/1.432788,0.189269,0.001308/
      
      x=0.5-q
      t=amax1(0.5-abs(x),0.000001)
      t=sqrt(-2.*alog(t))
      qerfi=t-((c2*t+c1)*t+c0)/(((d3*t+d2)*t+d1)*t+1.)
      if(x.LT.0.)qerfi=-qerfi
      return
      end
      function qtile(nn,a,ir)
      dimension a(nn)
      
      m=1
      n=nn
      k=min(max(1,ir),n)
110   continue
      q=a(k)
      i0=m
      j1=n
111   continue
      do 127 i=i0,n
      if(a(i).LT.q)go to 112
127   CONTINUE
      i=n
112   do 128 j=j1,m,-1
      if(a(j).GT.q)go to 113
128   CONTINUE
      j=m
113   if(i.LT.j)then
      r=a(i)
      a(i)=a(j)
      a(j)=r
      i0=i+1
      j1=j-1
      go to 111
      else if(i.LT.k)then
      a(k)=a(i)
      a(i)=q
      m=i+1
      go to 110
      else if(j.GT.k)then
      a(k)=a(j)
      a(j)=q
      n=j-1
      go to 110
      endif
      qtile=q
      return
      end
      subroutine zlsq1(z,x1,x2,z0,zn)
      dimension z(*)
      
      xn=z(1)
      xa=aint(dim(x1/z(2),0.))
      xb=xn-aint(dim(xn,x2/z(2)))
      if(xb.LE.xa)then
      xa=dim(xa,1.)
      xb=xn-dim(xn,xb+1.)
      endif
      ja=xa
      jb=xb
      n=jb-ja
      xa=xb-xa
      x=-0.5*xa
      xb=xb+x
      a=0.5*(z(ja+3)+z(jb+3))
      b=0.5*(z(ja+3)-z(jb+3))*x
      do 129 i=2,n
      ja=ja+1
      x=x+1.
      a=a+z(ja+3)
      b=b+z(ja+3)*x
129   CONTINUE
      a=a/xa
      b=b*12./((xa*xa+2.)*xa)
      z0=a-b*xb
      zn=a+b*(xn-xb)
      return
      end
      
