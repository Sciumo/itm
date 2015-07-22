      PROGRAM UAREA
C       A User's access to the ITM in the area prediction mode
C          Programming error here corrected 3/17/98
C
      CHARACTER KPSYS*6,KPENV*5,KPSTC*7,KPTAB*5
      CHARACTER LPSYS(6)*8,LPENV(5)*8,LPSTC(7)*8,LPTAB(5)*8
      DIMENSION IPSYS(6)
        EQUIVALENCE(SH1,IPSYS(1)),(SH2,IPSYS(2)),(FMHZ,IPSYS(3)),
     X    (KPOL,IPSYS(4)),(KST1,IPSYS(5)),(KST2,IPSYS(6))
      DIMENSION IPENV(5)
        EQUIVALENCE(SDH,IPENV(1)),(EN0,IPENV(2)),(KLMM,IPENV(3)),
     X    (EPS,IPENV(4)),(SGM,IPENV(5))
      DIMENSION IPSTC(7),QC(4)
        EQUIVALENCE(KVAR,IPSTC(1)),(QT,IPSTC(2)),(QL,IPSTC(3)),
     X    (QC,IPSTC(4))
      DIMENSION IPTAB(5)
        EQUIVALENCE(D0,IPTAB(1)),(D1,IPTAB(2)),(DS1,IPTAB(3)),
     X    (D2,IPTAB(4)),(DS2,IPTAB(5))
C
      COMMON/PROP/KWX,AREF,MDP,DIST,HG(2),WN,DH,ENS,GME,ZGND,
     X  HE(2),DL(2),THE(2) 
         COMPLEX ZGND 
      COMMON/PROPV/LVAR,SGC,MDVAR,KLIM
C
      CHARACTER LQ*1,TITLE*72,FNAME*40,FNQ*40
      DIMENSION ZC(4),XLB(4)
      DIMENSION KST(2)
C
      PARAMETER(GMA=157E-9)
      PARAMETER(DB=8.685890)
      PARAMETER(AKM=1000.)
C
      DATA LPSYS/' H1, m',' H2, m',' F, MHz','  Pol',' Sit1',' Sit2'/
        DATA SH1,SH2,FMHZ,KPOL,KST1,KST2/3.,3.,100.,86,82,82/
        DATA KPSYS/'111AAA'/
      DATA LPENV/' DH, m','  Ns',' Clim','  eps','  sgm'/
        DATA SDH,EN0,KLMM,EPS,SGM/90.,301.,67,15.,.005/
        DATA KPENV/'00A03'/
      DATA LPSTC/'  Var',' QT, %',' QL, %',
     X           '  QC1','  QC2','  QC3','  QC4'/
        DATA KVAR,QT,QL,QC/66,50.,50.,50.,90.,10.,0./
        DATA KPSTC/'A111111'/
      DATA LPTAB/' d0, km','  d1','  ds1','  d2','  ds2'/
        DATA D0,D1,DS1,D2,DS2/10.,150.,10.,500.,50./
        DATA KPTAB/'00000'/
C
      DATA TITLE/' '/,FNAME/'ITM.DAT'/,NNAME/7/,JFILE/1/
C
2000  FORMAT(A)
2002  FORMAT(2X,A,' ',$)
2102  FORMAT(8X,'At ',A6,',  F(50,',A2,') =',F6.1,' dBu')
C
      WRITE(*,2000) ' ',' '
      WRITE(*,2000) '     The Irregular Terrain Model'//
     X              ' of Radio Propagation'
      WRITE(*,2000) '     in the Area Prediction mode'
      WRITE(*,2000) ' '
      WRITE(*,2000) '     Input:'
10    CONTINUE
      WRITE(*,2000) '     System parameters--'
      WRITE(*,2000) '       Pol=H|V, Sit=R|C|V'
      CALL VCARD(6,KPSYS,LPSYS,IPSYS,*80)
      WRITE(*,2000) '     Environment parameters--'
      WRITE(*,2000) '       Clim=C|M|S|1|2...|7'
      CALL VCARD(5,KPENV,LPENV,IPENV,*80)
      WRITE(*,2000) '     Statistical parameters--'
      WRITE(*,2000) '       Var=S|A|M|B'
      CALL VCARD(7,KPSTC,LPSTC,IPSTC,*80)
      WRITE(*,2000) '     Tabulation parameters--'
      CALL VCARD(5,KPTAB,LPTAB,IPTAB,*80)
C
      WRITE(*,2002) '...needs editing (N)?'
      READ(*,2000) LQ
      IF(LQ .EQ. 'Y' .OR. LQ .EQ. 'y') GO TO 10
      WRITE(*,2002) '   A title?'
      READ(*,2000) TITLE(4:)
11    WRITE(*,2002) '   and the output file: '//FNAME(:NNAME)//'?'
      READ(*,2000) FNQ
      IF(FNQ .NE. ' ') THEN
        JFILE=1
        FNAME=' '
        NNAME=0
        DO I=1,40
          LQ=FNQ(I:I)
          IF(LQ .NE. ' ') THEN
            NNAME=NNAME+1
            IF(LQ .GE. 'a' .AND. LQ .LE. 'z') LQ=CHAR(ICHAR(LQ)-32)
            FNAME(NNAME:NNAME)=LQ
          ENDIF
        ENDDO
      ENDIF
      IF(JFILE .NE. 0) THEN
        OPEN(3,FILE=FNAME,STATUS='NEW',ERR=111)
        GO TO 112
111       WRITE(*,2002) ' ! '//FNAME(:NNAME)//' exists.  Overwrite?'
          READ(*,2000) LQ
          IF(LQ .NE. 'Y' .AND. LQ .NE. 'y') GO TO 11
          OPEN(3,FILE=FNAME,STATUS='UNKNOWN')
112     CONTINUE
        WRITE(3,2000) ' '
        JFILE=0
      ENDIF
      IF(QT .LE. 0.) QT=50.
      IF(QL .LE. 0.) QL=50.
      IF(QC(1) .LE. 0.) THEN
        QC(1)=50.
        QC(2)=0.
      ENDIF
      ZT=QERFI(QT/100.)
      ZL=QERFI(QL/100.)
      NC=0
      DO JC=1,4
        IF(QC(JC) .LE. 0.) GO TO 121
        NC=JC
        ZC(JC)=QERFI(QC(JC)/100.)
      ENDDO
121   CONTINUE
      IF(D0 .LE. 0.) D0=DS1
      IF(D0 .LE. 0.) D0=2.
      IF(D1 .LE. D0 .OR. DS1 .LE. 0.) THEN
        ND=1
        D1=D0
      ELSE
        DS=DS1
        ND=(D1-D0)/DS+1.75
        D1=D0+(ND-1)*DS
      ENDIF
      IF(D2 .LE. D1 .OR. DS2 .LE. 0.) THEN
        NDC=0
      ELSE
        NDC=ND
        ND=(D2-D1)/DS2+0.75
        D2=D1+ND*DS2
        ND=NDC+ND
      ENDIF
      IPOL=MAX(INDEX("HV01",CHAR(KPOL))-1,0)
      IF(IPOL .GT. 1) IPOL=IPOL-2
      JQ=MAX(INDEX("RCV012",CHAR(KST1))-1,0)
      IF(JQ .GT. 2) JQ=JQ-3
      KST(1)=JQ
      JQ=MAX(INDEX("RCV012",CHAR(KST2))-1,0)
      IF(JQ .GT. 2) JQ=JQ-3
      KST(2)=JQ
      KLIM=MAX(INDEX("CMS1234567",CHAR(KLMM))-1,0)+5
      IF(KLIM .GT. 7) KLIM=KLIM-7
      IVAR=MAX(INDEX("SAMB0123",CHAR(KVAR))-1,0)
      IF(IVAR .GT. 3) IVAR=IVAR-4
      WRITE(3,2000) TITLE,' '
      WRITE(*,2000) ' ',' ',' ',' ',' *******************************'
      WRITE(*,2000) ' ',TITLE,' '
      KWX=0
      HG(1)=SH1
      HG(2)=SH2
      DH=SDH
      CALL QLRPS(FMHZ,0.,EN0,IPOL,EPS,SGM)
      CALL QLRA(KST,KLIM,IVAR)
      WRITE(3,2015) FMHZ
      WRITE(3,2016) HG
      WRITE(3,2017) HE,KST
      WRITE(3,2018) DH
      WRITE(*,2015) FMHZ
      WRITE(*,2016) HG
      WRITE(*,2017) HE,KST
      WRITE(*,2018) DH
2015  FORMAT(12X,'Frequency',F12.0,' MHz')
2016  FORMAT(6X,'Antenna heights',2F8.1,' m')
2017  FORMAT(4X,'Effective heights',2F8.1,' m (siting=',I1,',',I1,')')
2018  FORMAT(5X,'Terrain, Delta H',F12.0,' m')
      Q=GMA/GME
      WRITE(3,2021) IPOL,EPS,SGM
      WRITE(3,2022) KLIM,ENS,Q
      WRITE(3,2000) ' '
      WRITE(*,2021) IPOL,EPS,SGM
      WRITE(*,2022) KLIM,ENS,Q
      WRITE(*,2000) ' '
2021  FORMAT(3X,'Pol=',I1,', eps=',F3.0,', sgm=',F6.3,' S/m')
2022  FORMAT(3X,'Clim=',I1,', Ns=',F5.0,'N-units, K=',F6.3)
      IF(IVAR .EQ. 0) THEN
        WRITE(3,2030)
        WRITE(*,2030)
      ELSE IF(IVAR .EQ. 1) THEN
        WRITE(3,2031) QT
        WRITE(*,2031) QT
      ELSE IF(IVAR .EQ. 2) THEN
        WRITE(3,2032) QT
        WRITE(*,2032) QT
      ELSE IF(IVAR .EQ. 3) THEN
        WRITE(3,2033) QT,QL
        WRITE(*,2033) QT,QL
      ENDIF
2030  FORMAT(3X,'Single-message service')
2031  FORMAT(3X,'Accidental service:  ',F5.1,'% time availability')
2032  FORMAT(3X,'Mobile service: required reliability-',F5.1,'%')
2033  FORMAT(3X,'Broadcast service: required reliability-',
     X            F5.1,'% time,',F5.1,'% locations')
      WRITE(3,2000) ' '
      WRITE(3,2040)
      WRITE(3,2000) ' '
      WRITE(3,2041) (QC(JC),JC=1,NC)
      WRITE(3,2000) ' '
      WRITE(*,2000) ' '
      WRITE(*,2040)
      WRITE(*,2000) ' '
      WRITE(*,2041) (QC(JC),JC=1,NC)
      WRITE(*,2000) ' '
2040  FORMAT(3X,'Estimated quantiles of basic transmission loss (dB)')
2041  FORMAT(7X,'Dist',4X,'Free',6X,'with confidence'/
     X            8X,'km',5X,'space ',4F8.1)
2045  FORMAT(2X,3F9.1,3F8.1)
      D=D0
      DT=DS
      DO JD=1,ND
        LVAR=MAX(1,LVAR)
        CALL LRPROP(D*AKM)
        FS=DB*LOG(2.*WN*DIST)
        DO JC=1,NC
          XLB(JC)=FS+AVAR(ZT,ZL,ZC(JC))
        ENDDO
        WRITE(3,2045) D,FS,(XLB(JC),JC=1,NC)
        IF(JD .EQ. 1) WRITE(*,2045) D,FS,(XLB(JC),JC=1,NC)
        IF(JD .EQ. NDC) DT=DS2
        D=D+DT
      ENDDO
      WRITE(3,2000) ' '
      WRITE(*,2000) ' '
      IF(KWX .NE. 0) THEN
        IF(KWX .EQ. 1) THEN
          WRITE(3,2081)
          WRITE(*,2081)
2081    FORMAT(
     .    3X,'!Warning! Some parameters are nearly out of range...'/
     .    3X,'   Results should be used with caution')
        ELSE IF(KWX .EQ. 2) THEN
          WRITE(3,2082)
          WRITE(*,2082)
2082    FORMAT(
     .    3X,'!Note- Default parameters have been substituted'/
     .    3X,'   for impossible ones.')
        ELSE IF(KWX .EQ. 3) THEN
          WRITE(3,2083)
          WRITE(*,2083)
2083    FORMAT(
     .    3X,'!Warning! A combination of parameters is out of range.'/
     .    3x,'   Results are probably invalid.')
        ELSE
          WRITE(3,2084)
          WRITE(*,2084)
2084    FORMAT(
     .    3X,'!Warning! Some parameters are out of range...'/
     .    3X,'   Results are probably invalid.')
        ENDIF
      ENDIF
      WRITE(3,2000) ' '
      WRITE(*,2000) ' '
      WRITE(*,2002) 'exit (N)?'
      READ(*,2000) LQ
      IF(.NOT.(LQ .EQ. 'Y' .OR. LQ .EQ. 'y')) GO TO 10
C
80    CONTINUE
        END
      SUBROUTINE VCARD(NPS,KPS,LPS,IPS,*)
        CHARACTER KPS*(*),LPS(*)*8
        INTEGER IPS(*)
C
C       A "virtual card" for input.
C       NPS, the number of parameters to be input through the
C         same number of columns.
C       KPS, a string of NPS characters indicating the corresponding
C         type of variable:  A, a single character; I, an integer;
C         digit, a real, displayed with that many decimal places.
C       LPS, an array of NPS character strings of length 8;  these
C         will be displayed as column headings, short reminders for
C         what the parameters represent.
C       IPS, an array of NPS parameter values; on entry, used to display
C         and to retain as default values; on exit, the input values.
C
C       NPS can be at most 7.  IPS is generic--integer or real--the
C         calling program must be able to reference each properly.
C         The character return is as the integer value of the ASCII
C         code of its upper case version.
C
C
      CHARACTER LLS(7)*10,L1*1,LLA*8
      CHARACTER FFMT*6
      EQUIVALENCE(IPA,APA)
C
C       The following formats are machine dependent
C       For Microsoft Fortran--
3001  FORMAT(1X,7(2X,A8))
3002  FORMAT(2H |,7A10)
3003  FORMAT(7A10)
3004  FORMAT(A,1X,$)
C
C       The following are ANSI
2000  FORMAT(A)
2001  FORMAT(I6)
2002  FORMAT(I10)
2003  FORMAT(F10.0)
C
      DATA FFMT/'(F8.X)'/
C
C
100   CONTINUE
      DO 10 I=1,NPS
      IPA=IPS(I)
      L1=KPS(I:I)
      IF(L1 .EQ. 'A') THEN
        LLA=' '
        LLA(5:5)=CHAR(IPA)
      ELSE IF(L1 .EQ. 'I') THEN
        WRITE(LLA,2001) IPA
      ELSE
        FFMT(5:5)=L1
        WRITE(LLA,FFMT) APA
      ENDIF
      LLS(I)=LLA//' |'
10    CONTINUE
      WRITE(*,3001) (LPS(I),I=1,NPS)
      WRITE(*,3002) (LLS(I),I=1,NPS)
      READ (*,3003) (LLS(I),I=1,NPS)
      DO 11 I=1,NPS
      IF(LLS(I) .NE. ' ') THEN
        L1=KPS(I:I)
        IF(L1 .EQ. 'A') THEN
          J=0
101       J=J+1
          L1=LLS(I)(J:J)
          IF(L1 .EQ. ' ') GO TO 101
          IPA=ICHAR(L1)
          IF(IPA .GE. 96) IPA=IPA-32
        ELSE IF(L1 .EQ. 'I') THEN
          READ(LLS(I),2002,ERR=13) IPA
        ELSE
          READ(LLS(I),2003,ERR=13) APA
        ENDIF
        IPS(I)=IPA
      ENDIF
11    CONTINUE
      GO TO 80
C
13    CONTINUE
      IF(I .EQ. 1) THEN
        WRITE(*,3004) '   finished?'
        READ(*,2000) L1
        IF(L1 .EQ. 'N' .OR. L1 .EQ. 'n') GO TO 100
          GO TO 81
      ENDIF
      WRITE(*,2000) '   no, no, ...'
      GO TO 100
C
81    CONTINUE
      RETURN 1
80    CONTINUE
      RETURN
        END
