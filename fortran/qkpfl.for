C     PROGRAM QKPFL 
C        *QUICK PROFILE*
C        TO ILLUSTRATE THE USE OF THE LONGLEY-RICE MODEL
C          IN THE POINT-TO-POINT MODE 
C 
C        INPUT IS IN 10-COL FIELDS, THE FIRST OF WHICH IS 
C          A SEQUENCE OF DIGITS 
C        IN PARTICULAR, 
C          COL 1 IS THE *EXECUTE* COLUMN--A NON-ZERO DIGIT
C             WILL FORCE OUTPUT 
C          COL 2 INDICATES THE CARD TYPE--
C                          COL
C                          12      11,... 
C            STOP-         X0         (OR A BLANK CARD) 
C            TITLE-        X1         (NEXT CARD HAS 60-COL TITLE)
C            PROFILE-      X2T     D,XI,ZSC 
C               -WITH PROFILE ELEVATIONS ON THE FOLLOWING CARDS 
C            RELIABILITY-  X3      QR1,QR2,...
C            CONFIDENCE-   X4      QC1,QC2,...
C            PARAMETERS-   X7PC    FMHZ,HG1,HG2,N0,NS,EPS,SGM 
C            EXECUTE-      X8 
C            RESET-        X9 
C 
C        PROFILE ELEVATION CARDS HAVE 5-COL FIELDS, THE FIRST OF WHICH
C           IS A SEQUENCE OF DIGITS, ALL IN THE FORMAT
C                          ENN  P0,P1,... 
C 
      COMMON/PROP/KWX,AREF,MDP,DIST,HG(2),WN,DH,ENS,GME,ZGND, 
     X   HE(2),DL(2),THE(2) 
         COMPLEX ZGND 
      COMMON/PROPV/LVAR,SGC,MDVAR,KLIM
      COMMON/PROPA/DLSA,DX,AEL,AK1,AK2,AED,EMD,AES,EMS,DLS(2),DLA,THA 
C 
      COMMON/SAVE/SAVE(50)
C 
      DIMENSION JIN(6),XIN(7) 
      DIMENSION ITL(15),IPTL(15)
      DIMENSION QFL(15),PFL(603)
      DIMENSION QC(7),ZC(7),QR(7),ZR(7),XLB(7)
C 
      LOGICAL WQIT,WCON,WPF,WTL,WPTL
C 
C        THE I/O UNITS ARE DEFINED HERE 
      DATA KIN,KOT/5,6/ 
C 
      DATA MZPFL/600/ 
C 
      DATA GMA/157E-9/
C 
      DATA DB/8.685890/ 
      DATA AKM/1000./ 
C 
       WQIT=.FALSE. 
       WCON=.TRUE.
       GO TO 190
C 
  10   CONTINUE 
C        READ INPUT SEQUENCE
1000  FORMAT(6I1,4X,7F10.0) 
1001  FORMAT(15A4)
1002  FORMAT(I1,I2,2X,15F5.0) 
C 
       JIN(1)=0 
       JIN(2)=0 
       READ(KIN,1000) JIN,XIN 
       WCON=JIN(1) .EQ. 0 
       JQ=JIN(2)
       IF(JQ .NE. 0)
     X    GO TO (110,120,130,140,150,160,170,180,190),JQ
C 
          WQIT=.TRUE. 
        GO TO 20
  110     CONTINUE
          READ(KIN,1001) ITL
          WTL=.TRUE.
        GO TO 20
  120     CONTINUE
          WPTL=JIN(3) .NE. 0
          IF(WPTL) READ(KIN,1001) IPTL
          ZSC=XIN(3)
          IF(ZSC .LE. 0.) ZSC=1.
          NP=-1 
  121       JQ=0
            NQ=0
            READ(KIN,1002) JQ,NQ,QFL
            WPF=JQ .EQ. 0 
            NQ=MIN0(NQ,15)
            IF(NQ .GT. 0) GO TO 122 
               WPF=.FALSE.
             GO TO 124
  122          DO 123 JQ=1,NQ 
                 NP=NP+1
                 IF(NP .LE. MZPFL) PFL(NP+3)=QFL(JQ)*ZSC
  123          CONTINUE 
  124     IF(WPF) GO TO 121 
          PFL(1)=NP 
          DKM=XIN(1)
          XKM=XIN(2)
          WPF=NP .GT. 0 
          IF(.NOT. WPF) GO TO 128 
          IF(DKM .LE. 0.) DKM=XKM*PFL(1)
          IF(XKM .LE. 0.) XKM=DKM/PFL(1)
          PFL(2)=DKM*AKM/PFL(1) 
          WPF=(NP .LE. MZPFL) .AND. (DKM .GT. 0.) 
     X       .AND. (ABS(DKM-XKM*PFL(1)) .LT. 0.5*XKM) 
  128   GO TO 20
  130     CONTINUE
          NR=0
          DO 131 JR=1,7 
            IF(XIN(JR) .LE. 0.) GO TO 131 
            NR=NR+1 
            QR(NR)=XIN(JR)
            ZR(NR)=QERFI(QR(NR)/100.) 
  131     CONTINUE
          IF(NR .GT. 0) GO TO 138 
             NR=1 
             QR(1)=50.
             ZR(1)=0. 
  138   GO TO 20
  140     CONTINUE
          NC=0
          DO 141 JC=1,7 
            IF(XIN(JC) .LE. 0.) GO TO 141 
            NC=NC+1 
            QC(NC)=XIN(JC)
            ZC(NC)=QERFI(QC(NC)/100.) 
  141     CONTINUE
          IF(NC .GT. 0) GO TO 148 
             NC=1 
             QC(1)=50.
             ZC(1)=0. 
  148   GO TO 20
  150     CONTINUE
  158   GO TO 20
  160     CONTINUE
        GO TO 20
  170     CONTINUE
          IPOL=MIN0(JIN(3),1) 
          IF(JIN(4) .LE. 0) GO TO 171 
             KLIM=MIN0(JIN(4),7)
             LVAR=5 
  171     IF(XIN(1) .GT. 0.) FMHZ=XIN(1)
          IF(XIN(2) .GT. 0.) HG(1)=XIN(2) 
          IF(XIN(3) .GT. 0.) HG(2)=XIN(3) 
          IF(XIN(5) .GT. 0.) ENS0=XIN(5)
          IF(XIN(4) .LE. 0.) GO TO 172
             EN0=XIN(4) 
             ENS0=0.
  172     IF(XIN(6) .LE. 0.) GO TO 178
             EPS=XIN(6) 
             SGM=XIN(7) 
  178   GO TO 20
  180     CONTINUE
          WCON=.FALSE.
        GO TO 20
  190     CONTINUE
          FMHZ=100. 
          HG(1)=3.
          HG(2)=3.
          EN0=310.
          ENS0=0. 
          EPS=15. 
          SGM=0.005 
          IPOL=1
          KLIM=5
          LVAR=5
          NC=3
          NR=3
          QC(1)=50. 
          QC(2)=90. 
          QC(3)=10. 
          QR(1)=50. 
          QR(2)=90. 
          QR(3)=10. 
          ZC(1)=0.
          ZC(2)=-1.28155
          ZC(3)= 1.28155
          ZR(1)=0.
          ZR(2)=-1.28155
          ZR(3)= 1.28155
          DKM=0.
          XKM=0.
          NP=-1 
          WPF=.FALSE. 
          WTL=.FALSE. 
C 
  20   CONTINUE 
       IF(WCON) GO TO 30
C 
C        EXECUTE
C 
C 
C        WRITE HEADING
2001  FORMAT(1H1/1H0) 
2002  FORMAT(1H ) 
2010  FORMAT(3X,
     .62HLINK PREDICTIONS FROM THE LONGLEY-RICE MODEL, VERSION 1.2.2   )
2011  FORMAT(3X,15A4) 
2014  FORMAT(13X,8HDISTANCE,F12.1,3H KM)
2015  FORMAT(12X,9HFREQUENCY,F12.1,4H MHZ)
2016  FORMAT(6X,15HANTENNA HEIGHTS,2F8.1,2H M)
2017  FORMAT(4X,17HEFFECTIVE HEIGHTS,2F8.1,2H M)
2018  FORMAT(5X,16HTERRAIN, DELTA H,F12.0,2H M) 
C 
       WRITE(KOT,2001)
       IF(WTL) GO TO 211
       WRITE(KOT,2010)
       GO TO 212
  211  WRITE(KOT,2011) ITL
  212  WRITE(KOT,2002)
       IF(.NOT. WPTL) GO TO 213 
       WRITE(KOT,2011) IPTL 
       WRITE(KOT,2002)
  213  WRITE(KOT,2002)
       WRITE(KOT,2014) DKM
       WRITE(KOT,2015) FMHZ 
       WRITE(KOT,2016) HG 
       IF(WPF) GO TO 215
       WRITE(KOT,2002)
       WRITE(KOT,2023) NP,XKM 
       WRITE(KOT,2029)
       GO TO 30 
  215  CONTINUE 
       KWX=0
       ZSYS=0.
       Q=ENS0 
       IF(Q .GT. 0.) GO TO 222
          JA=3.+0.1*PFL(1)
          JB=NP-JA+6
          DO 221 J=JA,JB
  221     ZSYS=ZSYS+PFL(J)
          ZSYS=ZSYS/FLOAT(JB-JA+1)
          Q=EN0 
  222  CALL QLRPS(FMHZ,ZSYS,Q,IPOL,EPS,SGM) 
       CALL QLRPFL(PFL,0,11)
       FS=DB*ALOG(2.*WN*DIST) 
C 
       WRITE(KOT,2017) HE 
       WRITE(KOT,2018) DH 
       WRITE(KOT,2002)
C 
2021  FORMAT(3X,4HPOL=,I1,6H, EPS=,F3.0,6H, SGM=,F6.3,4H S/M) 
2022  FORMAT(3X,5HCLIM=,I1,5H, NS=,F4.0,4H, K=,F6.3)
2023  FORMAT(3X,12HPROFILE- NP=,I4,5H, XI=,F6.3,3H KM)
2029  FORMAT(6X,32HERRORS--ANALYSIS CANNOT CONTINUE)
C 
       Q=GMA/GME
       WRITE(KOT,2021) IPOL,EPS,SGM 
       WRITE(KOT,2022) KLIM,ENS,Q 
       WRITE(KOT,2023) NP,XKM 
       WRITE(KOT,2002)
C 
2030  FORMAT(6X,50HA LINE-OF-SIGHT PATH                              )
2031  FORMAT(6X,50HA SINGLE HORIZON PATH                             )
2032  FORMAT(6X,50HA DOUBLE-HORIZON PATH                             )
2036  FORMAT(6X,50HDIFFRACTION IS THE DOMINANT MODE                  )
2037  FORMAT(6X,50HTROPOSPHERIC SCATTER IS THE DOMINANT MODE         )
C 
       Q=DIST-DLA 
       Q=DIM(Q,0.5*PFL(2))-DIM(-Q,0.5*PFL(2)) 
       IF(Q) 230,231,232
  230     WRITE(KOT,2030) 
        GO TO 238 
  231     WRITE(KOT,2031) 
        GO TO 235 
  232     WRITE(KOT,2032) 
  235     IF(DIST .LE. DLSA) GO TO 236
          IF(DIST .GT. DX) GO TO 237
  236     WRITE(KOT,2036) 
        GO TO 238 
  237     WRITE(KOT,2037) 
  238  CONTINUE 
C 
2040  FORMAT(3X,
     .62HESTIMATED QUANTILES OF BASIC TRANSMISSION LOSS (DB)           /
     .   6X,17HFREE SPACE VALUE-,F7.1,3H DB)
2041  FORMAT(9X,6HRELIA-,4X,15HWITH CONFIDENCE/ 
     .   9X,7HBILITY ,7F8.1)
2045  FORMAT(4X,2F10.1,6F8.1) 
C 
C        COMPUTE AND PRINT VALUES 
       WRITE(KOT,2002)
       WRITE(KOT,2040) FS 
       WRITE(KOT,2002)
       WRITE(KOT,2041) (QC(JC), JC=1,NC)
       WRITE(KOT,2002)
       DO 240 JR=1,NR 
         DO 241 JC=1,NC 
  241    XLB(JC)=FS+AVAR(ZR(JR),0.,ZC(JC))
         WRITE(KOT,2045) QR(JR),(XLB(JC), JC=1,NC)
  240  CONTINUE 
C 
2081  FORMAT(3X,
     .62H**WARNING- SOME PARAMETERS ARE NEARLY OUT OF RANGE.           /
     .   3X,
     .62H  RESULTS SHOULD BE USED WITH CAUTION.                        )
2082  FORMAT(3X,
     .62H**NOTE- DEFAULT PARAMETERS HAVE BEEN SUBSTITUTED              /
     .   3X,
     .62H  FOR IMPOSSIBLE ONES.                                        )
2083  FORMAT(3X,
     .62H**WARNING- A COMBINATION OF PARAMETERS IS OUT OF RANGE.       /
     .   3X,
     .62H  RESULTS ARE PROBABLY INVALID.                               )
2084  FORMAT(3X,
     .62H**WARNING- SOME PARAMETERS ARE OUT OF RANGE.                  /
     .   3X,
     .62H  RESULTS ARE PROBABLY INVALID.                               )
C 
       IF(KWX .EQ. 0) GO TO 28
C 
C        PRINT ERROR MESSAGES 
          WRITE(KOT,2002) 
          GO TO (281,282,283,284),KWX 
  281     WRITE(KOT,2081) 
        GO TO 28
  282     WRITE(KOT,2082) 
        GO TO 28
  283     WRITE(KOT,2083) 
        GO TO 28
  284     WRITE(KOT,2084) 
  28   CONTINUE 
C 
  30   CONTINUE 
       IF(.NOT. WQIT) GO TO 10
C 
       STOP 
      END 
