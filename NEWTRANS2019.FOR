C this progran was updated in jan 2010 to account for leaf area added in sept 2009 and because of a 
C problem with the growth calculation which was reading in zeros if the last line in 
C bio.out had only one data point




c this program was written APR 2002 FOR THE NI CENSUS DATA
c it reads output density, biomass, growth BIRTH AND DEATH files from NEWCENS.for
c it puts the data in columns by month with month, year and site
C FOR GOAT ISLAND SITES, THE TREATMENT CODES ARE AS FOLLOWS:
C DISTURBED CONTROLS  NTREAT=1
C UNDISTIRBED CONTROLS NTREAT=2
C NITROGEN PLUS PHOSPHORUS NTREAT=3
C NITROGEN PLUS IRON NTREAT=4
C IRON ONLY NTREAT=5
C  FOR OLHM SITES MULT BY 50. TO CONVERT TO M2
C  FOR LOW MARSH SITES MUTL BY 22.22 TO CONVERT TO M2
C  FOR GIHM SITES MULT BY 100 TO CONVERT TO M2

       dimension gro(28,1980),JDAY(1980,5),monday(11),BIO(28,1980)
       DIMENSION DEN(28,1980),BIR(28,1980),dea(28,980),ITRAN(16)
       DIMENSION SITE(16),TREAT(16),AGE(28,1980),PAREA(28,1980)
       INTEGER IPLOT(16)
       CHARACTER SITE*4, treat*2,rep*1,head*82,HOLDER*54
      data monday/31,59,90,120,151,181,212,243,273,304,334/
      head=
     1'   mm   dd  yy  site tr pl rep  den     bio     gro    bir     de
     2a     age   parea'
      HOLDER='    .        .       .      .      .       .      .  '
      write(*,'(A74)') head
C READ IN THE DATE FILE
      WRITE(*,69)                                                       CEN00220
 69   FORMAT(' ENTER A 1 FOR GIHM OR 2 FOR OLHM AND LOW MARSHES')
      READ(*,'(I1)') LTER                                               CEN00250
      area=100.
      OPEN(1,FILE='lterdate.txt')
            open(2,file='transdum.txt')
      DO 222 I=1,999
      READ(1,1,END=233) N,M,MD,MY
      MDATA=N
      MDP1=MDATA+1
      IF(N.EQ.1) MY1=MY
 1    FORMAT(I4,2I3,1X,I4)
C JDAY(N,1) IS FOR THE MONTH, JDAY(N,2) IS THE DAY OF MONTH, JDAY(N,3) IS YEAR
C JDAY(N,4) IS THE JULIAN DAY AND JDAY(N,5) IS THE CUMMULATIVE NUMBER OF DAYS
      JDAY(N,3)=MY
      JDAY(N,1)=M
      JDAY(N,2)=MD
      IF(MY.GT.MY1) MY1=MY1+1
      IF(M.GT.1) JDAY(N,4)=MONDAY(M-1)+MD
      IF(M.EQ.1) JDAY(N,4)=MD
      JDAY(N,5)=JDAY(N,4)+(MY1-1984)*365
       WRITE(*,'(I4,2I3,1X,I4,4I6.0)') N,M,MD,MY,(JDAY(N,KK),KK=1,4)
 222    CONTINUE
 233     CLOSE(1)
         close(2)
C **********************************************************************

 111   FORMAT(2I3,10F4.0)                                          
  2   format(6x,10f4.0)
c   333  WRITE(3,3) jday(m,1),jday(m,2),jday(m,3),SITE(1),treat(LL),
c     1 ITRAN(LL),rep,den(ll,m),bio(ll,m),GRO(LL,M),bir(ll,m),dea(ll,m)       
  3   FORMAT(3I5,1x,a4,1X,A2,i3,1x,a1,1x,F8.0,2f8.1,2f7.0,F7.2,F8.0)
  4   FORMAT(3I5,1x,a4,1X,A2,i3,1x,F6.1)
  5   FORMAT(2I3,10F7.2)                                          
  6   format(6x,10f7.2)
 101  FORMAT(4X,10F4.0)
 102  FORMAT(2I2,10F4.0)
c*************************************************************
c ****** now do it with the density files***************
c*************************************************************
C THIS FIRST SECTION IS FOR THE GIHM CENSUS DATA
       IF(LTER.EQ.2) GOTO 444
       SITE(1)='GIHM'
       AREA=100.
       OPEN(4,FILE='density.out')
       do 11 LL=1,16
         IF(LL.LT.3) IPLOT(LL)=2
         IF(LL.GT.2.AND.LL.LT.5) IPLOT(LL)=5
         IF(LL.GT.4.AND.LL.LT.7) IPLOT(LL)=7
         if(LL.GT.6.and.LL.LT.9) IPLOT(LL)=9
         IF(LL.GT.8.AND.LL.LT.11) IPLOT(LL)=12
         IF(LL.GT.10.AND.LL.LT.13) IPLOT(LL)=13
         IF(LL.GT.12.AND.LL.LT.15) IPLOT(LL)=14
         IF(LL.GT.14) IPLOT(LL)=15
        read(4,111) Itran(LL),NTREAT,(DEN(LL,M),M=1,10)  
         if(ntreat.eq.2) treat(LL)='UC'
         if(ntreat.eq.3) treat(LL)='NP'
  11      read(4,2) (DEN(LL,M),M=11,mdata)
       close(4)
c*************************************************************
c ****** now do the same with the biomass files***************
c*************************************************************
        OPEN(4,FILE='biomass.out')
       do 19 LL=1,16
        read(4,5) Itran(LL),NTREAT,(BIO(LL,M),M=1,10)  
         if(ntreat.eq.2) treat(LL)='UC'
         if(ntreat.eq.3) treat(LL)='NP'
  19      read(4,6) (BIO(LL,M),M=11,mdata)
       close(4)
c*************************************************************
c ****** now do the same with the PLANT AREA files***************
c*************************************************************
        OPEN(4,FILE='PAREA.out')
       do 20 LL=1,16
        read(4,5) Itran(LL),NTREAT,(PAREA(LL,M),M=1,10)  
         if(ntreat.eq.2) treat(LL)='UC'
         if(ntreat.eq.3) treat(LL)='NP'
  20      read(4,6) (PAREA(LL,M),M=11,mdata)
       close(4)     
c*************************************************************
c ****** now do the same with the AGE files***************
c*************************************************************
        OPEN(4,FILE='AGE.out')
       do 25 LL=1,16
        read(4,5) Itran(LL),NTREAT,(AGE(LL,M),M=1,10)  
         if(ntreat.eq.2) treat(LL)='UC'
         if(ntreat.eq.3) treat(LL)='NP'
  25      read(4,6) (AGE(LL,M),M=11,mdata)
       close(4)
c ****************************************************************************
c ***************** now do it for the birth files*****************************
c ****************************************************************************
       OPEN(4,FILE='births.out')
       do 35 LL=1,16
        read(4,111) Itran(LL), NTREAT,(BIR(LL,M),M=1,10)  
         if(ntreat.eq.2) treat(LL)='UC'
         if(ntreat.eq.3) treat(LL)='NP'
  35      read(4,2) (BIR(LL,M),M=11,mdata)
       close(4)
c ****************************************************************************
c ***************** now do it for the death files******************************
c ****************************************************************************
       OPEN(4,FILE='deaTHS.out')
c this is for reading in census 120 *******
         Do 41 LL=1,16
        read(4,111) Itran(LL), NTREAT,(DEA(LL,M),M=1,10)  
         if(ntreat.eq.2) treat(LL)='UC'
         if(ntreat.eq.3) treat(LL)='NP'
  41      read(4,2) (DEA(LL,M),M=11,mdata)
       close(4)
c ****************************************************************************
c ***************** now do it for the growth files***************
c ****************************************************************************
       OPEN(4,FILE='growth.out')
       do 29 LL=1,16
        read(4,5) Itran(LL), NTREAT,(GRO(LL,M),M=1,10) 
          if(ntreat.eq.2) treat(LL)='UC'
         if(ntreat.eq.3) treat(LL)='NP'
  29    read(4,6) (GRO(LL,M),M=11,MDATA)
       close(4)
C put yy 4.0 mm 3.0 site 5.0 treat 3.0 plot 3.0 rep 3.0 (den bio gro bir dea parea) (8.0);
       OPEN(3,FILE='GIHMCEN.DAT')
              write(3,'(A82)') head     
       DO 333 LL=1,16
       REP='a'
       IF(LL.EQ.2.or.LL.eq.4.or.LL.eq.6.or.LL.eq.8) REP='b'
       IF(LL.EQ.10.or.LL.eq.12.or.LL.eq.14.or.LL.eq.16) REP='b'
C plots 7 and 15 start in May 2015 month 363
C plots 12C and 13F will be discontinued
c output the final data file for gihm census  
     
      if(LL.EQ.5.OR.LL.EQ.6.OR.LL.EQ.15.OR.LL.EQ.16) GOTO 335
C this is for writing 4 blank lines for the Jan-Apr months of the 1984 GIHM plots
      Do 332 M=1,4
      WRITE(3,'(3I5,1x,a4,1X,A2,i3,1x,a1,A52)') M,JDAY(M,2),
     2 JDAY(M,3),SITE(1),TREAT(LL),IPLOT(LL),REP,HOLDER
       write(*,'(3I5,1x,a4,1X,A2,i3,1x,a1,A54)') M,JDAY(M,2),
     1 JDAY(M,3),SITE(1),TREAT(LL),IPLOT(LL),REP,HOLDER
  332 continue
      MSTART=1
      GOTO 337    
C this is for writing 3 blank lines for the new plots added in 2015 
  335 Do 336 M=1,3  
          MS=359+M   
       WRITE(3,'(3I5,1x,a4,1X,A2,i3,1x,a1,A52)') JDAY(MS,1),JDAY(MS,2),
     2 JDAY(MS,3),SITE(1),TREAT(LL),IPLOT(LL),REP,HOLDER
       write(*,'(3I5,1x,a4,1X,A2,i3,1x,a1,A54)') JDAY(MS,1),JDAY(M,2),
     1 JDAY(M,3),SITE(1),TREAT(LL),IPLOT(LL),REP,HOLDER
  336 continue
      MSTART=363
c convert plant area cm2 per sq meter 
c FIRST CENSUS DATE WITH NEW PLOT SIZES:  mm=409; July 9, 2019
 
        write(*,'(2i6)') MSTART,MDATA
  337  DO 333 M=MSTART,MDATA
       if(M.ge.409) then area=50.
       WRITE(3,3) jday(m,1),jday(m,2),jday(m,3),SITE(1),treat(LL),
     1 IPLOT(LL),rep,den(ll,m)*area,bio(ll,m)*area,GRO(LL,M)*area,
     2 bir(ll,m)*area,dea(ll,m)*area,AGE(LL,M),parea(ll,m)*area 
  333  continue     
       STOP
C       
C THIS NEXT SECTION IS FOR THE OYSTER LANDING AND GI LOW MARSH DATA       
c*************************************************************
c ****** now do it with the density files***************
c*************************************************************
C THIS second SECTION IS FOR THE OLHM AND LOW MARSH CENSUS DATA
  444     OPEN(4,FILE='densit2.out')  
        do 112 LL=1,18
        IF(LL.LT.7) SITE(LL)='OLHM'
        IF(LL.GT.6.AND.LL.LT.13) SITE(LL)='OLLM'
        IF(LL.GT.12) SITE(LL)='GILM'
        read(4,111) Itran(LL),NTREAT,(DEN(LL,M),M=1,10)  
        TREAT(LL)='UC'
  112      read(4,2) (DEN(LL,M),M=11,mdata)
       close(4)
c*************************************************************
c ****** now do the same with the biomass files***************
c*************************************************************
       OPEN(4,FILE='BIOMAS2.OUT')
       do 119 LL=1,18
        read(4,5) Itran(LL),NTREAT,(BIO(LL,M),M=1,10)  
         treat(LL)='UC'
  119      read(4,6) (BIO(LL,M),M=11,mdata)
        CLOSE(4)
c*************************************************************
c ****** now do the same with the plant leaf area files***************
c*************************************************************
       OPEN(4,FILE='PAREA2.OUT')
       do 120 LL=1,18
        read(4,5) Itran(LL),NTREAT,(PAREA(LL,M),M=1,10)  
         treat(LL)='UC'
  120      read(4,6) (PAREA(LL,M),M=11,mdata)
        CLOSE(4)
c*************************************************************
c ****** now do the same with the AGE files***************
c*************************************************************
       OPEN(4,FILE='AGE2.OUT')
       do 125 LL=1,18       
        read(4,5) Itran(LL),NTREAT,(AGE(LL,M),M=1,10)  
         treat(LL)='UC'
  125      read(4,6) (AGE(LL,M),M=11,mdata)
        CLOSE(4)
c ****************************************************************************
c ***************** now do it for the birth files*****************************
c ****************************************************************************
       OPEN(4,FILE='BIRTHS2.OUT')
       do 135 LL=1,18
        read(4,111) Itran(LL), NTREAT,(BIR(LL,M),M=1,10)  
         treat(LL)='UC'
  135      read(4,2) (BIR(LL,M),M=11,mdata)
        CLOSE(4)
c ****************************************************************************
c ***************** now do it for the death files******************************
c ****************************************************************************
c this is for reading in census 120 *******
       OPEN(4,FILE='DEATHs2.OUT')
         Do 141 LL=1,18
        read(4,111) Itran(LL), NTREAT,(DEA(LL,M),M=1,10)  
        treat(LL)='UC'
  141      read(4,2) (DEA(LL,M),M=11,mdata)
        CLOSE(4)
c ****************************************************************************
c ***************** now do it for the growth files***************
c ****************************************************************************
       OPEN(4,FILE='GROWTH2.OUT')
       do 129 LL=1,18
        read(4,5) Itran(LL), NTREAT,(GRO(LL,M),M=1,10) 
        treat(LL)='UC'
 129    read(4,6) (GRO(LL,M),M=11,MDATA)
         CLOSE(2)
C ******************************************************************       
       OPEN(3,FILE='LTERCEN.DAT')
       TREAT(1)='UC'
       DO 334 LL=1,18
         IF(LL.LT.3) IPLOT(LL)=1
         IF(LL.GT.2.AND.LL.LT.5) IPLOT(LL)=2
         IF(LL.GT.4.AND.LL.LT.7) IPLOT(LL)=3
         IF(LL.GT.6.AND.LL.LT.9) IPLOT(LL)=4
         IF(LL.GT.8.AND.LL.LT.11) IPLOT(LL)=5
         IF(LL.GT.10.AND.LL.LT.13) IPLOT(LL)=6
         IF(LL.GT.12.AND.LL.LT.15) IPLOT(LL)=7
         IF(LL.GT.14.AND.LL.LT.17) IPLOT(LL)=8
         IF(LL.GT.16.AND.LL.LT.19) IPLOT(LL)=9
       IF(LL.EQ.1.or.LL.eq.3.or.LL.eq.5.or.LL.eq.7) REP='a'
       IF(LL.EQ.2.or.LL.eq.4.or.LL.eq.6.or.LL.eq.8) REP='b'
       IF(LL.EQ.9.or.LL.eq.11.OR.LL.EQ.13.OR.LL.EQ.15.OR.LL.EQ.17)
     1   REP='a'
       IF(LL.EQ.10.or.LL.eq.12.OR.LL.EQ.14.OR.LL.EQ.16.OR.LL.EQ.18)
     2   REP='b'
       IF(LL.LT.7) AREA=50.
       IF(LL.GT.6) AREA=22.22
c output the final data file for LTER census       
       DO 334 M=27,MDATA
  334  WRITE(3,3) jday(m,1),jday(m,2),jday(m,3),SITE(ll),TREAT(1),
     1 IPLOT(LL),rep,den(ll,m)*area,bio(ll,m)*area,GRO(LL,M)*area,
     2 bir(ll,m)*area,dea(ll,m)*area,AGE(LL,M),PAREA(LL,M)*AREA      

      STOP
      END