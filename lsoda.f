        SUBROUTINE DLSODA (F, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK,
            1            ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, JT,
            2            rpar, ipar)
            EXTERNAL F, JAC

            integer ipar(*)
            double precision rpar(*)

            INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK, LIW, JT
            DOUBLE PRECISION Y, T, TOUT, RTOL, ATOL, RWORK
            DIMENSION NEQ(*), Y(*), RTOL(*), ATOL(*), RWORK(LRW), IWORK(LIW)
 
            EXTERNAL DPRJA, DSOLSY
            DOUBLE PRECISION DUMACH, DMNORM
            INTEGER INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS,
            1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
            2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
            3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
            INTEGER INSUFR, INSUFI, IXPR, IOWNS2, JTYP, MUSED, MXORDN, MXORDS
            INTEGER I, I1, I2, IFLAG, IMXER, KGO, LF0,
            1   LENIW, LENRW, LENWM, ML, MORD, MU, MXHNL0, MXSTP0
            INTEGER LEN1, LEN1C, LEN1N, LEN1S, LEN2, LENIWC, LENRWC
            DOUBLE PRECISION ROWNS,
            1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
            DOUBLE PRECISION TSW, ROWNS2, PDNORM
            DOUBLE PRECISION ATOLI, AYI, BIG, EWTI, H0, HMAX, HMX, RH, RTOLI,
            1   TCRIT, TDIST, TNEXT, TOL, TOLSF, TP, SIZE, SUM, W0
            DIMENSION MORD(2)
            LOGICAL IHIT
            CHARACTER(LEN=80) MSG
            SAVE MORD, MXSTP0, MXHNL0
       
            COMMON /DLS001/ ROWNS(209),
            1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
            2   INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS(6),
            3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
            4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
            5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
        C
            COMMON /DLSA01/ TSW, ROWNS2(20), PDNORM,
            1   INSUFR, INSUFI, IXPR, IOWNS2(2), JTYP, MUSED, MXORDN, MXORDS
        C
            DATA MORD(1),MORD(2)/12,5/, MXSTP0/500/, MXHNL0/10/
       
            
            IF (ISTATE .LT. 1 .OR. ISTATE .GT. 3) GO TO 601
            IF (ITASK .LT. 1 .OR. ITASK .GT. 5) GO TO 602
            IF (ISTATE .EQ. 1) GO TO 10
            IF (INIT .EQ. 0) GO TO 603
            IF (ISTATE .EQ. 2) GO TO 200
            GO TO 20
        10   INIT = 0
            IF (TOUT .EQ. T) RETURN
        
        20   IF (NEQ(1) .LE. 0) GO TO 604
            IF (ISTATE .EQ. 1) GO TO 25
            IF (NEQ(1) .GT. N) GO TO 605
        25   N = NEQ(1)
            IF (ITOL .LT. 1 .OR. ITOL .GT. 4) GO TO 606
            IF (IOPT .LT. 0 .OR. IOPT .GT. 1) GO TO 607
            IF (JT .EQ. 3 .OR. JT .LT. 1 .OR. JT .GT. 5) GO TO 608
            JTYP = JT
            IF (JT .LE. 2) GO TO 30
            ML = IWORK(1)
            MU = IWORK(2)
            IF (ML .LT. 0 .OR. ML .GE. N) GO TO 609
            IF (MU .LT. 0 .OR. MU .GE. N) GO TO 610
        30   CONTINUE
        
            IF (IOPT .EQ. 1) GO TO 40
            IXPR = 0
            MXSTEP = MXSTP0
            MXHNIL = MXHNL0
            HMXI = 0.0D0
            HMIN = 0.0D0
            IF (ISTATE .NE. 1) GO TO 60
            H0 = 0.0D0
            MXORDN = MORD(1)
            MXORDS = MORD(2)
            GO TO 60
        40   IXPR = IWORK(5)
            IF (IXPR .LT. 0 .OR. IXPR .GT. 1) GO TO 611
            MXSTEP = IWORK(6)
            IF (MXSTEP .LT. 0) GO TO 612
            IF (MXSTEP .EQ. 0) MXSTEP = MXSTP0
            MXHNIL = IWORK(7)
            IF (MXHNIL .LT. 0) GO TO 613
            IF (MXHNIL .EQ. 0) MXHNIL = MXHNL0
            IF (ISTATE .NE. 1) GO TO 50
            H0 = RWORK(5)
            MXORDN = IWORK(8)
            IF (MXORDN .LT. 0) GO TO 628
            IF (MXORDN .EQ. 0) MXORDN = 100
            MXORDN = MIN(MXORDN,MORD(1))
            MXORDS = IWORK(9)
            IF (MXORDS .LT. 0) GO TO 629
            IF (MXORDS .EQ. 0) MXORDS = 100
            MXORDS = MIN(MXORDS,MORD(2))
            IF ((TOUT - T)*H0 .LT. 0.0D0) GO TO 614
        50   HMAX = RWORK(6)
            IF (HMAX .LT. 0.0D0) GO TO 615
            HMXI = 0.0D0
            IF (HMAX .GT. 0.0D0) HMXI = 1.0D0/HMAX
            HMIN = RWORK(7)
            IF (HMIN .LT. 0.0D0) GO TO 616
       
        60   IF (ISTATE .EQ. 1) METH = 1
            IF (ISTATE .EQ. 1) NYH = N
            LYH = 21
            LEN1N = 20 + (MXORDN + 1)*NYH
            LEN1S = 20 + (MXORDS + 1)*NYH
            LWM = LEN1S + 1
            IF (JT .LE. 2) LENWM = N*N + 2
            IF (JT .GE. 4) LENWM = (2*ML + MU + 1)*N + 2
            LEN1S = LEN1S + LENWM
            LEN1C = LEN1N
            IF (METH .EQ. 2) LEN1C = LEN1S
            LEN1 = MAX(LEN1N,LEN1S)
            LEN2 = 3*N
            LENRW = LEN1 + LEN2
            LENRWC = LEN1C + LEN2
            IWORK(17) = LENRW
            LIWM = 1
            LENIW = 20 + N
            LENIWC = 20
            IF (METH .EQ. 2) LENIWC = LENIW
            IWORK(18) = LENIW
            IF (ISTATE .EQ. 1 .AND. LRW .LT. LENRWC) GO TO 617
            IF (ISTATE .EQ. 1 .AND. LIW .LT. LENIWC) GO TO 618
            IF (ISTATE .EQ. 3 .AND. LRW .LT. LENRWC) GO TO 550
            IF (ISTATE .EQ. 3 .AND. LIW .LT. LENIWC) GO TO 555
            LEWT = LEN1 + 1
            INSUFR = 0
            IF (LRW .GE. LENRW) GO TO 65
            INSUFR = 2
            LEWT = LEN1C + 1
            MSG='DLSODA-  Warning.. RWORK length is sufficient for now, but  '
            CALL XERRWD (MSG, 60, 103, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG='      may not be later.  Integration will proceed anyway.   '
            CALL XERRWD (MSG, 60, 103, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '      Length needed is LENRW = I1, while LRW = I2.'
            CALL XERRWD (MSG, 50, 103, 0, 2, LENRW, LRW, 0, 0.0D0, 0.0D0)
        65   LSAVF = LEWT + N
            LACOR = LSAVF + N
            INSUFI = 0
            IF (LIW .GE. LENIW) GO TO 70
            INSUFI = 2
            MSG='DLSODA-  Warning.. IWORK length is sufficient for now, but  '
            CALL XERRWD (MSG, 60, 104, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG='      may not be later.  Integration will proceed anyway.   '
            CALL XERRWD (MSG, 60, 104, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '      Length needed is LENIW = I1, while LIW = I2.'
            CALL XERRWD (MSG, 50, 104, 0, 2, LENIW, LIW, 0, 0.0D0, 0.0D0)
        70   CONTINUE
CCheck RTOL and ATOL for legality. ------------------------------------
            RTOLI = RTOL(1)
            ATOLI = ATOL(1)
            DO 75 I = 1,N
            IF (ITOL .GE. 3) RTOLI = RTOL(I)
            IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
            IF (RTOLI .LT. 0.0D0) GO TO 619
            IF (ATOLI .LT. 0.0D0) GO TO 620
        75     CONTINUE
            IF (ISTATE .EQ. 1) GO TO 100
CIf ISTATE = 3, set flag to signal parameter changes to DSTODA. -------
            JSTART = -1
            IF (N .EQ. NYH) GO TO 200
CNEQ was reduced.  Zero part of YH to avoid undefined references. -----
            I1 = LYH + L*NYH
            I2 = LYH + (MAXORD + 1)*NYH - 1
            IF (I1 .GT. I2) GO TO 200
            DO 95 I = I1,I2
            RWORK(I) = 0.0D0
        95   CONTINUE
            GO TO 200
        
        100  UROUND = DUMACH()
            TN = T
            TSW = T
            MAXORD = MXORDN
            IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 110
            TCRIT = RWORK(1)
            IF ((TCRIT - TOUT)*(TOUT - T) .LT. 0.0D0) GO TO 625
            IF (H0 .NE. 0.0D0 .AND. (T + H0 - TCRIT)*H0 .GT. 0.0D0)
            1   H0 = TCRIT - T
        110  JSTART = 0
            NHNIL = 0
            NST = 0
            NJE = 0
            NSLAST = 0
            HU = 0.0D0
            NQU = 0
            MUSED = 0
            MITER = 0
            CCMAX = 0.3D0
            MAXCOR = 3
            MSBP = 20
            MXNCF = 10
CInitial call to F.  (LF0 points to YH(*,2).) -------------------------
            LF0 = LYH + NYH
            CALL F (NEQ, T, Y, RWORK(LF0), rpar, ipar)
            NFE = 1
CLoad the initial value vector in YH. ---------------------------------
            DO 115 I = 1,N
            RWORK(I+LYH-1) = Y(I)
        115  CONTINUE
CLoad and invert the EWT array.  (H is temporarily set to 1.0.) -------
            NQ = 1
            H = 1.0D0
            CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
            DO 120 I = 1,N
            IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 621
            RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
        120  CONTINUE
        
            IF (H0 .NE. 0.0D0) GO TO 180
            TDIST = ABS(TOUT - T)
            W0 = MAX(ABS(T),ABS(TOUT))
            IF (TDIST .LT. 2.0D0*UROUND*W0) GO TO 622
            TOL = RTOL(1)
            IF (ITOL .LE. 2) GO TO 140
            DO 130 I = 1,N
            TOL = MAX(TOL,RTOL(I))
        130  CONTINUE
        140  IF (TOL .GT. 0.0D0) GO TO 160
            ATOLI = ATOL(1)
            DO 150 I = 1,N
            IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
            AYI = ABS(Y(I))
            IF (AYI .NE. 0.0D0) TOL = MAX(TOL,ATOLI/AYI)
        150    CONTINUE
        160  TOL = MAX(TOL,100.0D0*UROUND)
            TOL = MIN(TOL,0.001D0)
            SUM = DMNORM (N, RWORK(LF0), RWORK(LEWT))
            SUM = 1.0D0/(TOL*W0*W0) + TOL*SUM**2
            H0 = 1.0D0/SQRT(SUM)
            H0 = MIN(H0,TDIST)
            H0 = SIGN(H0,TOUT-T)
CAdjust H0 if necessary to meet HMAX bound. ---------------------------
        180  RH = ABS(H0)*HMXI
            IF (RH .GT. 1.0D0) H0 = H0/RH
CLoad H with H0 and scale YH(*,2) by H0. ------------------------------
            H = H0
            DO 190 I = 1,N
            RWORK(I+LF0-1) = H0*RWORK(I+LF0-1)
        190  CONTINUE
            GO TO 270
        C-----------------------------------------------------------------------
CBlock D.
CThe next code block is for continuation calls only (ISTATE = 2 or 3)
Cand is to check stop conditions before taking a step.
        C-----------------------------------------------------------------------
        200  NSLAST = NST
            IF (ITASK .EQ. 1) THEN
            GOTO 210
            ELSE IF (ITASK .EQ. 2) THEN
            GOTO 250
            ELSE IF (ITASK .EQ. 3) THEN
            GOTO 220
            ELSE IF (ITASK .EQ. 4) THEN
            GOTO 230
            ELSE IF (ITASK .EQ. 5) THEN
            GOTO 240
            ENDIF
Ckarline: changed from
C     GO TO (210, 250, 220, 230, 240), ITASK

        210  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
            CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
            IF (IFLAG .NE. 0) GO TO 627
            T = TOUT
            GO TO 420
        220  TP = TN - HU*(1.0D0 + 100.0D0*UROUND)
            IF ((TP - TOUT)*H .GT. 0.0D0) GO TO 623
            IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
            T = TN
            GO TO 400
        230  TCRIT = RWORK(1)
            IF ((TN - TCRIT)*H .GT. 0.0D0) GO TO 624
            IF ((TCRIT - TOUT)*H .LT. 0.0D0) GO TO 625
            IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 245
            CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
            IF (IFLAG .NE. 0) GO TO 627
            T = TOUT
            GO TO 420
        240  TCRIT = RWORK(1)
            IF ((TN - TCRIT)*H .GT. 0.0D0) GO TO 624
        245  HMX = ABS(TN) + ABS(H)
            IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
            IF (IHIT) T = TCRIT
            IF (IHIT) GO TO 400
            TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND)
            IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250
            H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND)
            IF (ISTATE .EQ. 2 .AND. JSTART .GE. 0) JSTART = -2
        
        250  CONTINUE
            IF (METH .EQ. MUSED) GO TO 255
            IF (INSUFR .EQ. 1) GO TO 550
            IF (INSUFI .EQ. 1) GO TO 555
        255  IF ((NST-NSLAST) .GE. MXSTEP) GO TO 500
            CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
            DO 260 I = 1,N
            IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 510
            RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
        260  CONTINUE
        270  TOLSF = UROUND*DMNORM (N, RWORK(LYH), RWORK(LEWT))
            IF (TOLSF .LE. 1.0D0) GO TO 280
            TOLSF = TOLSF*2.0D0
            IF (NST .EQ. 0) GO TO 626
            GO TO 520
        280  IF ((TN + H) .NE. TN) GO TO 290
            NHNIL = NHNIL + 1
            IF (NHNIL .GT. MXHNIL) GO TO 290
            MSG = 'DLSODA-  Warning..Internal T (=R1) and H (=R2) are'
            CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG='      such that in the machine, T + H = T on the next step  '
            CALL XERRWD (MSG, 60, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '     (H = step size). Solver will continue anyway.'
            CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 2, TN, H)
            IF (NHNIL .LT. MXHNIL) GO TO 290
            MSG = 'DLSODA-  Above warning has been issued I1 times.  '
            CALL XERRWD (MSG, 50, 102, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '     It will not be issued again for this problem.'
            CALL XERRWD (MSG, 50, 102, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0)
        290  CONTINUE
        
            CALL DSTODA (NEQ, Y, RWORK(LYH), NYH, RWORK(LYH), RWORK(LEWT),
            1   RWORK(LSAVF), RWORK(LACOR), RWORK(LWM), IWORK(LIWM),
            2   F, JAC, DPRJA, DSOLSY, rpar,ipar)
            KGO = 1 - KFLAG

            IF (KGO .EQ. 1) THEN
                GOTO 300
            ELSE IF (KGO .EQ. 2) THEN
                GOTO 530
            ELSE IF (KGO .EQ. 3) THEN
                GOTO 540
            ENDIF
       
        300  INIT = 1
            IF (METH .EQ. MUSED) GO TO 310
            TSW = TN
            MAXORD = MXORDN
            IF (METH .EQ. 2) MAXORD = MXORDS
            IF (METH .EQ. 2) RWORK(LWM) = SQRT(UROUND)
            INSUFR = MIN(INSUFR,1)
            INSUFI = MIN(INSUFI,1)
            JSTART = -1
            IF (IXPR .EQ. 0) GO TO 310
            IF (METH .EQ. 2) THEN
            MSG = 'Switch to BDF   at T (=R1), new step (=R2): %g, %g'
            CALL rprintfd2(MSG // char(0), TN, H)
            ENDIF
            IF (METH .EQ. 1) THEN
        
            MSG = 'Switch to Adams at T (=R1), new step (=R2): %g, %g'
            CALL rprintfd2(MSG // char(0), TN, H)
            ENDIF
       
        310  CONTINUE
            IF (ITASK .EQ. 1) THEN
            GOTO 320
            ELSE IF (ITASK .EQ. 2) THEN
            GOTO 400
            ELSE IF (ITASK .EQ. 3) THEN
            GOTO 330
            ELSE IF (ITASK .EQ. 4) THEN
            GOTO 340
            ELSE IF (ITASK .EQ. 5) THEN
            GOTO 350
            ENDIF
CKarline: changed from
CGO TO (320, 400, 330, 340, 350), ITASK
CITASK = 1.  If TOUT has been reached, interpolate. -------------------
        320  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
            CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
            T = TOUT
            GO TO 420
CITASK = 3.  Jump to exit if TOUT was reached. ------------------------
        330  IF ((TN - TOUT)*H .GE. 0.0D0) GO TO 400
            GO TO 250
CITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary.
        340  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 345
            CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
            T = TOUT
            GO TO 420
        345  HMX = ABS(TN) + ABS(H)
            IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
            IF (IHIT) GO TO 400
            TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND)
            IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250
            H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND)
            IF (JSTART .GE. 0) JSTART = -2
            GO TO 250
CITASK = 5.  See if TCRIT was reached and jump to exit. ---------------
        350  HMX = ABS(TN) + ABS(H)
            IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
        C-----------------------------------------------------------------------
CBlock G.
CThe following block handles all successful returns from DLSODA.
CIf ITASK .ne. 1, Y is loaded from YH and T is set accordingly.
CISTATE is set to 2, and the optional outputs are loaded into the
Cwork arrays before returning.
        C-----------------------------------------------------------------------
        400  DO 410 I = 1,N
            Y(I) = RWORK(I+LYH-1)
        410  CONTINUE
            T = TN
            IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 420
            IF (IHIT) T = TCRIT
        420  ISTATE = 2
            RWORK(11) = HU
            RWORK(12) = H
            RWORK(13) = TN
            RWORK(15) = TSW
            IWORK(11) = NST
            IWORK(12) = NFE
            IWORK(13) = NJE
            IWORK(14) = NQU
            IWORK(15) = NQ
            IWORK(19) = MUSED
            IWORK(20) = METH
            RETURN
        C-----------------------------------------------------------------------
CBlock H.
CThe following block handles all unsuccessful returns other than
Cthose for illegal input.  First the error message routine is called.
CIf there was an error test or convergence test failure, IMXER is set.
CThen Y is loaded from YH and T is set to TN.
CThe optional outputs are loaded into the work arrays before returning.
        C-----------------------------------------------------------------------
CThe maximum number of steps was taken before reaching TOUT. ----------
        500  MSG = 'DLSODA-  At current T (=R1), MXSTEP (=I1) steps   '
            CALL XERRWD (MSG, 50, 201, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '      taken on this call before reaching TOUT     '
            CALL XERRWD (MSG, 50, 201, 0, 1, MXSTEP, 0, 1, TN, 0.0D0)
            ISTATE = -1
            GO TO 580
CEWT(i) .le. 0.0 for some i (not at start of problem). ----------------
        510  EWTI = RWORK(LEWT+I-1)
            MSG = 'DLSODA-  At T (=R1), EWT(I1) has become R2 .le. 0.'
            CALL XERRWD (MSG, 50, 202, 0, 1, I, 0, 2, TN, EWTI)
            ISTATE = -6
            GO TO 580
CToo much accuracy requested for machine precision. -------------------
        520  MSG = 'DLSODA-  At T (=R1), too much accuracy requested  '
            CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '      for precision of machine..  See TOLSF (=R2) '
            CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 2, TN, TOLSF)
            RWORK(14) = TOLSF
            ISTATE = -2
            GO TO 580
CKFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. -----
        530  MSG = 'DLSODA-  At T(=R1) and step size H(=R2), the error'
            CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '      test failed repeatedly or with ABS(H) = HMIN'
            CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 2, TN, H)
            ISTATE = -4
            GO TO 560
CKFLAG = -2.  Convergence failed repeatedly or with ABS(H) = HMIN. ----
        540  MSG = 'DLSODA-  At T (=R1) and step size H (=R2), the    '
            CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '      corrector convergence failed repeatedly     '
            CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG = '      or with ABS(H) = HMIN   '
            CALL XERRWD (MSG, 30, 205, 0, 0, 0, 0, 2, TN, H)
            ISTATE = -5
            GO TO 560
CRWORK length too small to proceed. -----------------------------------
        550  MSG = 'DLSODA-  At current T(=R1), RWORK length too small'
            CALL XERRWD (MSG, 50, 206, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG='      to proceed.  The integration was otherwise successful.'
            CALL XERRWD (MSG, 60, 206, 0, 0, 0, 0, 1, TN, 0.0D0)
            ISTATE = -7
            GO TO 580
CIWORK length too small to proceed. -----------------------------------
        555  MSG = 'DLSODA-  At current T(=R1), IWORK length too small'
            CALL XERRWD (MSG, 50, 207, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG='      to proceed.  The integration was otherwise successful.'
            CALL XERRWD (MSG, 60, 207, 0, 0, 0, 0, 1, TN, 0.0D0)
            ISTATE = -7
            GO TO 580
CCompute IMXER if relevant. -------------------------------------------
        560  BIG = 0.0D0
            IMXER = 1
            DO 570 I = 1,N
            SIZE = ABS(RWORK(I+LACOR-1)*RWORK(I+LEWT-1))
            IF (BIG .GE. SIZE) GO TO 570
            BIG = SIZE
            IMXER = I
        570    CONTINUE
            IWORK(16) = IMXER
CSet Y vector, T, and optional outputs. -------------------------------
        580  DO 590 I = 1,N
            Y(I) = RWORK(I+LYH-1)
        590  CONTINUE
            T = TN
            RWORK(11) = HU
            RWORK(12) = H
            RWORK(13) = TN
            RWORK(15) = TSW
            IWORK(11) = NST
            IWORK(12) = NFE
            IWORK(13) = NJE
            IWORK(14) = NQU
            IWORK(15) = NQ
            IWORK(19) = MUSED
            IWORK(20) = METH
            RETURN
        C-----------------------------------------------------------------------
CBlock I.
CThe following block handles all error returns due to illegal input
C(ISTATE = -3), as detected before calling the core integrator.
CFirst the error message routine is called.  If the illegal input
Cis a negative ISTATE, the run is aborted (apparent infinite loop).
        C-----------------------------------------------------------------------
        601  MSG = 'DLSODA-  ISTATE (=I1) illegal.'
            CALL XERRWD (MSG, 30, 1, 0, 1, ISTATE, 0, 0, 0.0D0, 0.0D0)
            IF (ISTATE .LT. 0) GO TO 800
            GO TO 700
        602  MSG = 'DLSODA-  ITASK (=I1) illegal. '
            CALL XERRWD (MSG, 30, 2, 0, 1, ITASK, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        603  MSG = 'DLSODA-  ISTATE .gt. 1 but DLSODA not initialized.'
            CALL XERRWD (MSG, 50, 3, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        604  MSG = 'DLSODA-  NEQ (=I1) .lt. 1     '
            CALL XERRWD (MSG, 30, 4, 0, 1, NEQ(1), 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        605  MSG = 'DLSODA-  ISTATE = 3 and NEQ increased (I1 to I2). '
            CALL XERRWD (MSG, 50, 5, 0, 2, N, NEQ(1), 0, 0.0D0, 0.0D0)
            GO TO 700
        606  MSG = 'DLSODA-  ITOL (=I1) illegal.  '
            CALL XERRWD (MSG, 30, 6, 0, 1, ITOL, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        607  MSG = 'DLSODA-  IOPT (=I1) illegal.  '
            CALL XERRWD (MSG, 30, 7, 0, 1, IOPT, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        608  MSG = 'DLSODA-  JT (=I1) illegal.    '
            CALL XERRWD (MSG, 30, 8, 0, 1, JT, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        609  MSG = 'DLSODA-  ML (=I1) illegal: .lt.0 or .ge.NEQ (=I2) '
            CALL XERRWD (MSG, 50, 9, 0, 2, ML, NEQ(1), 0, 0.0D0, 0.0D0)
            GO TO 700
        610  MSG = 'DLSODA-  MU (=I1) illegal: .lt.0 or .ge.NEQ (=I2) '
            CALL XERRWD (MSG, 50, 10, 0, 2, MU, NEQ(1), 0, 0.0D0, 0.0D0)
            GO TO 700
        611  MSG = 'DLSODA-  IXPR (=I1) illegal.  '
            CALL XERRWD (MSG, 30, 11, 0, 1, IXPR, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        612  MSG = 'DLSODA-  MXSTEP (=I1) .lt. 0  '
            CALL XERRWD (MSG, 30, 12, 0, 1, MXSTEP, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        613  MSG = 'DLSODA-  MXHNIL (=I1) .lt. 0  '
            CALL XERRWD (MSG, 30, 13, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        614  MSG = 'DLSODA-  TOUT (=R1) behind T (=R2)      '
            CALL XERRWD (MSG, 40, 14, 0, 0, 0, 0, 2, TOUT, T)
            MSG = '      Integration direction is given by H0 (=R1)  '
            CALL XERRWD (MSG, 50, 14, 0, 0, 0, 0, 1, H0, 0.0D0)
            GO TO 700
        615  MSG = 'DLSODA-  HMAX (=R1) .lt. 0.0  '
            CALL XERRWD (MSG, 30, 15, 0, 0, 0, 0, 1, HMAX, 0.0D0)
            GO TO 700
        616  MSG = 'DLSODA-  HMIN (=R1) .lt. 0.0  '
            CALL XERRWD (MSG, 30, 16, 0, 0, 0, 0, 1, HMIN, 0.0D0)
            GO TO 700
        617  MSG='DLSODA-  RWORK length needed, LENRW (=I1), exceeds LRW (=I2)'
            CALL XERRWD (MSG, 60, 17, 0, 2, LENRW, LRW, 0, 0.0D0, 0.0D0)
            GO TO 700
        618  MSG='DLSODA-  IWORK length needed, LENIW (=I1), exceeds LIW (=I2)'
            CALL XERRWD (MSG, 60, 18, 0, 2, LENIW, LIW, 0, 0.0D0, 0.0D0)
            GO TO 700
        619  MSG = 'DLSODA-  RTOL(I1) is R1 .lt. 0.0        '
            CALL XERRWD (MSG, 40, 19, 0, 1, I, 0, 1, RTOLI, 0.0D0)
            GO TO 700
        620  MSG = 'DLSODA-  ATOL(I1) is R1 .lt. 0.0        '
            CALL XERRWD (MSG, 40, 20, 0, 1, I, 0, 1, ATOLI, 0.0D0)
            GO TO 700
        621  EWTI = RWORK(LEWT+I-1)
            MSG = 'DLSODA-  EWT(I1) is R1 .le. 0.0         '
            CALL XERRWD (MSG, 40, 21, 0, 1, I, 0, 1, EWTI, 0.0D0)
            GO TO 700
        622  MSG='DLSODA-  TOUT(=R1) too close to T(=R2) to start integration.'
            CALL XERRWD (MSG, 60, 22, 0, 0, 0, 0, 2, TOUT, T)
            GO TO 700
        623  MSG='DLSODA-  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  '
            CALL XERRWD (MSG, 60, 23, 0, 1, ITASK, 0, 2, TOUT, TP)
            GO TO 700
        624  MSG='DLSODA-  ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   '
            CALL XERRWD (MSG, 60, 24, 0, 0, 0, 0, 2, TCRIT, TN)
            GO TO 700
        625  MSG='DLSODA-  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   '
            CALL XERRWD (MSG, 60, 25, 0, 0, 0, 0, 2, TCRIT, TOUT)
            GO TO 700
        626  MSG = 'DLSODA-  At start of problem, too much accuracy   '
            CALL XERRWD (MSG, 50, 26, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
            MSG='      requested for precision of machine..  See TOLSF (=R1) '
            CALL XERRWD (MSG, 60, 26, 0, 0, 0, 0, 1, TOLSF, 0.0D0)
            RWORK(14) = TOLSF
            GO TO 700
        627  MSG = 'DLSODA-  Trouble in DINTDY.  ITASK = I1, TOUT = R1'
            CALL XERRWD (MSG, 50, 27, 0, 1, ITASK, 0, 1, TOUT, 0.0D0)
            GO TO 700
        628  MSG = 'DLSODA-  MXORDN (=I1) .lt. 0  '
            CALL XERRWD (MSG, 30, 28, 0, 1, MXORDN, 0, 0, 0.0D0, 0.0D0)
            GO TO 700
        629  MSG = 'DLSODA-  MXORDS (=I1) .lt. 0  '
            CALL XERRWD (MSG, 30, 29, 0, 1, MXORDS, 0, 0, 0.0D0, 0.0D0)
        C
        700  ISTATE = -3
            RETURN
        C
        800  MSG = 'DLSODA-  Run aborted.. apparent infinite loop.    '
            CALL XERRWD (MSG, 50, 303, 2, 0, 0, 0, 0, 0.0D0, 0.0D0)
            RETURN
        C----------------------- End of Subroutine DLSODA ----------------------
            END