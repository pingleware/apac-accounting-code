      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	 ******     ****    **	       *
      *   **	**     **     **    **	**    **   **  **   **	       *
      *   **	**     **     **    **	**	  **	**  **	       *
      *   **	**     **     *******	 ******	  ********  **	       *
      *   **	**     **     **	      **  **	**  **	       *
      *   **	**     **     **	**    **  **	**  **	       *
      *   *******      **     **	 ******	  **	**  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R ' S   S T O C K   S A L E S   P R O G R A M    *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      *								       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPSAL.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2011
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon (Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.  
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CARDEX.SL.

000120 COPY CONTROL.SL.

000140 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000120 COPY DEPART.SL.

000250 COPY INVOIC.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

000140 COPY STKMEM.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

000120 COPY CONTROL.FDE.

000310 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000420 COPY DEPART.FDE.

000540 COPY INVOIC.FDE.

000290 COPY PARAM.FDE.

000540 COPY STOCK.FDE.

       COPY STKMEM.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-DEPKEY	   PIC	9(06)	 COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
002780 77  WS-LOOK	   PIC	X(01) VALUE "S".
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-ENQ	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-MONTH	   PIC	9(02).
001820 77  WS-ETYPE	   PIC	X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003290	   03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003350	   03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "SAL".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

000140 COPY DEPART.ID.

000140 COPY INVOIC.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

000140 COPY STKMEM.ID.

       COPY W05.VAT.

004160 01  W10-STOCK.
	   03  W10-EXT-ENTRY.
002560	       05  W10-ENTRY	 PIC  X(14).
	       05  FILLER	 PIC  X(04).
	   03  W10-EXT-ITEM.
002560	       05  W10-ITEM.
		   07  W10-IC	 PIC  X(01) OCCURS 14.
	       05  W10-ITEM14 REDEFINES W10-ITEM.
		   07  W10-I14	 PIC  9(14).
	       05  W10-ITEM13 REDEFINES W10-ITEM.
		   07  W10-I13	 PIC  9(13).
		   07  FILLER	 PIC  X(01).
	       05  W10-ITEM12 REDEFINES W10-ITEM.
		   07  W10-I12	 PIC  9(12).
		   07  FILLER	 PIC  X(02).
	       05  W10-ITEM11 REDEFINES W10-ITEM.
		   07  W10-I11	 PIC  9(11).
		   07  FILLER	 PIC  X(03).
	       05  W10-ITEM10 REDEFINES W10-ITEM.
		   07  W10-I10	 PIC  9(10).
		   07  FILLER	 PIC  X(04).
	       05  W10-ITEM9 REDEFINES W10-ITEM.
		   07  W10-I9	 PIC  9(09).
		   07  FILLER	 PIC  X(05).
	       05  W10-ITEM8 REDEFINES W10-ITEM.
		   07  W10-I8	 PIC  9(08).
		   07  FILLER	 PIC  X(06).
	       05  W10-ITEM7 REDEFINES W10-ITEM.
		   07  W10-I7	 PIC  9(07).
		   07  W10-IQ	 PIC  9(02)V999.
		   07  FILLER	 PIC  X(02).
	       05  W10-ITEM6 REDEFINES W10-ITEM.
		   07  W10-I6	 PIC  9(06).
		   07  FILLER	 PIC  X(08).
	       05  W10-ITEM5 REDEFINES W10-ITEM.
		   07  W10-I5	 PIC  9(05).
		   07  FILLER	 PIC  X(09).
	       05  W10-ITEM4 REDEFINES W10-ITEM.
		   07  W10-I4	 PIC  9(04).
		   07  FILLER	 PIC  X(10).
	       05  W10-ITEM3 REDEFINES W10-ITEM.
		   07  W10-I3	 PIC  9(03).
		   07  FILLER	 PIC  X(11).
	       05  W10-ITM	 PIC  X(03).
	       05  FILLER	 PIC  X(01).
	   03  W10-SERNO       PIC  X(10).
004180     03  W10-DESC.
	       05  W10-SDSC    PIC  X(29).
	       05  FILLER      PIC  X(01).
002770	   03  W10-DESC2.
	     04  W10-SDSC2.
	       05  W10-DENT2	 PIC  X(14).
	       05  FILLER	 PIC  X(16).
004190	   03  W10-REMKS       PIC  X(29).
004200	   03  W10-IND	       PIC  X(01).
	   03  W10-LENGTH      PIC  9(04)V9999 COMP-3.
	   03  W10-BREADTH     PIC  9(04)V9999 COMP-3.
004210	   03  W10-QUANT       PIC  9(07)V99  COMP-3.
004220	   03  W10-REORD       PIC  9(07)V99  COMP-3.
004230	   03  W10-COST        PIC  9(06)V999 COMP-3.
002850	   03  W10-SELL        PIC S9(07)V999	COMP-3.
002860	   03  W10-CASH        PIC S9(07)V999	COMP-3.
002860	   03  W10-WSALE       PIC S9(07)V999	COMP-3.
002860	   03  W10-SPEC        PIC S9(07)V999	COMP-3.
004250	   03  W10-SPAPRC      PIC  9(07)V99  COMP-3.
004260	   03  W10-LEDG        PIC  9(04)     COMP-5.
004270	   03  W10-SMAN        PIC  9(02).
004280	   03  W10-DISC        PIC  9(02).
004290	   03  W10-ORDER       PIC  9(07)V99  COMP-3.
004300	   03  W10-DATE        PIC  X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.
004310	   03  W10-AVRG        PIC  9(06)V999 COMP-3.
004320	   03  W10-MARG        PIC  9(05)V99  COMP-3.
004330	   03  W10-ACTIVE      PIC  X(08).
004340	   03  W10-CODE1       PIC  X(14).
004350     03  W10-COD1 REDEFINES W10-CODE1.
004360	       05  W10-CD1     PIC  X(02).
004370	       05  W10-CDE1    PIC  X(12).
004380	   03  W10-CODE2       PIC  X(14).
004390     03  W10-COD2 REDEFINES W10-CODE2.
004400	       05  W10-CD2     PIC  X(02).
004410	       05  W10-CDE2    PIC  X(12).
004420	   03  W10-ACODE       PIC  X(10).
004430	   03  W10-ACDE1 REDEFINES W10-ACODE.
004440	       05  W10-ACRSE   PIC  X(01).
004450	       05  W10-ASFINE  PIC  X(02).
004460	       05  FILLER      PIC  X(07).
004470	   03  W10-VSELL       PIC S9(07)V99  COMP-3.
004480	   03  W10-VCASH       PIC S9(07)V99  COMP-3.
004490	   03  W10-VWSALE      PIC S9(07)V99  COMP-3.
	   03  W10-STCK        PIC  X(01).
	   03  W10-SLNGTH      PIC  X(01)    COMP-X.

       COPY W12.WS.

       COPY W15.WS.

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
	       07  W15-REC     PIC  X(08).
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
	       05  W15-REC1    PIC  X(08).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
	       05  W15-REC2    PIC  X(08).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
	       05  W15-REC3    PIC  X(08).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
	       05  W15-REC4    PIC  X(08).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
	       05  W15-REC5    PIC  X(08).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
	       05  W15-REC6    PIC  X(08).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
	       05  W15-REC7    PIC  X(08).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
	       05  W15-REC8    PIC  X(08).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
	       05  W15-REC9    PIC  X(08).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
	       05  W15-REC10   PIC  X(08).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
	       05  W15-REC11   PIC  X(08).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
	       05  W15-REC12   PIC  X(08).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
	       05  W15-REC13   PIC  X(08).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
	       05  W15-REC14   PIC  X(08).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).
	       05  W15-REC15   PIC  X(08).

       COPY W20.WS.

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.

       COPY W40.DBT.

       01  W40-BALANCES.
           03  W40-A-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-A-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-A-30        PIC S9(09)V99 COMP-3.
003410     03  W40-A-60        PIC S9(09)V99 COMP-3.
003420     03  W40-A-90        PIC S9(09)V99 COMP-3.
003420     03  W40-A-120       PIC S9(09)V99 COMP-3.
003430     03  W40-A-INT       PIC S9(09)V99 COMP-3.   
           03  W40-D-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-D-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-D-30        PIC S9(09)V99 COMP-3.
003410     03  W40-D-60        PIC S9(09)V99 COMP-3.
003420     03  W40-D-90        PIC S9(09)V99 COMP-3.
003420     03  W40-D-120       PIC S9(09)V99 COMP-3.
003430     03  W40-D-INT       PIC S9(09)V99 COMP-3.   
           03  W40-T-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-T-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-T-30        PIC S9(09)V99 COMP-3.
003410     03  W40-T-60        PIC S9(09)V99 COMP-3.
003420     03  W40-T-90        PIC S9(09)V99 COMP-3.
003420     03  W40-T-120       PIC S9(09)V99 COMP-3.
003430     03  W40-T-INT       PIC S9(09)V99 COMP-3.   

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

003530 COPY W50.WS.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-S2	      PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
005860     03  W75-DNO        PIC X(06) OCCURS 18.
      *    03  W75-ITEMS OCCURS 18.
      *        05  W75-CODE   PIC X(14).
      *        05  W75-SELL   PIC 9(07)V99 COMP-3.
      *        05  W75-VSELL  PIC 9(07)V99 COMP-3.
      *        05  W75-CASH   PIC 9(07)V99 COMP-3.
      *        05  W75-VCASH  PIC 9(07)V99 COMP-3.
      *        05  W75-WSALE  PIC 9(07)V99 COMP-3.
      *        05  W75-VWSALE PIC 9(07)V99 COMP-3.
	   03  W75-RNO	      PIC X(08) OCCURS 16.
	   03  W75-DATE       PIC 9(08) OCCURS 16.
	   03  W75-SM	      PIC 9(02) OCCURS 16.

003750 01  W75-STORE.
003760	   03  W75-SNAME      PIC  X(08).
	   03  W75-SREF       PIC  X(08).
	   03  W75-SACN       PIC  X(06).
	   03  W75-SDTE       PIC  9(08).
	   03  W75-SSM	      PIC  9(02).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO        PIC X(06).
003940     03  W80-TYPE        PIC 9(01).

004390 01  W95-STM.
004400	   03  W95-COMP        PIC X(40).
004410	   03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B       PIC Z(06)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.
004470     03  W95-POST        PIC 9(04).
004480     03  W95-TEL         PIC X(08).

004490 01  W100-EDIT.
007020	   03  W100-LBAL       PIC Z(06)9.99-.
007030	   03  W100-LARR       PIC Z(06)9.99.
	   03  W100-COST       PIC Z(06)9.99.
004500	   03  W100-SELL       PIC Z(06)9.99.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.  
004570     03  W100-KEY        PIC Z(05)9.
	   03  W100-SER        PIC ZZ9.
007020	   03  W100-PRC        PIC Z(06)9.99-.
007100	   03  W100-VAL        PIC Z(08)9.99-.
007120     03  W100-V2.
007130         05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170	   03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
	       05  FILLER      PIC  X(02).
	       05  W100-QNT    PIC Z(04)9.99.

       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

009340 01  S05D.
	   03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³   Transactions for Period.   ³".
	   03  LINE  9 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 10 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³  Starting Date :             ³".
	   03  LINE 11 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³  Ending Date   :             ³".
	   03  LINE 12 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 13 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³Dates to be keyed in: ".
	   03	       COLUMN 48 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMCCYY".
	   03	       COLUMN 56 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "³".
	   03  LINE 14 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
	   03  LINE 10 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE 11 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

009340 01  S10D.
009350	   03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
009360		       "SALES TRACKING MOVEMENT/REFERENCE LOOKUP".
	   03  LINE  3 COLUMN  3 VALUE "A/C.".
	   03	       COLUMN 56 VALUE "Item°°°°°°°°°°°°°°°°°°°°".

009930 01  S10.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

       01  S11.
	   03  LINE  6 COLUMN 24 VALUE "Details Required:".
	   03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 25 VALUE "ll Items".
	   03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "S".
	   03	       COLUMN 25 VALUE "pecific Item Code".
	   03  LINE 11 COLUMN 24 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Select ".
	   03	       COLUMN 31 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 32 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 36 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "S".
	   03	       COLUMN 37 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
				 VALUE "- press ".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 48 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
				 VALUE "ape to exit".

007910 01  S36-ITM.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14)/X(03)
				  USING W10-EXT-ITEM AUTO.

007910 01  S36-EXT.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(18) USING W10-EXT-ITEM AUTO.

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W10-ITEM AUTO.

005530 01  S36-3.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S36-4.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S36-5.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S36-6.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S36-7.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S36-8.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S36-9.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S36-10.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S36-11.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S36-12.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S36-13.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S36-14.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

       COPY DEBLUP.CRT.

       COPY STKLUP.CRT.

009340 01  S-MOV.
009390	   03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
009400	       "Ref. No.    Date    Supply Cost Price Sell.Price Quant
009410-        "ity     Item Code      T".
009160	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍ
      -        "ÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÏÍÍÍÍÍ
      -        "ÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

009530 01  SA-ACT.
009540	   03  LINE  3 COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009550				 PIC  X(06) USING W80-ACNO AUTO.

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270       PERFORM ZA000-INIT.
	     PERFORM BA000.
	     CLOSE CARDEX
		   STOCK.
       AA49.
013150       EXIT PROGRAM.

       COPY AA50DS.LUP.

       COPY FUNCTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY DATE.CHK.

       COPY AA300.STK.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

       COPY CARDEX.RD.

011670 COPY CONTROL.RD.

011710 COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

000140 COPY DEPART.RD.

011750 COPY PARAM.RD.

       COPY INVOIC.RD.

       COPY STOCK.RD.

000140 COPY STKMEM.RD.

      /
      *    ****    M O V E M E N T   L O O K U P
      *
000010 MOVEMENT-LOOKUP		 SECTION.

000030 MOVE-LUP-SCREEN.
000050	     DISPLAY S-MOV.
	     INITIALIZE CRD-REC1.
	     MOVE W10-ITEM	 TO CRD-CODE.
	     MOVE W80-ACNO	 TO CRD-AC.

       MOVE-LUP-CLR-SCREEN.
000060       MOVE 0601           TO CPOS.
000080	     PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
000180	     MOVE 6		 TO CLIN.
000200       MOVE 1              TO W75-S W75-S1.

       MOVE-LUP-TAB-LOOP.
	     MOVE SPACES	 TO W75-DNO (W75-S1)
				    W75-RNO (W75-S1).
	     MOVE ZERO		 TO W75-DATE (W75-S1)
				    W75-SM (W75-S1).
	   IF W75-S1 < 16
               ADD 1             TO W75-S1
	       GO TO MOVE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       MOVE-LUP-START-CRDX.
	     MOVE SPACES	 TO WS-OPTION.
000750	     PERFORM START-AT-CRDX-KEY THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO MOVE-NO-RECORD.

000810 MOVE-GET-CARDEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO MOVE-SKIP-CRDX-READ.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
               SUBTRACT 1        FROM W75-S
	       GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
	   IF NOT (CRD-CODE = STK-CODE)
	       MOVE CRD-CODE	 TO STK-CODE
	       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF NOT (CRD-AC = DEB-ACNO)
	       GO TO MOVE-FULL-SCREEN.
	   IF (WS-ENQ = "S") AND
	      (W10-ITEM NOT = CRD-CODE)
	       GO TO MOVE-FULL-SCREEN.

       MOVE-SKIP-CRDX-READ.
000890	     DISPLAY CRD-REF AT LINE CLIN COLUMN 2
		     WITH FOREGROUND-COLOR 11.
	     MOVE CRD-DTE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W95-DTE.
000900	     DISPLAY W95-DTE AT LINE CLIN COLUMN 11
		     WITH FOREGROUND-COLOR 11.
	   IF CRD-SUPP = "-LAB-"
	       DISPLAY CRD-SUPP AT LINE CLIN COLUMN 22
		       WITH FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
000920	       DISPLAY CRD-SUPP AT LINE CLIN COLUMN 22
		       WITH FOREGROUND-COLOR 11.
000940*	     DISPLAY CRD-SMAN AT LINE CLIN COLUMN 28
		     WITH FOREGROUND-COLOR 11.
	     MOVE CRD-COST	 TO W100-COST.
	     MOVE CRD-SELL	 TO W100-SELL.
	     MOVE CRD-QNT	 TO W100-QNT.
000960	     DISPLAY W100-COST AT LINE CLIN COLUMN 29
		     WITH FOREGROUND-COLOR 11.
	     DISPLAY W100-SELL AT LINE CLIN COLUMN 40
		     WITH FOREGROUND-COLOR 11.
	     DISPLAY W100-QNT AT LINE CLIN COLUMN 51
		     WITH FOREGROUND-COLOR 11.
	     DISPLAY STK-CODE AT LINE CLIN COLUMN 60
		     WITH FOREGROUND-COLOR 11.
	   IF CRD-SELL < ZERO
	       DISPLAY "C" AT LINE CLIN COLUMN 79
			WITH FOREGROUND-COLOR 11
	   ELSE
	       DISPLAY "I" AT LINE CLIN COLUMN 79
			WITH FOREGROUND-COLOR 11.
000970     IF CLIN < 21
000980         ADD 1             TO CLIN W75-S
000990	       GO TO MOVE-GET-CARDEX-REC.
001000
001010 MOVE-FULL-SCREEN.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
001030		     " - " WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     " - " WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape." WITH FOREGROUND-COLOR 7.

       MOVE-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
001140	     MOVE 0		 TO W75-S1.
001150       MOVE 0601           TO CPOS.

001180 MOVE-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001220	     MOVE 2000		 TO W41-LENGTH.

001240 MOVE-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO MOVE-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO MOVE-CHK-NO-KEYS
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO MOVE-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO MOVEMENT-LOOKUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO MOVE-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO MOVE-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO MOVE-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO MOVE-GET-USER-OPT.

001240 MOVE-GET-NEXT-PAGE.
001480     IF W75-S < 16
001490	       GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
001500       MOVE 0601           TO CPOS.
001520	     PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
001530       MOVE 6              TO CLIN.
001540       MOVE LOW-VALUES     TO W75-KEYS.
001550       MOVE 1              TO W75-S W75-S1.
	     PERFORM MOVE-LUP-TAB-LOOP.
001560	     GO TO MOVE-GET-CARDEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
	      MOVE "J"		  TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
	     PERFORM READ-CARDEX-PREV THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       MOVE SPACE	 TO WS-OPTION
	       GO TO MOVE-NEW-PAGE.
	   IF NOT (CRD-AC = DEB-ACNO)
	       PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT
	       GO TO MOVE-NEW-PAGE.
	   IF CRD-DTE < W15-START
	       PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT
	       GO TO MOVE-NEW-PAGE.
	   IF WS-IXS1 < 32
	       ADD 1		 TO WS-IXS1
	       GO TO MOVE-GET-PREV-LOOP.
	     GO TO MOVE-NEW-PAGE.

001580 MOVE-NO-RECORD.
	     DISPLAY CLEAR-L25.
001590	     DISPLAY "No record - Press " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001600		     "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001610		     " key"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
001770	     GO TO MOVEMENT-LOOKUP-EXIT.

001790 MOVE-ADJ-NO-KEYS.
001800     IF W75-S1 > 1
001810         SUBTRACT 1        FROM W75-S1 CLIN.
001820	     GO TO MOVE-HIGHLIGHT.

001840 MOVE-CHK-NO-KEYS.
001850     IF W75-S1 = ZERO
001860         MOVE 1            TO W75-S1
001870	       GO TO MOVE-HIGHLIGHT.
001880     IF W75-S1 < W75-S
001890         ADD 1             TO W75-S1 CLIN.

001910 MOVE-HIGHLIGHT.
001920	     PERFORM MOVE-CRT-ATTRIB.
001930       DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
001940	     GO TO MOVE-GET-USER-OPT.

002070 MOVE-CRT-EDGE.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     "        ³          ³      ³          ³          ³
      - 	     "       ³                  ³ "
		      WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
002090	     ADD 1		 TO CLIN.


002110 MOVEMENT-LOOKUP-EXIT.
002120       EXIT.

011630 COPY DEBTOR.ILU.

       COPY APAC.HLP.

      /
      *  ****  E N Q U I R Y   ( S A L E S   T R A C K I N G )
      *
021000 BA000		       SECTION 50.
021010 BA00.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY S10D.

       BA05.
018790	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018800		     " to do Debtor Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
018810	     ACCEPT S10 AT 0310.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO BA10
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50D THRU AA59
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE
018840	       DISPLAY S10 AT 0310
018850         IF W80-ACNO = SPACES
018860		   GO TO BA05.

       BA10.
018870	     DISPLAY CLEAR-L25.
018880	   IF W80-ACNO = SPACES
018900	       GO TO BA999.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

018910 BA15.
018920	     DISPLAY CLEAR-L25.
018930       PERFORM CA155-GET-DEBTOR.
018940     IF WS-ERROR = 1
018950	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
018960	       GO TO BA05.
018970     IF WS-ERROR = 2
018980	       MOVE "Invalid account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019000	       GO TO BA05.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
019020	     MOVE DEB-NAME	 TO W40-NAME.
019030     IF W80-TYPE = 1
019040         INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 0317
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY S11.

       BA20.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
018870		     DISPLAY CLEAR-L25
		     GO TO BA05
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA20
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF NOT (WS-OPTION = "A" OR "S")
		   CALL X"E5"
		   GO TO BA20
	       END-IF
	   ELSE
	       CALL X"E5"
	       GO TO BA20.
	     MOVE 0401	  TO CPOS.
020430       PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
	     MOVE SPACES	 TO W10-ITEM.
	   IF W10-STCK = "N"
	       IF W10-SLNGTH = 3
		   MOVE ZERO	 TO W10-I3
	       ELSE
	       IF W10-SLNGTH = 4
		   MOVE ZERO	 TO W10-I4
	       ELSE
	       IF W10-SLNGTH = 5
		   MOVE ZERO	 TO W10-I5
	       ELSE
	       IF W10-SLNGTH = 6
		   MOVE ZERO	 TO W10-I6
	       ELSE
	       IF W10-SLNGTH = 7
		   MOVE ZERO	 TO W10-I7
	       ELSE
	       IF W10-SLNGTH = 8
		   MOVE ZERO	 TO W10-I8
	       ELSE
	       IF W10-SLNGTH = 9
		   MOVE ZERO	 TO W10-I9
	       ELSE
	       IF W10-SLNGTH = 10
		   MOVE ZERO	 TO W10-I10
	       ELSE
	       IF W10-SLNGTH = 11
		   MOVE ZERO	 TO W10-I11
	       ELSE
	       IF W10-SLNGTH = 12
		   MOVE ZERO	 TO W10-I12
	       ELSE
	       IF W10-SLNGTH = 13
		   MOVE ZERO	 TO W10-I13
	       ELSE
		   MOVE ZERO	 TO W10-I14.
	     MOVE WS-OPTION	 TO WS-ENQ.
	     MOVE W12-MONTH	 TO WS-MONTH.
	   IF WS-ENQ = "A"
	       GO TO BA40.

       BA25.
020960	     PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   ACCEPT S36-EXT AT 0361
	       ELSE
	       IF WS-USE-ITM = "Y"
		   ACCEPT S36-ITM AT 0361
	       ELSE
021070		   ACCEPT S36 AT 0361
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S36-3 AT 0361
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S36-4 AT 0361
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S36-5 AT 0361
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S36-6 AT 0361
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S36-7 AT 0361
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S36-8 AT 0361
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S36-9 AT 0361
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S36-10 AT 0361
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S36-11 AT 0361
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S36-12 AT 0361
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S36-13 AT 0361
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S36-14 AT 0361.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-ITEM
018870		     DISPLAY CLEAR-L25
		     GO TO BA05
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "X"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA25
	       END-EVALUATE
	       MOVE "S" 	 TO WS-LOOK
016560	       PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S36-EXT AT 0361
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S36-ITM AT 0361
		   ELSE
		       DISPLAY S36 AT 0361
		   END-IF
	       ELSE
		   DISPLAY S36 AT 0361.

       BA30.
021040	   IF W10-ITEM = SPACES
021060	       GO TO BA25.

       BA35.
021070       PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   DISPLAY S36-EXT AT 0361
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S36-ITM AT 0361
	       ELSE
018630		   DISPLAY S36 AT 0361
	       END-IF
	   ELSE
	       DISPLAY S36 AT 0361.
021080	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S36-EXT AT 0361
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S36-ITM AT 0361
		   ELSE
		       DISPLAY S36 AT 0361
		   END-IF
	       ELSE
		   DISPLAY S36 AT 0361
	       END-IF
018640	       IF W10-ITEM = SPACES
018720		   GO TO BA25
               ELSE
		   GO TO BA35.
	     DISPLAY "Item Description" AT 0612.
	     DISPLAY STK-DESC AT 0629
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA25.

       BA40.
021020	     DISPLAY SAVE-SCREEN.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-TODAY	 TO W15-START W15-END.

       BA40A.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S05A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BA40A
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA40A.
	     DISPLAY CLEAR-L25.
	     MOVE W15-START	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA40A.
             MOVE W20-DTE        TO W15-S-YMD.
	     MOVE W15-END	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA40A.
             MOVE W20-DTE        TO W15-E-YMD.
	   IF W15-E-YMD < W15-S-YMD
	       MOVE "End date is before start date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA40A.
	     MOVE W15-SMM	 TO WS-CRDX.
	   IF NOT (W05-CID (WS-CRDX) = W02-MTH43)
	       CLOSE CARDEX
	       MOVE W05-CID (WS-CRDX)
				 TO W02-MTH43
	       OPEN I-O CARDEX
	       IF WS-STATUS = "05"
		   CLOSE CARDEX
		   OPEN OUTPUT CARDEX
		   CLOSE CARDEX
		   OPEN I-O CARDEX.
	     PERFORM MOVEMENT-LOOKUP.
	     CLOSE CARDEX.
	     MOVE W05-CID (W12-MONTH)
				 TO W02-MTH43.
	     OPEN I-O CARDEX.
	     MOVE SPACES	 TO W80-ACNO.
	     GO TO BA00.

021720 BA999.
021730       EXIT.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
013300 CA000	     SECTION 50.
013310 CA00.
013320       MOVE 14             TO W50-LENGTH.
013330 CA05.
013340       MOVE W10-ITEM       TO W50-DATA.
013350     IF W50-1 = SPACE
013360         MOVE W50-13       TO W10-ITEM
013370         GO TO CA05.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013430	     MOVE W50-ITEM	 TO W10-ITEM.
013440 CA06.
013450       MOVE W50-ITEM       TO STK-CODE.
013460 CA10.
013470       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
013480     IF WS-F-ERROR = 0
013490         PERFORM CA500.
013500 CA99.
013510       EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   
      *           A L T E R N A T E   C O D E
      *
013520 CA100	     SECTION 50.
013530 CA101.
013540       MOVE 10             TO W50-LENGTH.
013550 CA105.
013560       MOVE W10-ACODE      TO W50-DATA.
013570     IF W50-1 = SPACE
013580         MOVE W50-9        TO W10-ACODE
013590         GO TO CA105.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013650	     MOVE W50-ACODE	 TO STK-ACODE W10-ACODE.
013660 CA110.
013670       PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
013680     IF WS-F-ERROR = 22
013690         GO TO CA119.
013700       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
013710     IF WS-F-ERROR = 0
013720         PERFORM CA500.
013730 CA119.
013740       EXIT.
      *
      *    ****    Read debtor record - using account number.
      *
013610 CA155-GET-DEBTOR  SECTION 50.
013620 CA155.
013630       MOVE W80-ACNO       TO DEB-ACNO.
013640       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
013650     IF WS-F-ERROR = 6
013660         MOVE 1            TO WS-ERROR
013670     ELSE
013680     IF DEB-ACNO NOT = W80-ACNO
013690         MOVE 2            TO WS-ERROR
013700     ELSE
013710         MOVE 0            TO WS-ERROR
013720         MOVE DEB-TYPE     TO W80-TYPE.
013730 CA160-EXIT.
013740       EXIT.
      *
013750 CA165-NAME	 SECTION 50.
013760 CA165.
013770       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013780                           INTO W40-SURNAME 
013790                                W40-FNAME 
013800                                W40-SNAME
013810       OVERFLOW
013820           MOVE 1          TO WS-ERROR.
013830 CA170-EXIT.
013840       EXIT.
      *
014530 CA175-ADDRESS	 SECTION 50.
014540 CA175.
014550       UNSTRING W60-ADDRESS DELIMITED BY "*"
014560				 INTO W40-ADD1
				      W40-ADD2
				      W40-ADD3
				      W40-ADD4
014570       OVERFLOW
014580           MOVE 1          TO WS-ERROR.
014590 CA175-EXIT.
014600       EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
014260 CA200	     SECTION 50.
014270 CA201.
014280       MOVE 14             TO W50-LENGTH.
014290 CA205.
014300       MOVE W10-CODE2      TO W50-DATA.
014310     IF W50-1 = SPACE
014320         MOVE W50-13       TO W10-CODE2
014330         GO TO CA205.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
014390	     MOVE W50-ITEM	 TO W10-CODE2.
014400 CA206.
014410       MOVE W50-ITEM       TO STK-XREF.
014420 CA210.
014430       PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
014440     IF WS-F-ERROR = 22
014450         GO TO CA299.
014460       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
014470       PERFORM CA500.
014480 CA299.
014490       EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
014500 CA500	     SECTION 50.
014510 CA505.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
014520	   IF STK-WSALE = ZERO
014530         IF STK-CASH NOT = ZERO
014540             MOVE STK-CASH TO STK-WSALE
014550         ELSE
014560             MOVE STK-SELL TO STK-WSALE.
014570     IF STK-CASH = ZERO
014580         MOVE STK-SELL     TO STK-CASH.
014590       MOVE STK-CASH       TO W10-VCASH
014600       MOVE STK-SELL       TO W10-VSELL
014610       MOVE STK-WSALE      TO W10-VWSALE.
014620       MOVE ZERO           TO W05-RATE.
014700     IF STK-TAX > 0
014710	       MOVE W05-VAT (WS-VAT-SUB)
014720                           TO W05-RATE.
014730       COMPUTE W10-VCASH ROUNDED 
014740             = W10-VCASH + (W10-VCASH * W05-RTE).
014750       COMPUTE W10-VWSALE ROUNDED 
014760             = W10-VWSALE + (W10-VWSALE * W05-RTE).
014770       COMPUTE W10-VSELL ROUNDED 
014780             = W10-VSELL + (W10-VSELL * W05-RTE).
014790 CA599.
014800       EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 80.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
042080	     MOVE 3		 TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
      *	     MOVE PAR-CID (W12-MONTH)
      *				 TO W02-MTH43.
      *	     MOVE "123456789ABC" TO W05-CRDX-ID.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042170 ZA02.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
054710	     MOVE 5		 TO WS-PARKEY.
054720       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054740       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
      *       DISPLAY "Waiting for printer response" AT 1412
      * 	       WITH FOREGROUND-COLOR 14 BLINK.
042780*       OPEN OUTPUT PRNREP.
053830	     OPEN I-O CARDEX.
	   IF WS-STATUS = "05"
	       CLOSE CARDEX
	       OPEN OUTPUT CARDEX
	       CLOSE CARDEX
	       OPEN I-O CARDEX.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT OR
		 (WS-STATUS = "39")
		   MOVE 43	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 43		 TO WS-F-ERROR
053920         PERFORM OPEN-ERROR.
044300	     OPEN I-O STOCK.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
044330     IF WS-STATUS NOT = "00"
044340         MOVE 22           TO WS-F-ERROR
050210         PERFORM OPEN-ERROR.
044780	     GO TO ZA999-EXIT.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
039850 ZA50.
041850	   IF WS-F-ERROR = 22
041860	       MOVE "STOCK File"
				 TO WS-FILE
           ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042800 ZA200.
042810       DISPLAY "Files locked - Try later" AT 2312
042820                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042830       DISPLAY "Press any key" AT 2512 
042840		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 81.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
056420	   ELSE
056430     IF WS-F-ERROR = 7
056440         MOVE W02-DEPART TO WS-FILE
056450         MOVE WS-DEPKEY TO WS-KEY
056460	   ELSE
056470     IF WS-F-ERROR = 12
056480         MOVE W02-INVOIC   TO WS-FILE
056490         MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX.
041880	   IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
