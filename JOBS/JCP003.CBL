      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      *   ********   ******   *******	   ****      ****     ******   *
      *      **     **    **  **    **    **  **    **  **   **    **  *
      *      **     **        **    **   **    **  **    **        **  *
      *      **     **        *******    **    **  **    **     ****   *
      *      **     **        **         **    **  **    **        **  *
      *   ** **     **    **  **          **  **    **  **   **    **  *
      *    ***       ******   **           ****      ****     ******   *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *    J O B   L A B O U R / R E M A R K S	 M A I T E N A N C E   *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  September 2005 - Details of JOBS introduced. New sort key     *
      * 		  JOB-DETAILS = JOB-TYPE JOB-EXT-ITEM JOB-REF  *
      * 							       *
      *  May 2006	- New sort key (Lookup from Stock Item)        *
      * 		  JOB-CODE = JOB-EXT-ITEM JOB-REF	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCP003.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2008
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000130 COPY CONTROL.SL.

000170 COPY DEBTOR.ISL.

000190 COPY DEPART.SL.

000270 COPY JOBCST.SL.

000270 COPY JOBHDR.SL.

000210 COPY PARAM.SL.

000230 COPY RECOVER.SL.

000250 COPY SALE.SL.

       COPY SHARED.SL.

000290 COPY SPARTS.SL.

000140 COPY STKMEM.SL.

000310 COPY STOCK.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000400 COPY DEBTOR.FDE.

000420 COPY DEPART.FDE.

000500 COPY JOBCST.FDE.

000500 COPY JOBHDR.FDE.

000440 COPY PARAM.FDE.

000460 COPY RECOVER.GFD.

000480 COPY SALE.JFD.

       COPY SHARED.FDE.

000520 COPY SPARTS.FDE.

000140 COPY STKMEM.FDE.

000540 COPY STOCK.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000570 77  WS-DISC	   PIC	99V99	 COMP-3.
000580 77  WS-TDISC        PIC  99V99    COMP-3.
000590 77  WS-CASH-AC      PIC  X(06).
000600 77  WS-SUB          PIC  9(04)    COMP-5.
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
000610 77  WS-P1	   PIC	9(04)	 COMP-5.
000610 77  WS-S1           PIC  9(04)    COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
000650 77  WS-S5           PIC  9(04)    COMP-5.
000660 77  WS-S6           PIC  9(04)    COMP-5.
000670 77  WS-S7           PIC  9(04)    COMP-5.
000680 77  WS-S8           PIC  9(04)    COMP-5.
000690 77  WS-S9           PIC  9(04)    COMP-5.
000700 77  WS-IXS          PIC  9(04)    COMP-5.
000710 77  WS-IXS1         PIC  9(04)    COMP-5.
000720 77  WS-IXS2         PIC  9(04)    COMP-5.
000730 77  WS-IXS3         PIC  9(04)    COMP-5.
000740 77  WS-IXS4         PIC  9(04)    COMP-5.
000750 77  WS-PARKEY       PIC  9(04)    COMP-5.
000780 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
000800 77  WS-SALSTR	   PIC	9(04)	 COMP-5.
000810 77  WS-SALKEY       PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
000820 77  WS-NETKEY       PIC  9(04)    COMP-5.
000830 77  WS-AUDKEY       PIC  9(06)    COMP-5.
000840 77  WS-RECKEY       PIC  9(06)    COMP-5.
000850 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000820 77  WS-PARTS	   PIC	X(01).
000820 77  WS-STOCK	   PIC	X(01).
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000870 77  WS-PRN1	   PIC	9(01) VALUE 0.
000880 77  WS-PRN2         PIC  9(01) VALUE 0.  
000900 77  WS-START	   PIC	9(08).
000910 77  WS-END          PIC  9(08).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
000930 77  WS-TME2	   PIC	9(08).
000940 77  WS-PORT         PIC  9(05) VALUE 1004.
000950 77  WS-OPEN         PIC  X(01) VALUE X"01".
000960 77  WS-CLOSE        PIC  X(01) VALUE X"00".
000970 77  WS-PAR          PIC  9(01) VALUE 0.
000980 77  WS-INO	   PIC	9(03) VALUE 0.
000990 77  WS-LINES        PIC  9(02) VALUE 0.
001000 77  WS-FIRST        PIC  9(02) VALUE 0.
       77  WS-SALPOS	   PIC	9(03) VALUE 0.
001010 77  WS-LAST         PIC  9(02) VALUE 0.
001020 77  WS-LASTINV      PIC  9(04)    COMP-5.
001030 77  WS-LINE         PIC  9(04)    COMP-5.
001040 77  WS-PAGE         PIC  9(02)    COMP-5.
001050 77  WS-L1           PIC  9(04)    COMP-5.
001060 77  WS-OPTION       PIC  X(01).
001060 77  WS-IN-OPT       PIC  X(01).
001070 77  WS-MAINT        PIC  X(01).
001070 77  WS-TME          PIC  X(01).
001070 77  WS-SKIP         PIC  X(01) VALUE " ".
001080 77  WS-LOOK         PIC  X(01) VALUE "S".
001090 77  WS-ETYPE        PIC  X(01) VALUE "I".
001120 77  WS-ERROR        PIC  9(01).
001160 77  WS-COMP	   PIC	9(01).
001170 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
001180 77  WS-CONREC       PIC  9(01) VALUE ZERO.
001190 77  WS-HEAD         PIC  X(01).
001200 77  WS-TYPE         PIC  X(01).
001210 77  WS-AGE          PIC  X(01).
001220 77  WS-PRC          PIC  X(01).
001250 77  WS-BATCH	   PIC	9(05).
001280 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
001290 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
001300 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
001310 77  WS-ERR2         PIC  X(09) VALUE "No Record".
001320 77  WS-PER          PIC  Z(02)9.99.
001330 77  WS-CR           PIC  X(01).
       77  WS-PQUANT	   PIC S9(05)V99 COMP-3.
001340 77  WS-TAXRES	   PIC S9(07)V9(08) COMP-3.
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
001350 01  WS-CRT-LINES.
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
001400     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001440	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MJOB REDEFINES WS-MID-LINE.
	       05  WS-JOB1     PIC  X(01).
	       05  WS-JOB78    PIC  X(78).
	       05  WS-JOB80    PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
001500	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
	   03  WS-MM-LINE.
	       05  FILLER    PIC  X(01) VALUE "³".
	       05  WS-SPACE  PIC  X(60) VALUE SPACES.
	       05  FILLER    PIC  X(01) VALUE "³".
	   03  WS-HME-LINE   PIC  X(60) VALUE ALL "Í".
001590	   03  WS-MID-LNE.
001600	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
001650	   03  WS-MD-LNE.
001660	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
001710	   03  WS-MD-LINE.
001720	       05  WS-MCA   PIC  X(80) VALUE "³        ³      ³
      -        "                     ³          ³        ³           ³".
	   03  WS-JB78-L REDEFINES WS-MD-LINE.
	       05  WS-MJB1     PIC  X(01).
	       05  WS-MJB78    PIC  X(78).
	       05  WS-MJB80    PIC  X(01).
001710	   03  WS-MD-LIN.
001720	       05  WS-MCB   PIC  X(80) VALUE "³   ³        ³
      -        "³                               ³         ³          ³".
	   03  WS-RQ78-L REDEFINES WS-MD-LIN.
	       05  WS-RQ1      PIC  X(01).
	       05  WS-RQ78     PIC  X(78).
	       05  WS-RQ80     PIC  X(01).

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JCP".
	   03  WS-PROG	       PIC  X(03) VALUE "003".

001900 01  W00-TRAN-CODES.
001910     03  W00-T-VAT   PIC S9(09)V99 COMP-3.
001920     03  W00-E-DESC  PIC  X(12).
001930     03  W00-A-DESC  PIC  X(12).
001940     03  W00-ACTION  PIC  9(01).
001950     03  W00-T-CODE  PIC  9(02)    COMP-5.
001960     03  W00-T-GL    PIC  9(06)    COMP-3.
001970     03  W00-T-DAY   PIC  9(06)    COMP-5.
001980     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
001990     03  W00-T-MTD   PIC  9(06)    COMP-5.
002000     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
002010     03  W00-T-YTD   PIC  9(06)    COMP-5.
002020     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

002060 COPY CONTROL.ID.

002100 COPY DEBTOR.ID.

002120 COPY DEPART.ID.

002180 COPY JOBCST.ID.

002180 COPY JOBHDR.ID.

       COPY PARAM.ID.

002140 COPY RECOVER.ID.

002160 COPY SALE.ID.

       COPY SHARED.ID.

002200 COPY SPARTS.ID.

000140 COPY STKMEM.ID.

002220 COPY STOCK.ID.

       COPY W05.VAT.

002380 01  W05-INV.
002390     03  W05-ENTRY       PIC  ZZ9.
002400     03  FILLER          PIC  X(01) VALUE SPACES.
002410     03  W05-DATE        PIC  Z9/99/99.
002420     03  FILLER          PIC  X(01) VALUE SPACES.
002430     03  W05-TYPE        PIC  X(03).
002440     03  FILLER          PIC  X(03) VALUE SPACES.
002450     03  W05-REF         PIC  9(07).
002460     03  FILLER          PIC  X(01) VALUE SPACES.
002470     03  W05-VALUE       PIC  Z(06)9.99-.

       COPY W10.JCP.

       01  W10-JOB-TYPE.
           03  W10-KEY.
	       05  W10-JREM   PIC  X(06).
           03  W10-REMK1      PIC  X(30).
           03  W10-REMK2      PIC  X(30).
           03  W10-REMK3      PIC  X(30).
           03  W10-REMK4      PIC  X(30).

       COPY W12.WS.

003390 01  W14-JOB.
003400     03  W14-REF         PIC Z(08).
003410     03  W14-AC          PIC X(06).
003420     03  W14-NAME.
003430	       05  W14-NME     PIC X(30).
003440	       05  FILLER      PIC X(10).
003450	   03  W14-DATE        PIC Z9/99/9999.
003470     03  W14-DOC         PIC X(01).
003480     03  W14-DDOC.
003490         05  W14-INV     PIC Z(08).
003500     03  W14-VALUE       PIC Z(07)9.99.
           03  W14-STATUS      PIC X(01).

003510 01  W14-DELETE.
003520     03  W14-SUB         PIC 9(03)     COMP-3.
003530     03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.

003540 01  W15-DISCOUNT.
003550     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).

       COPY W20.WS.

003650 01  W25-CALCS.
003660     03  W25-RESULT      PIC 9(02)V99.
003670     03  W25-RESULT1 REDEFINES W25-RESULT.
003680         05  W25-WHOLE   PIC 9(02).
003690         05  W25-DECIMAL PIC 9(02).
003700     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
003710     03  W25-VALUE       PIC S9(07)V99 COMP-3.
003720     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

       01  W35-DETKEY.
           03  W35-KEY.
000050       05  W35-REF       PIC  X(08).
             05  W35-SEQN.
                 07  W35-SEQ   PIC  9(03).
                 07  W35-SUB   PIC  9(02).
             05  W35-SQN REDEFINES W35-SEQN
                               PIC  9(05).

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

004560 01  W60-NME-ADD.
004570     03  W60-NAME.
004580         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

004740 COPY W70DEBT.IWS.

004760 01  W75-KEYS.
004770     03  W75-S          PIC 9(02) COMP-5.
004780     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
004790     03  W75-DNO        PIC X(06) OCCURS 18.
	   03  W75-JOBS.
	       05  W75-JCOST OCCURS 18.
004810		   07  W75-JCST	 PIC  9(08).
		   07  W75-IDESC PIC  X(30).
           03  W75-TYPES REDEFINES W75-JOBS.
	       05  W75-JREM   PIC  X(06) OCCURS 18.
	       05  FILLER     PIC  X(576).

004820 01  W75-TITLES.
004830     03  W75-DESCRIP.
004840         05  FILLER      PIC X(28)      VALUE
004850                     "Mr     Mnr    Mrs    Mev    ".
004860         05  FILLER      PIC X(28)      VALUE
004870                     "Miss   Mej    Ms     Ms     ".
004880         05  FILLER      PIC X(28)      VALUE
004890                     "Dr     Dr     Rev    Ds     ".
004900         05  FILLER      PIC X(28)      VALUE
004910                     "Prof   Prof   The HonSy Edel".
004920         05  FILLER      PIC X(28)      VALUE
004930                     "                            ".
004940     03  W75-CODES REDEFINES W75-DESCRIP.
004950         05  W75-TITLE  OCCURS 10.
004960             07  W75-ETITLE PIC X(07).
004970             07  W75-ATITLE PIC X(07).

004980 01  W80-ACCOUNT.
004990     03  W80-ACNO        PIC X(06).
005000     03  W80-CRED REDEFINES W80-ACNO.
005010         05  W80-CNO     PIC 9(05).
005020         05  W80-CAC REDEFINES W80-CNO.
005030             07  W80-CTYPE PIC 9(01).
005040             07  W80-SER   PIC 9(04).
005050         05  FILLER        PIC 9(01).
005060     03  W80-TYPE        PIC 9(01).

005070 01  W85-PASS.
005080     03  W85-SUPER       PIC  X(06)    OCCURS 9.
005090     03  W85-ENTRY       PIC  9(02)    COMP-3.
005100     03  W85-MARG        PIC S9(02)V99.
005110 01  W85-BATCHES.
005120         05  W85-BATCH   PIC  9(05).

       COPY W90.JCP.

005680 01  W95-STM.
005690     03  W95-COMP        PIC X(40).
005700     03  W95-ADD1        PIC X(30).
005710     03  W95-ADD2        PIC X(30).
005720     03  W95-ADD3        PIC X(30).
005730     03  W95-ADD4        PIC X(30).
005740     03  W95-PC.
005750         05  W95-POST    PIC 9(04).
005760     03  W95-TEL         PIC X(14).
005770     03  W95-V1.
005780         05  W95-T       PIC Z(04)9.99.
005790     03  W95-V2.
005800         05  W95-B       PIC Z(06)9.99-.
005810     03  W95-D.
005820	       05  W95-DTE     PIC Z9/99/9999.

005830 01  W100-EDIT.
005840     03  W100-LBAL       PIC Z(06)9.99-.
005850     03  W100-LARR       PIC Z(06)9.99.
005860     03  W100-BAL        PIC Z(08)9.99-.
005870     03  W100-CUR        PIC Z(08)9.99-.
005880     03  W100-30         PIC Z(08)9.99-.
005890     03  W100-60         PIC Z(08)9.99-.
005900     03  W100-90         PIC Z(08)9.99-.
005910     03  W100-120        PIC Z(08)9.99-.
005920     03  W100-INT        PIC Z(08)9.99-.
005930     03  W100-CRL        PIC Z(06)9.
005940     03  W100-V2.
005950         05  W100-S6V2   PIC Z(05)9.99-.
005960     03  W100-V3 REDEFINES W100-V2.
005970         05  W100-PRICE  PIC X(09).
005980         05  FILLER      PIC X(01).
005990     03  W100-V4 REDEFINES W100-V2.
006000         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
	       05  W100-QNT    PIC Z(08)9-.

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
006010 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY JOBLUP.CRT.

       COPY JRMLUP.CRT.

       COPY DEBLUP.CRT.

007910 01  S07.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  S07-EXT.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  S07-3.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S07-4.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S07-5.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S07-6.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S07-7.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S07-8.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S07-9.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S07-10.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S07-11.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S07-12.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S07-13.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S07-14.
007920	   03	       COLUMN  6 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(14) USING W10-I14 AUTO.

009840 01  S26-QNT.
009850	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009860                           PIC Z(04)9.99 USING W10-QUANT AUTO.
009870 01  S26-PRC.
009880	   03	       COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009890                           PIC Z(05)9.99 USING W10-SELL AUTO.
009900 01  S26-DSC.
009910	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009920                           PIC Z9.99 USING WS-DISC AUTO.
009930 01  S26-TDSC.
009940	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009950                           PIC Z9.99 USING WS-TDISC AUTO.
009960 01  S26-AVGE.
009970	   03	       COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009980				 PIC Z(04)9.999 USING STK-AVGE AUTO.

       COPY JOB.CRT.

010240 01  S30A.
010250	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC Z(08) USING W10-JCST AUTO.

010260 01  S31.
010270	   03  LINE  2 COLUMN 26 FOREGROUND-COLOR 15
				 VALUE "JOB LABOUR/REMARKS MAINTANANCE".

010290 01  S31A.
010300     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15 
010310                           VALUE "CANCEL JOB".

010400 01  S36.
010410	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC X(06) USING W80-ACNO AUTO.

010420 01  S38.
010430	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC ZZZZ9.9999 USING W10-MQNT AUTO.

010440 01  S39.
           03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
                                 VALUE "JOB MAINTENANCE".

       01  S40.
	   03  LINE  8 COLUMN 15 FOREGROUND-COLOR 14 VALUE "Ú".
           03          COLUMN 16 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "¿".
007450	   03  LINE  9 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 34 FOREGROUND-COLOR 6
                                 VALUE "CLIENT DETAILS".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 10 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 16 FOREGROUND-COLOR 6
                                 VALUE "Name    >".
           03          COLUMN 65 FOREGROUND-COLOR 6
                                 VALUE "<".     
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 11 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 16 FOREGROUND-COLOR 6
                                 VALUE "Address >".
           03          COLUMN 55 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 12 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 24 FOREGROUND-COLOR 6
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 13 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 24 FOREGROUND-COLOR 6
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 14 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 24 FOREGROUND-COLOR 6
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 15 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 24 FOREGROUND-COLOR 6
                                 VALUE ">".
           03          COLUMN 29 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 16 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 16 FOREGROUND-COLOR 6
                                 VALUE "Tel.    >".
           03          COLUMN 39 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 17 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 16 FOREGROUND-COLOR 6
                                 VALUE "Id.No.  >".
           03          COLUMN 39 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
	   03  LINE 18 COLUMN 15 FOREGROUND-COLOR 14 VALUE "À".
           03          COLUMN 16 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "Ù".

       01  S41.
           03  LINE 10 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(40) USING W40-NM1 AUTO.
           03  LINE 11 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD1 AUTO.
           03  LINE 12 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD2 AUTO.
           03  LINE 13 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD3 AUTO.
           03  LINE 14 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD4 AUTO.
           03  LINE 15 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(04) USING W40-PC AUTO.
           03  LINE 16 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-HTEL AUTO.
           03  LINE 17 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-ID AUTO.

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ú".
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "¿".
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Æ".
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "µ".
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
				 BACKGROUND-COLOR 7 VALUE "À".
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ù".

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

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010580 AA000-MAIN      SECTION.
010590 AA000-INIT.
003240	   IF LS0-JBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010600	     PERFORM ZA000-INIT.
	     MOVE "J"		 TO W90-ORG.
	     PERFORM BF000.
	     CLOSE RECOVER
		   SALE.
       AA49.
	     EXIT PROGRAM.

       COPY AA50JS.LUP.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
010990 AA300		  SECTION 51.
011000 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Item/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     "=Alt/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     "=Desc/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     "=Desc2/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     "=X/Ref Find,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F7" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     "=Ord,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Prompt,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
011570 AA400              SECTION.
011580 AA401.
011590	     DISPLAY "F1" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
				       BACKGROUND-COLOR 3
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011600		     "=Job no," WITH FOREGROUND-COLOR 1
				     BACKGROUND-COLOR 3
011610		     "F3" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
011620		     "=Acnt no," WITH FOREGROUND-COLOR 1
				      BACKGROUND-COLOR 3
		     "F4" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Item desc," WITH FOREGROUND-COLOR 1
					BACKGROUND-COLOR 3
		     "F5" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Client name," WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "F6" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Contact," WITH FOREGROUND-COLOR 1
				      BACKGROUND-COLOR 3
		     "F7" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Date Lookup" WITH FOREGROUND-COLOR 1
					 BACKGROUND-COLOR 3.
011630 AA405.
011640       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

011670 COPY CONTROL.RD.

011710 COPY DEBTOR.RD.

011730 COPY DEPART.RD.

011790 COPY JOBCST.RD.

011790 COPY JOBHDR.RD.

011750 COPY PARAM.RD.

011770 COPY SALE.RD.

       COPY SHARED.RD.

011810 COPY SPARTS.RD.

000140 COPY STKMEM.RD.

011830 COPY STOCK.RD.

      /
011850 AD000-WRITE             SECTION.

011870 COPY CONTROL.WR.

011930 COPY JOBCST.WR.

011930 COPY JOBHDR.WR.

011890 COPY PARAM.WR.

011910 COPY SALE.WR.

011910 COPY STOCK.WR.

011990 COPY JOBCST.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
012010 AY000           SECTION.
011490 AY05.
011500       MOVE 05             TO REC-FILE.
011510       MOVE ZERO           TO REC-KEY.
	     PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
	     MOVE WS-ACTION	 TO REC-TYPE.
011520       MOVE STK-RECORD1    TO REC-STOCKF.
011530       GO TO AY50.

012020 AY21.
012030	     MOVE 21		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060	     MOVE JHD-REC1	 TO REC-JOBHDR.
012070       GO TO AY50.

012020 AY25.
012030	     MOVE 25		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060       MOVE JOB-REC1       TO REC-JOBCST.
012070       GO TO AY50.

012080 AY39.
012090       MOVE 39             TO REC-FILE.
012100       MOVE WS-NETKEY      TO REC-KEY.
012110       MOVE NET-RECORD     TO REC-NETWORK.
012120       GO TO AY50.

012130 AY40.
012140       MOVE 99             TO REC-FILE.
012150       MOVE ZERO           TO REC-KEY.
012160       MOVE SPACES         TO REC-DETAIL.
012170       PERFORM AY50.
012180       ADD 1               TO WS-TRANS.
012190     IF WS-RECOVER > 95
012200         CLOSE RECOVER
012210         OPEN OUTPUT RECOVER
012220         CLOSE RECOVER
012230         OPEN I-O RECOVER
012240         MOVE ZERO         TO WS-RECOVER.
012250       GO TO AY59.

012260 AY50.
012270       ADD 1               TO WS-RECOVER.
012280       MOVE WS-RECOVER     TO WS-RECKEY.
012290       MOVE WS-TRANS       TO REC-TRAN.
012300       WRITE REC-RECORD.
012310     IF WS-STATUS NOT = "00"
012320         DISPLAY "Write error Recovery file - Status " AT 2212
012330                  WITH FOREGROUND-COLOR 14
012340                  WS-STATUS WITH FOREGROUND-COLOR 15
012350         STOP RUN.
012380 AY59.
012390       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "DS"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
012990	     MOVE 1		 TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY40 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

013000 AY999.
013010       EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

      *
      *    ****    Read debtor record - using account number.
      *
013610 CA155-GET-DEBTOR  SECTION 2.
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

       COPY CA200.STK.

       COPY CA500.STK.

      *
      *    ****   G E T   J O B C S T	R E C O R D   U S I N G   JOB-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT JOB RECORD (INVOICE)
      *                    3 = JOB DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *
014550 GET-JOBCST	    SECTION 2.

014570 GET-JCST-JOBNO.
             INITIALIZE W14-DELETE.
             MOVE 1              TO W14-SUB.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC.
014580	     MOVE W10-JCST	 TO JHD-REF.
014600       MOVE ZERO           TO WS-ERROR.
014610	     PERFORM READ-JOBHDR THRU READ-JOBHDR-EXIT.
014620	   IF WS-F-ERROR = 14
014630         MOVE 1            TO WS-ERROR
014640	       GO TO GET-JOBCST-EXIT.
014650	   IF JHD-DOC NOT = "J"
014660         MOVE 2            TO WS-ERROR
014670         GO TO GET-JOBCST-EXIT.
014680	     MOVE 1		 TO WS-LAST WS-SALPOS WS-SALKEY.
014790	     MOVE JHD-AC	 TO W80-ACNO DEB-ACNO.
014800	     PERFORM CA155-GET-DEBTOR.
014810	   IF WS-ERROR NOT = ZERO
014820	       MOVE 4		 TO WS-ERROR
014830	       GO TO GET-JOBCST-EXIT.
	     MOVE JHD-DTE	 TO W40-DTE.
	     MOVE JHD-ITEM	 TO W10-IDESC.
014860	     MOVE JHD-NAME	 TO W40-NAME.
	     MOVE JHD-AD1	 TO W40-AD1.
	     MOVE JHD-AD2	 TO W40-AD2.
	     MOVE JHD-AD3	 TO W40-AD3.
	     MOVE JHD-AD4	 TO W40-AD4.
	     MOVE JHD-PC	 TO W40-PC.
	     MOVE JHD-TEL	 TO W40-TEL.
	     MOVE JHD-HTEL	 TO W40-HTEL.
	     MOVE JHD-CONTACT	 TO W40-CONTACT.
	     MOVE JHD-ID	 TO W40-ID.
014870	     MOVE JHD-ORD	 TO W90-ORD.
014850	     MOVE JHD-OREF	 TO W90-REG.
014890	     MOVE JHD-EXP	 TO W90-EXP.
014910	     MOVE JHD-PRC	 TO WS-PRC.
014880	     MOVE JHD-SMAN	 TO W10-SMAN.
	     MOVE JHD-STATUS	 TO W90-STATUS.
	     MOVE JHD-COMMENT	 TO W10-COMMENT.
	     MOVE JHD-START	 TO W90-START.
015230	     MOVE JHD-SUBTOT	 TO W90-DB.
015240	     MULTIPLY -1 BY JHD-DSCNT
015250				 GIVING W90-TDISC.
015260	     MOVE W90-TDISC	 TO W90-LDISC.
015270	     MOVE JHD-NON-TXBL	 TO W90-NONTAX.
015280	     MOVE JHD-TAXABLE	 TO W90-EXCTOT.
015290	     MOVE JHD-TAX	 TO W90-TTAX.
015300	     MOVE JHD-TOT	 TO W90-TOTAL.
015310	     MOVE JHD-BAL	 TO W90-BAL.
	     MOVE JHD-INSTRUCT	 TO W90-INSTRUCT.
	     MOVE JHD-PMETH	 TO W90-PMETH.
      *
014580	     MOVE W10-JCST	 TO JOB-REF.
022140       MOVE 100            TO JOB-SQN.
014610	     PERFORM START-AT-JOB-NO THRU READ-JOBCST-EXIT.
014680	     MOVE 1		 TO WS-LAST WS-SALKEY.
014620     IF WS-F-ERROR = 13
014640         GO TO GET-JOBCST-EXIT.

014710 GET-JCST-NEXT.
014720       PERFORM READ-JOBCST-NEXT THRU READ-JOBCST-EXIT.
014730     IF WS-F-ERROR = 13
014750         GO TO GET-JOBCST-EXIT.
	   IF JOB-REF NOT = W10-JCST
	       MOVE W10-JCST   TO JOB-REF
	       GO TO GET-JOBCST-TEST-EXIT.

014770 GET-JCST-TEST-TYPE.
      * 		     I T E M   D E T A I L S
014930     IF JOB-TYPE = 1
               PERFORM GET-CHECK-STOCK-FILE
               IF WS-F-ERROR = 0
014940             INITIALIZE SAL-RECORD1
		   MOVE "S"	 TO SAL-TYPE
014950		   MOVE JOB-EXT-ITEM
				 TO SAL-EXT-ITEM
014960             MOVE JOB-DESC TO SAL-DESC
014970             MOVE JOB-QNT  TO SAL-QNT
014980             MOVE JOB-COST TO SAL-COST
014990             MOVE JOB-SELL TO SAL-SELL
015000             MOVE JOB-VAT  TO SAL-VAT
		   MOVE JOB-VAT-SUB
				 TO SAL-VAT-SUB
015010		   MOVE JOB-VAL  TO SAL-VAL
015020             MOVE JOB-INCTAX
                                 TO SAL-INCTAX
015030             MOVE JOB-NONTAX
                                 TO SAL-NONTAX
015040             MOVE JOB-TAXVAL   
                                 TO SAL-TAXVAL
015050             MOVE JOB-DSC  TO SAL-DSC
015060             MOVE JOB-DISC TO SAL-DISC
015070             MOVE JOB-SQN  TO SAL-SQN
                   MOVE JOB-TECH TO SAL-TECH
		   IF SAL-VAT-SUB > ZERO
		       ADD JOB-TAXVAL
				 TO W90-TAXVAL(SAL-VAT-SUB)
023060		       MOVE W05-VAT(SAL-VAT-SUB)
023060				 TO W05-RATE
023080		       COMPUTE W90-VAT(SAL-VAT-SUB) ROUNDED =
			      (W90-TAXVAL(SAL-VAT-SUB) * W05-RTE)
		   END-IF
015080		   PERFORM GET-JCST-WRITE-SALE
               ELSE
                   PERFORM GET-CHANGE-TO-REMKS
015090     ELSE
      *                      P A R T S   D E T A I L S
015100     IF JOB-TYPE = 2
015110         INITIALIZE SAL-RECORD1
	       MOVE "T" 	 TO SAL-TYPE
014950	       MOVE JOB-EXT-ITEM TO SAL-EXT-ITEM
014960         MOVE JOB-DESC     TO SAL-DESC
014970         MOVE JOB-QNT      TO SAL-QNT
015140         MOVE JOB-SQN      TO SAL-SQN
015160	   ELSE
      *                      R E M A R K S
015170     IF JOB-TYPE = 3
015180         INITIALIZE SAL-RECORD1
	       MOVE "R" 	 TO SAL-TYPE
015190         MOVE JOB-SERIAL   TO SAL-DESC
015200         MOVE JOB-SQN      TO SAL-SQN
015210         PERFORM GET-JCST-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF JOB-TYPE = 4 
               INITIALIZE SAL-RECORD1
	       MOVE "I" 	 TO SAL-TYPE
               MOVE JOB-DESC     TO SAL-DESC
               MOVE JOB-SQN      TO SAL-SQN
               MOVE 1            TO SAL-SUPER
	       PERFORM GET-JCST-WRITE-SALE.
015340	     GO TO GET-JCST-NEXT.

015360 GET-JCST-WRITE-SALE.
015370       MOVE WS-LAST        TO SAL-CODE.
015380       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
015390       ADD 1               TO WS-LAST WS-SALKEY.
015400 
       GET-CHECK-STOCK-FILE.
	     MOVE JOB-EXT-ITEM	 TO STK-CODE.
013560       PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       GET-CHANGE-TO-REMKS.
             INITIALIZE SAL-RECORD1.
	     STRING JOB-EXT-ITEM DELIMITED BY SIZE
                    JOB-DESC DELIMITED BY SIZE
                                 INTO SAL-DESC.
	     MOVE JOB-SQN	 TO SAL-SQN W14-SQN(W14-SUB).
             ADD 1               TO W14-SUB.
             PERFORM GET-JCST-WRITE-SALE.

       GET-CH-TO-RMK-EXIT.
	     GO TO GET-JCST-NEXT.

       GET-JOBCST-TEST-EXIT.
	   IF NOT (W14-SUB > 1)
015330	       GO TO GET-JOBCST-EXIT.

       GET-ADJUST-TOTALS.
             SUBTRACT 1 FROM W14-SUB
                                 GIVING WS-SUB.
             MOVE 1              TO W14-SUB.

       GET-ADJUST-TOTALS-LOOP.
	     MOVE W14-SQN(W14-SUB)
                                 TO JOB-SQN.
             PERFORM READ-JOBCST-LOCK THRU READ-JOBCST-EXIT.
025000       SUBTRACT JOB-VAL    FROM W90-DB.
025010       SUBTRACT JOB-DISC   FROM W90-LDISC.
025020       SUBTRACT JOB-NONTAX FROM W90-NONTAX.
025030	     SUBTRACT JOB-TAXVAL FROM W90-EXCTOT.
025050       COMPUTE W90-BAL = W90-BAL - (JOB-COST * JOB-QNT).
             MOVE 3              TO JOB-TYPE.
	     STRING JOB-EXT-ITEM DELIMITED BY SIZE
                    JOB-DESC DELIMITED BY SIZE
                                 INTO JOB-DESC.
	     MOVE SPACES	 TO JOB-EXT-ITEM JOB-SERNO.
             MOVE ZERO           TO JOB-QNT JOB-COST JOB-INCTAX
                                    JOB-VAT JOB-SELL JOB-NONTAX
                                    JOB-VAL JOB-DISC JOB-TAXVAL
                                    JOB-DSC.
             PERFORM REWRITE-JOBCST-UNLOCK THRU WRITE-JOBCST-EXIT.
           IF W14-SUB < WS-SUB
               ADD 1             TO W14-SUB
               GO TO GET-ADJUST-TOTALS-LOOP.

015410 GET-JOBCST-EXIT.
015420       EXIT.

      /
015800 BB100		   SECTION 3.
015810 BB100-MARG.
015820	     MOVE ZERO		 TO WS-ERROR.
018010	   IF (W90-TAX = SPACES) AND
	      (W90-EXP NOT = "C" AND "X")
015840         IF STK-TAX NOT = ZERO
015850             COMPUTE W10-SPAPRC = (W10-SPAPRC /
015860                                  (100.0000 + W05-RATE)) * 100. 
015870     IF STK-WSALE NOT > W10-SPAPRC
015880         GO TO BB110.
015890       COMPUTE W10-MARG = ((W10-SPAPRC - W10-COST) / W10-SPAPRC)
015900                          * 100.00.
015910     IF W10-MARG NOT < W85-MARG
015920         GO TO BB110.
015950 BB105.
018110	     MOVE "Margin less than allowed - Override Y/N?"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
016020	   IF NOT (WS-OPTION = "N" OR "Y")
016030         GO TO BB105.
016040     IF WS-OPTION = "N"
016050	       MOVE 1		 TO WS-ERROR
016060         GO TO BB110.
016070 BB106.
016080	     DISPLAY "Enter password" AT 2551
016090		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
016100	     ACCEPT W40-CSHPASS AT 2567
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
016110			 AUTO NO-ECHO.
016120	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016180	     MOVE 1		 TO WS-LENGTH.
       BB107.
           IF W40-CSHPASS = "!@$^*)"
               MOVE 10           TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (1)
               MOVE 1            TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (9)
               MOVE 9            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
           GO TO BB110.
       BB108.
           IF W40-CSHPASS = "!#%&()"
               MOVE 10           TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (1)
               MOVE 1            TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (6)
               MOVE 6            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
018480 BB110.
018490       EXIT.
      /
      *    ****    M A I N T A I N   J O B   -	 L A B O U R
      *
033340 BF000	     SECTION 51.
033350 BF00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
033360       MOVE 5              TO WS-PARKEY.
033370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
033380       MOVE PAR-PW         TO W85-PASS. 
      *
      *    ****    J O B   C O S T I N G   S C R E E N
      *
033390 BF05.
	     DISPLAY CLR-SCREEN.
             DISPLAY S30. 
      *            C H A N G E   S C R E E N   H E A D I N G
             DISPLAY S39.
033430       MOVE 8              TO LIN.
033440       PERFORM BF115 UNTIL LIN > 18.
033450       MOVE W12-TODAY      TO W90-DTE.
033460	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033470       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
033480				    W90-BAL W90-SALES
033490				    W90-CUR W90-EXCTOT.
033500	     MOVE 1		 TO WS-FIRST WS-SALPOS WS-LAST.
018700       MOVE SPACES         TO W90-TAX.
033510       MOVE ZERO           TO W10-JCST.
             MOVE "N"            TO WS-IN-OPT.
	     MOVE 1		 TO WS-S9.

       BF05A.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BF05A.

033520 BF10.
033530       PERFORM AA400.
033540       ACCEPT S30A AT 0510.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-JCST
		     GO TO BF265
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "J"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "N"	 TO WS-ETYPE
		 WHEN F6-KEY
		     MOVE "C"	 TO WS-ETYPE
		 WHEN F7-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BF10
	       END-EVALUATE
016560	       PERFORM AA50J THRU AA59
016570	       DISPLAY S30A AT 0510
016580	       IF W10-JCST = ZERO
016590		   GO TO BF10.
033600	     DISPLAY CLEAR-L25.
033610     IF W10-JCST = ZERO
033620	       GO TO BF265.
033630	     MOVE W10-JCST	 TO W90-REF.

033640 BF12.
033650       PERFORM GET-JOBCST.
033660     IF WS-ERROR = 1
033670	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033680	       GO TO BF10.
033690     IF WS-ERROR = 2
033700	       MOVE "Not a job"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033710	       GO TO BF10.
033720     IF WS-ERROR = 3
033730	       MOVE "Job corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033750	       GO TO BF10.
033760     IF WS-ERROR = 4
033770	       MOVE "Invalid a/c no"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033780	       GO TO BF10.
025820       INITIALIZE W14-DELETE.
	   IF W90-EXP = "X" OR "C" OR "E"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
033790       DISPLAY S36 AT 0308
033800	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033810	     DISPLAY W90-ORD AT 0368
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033820	     DISPLAY W10-SMAN AT 0411
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033830     IF W10-SMAN NOT = ZERO
033840         ADD 200 W10-SMAN    GIVING WS-PARKEY
033850         PERFORM READ-PARAM THRU READ-PARAM-EXIT
033860	       DISPLAY PAR-TECHNICIAN AT 0420
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0.
	     DISPLAY W40-TEL AT 0466
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	     DISPLAY W40-CONTACT AT 0528
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	   IF NOT (W10-IDESC = SPACES)
	       DISPLAY "¸" AT 0720
			W10-IDISP WITH FOREGROUND-COLOR 6 HIGHLIGHT
		       "Õ" AT 0750 WITH FOREGROUND-COLOR 3
					BACKGROUND-COLOR 1.
033870	     MOVE 1		 TO WS-SALKEY.
033880       SUBTRACT 1          FROM WS-LAST.

033890 BF15.
033900       MOVE W90-DB         TO W90-TOTAL.
033910       SUBTRACT W90-LDISC  FROM W90-TOTAL.
033920       MOVE W90-DB         TO W100-BAL.
033930	     DISPLAY W100-BAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033940       MULTIPLY -1 BY W90-LDISC 
033950                           GIVING W100-BAL.
033960	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
034000	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
034010         GO TO BF20.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BF16.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BF16.
034060	     MOVE W25-TOTAL	 TO W100-BAL.
034080	     ADD W25-TOTAL	 TO W90-TOTAL.
034070	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

034090 BF20.
034140	     MOVE W90-TOTAL	 TO W100-BAL.
034150	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *
      *    ****    D I S P L A Y   J O B   D E T A I L S
      *
034160 BF25.
034170       MOVE 8              TO LIN.
034180       PERFORM BF115 UNTIL LIN > 18.
034190       MOVE 8              TO LIN.
034200       MOVE 1              TO WS-LINES WS-FIRST.

034210 BF30.
034220	     MOVE WS-FIRST	 TO WS-SALKEY.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
034230 BF35.
034240	   IF WS-SALKEY > WS-LAST
               ADD 1             TO WS-LAST
034250         GO TO BF45.
034260       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
034280	   IF SAL-TYPE = "I" OR "R"
034290	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
	       DISPLAY (LIN, 5) "("
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
			SAL-TYPE WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 0
			")" WITH FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
034300         GO TO BF40.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
034320	     DISPLAY (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
034330       MOVE SAL-QNT        TO W10-QUANT.
034340       DISPLAY S26-QNT AT LINE LIN.
034350       MOVE SAL-SELL       TO W10-SELL.
034360       DISPLAY S26-PRC AT LINE LIN.
	   IF SAL-TYPE = "P"
	       DISPLAY ( , 70) "Requisition"
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
	   ELSE
034370	       MOVE SAL-VAL	 TO W100-EXT
034380	       DISPLAY ( , 70) W100-EXT
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
034390 BF40.
034400     IF LIN < 18
034410         ADD 1             TO LIN WS-SALKEY WS-LINES
034420         GO TO BF35.
	     MOVE "Press any key to continue"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     PERFORM BF25.
	     GO TO BF35.

034430 BF45.
034440	     EXIT.

       BF50.
	     DISPLAY CLEAR-L25.
	     DISPLAY "C" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "hange Technician, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "L" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "abour, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "R" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "eceive spares/parts or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "nd > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     MOVE "L"		 TO WS-MAINT.

       BF52.
034890	     ACCEPT WS-MAINT AT 2558 WITH
		    BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-MAINT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-MAINT = "C" OR "E" OR "L" OR "R")
               GO TO BF52.
018780	     DISPLAY CLEAR-L25.
           IF WS-MAINT = "C"
               PERFORM  BF215 THRU BF216
               GO TO BF50.
           IF WS-MAINT = "E"
018670         SUBTRACT 1        FROM WS-LAST
018680         GO TO BF120.

018540 BF55.
018550	     MOVE SPACES	 TO W10-EXT-ITEM.
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

       BF56.
018550	     MOVE SPACES	 TO W10-DESC.
018560	     MOVE ZERO		 TO W10-SELL.
             MOVE 1              TO W10-QUANT.

018570 BF60.
018580       PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
	   IF WS-EXT-STK = "Y"
	       ACCEPT S07-EXT AT LINE LIN
	   ELSE
      *
      *        ***** ALPHA-NUMERIC CODE *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S07-ITM AT LINE LIN
	       ELSE
021070		   ACCEPT S07 AT LINE LIN
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S07-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S07-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S07-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH =15
	       ACCEPT S07-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S07-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S07-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S07-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S07-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S07-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S07-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S07-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S07-14 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO BF60A
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BF60
	       END-EVALUATE
016560	       PERFORM AA50 THRU AA59
	       PERFORM BF56
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF (W10-STCK = "A") AND
		  (WS-USE-ITM = "Y")
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN.
020790	       IF W10-EXT-ITEM = SPACES
016590		   GO TO BF60.

       BF60A.
018660	   IF W10-EXT-ITEM = SPACES
018670         SUBTRACT 1        FROM WS-LAST
018680         GO TO BF120.

       BF61.
018690       PERFORM CA000.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
018700	   IF WS-F-ERROR = 22
	       MOVE "Z"		 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
	       PERFORM BF56
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN
		   END-IF
	       END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BF60
               ELSE
                   GO TO BF61.
           IF WS-MAINT = "L"
               IF STK-IND NOT = 2
		   MOVE "Not a labour code"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
                   GO TO BF60.
018730       MOVE WS-LAST        TO WS-SALKEY.
018740       INITIALIZE SAL-RECORD1.
018750       MOVE WS-LAST        TO SAL-CODE.
	     MOVE "X"		 TO SAL-AGE.
018770	     MOVE STK-CODE	 TO SAL-EXT-ITEM.
	     MOVE ZERO		 TO SAL-TECH.
      *
      *    ****    I S   I T   A   L A B O U R   I T E M
      *
           IF (W10-SMAN = ZERO) OR (STK-IND NOT = 2)
	       MOVE "S"		 TO SAL-TYPE
	       GO TO BF65.
	     MOVE W10-SMAN	 TO SAL-TECH.
	     MOVE "S"		 TO SAL-TYPE.
033840       ADD 200 W10-SMAN    GIVING WS-PARKEY.
033850       PERFORM READ-PARAM THRU READ-PARAM-EXIT.

       BF62.    
	     MOVE "'N'ormal/'O'ver/'D'ouble time  [L]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-TME.
	   IF NOT (WS-TME = "D" OR "N" OR "O")
               GO TO BF62.
           IF WS-TME = "N"
               MOVE PAR-NORM-CRTE
                                 TO STK-SELL
               MOVE PAR-NORM-RATE
                                 TO STK-CASH
               MOVE PAR-NORM-DRTE
                                 TO STK-WSALE
           ELSE
           IF WS-TME = "N"
               MOVE PAR-OVER-CRTE
                                 TO STK-SELL
               MOVE PAR-OVER-RATE
                                 TO STK-CASH
               MOVE PAR-OVER-DRTE
                                 TO STK-WSALE
           ELSE
           IF WS-TME = "N"
               MOVE PAR-SUND-CRTE
                                 TO STK-SELL
               MOVE PAR-SUND-RATE
                                 TO STK-CASH
               MOVE PAR-SUND-DRTE
                                 TO STK-WSALE.
             PERFORM CA500.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
018790 BF65.
018800     IF STK-IND = 3
018810         MOVE ZERO         TO STK-AVGE
018820	       ACCEPT (LIN, 21) W10-SDSC
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
018840         DISPLAY "Index Unit cost" AT 2512
018850			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018860         ACCEPT S26-AVGE AT LINE 25
018870     ELSE
018880         MOVE STK-DESC     TO W10-DESC
018890	       DISPLAY (LIN, 21) W10-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0.
018900     IF STK-IND = 3
018910         IF STK-AVGE = ZERO
021070		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
018940		   GO TO BF65.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
018960	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
018970       MOVE W10-DESC       TO SAL-DESC.
018980       MOVE STK-AVGE       TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
018990       ACCEPT S26-QNT AT LINE LIN.
019000     IF W10-QUANT = ZERO
019010	       DISPLAY (LIN, 2) WS-JOB78
019020         GO TO BF55.
	     MOVE STK-TAX	 TO WS-VAT-SUB.
      *
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *
       BF66.
019040     IF STK-IND < 3
019050         IF W90-TAX NOT = SPACES
019060		   IF WS-PRC = "1"
019070                 MOVE STK-SELL 
019080                           TO W10-SELL
019090             ELSE
019100		   IF WS-PRC = "2"
019110                 MOVE STK-CASH 
019120                           TO W10-SELL
019130             ELSE
019140                 MOVE STK-WSALE 
019150                           TO W10-SELL
019160         ELSE
019170		   IF WS-PRC = "1"
019180                 MOVE W10-VSELL 
019190                           TO W10-SELL
019200             ELSE
019210		   IF WS-PRC = "2"
019220                 MOVE W10-VCASH 
019230                           TO W10-SELL
019240             ELSE
019250                 MOVE W10-VWSALE 
019260                           TO W10-SELL.

       BF68.
             MOVE W10-SELL       TO W10-SPAPRC.

019270 BF70.
019280       ACCEPT S26-PRC AT LINE LIN.
	   IF W10-SELL < (W10-SPAPRC - 0.009)
               COMPUTE WS-DISC ROUNDED = ((W10-SELL - W10-SPAPRC) / 
					   W10-SELL) * 100.00000
	       DISPLAY CLEAR-L25
022110	       DISPLAY "Discount %" AT 2518
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
022190         DISPLAY S26-DSC AT LINE 25
	       DISPLAY "R" AT 2541
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "ekey or "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 2
		       "ENTER"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       " to continue"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
	       ACCEPT WS-OPTION AT 2568
		      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
022440	       DISPLAY CLEAR-L25
               IF WS-OPTION = "R"
                   GO TO BF70.
	   IF W10-SELL < (W10-SPAPRC - 0.009)
019290	       MOVE W10-SELL	 TO W10-SPAPRC
019310	       PERFORM BB100
019320	       IF WS-ERROR = 1
019330		   MOVE ZERO	  TO WS-ERROR
019340		   GO TO BF66.
019350	     DISPLAY CLEAR-L25.

019360 BF75.
019370       MOVE W10-SELL       TO SAL-SELL.

019410 BF80.
      *
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *
019420       MOVE W10-QUANT      TO SAL-QNT.
019430       COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
019440       MOVE W25-VALUE      TO W100-EXT SAL-VAL.
	   IF SAL-TYPE = "P"
	       DISPLAY (LIN, 70) "Requisition"
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
	       GO TO BF90
	   ELSE
019450	       DISPLAY (LIN, 70) W100-EXT
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0.
      *
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *
019460	     DISPLAY "Discount %" AT 2518
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
019470       MOVE ZERO           TO WS-DISC W90-CDISC.
019480	   IF (DEB-DISC > 0 AND < 11)
019490	       MOVE STK-LDG	 TO DPT-CODE
019500         PERFORM READ-DEPART THRU READ-DEPART-EXIT
019510	       MOVE DPT-DSETT(DEB-DISC)
019520                           TO WS-DISC.
019530 BF85.
019540       ACCEPT S26-DSC AT LINE 25.
019550     IF WS-DISC = ZERO
019560         GO TO BF90.
019570	     COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
019580		(W10-SELL * WS-DISC / 100.0000).
      *
      *    ****    C H E C K   M A R G I N
      *
019590	   IF (DEB-DISC > 0 AND < 11)
019600	       IF DPT-DSETT(DEB-DISC) = WS-DISC
019610             GO TO BF86.
019620       PERFORM BB100.
019630     IF WS-ERROR = 1
019640         GO TO BF85.

019650 BF86.
019660	     DISPLAY CLEAR-L25.
019670       MOVE WS-DISC        TO SAL-DSC.
019680       COMPUTE W90-CDISC ROUNDED = ((W25-VALUE * WS-DISC) / 100).
019690       MOVE W90-CDISC      TO W100-LBAL SAL-DISC.
019700	     DISPLAY W100-LBAL AT 2540
		     WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 11.

019710 BF90.
      *
      *    ****    A R E   T H E   D E T A I L S   C O R R E C T
      *
	     PERFORM CHECK-CORRECT.
019800	   IF WS-OPTION = "N"
019810         GO TO BF60.
019820 BF95.
	   IF SAL-TYPE = "P"
	       GO TO BF105.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
019830     IF (STK-IND < 2) OR (STK-IND = 3)
019840         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
019850       ADD W90-CDISC       TO W90-LDISC.
019860       SUBTRACT W90-CDISC FROM W25-VALUE 
019870                           GIVING W90-VALUE.
022840	   IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C")
019890         ADD W90-VALUE     TO W90-NONTAX
019900         MOVE W90-VALUE    TO SAL-NONTAX
	       MOVE ZERO	 TO SAL-VAT-SUB
019910     ELSE
	       IF W90-EXP = "I"
		   COMPUTE W90-TAXVAL(WS-VAT-SUB) =
			   W90-TAXVAL(WS-VAT-SUB) + (W90-VALUE -
			 ((W90-VALUE / (100.0000 + W05-RATE)) *
			   W05-RATE))
		   COMPUTE SAL-TAXVAL = W90-VALUE -
			 ((W90-VALUE / (100.0000 + W05-RATE)) *
			   W05-RATE)
	       ELSE
		   MOVE W90-VALUE
				 TO SAL-TAXVAL
022890		   ADD W90-VALUE TO W90-TAXVAL(WS-VAT-SUB)
	       END-IF
	       MOVE WS-VAT-SUB	 TO SAL-VAT-SUB.
019940	   IF (W90-TAX NOT = SPACES) AND
	      (W90-EXP NOT = "C" AND "X")
019950         COMPUTE SAL-VAT ROUNDED = 
019960		       SAL-TAXVAL * W05-RTE.
	   IF (W90-EXP = "I") AND (STK-TAX NOT = 0)
	       COMPUTE SAL-VAT ROUNDED = (SAL-TAXVAL /
		      (100.0000 + W05-RATE)) * W05-RATE.
	   IF SAL-VAT-SUB > 0
	       ADD SAL-VAT	 TO W90-VAT(SAL-VAT-SUB).
      *
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *
019970       ADD W25-VALUE       TO W90-DB.

019980 BF100.
019990       MOVE W90-DB         TO W90-TOTAL.
020000       SUBTRACT W90-LDISC  FROM W90-TOTAL.
020010       MOVE W90-DB         TO W100-BAL.
020020	     DISPLAY W100-BAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
020030       MULTIPLY -1 BY W90-LDISC 
020040                           GIVING W100-BAL.
020050	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
020090	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
020100         GO TO BF102.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BF101.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BF101.
023100	     MOVE W25-TOTAL	 TO W100-BAL.
023120	     ADD W25-TOTAL	 TO W90-TOTAL.
020160	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

020180 BF102.
020230	     MOVE W90-TOTAL	 TO W100-BAL.
020240	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

020250 BF105.
036100	     MOVE "'A'ccept or 'C'ancel  [A]"
				 TO WS-ERR-MES.
023210	     MOVE "A"		 TO WS-OPTION.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *
020310 BF110.
	     PERFORM OPT-MESSAGE.
020390	   IF NOT (WS-OPTION = "A" OR "C")
020400	       GO TO BF105.
020420     IF WS-OPTION = "C"
020430	       DISPLAY (LIN, 2) WS-JOB78
			WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
020440         PERFORM BF114
020450         PERFORM BF100 THRU BF102
020460         GO TO BF55.               

020470 BF111.
	     MOVE WS-SUPER	 TO SAL-SUPER.
020480       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
020490       ADD 1               TO WS-LAST.
      *
      *    ****    T E S T   I F   S C R E E N	 F U L L
      *
020500     IF LIN < 18
020510         ADD 1             TO LIN
020520         GO TO BF112.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO WS-S7.
      *
      *    ****    S C R O L L	 D E T A I L   L I N E S   U P
      *
       BC111-SHIFT.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BC111-SHIFT.
	     PERFORM RESTORE-SCREEN-2.
      *
      *    ****    C L E A R   L A S T	 D E T A I L   L I N E
      *
023890	     DISPLAY (LIN, 1) WS-JOB1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-JOB78 WITH FOREGROUND-COLOR 3
			      WS-JOB80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
	     ADD 1		 TO WS-SALPOS.

020560 BF112.
	     MOVE SPACES	 TO WS-ERR-MES.
	     STRING "'R'emarks, 'N'ext or 'E'nd  ["
		    DELIMITED SIZE
		    WS-IN-OPT DELIMITED SIZE
		    "]" DELIMITED SIZE
				 INTO WS-ERR-MES.
023180	     MOVE WS-IN-OPT	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020710	   IF NOT (WS-OPTION = "R" OR "N" OR "E")
020720         GO TO BF112.
             MOVE WS-OPTION      TO WS-IN-OPT.
020740     IF WS-OPTION = "N"
020750         GO TO BF50.
020760     IF WS-OPTION = "E"
020770         SUBTRACT 1        FROM WS-LAST
020780         GO TO BF120.
020790       MOVE SPACES         TO W10-REMKS.
020800	     ACCEPT (LIN, 21) W10-REMKS
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
020820     IF W10-REMKS = SPACES
020830         GO TO BF112.
020850	     MOVE WS-LAST	 TO WS-SALKEY.
020860       INITIALIZE SAL-RECORD1.
	     MOVE "X"		 TO SAL-AGE.
	     MOVE WS-OPTION	 TO SAL-TYPE.
020870       MOVE WS-LAST        TO SAL-CODE.
020880       MOVE W10-REMKS      TO SAL-DESC.
	     DISPLAY (LIN, 9) "(R)"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0.
020890       GO TO BF111.

020900 BF114.
020910       SUBTRACT W25-VALUE  FROM W90-DB.
020920       SUBTRACT W90-CDISC  FROM W90-LDISC.
020930       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF SAL-VAT-SUB > ZERO
036780	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(SAL-VAT-SUB).
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
020960         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *
      *    ****   C L E A R   S C R E E N   L I N E / S
      *
020970 BF115.
023890	     DISPLAY (LIN, 1) WS-JOB1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-JOB78 WITH FOREGROUND-COLOR 3
			      WS-JOB80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
020990	     ADD 1		 TO LIN.
      *
      *    ****    W A S   T H E R E   A N   E N T R Y
      *
       BF120.
021010	   IF WS-LAST = ZERO
021020         GO TO BF05.
      *
      *    ****    P R O C E S S   T H E   J O B
      *
028820 BF125.
028840	     MOVE "'U'pdate, 'A'mend, 'C'ancel or 'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

028920 BF130.
	     PERFORM OPT-MESSAGE.
028990	   IF NOT (WS-OPTION = "U" OR "A" OR "C" OR "M")
029000         GO TO BF130.
029020     IF WS-OPTION = "M"
029030         ADD 1             TO WS-LAST
029040         GO TO BF112.
029050     IF WS-OPTION = "A"
029060	       PERFORM BF270A
029070         GO TO BF125.
029080     IF WS-OPTION = "C"
029090	       GO TO BF265.
029100       MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
029110     IF W90-LDISC NOT = ZERO
029120         MOVE W90-LDISC    TO W90-TDISC
029130         GO TO BF140.
029140       MOVE W90-DB         TO W90-DVAL.

029150 BF140.
029160       SUBTRACT W90-TDISC FROM W90-DB 
029170                           GIVING W90-TSELL W90-TOTAL.
029180       MULTIPLY -1 BY W90-TDISC 
029190                           GIVING W100-BAL.
029200	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
029240	     MOVE ZERO		 TO W90-TTAX
				    W90-EXCTOT.
	     MOVE 1		 TO WS-P1.
029250	   IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
029260	       GO TO BF142.
      *
      *    ****    C A L C U L A T E   T O T A L   V A T
      *
       BF141.
037540	   IF (W90-TAXVAL(WS-P1) = ZERO)
024430	       GO TO BF142.
024440	     MOVE W05-VAT(WS-P1) TO W05-RATE.
021490	     COMPUTE W90-TTAX ROUNDED =
		    (W90-TAXVAL(WS-P1) * W05-RTE).
	     ADD W90-TAXVAL(WS-P1)
				 TO W90-EXCTOT.
      *
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      * 	   H A V E   B E E N   C A L C U L A T E D
      *
       BF142.
	   IF WS-P1 < 6
	       ADD 1		 TO WS-P1
	       GO TO BF141.
029300	     MOVE W90-TTAX	 TO W100-BAL.
029310	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
029320       ADD W90-TTAX        TO W90-TOTAL.

029330 BF185.
029380	     MOVE W90-TOTAL	 TO W100-BAL.
029390	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
029400	     DISPLAY CLEAR-L25.
      *
      *    ****    P R O C E S S   T H E   J O B 
      *
029410 BF188.
029420	     DISPLAY "Updating Job" AT 2520 WITH
029430		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
029440       MOVE ZERO           TO WS-SALKEY.
029450       PERFORM AY60 THRU AY999.
      *
      *    ****    U P D A T E   J O B   F I L E 
      *
029460 BF190.
029470	     MOVE W90-REF	 TO JOB-REF.
029480     IF W14-SUB = ZERO
029490         GO TO BF205.
      *
      *    ****    D E L E T E   R E D U N D A N T   R E C O R D S
      *
029500       MOVE 1              TO WS-S1.

029510 BF195.
	   IF W14-SQN (WS-S1) NOT = ZERO
029520	       MOVE W14-SQN (WS-S1)
029530				 TO JOB-SQN
029540	       PERFORM READ-JOBCST THRU READ-JOBCST-EXIT
	       IF WS-F-ERROR = 0
029550		   MOVE 2	 TO WS-ACTION
029560		   PERFORM AY25 THRU AY59
029570		   PERFORM DELETE-JOBCST-REC THRU WRITE-JOBCST-EXIT
		   IF JOB-TYPE = 1
		       PERFORM BF196 THRU BF199.
029580     IF WS-S1 < W14-SUB
029590         ADD 1             TO WS-S1
029600         GO TO BF195.
             GO TO BF205.

       BF196.
	     MOVE JOB-EXT-ITEM	 TO STK-CODE PRT-EXT-ITEM.
	     MOVE 0		 TO WS-ACTION.
             PERFORM AY05 THRU AY59.
	   IF NOT (STK-IND = 2)
	       SUBTRACT JOB-QNT	 FROM STK-QJOB
	       ADD JOB-QNT	 TO STK-QUANT.
	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.

       BF199.
             EXIT.

029610 BF200.
029620       PERFORM REWRITE-JOBCST THRU WRITE-JOBCST-EXIT.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   J O B   F I L E
      *
029630 BF205.
029640	     ADD 1		 TO WS-SALKEY.
029650     IF WS-SALKEY > WS-LAST
029660         GO TO BF255.
029670       PERFORM READ-SALE THRU READ-SALE-EXIT.
029680     IF SAL-SQN = ZERO
029690         SUBTRACT 1        FROM WS-SALKEY
029700         GO TO BF235.
029710       MOVE SAL-SQN        TO JOB-SQN.
029720       PERFORM READ-JOBCST THRU READ-JOBCST-EXIT.
029730       MOVE 0              TO WS-ACTION.
029740	     PERFORM AY25 THRU AY59.
029750	   IF JOB-TYPE NOT = 1
029760         GO TO BF210.
	     MOVE JOB-EXT-ITEM	 TO STK-CODE.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
      *
      *    ****   U P D A T E	I F   N O T   L A B O U R
      *
019830	   IF NOT (STK-IND = 2)
	       SUBTRACT JOB-QNT	 FROM STK-QJOB
	       ADD JOB-QNT	 TO STK-QUANT
	       SUBTRACT SAL-QNT	 FROM STK-QUANT
	       ADD SAL-QNT	 TO STK-QJOB.
	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
029770	     MOVE SAL-QNT	 TO JOB-QNT.
029780       MOVE SAL-COST       TO JOB-COST.
029790	     MOVE SAL-VAT	 TO JOB-VAT.
	     MOVE SAL-VAT-SUB	 TO JOB-VAT-SUB.
029800       MOVE SAL-SELL       TO JOB-SELL.
029810       MOVE SAL-VAL        TO JOB-VAL.
029820       MOVE SAL-DISC       TO JOB-DISC.
029830       MOVE SAL-DSC        TO JOB-DSC.
029840       MOVE SAL-INCTAX     TO JOB-INCTAX.
029850       MOVE SAL-NONTAX     TO JOB-NONTAX.
029860	     MOVE SAL-TAXVAL	 TO JOB-TAXVAL.
	     MOVE SAL-TECH	 TO JOB-TECH.
029870       GO TO BF200.

029880 BF210.
029890       MOVE SAL-DESC       TO JOB-DESC.
029900       GO TO BF200.

       BF215.
017700	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
017720     IF W10-SMAN > 50
017730         GO TO BF215.
017740     IF W10-SMAN = ZERO
	       DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 0420
                       WITH FOREGROUND-COLOR 3
017750         GO TO BF216.
017760       ADD 200 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-TECHNICIAN = SPACES
017790	       MOVE "Invalid Technician Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017810	       GO TO BF215.
017830	     DISPLAY PAR-TECHNICIAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
       BF216.
             EXIT.
      *
      *    ****    L O G   R E C O R D   T O   J O B   F I L E
      *
029920 BF225.
029930       PERFORM WRITE-JOBCST THRU WRITE-JOBCST-EXIT.
029940       MOVE 1              TO WS-ACTION.
029950	     PERFORM AY25 THRU AY59.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   J O B   F I L E
      *
029960 BF235.
029970       ADD 1               TO WS-SALKEY.
029980     IF WS-SALKEY > WS-LAST
029990         GO TO BF255.
030000       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    C H E C K   I F   R E M A R K S   O R
      * 	   I N T E R N A L   N O T E S
      *
030010	   IF SAL-TYPE = "I" OR "R"
030020	       GO TO BF247.
      *
      *    ****    I T E M   /	 L A B O U R   D E T A I L S
      *
	   IF SAL-TYPE = "T"
	       MOVE 2		 TO JOB-TYPE
	   ELSE
	   IF SAL-TYPE = "S"
030050	       MOVE 1		 TO JOB-TYPE.
030060	     MOVE SAL-EXT-ITEM	 TO JOB-EXT-ITEM.
030070       MOVE SAL-DESC       TO JOB-DESC.
030080       MOVE SAL-QNT        TO JOB-QNT.
030090       MOVE SAL-COST       TO JOB-COST.
030100       MOVE SAL-VAT        TO JOB-VAT.
	     MOVE SAL-VAT-SUB	 TO JOB-VAT-SUB.
030110       MOVE SAL-SELL       TO JOB-SELL.
030120       MOVE SAL-VAL        TO JOB-VAL.
030130       MOVE SAL-DISC       TO JOB-DISC.
030140       MOVE SAL-DSC        TO JOB-DSC.
030150       MOVE SAL-INCTAX     TO JOB-INCTAX.
030160       MOVE SAL-NONTAX     TO JOB-NONTAX.
030170	     MOVE SAL-TAXVAL	 TO JOB-TAXVAL.
	     MOVE SAL-TECH	 TO JOB-TECH.
030180	     MOVE SPACES	 TO JOB-SERNO.
030190       ADD 1               TO JOB-SEQ.
             MOVE ZERO           TO JOB-SUB.
             PERFORM BF225.
      *
      *    ****    U P D A T E	 J O B C A R D	 Q U A N T I T Y
      * 	   O N	 S T O C K   R E C O R D
      *
024920	     MOVE SAL-EXT-ITEM	 TO STK-CODE PRT-EXT-ITEM.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE "N"		 TO WS-PARTS.
019830	   IF NOT (STK-IND = 2)
	       MOVE 0		 TO WS-ACTION
	       PERFORM AY05 THRU AY59
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the quantity is less than required and the item is     ³
      *    ³ produced then draw parts from production template.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF STK-QUANT < SAL-QNT
		   PERFORM READ-SPARTS THRU READ-SPARTS-EXIT
		   IF NOT(WS-F-ERROR = 21)
		       SUBTRACT STK-QUANT FROM SAL-QNT
				 GIVING WS-PQUANT
		       ADD WS-PQUANT
				 TO STK-QUANT
		       MOVE "Y"	 TO WS-PARTS
		   END-IF
	       END-IF
	       SUBTRACT SAL-QNT	 FROM STK-QUANT
	       ADD SAL-QNT	 TO STK-QJOB
	       PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
	   IF WS-PARTS = "Y"
	       PERFORM BF248 THRU BF254.
030200       GO TO BF235.
      *
      *    ****   R E M A R K S   /   I N T E R N A L	N O T E S
      *
030210 BF247.
	   IF SAL-TYPE = "I"
030220	       MOVE 4		 TO JOB-TYPE
	   ELSE
030220	       MOVE 3		 TO JOB-TYPE.
030230	     INITIALIZE JOB-DETAIL.
030240       MOVE SAL-DESC       TO JOB-DESC.
030250       ADD 1               TO JOB-SEQ.
             MOVE ZERO           TO JOB-SUB.
030260       GO TO BF225.

       BF248.
	     MOVE JOB-EXT-ITEM	 TO PRT-EXT-ITEM.
020530       PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 21
               GO TO BF254.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
021370       MOVE 1              TO WS-S6.

021420 BF249.
021430	   IF PRT-EXT-CODE(WS-S6) = SPACES
021440         ADD 1             TO WS-S6
021450         GO TO BF250.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520         GO TO BF250.
022580       MOVE 2              TO JOB-TYPE.
022590	     INITIALIZE JOB-DETAIL.
	     MOVE STK-CODE	 TO JOB-EXT-ITEM.
021540       MOVE STK-DESC       TO JOB-DESC.
	     MULTIPLY WS-PQUANT BY PRT-QUANT (WS-S6)
                                 GIVING JOB-QNT.
022610       ADD 1               TO JOB-SEQ.
             MOVE ZERO           TO JOB-SUB.
019830	   IF NOT (STK-IND = 2)
	       MOVE 0		 TO WS-ACTION
	       PERFORM AY05 THRU AY59
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Update the quantities on the stock record (Reduce quantity³
      *    ³ on stock record)					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       SUBTRACT JOB-QNT	 FROM STK-QUANT
	       PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
	     PERFORM BF225.
021600	     ADD 1		 TO WS-S6.

021660 BF250.
021670     IF WS-S6 < 31
021680         GO TO BF249.

       BF254.
             EXIT.
      *
      *    ****    J O B   T O T A L S
      *
030270 BF255.
014610	     PERFORM READ-JOBHDR-LOCK THRU READ-JOBHDR-EXIT.
030300       MOVE 0              TO WS-ACTION.
030310	     PERFORM AY21 THRU AY59.
030320	     MOVE W90-DB	 TO JHD-SUBTOT.
030330       MULTIPLY -1 BY W90-TDISC 
030340				 GIVING JHD-DSCNT
030350	     MOVE W90-EXCTOT	 TO JHD-TAXABLE.
030360	     MOVE W90-NONTAX	 TO JHD-NON-TXBL.
030370	     MOVE W90-TTAX	 TO JHD-TAX.
030380	     MOVE W90-TOTAL	 TO JHD-TOT.
030390	     MOVE W90-BAL	 TO JHD-BAL.
	     MOVE W10-COMMENT	 TO JHD-COMMENT.
	   IF W10-SMAN > ZERO
	       MOVE W10-SMAN	 TO JHD-SMAN.
030410	     PERFORM REWRITE-JOBHDR-UNLOCK THRU WRITE-JOBHDR-EXIT.
      *
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *
030420       PERFORM AY70 THRU AY999.
      *
      *    ****    U S E R   O P T I O N   T O	 P R I N T
      *
030540 BF260.
022810	     MOVE "Print job details (Y/N)?  [ ]"
				 TO WS-ERR-MES.
022870	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030620	   IF NOT (WS-OPTION = "Y" OR "N")
030630         GO TO BF260.
      *
      *    ****    C L O S E   F I L E
      *            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
      *
	     CLOSE SALE.
030640     IF WS-OPTION = "Y"
030650         DISPLAY CLR-SCREEN
030660         DISPLAY "AMEND JOB" AT 0235 WITH FOREGROUND-COLOR 15
030670         DISPLAY "Printing job details" AT 1212
030680                  WITH FOREGROUND-COLOR 14
030690	       MOVE W90-REF	 TO LS-SLS-JOBNO
	       MOVE LS-RET	 TO WS-OPTION
	       MOVE "R" 	 TO LS-RET
030750	       CALL "JCP\JCPPRN"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
030770	       CANCEL "JCP\JCPPRN"
	       MOVE WS-OPTION	 TO LS-RET.
030830       OPEN OUTPUT SALE.
030840       CLOSE SALE.
030450	     OPEN I-O SALE.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030970	   IF WS-OPTION = "E"
030980	       GO TO BF265.
030990	     GO TO BF05.

       BF265.
	     EXIT.
      *
      *    ****    A M E N D   J O B   D E T A I L S
      * 	   ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	   This routine reads the existing entries for the selected
      * 	   job and in addition to displaying them, will allow the
      * 	   user to amend or delete selected entries.
      *
       BF270A		SECTION 52.
031000 BF270.
031010       MOVE 8              TO LIN.
031020       PERFORM BF115 UNTIL LIN > 18.
031030       MOVE 8              TO LIN.
031040       MOVE 1              TO WS-LINES WS-FIRST.

031050 BF275.
029180	     MOVE WS-FIRST	 TO WS-SALKEY WS-SALPOS.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
031070 BF280.
031080	   IF WS-SALKEY > WS-LAST
	       SUBTRACT 1	 FROM WS-SALKEY
031090	       GO TO BF284.
031100       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
034280	   IF SAL-TYPE = "I" OR "R"
034290	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
	       DISPLAY (LIN, 5) "("
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
			SAL-TYPE WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 0
			")" WITH FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
031140	       GO TO BF282.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
034320	     DISPLAY (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
031170       MOVE SAL-QNT        TO W10-QUANT.
031180       DISPLAY S26-QNT AT LINE LIN.
031190       MOVE SAL-SELL       TO W10-SELL.
031200       DISPLAY S26-PRC AT LINE LIN.
	   IF SAL-TYPE = "P"
	       DISPLAY ( , 70) "Requisition"
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
	   ELSE
031210	       MOVE SAL-VAL	 TO W100-EXT
031220	       DISPLAY ( , 70) W100-EXT
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
031230 BF282.
031240     IF LIN < 18
031250         ADD 1             TO LIN WS-SALKEY WS-LINES
031260         GO TO BF280.

031270 BF284.
	     MOVE 7		 TO LIN.
	     DISPLAY CLEAR-L25.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
031290 BF285.
001020	     DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 14
					BACKGROUND-COLOR 3
001030		     " highlight," WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     "," WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3
001040		     "A" WITH FOREGROUND-COLOR 14
				  BACKGROUND-COLOR 3
001050		     "mend," WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001040		     "D" WITH FOREGROUND-COLOR 14
				  BACKGROUND-COLOR 3
001050		     "elete or " WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 14
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3.
      *
      *    ****    S A V E   C U R R E N T   P A G E
      *
	     PERFORM SAVE-SCREEN-2.
      *
      *    ****    S C R E E N	 A T T R I B U T E S
      *
       BF289.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *
      *    ****    G E T   U S E R   O P T I O N
      *
031830 BF290.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BF292
		 WHEN DOWN-KEY
001340		     GO TO BF293
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BF290
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO BF582
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
		 WHEN PAGE-DOWN
001340		     GO TO BF305
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
		 WHEN PAGE-UP
001340		     GO TO BF300
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BF290
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION = "A" OR "D") AND
		  (LIN > 7)
		   DISPLAY CLEAR-L25
		   GO TO BF310.
	     CALL X"E5".
001280	     GO TO BF290.
      *
      *    ****    U P	 K E Y	 ( M O V E   H I G H L I G H T	 U P )
      *
       BF292.
	   IF (WS-SALPOS = 1) OR (LIN < 8)
	       GO TO BF294.
	     SUBTRACT 1 	 FROM WS-SALPOS.
	   IF LIN > 8
	       SUBTRACT 1	 FROM LIN
	       GO TO BF294.
	     MOVE 18		 TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      * 	    O N E   L I N E
      *
       BF292-LOOP.
	     SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S7 > 9
	       SUBTRACT 1	 FROM WS-S7
	       GO TO BF292-LOOP.
	     MOVE WS-SALPOS	 TO WS-SALKEY WS-FIRST.
      *
      *    ****    C L E A R   F I R S T   O R	 L A S T
      * 	   D E T A I L	 L I N E.
      *
       BF292-DISPLAY.
	     PERFORM RESTORE-SCREEN-2.
023890	     DISPLAY (LIN, 1) WS-JOB1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-JOB78 WITH FOREGROUND-COLOR 3
			      WS-JOB80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
      *
      *    ****    G E T   N E X T   O R   P R E V I O U S
      * 	   D E T A I L	 L I N E.
      *
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
034280	   IF SAL-TYPE = "I" OR "R"
034290	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
	       DISPLAY (LIN, 5) "("
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
			SAL-TYPE WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 0
			")" WITH FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
030120	       GO TO BF292-END-DISP.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
030140	     DISPLAY (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030150       MOVE SAL-QNT        TO W10-QUANT.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY ( , 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.

       BF292-END-DISP.
	     PERFORM SAVE-SCREEN-2.
	     GO TO BF294.
      *
      *    ****    D O W N   K E Y   ( M O V E	 H I G H L I G H T
      * 			       D O W N )
      *
       BF293.
	   IF (WS-SALPOS = WS-LAST) AND (LIN > 7)
	       GO TO BF294.
      *
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *
	   IF LIN > 7
	       ADD 1		 TO WS-SALPOS.
	   IF LIN < 18
	       ADD 1		 TO LIN
	       GO TO BF294.
	     MOVE 8		 TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      * 	    L I N E
      *
       BF293-LOOP.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BF293-LOOP.
	     ADD 1		 TO WS-FIRST.
	     MOVE WS-SALPOS	 TO WS-SALKEY.
	     GO TO BF292-DISPLAY.
      *
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *
001980 BF294.
001990	     PERFORM BF289.
	   IF LIN > 7
002000	       DISPLAY (LIN, 1) W43-DET (LIN)
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
002010	     GO TO BF290.
      *
      *    ****    C L E A R   S C R E E N
      *
032090 BF295.
032100	     MOVE 8		 TO LIN.
032110       PERFORM BF115 UNTIL LIN > 18.
032120       MOVE 8              TO LIN.
032130       MOVE 1              TO WS-LINES.
      *
      *    ****   P R E V I O U S   S C R E E N	  R E Q U I R E D
      *
       BF300.
	   IF WS-FIRST = 1
	       GO TO BF290.
	   IF WS-FIRST > 11
	       SUBTRACT 11	 FROM WS-FIRST
	   ELSE
	       MOVE 1		 TO WS-FIRST.
	     PERFORM BF295.
	     GO TO BF275.
      *
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *
       BF305.
	   IF (WS-FIRST + 11) > WS-LAST
	       GO TO BF290.
	     ADD 11		 TO WS-FIRST.
	     PERFORM BF295.
	     GO TO BF275.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
       BF310.
032150     IF WS-OPTION = "A"
032160	       GO TO BF320.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *
032200 BF315.
043780	     MOVE "Are you sure Y/N?  [N]"
				 TO WS-ERR-MES.
030880	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
032280	   IF NOT (WS-OPTION = "N" OR "Y")
032290	       GO TO BF315.
032310	   IF WS-OPTION = "N"
032320         MOVE WS-LINE      TO WS-LINES
032330	       GO TO BF285.
032340       MOVE "D"            TO WS-OPTION.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
       BF320.
032550	     MOVE WS-SALPOS	 TO WS-SALKEY.
032560       PERFORM READ-SALE THRU READ-SALE-EXIT.
032570	   IF SAL-TYPE = "I" OR "R" OR "P"
032580	       GO TO BF322.
032590	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM STK-CODE.
032600	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
032610	     MOVE SAL-QNT	 TO W10-QUANT.
032620       MOVE SAL-COST       TO W10-COST.
032630       MOVE SAL-SELL       TO W10-SELL.
032640       MOVE SAL-DISC       TO W90-CDISC.
032650       MOVE SAL-DSC        TO WS-DISC.
032660       MOVE SAL-VAL        TO W25-VALUE.
	     MOVE SAL-VAT-SUB	 TO WS-VAT-SUB.
      *
      *    ****    R E D U C E   T O T A L S 
      *
032670       SUBTRACT W25-VALUE  FROM W90-DB.
032680       SUBTRACT W90-CDISC  FROM W90-LDISC.
032690       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF WS-VAT-SUB > ZERO
032700	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(SAL-VAT-SUB).
019830     IF (STK-IND < 2) OR (STK-IND = 3)
032720         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
032730 BF322.
	     PERFORM BF289.
032740     IF WS-OPTION = "A"
032750         GO TO BF325.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
032760	   IF SAL-TYPE = "S" OR "L"
032770	       PERFORM BF100 THRU BF102.
      *
      *    ****    D E L E T E   I T E M   F R O M   O R D E R   F I L E 
      *
032780       ADD 1               TO W14-SUB.
032790       MOVE SAL-SQN        TO W14-SQN (W14-SUB).
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
032800 BF324.
032810	     MOVE WS-SALKEY	 TO WS-SALSTR.
032820       ADD 1               TO WS-SALKEY.
032830     IF WS-SALKEY > WS-LAST
032840         SUBTRACT 1        FROM WS-LAST
032850	       GO TO BF270.
032860       PERFORM READ-SALE THRU READ-SALE-EXIT.
032870       SUBTRACT 1          FROM SAL-CODE.
032880       MOVE WS-SALSTR      TO WS-SALKEY.
032890       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
032900       ADD 1               TO WS-SALKEY.
032910	     GO TO BF324.

032920 BF325.
	     PERFORM BF289.
032930	   IF SAL-TYPE = "I" OR "R"
032940	       ACCEPT (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0 UPDATE AUTO
032960	       GO TO BF581.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
032970       ACCEPT S26-QNT AT LINE LIN.
032990     IF W10-QUANT = ZERO
025310	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033020	       MOVE SAL-QNT	 TO W10-QUANT
033030         GO TO BF325.
      *
      *    ****    A M E N D   S E L L I N G   P R I C E
      *
	     PERFORM BF66 THRU BF75.
      *
      *    ****    R E C A L C U L A T E   V A L U E S
      *
033040       MULTIPLY W10-SELL BY W10-QUANT 
033050                           GIVING W25-VALUE.
033060       MOVE W25-VALUE      TO W100-EXT.
033070	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     PERFORM CHECK-CORRECT.
033160	   IF WS-OPTION = "N"
033170         GO TO BF325.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
033180       MOVE W10-QUANT      TO SAL-QNT.
             MOVE W10-SELL       TO SAL-SELL.
033190       MOVE W25-VALUE      TO SAL-VAL.
033200     IF WS-DISC = ZERO
033210         GO TO BF330.
033220       COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
033230       MOVE W90-CDISC      TO SAL-DISC.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
033240 BF330.
033250	     PERFORM BF95 THRU BF102.

033260 BF581.
033280       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
033290       GO TO BF270.
      *
      *    ****    C L E A R   D E T A I L   L I N E S
      *
031940 BF582.
	     PERFORM BF295.
      *
      *    ****    D I S P L A Y   L A S T   P A G E
      *
	   IF WS-LAST > 10
	       SUBTRACT 9 FROM WS-LAST
				 GIVING WS-SALKEY
	   ELSE
	       MOVE 1		 TO WS-SALKEY.

030050 BF583.
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
034280	   IF SAL-TYPE = "I" OR "R"
034290	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
	       DISPLAY (LIN, 5) "("
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
			SAL-TYPE WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 0
			")" WITH FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
030120	       GO TO BF584.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
030140	     DISPLAY (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030150	     MOVE SAL-QNT	 TO W10-QUANT.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY ( , 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.

       BF584.
	     ADD 1		 TO LIN.
030060	   IF WS-SALKEY < WS-LAST
	       ADD 1		 TO WS-SALKEY
030070	       GO TO BF583.

033300 BF590.
033310	     EXIT.

       BF599.
034440       EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
038140 ZA000-INIT	       SECTION 90.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ D O   N O T   D I S P L A Y   E R R O R   M E S S A G E S ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** J O B	C O S T I N G	-   D E T A I L S   F I L E ****
      *    *************************************************************
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
      *    *************************************************************
      *    ****	 J O B	 C O S T I N G	 -   H E A D E R   F I L E  ****
      *    *************************************************************
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	    R E C O V E R Y   F I L E		    ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
      *    *************************************************************
      *    ****    S A L E S   F I L E	-  J O B   C O S T I N G    ****
      *    *************************************************************
	     MOVE "SALEJ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *	   *************************************************************
      *    *** S T O C K   ( I V E N T O R Y )	 P A R T S   F I L E ***
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    ***	S T O C K   ( I V E N T O R Y )   M E M O   F I L E  ***
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    ****      S T O C K	 ( I V E N T O R Y )   F I L E	    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
038300	     MOVE 3		 TO WS-PARKEY.
038310       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
038430	     MOVE PAR-VAT-REG	 TO W05-VATNO.
038440       OPEN OUTPUT RECOVER.
038450       CLOSE RECOVER.
038460       OPEN I-O RECOVER.
038940	     OPEN OUTPUT SALE.
038950       CLOSE SALE.
038960       OPEN I-O SALE.
039390	     MOVE 5		 TO WS-PARKEY.
039400       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039430	     MOVE PAR-DISCOUNT	 TO W15-DISCOUNT.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
039440	     MOVE 1		 TO WS-PARKEY.
039450       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039460       MOVE PAR-DMY        TO W12-TODAY.
039470       MOVE PAR-YMD        TO W12-T-YMD.
039480       MOVE PAR-COMPANY    TO W95-COMP.
039500       MOVE PAR-TELEPHONE  TO W95-TEL.
039770       GO TO ZA999.
039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
040090 ZA60.
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
040660 ZA200.
040670       DISPLAY "Files locked - Try later" AT 2312
040680                WITH FOREGROUND-COLOR 14
040690               " " WS-STATUS WITH FOREGROUND-COLOR 15.
040700       DISPLAY "Press any key" AT 2512 
040710		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040730 ZA205.
040740       EXIT PROGRAM.
040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION	91.
040790 OPEN-ERROR.
040800       DISPLAY CLR-SCREEN.
040810       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
040820       PERFORM DISPLAY-FILE-NAME.
040830
040840 READ-ERROR.
040850       DISPLAY CLR-SCREEN.
040860       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
040870       PERFORM DISPLAY-FILE-NAME.
040880
040890 WRITE-ERROR.
040900       DISPLAY CLR-SCREEN.
040910       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
040920       PERFORM DISPLAY-FILE-NAME.
040930

040950 DISPLAY-FILE-NAME.
040960     IF WS-F-ERROR = 2
040970         MOVE W02-NETWORK TO WS-FILE
040980         MOVE WS-NETKEY TO WS-KEY
041030	   ELSE
041040     IF WS-F-ERROR = 6
041050         MOVE W02-DEBTOR   TO WS-FILE
041060         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041070     ELSE
041080     IF WS-F-ERROR = 7
041090         MOVE W02-DEPART   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041230	   ELSE
041240     IF WS-F-ERROR = 13
041250         MOVE W02-JOBCST   TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
               MOVE JOB-KEY      TO WS-KEYX
041110	   ELSE
041240	   IF WS-F-ERROR = 14
041250	       MOVE W02-JOBHDR	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JHD-KEY2	 TO WS-KEYX
041110	   ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041150     ELSE
041160     IF WS-F-ERROR = 18
041170         MOVE W02-RECOVER  TO WS-FILE
041180         MOVE WS-RECKEY    TO WS-KEY
041190     ELSE
041200     IF WS-F-ERROR = 19
041210         MOVE W02-SALE TO WS-FILE
041220         MOVE WS-SALKEY TO WS-KEY
041270     ELSE
041280     IF WS-F-ERROR = 21
041290         MOVE W02-SPARTS   TO WS-FILE
041300         MOVE ZERO         TO WS-KEY
	       MOVE PRT-EXT-ITEM TO WS-KEYX
041310	   ELSE
041320     IF WS-F-ERROR = 22
041330         MOVE W02-STOCKF   TO WS-FILE
041340         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.
041350	   IF WS-STATUS = "10"
041360         MOVE "End of FILE" TO WS-STAT-MESSAGE
041370     ELSE
041380     IF WS-STATUS = "22"
041390         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041400     ELSE
041410     IF WS-STATUS = "23"
041420         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041430     ELSE
041440     IF WS-STATUS = "24"
041450         MOVE "DISK full" TO WS-STAT-MESSAGE
041460     ELSE
041470     IF WS-STATUS = "30"
041480         MOVE "DISK error" TO WS-STAT-MESSAGE
041490     ELSE
041500     IF WS-STATUS = "94"
041510         MOVE "FILE locked" TO WS-STAT-MESSAGE.
041520       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
041530       DISPLAY "Status " AT 1212 
041540                WS-STATUS WITH FOREGROUND-COLOR 11
041550               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
041560     IF WS-STATUS NOT = "94" 
041570         DISPLAY "Key    " AT 1412 
041580                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
041590         DISPLAY "Reverse backup or contact program Support"
041600                  AT 1612.
041610         DISPLAY "Please make a note of these details" AT 1812.
041620       STOP RUN.
