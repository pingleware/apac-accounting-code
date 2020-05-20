      $SET LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	   ****	      **       **      *
      *   **	**     **     **    **	  **  **     ***      ***      *
      *   **	**     **     **    **	 **    **     **       **      *
      *   **	**     **     *******	 **    **     **       **      *
      *   **	**     **     **	 **    **     **       **      *
      *   **	**     **     **	  **  **      **       **      *
      *   *******      **     **	   ****	    ******   ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      * 	A C C O U N T S	  R E C E I V A B L E	/	       *
      *                                                                *
      *   D E B T O R	P O S T   A R R E A R	I N T E R E S T        *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP011.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2011
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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

000160 COPY DBTRAN.SL.

000140 COPY DEBTOR.ISL.

000140 COPY DEPART.SL.

       COPY LEDTRF.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

       COPY TXTRAN.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000330 COPY DBTRAN.FDE.

000310 COPY DEBTOR.FDE.

000140 COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000290 COPY PARAM.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

       COPY TXTRAN.FDE.

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
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
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
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001720 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
001760 77  WS-PERIOD	   PIC	9(02).
001770 77  WS-NTP          PIC  X(01).
001750 77  WS-FIN-RATE     PIC S9(03)V99 COMP-3.
001800 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
001810 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
001820 77  WS-TAX          PIC  X(01).
001820 77  WS-TYPE         PIC  X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
001870 77  WS-EKEY         PIC  9(06)    COMP-5.
001880 77  WS-CHAR1        PIC  X(01).
001890 77  WS-CHAR2        PIC  X(01).
001900 77  WS-COMP         PIC  9(01).
001980 77  WS-BATCH        PIC  9(06) COMP-5.
002010 77  WS-ERR1	   PIC	X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002100 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002110 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002120 77  WS-M            PIC  X(17) VALUE "New batch (Y/N) ?".
002130 77  WS-AC           PIC  X(07) VALUE "ACCOUNT".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
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
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
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

       COPY LEDTRF.WS.

002250 01  W00-TRAN-CODES.
           03  W00-T-VAT   PIC  9(09)V99 COMP-3.
001410     03  W00-E-DESC  PIC  X(12).
001420     03  W00-A-DESC  PIC  X(12).
001430     03  W00-ACTION  PIC  9(01).
001440     03  W00-T-CODE  PIC  9(02)    COMP-5.
001450     03  W00-T-GL    PIC  9(06)    COMP-3.
001460     03  W00-T-DAY   PIC  9(06)    COMP-5.
001470     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
001480     03  W00-T-MTD   PIC  9(06)    COMP-5.
001490     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
001500     03  W00-T-YTD   PIC  9(06)    COMP-5.
001510     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DBTRAN.ID.

       COPY DEBTOR.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY TXTRAN.ID.

       01  W02-PRINTER-DETAILS.
	   03  W02-PRINTER     PIC  X(12).
002570	   03  W02-PGE-LENGTH  PIC  9(02).
001870	   03  W02-PRN-LENGTH  PIC  9(02).
	   03  W02-LINAGE      PIC  9(02).
	   03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).

	   03  R-SL1 REDEFINES W02-PRN-LINE.
	       05  R-SLA       PIC  X(78).
	       05  FILLER      PIC  X(58).
	   03  R-SL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(58).
	       05  R-SLB       PIC  X(78).
000350	   03  R-L1 REDEFINES W02-PRN-LINE.
000360	       05  R-DET1      PIC  X(136).
000370	   03  R-L2 REDEFINES W02-PRN-LINE.
000380	       05  R-DET2      PIC  X(80).
000390	   03  R-L3 REDEFINES W02-PRN-LINE.
000400	       05  R-DATE      PIC  X(06).
000410	       05  R-DTE       PIC  Z9/99/9999.
000420	       05  FILLER      PIC  X(04).
000430	       05  R-H5        PIC  X(46).
000440	       05  R-H6        PIC  X(07).
000450	       05  R-BATCH     PIC  Z(05)9.
000460	       05  FILLER      PIC  X(07).
	       05  R-PAGE      PIC  X(06).
	       05  R-P-NO      PIC  Z(03)9.
	   03  R-L3A REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(04).
	       05  R-CO        PIC  X(40).
	       05  FILLER      PIC  X(04).
000470	   03  R-L4 REDEFINES W02-PRN-LINE.
000480	       05  FILLER      PIC  X(11).
000490	       05  R-H1        PIC  X(04).
000500	       05  FILLER      PIC  X(06).
000510	       05  R-H2        PIC  X(15).
000520	       05  FILLER      PIC  X(06).
000530	       05  R-H3        PIC  X(15).
000540	       05  FILLER      PIC  X(06).
000550	       05  R-H4        PIC  X(06).
000560	       05  FILLER      PIC  X(11).
000570	   03  R-L5 REDEFINES W02-PRN-LINE.
000580	       05  FILLER      PIC  X(12).
000590	       05  R-TRAN      PIC  9(02).
000600	       05  FILLER      PIC  X(09).
000610	       05  R-ENG       PIC  X(12).
000620	       05  FILLER      PIC  X(09).
000630	       05  R-AFR       PIC  X(12).
000640	       05  FILLER      PIC  X(10).
000650	       05  R-ACT       PIC  X(01).
000660	       05  FILLER      PIC  X(13).
000740	   03  R-L7 REDEFINES W02-PRN-LINE.
000750	       05  R-H7        PIC  X(07).
	       05  R-H7A       PIC  X(27).
000760	       05  R-H8        PIC  X(07).
000770	       05  R-H10       PIC  X(09).
000780	       05  R-H11       PIC  X(15).
000790	       05  R-H12       PIC  X(14).
	       05  R-H13       PIC  X(11).
	       05  R-H14       PIC  X(06).
000800	   03  R-L8 REDEFINES W02-PRN-LINE.
000810	       05  R-TAC       PIC  X(06).
000820	       05  FILLER      PIC  X(01).
000830	       05  R-DNME      PIC  X(24).
000840	       05  FILLER      PIC  X(01).
000850	       05  R-TDTE      PIC  X(08).
000860	       05  FILLER      PIC  X(01).
	       05  R-TDESC     PIC  X(08).
	       05  FILLER      PIC  X(01).
000870	       05  R-TREF      PIC  X(08).
000880	       05  FILLER      PIC  X(01).
000890	       05  R-VAL       PIC  Z(07)9.99-.
001020	       05  FILLER      PIC  X(01).
	       05  R-TAX       PIC  Z(06)9.99-.
001020	       05  FILLER      PIC  X(01).
001040	       05  R-TOT       PIC  Z(07)9.99-.
000900	   03  R-L9 REDEFINES W02-PRN-LINE.
000910	       05  FILLER      PIC  X(54).
000920	       05  R-BTOT      PIC  Z(06)9.99-.
000930	       05  FILLER      PIC  X(15).
001090	   03  R-L10 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(10).
001100	       05  I-H8        PIC  X(09).
001110	       05  I-TOTTAX    PIC  Z(07)9.99-.
001120	       05  FILLER      PIC  X(02).
001130	       05  I-H9        PIC  X(08).
001140	       05  I-TOTEXE    PIC  Z(07)9.99-.
001150	       05  FILLER      PIC  X(02).
001160	       05  I-H10       PIC  X(08).
001170	       05  I-TOTGST    PIC  Z(07)9.99-.
001180	       05  FILLER      PIC  X(02).
001190	       05  I-H11       PIC  X(07).
001200	       05  I-TOTBCH    PIC  Z(07)9.99-.
001210	   03  TL-L3 REDEFINES W02-PRN-LINE.
001220	       05  FILLER      PIC  X(02).
001230	       05  TL-LEDG     PIC  9(03).
001240	       05  FILLER      PIC  X(01).
001250	       05  TL-NME      PIC  X(37).
001260	       05  FILLER      PIC  X(01).
001270	       05  TL-ACT      PIC  99/99/9999.
	       05  FILLER      PIC  X(01).
001280	       05  TL-PD       PIC  Z(05)9.99-.
001290	       05  FILLER      PIC  X(01).
001300	       05  TL-MTH      PIC  9(01).
001310	       05  FILLER      PIC  X(02).
001320	       05  TL-BAL      PIC  Z(05)9.99-.
001330	   03  I-L1 REDEFINES W02-PRN-LINE.
001340	       05 I-H1	       PIC  X(07).
001350	       05 I-H2	       PIC  X(35).
001360	       05 I-H3	       PIC  X(09).
001370	       05 I-H4	       PIC  X(14).
001380	       05 I-H5	       PIC  X(14).
001390	       05 I-H6	       PIC  X(11).
001400	       05 I-H7	       PIC  X(06).
001410	   03  I-L8 REDEFINES W02-PRN-LINE.
001420	       05 I-AC	       PIC  X(06).
001430	       05 FILLER       PIC  X(01).
001440	       05 I-NAME       PIC  X(31).
001450	       05 FILLER       PIC  X(01).
001460	       05 I-DATE       PIC  Z9/99/9999.
001470	       05 FILLER       PIC  X(01).
001480	       05 I-REF        PIC  X(08).
001490	       05 FILLER       PIC  X(01).
001500	       05 I-CAP        PIC  Z(07)9.99-.
001510	       05 FILLER       PIC  X(01).
001520	       05 I-GST        PIC  Z(06)9.99-.
001530	       05 FILLER       PIC  X(01).
001540	       05 I-TOT        PIC  Z(07)9.99-.

       COPY W05.VAT.

       01  W05-DISC.
000630	   03  W05-DISCOUNT.
000640         05  W05-DSETT   PIC S9(03)V99 COMP-3 OCCURS 10.

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC REDEFINES W08-CHAR
			       PIC  X(01) COMP-X OCCURS 6.

002500 01  W10-SPARES.
002510	   03  W10-DATE	    PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).
	   03  W10-REF1 REDEFINES W10-NUM.
	       05  FILLER      PIC  X(07).
	       05  W10-N1.
	       07  W10-NUM1    PIC  9(01).
	   03  W10-REF2 REDEFINES W10-NUM.
	       05  FILLER      PIC  X(06).
	       05  W10-N2.
	       07  W10-NUM2    PIC  9(02).
	   03  W10-REF3 REDEFINES W10-NUM.
	       05  FILLER      PIC  X(05).
	       05  W10-N3.
	       07  W10-NUM3    PIC  9(03).

       COPY W12.WS.

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

       01  W30-AGE-DATES.
	   03  W30-120	       PIC  9(08) COMP-3.
	   03  W30-90	       PIC  9(08) COMP-3.
	   03  W30-60	       PIC  9(08) COMP-3.
	   03  W30-30	       PIC  9(08) COMP-3.
	   03  W30-CUR	       PIC  9(08) COMP-3.

       COPY W40.DBT.

       COPY W40.WS.

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
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.

003750 01  W75-TITLES.
003760     03  W75-DESCRIP.
003770         05  FILLER      PIC X(28)      VALUE
003780                     "Mr     Mnr    Mrs    Mev    ".
003790         05  FILLER      PIC X(28)      VALUE
003800                     "Miss   Mej    Ms     Ms     ".
003810         05  FILLER      PIC X(28)      VALUE
003820                     "Dr     Dr     Rev    Ds     ".
003830         05  FILLER      PIC X(28)      VALUE
003840                     "Prof   Prof   The HonSy Edel".
003850         05  FILLER      PIC X(28)      VALUE
003860                     "                            ".
003870     03  W75-CODES REDEFINES W75-DESCRIP.
003880         05  W75-TITLE  OCCURS 10.
003890             07  W75-ETITLE  PIC X(07).
003900             07  W75-ATITLE  PIC X(07).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO        PIC X(06).
003940     03  W80-TYPE        PIC 9(01).

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

003960 01  W85-BATCHES.
003970         05  W85-BATCH   PIC  9(06)    COMP-5.

       COPY W90.DBT.

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004410     03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B       PIC Z(06)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.
004470     03  W95-POST        PIC 9(04).
004480     03  W95-TEL         PIC X(08).

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.  
004570     03  W100-KEY        PIC Z(05)9.

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

005520 01  S10.
005530	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
005540                           VALUE "DEBTORS ARREAR INTEREST".

007640 01  S27.
007650	   03  LINE  5 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007660                           VALUE "This routine does not include a
007670-                          "recovery procedure. Security".
007680	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007690                           VALUE "copies  of the data files should
007700-                          " be made as a power  failure".
007710	   03  LINE  7 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007720                           VALUE "or any other problem during this
007730-                          " procedure would require the".
007740	   03  LINE  8 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007750                           VALUE "DATA FILES from the previous bac
007760-                          "kup to be restored !!!      ".
007770     03  LINE 10 COLUMN 12 VALUE "Do you wish to continue (Y/N)?".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-DBLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM CF000.
013040	     CLOSE RECOVER.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

013140 AA49.
013150       EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY DEBTOR.TRN.

       COPY PRINTUTL.AS5.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY DBTRAN.RD1.

       COPY DEBTOR.RD.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY DBTRAN.WRR.

       COPY DEBTOR.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

       COPY TXTRAN.WR.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
010910 AY000           SECTION.
010920 AY01.
010930       MOVE 01             TO REC-FILE.
010940       MOVE WS-PARKEY      TO REC-KEY.
010950       MOVE PAR-RECORD1    TO REC-PARAM.
010960       GO TO AY50.
010970 AY02.
010980       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011000       MOVE DEB-RECORD1    TO REC-DEBTOR.
011010       GO TO AY50.
011070 AY04.
011080       MOVE 04             TO REC-FILE.
011090       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
011100       MOVE TRA-RECORD1    TO REC-DBTRAN.
011110       GO TO AY50.

       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.

       AY38.
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE.
	     MOVE DPT-RECORD	 TO REC-DEPART.
             GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

011070 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
011120 AY49.
011130       MOVE 99             TO REC-FILE.
011140       MOVE ZERO           TO REC-KEY.
011150       MOVE SPACES         TO REC-DETAIL.
011160       PERFORM AY50 THRU AY59.
011170       ADD 1               TO WS-TRANS.
011180     IF WS-RECOVER > 95
011190         CLOSE RECOVER
011200         OPEN OUTPUT RECOVER
011210         CLOSE RECOVER
011220         OPEN I-O RECOVER
011230         MOVE ZERO         TO WS-RECOVER.
011240       GO TO AY59.
011250 AY50.
011260	     ADD 1		 TO WS-RECOVER.
011270       MOVE WS-RECOVER     TO WS-RECKEY.
011280       MOVE WS-TRANS       TO REC-TRAN.
011290       WRITE REC-RECORD.
011300     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320                  WITH FOREGROUND-COLOR 14 WS-STATUS 
011330                  WITH FOREGROUND-COLOR 15
011340         STOP RUN.
011370 AY59.
011380       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012480	   IF WS-SKIP = "A"
012490         GO TO AY999.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *	     MOVE SHR-STOCK	 TO WS-STOCK.
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
012620	     MOVE "DB"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010	     MOVE 1		 TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
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
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
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
      * 	   NETWORK record 2.
      *
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       IF WS-USUB < 24
		   ADD 1	 TO WS-USUB
		   GO TO AY72
	       ELSE
		   GO TO AY85.

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

       AY85.
013090	     COMMIT.

011860 AY999.
011870       EXIT.

       COPY DEBTOR.TRA.

       COPY LEDTRF.UPD.

      /
      *    ****    A R R E A R   I N T E R E S T
      *   
024740 CF000		   SECTION 5.
024750 CF00.
024760       DISPLAY CLR-SCREEN.
024770       DISPLAY S10.
024780       DISPLAY S27.
024790 CF-CHK.
024800	     ACCEPT WS-OPTION AT 1043
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024860	   IF NOT (WS-OPTION = "Y" OR "N")
024870         GO TO CF-CHK.
024880     IF WS-OPTION = "N"
024890         GO TO CF999.
024900       DISPLAY "Are you sure Y/N" AT 1212.
024910       ACCEPT WS-OPTION AT 1230 WITH FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024970	   IF WS-OPTION = "N"
024980         GO TO CF999.
024990 CF00A.
025000       MOVE 5              TO WS-PARKEY.
025010       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025020       MOVE PAR-FIN-RATE   TO WS-FIN-RATE.
025030       MOVE PAR-PERIOD     TO WS-PERIOD.
             MOVE "N"            TO WS-SKIP.
             PERFORM AY60 THRU AY999.
025050     IF WS-PERIOD = ZERO OR WS-FIN-RATE = ZERO
025060         GO TO CF999.
025080       DISPLAY "Busy posting arrear interest" AT 1412.
025090       DISPLAY "Account No:" AT 1612.
025100	     MOVE ZERO		 TO W90-DB W90-VATTOT
				    W90-CR W90-EX W40-SDISC.
	     MOVE 1		 TO WS-S9.

       CF02.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO CF02.
025200	     MOVE W12-TODAY	 TO W90-DTE.
025210       MOVE W12-T-YMD      TO W20-DATE.
025220       MOVE 20             TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-RECORD.
025230       MOVE W00-E-DESC     TO W90-DESC.
025240       MOVE SPACES         TO W90-REF.
025260       MOVE DEB-BATCH      TO W85-BATCH.
025270     IF DEB-BATCH = 999999
025280         MOVE 1            TO DEB-BATCH
025290     ELSE
025300         ADD 1             TO DEB-BATCH.
025310       MOVE W85-BATCH      TO WS-BATCH.
025320       PERFORM CG06 THRU CG15.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
             MOVE " !"           TO DEB-NAME.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CF15.
       CF05.
025550       PERFORM READ-DEBTOR-NEXT-LOCK THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CF15.
025560     IF DEB-FIN = 1
025570         GO TO CF12.
	   IF WS-PERIOD = 90
025580	       ADD DEB-120
		   DEB-90
025630		   DEB-60	 GIVING W25-TOTAL
	   ELSE
	   IF WS-PERIOD = 60
025580	       ADD DEB-120
		   DEB-90
025630		   DEB-60
025790		   DEB-30	 GIVING W25-TOTAL
	   ELSE
025580	       ADD DEB-120
		   DEB-90
025630		   DEB-60
025790		   DEB-30
025810		   DEB-CUR	 GIVING W25-TOTAL.
025820 CF10.
025830	     DISPLAY DEB-ACNO AT 1624
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
025840     IF W25-TOTAL = ZERO
025850         GO TO CF12.
025860     IF W25-TOTAL > DEB-BAL
025870         MOVE DEB-BAL      TO W25-TOTAL.
025880       COMPUTE W90-DB = W25-TOTAL * WS-FIN-RATE / 100.
025890     IF W90-DB < .05
025900         GO TO CF12.
	     MOVE W90-DB	 TO W90-EX.
025910       MOVE DEB-ACNO       TO W80-ACNO.
             MOVE DEB-TYPE       TO W80-TYPE.
025920       MOVE DEB-NAME       TO W60-NAME.
025930       INSPECT W60-NAME REPLACING FIRST "*" BY ".".
025940       INSPECT W60-NAME REPLACING FIRST "*" BY " ".
025950       INSPECT W60-NAME REPLACING FIRST "!" BY " ".
025960	     DISPLAY W60-NAME AT 1630
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
025970	     MOVE "A"		 TO WS-SKIP.
025980       PERFORM GENERATE-DEBTOR-TRAN.
025990       ADD W90-DB          TO W90-BDT.
026000       PERFORM CG61.
	     DISPLAY WS-BLNK78 AT 1602 WITH FOREGROUND-COLOR 3.
025090       DISPLAY "Account No:" AT 1612.
       CF12.
             UNLOCK DEBTOR.
             GO TO CF05.
026020 CF15.
026250       MOVE "N"            TO WS-SKIP.
026190       MOVE "Total Interest:" 
                                 TO R-DNME.
026200       MOVE W90-DTE        TO R-TDTE.
026210       MOVE W90-BDT        TO R-TOT.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026230	     MOVE SPACES	 TO R-DET1.
026240       PERFORM CG070-NEW.
             PERFORM AY70 THRU AY999.

026290 CF999.
026300       EXIT.
      /
       CG000	       SECTION 5.
       CG06.
026480	     MOVE ZERO		 TO W90-BDT W90-30
026510                              W90-BCR W90-60 
                                    W90-CUR W90-90 
                                    W90-INT W90-DISC
                                    W90-120 W40-SDISC.
       CG08.
026560	     MOVE 0		 TO WS-PAGE.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.

026570 CG10.
      *
      *    ****    Print the audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
026580	     MOVE SPACES	 TO R-DET1.
026590       MOVE "PAGE:"        TO R-PAGE.
026600       MOVE "DATE:"        TO R-DATE.
026610       ADD 1               TO WS-PAGE.
026620       MOVE W12-TODAY      TO R-DTE.
026630       MOVE WS-PAGE        TO R-P-NO.
026700       MOVE "DEBTOR TRANSACTIONS - AUDIT TRAIL" 
                                 TO R-H5.
026680       MOVE "BATCH:"       TO R-H6.
026690       MOVE WS-BATCH       TO R-BATCH.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026660	     MOVE SPACES	 TO R-DET1.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET1.
015410       MOVE W95-COMP       TO R-CO.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

026670 CG15.
	     MOVE "H"		 TO W02-PRN-TYPE.
026720	     MOVE SPACES	 TO R-DET1.
026730       MOVE "AC.NO"        TO R-H7.
026740       MOVE "DEBTOR NAME"  TO R-H7A.
026750       MOVE "DATE"         TO R-H8.
026760       MOVE "TRAN TYPE"    TO R-H10.
026770       MOVE "REF"          TO R-H11.
026780       MOVE "VALUE"        TO R-H12.
026790       MOVE "VAT"          TO R-H13.
026790       MOVE "TOTAL"        TO R-H14.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026810	     MOVE SPACES	 TO R-DET1.
	     MOVE "D"		 TO W02-PRN-TYPE.

030810 CG61.
030820       MOVE DEB-ACNO       TO R-TAC.
030830       MOVE W60-NAME       TO R-DNME.
030840       MOVE W90-DTE        TO R-TDTE.
030860       MOVE W90-DESC       TO R-TDESC.
030870       MOVE W90-REF        TO R-TREF.
             MOVE W90-VAL        TO R-VAL.
	     MOVE W90-VAT(1)	 TO R-TAX.
030880     IF W90-DB > 0
030890         MOVE W90-DB       TO R-TOT
030900     ELSE
030910         MULTIPLY -1 BY W90-CR 
                                 GIVING R-TOT.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
030960	       PERFORM CG10 THRU CG15.
030970       MOVE SPACES         TO R-DET1.
030980       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.

031230 CG070-NEW.
031320       SUBTRACT W90-BCR FROM W90-BDT 
                                 GIVING W25-VALUE.
031350       ADD W25-VALUE       TO DEB-OUT.
031360       ADD W90-BDT         TO DEB-DT.
031370       ADD W90-BCR         TO DEB-CR.
031380       SUBTRACT W90-120    FROM DEB-FOUR.
031380       SUBTRACT W90-90     FROM DEB-THREE.
031390       SUBTRACT W90-60     FROM DEB-TWO.
031400       SUBTRACT W90-30     FROM DEB-ONE.
031410       SUBTRACT W90-CUR    FROM DEB-MTD.
031830       SUBTRACT W90-INT    FROM DEB-INTEREST.

031870 CG70A.
	     MOVE 9		 TO WS-PARKEY.
031900	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
	     MOVE "VAT1"	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             PERFORM AY38 THRU AY59.
           IF W90-VAL NOT = ZERO
031880	       IF W90-TX = ZERO
                   IF W90-DB = ZERO
                       ADD W90-VAL 
                                 TO PAR-CNGS-DAY
                                    PAR-CNGS-MTD
                                    PAR-CNGS-YTD
                   ELSE
                       ADD W90-VAL 
                                 TO PAR-DNGS-DAY
                                    PAR-DNGS-MTD
                                    PAR-DNGS-YTD
               ELSE
               IF W90-DB = ZERO
                   ADD W90-VAL   TO PAR-CVAT-DAY
                                    PAR-CVAT-MTD
                                    PAR-CVAT-YTD
				    DPT-V-RET-DAY
				    DPT-V-RET-MTD
				    DPT-V-RET-YTD
		   ADD W90-TX	 TO DPT-O-VRET-DAY
				    DPT-O-VRET-MTD
				    DPT-O-VRET-YTD
                   IF WX-INT = "Y"
		       COMPUTE WX-VALUE = W90-TX * -1
		       MOVE DPT-O-VAT-GL TO WX-AC
                       MOVE 11           TO WX-TYPE
                       PERFORM GL-TRF-UPDATE
                   END-IF
               ELSE
                   ADD W90-VAL   TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD
				    DPT-V-SALES-DAY
				    DPT-V-SALES-MTD
				    DPT-V-SALES-YTD
		   ADD W90-TX	 TO DPT-O-VAT-DAY
				    DPT-O-VAT-MTD
				    DPT-O-VAT-YTD
                   IF WX-INT = "Y"
		       COMPUTE WX-VALUE = W90-TX * -1
		       MOVE DPT-O-VAT-GL
                                     TO WX-AC
                       MOVE 10       TO WX-TYPE
                       PERFORM GL-TRF-UPDATE.
031930       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
031930       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

031940 CG70B.
031860	     PERFORM AY70 THRU AY999.

      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
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
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.

042170 ZA02.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
042720       MOVE 1              TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
042520	     MOVE 5		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DISCOUNT   TO W05-DISCOUNT.
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
042520	     MOVE 6		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
001070       MOVE PAR-DEBGL      TO WX-DEBGL.
001080       MOVE PAR-CREGL      TO WX-CREGL.
001090       MOVE PAR-INTGL      TO WX-INTGL.
001100       MOVE PAR-BNKGL      TO WX-BNKGL.
001110       MOVE PAR-UNPROF     TO WX-UNPROF.
001120       MOVE PAR-REDGL      TO WX-REDGL.
001130       MOVE PAR-ADJGL      TO WX-ADJGL.
001140       MOVE PAR-RLGL       TO WX-RLGL.
001150       MOVE PAR-DSGL       TO WX-DSGL. 
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
             MOVE W12-T-YMD      TO W22-DTE2.
      *	   IF W22-DD2 > 27
      *        ADD 1		 TO W22-MM2.
       ZA47.
           IF W22-MM2 > 01
               SUBTRACT 1        FROM W22-MM2
           ELSE
               MOVE 12           TO W22-MM2
	       IF W22-YY2 > ZERO
		   SUBTRACT 1	 FROM W22-YY2
	       ELSE
		   SUBTRACT 1	 FROM W22-CC2
		   MOVE 99	 TO W22-YY2.
	   IF W22-MM2 = 02
               MOVE 28           TO W22-DD2
           ELSE
           IF W22-MM2 = 04 OR 06 OR 09 OR 11
               MOVE 30           TO W22-DD2
           ELSE
               MOVE 31           TO W22-DD2.
       ZA48.
           IF W12-MM = W22-MM2
               IF W12-DD < W22-DD2
                   MOVE W12-DD   TO W22-DD2.
             MOVE W22-DTE2       TO W30-CUR.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-30.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-60.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-90.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-120.
044780	     GO TO ZA999-EXIT.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
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
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
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
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 8.
      
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
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840	   ELSE
041810	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER TO WS-FILE
041870         MOVE WS-RECKEY TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX.
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
