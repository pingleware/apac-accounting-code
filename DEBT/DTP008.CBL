      $SET LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	    ****     ******    *
      *   **	**     **     **    **	 **  **    **  **   **	  **   *
      *   **	**     **     **    **	**    **  **	**  **	  **   *
      *   **	**     **     *******	**    **  **	**   ******    *
      *   **	**     **     **	**    **  **	**  **	  **   *
      *   **	**     **     **	 **  **    **  **   **	  **   *
      *   *******      **     **	  ****	    ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      * 	A C C O U N T S	  R E C E I V A B L E	/	       *
      *                                                                *
      *   D E B T O R	I N V O I C E S   A N A L I S E D	       *
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
      *  October 2006	- Amended Accounts receivable ageing from a    *
      * 		  specific date to the actual date on which    *
      * 		  statements and month-end is run.	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP008.
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

000140 COPY DEBMEM.SL.

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

000140 COPY DEBMEM.FDE.

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
001750 77  WS-FIN-RATE     PIC S9(03)V99 COMP-3.
001760 77  WS-PERIOD       PIC  9(02).
001770 77  WS-NTP          PIC  X(01).
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
002010 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
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
	       05  WS-SPC  PIC	X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "008".

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

000140 COPY DEBMEM.ID.

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
000830	       05  R-DNME      PIC  X(23).
000840	       05  FILLER      PIC  X(01).
000850	       05  R-TDTE      PIC  X(10).
000860	       05  FILLER      PIC  X(01).
	       05  R-TDESC     PIC  X(07).
	       05  FILLER      PIC  X(01).
000870	       05  REP-TREF    PIC  X(08).
000880	       05  FILLER      PIC  X(01).
000890	       05  REP-TVAL    PIC  Z(07)9.99-.
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

       01  W15-CONTROL.
003540	   03  W15-LEDG       PIC  9(03)    COMP.

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
	   03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.

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

       COPY DEBLUP.CRT.

       COPY DEPART.CRT.

007910 01  S35.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(06)9.99- USING W40-INT AUTO.

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

007930 01  S50.
007940	   03  LINE  2 COLUMN 37 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "INVOICE".
007950     03  LINE  4 COLUMN  2 VALUE "Account No:".
007960     03          COLUMN 22 VALUE "Name:".
	   03	       COLUMN 28 BACKGROUND-COLOR 5
				 PIC  X(40) FROM WS-SPC.
007970     03  LINE  6 COLUMN  2 VALUE "Date:".
	   03	       COLUMN  8 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPC.
007980	   03	       COLUMN 22 VALUE "Invoice No:".
	   03	       COLUMN 34 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPC.
007990	   03  LINE  8 COLUMN 12 VALUE "Taxable:".
	   03	       COLUMN 21 BACKGROUND-COLOR 5
				 PIC  X(12) FROM WS-SPC.
008000	   03  LINE  9 COLUMN 12 VALUE "Exempt :".
	   03	       COLUMN 21 BACKGROUND-COLOR 5
				 PIC  X(12) FROM WS-SPC.
008010	   03  LINE 10 COLUMN 12 VALUE "Tax    :".
	   03	       COLUMN 21 BACKGROUND-COLOR 5
				 PIC  X(12) FROM WS-SPC.
008020	   03  LINE 12 COLUMN 12 VALUE "Total  :".
	   03	       COLUMN 21 BACKGROUND-COLOR 5
				 PIC  X(12) FROM WS-SPC.

008030 01  S51.
008040     03  S51A.
008050	       05  LINE  6 COLUMN  8 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 5
008060				     PIC Z9/99/9999 USING W90-IDTE AUTO.
008070	       05	   COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 5
008080                               PIC X(08) USING W90-REF AUTO.
008090	       05  LINE  8 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 5
008100                               PIC Z(08)9.99 USING W90-CAP AUTO.
008110	       05  LINE  9 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 5
008120				     PIC Z(08)9.99 USING W90-EX AUTO.
008130     03  S51AB.
008140	       05  LINE 10 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 5
008150				     PIC Z(06)9.99 USING W90-TX AUTO.
008160     03  S51B.
008170	       05  LINE 12 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 5
008180                               PIC Z(08)9.99 USING W90-DB AUTO.
008190     03  S51C.
008200	       05  LINE 12 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
008210                               PIC Z(08)9.99 USING W90-CR AUTO.

       01  S52.
           03  LINE  14 COLUMN 22 VALUE "Balance  :".
	   03		COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				  BACKGROUND-COLOR 5
                                  PIC Z(08)9.99- USING DEB-BAL.
           03  LINE  15 COLUMN 22 VALUE "Current  :".
	   03		COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				  BACKGROUND-COLOR 5
                                  PIC Z(08)9.99- USING W40-CUR.
           03  LINE  16 COLUMN 22 VALUE " 30 days :".
	   03		COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				  BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING DEB-30.
           03  LINE  17 COLUMN 22 VALUE " 60 days :".
	   03		COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				  BACKGROUND-COLOR 5
                                  PIC Z(08)9.99- USING DEB-60.
           03  LINE  18 COLUMN 22 VALUE " 90 days :".
	   03		COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				  BACKGROUND-COLOR 5
                                  PIC Z(08)9.99- USING DEB-90.
           03  LINE  19 COLUMN 22 VALUE "120 days :".
	   03		COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				  BACKGROUND-COLOR 5
                                  PIC Z(08)9.99- USING DEB-120.
           03  LINE  20 COLUMN 22 VALUE "Interest :".
	   03		COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				  BACKGROUND-COLOR 5
                                  PIC Z(08)9.99- USING DEB-INT.

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

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

       COPY UPDATE.CRT.

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
	     PERFORM CK000.
013040	     CLOSE RECOVER.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM DEBTOR-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY DEBTOR.TRN.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY DBTRAN.RD1.

       COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

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

000140 COPY DEBMEM.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

       COPY TXTRAN.WR.

       COPY DEBTOR.LUP.

       COPY DEPART.LUP.

       COPY APAC.HLP.

       COPY PRINTUTL.AS5.

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
             MOVE 38             TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
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
      *
      *    ****    Read debtor record - using account number.
      *
014260 CA155-GET-DEBTOR  SECTION 2.
014270 CA155.
012010       MOVE W80-ACNO       TO DEB-ACNO.
012020       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
014380     ELSE
014390     IF DEB-ACNO NOT = W80-ACNO
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420         MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
014430 CA160-EXIT.
014440       EXIT.

      /
      *    ****    G E N E R A T E   A   D E B T O R
      *            T R A N S A C T I O N
      *
000010 GENERATE-DEBTOR-TRAN SECTION 2.

000030 DEBTOR-TRAN.
000040	     PERFORM AY60 THRU AY999.
	     MOVE 0		 TO WS-ACTION.
000050	     PERFORM AY02 THRU AY59.

000140 LINK-DEBTOR-TRANS.
000230       INITIALIZE TRA-RECORD1.
000310       MOVE W20-DTE        TO TRA-DATE.
000320     IF W90-CODE = 01 OR 02
000330         MOVE W20-DTE      TO DEB-ACTIVE.
000340	   IF W90-CODE = 02
000350         MOVE W20-DTE      TO DEB-PMT
               MOVE W90-CR       TO DEB-LAST-PMT.
000360       MOVE W90-CODE       TO TRA-CODE.
000370       MOVE W90-REF        TO TRA-REF TRA-REG.
000380       MOVE DEB-ACNO       TO TRA-AC.
000390       SUBTRACT W90-CR FROM W90-DB 
000400                           GIVING TRA-VALUE.
           IF NOT (W90-CODE = 02 OR 15 OR 16 OR 20)
000410         IF W00-ACTION = 1
000420		   MULTIPLY -1 BY W90-VAT(1)
000430                           GIVING TRA-TAX
000440		   MULTIPLY -1 BY W90-EX
000450                           GIVING TRA-TAXFREE
000460         ELSE
000470		   MOVE W90-EX	 TO TRA-TAXFREE
000480		   MOVE W90-VAT(1)
				 TO TRA-TAX
               END-IF
000490         ADD TRA-TAX       TO W00-T-VAT W90-VATTOT.
000620	     MOVE TRA-TAX	 TO W90-TX.
           IF OPEN-ITEM
               IF TRA-VALUE < ZERO
                   MOVE W75-ALLOCATED
				 TO TRA-ALLOCATED
	       ELSE
	       IF NOT (TRA-DATE > W30-120)
		   MOVE 5	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-90)
		   MOVE 4	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-60)
		   MOVE 3	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-30)
		   MOVE 2	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-CUR)
		   MOVE 1	 TO TRA-AGE.
	   IF TRA-VALUE > ZERO
	       IF NOT (TRA-CODE = 17 OR 20) AND (TRA-AGE NOT = ZERO)
		   IF TRA-AGE = 5
		       ADD TRA-VALUE TO DEB-120
		       SUBTRACT TRA-VALUE
				     FROM W90-120
		   ELSE
		   IF TRA-AGE = 4
		       ADD TRA-VALUE TO DEB-90
		       SUBTRACT TRA-VALUE
				     FROM W90-90
		   ELSE
		   IF TRA-AGE = 3
		       ADD TRA-VALUE TO DEB-60
		       SUBTRACT TRA-VALUE
				     FROM W90-60
		   ELSE
		   IF TRA-AGE = 2
		       ADD TRA-VALUE TO DEB-30
		       SUBTRACT TRA-VALUE
				     FROM W90-30
		   ELSE
		   IF TRA-AGE = 1
		       ADD TRA-VALUE TO DEB-CUR
		       SUBTRACT TRA-VALUE
				     FROM W90-CUR.
000660	     PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
000900       MOVE 1              TO WS-ACTION.
000220       PERFORM AY04 THRU AY59.
000670     IF (TRA-TAX NOT = ZERO) OR 
              (TRA-CODE = 01) OR
000680        (TRA-CODE > 02 AND < 05) OR
000690        (TRA-CODE = 20) 
000700         MOVE 1            TO TAX-ACTYPE
000710         MOVE TRA-CODE     TO TAX-CODE
000720         MOVE TRA-DATE     TO TAX-DATE
000730         MOVE TRA-REF      TO TAX-REF
000740         MOVE TRA-VALUE    TO TAX-VALUE
000750         MOVE TRA-TAXFREE  TO TAX-TAXFREE
	       MOVE 1		 TO WS-S9
	       PERFORM DEBTOR-TAX
000770	       MOVE TRA-AC	 TO TAX-AC
000780         MOVE ZERO         TO TAX-SEQ
000790         IF W90-CR > ZERO
000810             MOVE "1"      TO TAX-TYPE
000840         ELSE
000850             MOVE "2"      TO TAX-TYPE
	       END-IF
000890	       PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT
000900	       MOVE 1		 TO WS-ACTION
000910	       PERFORM AY19 THRU AY59.
000920	     ADD TRA-VALUE	 TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
000930       ADD 1               TO W00-T-DAY W00-T-MTD  W00-T-YTD.
           IF WX-INT = "Y"
               PERFORM DEBTOR-INTEGRATE. 
000950       PERFORM UPDATE-TRAN-RECORD.
000960     IF W90-CR > ZERO
000970         GO TO DEBTOR-CR-TRANS.
000980       ADD W90-DB          TO DEB-DEBT.
000990       ADD W90-DB          TO DEB-BAL.
001000     IF W90-CODE = 01
001010         ADD W90-DB        TO DEB-PURYTD
                                    DEB-MTHPUR (W20-MONTH)
001020     ELSE
001030     IF W90-CODE = 20
001040         ADD W90-DB        TO DEB-INT.
001050       GO TO DEBTOR-TRAN-EXIT.

       DEBTOR-TAX.
	     MOVE W90-TAXVAL(WS-S9)
				 TO TAX-VAL(WS-S9).
	     MOVE W90-VAT(WS-S9) TO TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO DEBTOR-TAX.

       DEBTOR-INTEGRATE.
             MOVE W20-DTE        TO WX-DATE.
	     MOVE "  DA"	 TO WX-LEDG.
	     MOVE W90-CODE	 TO WX-TRN.
	     MOVE W00-E-DESC	 TO WX-DESC.
             MOVE "- DEBTORS"    TO WX-BOOK.
             MOVE TRA-VALUE      TO WX-VALUE.
             MOVE WX-DEBGL       TO WX-AC.
             MOVE 01             TO WX-TYPE.
             PERFORM GL-TRF-UPDATE.

001070 DEBTOR-CR-TRANS.
001080       ADD W90-CR          TO DEB-CRED.
001090     IF W90-CODE = 04
001100         SUBTRACT W90-CR   FROM DEB-PURYTD
                                      DEB-MTHPUR (W20-MONTH).
001110     IF W90-CODE = 17
001120         SUBTRACT W90-CR   FROM DEB-INT
001130         SUBTRACT W90-CR   FROM DEB-BAL.
001140       MOVE W90-CR         TO W25-VALUE.
001150       GO TO DEBTOR-TRAN-EXIT.
001160
001170 DEBTOR-CREDITS.
001180       SUBTRACT W90-CR     FROM DEB-BAL.
001190     IF W25-VALUE < DEB-120
001200         SUBTRACT W25-VALUE
001210                           FROM DEB-120
001220         ADD W25-VALUE     TO W90-120
001230         GO TO DEBTOR-TRAN-EXIT.
001240       SUBTRACT DEB-120    FROM W25-VALUE.
001250       ADD DEB-120         TO W90-120.
001260       MOVE ZERO           TO DEB-120.
001270     IF W25-VALUE < DEB-90
001280         SUBTRACT W25-VALUE
001290                           FROM DEB-90
001300         ADD W25-VALUE     TO W90-90
001310         GO TO DEBTOR-TRAN-EXIT.
001320       SUBTRACT DEB-90     FROM W25-VALUE.
001330       ADD DEB-90          TO W90-90.
001340       MOVE ZERO           TO DEB-90.
001350     IF W25-VALUE < DEB-60
001360         SUBTRACT W25-VALUE 
001370                           FROM DEB-60
001380         ADD W25-VALUE     TO W90-60
001390         GO TO DEBTOR-TRAN-EXIT.
001400       SUBTRACT DEB-60     FROM W25-VALUE.
001410       ADD DEB-60          TO W90-60.
001420       MOVE ZERO           TO DEB-60.
001430     IF W25-VALUE < DEB-30
001440         SUBTRACT W25-VALUE 
001450                           FROM DEB-30
001460         ADD W25-VALUE     TO W90-30
001470         GO TO DEBTOR-TRAN-EXIT.
001480       SUBTRACT DEB-30     FROM W25-VALUE.
001490       ADD DEB-30          TO W90-30.
001500       MOVE ZERO           TO DEB-30.
001510     IF W25-VALUE < DEB-CUR
001520         SUBTRACT W25-VALUE 
001530                           FROM DEB-CUR
001540         ADD W25-VALUE     TO W90-CUR
001550         GO TO DEBTOR-TRAN-EXIT.
001560       SUBTRACT DEB-CUR    FROM W25-VALUE.
001570       ADD DEB-CUR         TO W90-CUR.
001580       MOVE ZERO           TO DEB-CUR.
001590     IF W90-CODE = 02
001600         IF W25-VALUE < DEB-INT
001610             SUBTRACT W25-VALUE 
001620                           FROM DEB-INT
001630             ADD W25-VALUE TO W90-INT
001640             GO TO DEBTOR-TRAN-EXIT
001650         ELSE 
001660             ADD DEB-INT   TO W90-INT
001670             MOVE ZERO     TO DEB-INT.

001690 DEBTOR-TRAN-EXIT.
001700       EXIT.

       COPY LEDTRF.UPD.

      /
      *    ****    D E B T O R	 I N V O I C E	 A N A L I S E D
      *
037050 CK000	       SECTION 50.
037060 CK00.
037070       DISPLAY CLR-SCREEN.
037210	     MOVE 1		 TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.

037270 CK04.
037280       DISPLAY CLR-SCREEN.
037290       DISPLAY S50.
037320	     MOVE SPACES	 TO W80-ACNO.

037330 CK05.
      *
      *    ****    Update the BATCH number
      *
037340       DISPLAY "Batch:" AT 0302.
026380       MOVE 1              TO WS-NETKEY.
026390       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
037370       MOVE DEB-BATCH      TO W85-BATCH.
037380     IF DEB-BATCH = 99999
037390         MOVE 1            TO DEB-BATCH
037400     ELSE
037410         ADD 1             TO DEB-BATCH.
037430       MOVE W85-BATCH      TO WS-BATCH.
026460       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
037440	     DISPLAY WS-BATCH AT 0309
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *
      *    ****    Clear the working storage fields
      *
	     MOVE ZERO		 TO W90-TTAX W90-TBDT
                                    W90-TCAP W90-TBCR
                                    W90-TEXP W90-VATTOT.

       CK06.
037450       MOVE ZERO           TO W90-30  W90-CUR 
                                    W90-60  W90-BCR 
                                    W90-90  W90-BDT
                                    W90-120.
037550       MOVE 0              TO WS-PAGE.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.

037560 CK10.
      *
      *    ****    Print the audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
026580       MOVE SPACES         TO R-DET1.
026590       MOVE "PAGE:"        TO R-PAGE.
026600       MOVE "DATE:"        TO R-DATE.
026610       ADD 1               TO WS-PAGE.
026620       MOVE W12-TODAY      TO R-DTE.
026630       MOVE WS-PAGE        TO R-P-NO.
037720	     MOVE "DEBTOR INVOICES - AUDIT TRAIL"
                                 TO R-H5.
026680       MOVE "BATCH:"       TO R-H6.
026690       MOVE WS-BATCH       TO R-BATCH.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026660	     MOVE SPACES	 TO R-DET1.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
             MOVE SPACES         TO R-DET1.
015410       MOVE W95-COMP       TO R-CO.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

037660 CK15.
	     MOVE "H"		 TO W02-PRN-TYPE.
037740       MOVE SPACES         TO R-DET1.
037750       MOVE "AC/NO"        TO I-H1.
037760       MOVE "NAME"         TO I-H2.
037770       MOVE "DATE"         TO I-H3.
037780       MOVE "REF"          TO I-H4.
037790       MOVE "VALUE"        TO I-H5.
037800       MOVE "TAX"          TO I-H6.
037810       MOVE "TOTAL"        TO I-H7.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
037830	     MOVE SPACES	 TO R-DET1.
	     MOVE "D"		 TO W02-PRN-TYPE.

037840 CK20.
	     MOVE 1		 TO WS-S9.

       CK22.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO CK22.
037850	     MOVE ZERO		 TO W90-DB W90-CAP W90-DISC
037860				    W90-CR W90-TX  W90-EX   W40-SDISC.
037870       MOVE SPACES         TO W90-REF W80-ACNO.
037880       MOVE W12-TODAY      TO W90-IDTE.

037890 CK25.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Debtor Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12
		     "=Create,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
037900       ACCEPT S36 AT 0414.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO CK75
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN F8-KEY
		     PERFORM DA000
		     GO TO CK25
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CK25
	       END-EVALUATE
016570	       DISPLAY S36 AT 0415
016580	       IF W80-ACNO = SPACES
016590		   GO TO CK25.
	     DISPLAY CLEAR-L25.
037930     IF W80-ACNO = SPACES
037940         GO TO CK75.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             MOVE W80-ACNO       TO DEB-ACNO.
037950       PERFORM CA155-GET-DEBTOR.
037960     IF WS-ERROR = 1
037970	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
037980         GO TO CK25.
037990     IF WS-ERROR = 2
038000	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
038010         GO TO CK25.
038020       MOVE DEB-NAME       TO W60-NAME.
038030       INSPECT W60-NAME REPLACING FIRST "*" BY ".".
038040       INSPECT W60-NAME REPLACING FIRST "*" BY " ".
038050       INSPECT W60-NAME REPLACING FIRST "!" BY " ".
038060	     DISPLAY W60-NAME AT 0428
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
038080       COMPUTE W40-CUR = DEB-BAL - (DEB-30 + DEB-60 
                                          + DEB-90 + DEB-120).
             DISPLAY S52.
	     PERFORM CHECK-CORRECT.
038230	   IF WS-OPTION = "N"
038240         GO TO CK20.

038250 CK30.
038260       ACCEPT S51A.
038270	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
038280       MOVE W90-IDTE       TO W10-EDTE.
038290       PERFORM CHECK-DATE.
038300     IF WS-ERROR = 1
038310	       MOVE "Date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
038320         GO TO CK30.
038330     IF W90-REF = SPACES
038340	       MOVE "Ref No"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
038350         GO TO CK30.
	   IF W22-DTE1 < WS-VAT-DATE(1)
	       MOVE 7		 TO WS-VAT-SUB
	   ELSE
	       MOVE 1		 TO WS-VAT-SUB.
	     MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE.
038360	     MOVE ZERO		 TO W90-TX.
038370	   IF W90-CAP NOT > ZERO AND W90-EX NOT > ZERO
038380         PERFORM CK80 THRU CK90
038390         IF WS-ERROR = 1
038400		   MOVE "Value"  TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
038410		   GO TO CK25
	       END-IF
	   ELSE
038430	       COMPUTE W90-TX ROUNDED = (W90-CAP * W05-RTE).
038450       DISPLAY S51AB.
038470	     MOVE W90-CAP	 TO W90-DB W90-TAXVAL(1).
038480	     ADD W90-EX		 TO W90-DB.
038490	     ADD W90-TX		 TO W90-DB.
038500	     DISPLAY S51B.
	     PERFORM CHECK-CORRECT.
038640	   IF WS-OPTION = "N"
038650         GO TO CK30.
038660	   IF (W90-EX = ZERO) AND (W90-CAP = ZERO)
038670		MOVE "N"	 TO WS-OPTION
038680          GO TO CK38.
	   IF DEB-SDISC = ZERO
	       GO TO CK32.

       CK31.
	     MOVE "Does settlement discount apply Y/N?  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO CK31.
           IF WS-OPTION = "Y"
               MOVE DEB-SDISC    TO W40-SDISC
               MOVE W05-DSETT (W40-SDISC)
                                 TO W90-DISC.

       CK32.
038690	     MOVE "Allocate to selected Sales Analyses Y/N  [ ]"
				 TO WS-ERR-MES.
	     DISPLAY "Press " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     " to CANCEL"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       CK35.
	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CK20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CK32
	       END-EVALUATE.
038790	   IF NOT (WS-OPTION = "N" OR "Y")
038800	       GO TO CK32.

038840 CK38.
             MOVE 1401           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
038810	     DISPLAY CLEAR-L25.
038850       MOVE DEB-ACNO       TO I-AC.
038860       MOVE W60-NAME       TO I-NAME.
038870       MOVE W90-IDTE       TO I-DATE.
038880       MOVE W90-REF        TO I-REF.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
038900	     MOVE SPACES	 TO R-L8.
038910	     ADD W90-EX W90-CAP GIVING W90-INT.
038920       DISPLAY "Sales" AT 1410.
038930       DISPLAY "Ledger" AT 1510.
038940       DISPLAY "Description" AT 1524.
038950       DISPLAY "Value" AT 1567.
             PERFORM AY60 THRU AY999.
038960     IF WS-OPTION = "N"
038970	       IF (W90-EX = ZERO) AND (W90-CAP = ZERO)
038980             GO TO CK60
038990         ELSE
039000		   MOVE "XXXX"	 TO DPT-CODE
039010		   MOVE SPACES	 TO W40-LEDG
039020             GO TO CK50.
039030 CK40.
039040       DISPLAY "Balance :" AT 1910.
039050       MOVE W90-INT        TO W95-B.
039060	     DISPLAY W95-B AT 1920
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
039070	     MOVE ZERO		 TO W40-INT.
	     MOVE SPACES	 TO W40-LEDG.
      *      DISPLAY "001 to 296" AT 2302 WITH FOREGROUND-COLOR 14.
011010	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Sales Ledger Lookup, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
                     " to post as UNSPECIFIED SALES"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
039080 CK45.
039090       ACCEPT W40-LEDG AT 1712
039100		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W40-LEDG
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO CK45
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CK45
	       END-EVALUATE.
039120	   IF (W40-LEDG = "0000" OR "YYYY") OR
	      (W40-SCDE = "VAT")
039130	       GO TO CK45.
039110	     DISPLAY  WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
039140	   IF W40-LEDG = SPACES
039150	       MOVE "XXXX"	 TO DPT-CODE
039160     ELSE
039170	       MOVE W40-LEDG	 TO DPT-CODE.

039180 CK50.
039190       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
039200     IF DPT-DESC = SPACES
039210	       MOVE "No Record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
039220         GO TO CK45.
039230	     DISPLAY DPT-DESC AT 1724
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
039310	   IF WS-OPTION = "N"
039320         GO TO CK40.

039340 CK52.
039350     IF W40-LEDG NOT = ZERO
039360         ACCEPT S35 AT 1762
039370     ELSE
039380	       MOVE W90-INT	 TO W40-INT
039390         DISPLAY S35 AT 1762.
039400	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
039410     IF W40-INT > W90-INT
039420	       MOVE "Value allocated exceeds Tran-balance"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
039440	       GO TO CK52.
	     PERFORM CHECK-CORRECT.
039520	   IF WS-OPTION = "N"
039530	       IF W40-LEDG = SPACES
039540             GO TO CK45
039550         ELSE
039560             GO TO CK52.
039570	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
039590       MOVE W40-LEDG       TO TL-LEDG.
039600       MOVE DPT-DESC       TO TL-NME.
039610       PERFORM AY38 THRU AY59.
039630	     MOVE W40-INT	 TO I-CAP.
039640	     ADD W40-INT	 TO DPT-SALES-DAY
039650                              DPT-SALES-MTD
				    DPT-SALES-YTD.
      *
      *    G/LEDGER INTEGRATION
      *
           IF WX-INT = "Y"
	       MOVE W20-DTE	 TO WX-DATE
	       MOVE W40-LEDG	 TO WX-LEDG
	       MOVE "Invoicing"	 TO WX-DESC
	       MOVE "- DEBTORS"	 TO WX-BOOK
	       COMPUTE WX-VALUE = W40-INT * -1
	       MOVE 04		 TO WX-TYPE
	       MOVE DPT-SALE-GL	 TO WX-AC
	       PERFORM GL-TRF-UPDATE.
039700       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
039750	       PERFORM CK10 THRU CK15.
039760       MOVE SPACES         TO R-L8.
039770       SUBTRACT W40-INT    FROM W90-INT.
039780     IF W90-INT > ZERO
	       MOVE 1601	 TO CPOS
               PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24
039800         GO TO CK40.

039810 CK60.
             PERFORM GET-DEBTOR-TRAN-CODE THRU GET-DEBTOR-TRAN-EXIT.
             PERFORM AY01 THRU AY59.
             MOVE 0              TO WS-ACTION.
             PERFORM AY02 THRU AY59.
             MOVE "A"            TO WS-SKIP.
	     MOVE W90-TX	 TO W90-VAT(1).
039820       PERFORM GENERATE-DEBTOR-TRAN.
             MOVE "N"            TO WS-SKIP.
	   IF OPEN-ITEM
	       GO TO CK60B.

039870 CK60A.
039830	     MOVE "'M'td,'C'ur, '3'0, '6'0, '9'0 or '1'20days  [M]"
				 TO WS-ERR-MES.
039860	     MOVE "M"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
039950	   IF NOT (WS-OPTION = "M" OR "C" OR "3" OR "6" OR "9" OR "1")
039960         GO TO CK60A.
039980	     ADD W90-DB		 TO W90-BDT.
039990	     MOVE W90-DB	 TO W90-INT.
040040     IF WS-OPTION = "M"
040050         NEXT SENTENCE
040060     ELSE
040070     IF WS-OPTION = "C"
040080         ADD W90-INT       TO DEB-CUR W90-CUR
040090     ELSE
040100     IF WS-OPTION = "3"
040110	       ADD W90-INT	 TO DEB-30 W90-30
040120     ELSE
040130     IF WS-OPTION = "6"
040140         ADD W90-INT       TO DEB-60 W90-60
040150     ELSE
040160     IF WS-OPTION = "9"
040170         ADD W90-INT       TO DEB-90 W90-90
040150     ELSE
040160     IF WS-OPTION = "1"
040170         ADD W90-INT       TO DEB-120 W90-120.

040180 CK60B.
             ADD 121 DEB-CAT     GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
                                 REMAINDER W90-CS.
             ADD 1               TO W90-CS.
040190       MOVE W90-CATKEY     TO WS-PARKEY.
040200	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
040210       PERFORM AY01 THRU AY59.
040230	     ADD W90-DB		 TO PAR-CDRMTD (W90-CS)
040240				    PAR-CDRYTD (W90-CS).
040280       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

040290 CK61.
	     ADD W90-DB 	 TO W90-TBDT.
	     ADD W90-CAP	 TO W90-TCAP.
	     ADD W90-EX		 TO W90-TEXP.
	     ADD W90-TX		 TO W90-TTAX.
040300       MOVE "TOTAL"        TO I-NAME.
040310       MOVE W90-REF        TO I-REF.
040330	     ADD W90-CAP W90-EX  GIVING I-CAP.
040340	     MOVE W90-TX	 TO I-GST.
040350	     MOVE W90-DB	 TO I-TOT.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
040440	       PERFORM CK10 THRU CK15.
040480       MOVE SPACES         TO R-DET1.
040490       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
             PERFORM CK70-NEW THRU CK71.
040600       PERFORM AY70 THRU AY999.

040500 CK62.
040510       DISPLAY CLR-SCREEN.
040520       DISPLAY S50.
040540	     MOVE W90-TBDT	 TO W100-BAL.
040570       DISPLAY "Batch Total: " AT 2112
040580		      W100-BAL WITH FOREGROUND-COLOR 3 HIGHLIGHT
				    BACKGROUND-COLOR 5.
037450       MOVE ZERO           TO W90-30  W90-CUR 
                                    W90-60  W90-BCR 
                                    W90-90  W90-BDT
                                    W90-120.
040630       GO TO CK20.

040750 CK70-NEW.
040840	     MOVE W90-BDT	 TO W25-VALUE.
040870	     ADD W25-VALUE	 TO DEB-OUT.
040880	     ADD W90-BDT	 TO DEB-DT.
040890	     ADD W90-BCR	 TO DEB-CR.
040900	     ADD W90-120	 TO DEB-FOUR.
040900	     ADD W90-90 	 TO DEB-THREE.
040910	     ADD W90-60 	 TO DEB-TWO.
040920	     ADD W90-30 	 TO DEB-ONE.
040930	     ADD W90-CUR	 TO DEB-MTD.

041220 CK71.
             MOVE 9              TO WS-PARKEY.
031900	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
	     MOVE "VAT1"	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             PERFORM AY38 THRU AY59.
	     ADD W90-EX		 TO PAR-DNGS-DAY
                                    PAR-DNGS-MTD
				    PAR-DNGS-YTD.
	     ADD W90-CAP	 TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
				    PAR-DVAT-YTD
				    DPT-V-SALES-DAY
				    DPT-V-SALES-MTD
				    DPT-V-SALES-YTD.
	     ADD W90-TX		 TO DPT-O-VAT-DAY
				    DPT-O-VAT-MTD
				    DPT-O-VAT-YTD.
	   IF (WX-INT = "Y") AND (W90-TX NOT = ZERO)
	       COMPUTE WX-VALUE = W90-TX * -1
	       MOVE DPT-O-VAT-GL TO WX-AC
	       MOVE 10		 TO WX-TYPE
	       PERFORM GL-TRF-UPDATE.
031930	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
031930	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
       CK75.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
041370	     MOVE "TAXABLE:"	 TO I-H8.
041380       MOVE "EXEMPT:"      TO I-H9.
041390       MOVE "V.A.T.:"      TO I-H10.
041400       MOVE "TOTAL:"       TO I-H11.
041420	     MOVE W90-TCAP	 TO I-TOTTAX.
041430	     MOVE W90-TEXP	 TO I-TOTEXE.
041440	     MOVE W90-TTAX	 TO I-TOTGST.
041450	     MOVE W90-TBDT	 TO I-TOTBCH.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026230       MOVE SPACES         TO R-DET1.
	     MOVE "S"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
041550	     MOVE 0701		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
041560	     MOVE "New Batch Y/N?  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
041630	   IF WS-OPTION = "N"
041640         GO TO CK999.
041650       GO TO CK00.

041660 CK80.
041670	     MOVE "Do you want to adjust - TAX Y/N?  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
041750	   IF NOT (WS-OPTION = "N" OR "Y")
041760         GO TO CK80.
041780     IF WS-OPTION = "N"
041790	       MOVE 1		 TO WS-ERROR
041800         GO TO CK90.
041810	     MOVE ZERO		 TO WS-ERROR.
041820 CK85.
041830       ACCEPT S51AB.
	     PERFORM CHECK-CORRECT.
041920	   IF WS-OPTION = "N"
041930         GO TO CK85.
041940 CK90.
041950       EXIT.
041960 CK999.
041970       EXIT.
      *
      *    ****    C A L L   D E B T O R   C R E A T I O N
      *
       DA000	       SECTION 51.
       DA00.
	     PERFORM SAVE-SCREEN.
	     CLOSE RECOVER.
004200	     CALL "DTP\DTP001"
		   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
	     CANCEL "DTP\DTP001".
	     OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
012150       MOVE ZERO           TO WS-RECOVER.
	     PERFORM RESTORE-SCREEN.
       DA99.
             EXIT.

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
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
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
	     MOVE W12-T-YMD	 TO W22-DTE2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The month end dates are stored in the Control file in the ³
      *    ³ first record as Cur, 30d, 60d, 90d and 120d and the next  ³
      *    ³ routine is used since Ver 8.11.06. 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO WS-NETKEY.
	     PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	     MOVE DEB-CUR-DTE	 TO W30-CUR.
	     MOVE DEB-30D-DTE	 TO W30-30.
	     MOVE DEB-60D-DTE	 TO W30-60.
	     MOVE DEB-90D-DTE	 TO W30-90.
	     MOVE DEB-120-DTE	 TO W30-120.
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
041720	   ELSE
041730     IF WS-F-ERROR = 5
041740         MOVE W02-DBTRAN   TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
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
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 40
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
