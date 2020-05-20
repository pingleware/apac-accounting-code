      $SET LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	     **      ****      *
      *   **	**     **     **    **	 **  **     ***     **	**     *
      *   **	**     **     **    **	**    **     **    **	 **    *
      *   **	**     **     *******	**    **     **    **	 **    *
      *   **	**     **     **	**    **     **    **	 **    *
      *   **	**     **     **	 **  **      **     **	**     *
      *   *******      **     **	  ****	   ******    ****      *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      * 	A C C O U N T S	  R E C E I V A B L E	/	       *
      *                                                                *
      *   D E B T O R	T R A N S A C T I O N S   U S I N G   C O D E  *
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
000020 PROGRAM-ID.     DTP010.
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

       COPY DEBTRN.SL.

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

       COPY DEBTRN.GFD.

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
001680 77  WS-DBTKEY	   PIC	9(06)	 COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001720 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
001760 77  WS-PERIOD	   PIC	9(02).
001770 77  WS-NTP          PIC  X(01).
       77  WS-MTHEND	   PIC	9(02)	 COMP-5.
001800 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
001810 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-ALLOCATE	   PIC	X(01)	 VALUE "M".
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
001820 77  WS-TAX          PIC  X(01).
001820 77  WS-TYPE         PIC  X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
001870 77  WS-EKEY         PIC  9(06)    COMP-5.
001880 77  WS-CHAR1        PIC  X(01).
001890 77  WS-CHAR2        PIC  X(01).
001900 77  WS-COMP         PIC  9(01).
001980 77  WS-BATCH        PIC  9(06) COMP-5.
       77  WS-VAT-CODE	   PIC	X(04).
       77  WS-VAT-VALUE    PIC S9(09)V99 COMP-3.
002010 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002100 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002110 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002130 77  WS-AC	   PIC	X(07) VALUE "ACCOUNT".
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "010".

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

       COPY DEBTRN.ID.

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

001270* 01  W08-EXPD.
      *	   03  W08-CHAR        PIC  X(06).
001280*	   03  W08-DEC REDEFINES W08-CHAR
      *			       PIC  X(01) COMP-X OCCURS 6.

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
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.
	   03  W75-CODE       PIC Z9.

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
004550	   03  W100-INT        PIC Z(08)9.99B.
004560	   03  W100-CRL        PIC Z(06)9.
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

       COPY TRNLUP.CRT.

       01  S08A.
           03  LINE 16 COLUMN  6 VALUE "Allocation:".
           03  LINE 17 COLUMN  6 VALUE "Ref. No.".
	   03	       COLUMN 18 VALUE "Date".
	   03	       COLUMN 26 VALUE "Transaction".
	   03	       COLUMN 46 VALUE "Value".
	   03	       COLUMN 57 VALUE "Balance".
	   03	       COLUMN 69 VALUE "Allocate".

005580 01  S12.
005590	   03  LINE  9 COLUMN 12 VALUE "Name :".
005600	   03  LINE 11 COLUMN 12 VALUE "Bal  :".
005610	   03  LINE 12 COLUMN 12 VALUE "Mtd  :".
005620	   03  LINE 13 COLUMN 12 VALUE "Cur  :".
005630	   03  LINE 14 COLUMN 12 VALUE "30d  :".
005640	   03  LINE 15 COLUMN 12 VALUE "60d  :".
005650	   03  LINE 16 COLUMN 12 VALUE "90d  :".
005650	   03  LINE 17 COLUMN 12 VALUE "120d :".
005660	   03  LINE 18 COLUMN 12 VALUE "Int  :".
005670	   03  LINE 19 COLUMN 12 VALUE "Limit:".

007460 01  S26.
007470	   03  LINE 2 COLUMN 31  FOREGROUND-COLOR 7 HIGHLIGHT
007480                           VALUE "DEBTOR TRANSACTIONS".
007490     03  LINE 3 COLUMN  2  VALUE "Batch:".
007510     03  LINE 5 COLUMN  2  VALUE "Account".
007520	   03	      COLUMN 12  VALUE "Date".
007530	   03	      COLUMN 20  VALUE "Cde/description".
007550	   03	      COLUMN 38  VALUE "Ref".
007560	   03	      COLUMN 51  VALUE "Value".
007560	   03	      COLUMN 64  VALUE "TAX".
007570	   03	      COLUMN 75  VALUE "Total".

007580 01  S26-DTE.
007590	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 99/99/9999 USING W90-DATE AUTO.

007580 01  S26-DT.
007590	   03  LINE  7 COLUMN 68 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
007600				 PIC Z(08)9.99 USING W90-DB AUTO.

007610 01  S26-CR.
007620	   03  LINE  7 COLUMN 68 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007630				 PIC Z(08)9.99 USING W90-CR AUTO.

007580 01  S26-VAL.
007590	   03  LINE  7 COLUMN 45 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
007600				 PIC Z(07)9.99 USING W90-VAL AUTO.

007610 01  S26-VAT.
007620	   03  LINE  7 COLUMN 57 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
007630				 PIC Z(06)9.99 USING WS-VAT-VALUE AUTO.

007810 01  S29.
007820	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-MTD AUTO.

007830 01  S30.
007840	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-CUR AUTO.

007850 01  S31.
007860	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-30 AUTO.

007870 01  S32.
007880	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-60 AUTO.

007890 01  S33.
007900	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-90 AUTO.

007890 01  S34.
007900	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-120 AUTO.

007910 01  S35.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-INT AUTO.

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

       01  S-ALLOC.
	   03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
	   03	       COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.99 FROM W75-BALANCE.
	   03	       COLUMN 51 VALUE "Allocated : ".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.99 FROM W75-ALLOCATED.

       01  S-DTRN.
	   03  COLUMN  6 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC X(08) FROM DBT-REF.
	   03  COLUMN 15 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC 9999/99/99 FROM DBT-DATE.
	   03  COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC X(12) FROM W00-E-DESC.
	   03  COLUMN 39 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 FROM DBT-VALUE.
	   03  COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 FROM DBT-OUTSTD.
	   03  COLUMN 65 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 USING DBT-ALLOC AUTO.

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
	     PERFORM CG000.
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

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY DBTRAN.RD1.

       COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

000140 COPY DEBTRN.RD.

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

000140 COPY DEBTRN.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

       COPY TXTRAN.WR.

       COPY DEBTOR.LUP.

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
	     MOVE ZERO		 TO REC-TYPE.
	     MOVE DPT-RECORD	 TO REC-DEPART.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
	     MOVE WS-NETKEY	 TO REC-KEY.
	     MOVE NET-RECORD	 TO REC-NETWORK.
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
      *    ****    D E B T O R   T R A N S A C T I O N S
      *   
026310 CG000		       SECTION 50.
026320 CG00.
026330       DISPLAY CLR-SCREEN.
026340       DISPLAY S26.

026360 CG05.
026380       MOVE 1              TO WS-NETKEY.
026390       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
026400       MOVE DEB-BATCH      TO W85-BATCH.
026410     IF DEB-BATCH = 999999
026420         MOVE 1            TO DEB-BATCH
026430     ELSE
026440         ADD 1             TO DEB-BATCH.
026450       MOVE W85-BATCH      TO WS-BATCH.
026460       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
026470	     DISPLAY WS-BATCH AT 0309
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE ZERO		 TO W90-DEBIT  W90-CREDIT
				    W90-VATTOT.

       CG10.
026480       MOVE ZERO           TO W90-BDT W90-30 
026510                              W90-BCR W90-60 
                                    W90-CUR W90-90 
                                    W90-INT W90-DISC
				    W90-120 W40-SDISC
				    WS-VAT-VALUE.
	     MOVE 1		 TO WS-S9.

       CG12.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO CG12.

       CG15.
026560	     MOVE 0		 TO WS-PAGE.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.

026570 CG20.
	     PERFORM HA000.

026820 CG30-TRAN.
             MOVE SPACES         TO W80-ACNO.
026830       MOVE ZERO           TO W90-CODE.
026840       MOVE W12-TODAY      TO W90-DTE W90-DATE.

026850 CG35.
026860	     MOVE ZERO		 TO W90-DB W90-CR WS-VAT-VALUE W90-EX.
026870       MOVE SPACES         TO W90-REF.

026880 CG40-ACCEPT.
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
026890	     ACCEPT S36 AT 0702.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO CG280
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN F8-KEY
		     PERFORM GA000
		     GO TO CG40-ACCEPT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CG40-ACCEPT
	       END-EVALUATE
016570	       DISPLAY S36 AT 0702
016580	       IF W80-ACNO = SPACES
016590		   GO TO CG40-ACCEPT.
	     DISPLAY CLEAR-L25.
026910     IF W80-ACNO = SPACES
026920	       GO TO CG280.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             MOVE W80-ACNO       TO DEB-ACNO.
026930       PERFORM CA155-GET-DEBTOR.
026940     IF WS-ERROR = 1
026950	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
026960	       GO TO CG40-ACCEPT.
026970     IF WS-ERROR = 2
026980	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
026990	       GO TO CG40-ACCEPT.
             MOVE 10             TO CLIN.
             MOVE 01             TO CCOL.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 20.
027010       DISPLAY S12.
027020       MOVE DEB-NAME TO W60-NAME.
027030       INSPECT W60-NAME REPLACING FIRST "*" BY ".".
027040       INSPECT W60-NAME REPLACING FIRST "*" BY " ".
027050       INSPECT W60-NAME REPLACING FIRST "!" BY " ".
027060	     DISPLAY W60-NAME AT 0920
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027070       MOVE DEB-BAL        TO W100-BAL.
027080       COMPUTE W40-ONE = DEB-BAL - (DEB-CUR + DEB-30 + DEB-60
027090                                    + DEB-90 + DEB-120 + DEB-INT).
027100	     DISPLAY W100-BAL AT 1120
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027110       MOVE W40-ONE        TO W100-CUR.
027120	     DISPLAY W100-CUR AT 1220
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027130       MOVE DEB-CUR        TO W100-CUR.
027140	     DISPLAY W100-CUR AT 1320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027150       MOVE DEB-30         TO W100-30.
027160	     DISPLAY W100-30 AT 1420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027170       MOVE DEB-60         TO W100-60.
027180	     DISPLAY W100-60 AT 1520
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027190       MOVE DEB-90         TO W100-90.
027200	     DISPLAY W100-90 AT 1620
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027190       MOVE DEB-120        TO W100-120.
027200	     DISPLAY W100-120 AT 1720
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027210       MOVE DEB-INT        TO W100-INT.
027220	     DISPLAY W100-INT AT 1820
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027230       MOVE DEB-CRL        TO W100-CRL.
027240	     DISPLAY "  " AT 1920
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5
		      W100-CRL WITH FOREGROUND-COLOR 3 HIGHLIGHT
				    BACKGROUND-COLOR 5
		     "    " WITH FOREGROUND-COLOR 3
				 BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
027320	   IF WS-OPTION = "N"
               UNLOCK DEBTOR
027330	       GO TO CG35.
022680	     DISPLAY CLEAR-L25.
	     DISPLAY "Press " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to CANCEL"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
027340 CG45-ACCEPT.
027350       ACCEPT S26-DTE AT 0709.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY WS-BLNK78 AT 0702 WITH FOREGROUND-COLOR 3
		     GO TO CG35
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CG45-ACCEPT
	       END-EVALUATE.
             MOVE W90-DATE       TO W90-DTE.
022680	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
027380       MOVE W90-DTE        TO W10-DATE.
027390       PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO CG45-ACCEPT.
	   IF W22-DTE1 < WS-VAT-DATE(WS-VAT-SUB)
	       ADD 6		 TO WS-VAT-SUB.

027430 CG50-ACCEPT.
	     DISPLAY "F6" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "=View Transaction Codes, "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Cancel"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
027440	     ACCEPT W90-CODE AT 0720
027450		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY WS-BLNK78 AT 0702 WITH FOREGROUND-COLOR 3
		     GO TO CG35
		 WHEN F6-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM TRANCDE-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO CG50-ACCEPT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CG50-ACCEPT
	       END-EVALUATE.
027460	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
027470     IF W90-CODE = ZERO
027480	       GO TO CG40-ACCEPT.
027490     IF W90-CODE < 01 OR > 99
027500	       GO TO CG55-ERROR.
             PERFORM GET-DEBTOR-TRAN-DESC.
027560     IF W00-T-CODE = ZERO
027570	       GO TO CG55-ERROR.
027580       MOVE W00-E-DESC     TO W90-DESC.
027590	     DISPLAY W90-DESC AT 0723
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027600	     GO TO  CG60-REF.

027610 CG55-ERROR.
027620	     MOVE "Code"	 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
027630	     GO TO CG50-ACCEPT.

027640 CG60-REF.
027650	     ACCEPT W90-REF AT 0736
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY WS-BLNK78 AT 0702 WITH FOREGROUND-COLOR 3
		     GO TO CG35
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CG60-REF
	       END-EVALUATE.

027670 CG65-VALUE.
	     DISPLAY CLEAR-L25.
           IF W00-T-CODE = 02 OR 05 OR 06 OR 17 OR 20
               MOVE "N"          TO WS-TAX
           ELSE
	       PERFORM CG70 THRU CG85.
027680     IF W00-ACTION = 1
027690         MOVE ZERO         TO W90-DB
027700	       GO TO CG120-CREDIT.
	     GO TO CG90-DEBIT.

       CG70.
	     MOVE "I"		 TO WS-OPTION.

       CG75.
	     MOVE "VAT: 'N'o, 'I'ncluded, 'K'ey in, 'C'alculate [I]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-TAX.
027950	   IF NOT (WS-TAX = "N" OR "I" OR "C" OR "K")
	       GO TO CG75.
	   IF NOT (WS-TAX = "I" OR "C" OR "K")
	       GO TO CG85.
	     MOVE "1"		 TO WS-OPTION.

       CG80.
      *	     MOVE "VAT Rate code 1 - 6 [1]"
      *				 TO WS-ERR-MES.
      *	     PERFORM OPT-MESSAGE.
      *	   IF WS-OPTION < "1" OR > "6"
      *	       GO TO CG80.
      *	     STRING "VAT" DELIMITED SIZE
      *		     WS-OPTION DELIMITED SIZE
      *				 INTO WS-VAT-CODE.
	     MOVE "VAT1"	 TO WS-VAT-CODE.
	     MOVE WS-OPTION	 TO WS-VAT-SUB.

       CG85.
             EXIT.
      *
      *   ****    D E B I T   T R A N S A C T I O N
      *
027710 CG90-DEBIT.
           IF W00-T-CODE = 13
               ACCEPT S26-VAT
	       MOVE WS-VAT-VALUE TO W90-VAT(WS-VAT-SUB) W90-DB
               DISPLAY S26-DT
           ELSE
           IF WS-TAX = "N"
027720         ACCEPT S26-DT
	       MOVE W90-DB	 TO W90-VAL W90-EX
               DISPLAY S26-VAL
           ELSE
           IF WS-TAX = "C"
               ACCEPT S26-VAL
	       PERFORM CG110
	       ADD W90-VAL W90-VAT(WS-VAT-SUB)
                                 GIVING W90-DB
               DISPLAY S26-VAT
               DISPLAY S26-DT
           ELSE
	   IF WS-TAX = "K"
               ACCEPT S26-VAL
	       ACCEPT S26-VAT
	       MOVE WS-VAT-VALUE TO W90-VAT(WS-VAT-SUB)
	       ADD W90-VAL WS-VAT-VALUE
                                 GIVING W90-DB
               DISPLAY S26-DT
           ELSE
	   IF WS-TAX = "I"
027720         ACCEPT S26-DT
	       PERFORM CG105
               DISPLAY S26-VAL
               DISPLAY S26-VAT.
027460	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
           IF W90-DB = ZERO
	       GO TO CG125-CORRECT.
	   IF NOT ((W90-CODE = 01) AND (DEB-SDISC NOT = ZERO))
	       GO TO CG100.

       CG95.
	     MOVE "Does settlement discount apply Y/N?  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO CG95.
           IF WS-OPTION = "Y"
               MOVE DEB-SDISC    TO W40-SDISC
               MOVE W05-DSETT (W40-SDISC)
                                 TO W90-DISC.
       CG100.
027730       MOVE ZERO           TO W90-CR.
027740	     GO TO CG125-CORRECT.

       CG105.
	     MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE.
	     COMPUTE W90-VAT(WS-VAT-SUB) ROUNDED = ((W90-DB /
			    (100.000 + W05-RATE)) * W05-RATE)
	     SUBTRACT W90-VAT(WS-VAT-SUB) FROM W90-DB
				 GIVING W90-VAL.
	     MOVE W90-VAT(WS-VAT-SUB)
				 TO WS-VAT-VALUE.
	     MOVE W90-VAL	 TO W90-TAXVAL(WS-VAT-SUB).

       CG110.
	     MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE.
	     COMPUTE W90-VAT(WS-VAT-SUB) ROUNDED = (W90-VAL * W05-RTE).
	     MOVE W90-VAT(WS-VAT-SUB)
				 TO WS-VAT-VALUE.
	     MOVE W90-VAL	 TO W90-TAXVAL(WS-VAT-SUB).

       CG115.
	     MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE.
	     COMPUTE W90-VAT(WS-VAT-SUB) ROUNDED = ((W90-CR /
			    (100.000 + W05-RATE)) * W05-RATE)
	     SUBTRACT W90-VAT(WS-VAT-SUB) FROM W90-CR
                                 GIVING W90-VAL.
	     MOVE W90-VAT(WS-VAT-SUB)
				 TO WS-VAT-VALUE.
	     MOVE W90-VAL	 TO W90-TAXVAL(WS-VAT-SUB).
      *
      *   ****    C R E D I T   T R A N S A C T I O N
      *
027750 CG120-CREDIT.
           IF W00-T-CODE = 14
               ACCEPT S26-VAT
	       MOVE WS-VAT-VALUE TO W90-VAT(WS-VAT-SUB) W90-CR
               DISPLAY S26-CR
           ELSE
           IF WS-TAX = "N"
027720         ACCEPT S26-CR
	       MOVE W90-CR	 TO W90-VAL W90-EX
               DISPLAY S26-VAL
           ELSE
           IF WS-TAX = "C"
               ACCEPT S26-VAL
	       PERFORM CG110
	       ADD W90-VAL W90-VAT(WS-VAT-SUB)
                                 GIVING W90-CR
               DISPLAY S26-VAT
               DISPLAY S26-CR
           ELSE
	   IF WS-TAX = "K"
               ACCEPT S26-VAL
               ACCEPT S26-VAT
	       MOVE WS-VAT-VALUE TO W90-VAT(WS-VAT-SUB)
	       ADD W90-VAL WS-VAT-VALUE
                                 GIVING W90-CR
               DISPLAY S26-CR
           ELSE
	   IF WS-TAX = "I"
027720         ACCEPT S26-CR
	       PERFORM CG115
               DISPLAY S26-VAL
               DISPLAY S26-VAT.

027770 CG125-CORRECT.
027460	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
027780     IF W90-CR = 0 AND W90-DB = 0
027790	       MOVE "Value"	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
027800	       GO TO CG40-ACCEPT.
027810     IF W90-CODE NOT = 17
027820	       GO TO CG130.
027830     IF W90-CR NOT > DEB-INT
027840	       GO TO CG130.
027850	     MOVE "Credit > Interest on account"
				 TO WS-ERR-MES.
027870       DISPLAY "'ENTER' to change - 'X' to exit" AT 2312
027880                WITH FOREGROUND-COLOR 14.
027890	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     MOVE 2201		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
027950     IF WS-OPTION = "X"
027960	       GO TO CG40-ACCEPT.
027970	     GO TO CG65-VALUE.

027980 CG130.
	     PERFORM CHECK-CORRECT.
028060	   IF WS-OPTION = "N"
028070	       GO TO CG40-ACCEPT.

028250 CG135.
             MOVE ZERO           TO W75-KEY W75-ALLOCATED.
           IF W90-CR NOT = ZERO
               IF OPEN-ITEM
                   MOVE 1301     TO CPOS
                   PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24
                   DISPLAY S08A
                   MOVE 19       TO CLIN
		   MOVE ZERO	 TO W90-ALLOCATED W90-DISCOUNT
                   PERFORM ALLOCATE-CREDIT
                   IF WS-ERROR = 9
		       GO TO CG260.
             PERFORM GET-DEBTOR-TRAN THRU GET-DEBTOR-TRAN-EXIT.
             PERFORM AY01 THRU AY59.
028260       PERFORM GENERATE-DEBTOR-TRAN.
028270       ADD W90-DB          TO W90-BDT W90-DEBIT.
028280       ADD W90-CR          TO W90-BCR W90-CREDIT.

028290 CG140.
	   IF W90-CODE = 01 OR 03 OR 04 OR 19
               ADD 121 DEB-CAT   GIVING W90-RESULT
               DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
                                 REMAINDER W90-CS
               ADD 1             TO W90-CS
040190         MOVE W90-CATKEY   TO WS-PARKEY
028310	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
               PERFORM AY01 THRU AY59
028320         ADD W90-DB        TO PAR-CDRMTD (W90-CS)
028330                              PAR-CDRYTD (W90-CS)
028340         ADD W90-CR        TO PAR-CCRMTD (W90-CS)
028350                              PAR-CCRYTD (W90-CS)
028360         PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
028400     IF W90-CODE = 17 OR 20
028410	       GO TO CG255.
	   IF OPEN-ITEM
001180         SUBTRACT W90-CR   FROM DEB-BAL
	       GO TO CG255.
028420	     MOVE "Allocate to selected age-analysis (Y/N)  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
028510	     MOVE ZERO		 TO W40-MTD W40-CUR W40-30 W40-60
                                    W40-120 W40-INT W40-90.
028520     IF NOT (WS-OPTION = "Y")
028530         IF W00-ACTION = 1
028540             PERFORM DEBTOR-CREDITS THRU DEBTOR-TRAN-EXIT
028550		   GO TO CG255
028560         ELSE
028570		   GO TO CG255.
028580       DISPLAY "Allocation" AT 1135.
028590     IF W00-ACTION = ZERO
028600         MOVE W90-DB       TO W25-VALUE
028610	       GO TO CG205.

028620 CG145.
028630	     MOVE 1234		 TO CPOS.

028640 CG150.
028650	     DISPLAY "             " AT CPOS
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
028660     IF CLIN < 18
028670         ADD 1             TO CLIN
028680	       GO TO CG150.
028690       DISPLAY "Transaction balance:" AT 2012
                      WITH FOREGROUND-COLOR 3.

028700 CG155.
028710	     MOVE W25-VALUE	 TO W95-B.
028720	     DISPLAY W95-V2 AT 2034
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028730	     DISPLAY CLEAR-L25.

028740 CG160.
028750	     ACCEPT S29 AT 1234.
028760     IF W40-MTD > W25-VALUE
028770	       PERFORM CG190
028780	       GO TO CG160.
028790     IF W40-MTD NOT = ZERO
028800     IF W40-MTD > W40-ONE
028810         IF W40-MTD < DEB-BAL
028820		   PERFORM CG195
028830		   GO TO CG160.
028840       SUBTRACT W40-MTD    FROM W25-VALUE.
028850	     PERFORM CG155.
028860     IF W25-VALUE = ZERO
028870	       GO TO CG200.

028880 CG165.
028890	     ACCEPT S30 AT 1334.
028900     IF W40-CUR > W25-VALUE
028910	       PERFORM CG190
028920	       GO TO CG165.
028930     IF W40-CUR NOT = ZERO
028940     IF W40-CUR > DEB-CUR
028950	       PERFORM CG195
028960	       GO TO CG165.
028970       SUBTRACT W40-CUR    FROM W25-VALUE.
028980	     PERFORM CG155.
028990     IF W25-VALUE = ZERO
029000	       GO TO CG200.

029010 CG170.
029020	     ACCEPT S31 AT 1434.
029030     IF W40-30 > W25-VALUE
029040	       PERFORM CG190
029050	       GO TO CG170.
029060     IF W40-30 > DEB-30
029070	       PERFORM CG195
029080	       GO TO CG170.
029090       SUBTRACT W40-30     FROM W25-VALUE.
029100	     PERFORM CG155.
029110     IF W25-VALUE = ZERO
029120	       GO TO CG200.

029130 CG175.
029140	     ACCEPT S32 AT 1534.
029150     IF W40-60 > W25-VALUE
029160	       PERFORM CG190
029170	       GO TO CG175.
029180     IF W40-60 > DEB-60
029190	       PERFORM CG195
029200	       GO TO CG175.
029210       SUBTRACT W40-60     FROM W25-VALUE.
029220	     PERFORM CG155.
029230     IF W25-VALUE = ZERO
029240	       GO TO CG200.

029250 CG180.
029260	     ACCEPT S33 AT 1634.
029270     IF W40-90 > W25-VALUE
029280	       PERFORM CG190
029290	       GO TO CG180.
029300     IF W40-90 > DEB-90
029310	       PERFORM CG195
029320	       GO TO CG180.
029330       SUBTRACT W40-90     FROM W25-VALUE.
029340	     PERFORM CG155.
029350     IF W25-VALUE = ZERO
029360	       GO TO CG200.

029250 CG185.
029260	     ACCEPT S34 AT 1734.
029270     IF W40-120 > W25-VALUE
029280	       PERFORM CG190
029290	       GO TO CG185.
029300     IF W40-120 > DEB-120
029310	       PERFORM CG195
029320	       GO TO CG180.
029330       SUBTRACT W40-120    FROM W25-VALUE.
029340	     PERFORM CG155.
029350     IF W25-VALUE = ZERO
029360	       GO TO CG200.
029370     IF W25-VALUE NOT > DEB-INT
029380         MOVE W25-VALUE    TO W40-INT
029390     ELSE
029400         MOVE DEB-INT      TO W40-INT.
029410	     MOVE W40-INT	 TO W95-B.
029420	     DISPLAY W95-V2 AT 1834
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
029430	     SUBTRACT W40-INT	 FROM W25-VALUE.
029440	     PERFORM CG155.
029450     IF W25-VALUE = ZERO
029460	       GO TO CG200.
029470	     MOVE "Total not allocated - 'R'edo,'A'uto-allocation"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
029500	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
029560	   IF WS-OPTION = "A"
029570             PERFORM DEBTOR-CREDITS THRU DEBTOR-TRAN-EXIT
029580		   GO TO CG255.
029590	     MOVE ZERO		 TO W40-CUR W40-30
				    W40-60  W40-90
				    W40-120 W40-INT.
029600	     MOVE W90-CR	 TO W25-VALUE.
029610	     GO TO CG145.

029620 CG190.
029630	     MOVE "Value allocated > Receipt value"
				 TO WS-ERR-STRING.
029640	     PERFORM ERROR-MESSAGE.

029650 CG195.
029660	     MOVE "Value allocated > Analysis value"
				 TO WS-ERR-STRING.
029640	     PERFORM ERROR-MESSAGE.

029680 CG200.
	     PERFORM CHECK-CORRECT.
029770	   IF WS-OPTION = "N"
029780         MOVE W90-CR       TO W25-VALUE
029790         MOVE ZERO         TO W40-CUR W40-30 W40-60 
                                    W40-INT W40-90 W40-120
029800	       GO TO CG145.
029810       SUBTRACT W40-30     FROM DEB-30.
029820       SUBTRACT W40-60     FROM DEB-60.
029830       SUBTRACT W40-90     FROM DEB-90.
029830       SUBTRACT W40-120    FROM DEB-120.
029840       SUBTRACT W40-INT    FROM DEB-INT.
029850       SUBTRACT W90-CR     FROM DEB-BAL.
029860       ADD W40-30          TO W90-30.
029870       ADD W40-60          TO W90-60.
029880       ADD W40-90          TO W90-90.
029880       ADD W40-120         TO W90-120.
029890       ADD W40-INT         TO W90-INT.
029900     IF W40-CUR < DEB-CUR
029910         SUBTRACT W40-CUR  FROM DEB-CUR
029920         ADD W40-CUR       TO W90-CUR
029930     ELSE
029940         ADD DEB-CUR       TO W90-CUR
029950         MOVE ZERO         TO DEB-CUR.
029960	     GO TO CG255.

029970 CG205.
029980	     MOVE 1234		 TO CPOS.

029990 CG210.
030000	     DISPLAY "             " AT CPOS WITH FOREGROUND-COLOR 3.
030010     IF CLIN < 18
030020         ADD 1             TO CLIN
030030	       GO TO CG210.
030040       DISPLAY "Transaction balance:" AT 2012
                      WITH FOREGROUND-COLOR 3.

030050 CG215.
030060       MOVE W25-VALUE      TO W95-B.
030070	     DISPLAY W95-V2 AT 2035
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
030080	     DISPLAY CLEAR-L25.

030090 CG220.
030100	     ACCEPT S35 AT 1235.
030110     IF W40-INT > W25-VALUE
030120	       PERFORM CG190
030130	       GO TO CG220.
030140       SUBTRACT W40-INT    FROM W25-VALUE.
030150	     PERFORM CG215.
030160     IF W25-VALUE = ZERO
030170	       GO TO CG250.

030180 CG225.
030190	     ACCEPT S30 AT 1335.
030200     IF W40-CUR > W25-VALUE
030210	       PERFORM CG190
030220	       GO TO CG225.
030230       SUBTRACT W40-CUR    FROM W25-VALUE.
030240	     PERFORM CG215.
030250     IF W25-VALUE = ZERO
030260	       GO TO CG250.

030270 CG230.
030280	     ACCEPT S31 AT 1435.
030290     IF W40-30 > W25-VALUE
030300	       PERFORM CG190
030310	       GO TO CG230.
030320       SUBTRACT W40-30     FROM W25-VALUE.
030330	     PERFORM CG215.
030340     IF W25-VALUE = ZERO
030350	       GO TO CG250.

030360 CG235.
030370	     ACCEPT S32 AT 1535.
030380     IF W40-60 > W25-VALUE
030390	       PERFORM CG190
030400	       GO TO CG235.
030410       SUBTRACT W40-60     FROM W25-VALUE.
030420	     PERFORM CG215.
030430     IF W25-VALUE = ZERO
030440	       GO TO CG250.

030450 CG240.
030460	     ACCEPT S33 AT 1635.
030470     IF W40-90 > W25-VALUE
030480	       PERFORM CG190
030490	       GO TO CG240.
030500       SUBTRACT W40-90     FROM W25-VALUE.
030510	     PERFORM CG215.
030520     IF W25-VALUE = ZERO
030530	       GO TO CG250.
030430     IF W25-VALUE = ZERO
030440	       GO TO CG250.

030450 CG245.
030460	     ACCEPT S34 AT 1735.
030470     IF W40-120 > W25-VALUE
030480	       PERFORM CG190
030490	       GO TO CG245.
030500       SUBTRACT W40-120    FROM W25-VALUE.
030510	     PERFORM CG215.
030520     IF W25-VALUE = ZERO
030530	       GO TO CG250.
030540	     MOVE "Total must be allocated"
030550				 TO WS-ERR-STRING.
030560	     PERFORM ERROR-MESSAGE.
030570       MOVE ZERO           TO W40-CUR W40-30 W40-90 
                                    W40-INT W40-60 W40-120.
030580       MOVE W90-DB         TO W25-VALUE.
030590	     GO TO CG205.

030600 CG250.
	     PERFORM CHECK-CORRECT.
030690	   IF WS-OPTION = "N"
030700         MOVE W90-DB       TO W25-VALUE
030710         MOVE ZERO         TO W40-CUR W40-30 W40-90 
                                    W40-INT W40-60 W40-120 
030720	       GO TO CG205.
030730       ADD W40-CUR         TO DEB-CUR.
030740       ADD W40-30          TO DEB-30.
030750       ADD W40-60          TO DEB-60.
030760       ADD W40-90          TO DEB-90.
030760       ADD W40-120         TO DEB-120.
030770       SUBTRACT W40-CUR    FROM W90-CUR.
030780       SUBTRACT W40-30     FROM W90-30.
030790       SUBTRACT W40-60     FROM W90-60.
030800       SUBTRACT W40-90     FROM W90-90.
030800       SUBTRACT W40-120    FROM W90-120.

030810 CG255.
030820       MOVE DEB-ACNO       TO R-TAC.
030830       MOVE W60-NAME       TO R-DNME.
030840       MOVE W90-DTE        TO R-TDTE.
030860       MOVE W90-DESC       TO R-TDESC.
030870       MOVE W90-REF        TO R-TREF.
	     COMPUTE W90-TTAX = W90-VAT(1)  + W90-VAT(2)  + W90-VAT(3)
			      + W90-VAT(4)  + W90-VAT(5)  + W90-VAT(6)
			      + W90-VAT(7)  + W90-VAT(8)  + W90-VAT(9)
			      + W90-VAT(10) + W90-VAT(11) + W90-VAT(12).
030880     IF W90-DB > 0
	       MOVE W90-VAL	 TO R-VAL
	       MOVE W90-TTAX	 TO R-TAX
030890         MOVE W90-DB       TO R-TOT
030900     ELSE
	       MULTIPLY -1 BY W90-VAL
				 GIVING R-VAL
	       MULTIPLY -1 BY W90-TTAX
				 GIVING R-TAX
030910	       MULTIPLY -1 BY W90-CR
                                 GIVING R-TOT.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
030960	       PERFORM HA000.
030970       MOVE SPACES         TO R-DET1.
           IF W90-CR NOT = ZERO
               IF OPEN-ITEM
		   PERFORM CG290-OPEN
		   CLOSE DEBTRN.
030980       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.

030990 CG260.
             MOVE 0701           TO CPOS.
031000       PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
031010       MOVE W90-DEBIT      TO W100-BAL.
031020       MOVE W90-CREDIT     TO W100-CUR.
031030	     DISPLAY "Debit total:" AT 2112.
031040	     DISPLAY W100-BAL AT 2125
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
031050	     DISPLAY "Credit total:" AT 2139.
031060	     DISPLAY W100-CUR AT 2153
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 4.
	     PERFORM CG265-NEW THRU CG275.
	     PERFORM CG10 THRU CG12.
031110	     GO TO CG35.

031230 CG265-NEW.
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

031870 CG270.
	   IF WS-TAX = "N"
	       GO TO CG275.
             MOVE 9              TO WS-PARKEY.
031900	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
	     MOVE WS-VAT-CODE	 TO DPT-CODE.
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
		   ADD W90-VAL	 TO PAR-CVAT-DAY
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
		       MOVE DPT-O-VAT-GL
				 TO WX-AC
		       MOVE 11	 TO WX-TYPE
		       PERFORM GL-TRF-UPDATE
		   END-IF
	       ELSE
		   ADD W90-VAL	 TO PAR-DVAT-DAY
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
		       MOVE 10	 TO WX-TYPE
		       PERFORM GL-TRF-UPDATE.
031930       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
031930       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

031940 CG275.
031950     IF W00-T-CODE = 19
	       MOVE "XXXX"	 TO DPT-CODE
	       PERFORM READ-DEPART THRU READ-DEPART-EXIT
               PERFORM AY38 THRU AY59
031980         ADD W90-VAL       TO DPT-SALES-DAY
031990                              DPT-SALES-MTD
031990                              DPT-SALES-YTD
032000         PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
031860       PERFORM AY70 THRU AY999.

       CG280.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
032040	     MOVE "Total"	 TO R-TDESC.
             MOVE W90-VATTOT     TO R-TAX.
032050       COMPUTE R-TOT = W90-DEBIT - W90-CREDIT.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0701		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
	     DISPLAY CLEAR-L25.

       CG285.
041560	     MOVE "New Batch Y/N?  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO CG285.
032160	   IF WS-OPTION = "N"
032170	       GO TO CG999.
032180       GO TO CG00.

032190 CG999.
032200	     EXIT.

       CG290-OPEN      SECTION 51.
       CG290.
           IF W75-KEY < WS-DBTKEY
	       GO TO CG300.
             PERFORM READ-DEBTRN THRU READ-DEBTRN-EXIT.
           IF DBT-UPDATE = SPACE
	       GO TO CG295.
             MOVE DBT-TRA        TO TRA-RECORD1.
             MOVE 0              TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             ADD DBT-ALLOC       TO DBT-PAID.
           IF DBT-VALUE > DBT-PAID
               MOVE "X"          TO DBT-TYPE
           ELSE
               MOVE "P"          TO DBT-TYPE.
           IF DBT-CODE = 17
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-INT
               ADD DBT-ALLOC     TO W90-INT
           ELSE
           IF DBT-AGE = 5
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-120
               ADD DBT-ALLOC     TO W90-120
           ELSE
           IF DBT-AGE = 4
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-90
               ADD DBT-ALLOC     TO W90-90
           ELSE
           IF DBT-AGE = 3
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-60
               ADD DBT-ALLOC     TO W90-60
           ELSE
           IF DBT-AGE = 2
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-30
               ADD DBT-ALLOC     TO W90-30
           ELSE
           IF DBT-AGE = 1
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-CUR
               ADD DBT-ALLOC     TO W90-CUR.
             MOVE DBT-TRA        TO TRA-RECORD1.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.

       CG295.
             ADD 1               TO WS-DBTKEY.
	     GO TO CG290.

       CG300.
             EXIT.

       COPY DEBTOR52.TRN.

       COPY DBTRAN53.LUP.

       COPY DEBTOR54.TRA.

       COPY LEDTRF55.UPD.

       COPY DEBTOR56.ALL.

       HA000	       SECTION 57.
       HA00.
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

026670 HA05.
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
037830	     MOVE SPACES	 TO R-DET1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       HA99.
	     EXIT.
      *
      *    ****    C A L L   D E B T O R   C R E A T I O N
      *
       GA000	       SECTION 58.
       GA00.
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
       GA99.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E
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
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
      *    *************************************************************
      *    ** D E B T O R   T R A N S A C T I O N   W O R K   F I L E **
      *    *************************************************************
	     MOVE "DEBTRN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-DEBTRN.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
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
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
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
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *    *************************************************************
      *    ****      V A T   T R A N S A C T I O N S   F I L E	    ****
      *    *************************************************************
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
      *    *************************************************************
      *    ****   G E T   D E B T O R	M O N T H - E N D   D A Y   ****
      *    *************************************************************
	     MOVE 1		 TO WS-NETKEY.
	     PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	   IF DEB-MTH-END < 21 OR > 31 AND < 99
	       MOVE 99		 TO DEB-MTH-END.
	     MOVE DEB-MTH-END	 TO WS-MTHEND.
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
041840     ELSE
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
041850     IF WS-F-ERROR = 31
041860         MOVE W02-DEBTRN   TO WS-FILE
041870         MOVE WS-DBTKEY    TO WS-KEY
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
