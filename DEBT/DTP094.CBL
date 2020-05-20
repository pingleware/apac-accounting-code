      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******	**     *
      *   **	**     **     **    **	 **  **   **	**     ***     *
      *   **	**     **     **    **	**    **  **	**    * **     *
      *   **	**     **     *******	**    **   *******   *	**     *
      *   **	**     **     **	**    **	**  *******    *
      *   **	**     **     **	 **  **   **	**	**     *
      *   *******      **     **	  ****	   ******      ****    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       D E B T O R  B A L A N C I N G   U T I L I T Y	       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      * This program is still under development and is scheduled to be *
      * completed during 2011 (This is a nice to have, but not a must  *
      * have or critical requirement). The G/L balancing program has   *
      * been copied and the necessary amendments are being made to     *
      * adapt it for Debtors/Accounts receivable.		       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP094.
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

000010	   SELECT DTBTCH  ASSIGN DISK
000020			  LOCK MANUAL
000030                    STATUS WS-STATUS
000040                    ACCESS DYNAMIC
000050                    ORGANIZATION INDEXED
000060			  RECORD BCH-KEY
			  ALTERNATE RECORD BCH-VALUE DUPLICATES.

000120 COPY PARAM.SL.

000550	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000330 COPY DBTRAN.FDE.

000310 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

      /
000010 FD  DTBTCH    LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-DTBTCH.
000030 01  BCH-RECORD1.
	   03  BCH-KEY.
000170	       05  BCH-AC      PIC  X(06).
000060	       05  BCH-DATE    PIC  9(06)    COMP.
000040	       05  BCH-CODE    PIC  9(02).
000070	       05  BCH-REF     PIC  X(08).
000100	   03  BCH-VALUE       PIC S9(09)V99 COMP.

000290 COPY PARAM.FDE.

000730 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000750 01  R-L1.
000760     03  R-DET       PIC X(132).
000770 01  R-L2.
000780     03  R-H1        PIC X(06).
000790     03  R-DTE1      PIC Z9/99/99.
000800     03  FILLER      PIC X(10).
000810     03  R-H2        PIC X(40).
000820     03  R-H2A       PIC X(27).
000830     03  FILLER      PIC X(10).
000840     03  R-H3        PIC X(07).
000850     03  R-BTCH      PIC Z(04)9.
000860     03  FILLER      PIC X(10). 
000870     03  R-H4        PIC X(06).
000880     03  R-PGE       PIC ZZ9.
000890 01  R-L3.
002440	   03  LED-H1	       PIC  X(07).
002450     03  FILLER          PIC  X(02).
002460     03  LED-H2          PIC  X(12).
002470     03  FILLER          PIC  X(06).
           03  LED-H2A         PIC  X(10).
           03  FILLER          PIC  X(02).
002480     03  LED-H3          PIC  X(04).
002490     03  FILLER          PIC  X(04).
002500     03  LED-H4          PIC  X(04).
           03  FILLER          PIC  X(03).
002500     03  LED-H4A         PIC  X(05).
002510     03  FILLER          PIC  X(04).
002520     03  LED-H5          PIC  X(05).
002530     03  FILLER          PIC  X(02).
002540     03  LED-H6          PIC  X(15).
002550     03  FILLER          PIC  X(28).
002560     03  LED-H7          PIC  X(05).
002570     03  FILLER          PIC  X(06).
002580     03  LED-H8          PIC  X(07).
002590     03  FILLER          PIC  X(01).
001060 01  R-L4.
001080     03  R-ACC       PIC 9(04)/99.
001090	   03  FILLER	   PIC X(02).
001100     03  R-ANME      PIC X(24).
001110     03  FILLER      PIC X(04).
001120     03  R-DTE2      PIC Z9/99/99.
001130	   03  FILLER	   PIC X(02).
002680	   03  R-CDE	   PIC X(04).
	   03  FILLER	   PIC X(02).
	   03  R-BATCH	   PIC Z(06).
002690	   03  FILLER	   PIC X(02).
001140	   03  R-REF	   PIC Z(07).
001150	   03  FILLER	   PIC X(02).
001160     03  R-NAR       PIC X(36).
001170     03  FILLER      PIC X(01).
001180     03  R-DBT       PIC Z(08)9.99.
	   03  FILLER	   PIC X(01).
001200	   03  R-CRD	   PIC Z(07)9.99-.
001220 01  R-L5.
001230     03  RI-H1       PIC X(06).
001240     03  RI-H2       PIC X(40).
001250     03  RI-H3       PIC X(39).
001260     03  RI-H4       PIC X(42).
001270     03  RI-H5       PIC X(05).
001280 01  R-L6.
001290     03  RI-NO       PIC Z(04)9.
001300     03  FILLER      PIC X(01).
001310     03  RI-ACDT     PIC 9999/99.
001320     03  FILLER      PIC X(01).
001330     03  RI-DTNAME   PIC X(30).
001340     03  FILLER      PIC X(02).
001350     03  RI-ACCR     PIC 9999/99.
001360     03  FILLER      PIC X(01).
001370     03  RI-CRNAME   PIC X(30).
001380     03  FILLER      PIC X(01).
001390     03  RI-NAR      PIC X(36).
001400     03  RI-VAL      PIC Z(07)9.99.

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
001680 77  WS-DBTKEY       PIC  9(06)    COMP-5.
001670 77  WS-DEBKEY       PIC  9(06)    COMP-5.
001670 77  WS-DEPKEY       PIC  9(06)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001720 77  WS-RECOVER      PIC  9(06)    COMP-5.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
001740 77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
001750 77  WS-FIN-RATE     PIC S9(03)V99 COMP-3.
001760 77  WS-PERIOD       PIC  9(02).
001770 77  WS-NTP          PIC  X(01).
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
002100 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002110 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "094".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DBTRAN.ID.

       COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

000970	   03  W02-DTBTCH.
000990	       05  FILLER      PIC  X(05) VALUE "DBAL_".
001000	       05  W02-EXT5B   PIC  X(03).
	       05  FILLER      PIC  X(01) VALUE ".".
	       05  W02-USE5B   PIC  X(03) VALUE "APC".

       COPY PARAM.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "DTP094.".
               05  W02-USER   PIC X(05) VALUE SPACES.

002500 01  W10-SPARES.
002510	   03  W10-DATE	    PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

002530 01  W12-DATE.
003170 01  W12-DATE.
003180	   03  W12-TODAY      PIC 9(08).
003190     03  W12-DATE-DMY REDEFINES W12-TODAY.
003200         05  W12-DAY    PIC 9(02).
003210         05  W12-MONTH  PIC 9(02).
	       05  W12-CENT   PIC 9(02).
003220         05  W12-YEAR   PIC 9(02).
003230	   03  W12-T-YMD      PIC 9(08).
003240     03  W12-DATE-YMD REDEFINES W12-T-YMD.
003250	       05  W12-CC     PIC 9(02).
003250         05  W12-YY     PIC 9(02).
003260         05  W12-MM     PIC 9(02).
003270         05  W12-DD     PIC 9(02).

002640 01  W20-DATE.
	   03  W20-CCYY.
	       05  W20-CC.
		   07  W20-CENT	PIC  9(02).
002650	       05  W20-YY.
002660		   07  W20-YEAR	PIC  9(02).
	   03  W20-CNYR REDEFINES W20-CCYY
			       PIC  9(04).
002670     03  W20-MM.
002680	       05  W20-MONTH   PIC  9(02).
002690     03  W20-DD.
002700	       05  W20-DAY     PIC  9(02).
002710 01  W20-DTE1 REDEFINES W20-DATE.
002720	   03  W20-DTE	       PIC  9(08).

002730 01  W22-DATES.
002740	   03  W22-DTE1        PIC  9(08).
002750	   03  W22-DTE2        PIC  9(08).
002760     03  W22-DATE2 REDEFINES W22-DTE2.
	       05  W22-CC2     PIC  9(02).
002770	       05  W22-YY2     PIC  9(02).
002780	       05  W22-MM2     PIC  9(02).
002790	       05  W22-DD2     PIC  9(02).
002760	   03  W22-DATE3 REDEFINES W22-DTE2.
	       05  W22-CY2     PIC  9(04).
	       05  FILLER      PIC  9(04).
002800	   03  W22-DTE3        PIC  9(08).
002810     03  W22-DATE3 REDEFINES W22-DTE3.
002820	       05  W22-DD3     PIC  9(02).
002830	       05  W22-MM3     PIC  9(02).
	       05  W22-CC3     PIC  9(02).
002840	       05  W22-YY3     PIC  9(02).

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

003980 01  W90-VALUES.
004090	   03  W90-DEBIT       PIC S9(09)V99 COMP-3.
004090	   03  W90-CREDIT      PIC S9(09)V99 COMP-3.

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

004580 SCREEN SECTION.

       COPY BLANK.CRT.

006150 01  S11.
006160	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
006170				 VALUE "BALANCING UTILITIES".
006180	   03  LINE  4 COLUMN 12 VALUE "Balance an ".
	   03	       COLUMN 23 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 24 VALUE "ccount, ".
	   03	       COLUMN 32 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "T".
	   03	       COLUMN 33 VALUE "ran. code or ".
	   03	       COLUMN 46 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "D"
	   03	       COLUMN 47 VALUE "ate > < ".
	   03	       COLUMN 55 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 58 VALUE "ape to Exit".

007910 01  S12.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

007910 01  S13.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(02) USING W40-CODE AUTO.

007910 01  S14.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 9(06) USING W40-DATE AUTO.

       COPY DEBLUP.CRT.

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
006540 AA000	       SECTION.
006550 AA00.
003240	   IF LS0-DBLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006560	     PERFORM ZA000.
	     PERFORM DA000.
	     CLOSE DTBTCH
		   PRNREP.

       AA49.
	     EXIT PROGRAM.

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

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY DBTRAN.RD1.

      *
      *    ****  ***** ****  *****  ***  *   *	  ***** *** *	  *****
      *    *   *   *   *   *   *   *   * *   *	  *	 *  *	  *
      *    *   *   *   *   *   *   *	 *   *	  *	 *  *	  *
      *    *   *   *   ****    *   *	 *****	  ***	 *  *	  ***
      *    *   *   *   *   *   *   *	 *   *	  *	 *  *	  *
      *    *   *   *   *   *   *   *   * *   *	  *	 *  *	  *
      *    ****    *   ****    *    ***  *   *	  *	*** ***** *****
      *
008370 READ-DTBTCH.
008380	     READ DTBTCH
	       KEY IS BCH-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 55		 TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 55		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-DTBTCH	 TO WS-FILE
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-DTBTCH.
008470	     MOVE 55		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-DTBTCH-EXIT.

008370 READ-DTBTCH-NEXT.
008380	     READ DTBTCH NEXT.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 55		 TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 55		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-DTBTCH	 TO WS-FILE
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-DTBTCH-NEXT.
008470	     MOVE 55		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-DTBTCH-EXIT.

008370 READ-DTBTCH-PREV.
008380	     READ DTBTCH PREVIOUS.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 55		 TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 55		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
008440     IF RECORD-LOCKED
	       MOVE W02-DTBTCH	 TO WS-FILE
008450         PERFORM LOCKED-RECORD
008460	       GO TO READ-DTBTCH-PREV.
008470	     MOVE 55		 TO WS-F-ERROR.
008480       PERFORM READ-ERROR.
	     GO TO READ-DTBTCH-EXIT.

008370 START-AT-DTBTCH-KEY.
008380	     START DTBTCH
	       KEY >= BCH-KEY.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 55		 TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 55		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.
	     GO TO READ-DTBTCH-EXIT.

008370 START-AT-DTBTCH-VAL.
008380	     START DTBTCH
	       KEY >= BCH-VAL.
008390     IF WS-STAT1 = "0"
               MOVE ZERO         TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
008420	       MOVE 55		 TO WS-F-ERROR
008400	       GO TO READ-DTBTCH-EXIT.
008410     IF WS-STAT1 = "2" OR "3" OR "4"
008420	       MOVE 55		 TO WS-F-ERROR
008430         PERFORM READ-ERROR.

008490 READ-DTBTCH-EXIT.
008500       EXIT.

       COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

009200 WRITE-DTBTCH.
009260	     WRITE BCH-RECORD1.
009270     IF WS-STAT1 NOT = "0"
009330	       MOVE 55		 TO WS-F-ERROR
009340         PERFORM WRITE-ERROR.

009350 WRITE-DTBTCH-EXIT.
009360       EXIT.

       COPY APAC.HLP.

      /
024350 DA000	     SECTION 5.
024360 DA00.
024370       DISPLAY CLR-SCREEN.
024380	     DISPLAY S11.
	     MOVE "T"		 TO WS-OPTION.

       DA02.
	     ACCEPT WS-OPTION AT 0452
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA02
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     EVALUATE WS-OPTION
	       WHEN "A"

	       WHEN "D"

	       WHEN "T"

	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO DA02
	     END-EVALUATE.





	   IF WS-OPTION = "B"
	       GO TO DA15.

       DA03.
	     DISPLAY "Account Number" AT 0612.
024390	     MOVE SPACES	 TO W80-ACNO.

024400 DA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
024410	     ACCEPT S17 AT 0628.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO DA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE
	       DISPLAY S17 AT 0628
	       IF W80-ACNO = SPACES
015990		   GO TO DA05.
	     DISPLAY CLEAR-L25.
024430	   IF W80-ACNO = SPACES
024440	       GO TO DA999.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
024460	     DISPLAY CLEAR-L25.
024470	   IF WS-F-ERROR NOT = ZERO
024480	       MOVE "No Record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
024490	       GO TO DA05.
	     MOVE DEB-NAME	 TO W4O-NAME.
	   IF DEB-TYPE = 1
019040	       INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
024500	     DISPLAY W40-NAME AT 0636
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
024580	   IF WS-OPTION = "N"
024590	       GO TO DA03.
	     INITIALIZE TRA-RECORD1.
	     MOVE W80-ACNO	 TO TRA-AC.
	     PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5
	       MOVE "No transactions for this account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA00.
	     MOVE ZERO		 TO W90-DEBIT W90-CREDIT.
	     DISPLAY "Reading :" AT 0812.

       DA10.
020110	     PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
	     COMMIT.
           IF (WS-F-ERROR = 11) OR
              (T-AC NOT = G-AC)
	       MOVE "A" 	 TO WS-BAL-TYPE
	       GO TO DA35.
	     DISPLAY T-KEY AT 0822
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *
      *    ****    I G N O R E	 S E C O N D   N A R R A T I V E
      *
	   IF T-SUB = 5
	       GO TO DA10.
	     MOVE T-KEY 	 TO B-KEY.
002080	     MOVE T-NAR 	 TO B-NAR.
002130	     MOVE T-VAL 	 TO B-VAL.
002170	     MOVE T-FLAG	 TO B-FLAG.
	     MOVE T-BATCH	 TO B-BATCH.
	     PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
	   IF T-VAL < ZERO
	       ADD T-VAL	 TO W20-CREDIT
	   ELSE
	       ADD T-VAL	 TO W20-DEBIT.
	     GO TO DA10.

       DA15.
	     DISPLAY "Batch Number" AT 0612.
024390	     MOVE ZERO		 TO W15-ACCOUNT.
	     MOVE "B"		 TO WS-BAL-TYPE.
       DA20.
024410	     ACCEPT S17 AT 0626.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA20
	       END-EVALUATE.
	   IF W15-ACCOUNT = ZERO
	       GO TO DA999.
	     MOVE W15-ACCOUNT	 TO WS-BATCH.
	     PERFORM CHECK-CORRECT.
024580	   IF WS-OPTION = "N"
024590	       GO TO DA20.
	     INITIALIZE T-REC.
	     PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
	       MOVE "No transactions on file" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA00.
	     MOVE ZERO		 TO W20-DEBIT W20-CREDIT.
	     DISPLAY "Reading :" AT 0812.

       DA25.
020110	     PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
	     COMMIT.
	   IF WS-F-ERROR = 11
	       GO TO DA30.
	     DISPLAY T-KEY AT 0822
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF T-BATCH NOT = WS-BATCH
	       GO TO DA25.
      *
      *    ****    I G N O R E	 S E C O N D   N A R R A T I V E
      *
	   IF T-SUB = 5
	       GO TO DA25.
	     MOVE T-KEY 	 TO B-KEY.
002080	     MOVE T-NAR 	 TO B-NAR.
002130	     MOVE T-VAL 	 TO B-VAL.
002170	     MOVE T-FLAG	 TO B-FLAG.
	     MOVE T-BATCH	 TO B-BATCH.
	     PERFORM WRITE-GLBTCH THRU WRITE-GLBTCH-EXIT.
	   IF T-VAL < ZERO
	       ADD T-VAL	 TO W20-CREDIT
	   ELSE
	       ADD T-VAL	 TO W20-DEBIT.
	     GO TO DA25.

       DA30.
	   IF (W20-DEBIT + W20-CREDIT) = ZERO
	       DISPLAY "Batch is in balance - " AT 2502
			WITH FOREGROUND-COLOR 1
			     BACKGROUND-COLOR 3
		       "P" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		       "rint, " WITH BACKGROUND-COLOR 3
				     FOREGROUND-COLOR 1
		       "N" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		       "ext or " WITH BACKGROUND-COLOR 3
				      FOREGROUND-COLOR 1
		       "E" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		       "xit " WITH BACKGROUND-COLOR 3
				   FOREGROUND-COLOR 1
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       IF USER-FUNC
		   EVALUATE KEY-CODE-1
		     WHEN ESC-KEY
			  GO TO DA999
		     WHEN OTHER	 CALL X"E5"
		   END-EVALUATE
		   GO TO DA30
	       ELSE
	       IF DATA-8BIT
		   MOVE KEY-CODE-1X  TO WS-OPTION
		   IF NOT (WS-OPTION = "E" OR "N" OR "P")
		       CALL X"E5"
		       GO TO DA30
		   ELSE
		   DISPLAY CLEAR-L25
		   IF WS-OPTION = "E"
		       GO TO DA999
		   ELSE
		   IF WS-OPTION = "N"
		       GO TO DA15
		   ELSE
		       GO TO DA35
	       ELSE
		   CALL X"E5"
		   GO TO DA30.

       DA35.
013390	     MOVE ZERO		 TO WS-PAGE.
	     MOVE ZERO		 TO W15-ACCOUNT.
	   IF WS-BAL-TYPE = "B"
013400	       MOVE "G/L BATCH LISTING"
				 TO W10-HEAD
	   ELSE
	       MOVE "G/L ACCOUNT TRANSACTIONS"
				 TO W10-HEAD.
013460       MOVE WS-COND        TO R-DET.
013470       WRITE R-L1 BEFORE 0 LINES.
	     MOVE "V"		 TO WS-OPTION.
	     DISPLAY "D" AT 1012 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ate or " "V" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "alue sequence".

       DA38.
	     ACCEPT WS-OPTION AT 1036
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024580	   IF NOT (WS-OPTION = "D" OR "V")
	       GO TO DA38.

       DA40.
005460	     MOVE SPACES	 TO R-DET.
005470       MOVE WS-H1          TO R-H1.
005480       MOVE W90-COMP       TO R-H2.
005490       MOVE W10-H2         TO R-H2A.
005510       MOVE WS-H4          TO R-H4.
005520       ADD 1               TO WS-PAGE.
005530       MOVE WS-PAGE        TO R-PGE.
	   IF WS-BAL-TYPE = "B"
005500	       MOVE WS-H3	 TO R-H3
005540	       MOVE WS-BATCH	 TO R-BTCH.
005550       MOVE W05-TODAY      TO R-DTE1.
005560       WRITE R-L1 BEFORE 2 LINES.
005570       MOVE SPACES         TO R-DET.
019400	     MOVE "ACNT NO"	 TO LED-H1.
019410       MOVE "ACCOUNT NAME" TO LED-H2.
019420       MOVE "DATE"         TO LED-H3.
019430       MOVE "CODE"         TO LED-H4.
             MOVE "BATCH"        TO LED-H4A.
019440       MOVE "REFER"        TO LED-H5.
019450       MOVE "PAYEE/NARRATIVE"
                                 TO LED-H6.
019460	     MOVE "DEBIT"	 TO LED-H7.
019470	     MOVE " CREDIT"	 TO LED-H8.
013610	     WRITE R-L1 BEFORE 2 LINES.
013620       MOVE SPACES         TO R-DET.
	   IF WS-BAL-TYPE = "A"
	       MOVE G-AC	 TO R-ACC
014140	       MOVE G-NAME	 TO R-ANME W15-ACCOUNT.

       DA45.
	     INITIALIZE B-REC.
	   IF WS-OPTION = "V"
	       MOVE ZERO	 TO B-VAL
	       PERFORM START-AT-GLBTCH-VAL THRU READ-GLBTCH-EXIT
	   ELSE
	       PERFORM START-AT-GLBTCH-KEY THRU READ-GLBTCH-EXIT.
	   IF WS-F-ERROR = 61
	       GO TO DA55.

       DA50.
	     PERFORM READ-GLBTCH-NEXT THRU READ-GLBTCH-EXIT.
	   IF WS-F-ERROR = 61
	       GO TO DA55.
	   IF WS-BAL-TYPE = "B"
	       IF B-AC NOT = W15-ACCOUNT
		   MOVE B-AC	 TO W15-ACCOUNT
		   PERFORM AD000
		   MOVE G-AC	 TO R-ACC
		   MOVE G-NAME	 TO R-ANME.
014170	     MOVE B-DATE	 TO W15-YMD
	     MOVE W15-YR	 TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
020280	     MOVE W15-DATE	 TO R-DTE2.
020290	   IF B-CODE = 01 OR 02
020300	       MOVE "JNL"	 TO R-CDE
020310     ELSE
020320	   IF B-CODE = 03 OR 04
020330	       MOVE "CHEQ"	 TO R-CDE
020340     ELSE
020350	   IF B-CODE = 06 OR 07
020360	       MOVE "DEP"	 TO R-CDE.
	   IF WS-BAL-TYPE = "A"
	       MOVE B-BATCH	 TO R-BATCH.
014150	     MOVE B-NAR		 TO R-NAR.
014160	     MOVE B-REFER	 TO R-REF.
014180	   IF B-VAL < ZERO
014190	       MOVE B-VAL	 TO R-CRD
014200     ELSE
014210	       MOVE B-VAL	 TO R-DBT.
014570	   IF LINAGE-COUNTER < WS-PRN-LENGTH
014580         WRITE R-L1 BEFORE 1 LINES
014590     ELSE
014600         WRITE R-L1 BEFORE PAGE
014610	       PERFORM DA40
	       IF WS-BAL-TYPE = "B"
		   MOVE ZERO	 TO W15-ACCOUNT.
014620	     MOVE SPACES TO R-DET.
	     GO TO DA50.

       DA55.
014580	     WRITE R-L1 BEFORE 1 LINES
	     MOVE "****  E N D   R E P O R T  ****"
				 TO R-NAR.
	     MOVE W20-DEBIT	 TO R-DBT.
	     MOVE W20-CREDIT	 TO R-CRD.
014600	     WRITE R-L1 BEFORE PAGE.
	     MOVE SPACES	 TO R-DET.
	     CLOSE GLBTCH.
	     OPEN OUTPUT GLBTCH.
	     CLOSE GLBTCH.
	     OPEN I-O GLBTCH.
	     GO TO DA00.

025200 DA999.
025210       EXIT.




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
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
042110	     MOVE WS-SYS-ID	 TO W02-PTH5B.
042160	     MOVE LS-USER	 TO W02-USE5B.
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
	   IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
	   ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
042170 ZA02.
	     OPEN OUTPUT DTBTCH.
035480	   IF RUNTIME-ERROR
               IF FLE-LIMIT
                   GO TO ZA49.
035500     IF WS-STATUS NOT = "00"
035510	       MOVE 55		 TO WS-F-ERROR
050480         PERFORM OPEN-ERROR.
	     CLOSE DTBTCH.
	     OPEN I-O DTBTCH.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
022530	     OPEN OUTPUT PRNREP.
044780	     GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup the Screen Graphics Characters: ³ Ä Ú ¿ À Ù Â Á etc ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
036260 ZA200.
036270	     DISPLAY "Files locked - Try later" AT 2502
036280		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     WS-STATUS
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
036290		     " Press any key"
036300		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
036310	     ACCEPT WS-OPTION AT 2545
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA210.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	   IF WS-PRN-STAT > 127
	       MOVE 1		 TO WS-S1
	       SUBTRACT 128	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 63
	       MOVE 1		 TO WS-S2
	       SUBTRACT 64	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 31
	       MOVE 1		 TO WS-S3
	       SUBTRACT 32	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 15
	       MOVE 1		 TO WS-S4
	       SUBTRACT 16	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 7
	       MOVE 1		 TO WS-S5
	       SUBTRACT 8	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 3
	       MOVE 1		 TO WS-S6
	       SUBTRACT 4	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 1
	       MOVE 1		 TO WS-S7
	       SUBTRACT 2	 FROM WS-PRN-STAT.
	     MOVE WS-PRN-STAT	 TO WS-S8.
	   IF WS-S3 = 0 AND
	      WS-S5 = 0 AND
	      WS-S8 = 0
	       GO TO ZA215.
	     DISPLAY "Check printer - " AT 2304
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "etry - "
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "bort".
       ZA212.
	     ACCEPT WS-OPTION AT 2334
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     IF NOT (WS-OPTION = "A" OR "R")
		 GO TO ZA212.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO ZA215.
	       GO TO ZA210.
       ZA215.
	     EXIT.
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
041640     ELSE
041730     IF WS-F-ERROR = 5
041740         MOVE W02-DBTRAN TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760     ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 12
041860         MOVE W02-INVOIC TO WS-FILE
041870         MOVE ZERO      TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
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
	       MOVE DME-KEY	 TO WS-KEYX.
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
