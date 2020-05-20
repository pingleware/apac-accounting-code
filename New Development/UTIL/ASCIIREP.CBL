      $set LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    ***	*****	  *****  **** ****     ******  ****** ******   *
      *   ** **  **   ** **   **  **   **      **   ** **     **   **  *
      *  **   ** **	 **	    **   **      **   ** **     **   **  *
      *  *******  *****	 **	    **   **  *** *****   ****   ******   *
      *  **   **      ** **	    **   **      **  **  **     **       *
      *  **   ** **   ** **   **  **   **      **   ** **     **       *
      *  **   **  *****	  *****  **** ****     **   ** ****** **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   P R I N T   A S C I I   C H A R A C T E R S  *
      *                                                                *
      *     VERSION 9.00.00 - August 2012		                       *
      * 							                       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     ASCIIREP.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2012
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

000160	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000170 DATA DIVISION.
000180 FILE SECTION.

      /
000240 FD  PRNREP      LABEL RECORD OMITTED
000250		       LINAGE WS-PGE-LENGTH.
000260 01  REP-L1.
000270	   03  REP-DETAIL      PIC  X(132).
000280 01  REP-L2.
000290	   03  FILLER	     PIC  X(28).
000300	   03  REP-HEAD        PIC  X(24).
000310	   03  FILLER	     PIC  X(28).
000320 01  REP-L3.
000330     03  REP-GRPH        OCCURS 4.
000340	       05  FILLER      PIC  X(01).
000350	       05  REP-H1      PIC  X(06).
000360	       05  REP-H2      PIC  X(06).
000370	       05  REP-H3      PIC  X(06).
000380	       05  REP-DIV     PIC  X(01).
000390 01  REP-L4.
000400     03  REP-ASCII       OCCURS 4.
000410	       05  FILLER      PIC  X(01).
000420	       05  REP-DEC     PIC  Z99.
000430	       05  FILLER      PIC  X(03).
000440	       05  REP-HEX     PIC  X(02).
000450	       05  FILLER      PIC  X(04).
000460         05  REP-GRAPH.
000470		   07  FILLER    PIC  X(02).
000480               07  REP-ALPH  PIC X(01).
000490		   07  FILLER    PIC  X(04).
000500 01  SYS-L1.
000510	   03  FILLER	     PIC  X(05).
000520	   03  SYS-H1	     PIC  X(09).
000530	   03  FILLER	     PIC  X(03).
000540	   03  SYS-H2	     PIC  X(05).
000550	   03  FILLER	     PIC  X(03).
000560	   03  SYS-H3	     PIC  X(09).
000570	   03  FILLER	     PIC  X(03).
000580	   03  SYS-H4	     PIC  X(07).
000590	   03  FILLER	     PIC  X(36).
000600 01  SYS-L2.
000610	   03  FILLER	     PIC  X(07).
000620	   03  SYS-FILE        PIC  X(06).
000630	   03  FILLER	     PIC  X(06).
000640	   03  SYS-DRIVE       PIC  X(01).
000650	   03  FILLER	     PIC  X(08).
000660	   03  SYS-EXT	     PIC  X(03).
000670	   03  FILLER	     PIC  X(07).
000680	   03  SYS-SIZE        PIC  Z(04)9.
000690	   03  FILLER	     PIC  X(37).
      /
000700 WORKING-STORAGE SECTION.
       77  WS-CHECK	 PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000710 77  WS-S1	       PIC  9(04) COMP-5.
000720 77  WS-S2	       PIC  9(04) COMP-5.
000730 77  WS-S3	       PIC  9(04) COMP-5.
000740 77  WS-S4	       PIC  9(04) COMP-5.

001490 01  W15-HEX.
001500     03  W15-ASCII PIC X(01) OCCURS 256.

      /
010570 PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      DECIMAL/HEX/ASCII TABLE		           ³
      *    ³ This routine has  been inserted  to help  both developers ³
      *    ³ and users.  This is a table  that is never available when ³
      *    ³ needed so I included this routine to solve the problem    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015230 AJ000		       SECTION.
       AJ0.
             DISPLAY "Waiting for printer response" AT 2312 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
006780       OPEN OUTPUT PRNREP.
             CALL X"91" USING X91-RES X91-FUN PRNREP.
             DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.

015240 AJ00.
015250       DISPLAY CLR-SCREEN.
015260       DISPLAY "Busy printing DECIMAL/HEX/ASCII Table" AT 1212.
015270       MOVE SPACES	 TO REP-DETAIL.
015280       MOVE "DECIMAL/HEX/ASCII TABLE"
                               TO REP-HEAD.
015290       WRITE REP-L1 BEFORE 1 LINES.
015300       MOVE 0            TO WS-C1.
015310       MOVE 64	       TO WS-C2.
015320       MOVE 128	       TO WS-C3.
015330       MOVE 192	       TO WS-C4.
015340       MOVE SPACES	 TO REP-DETAIL.
015350       MOVE "DEC" 	 TO REP-H1(1) REP-H1(2) REP-H1(3) REP-H1(4).
015360       MOVE "HEX" 	 TO REP-H2(1) REP-H2(2) REP-H2(3) REP-H2(4).
015370       MOVE "ASCII"	 TO REP-H3(1) REP-H3(2) REP-H3(3) REP-H3(4).

015390 AJ05.
015400       MOVE "|"	       TO REP-DIV(1) REP-DIV(2) REP-DIV(3) REP-DIV(4).
015420       WRITE REP-L1 BEFORE 1 LINES.
015430	     MOVE SPACES	 TO REP-DETAIL.
015440	     MOVE WS-C1 	 TO REP-DEC (1).
015450	     MOVE WS-C2 	 TO REP-DEC (2).
015460	     MOVE WS-C3 	 TO REP-DEC (3).
015470	     MOVE WS-C4 	 TO REP-DEC (4).
015480	     ADD 1		 TO WS-C1.
015490	     ADD 1		 TO WS-C2.
015500           ADD 1		 TO WS-C3.
015510	     ADD 1		 TO WS-C4.

015520 AJ15.
015530	     MOVE "00"		 TO REP-HEX (1).
015540	     MOVE "40"		 TO REP-HEX (2).
015550	     MOVE "80"		 TO REP-HEX (3).
015560	     MOVE "C0"		 TO REP-HEX (4).
015570	     MOVE "NUL" 	 TO REP-GRAPH (1).
015580	     MOVE "@"		 TO REP-ALPH (2).
015580	     MOVE X"80" 	 TO REP-ALPH (3).
015580	     MOVE X"C0" 	 TO REP-ALPH (4).
015590       PERFORM AJ05.
015600	     MOVE "01"		 TO REP-HEX (1).
015610	     MOVE "41"		 TO REP-HEX (2).
015620	     MOVE "81"		 TO REP-HEX (3).
015630	     MOVE "C1"		 TO REP-HEX (4).
015640	     MOVE "SOH" 	 TO REP-GRAPH (1).
015650	     MOVE "A"		 TO REP-ALPH (2).
015580	     MOVE X"81" 	 TO REP-ALPH (3).
015580	     MOVE X"C1" 	 TO REP-ALPH (4).
015660       PERFORM AJ05.
015670	     MOVE "02"		 TO REP-HEX (1).
015680	     MOVE "42"		 TO REP-HEX (2).
015690	     MOVE "82"		 TO REP-HEX (3).
015700	     MOVE "C2"		 TO REP-HEX (4).
015710	     MOVE "STX" 	 TO REP-GRAPH (1).
015720	     MOVE "B"		 TO REP-ALPH (2).
015580	     MOVE X"82" 	 TO REP-ALPH (3).
015580	     MOVE X"C2" 	 TO REP-ALPH (4).
015730       PERFORM AJ05.
015740	     MOVE "03"		 TO REP-HEX (1).
015750	     MOVE "43"		 TO REP-HEX (2).
015760	     MOVE "83"		 TO REP-HEX (3).
015770	     MOVE "C3"		 TO REP-HEX (4).
015780	     MOVE "ETX" 	 TO REP-GRAPH (1).
015790	     MOVE "C"		 TO REP-ALPH (2).
015580	     MOVE X"83" 	 TO REP-ALPH (3).
015580	     MOVE X"C3" 	 TO REP-ALPH (4).
015800       PERFORM AJ05.
015810	     MOVE "04"		 TO REP-HEX (1).
015820	     MOVE "44"		 TO REP-HEX (2).
015830	     MOVE "84"		 TO REP-HEX (3).
015840	     MOVE "C4"		 TO REP-HEX (4).
015850	     MOVE "EOT" 	 TO REP-GRAPH (1).
015860	     MOVE "D"		 TO REP-ALPH (2).
015580	     MOVE X"84" 	 TO REP-ALPH (3).
015580	     MOVE X"C4" 	 TO REP-ALPH (4).
015870       PERFORM AJ05.
015880	     MOVE "05"		 TO REP-HEX (1).
015890	     MOVE "45"		 TO REP-HEX (2).
015900	     MOVE "85"		 TO REP-HEX (3).
015910	     MOVE "C5"		 TO REP-HEX (4).
015920	     MOVE "ENQ" 	 TO REP-GRAPH (1).
015930	     MOVE "E"		 TO REP-ALPH (2).
015580	     MOVE X"85" 	 TO REP-ALPH (3).
015580	     MOVE X"C5" 	 TO REP-ALPH (4).
015940       PERFORM AJ05.
015950	     MOVE "06"		 TO REP-HEX (1).
015960	     MOVE "46"		 TO REP-HEX (2).
015970	     MOVE "86"		 TO REP-HEX (3).
015980	     MOVE "C6"		 TO REP-HEX (4).
015990	     MOVE "ACK" 	 TO REP-GRAPH (1).
016000	     MOVE "F"		 TO REP-ALPH (2).
015580	     MOVE X"86" 	 TO REP-ALPH (3).
015580	     MOVE X"C6" 	 TO REP-ALPH (4).
016010       PERFORM AJ05.
016020	     MOVE "07"		 TO REP-HEX (1).
016030	     MOVE "47"		 TO REP-HEX (2).
016040	     MOVE "87"		 TO REP-HEX (3).
016050	     MOVE "C7"		 TO REP-HEX (4).
016060	     MOVE "BEL" 	 TO REP-GRAPH (1).
016070	     MOVE "G"		 TO REP-ALPH (2).
015580	     MOVE X"87" 	 TO REP-ALPH (3).
015580	     MOVE X"C7" 	 TO REP-ALPH (4).
016080       PERFORM AJ05.
016090	     MOVE "08"		 TO REP-HEX (1).
016100	     MOVE "48"		 TO REP-HEX (2).
016110	     MOVE "88"		 TO REP-HEX (3).
016120	     MOVE "C8"		 TO REP-HEX (4).
016130	     MOVE "BS"		 TO REP-GRAPH (1).
016140	     MOVE "H"		 TO REP-ALPH (2).
015580	     MOVE X"88" 	 TO REP-ALPH (3).
015580	     MOVE X"C8" 	 TO REP-ALPH (4).
016150       PERFORM AJ05.
016160	     MOVE "09"		 TO REP-HEX (1).
016170	     MOVE "49"		 TO REP-HEX (2).
016180	     MOVE "89"		 TO REP-HEX (3).
016190	     MOVE "C9"		 TO REP-HEX (4).
016200	     MOVE "HT"		 TO REP-GRAPH (1).
016210	     MOVE "I"		 TO REP-ALPH (2).
015580	     MOVE X"89" 	 TO REP-ALPH (3).
015580	     MOVE X"C9" 	 TO REP-ALPH (4).
016220       PERFORM AJ05.
016230	     MOVE "0A"		 TO REP-HEX (1).
016240	     MOVE "4A"		 TO REP-HEX (2).
016250	     MOVE "8A"		 TO REP-HEX (3).
016260	     MOVE "CA"		 TO REP-HEX (4).
016270	     MOVE "LF"		 TO REP-GRAPH (1).
016280	     MOVE "J"		 TO REP-ALPH (2).
015580	     MOVE X"8A" 	 TO REP-ALPH (3).
015580	     MOVE X"CA" 	 TO REP-ALPH (4).
016290       PERFORM AJ05.
016300	     MOVE "0B"		 TO REP-HEX (1).
016310	     MOVE "4B"		 TO REP-HEX (2).
016320	     MOVE "8B"		 TO REP-HEX (3).
016330	     MOVE "CB"		 TO REP-HEX (4).
016340	     MOVE "VT"		 TO REP-GRAPH (1).
016350	     MOVE "K"		 TO REP-ALPH (2).
015580	     MOVE X"8B" 	 TO REP-ALPH (3).
015580	     MOVE X"CB" 	 TO REP-ALPH (4).
016360       PERFORM AJ05.
016370	     MOVE "0C"		 TO REP-HEX (1).
016380	     MOVE "4C"		 TO REP-HEX (2).
016390	     MOVE "8C"		 TO REP-HEX (3).
016400	     MOVE "CC"		 TO REP-HEX (4).
016410	     MOVE "FF"		 TO REP-GRAPH (1).
016420	     MOVE "L"		 TO REP-ALPH (2).
015580	     MOVE X"8C" 	 TO REP-ALPH (3).
015580	     MOVE X"CC" 	 TO REP-ALPH (4).
016430       PERFORM AJ05.
016440	     MOVE "0D"		 TO REP-HEX (1).
016450	     MOVE "4D"		 TO REP-HEX (2).
016460	     MOVE "8D"		 TO REP-HEX (3).
016470	     MOVE "CD"		 TO REP-HEX (4).
016480	     MOVE "CR"		 TO REP-GRAPH (1).
016490	     MOVE "M"		 TO REP-ALPH (2).
015580	     MOVE X"8D" 	 TO REP-ALPH (3).
015580	     MOVE X"CD" 	 TO REP-ALPH (4).
016500       PERFORM AJ05.
016510	     MOVE "0E"		 TO REP-HEX (1).
016520	     MOVE "4E"		 TO REP-HEX (2).
016530	     MOVE "8E"		 TO REP-HEX (3).
016540	     MOVE "CE"		 TO REP-HEX (4).
016550	     MOVE "SO"		 TO REP-GRAPH (1).
016560	     MOVE "N"		 TO REP-ALPH (2).
015580	     MOVE X"8E" 	 TO REP-ALPH (3).
015580	     MOVE X"CE" 	 TO REP-ALPH (4).
016570       PERFORM AJ05.
016580	     MOVE "0F"		 TO REP-HEX (1).
016590	     MOVE "4F"		 TO REP-HEX (2).
016600	     MOVE "8F"		 TO REP-HEX (3).
016610	     MOVE "CF"		 TO REP-HEX (4).
016620	     MOVE "SI"		 TO REP-GRAPH (1).
016630	     MOVE "O"		 TO REP-ALPH (2).
015580	     MOVE X"8F" 	 TO REP-ALPH (3).
015580	     MOVE X"CF" 	 TO REP-ALPH (4).
016640       PERFORM AJ05.
016650	     MOVE "10"		 TO REP-HEX (1).
016660	     MOVE "50"		 TO REP-HEX (2).
016670	     MOVE "90"		 TO REP-HEX (3).
016680	     MOVE "D0"		 TO REP-HEX (4).
016690	     MOVE "DLE" 	 TO REP-GRAPH (1).
016700	     MOVE "P"		 TO REP-ALPH (2).
015580	     MOVE X"90" 	 TO REP-ALPH (3).
015580	     MOVE X"D0" 	 TO REP-ALPH (4).
016710       PERFORM AJ05.
016720	     MOVE "11"		 TO REP-HEX (1).
016730	     MOVE "51"		 TO REP-HEX (2).
016740	     MOVE "91"		 TO REP-HEX (3).
016750	     MOVE "D1"		 TO REP-HEX (4).
016760	     MOVE "DC1" 	 TO REP-GRAPH (1).
016770	     MOVE "Q"		 TO REP-ALPH (2).
015580	     MOVE X"91" 	 TO REP-ALPH (3).
015580	     MOVE X"D1" 	 TO REP-ALPH (4).
016780       PERFORM AJ05.
016790	     MOVE "12"		 TO REP-HEX (1).
016800	     MOVE "52"		 TO REP-HEX (2).
016810	     MOVE "92"		 TO REP-HEX (3).
016820	     MOVE "D2"		 TO REP-HEX (4).
016830	     MOVE "DC2" 	 TO REP-GRAPH (1).
016840	     MOVE "R"		 TO REP-ALPH (2).
015580	     MOVE X"92" 	 TO REP-ALPH (3).
015580	     MOVE X"D2" 	 TO REP-ALPH (4).
016850       PERFORM AJ05.
016860	     MOVE "13"		 TO REP-HEX (1).
016870	     MOVE "53"		 TO REP-HEX (2).
016880	     MOVE "93"		 TO REP-HEX (3).
016890	     MOVE "D3"		 TO REP-HEX (4).
016900	     MOVE "DC3" 	 TO REP-GRAPH (1).
016910	     MOVE "S"		 TO REP-ALPH (2).
015580	     MOVE X"93" 	 TO REP-ALPH (3).
015580	     MOVE X"D3" 	 TO REP-ALPH (4).
016920       PERFORM AJ05.
016930	     MOVE "14"		 TO REP-HEX (1).
016940	     MOVE "54"		 TO REP-HEX (2).
016950	     MOVE "94"		 TO REP-HEX (3).
016960	     MOVE "D4"		 TO REP-HEX (4).
016970	     MOVE "DC4" 	 TO REP-GRAPH (1).
016980	     MOVE "T"		 TO REP-ALPH (2).
015580	     MOVE X"94" 	 TO REP-ALPH (3).
015580	     MOVE X"D4" 	 TO REP-ALPH (4).
016990       PERFORM AJ05.
017000	     MOVE "15"		 TO REP-HEX (1).
017010	     MOVE "55"		 TO REP-HEX (2).
017020	     MOVE "95"		 TO REP-HEX (3).
017030	     MOVE "D5"		 TO REP-HEX (4).
017040	     MOVE "NAK" 	 TO REP-GRAPH (1).
017050	     MOVE "U"		 TO REP-ALPH (2).
015580	     MOVE X"95" 	 TO REP-ALPH (3).
015580	     MOVE X"D5" 	 TO REP-ALPH (4).
017060       PERFORM AJ05.
017070	     MOVE "16"		 TO REP-HEX (1).
017080	     MOVE "56"		 TO REP-HEX (2).
017090	     MOVE "96"		 TO REP-HEX (3).
017100	     MOVE "D6"		 TO REP-HEX (4).
017110	     MOVE "SYN" 	 TO REP-GRAPH (1).
017120	     MOVE "V"		 TO REP-ALPH (2).
015580	     MOVE X"96" 	 TO REP-ALPH (3).
015580	     MOVE X"D6" 	 TO REP-ALPH (4).
017130       PERFORM AJ05.
017140	     MOVE "17"		 TO REP-HEX (1).
017150	     MOVE "57"		 TO REP-HEX (2).
017160	     MOVE "97"		 TO REP-HEX (3).
017170	     MOVE "D7"		 TO REP-HEX (4).
017180	     MOVE "ETB" 	 TO REP-GRAPH (1).
017190	     MOVE "W"		 TO REP-ALPH (2).
015580	     MOVE X"97" 	 TO REP-ALPH (3).
015580	     MOVE X"D7" 	 TO REP-ALPH (4).
017200       PERFORM AJ05.
017210	     MOVE "18"		 TO REP-HEX (1).
017220	     MOVE "58"		 TO REP-HEX (2).
017230	     MOVE "98"		 TO REP-HEX (3).
017240	     MOVE "D8"		 TO REP-HEX (4).
017250	     MOVE "CAN" 	 TO REP-GRAPH (1).
017260	     MOVE "X"		 TO REP-ALPH (2).
015580	     MOVE X"98" 	 TO REP-ALPH (3).
015580	     MOVE X"D8" 	 TO REP-ALPH (4).
017270       PERFORM AJ05.
017280	     MOVE "19"		 TO REP-HEX (1).
017290	     MOVE "59"		 TO REP-HEX (2).
017300	     MOVE "99"		 TO REP-HEX (3).
017310	     MOVE "D9"		 TO REP-HEX (4).
017320	     MOVE "EM"		 TO REP-GRAPH (1).
017330	     MOVE "Y"		 TO REP-ALPH (2).
015580	     MOVE X"99" 	 TO REP-ALPH (3).
015580	     MOVE X"D9" 	 TO REP-ALPH (4).
017340       PERFORM AJ05.
017350	     MOVE "1A"		 TO REP-HEX (1).
017360	     MOVE "5A"		 TO REP-HEX (2).
017370	     MOVE "9A"		 TO REP-HEX (3).
017380	     MOVE "DA"		 TO REP-HEX (4).
017390	     MOVE "SUB" 	 TO REP-GRAPH (1).
017400	     MOVE "Z"		 TO REP-ALPH (2).
015580	     MOVE X"9A" 	 TO REP-ALPH (3).
015580	     MOVE X"DA" 	 TO REP-ALPH (4).
017410       PERFORM AJ05.
017420	     MOVE "1B"		 TO REP-HEX (1).
017430	     MOVE "5B"		 TO REP-HEX (2).
017440	     MOVE "9B"		 TO REP-HEX (3).
017450	     MOVE "DB"		 TO REP-HEX (4).
017460	     MOVE "ESC" 	 TO REP-GRAPH (1).
017470	     MOVE "["		 TO REP-ALPH (2).
015580	     MOVE X"9B" 	 TO REP-ALPH (3).
015580	     MOVE X"DB" 	 TO REP-ALPH (4).
017480       PERFORM AJ05.
017490	     MOVE "1C"		 TO REP-HEX (1).
017500	     MOVE "5C"		 TO REP-HEX (2).
017510	     MOVE "9C"		 TO REP-HEX (3).
017520	     MOVE "DC"		 TO REP-HEX (4).
017530	     MOVE "FS"		 TO REP-GRAPH (1).
017540	     MOVE "\"            TO REP-ALPH (2).
015580	     MOVE X"9C" 	 TO REP-ALPH (3).
015580	     MOVE X"DC" 	 TO REP-ALPH (4).
017550       PERFORM AJ05.
017560	     MOVE "1D"		 TO REP-HEX (1).
017570	     MOVE "5D"		 TO REP-HEX (2).
017580	     MOVE "9D"		 TO REP-HEX (3).
017590	     MOVE "DD"		 TO REP-HEX (4).
017600	     MOVE "GS"		 TO REP-GRAPH (1).
017610	     MOVE "]"		 TO REP-ALPH (2).
015580	     MOVE X"9D" 	 TO REP-ALPH (3).
015580	     MOVE X"DD" 	 TO REP-ALPH (4).
017620       PERFORM AJ05.
017630	     MOVE "1E"		 TO REP-HEX (1).
017640	     MOVE "5E"		 TO REP-HEX (2).
017650	     MOVE "9E"		 TO REP-HEX (3).
017660	     MOVE "DE"		 TO REP-HEX (4).
017670	     MOVE "RS"		 TO REP-GRAPH (1).
017680	     MOVE "^"		 TO REP-ALPH (2).
015580	     MOVE X"9E" 	 TO REP-ALPH (3).
015580	     MOVE X"DE" 	 TO REP-ALPH (4).
017690       PERFORM AJ05.
017700	     MOVE "1F"		 TO REP-HEX (1).
017710	     MOVE "5F"		 TO REP-HEX (2).
017720	     MOVE "9F"		 TO REP-HEX (3).
017730	     MOVE "DF"		 TO REP-HEX (4).
017740	     MOVE "US"		 TO REP-GRAPH (1).
017750	     MOVE "<"		 TO REP-ALPH (2).
015580	     MOVE X"9F" 	 TO REP-ALPH (3).
015580	     MOVE X"DF" 	 TO REP-ALPH (4).
017760       PERFORM AJ05.
017770	     MOVE "20"		 TO REP-HEX (1).
017780	     MOVE "60"		 TO REP-HEX (2).
017790	     MOVE "A0"		 TO REP-HEX (3).
017800	     MOVE "E0"		 TO REP-HEX (4).
017810	     MOVE "SPACE"	 TO REP-GRAPH (1).
017820	     MOVE "'"		 TO REP-ALPH (2).
015580	     MOVE X"A0" 	 TO REP-ALPH (3).
015580	     MOVE X"E0" 	 TO REP-ALPH (4).
017830       PERFORM AJ05.
017840	     MOVE "21"		 TO REP-HEX (1).
017850	     MOVE "61"		 TO REP-HEX (2).
017860	     MOVE "A1"		 TO REP-HEX (3).
017870	     MOVE "E1"		 TO REP-HEX (4).
017880	     MOVE "!"		 TO REP-ALPH (1).
017890	     MOVE "a"		 TO REP-ALPH (2).
015580	     MOVE X"A1" 	 TO REP-ALPH (3).
015580	     MOVE X"E1" 	 TO REP-ALPH (4).
017900       PERFORM AJ05.
017910	     MOVE "22"		 TO REP-HEX (1).
017920	     MOVE "62"		 TO REP-HEX (2).
017930	     MOVE "A2"		 TO REP-HEX (3).
017940	     MOVE "E2"		 TO REP-HEX (4).
017950	     MOVE """"		 TO REP-ALPH (1).
017960	     MOVE "b"		 TO REP-ALPH (2).
015580	     MOVE X"A2" 	 TO REP-ALPH (3).
015580	     MOVE X"E2" 	 TO REP-ALPH (4).
017970       PERFORM AJ05.
017980	     MOVE "23"		 TO REP-HEX (1).
017990	     MOVE "63"		 TO REP-HEX (2).
018000	     MOVE "A3"		 TO REP-HEX (3).
018010	     MOVE "E3"		 TO REP-HEX (4).
018020	     MOVE "#"		 TO REP-ALPH (1).
018030	     MOVE "c"		 TO REP-ALPH (2).
015580	     MOVE X"A3" 	 TO REP-ALPH (3).
015580	     MOVE X"E3" 	 TO REP-ALPH (4).
018040       PERFORM AJ05.
018050	     MOVE "24"		 TO REP-HEX (1).
018060	     MOVE "64"		 TO REP-HEX (2).
018070	     MOVE "A4"		 TO REP-HEX (3).
018080	     MOVE "E4"		 TO REP-HEX (4).
018090	     MOVE "$"		 TO REP-ALPH (1).
018100	     MOVE "d"		 TO REP-ALPH (2).
015580	     MOVE X"A4" 	 TO REP-ALPH (3).
015580	     MOVE X"E4" 	 TO REP-ALPH (4).
018110       PERFORM AJ05.
018120	     MOVE "25"		 TO REP-HEX (1).
018130	     MOVE "65"		 TO REP-HEX (2).
018140	     MOVE "A5"		 TO REP-HEX (3).
018150	     MOVE "E5"		 TO REP-HEX (4).
018160	     MOVE "%"		 TO REP-ALPH (1).
018170	     MOVE "e"		 TO REP-ALPH (2).
015580	     MOVE X"A5" 	 TO REP-ALPH (3).
015580	     MOVE X"E5" 	 TO REP-ALPH (4).
018180       PERFORM AJ05.
018190	     MOVE "26"		 TO REP-HEX (1).
018200	     MOVE "66"		 TO REP-HEX (2).
018210	     MOVE "A6"		 TO REP-HEX (3).
018220	     MOVE "E6"		 TO REP-HEX (4).
018230	     MOVE "&"		 TO REP-ALPH (1).
018240	     MOVE "f"		 TO REP-ALPH (2).
015580	     MOVE X"A6" 	 TO REP-ALPH (3).
015580	     MOVE X"E6" 	 TO REP-ALPH (4).
018250       PERFORM AJ05.
018260	     MOVE "27"		 TO REP-HEX (1).
018270	     MOVE "67"		 TO REP-HEX (2).
018280	     MOVE "A7"		 TO REP-HEX (3).
018290	     MOVE "E7"		 TO REP-HEX (4).
018300	     MOVE "'"		 TO REP-ALPH (1).
018310	     MOVE "g"		 TO REP-ALPH (2).
015580	     MOVE X"A7" 	 TO REP-ALPH (3).
015580	     MOVE X"E7" 	 TO REP-ALPH (4).
018320       PERFORM AJ05.
018330	     MOVE "28"		 TO REP-HEX (1).
018340	     MOVE "68"		 TO REP-HEX (2).
018350	     MOVE "A8"		 TO REP-HEX (3).
018360	     MOVE "E8"		 TO REP-HEX (4).
018370	     MOVE "("		 TO REP-ALPH (1).
018380	     MOVE "h"		 TO REP-ALPH (2).
015580	     MOVE X"A8" 	 TO REP-ALPH (3).
015580	     MOVE X"E8" 	 TO REP-ALPH (4).
018390       PERFORM AJ05.
018400	     MOVE "29"		 TO REP-HEX (1).
018410	     MOVE "69"		 TO REP-HEX (2).
018420	     MOVE "A9"		 TO REP-HEX (3).
018430	     MOVE "E9"		 TO REP-HEX (4).
018440	     MOVE ")"		 TO REP-ALPH (1).
018450	     MOVE "i"		 TO REP-ALPH (2).
015580	     MOVE X"A9" 	 TO REP-ALPH (3).
015580	     MOVE X"E9" 	 TO REP-ALPH (4).
018460       PERFORM AJ05.
018470	     MOVE "2A"		 TO REP-HEX (1).
018480	     MOVE "6A"		 TO REP-HEX (2).
018490	     MOVE "AA"		 TO REP-HEX (3).
018500	     MOVE "EA"		 TO REP-HEX (4).
018510	     MOVE "*"		 TO REP-ALPH (1).
018520	     MOVE "j"		 TO REP-ALPH (2).
015580	     MOVE X"AA" 	 TO REP-ALPH (3).
015580	     MOVE X"EA" 	 TO REP-ALPH (4).
018530       PERFORM AJ05.
018540	     MOVE "2B"		 TO REP-HEX (1).
018550	     MOVE "6B"		 TO REP-HEX (2).
018560	     MOVE "AB"		 TO REP-HEX (3).
018570	     MOVE "EB"		 TO REP-HEX (4).
018580	     MOVE "+"		 TO REP-ALPH (1).
018590	     MOVE "k"		 TO REP-ALPH (2).
015580	     MOVE X"AB" 	 TO REP-ALPH (3).
015580	     MOVE X"EB" 	 TO REP-ALPH (4).
018600       PERFORM AJ05.
018610	     MOVE "2C"		 TO REP-HEX (1).
018620	     MOVE "6C"		 TO REP-HEX (2).
018630	     MOVE "AC"		 TO REP-HEX (3).
018640	     MOVE "EC"		 TO REP-HEX (4).
018650	     MOVE ","		 TO REP-ALPH (1).
018660	     MOVE "l"		 TO REP-ALPH (2).
015580	     MOVE X"AC" 	 TO REP-ALPH (3).
015580	     MOVE X"EC" 	 TO REP-ALPH (4).
018670       PERFORM AJ05.
018680	     MOVE "2D"		 TO REP-HEX (1).
018690	     MOVE "6D"		 TO REP-HEX (2).
018700	     MOVE "AD"		 TO REP-HEX (3).
018710	     MOVE "ED"		 TO REP-HEX (4).
018720	     MOVE "-"		 TO REP-ALPH (1).
018730	     MOVE "m"		 TO REP-ALPH (2).
015580	     MOVE X"AD" 	 TO REP-ALPH (3).
015580	     MOVE X"ED" 	 TO REP-ALPH (4).
018740       PERFORM AJ05.
018750	     MOVE "2E"		 TO REP-HEX (1).
018760	     MOVE "6E"		 TO REP-HEX (2).
018770	     MOVE "AE"		 TO REP-HEX (3).
018780	     MOVE "EE"		 TO REP-HEX (4).
018790	     MOVE "."		 TO REP-ALPH (1).
018800	     MOVE "n"		 TO REP-ALPH (2).
015580	     MOVE X"AE" 	 TO REP-ALPH (3).
015580	     MOVE X"EE" 	 TO REP-ALPH (4).
018810       PERFORM AJ05.
018820	     MOVE "2F"		 TO REP-HEX (1).
018830	     MOVE "6F"		 TO REP-HEX (2).
018840	     MOVE "AF"		 TO REP-HEX (3).
018850	     MOVE "EF"		 TO REP-HEX (4).
018860	     MOVE "/"		 TO REP-ALPH (1).
018870	     MOVE "o"		 TO REP-ALPH (2).
015580	     MOVE X"AF" 	 TO REP-ALPH (3).
015580	     MOVE X"EF" 	 TO REP-ALPH (4).
018880       PERFORM AJ05.
018890	     MOVE "30"		 TO REP-HEX (1).
018900	     MOVE "70"		 TO REP-HEX (2).
018910	     MOVE "B0"		 TO REP-HEX (3).
018920	     MOVE "F0"		 TO REP-HEX (4).
018930	     MOVE "0"		 TO REP-ALPH (1).
018940	     MOVE "p"		 TO REP-ALPH (2).
015580	     MOVE X"B0" 	 TO REP-ALPH (3).
015580	     MOVE X"F0" 	 TO REP-ALPH (4).
018950       PERFORM AJ05.
018960	     MOVE "31"		 TO REP-HEX (1).
018970	     MOVE "71"		 TO REP-HEX (2).
018980	     MOVE "B1"		 TO REP-HEX (3).
018990	     MOVE "F1"		 TO REP-HEX (4).
019000	     MOVE "1"		 TO REP-ALPH (1).
019010	     MOVE "q"		 TO REP-ALPH (2).
015580	     MOVE X"B1" 	 TO REP-ALPH (3).
015580	     MOVE X"F1" 	 TO REP-ALPH (4).
019020       PERFORM AJ05.
019030	     MOVE "32"		 TO REP-HEX (1).
019040	     MOVE "72"		 TO REP-HEX (2).
019050	     MOVE "B2"		 TO REP-HEX (3).
019060	     MOVE "F2"		 TO REP-HEX (4).
019070	     MOVE "2"		 TO REP-ALPH (1).
019080	     MOVE "r"		 TO REP-ALPH (2).
015580	     MOVE X"B2" 	 TO REP-ALPH (3).
015580	     MOVE X"F2" 	 TO REP-ALPH (4).
019090       PERFORM AJ05.
019100	     MOVE "33"		 TO REP-HEX (1).
019110	     MOVE "73"		 TO REP-HEX (2).
019120	     MOVE "B3"		 TO REP-HEX (3).
019130	     MOVE "F3"		 TO REP-HEX (4).
019140	     MOVE "3"		 TO REP-ALPH (1).
019150	     MOVE "s"		 TO REP-ALPH (2).
015580	     MOVE X"B3" 	 TO REP-ALPH (3).
015580	     MOVE X"F3" 	 TO REP-ALPH (4).
019160       PERFORM AJ05.
019170	     MOVE "34"		 TO REP-HEX (1).
019180	     MOVE "74"		 TO REP-HEX (2).
019190	     MOVE "B4"		 TO REP-HEX (3).
019200	     MOVE "F4"		 TO REP-HEX (4).
019210	     MOVE "4"		 TO REP-ALPH (1).
019220	     MOVE "t"		 TO REP-ALPH (2).
015580	     MOVE X"B4" 	 TO REP-ALPH (3).
015580	     MOVE X"F4" 	 TO REP-ALPH (4).
019230       PERFORM AJ05.
019240	     MOVE "35"		 TO REP-HEX (1).
019250	     MOVE "75"		 TO REP-HEX (2).
019260	     MOVE "B5"		 TO REP-HEX (3).
019270	     MOVE "F5"		 TO REP-HEX (4).
019280	     MOVE "5"		 TO REP-ALPH (1).
019290	     MOVE "u"		 TO REP-ALPH (2).
015580	     MOVE X"B5" 	 TO REP-ALPH (3).
015580	     MOVE X"F5" 	 TO REP-ALPH (4).
019300       PERFORM AJ05.
019310	     MOVE "36"		 TO REP-HEX (1).
019320	     MOVE "76"		 TO REP-HEX (2).
019330	     MOVE "B6"		 TO REP-HEX (3).
019340	     MOVE "F6"		 TO REP-HEX (4).
019350	     MOVE "6"		 TO REP-ALPH (1).
019360	     MOVE "v"		 TO REP-ALPH (2).
015580	     MOVE X"B6" 	 TO REP-ALPH (3).
015580	     MOVE X"F6" 	 TO REP-ALPH (4).
019370       PERFORM AJ05.
019380	     MOVE "37"		 TO REP-HEX (1).
019390	     MOVE "77"		 TO REP-HEX (2).
019400	     MOVE "B7"		 TO REP-HEX (3).
019410	     MOVE "F7"		 TO REP-HEX (4).
019420	     MOVE "7"		 TO REP-ALPH (1).
019430	     MOVE "w"		 TO REP-ALPH (2).
015580	     MOVE X"B7" 	 TO REP-ALPH (3).
015580	     MOVE X"F7" 	 TO REP-ALPH (4).
019440       PERFORM AJ05.
019450	     MOVE "38"		 TO REP-HEX (1).
019460	     MOVE "78"		 TO REP-HEX (2).
019470	     MOVE "B8"		 TO REP-HEX (3).
019480	     MOVE "F8"		 TO REP-HEX (4).
019490	     MOVE "8"		 TO REP-ALPH (1).
019500	     MOVE "x"		 TO REP-ALPH (2).
015580	     MOVE X"B8" 	 TO REP-ALPH (3).
015580	     MOVE X"F8" 	 TO REP-ALPH (4).
019510       PERFORM AJ05.
019520	     MOVE "39"		 TO REP-HEX (1).
019530	     MOVE "79"		 TO REP-HEX (2).
019540	     MOVE "B9"		 TO REP-HEX (3).
019550	     MOVE "F9"		 TO REP-HEX (4).
019560	     MOVE "9"		 TO REP-ALPH (1).
019570	     MOVE "y"		 TO REP-ALPH (2).
015580	     MOVE X"B9" 	 TO REP-ALPH (3).
015580	     MOVE X"F9" 	 TO REP-ALPH (4).
019580       PERFORM AJ05.
019590	     MOVE "3A"		 TO REP-HEX (1).
019600	     MOVE "7A"		 TO REP-HEX (2).
019610	     MOVE "BA"		 TO REP-HEX (3).
019620	     MOVE "FA"		 TO REP-HEX (4).
019630	     MOVE ":"		 TO REP-ALPH (1).
019640	     MOVE "z"		 TO REP-ALPH (2).
015580	     MOVE X"BA" 	 TO REP-ALPH (3).
015580	     MOVE X"FA" 	 TO REP-ALPH (4).
019650       PERFORM AJ05.
019660	     MOVE "3B"		 TO REP-HEX (1).
019670	     MOVE "7B"		 TO REP-HEX (2).
019680	     MOVE "BB"		 TO REP-HEX (3).
019690	     MOVE "FB"		 TO REP-HEX (4).
019700	     MOVE ";"		 TO REP-ALPH (1).
019710	     MOVE "{"		 TO REP-ALPH (2).
015580	     MOVE X"BB" 	 TO REP-ALPH (3).
015580	     MOVE X"FB" 	 TO REP-ALPH (4).
019720       PERFORM AJ05.
019730	     MOVE "3C"		 TO REP-HEX (1).
019740	     MOVE "7C"		 TO REP-HEX (2).
019750	     MOVE "BC"		 TO REP-HEX (3).
019760	     MOVE "FC"		 TO REP-HEX (4).
019770	     MOVE "<"		 TO REP-ALPH (1).
019780	     MOVE "|"		 TO REP-ALPH (2).
015580	     MOVE X"BC" 	 TO REP-ALPH (3).
015580	     MOVE X"FC" 	 TO REP-ALPH (4).
019790       PERFORM AJ05.
019800	     MOVE "3D"		 TO REP-HEX (1).
019810	     MOVE "7D"		 TO REP-HEX (2).
019820	     MOVE "BD"		 TO REP-HEX (3).
019830	     MOVE "FD"		 TO REP-HEX (4).
019840	     MOVE "="		 TO REP-ALPH (1).
019850	     MOVE "}"		 TO REP-ALPH (2).
015580	     MOVE X"BD" 	 TO REP-ALPH (3).
015580	     MOVE X"FD" 	 TO REP-ALPH (4).
019860       PERFORM AJ05.
019870	     MOVE "3E"		 TO REP-HEX (1).
019880	     MOVE "7E"		 TO REP-HEX (2).
019890	     MOVE "BE"		 TO REP-HEX (3).
019900	     MOVE "FE"		 TO REP-HEX (4).
019910	     MOVE ">"		 TO REP-ALPH (1).
019920	     MOVE "~"		 TO REP-ALPH (2).
015580	     MOVE X"BE" 	 TO REP-ALPH (3).
015580	     MOVE X"FE" 	 TO REP-ALPH (4).
019930       PERFORM AJ05.
019940	     MOVE "3F"		 TO REP-HEX (1).
019950	     MOVE "7F"		 TO REP-HEX (2).
019960	     MOVE "BF"		 TO REP-HEX (3).
019970	     MOVE "FF"		 TO REP-HEX (4).
019980	     MOVE "?"		 TO REP-ALPH (1).
019990	     MOVE "DEL" 	 TO REP-GRAPH (2).
015580	     MOVE X"BF" 	 TO REP-ALPH (3).
015580	     MOVE X"FF" 	 TO REP-ALPH (4).
020000       PERFORM AJ05.
020010	     MOVE SPACES	 TO REP-DETAIL.
020020       WRITE REP-L1 BEFORE PAGE.
022120       CLOSE PRNREP.

020030 AJ999.
020040	     EXIT PROGRAM.
