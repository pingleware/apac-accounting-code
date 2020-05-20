      *
      *    T E R M I N A T O  R   K E Y S
      *
       78 NORM-TERM	     VALUE "0".
       78 USER-FN-KEY	     VALUE "1".
       78 ADIS-FN-KEY	     VALUE "2".
       78 8BIT-DATA	     VALUE "3".
       78 16BIT-DATA	     VALUE "4".
       78 ERROR-TERM	     VALUE "9".
      *
      *    A D I S   F U N C T I O N   K E Y S
      *
       78  ENTER-KEY	    VALUE 0.
       78  CR-KEY	          VALUE 2.
       78  LEFT-KEY	    VALUE 3.
       78  RIGHT-KEY	    VALUE 4.
       78  UP-KEY	          VALUE 5.
       78  DOWN-KEY	    VALUE 6.
       78  HOME-KEY	    VALUE 7.
       78  TAB-KEY	    VALUE 8.
       78  BACKTAB-KEY	    VALUE 9.
       78  END-KEY	    VALUE 10.
       78  BACK-SPACE	    VALUE 14.
       78  DEL-KEY	    VALUE 17.
       78  INS-KEY	    VALUE 23.
       78  MOUSE-KEY	    VALUE 27.
      *
      *    U S E R   F U N C T I O N   K E Y S
      *
       78  ESC-KEY	    VALUE 0.
       78  F1-KEY	          VALUE 1.
       78  F2-KEY	          VALUE 2.
       78  F3-KEY	          VALUE 3.
       78  F4-KEY	          VALUE 4.
       78  F5-KEY	          VALUE 5.
       78  F6-KEY	          VALUE 6.
       78  F7-KEY	          VALUE 7.
       78  F8-KEY	          VALUE 8.
       78  F9-KEY	          VALUE 9.
       78  F10-KEY	    VALUE 10.
       78  SF1-KEY	    VALUE 11.
       78  SF2-KEY	    VALUE 12.
       78  SF3-KEY	    VALUE 13.
       78  SF4-KEY	    VALUE 14.
       78  SF5-KEY	    VALUE 15.
       78  SF6-KEY	    VALUE 16.
       78  SF7-KEY	    VALUE 17.
       78  SF8-KEY	    VALUE 18.
       78  SF9-KEY	    VALUE 19.
       78  SF10-KEY	    VALUE 20.
       78  CF1-KEY	    VALUE 21.
       78  CF2-KEY	    VALUE 22.
       78  CF3-KEY	    VALUE 23.
       78  CF4-KEY	    VALUE 24.
       78  CF5-KEY	    VALUE 25.
       78  CF6-KEY	    VALUE 26.
       78  CF7-KEY	    VALUE 27.
       78  CF8-KEY	    VALUE 28.
       78  CF9-KEY	    VALUE 29.
       78  CF10-KEY	    VALUE 30.
       78  AF1-KEY	    VALUE 31.
       78  AF2-KEY	    VALUE 32.
       78  AF3-KEY	    VALUE 33.
       78  AF4-KEY	    VALUE 34.
       78  AF5-KEY	    VALUE 35.
       78  AF6-KEY	    VALUE 36.
       78  AF7-KEY	    VALUE 37.
       78  AF8-KEY	    VALUE 38.
       78  AF9-KEY	    VALUE 39.
       78  AF10-KEY	    VALUE 40.
       78  ALT-1	          VALUE 41.
       78  ALT-2      	    VALUE 42.
       78  ALT-3	          VALUE 43.
       78  ALT-4	          VALUE 44.
       78  ALT-5      	    VALUE 45.
       78  ALT-6	          VALUE 46.
       78  ALT-7      	    VALUE 47.
       78  ALT-8	          VALUE 48.
       78  ALT-9      	    VALUE 49.
       78  ALT-0	          VALUE 50.
       78  ALT-HYPH	    VALUE 51.
       78  ALT-EQUAL	    VALUE 52.
       78  PAGE-UP	    VALUE 53.
       78  PAGE-DOWN	    VALUE 54.
       78  ALT-A      	    VALUE 65.
       78  ALT-B	          VALUE 66.
       78  ALT-C      	    VALUE 67.
       78  ALT-D	          VALUE 68.
       78  ALT-E	          VALUE 69.
       78  ALT-F      	    VALUE 70.
       78  ALT-G	          VALUE 71.
       78  ALT-H	          VALUE 72.
       78  ALT-I      	    VALUE 73.
       78  ALT-J	          VALUE 74.
       78  ALT-K      	    VALUE 75.
       78  ALT-L	          VALUE 76.
       78  ALT-M      	    VALUE 77.
       78  ALT-N	          VALUE 78.
       78  ALT-O      	    VALUE 79.
       78  ALT-P	          VALUE 80.
       78  ALT-Q      	    VALUE 81.
       78  ALT-R	          VALUE 82.
       78  ALT-S      	    VALUE 83.
       78  ALT-T      	    VALUE 84.
       78  ALT-U	          VALUE 85.
       78  ALT-V      	    VALUE 86.
       78  ALT-W	          VALUE 87.
       78  ALT-X      	    VALUE 88.
       78  ALT-Y	          VALUE 89.
       78  ALT-Z	          VALUE 90.
       78  F11-KEY	    VALUE 91.
       78  F12-KEY	    VALUE 92.
       78  SF11-KEY	    VALUE 93.
       78  SF12-KEY	    VALUE 94.
       78  CF11-KEY	    VALUE 95.
       78  CF12-KEY	    VALUE 96.
       78  AF11-KEY	    VALUE 97.
       78  AF12-KEY	    VALUE 98.

       01  SET-BIT-PAIRS       PIC  9(02)    COMP-X VALUE 1.

       01  GET-SINGLE-CHAR     PIC  9(02)    COMP-X VALUE 26.

      *
      *    ACTION :  1 = CONTROL USER FUNCTION KEYS
      *
      * 		 SETTING 0 = DISABLE
      * 			 1 = ENABLE
      * 		 NUMBER    = NUMBER OF FIRST USER KEY
      * 		 KEYS	   = NUMBER OF CONSECUTIVE KEYS
      *
      * 		 SETTING 1 = STANDARD USER FUNCTION KEY LIST
      * 			 2 = COMPATIBILITY KEY LIST
      * 		 NUMBER    = 87
      * 		 KEYS	   = 1
      *
      * 	     2 = CONTROL ADIS KEY MAPPINGS
      *
      * 		 SETTING 0 = DISABLE KEYS
      * 			 1 = ENABLE
      * 			 2 = NORMAL ACTION
      * 			 3 = NORMAL ACTION UNLESS CURSOR
      * 			     LEAVES CURRENT FIELD
      * 		 NUMBER    = NUMBER OF FIRST ADIS KEY
      * 		 KEYS	   = NUMBER OF CONSECUTIVE KEYS
      *
      * 		 SETTING 0 = PRE-DISPLAY FIXED-FORMAT
      * 			     NUMERIC/NUM EDITED
      * 			 1 = NUMERIC AND FIXED-FORMAT
      * 			     NUM EDITED FIELDS PRE-DISPLAYED
      * 			 2 = PRE-DISPLAY FIELD WHEN CURSOR
      * 			     MOVED TO IT
      * 			 3 = ALL FIELDS IN ACCEPT ARE
      * 			     PRE-DISPLAYED.
      * 		 NUMBER    = 76
      * 		 KEYS	   = 1
      *
      * 		 SETTING 0 = INDICATOR DISPLAYED IF NECCESSARY
      * 			 3 = INDICATOR IS NEVER DISPLAYED
      * 		 NUMBER    = 56 INSERT/REPLACE IND
      * 			   = 57 END OF FIELD IND
      * 			   = 58 AUTOCLEAR IND
      * 		 KEYS	   = 1
      *
      * 		 SETTING 0 = ERROR MESSAGES NEVER DISPLAYED
      * 			     BELL IS RUNG
      * 			 1 = MESSAGES NEVER DISPLAYED
      * 			     BELL IS RUNG FOR INVALID NUMERIC
      * 			 2 = MESSAGES DISPLAYED
      * 			     INVALID NUMERIC NOT REPORTED
      * 			 3 = MESSAGES DISPLAYED
      * 			     BELL RUNG FOR INVALID NUMERIC
      * 		 NUMBER    = 44
      * 		 KEYS	   = 1
      *
       01  USER-KEY-CONTROL.
	   03  USER-SETTING    PIC  9(02)    COMP-X.
	   03  USER-ACTION     PIC  X(01)    VALUE "2".
	   03  USER-NUMBER     PIC  9(02)    COMP-X.
	   03  USER-KEYS       PIC  9(02)    COMP-X VALUE 1.
