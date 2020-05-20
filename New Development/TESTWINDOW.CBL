      $set preprocess"window1"
        working-storage section.
        78 note-height  value 16.
        78 note-width  value 41.
        78 no-of-chars  value note-height * note-width.
        01 note-window  pic x(10).
        01 dummy    pic x.
           88 d1 values "A" "C" "E".
           88 D2 VALUES "B" "D" "F".
        01 note-data value all "- wallpaper ".
          03 note-char pic x occurs no-of-chars.
        screen section.
        01 input-data highlight.
          03 line 4 column 6 value " Accept and Display positions ".
          03 line 5 column 6 value " are relative to the top left ".
          03 line 6 column 6 value " corner of the window. ".
          03    pic x using dummy.
        01 note-screen  pic x(no-of-chars)
                      using note-data prompt " " reverse-video.
        procedure division.
      * Put a blank window on the screen with a border and title
             DISPLAY "TESTING DATA UNDER WINDOW" AT 0442
             display window, line 2, column 38, lines note-height,
                 size note-width, boxed, erase, reverse
      * Define a reference for this window so that it can be
      * removed and the previous display restored
                 pop-up area is note-window
                 bottom right title "Press Enter to remove window"
      * Fill the window with the contents of note-screen
             display note-screen
             display input-data
             accept input-data
             close window note-window.
           IF D1
               DISPLAY "1" AT 0725
           ELSE
           IF D2
               DISPLAY "2" AT 0725.
             STOP RUN
