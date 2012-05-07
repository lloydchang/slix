'$DYNAMIC

'slix0987.BAS

'.--------------------------------------------------------------.
'|                         .------------.                       |
'|                         | slix 0.987 |                       |
'|                         `------------'                       |
'|                                                              |
'|                  sprite library for mode x                   |
'|                  (and other tweaked modes)                   |
'|              Full QuickBASIC source code included            |
'|                                                              |
'|                     Written by Lloyd Chang                   |
'|                         August 10, 1996                      |
'|                                                              |
'|                  FREEWARE, NOT PUBLIC DOMAIN!                |
'|                                                              |
'`--------------------------------------------------------------'

'   #########################################################
'   # DISCLAIMER: USE slix AT YOUR OWN RISK!                #
'   #             The author is not liable for any problems #
'   #             that may result from the use of slix.     #
'   #                                                       #
'   #########################################################

'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'% slix is FREEWARE.  The program may be freely distributed    %
'% under the condition that the author be given credit for     %
'% slix.  Modifications are encouraged.                        %
'% Feel free to contact me if modifications are made to any    %
'% part of slix.                                               %
'%                                                             %
'% There are currently no other restrictions with regard to    %
'% the use of slix.                                            %
'%                                                             %
'%     **************************************************      %
'%     * PLEASE READ THE DISCLAIMER BEFORE YOU USE slix *      %
'%     **************************************************      %
'%                                                             %
'% I can be reached via:                                       %
'%                                                             %
'% Internet: lloyd.chang@tglbbs.com (!!The Game Line!!)        %
'%  Fidonet: Lloyd Chang [1:278/304] (BlueDog)                 %
'%                                                             %
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'----------------------------------------------------------------

'Some people to thank...(in alphabetical order)

'Michael Abrash -- For documenting Mode X in Dr. Dobb's Journal
'                  [mabrash@bix.com]
'                  [mabrash@mcimail.com]

'Phil Carlisle -- Mode X FAQ
'                 [pc@espr.demon.co.uk]

'Carl Gorringe -- For showing interest in slix

'Rich Geldreich -- Original GIF-displaying code

'Themie Gouthas -- XLIB
'                  [egg@dstos3.dsto.gov.au]
'                  [teg@bart.dsto.gov.au]

'Petri Hassinen -- Game Making Utilities
'                  [phassine@alpha.hut.fi]

'Petri Hodju -- Game Making Utilities

'Saku Jalkanen -- Game Making Utilities (GRAPHICS)

'Christopher G. Mann -- ASPHYXIA VGA trainer series
'(a.k.a. Snowman)       [r3cgm@dax.cc.uakron.edu]

'Robert Schmidt -- XINTRO & Tweak
'(a.k.a. Buuud)    [robert@stud.unit.no]

'Dave Shea -- For showing interest in slix

'Grant Smith      -- ASPHYXIA VGA trainer series
'(a.k.a. Denthor)    [smith9@batis.bis.und.ac.za]

'Nikolai Soumarokov -- GNOOM II

'Mike Valley -- Retrieving Mode X related info
'               Numerous programming tips

'[zabudsk@ecf.utoronto.ca] -- Original BMP-displaying code
'(Sorry, I should have contacted you for your real name)

'Others -- Sorry if your name was not listed...
'          Anyway, thanks!

'(I hope everyone's name was spelled correctly.)

'If anyone above wish to have their internet e-mail address(es)
'listed in this document, please contact me.

'Also, please contact me if I forgot to
'include someone's name in the above list.

'---------------------------------------------------------------

'slix contains two other FREEWARE packages.
'They are GNOOM II & Game Making Utilities.  GNOOM II may be used to
'create GN2 files.  Game Making Utilities is included as a token of thanks
'for the use of GMU images in slix.  I have not yet made any contacts
'with either authors of GNOOM II or GMU.  Hopefully, Nikolai Soumarokov
'and Petri Hassinen, respectively, will not be offended by the inclusion
'of their programs.

'----------------------------------------------------------------

'                 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
'                 $$                          $$
'                 $$ COMPILING SPEEDS UP slix $$
'                 $$                          $$
'                 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

'Sorry.  slix is currently not available as
'a QuickBASIC Library (QLB format).  However,
'the user should be able to create a QLB on
'with a QLB-creation utility.

'This program MAY NOT run under
'the QuickBASIC interpreter.

'Note: The /AH (AH must be in captial letters)
'      option may need to be invoked while compiling
'      and while loading up the QuickBASIC interpreter

'Note: The stacksize must be adjusted for this program
'      run correctly, or it could possibly CRASH the
'      computer.
'      (SEE "CLEAR , , 6144" BELOW)
 
'slix contains sub-routines that implement tweaked video modes
'(all video modes are unchained/planared with 256 colors)

'There are other modes, but they have not been added in yet.

'!!!USE AT YOUR OWN RISK!!!
'This program may !!!CRASH!!! under certain shells and
'certain operating systems.  !!!USE AT YOUR OWN RISK!!!

'Note: Some modes may not line up perfectly with certain monitors.
'      The user might have to adjust the size and position of the
'      screen with the monitor's control knobs.

'Note: Some modes may not work on certain monitors and certain VGA
'      cards.  Discoloration may also occur.

'Recommended: 100% VGA compatible card
'             SVGA compatible monitor

' MODE NAME    NUMBER OF PAGES
'256x200x256 = 5.12
'256x224x256 = 4.571428(571428...)
'256x240x256 = 4.26(6...)
'320x200x256 = 4.096
'256x256x256 = 4
'360x200x256 = 3.6408(8...)
'320x240x256 = 3.413(3...)
'360x240x256 = 3.03407(407...)
'360x270x256 = 2.696954732510288...
'376x282x256 = 2.472310245963483...
'256x400x256 = 2.56
'376x308x256 = 2.263608731693838...
'400x300x256 = 2.18453(3...)
'256x480x256 = 2.13(3...)
'320x400x256 = 2.048
'360x360x256 = 2.022716049382716...
'320x480x256 = 1.706(6...)
'360x400x256 = 1.8204(4...)
'360x480x256 = 1.51703(703...)
'376x564x256 = 1.236155122981741...
'400x600x256 = 1.09226(6...)
'
'(x...) = repeating x (only if x is in parentheses)

'Addresses: &H3D4   = base port of the CRT controller (color)
'           &H3C5   = base port of the sequencer
'           &H3CE   = base port of the graphics controller
'           &HA000& = segment of the VGA video memory
'&H = hexdecimal
'& (after &Hxxxx) = used for hexdecimal numbers larger
'                   than 7FFF

DECLARE FUNCTION BIN$ (Number&)
DECLARE FUNCTION BIND& (Number$)
DECLARE SUB ClearPage ()
DECLARE SUB COLOUR (DUMMY%)
DECLARE SUB DEMO (Mode$)
DECLARE SUB DrawFrame (SpriteNum%, FrameNum%, XCord%, YCord%)
DECLARE SUB FILEX (filename$, XCord%, YCord%, UseZero%, Center%)
DECLARE SUB FreeObject (ObjectNumber%)
DECLARE FUNCTION GETX% (XCord%, YCord%)
DECLARE SUB GPRINT (DUMMY$)
DECLARE FUNCTION inport$ (Addr&)
DECLARE SUB LoadBG (filename$, headersize%)
DECLARE SUB LoadCharSet ()
DECLARE SUB LoadSprites (filename$, SpriteNum%)
DECLARE SUB memset (Segment&, Addr&, BYTE%, Size&)
DECLARE SUB outport (Addr&, WORD&)
DECLARE SUB PageCopy (FromPage%, ToPage%)
DECLARE SUB PAGEFLIP ()
DECLARE SUB PSETX (XCord%, YCord%, PixelColor%)
DECLARE SUB PUTBG ()
DECLARE SUB PUTX (XCord%, YCord%, xsize%, ysize%, Buffer$, UseZero%)
DECLARE SUB ReadyFrame (Object%, Repeat%, Direction%, SX%, SY%, EX%, EY%)
DECLARE SUB RGBLoad ()
DECLARE SUB RGBSave ()
DECLARE FUNCTION SBR& (BYTE&, Shifter%)
DECLARE FUNCTION SBL& (BYTE&, Shifter%)
DECLARE SUB SetActivePage (PAGE%)
DECLARE SUB SetActiveStart (offset&)
DECLARE SUB SetObject (Object%, Repeat%, Direction%, SX%, SY%, EX%, EY%)
DECLARE SUB SetVisiblePage (PAGE%)
DECLARE SUB SetVisibleStart (offset&)
DECLARE SUB VGA (Mode$)
DECLARE SUB WaitRetrace ()
DECLARE SUB WARNING ()

'Designating global variables
COMMON SHARED slixVERSION%, slixDATE$
COMMON SHARED CursorX%, CursorY%, CurrentColour%
COMMON SHARED VGAWidth%, VGAHeight%, VGAWidthBytes%
COMMON SHARED ActivePage%, VisiblePage%, TotalPages%
COMMON SHARED ActiveStart&, VisibleStart&, ModeName$
COMMON SHARED AfterActiveEnd&, BGOn%
COMMON SHARED BGWidth%, BGHeight%, BGSize&
COMMON SHARED ReservedPage%, UseReservedPage%

DEFINT A-Z

'This sets the QuickBASIC stack size
'at 6144 bytes  It crashed at 4096.
CLEAR , , 6144

'Variable definitions
OPTION BASE 0
DIM SHARED CharSet(0 TO 255, 1 TO 5, 1 TO 5) AS INTEGER

'The following TYPE will use far memory, thus
'preventing "Out of string space" errors.

'However, errors may still occur.  The only solutions
'I can think of are 1) load the background from disk
'everytime, 2) store the background in the video ram
'(onto one of the pages), 3) use EMS functions (INT 67),
'4) allocate conventional memory (INT 21, 48)

'Option 1 is slow, but it is actually faster
'than loading the background into an TYPED array

'Option 2 is fast, but it wastes one video page

'Option 3 requires more coding

'Option 4 requires more coding as well, possibly
'using a CALL ABSOLUTE instead of linking the
'OBJ file in

TYPE Custom32K
  Each32K AS STRING * 32767
END TYPE

TYPE Custom4K
  Each4K AS STRING * 4096
END TYPE

'While Background0(0 TO 1), Background1(0 TO 1), Background2(0 TO 1), and
'Background3(0 TO 1), can each hold up to 65534 bytes (not exactly 64K)
'of data, they may not be used to their full capacity due to the way the
'PUTX sub-routine was designed.  Plus, a 256K picture will lack 8 bytes
'(since each Custom 32K is one byte off from 32768 bytes).
'
'Therefore, a slack space is needed.  Background4(0) is the slack
'space.  4K should be plenty, but if not, change it if necessary.
'
'A change in the size of Background4(0) may require changing of
'other parts of the program
'
'Giving these arrays one dimension saves some memory if the other
'dimension is not needed.  The program will redim the array if the
'another dimension is needed.

'While it is extremely slow, the program could use the following
'background arrays as a simple virtual screen.

BGOn% = 0
'By default, LoadBG & PUTBG are not supported

SELECT CASE BGOn%
  CASE IS = 1
    DIM SHARED Background0(0 TO 0) AS Custom32K
    DIM SHARED Background1(0 TO 0) AS Custom32K
    DIM SHARED Background2(0 TO 0) AS Custom32K
    DIM SHARED Background3(0 TO 0) AS Custom32K
    DIM SHARED Background4(0 TO 0) AS Custom4K
END SELECT

'Sprites should not exceed 4K
DIM SHARED Sprite0(0 TO 0) AS Custom4K
DIM SHARED Sprite1(0 TO 0) AS Custom4K
DIM SHARED Sprite2(0 TO 0) AS Custom4K
DIM SHARED Sprite3(0 TO 0) AS Custom4K
DIM SHARED Sprite4(0 TO 0) AS Custom4K
DIM SHARED Sprite5(0 TO 0) AS Custom4K
DIM SHARED Sprite6(0 TO 0) AS Custom4K
DIM SHARED Sprite7(0 TO 0) AS Custom4K
DIM SHARED SpriteWidth(0 TO 7) AS INTEGER
DIM SHARED SpriteHeight(0 TO 7) AS INTEGER
DIM SHARED SpriteLoaded(0 TO 7) AS INTEGER
DIM SHARED MaxSpriteFrame(0 TO 7) AS INTEGER
DIM SHARED Object(0 TO 15) AS INTEGER
DIM SHARED ObjectLock(0 TO 15) AS INTEGER
DIM SHARED ObjectSX(0 TO 15) AS INTEGER
DIM SHARED ObjectEX(0 TO 15) AS INTEGER
DIM SHARED ObjectSY(0 TO 15) AS INTEGER
DIM SHARED ObjectEY(0 TO 15) AS INTEGER
DIM SHARED MaxFrameNumber(0 TO 15) AS INTEGER
DIM SHARED CurrentFrame(0 TO 15) AS INTEGER
DIM SHARED LastFrame(0 TO 15) AS INTEGER
DIM SHARED ObjectRepeat(0 TO 15) AS INTEGER
DIM SHARED ObjectUseCount(0 TO 15) AS INTEGER
DIM SHARED ObjectLastCount(0 TO 15) AS INTEGER


'MAIN PROGRAM

'.----------------------------.
'| SETS slix's version number |
'`----------------------------'
slixVERSION% = 987

'.--------------------------.
'| SETS slix's release date |
'`--------------------------'
slixDATE$ = "August 10, 1996"

WARNING 'DO NOT REMOVE THIS UNLESS YOU UNDERSTAND
        'THE CONSEQUENCES

DO
  WIDTH 80, 25
  CLS
  COLOR 9, 0
  PRINT " 1) sprite demonstration"
  COLOR 10
  PRINT " 2) picture display (256x200x256)"
  COLOR 11
  PRINT " 3) picture display (256x224x256)"
  COLOR 12
  PRINT " 4) picture display (256x240x256)"
  COLOR 13
  PRINT " 5) picture display (256x400x256)"
  COLOR 14
  PRINT " 6) picture display (256x480x256)"
  COLOR 15
  PRINT " 7) picture display (320x200x256)"
  COLOR 9
  PRINT " 8) picture display (320x240x256)"
  COLOR 10
  PRINT " 9) picture display (320x400x256)"
  COLOR 11
  PRINT "10) picture display (320x480x256)"
  COLOR 12
  PRINT "11) picture display (360x200x256)"
  COLOR 13
  PRINT "12) picture display (360x240x256)"
  COLOR 14
  PRINT "13) picture display (360x270x256)"
  COLOR 15
  PRINT "14) picture display (360x360x256)"
  COLOR 9
  PRINT "15) picture display (360x400x256)"
  COLOR 10
  PRINT "16) picture display (360x480x256)"
  COLOR 11
  PRINT "17) picture display (376x282x256)"
  COLOR 12
  PRINT "18) picture display (376x308x256)"
  COLOR 13
  PRINT "19) picture display (376x564x256)"
  COLOR 14
  PRINT "20) picture display (400x300x256)"
  COLOR 15
  PRINT "21) picture display (400x600x256)"
  COLOR 7
  PRINT "[Q] Quit"
  COLOR 8
  PRINT "[D] Disclaimer"
  COLOR 10
  INPUT "Your Choice:", Selection$
  SELECT CASE UCASE$(Selection$)
    CASE IS = "1"
      VGA "320x200x256"
      SetVisiblePage 1
      SetActivePage 2
      FILEX "320x200.hr8", 0, 0, 0, 0
      'FILEX "320x200.bmp", 0, 0, 0, 0
      'FILEX "320x200.gif", 0, 0, 0, 0
      UseBackground% = 1
      SetVisiblePage 0
      SetActivePage 1
      ReservedPage% = 2
      UseReservedPage% = 1
      LoadSprites "gnoom2.gn2", Object(0)
      LoadSprites "sol07.gn2", Object(1)
      LoadSprites "sol14.gn2", Object(2)
      LoadSprites "ship.gn2", Object(3)
      LoadSprites "slix.gn2", Object(4)
     
      Object(5) = Object(4)
      'The sprite in Object(4) is now shared with Object(5)
     
      ShipNum% = Object(3)
      ShipX% = (VGAWidth% / 2) - (SpriteWidth(ShipNum%) / 2)
      ShipY% = (VGAHeight% / 2) - (SpriteHeight(ShipNum%) / 2)
      ColorCount% = 7
      PrevCursorX% = 0
      PrevCursorY% = 0
      DO
        Keyed$ = INKEY$
        SELECT CASE Keyed$
          CASE "B", "b"
            UseBackground% = (-1) * UseBackground%
          CASE IS = CHR$(0) + CHR$(&H4B)
            ShipX% = ShipX% - ((SpriteWidth(ShipNum%)) / 2)
          CASE IS = CHR$(0) + CHR$(&H4D)
            ShipX% = ShipX% + ((SpriteWidth(ShipNum%)) / 2)
          CASE IS = CHR$(0) + CHR$(&H48)
            ShipY% = ShipY% - ((SpriteHeight(ShipNum%)) / 2)
          CASE IS = CHR$(0) + CHR$(&H50)
            ShipY% = ShipY% + ((SpriteHeight(ShipNum%)) / 2)
        END SELECT
        SELECT CASE ShipX%
          CASE IS < 0
            ShipX% = 0
          CASE IS > (VGAWidth% - SpriteWidth(ShipNum%))
            ShipX% = (VGAWidth% - SpriteWidth(ShipNum%))
        END SELECT
        SELECT CASE ShipY%
          CASE IS < 0
            ShipY% = 0
          CASE IS > (VGAHeight% - SpriteHeight(ShipNum%))
            ShipY% = (VGAHeight% - SpriteHeight(ShipNum%))
        END SELECT
        SELECT CASE UseBackground%
          CASE IS = 1
            PageCopy ReservedPage%, ActivePage%
          CASE ELSE
            ClearPage
        END SELECT
        CursorX% = (((VGAWidth% / 5) / 2) - (44 / 2))
        CursorY% = 0
        COLOUR 255
        GPRINT "USE THE ARROW KEYS"
        COLOUR 254
        GPRINT " (and try pressing [B]...)"
        SetObject 0, 2, 1, 0, ((VGAHeight% - 1) - SpriteHeight(Object(0))), ((VGAWidth% - 1) - SpriteWidth(Object(0))), ((VGAHeight% - 1) - SpriteHeight(Object(0)))
        SetObject 1, 0, 1, ((VGAWidth% - 1) - SpriteWidth(Object(1))), 0, 0, 0
        SetObject 2, 0, 1, ((VGAWidth% - 1) - SpriteWidth(Object(2))), 50, 0, 50
        SetObject ShipNum%, 0, 1, ShipX%, ShipY%, ShipX%, ShipY%
        SetObject 4, 3, 1, ((VGAWidth% - 1) - SpriteWidth(Object(4))), ((VGAHeight% - 1) - SpriteHeight(Object(4))), 0, 0
        SetObject 5, 1, 1, 0, 12, 0, ((VGAHeight% - 1) - SpriteHeight(Object(5)))
        IF PrevCursorX% = ((VGAWidth% / 5) - 20) THEN DirectionX% = -1
        IF PrevCursorX% = 0 THEN DirectionX% = 1
        CursorX% = PrevCursorX% + DirectionX%
        PrevCursorX% = CursorX%
        IF PrevCursorY% = ((VGAHeight% / 5) - 1) THEN DirectionY% = -1
        IF PrevCursorY% = 0 THEN DirectionY% = 1
        CursorY% = PrevCursorY% + DirectionY%
        PrevCursorY% = CursorY%
        ColorCount% = ColorCount% + 8
        IF ColorCount% > 255 THEN ColorCount% = 7
        COLOUR ColorCount%
        GPRINT "Press "
        COLOUR 255
        GPRINT "["
        COLOUR 248
        GPRINT "Q"
        COLOUR 255
        GPRINT "]"
        COLOUR ColorCount%
        GPRINT " to quit..."
        PAGEFLIP
      LOOP UNTIL UCASE$(INKEY$) = "Q"
      UseReservedPage% = 0
      FreeObject 0
      FreeObject 1
      FreeObject 2
      FreeObject 3
      FreeObject 4
      FreeObject 5
    CASE IS = "2"
      DEMO "256x200x256"
    CASE IS = "3"
      DEMO "256x224x256"
    CASE IS = "4"
      DEMO "256x240x256"
    CASE IS = "5"
      DEMO "256x400x256"
    CASE IS = "6"
      DEMO "256x480x256"
    CASE IS = "7"
      DEMO "320x200x256"
    CASE IS = "8"
      DEMO "320x240x256"
    CASE IS = "9"
      DEMO "320x400x256"
    CASE IS = "10"
      DEMO "320x480x256"
    CASE IS = "11"
      DEMO "360x200x256"
    CASE IS = "12"
      DEMO "360x240x256"
    CASE IS = "13"
      DEMO "360x270x256"
    CASE IS = "14"
      DEMO "360x360x256"
    CASE IS = "15"
      DEMO "360x400x256"
    CASE IS = "16"
      DEMO "360x480x256"
    CASE IS = "17"
      DEMO "376x282x256"
    CASE IS = "18"
      DEMO "376x308x256"
    CASE IS = "19"
      DEMO "376x564x256"
    CASE IS = "20"
      DEMO "400x300x256"
    CASE IS = "21"
      DEMO "400x600x256"
    CASE IS = "Q"
      SCREEN 0
      WIDTH 80, 25
      COLOR 7, 0
      CLS
      PRINT "Thanks for testing out slix."
      END
    CASE IS = "D"
      CLS
      COLOR 12
      PRINT "#########################################################"
      PRINT "# DISCLAIMER: USE slix AT YOUR OWN RISK!                #"
      PRINT "#             The author is not liable for any problems #"
      PRINT "#             that may result from the use of slix.     #"
      PRINT "#                                                       #"
      PRINT "#########################################################"
      PRINT
      COLOR 7
      PRINT "Press any key to continue..."
      SLEEP
  END SELECT
LOOP

REM $STATIC
FUNCTION BIN$ (Number&)
DO WHILE Number& > 0
  bit% = Number& MOD 2
  Number& = Number& \ 2
  Number$ = RIGHT$(STR$(bit%), 1) + Number$
LOOP
BIN$ = Number$
END FUNCTION

'Limited from 0 to 2,147,483,647 (1111111111111111111111111111111)
FUNCTION BIND& (Number$)
FOR Count% = LEN(Number$) TO 1 STEP -1
  IF MID$(Number$, Count%, 1) = "1" THEN Number& = (Number& + (2 ^ (LEN(Number$) - Count%)))
NEXT Count%
BIND& = Number&
END FUNCTION

SUB ClearPage
OUT &H3C4, 2
DEF SEG = &HA000&
PageSize& = ((VGAWidthBytes% * VGAHeight%) - 1)
OUT &H3C5, 1
FOR Count& = 0 TO PageSize&
  POKE Addr& + Count& + ActiveStart&, 0
NEXT Count&
OUT &H3C5, 2
FOR Count& = 0 TO PageSize&
  POKE Addr& + Count& + ActiveStart&, 0
NEXT Count&
OUT &H3C5, 4
FOR Count& = 0 TO PageSize&
  POKE Addr& + Count& + ActiveStart&, 0
NEXT Count&
OUT &H3C5, 8
FOR Count& = 0 TO PageSize&
  POKE Addr& + Count& + ActiveStart&, 0
NEXT Count&
DEF SEG
END SUB

SUB COLOUR (DUMMY%)
SELECT CASE DUMMY%
  CASE IS > 255
    DUMMY% = 255
  CASE IS < 0
    DUMMY% = 0
END SELECT
CurrentColour% = DUMMY%
END SUB

SUB DEMO (Mode$)
'Begin demonstration
SecondsWait% = 12
VGA Mode$
SetActivePage 0
CursorX% = 0
CursorY% = 7
COLOUR 232
GPRINT "Please wait..."
GPRINT ""
COLOUR 255
GPRINT "(THIS MESSAGE SHOULD NOT BE ON THE SAME PAGE"
GPRINT ""
COLOUR 232
GPRINT " AS THE PICTURE, UNLESS...)"
GPRINT ""
COLOUR 255
GPRINT "THE PAGE FLIPPING DEMONSTRATION WON'T WORK"
GPRINT ""
COLOUR 232
GPRINT "CORRECTLY ON MODES THAT HAVE LESS THAN 2 PAGES:"
GPRINT ""
COLOUR 255
GPRINT " 320x480x256, 360x400x256, 360x480x256,"
GPRINT ""
COLOUR 232
GPRINT " 376x564x256, 400x600x256)"
GPRINT ""
GPRINT ""
BeginFILEXTimer# = TIMER
SetVisiblePage 0
SetActivePage 1
FILEX "256x200.HR8", 0, 0, 0, 0
EndFILEXTimer# = TIMER
SetVisiblePage 1
CursorX% = 0
CursorY% = 0
COLOUR 7
GPRINT "This is "
COLOUR 255
GPRINT Mode$
COLOUR 7
GPRINT " mode."
GPRINT ""
GPRINT ""
COLOUR 24
GPRINT "The picture is 256 pixels wide,"
GPRINT ""
COLOUR 252
GPRINT "200 pixels long, and has 256 colors"
GPRINT ""
GPRINT ""
COLOUR 71
GPRINT "Press any key to continue...or wait" + STR$(SecondsWait%) + " seconds..."
GPRINT ""
GPRINT ""
SLEEP SecondsWait%
SetVisiblePage 0
SetActivePage 0
CursorX% = 0
CursorY% = 15
COLOUR 154
GPRINT "The FILEX sub-routine took" + STR$(EndFILEXTimer# - BeginFILEXTimer#) + " seconds."
GPRINT ""
GPRINT ""
COLOUR 63
GPRINT STR$(1 / (EndFILEXTimer# - BeginFILEXTimer#))
COLOUR 255
GPRINT " frame(s) per second."
GPRINT ""
COLOUR 17
GPRINT SPACE$(19) + "(for 256x200x256 frames)."
GPRINT ""
GPRINT ""
COLOUR 255
GPRINT "Press any key to continue...or wait" + STR$(SecondsWait%) + " seconds..."
GPRINT ""
GPRINT ""
CursorX% = 0
CursorY% = 7
COLOUR 0
GPRINT STRING$(14, 219)
SetVisiblePage 0
SLEEP SecondsWait%
END SUB

SUB DrawFrame (SpriteNum%, FrameNum%, XCord%, YCord%)
SELECT CASE SpriteNum%
  CASE IS = 0
    PUTX XCord%, YCord%, SpriteWidth(SpriteNum%), SpriteHeight(SpriteNum%), Sprite0(FrameNum%).Each4K, 0
  CASE IS = 1
    PUTX XCord%, YCord%, SpriteWidth(SpriteNum%), SpriteHeight(SpriteNum%), Sprite1(FrameNum%).Each4K, 0
  CASE IS = 2
    PUTX XCord%, YCord%, SpriteWidth(SpriteNum%), SpriteHeight(SpriteNum%), Sprite2(FrameNum%).Each4K, 0
  CASE IS = 3
    PUTX XCord%, YCord%, SpriteWidth(SpriteNum%), SpriteHeight(SpriteNum%), Sprite3(FrameNum%).Each4K, 0
  CASE IS = 4
    PUTX XCord%, YCord%, SpriteWidth(SpriteNum%), SpriteHeight(SpriteNum%), Sprite4(FrameNum%).Each4K, 0
  CASE IS = 5
    PUTX XCord%, YCord%, SpriteWidth(SpriteNum%), SpriteHeight(SpriteNum%), Sprite5(FrameNum%).Each4K, 0
  CASE IS = 6
    PUTX XCord%, YCord%, SpriteWidth(SpriteNum%), SpriteHeight(SpriteNum%), Sprite6(FrameNum%).Each4K, 0
  CASE IS = 7
    PUTX XCord%, YCord%, SpriteWidth(SpriteNum%), SpriteHeight(SpriteNum%), Sprite7(FrameNum%).Each4K, 0
END SELECT
END SUB

SUB FILEX (filename$, XCord%, YCord%, UseZero%, Center%)

RGBPaletteFile% = FREEFILE
PaletteRGB$ = SPACE$(768)
DIM RGBPaletteTranslator(255) AS INTEGER
OPEN "RGB.PAL" FOR BINARY AS #RGBPaletteFile%
GET #RGBPaletteFile%, 1, PaletteRGB$
CLOSE #RGBPaletteFile%

FILEXFile% = FREEFILE

'Open file for input so QB stops with an error if it doesn't exist.
OPEN filename$ FOR INPUT AS #FILEXFile%
CLOSE #FILEXFile%

OPEN filename$ FOR BINARY AS #FILEXFile%

SELECT CASE LOF(FILEXFile%)
  CASE IS > 32
    HeaderTest$ = SPACE$(32)
  CASE ELSE
    HeaderTest$ = SPACE$(LOF(FILEXFile%))
END SELECT

GET #FILEXFile%, , HeaderTest$
SELECT CASE LEFT$(HeaderTest$, 3)
  CASE IS = "HR8"
    HeaderSig% = 3
    HeaderSig$ = "HR8"
END SELECT

SELECT CASE LEFT$(HeaderTest$, 2)
  CASE IS = "BM"
    HeaderSig% = 2
    HeaderSig$ = "BMPWIN"
END SELECT

SELECT CASE LEFT$(HeaderTest$, 3)
  CASE IS = "GIF"
    HeaderSig% = 3
    HeaderSig$ = "GIF"
END SELECT

'HR8 header format:
'
'Bytes 1, 2, 3   = The characters: "HR8" (without quotes)
'Bytes 4 and 5   = Horizontal size (signed integer)
'Bytes 6 and 7   = Vertical size (signed integer)
'
'note: All numbers are stored in their hexdecimal equivalent.
'      WORDs should not exceed "7FFF", which equals 32767.
'
'note: All HR8 files contain one byte per color, the RGB palette
'      should be used to view HR8 files.

SELECT CASE HeaderSig$
  CASE IS = "HR8"
    HeaderInfo% = 4
    HeaderInfo$ = SPACE$(HeaderInfo%)
    GET #FILEXFile%, HeaderSig% + 1, HeaderInfo$
    headersize% = HeaderSig% + HeaderInfo%
    HR8Width% = ((ASC(MID$(HeaderInfo$, 1, 1)) * 256) + ASC(MID$(HeaderInfo$, 2, 1)))
    HR8Height% = ((ASC(MID$(HeaderInfo$, 3, 1)) * 256) + ASC(MID$(HeaderInfo$, 4, 1)))
    AfterHeader& = CLNG(HR8Width%) * HR8Height%
    BufferSize% = (32767 - (32767 MOD HR8Width%))
    Buffer$ = SPACE$(BufferSize%)
    'Centering disregards the XCord%
    'and YCord% values specified
    SELECT CASE Center%
      CASE IS = 1
        XCord% = ABS((VGAWidth% - HR8Width%) / 2)
        YCord% = ABS((VGAHeight% - HR8Height%) / 2)
    END SELECT
    DO
      GET #FILEXFile%, , Buffer$
      Counter& = (LEN(Buffer$) + Counter&)
      SELECT CASE Counter&
        CASE IS > AfterHeader&
          Counter& = (Counter& - (Counter& MOD AfterHeader&))
      END SELECT
      PUTX (XCord% + (Counter& MOD HR8Width%)), (YCord% + ((OldCounter&) / HR8Width%)), HR8Width%, (LEN(Buffer$) / HR8Width%), Buffer$, UseZero%
      OldCounter& = Counter&
    LOOP UNTIL EOF(FILEXFile%)
  CASE IS = "BMPWIN"
    header$ = SPACE$(14)
    sizing$ = SPACE$(4)
    GET #FILEXFile%, 1, header$
    GET #FILEXFile%, 15, sizing$
    bmpinfosize = CVI(sizing$)
    'bmpinfosize - Is the size of the information header for the bitmap.
    '              Different bitmap versions have variations in filetypes.
    '              40 is a standard windows 3.1 bitmap.
    '              12 is for OS/2 bitmaps
    'The next routine reads in the appropriate headers and colour tables.
    'nbits is the number of bits per pixel - i.e. number of colours
    '1 bit = 2 colours, 4 bits = 16 colours, 8 bits = 256 colours, etc.
    'the 24 bit mode does not have a palette, its colours are expressed as
    'image data

    'Design of a windows 3.1 bitmap - Taken from bmp.zip on the
    'x2ftp.oulu.fi ftp site under /pub/msdos/programming/formats
    'Specifications for a Windows 3.1 bitmap. (.BMP)
    'Email any questions/responses to me at zabudsk@ecf.utoronto.ca
    'or post to alt.lang.basic or comp.lang.basic.misc.

    '       | # of   |
    'Offset | bytes  | Function (value)
    '-------+--------+--- General Picture information starts here---------
    '  0    |   2    | (BM) - Tells us that the picture is in bmp format
    '  2    |   4    | Size of the file (without header?)
    '  6    |   2    | (0) Reserved1 - Must be zero
    '  8    |   2    | (0) Reserved2 - Must be zero
    '  10   |   4    | Number of bytes offset of the picture data
    '-------+--------+--- Information Header starts here -----------------
    '  14   |   4    | (40/12) Size of information header (Win3.1/OS2)
    '  18   |   4    | Picture width in pixels
    '  22   |   4    | Picture Height in pixels
    '  26   |   2    | (1) Number of planes, must be 1
    '  28   |   2    | Number of bits per pixel (bpp), must be 1,4,8 or 24
    '  30   |   4    | (0) Compression - 0 means no compression, 1,2 are RLEs
    '  34   |   4    | Image size in bytes
    '  38   |   4    | picture width in pels per metre
    '  42   |   4    | picture height in pels per metre
    '  46   |   4    | (0) Number of colours used in the picture, 0 means all
    '  50   |   4    | (0) Number of important colours, 0 means all
    '-------+--------+--- Palette data starts here -----------------------
    '  54   |   1    | (b) - blue intensity component, color 0 - range 0 to 255
    '  55   |   1    | (g) - green intensity component, color 0 - range 0 to 255
    '  56   |   1    | (r) - red intensity component, color 0 - range 0 to 255
    '  57   |   1    | (0) - unused
    '  58   |   1    | (b) - blue intensity component, color 0 - range 0 to 255
    '  ...  | ...    |
    '  54   | 4*2^bpp| total range of palette
    '-------+--------+--- Image data starts here -------------------------
    '54+    | width* | Bitmap data starting at lower left portion of the
    '(4*2^n)| height*| image moving from left towards right. Moving up 1
    '       | (8/bpp)| pixel when at the right hand side of the image, starting
    '       |        | from the left side again, until the top right of the
    '       |        | image is reached

    'Note that this format is slightly different for a OS/2 Bitmap.
    'The header is the same up to (but not including) bit 30-
    'The palette colour values follow at bit 30, with the form...
    '1 byte blue intensity
    '1 byte green intensity
    '1 byte red intensity
    'For each colour of the picture.
    'Bitmapped image data follows the colour tables


    'Special note: When storing 1 bit (2 colour) pictures.
    '8 horizontal pixels are packed into 1 byte. Each bit determines
    'the colour of one pixel (colour 0 or colour 1)

    '4 bit pictures (16 colours) use 2 nibbles (4 bits) for each pixel
    'thus there are 2 pixels for each byte of image data.

    '8 bit pictures use 1 byte per pixel. Each byte of image data
    'represents one of 256 colours.

    '24 bit pictures express colour values by using 3 bytes and each has a
    'value between 0 and 255. The first byte is for red, the second is for
    'green and the third is for blue. Thus (256)^3 or 2^24 of 16777216 different
    'colours.

    IF bmpinfosize = 12 THEN
       infoheader$ = SPACE$(12)
       GET #FILEXFile%, 15, infoheader$
       nbits = CVI(MID$(infoheader$, 15, 4))

       IF nbits = 1 THEN
          palet$ = SPACE$(6)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       ELSEIF nbits = 4 THEN
          palet$ = SPACE$(48)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       ELSEIF nbits = 8 THEN
          palet$ = SPACE$(768)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       END IF
    ELSEIF bmpinfosize = 40 THEN
       infoheader$ = SPACE$(40)
       GET #FILEXFile%, 15, infoheader$
       nbits = CVI(MID$(infoheader$, 15, 4))
       IF nbits = 1 THEN
          palet$ = SPACE$(8)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       ELSEIF nbits = 4 THEN
          palet$ = SPACE$(64)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       ELSEIF nbits = 8 THEN
          palet$ = SPACE$(1024)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       END IF
    END IF
    

    ft$ = MID$(header$, 1, 2)
    'PRINT "Type of file (Should be BM): "; ft$

    filesize& = CVL(MID$(header$, 3, 4))
    'PRINT "Size of file: "; filesize&

    r1 = CVI(MID$(header$, 7, 2))
    'PRINT "Reserved 1: "; r1

    r2 = CVI(MID$(header$, 9, 2))
    'PRINT "Reserved 2: "; r2

    offset = CVL(MID$(header$, 11, 4))
    'PRINT "Number of bytes offset from beginning: "; offset

    'PRINT

    headersize = CVL(MID$(infoheader$, 1, 4))
    'PRINT "Size of header: "; headersize

    picwidth = CVL(MID$(infoheader$, 5, 4))
    'PRINT "Width: "; picwidth

    picheight = CVL(MID$(infoheader$, 9, 4))
    'PRINT "Height: "; picheight

    nplanes = CVI(MID$(infoheader$, 13, 4))
    'PRINT "Planes: "; nplanes

    'PRINT "Bits per plane: "; nbits

    'PRINT

    IF headersize = 40 THEN
       'PRINT "Compression: ";
       comptype = CVL(MID$(infoheader$, 17, 4))
       'IF comptype = 0 THEN PRINT "None"
       'IF comptype = 1 THEN PRINT "Run Length - 8 Bits"
       'IF comptype = 2 THEN PRINT "Run Length - 4 Bits"
 
       imagesize& = CVL(MID$(infoheader$, 21, 4))
       'PRINT "Image Size (bytes): "; imagesize&
 
       xsize = CVL(MID$(infoheader$, 25, 4))
       'PRINT "X size (pixels per metre): "; xsize
 
       ysize = CVL(MID$(infoheader$, 29, 4))
       'PRINT "Y size (pixels per metre): "; ysize
 
       colorsused = CVL(MID$(infoheader$, 33, 4))
       'PRINT "Number of colours used: "; colorsused
 
       neededcolours = CVL(MID$(infoheader$, 37, 4))
       'PRINT "Number of important colours: "; neededcolours
    END IF
    'PRINT
    'PRINT "Press Any key to continue."
    'WHILE INKEY$ = ""
    'WEND

    IF nbits = 1 THEN
       'SCREEN 11
    ELSEIF nbits = 4 THEN
       'SCREEN 12
    ELSEIF nbits = 8 OR nbits = 24 THEN
       'SCREEN 13
    END IF
    IF bmpinfosize = 40 THEN ngroups = 4
    IF bmpinfosize = 12 THEN ngroups = 3

    IF nbits = 24 THEN
       IF ngroups = 3 THEN
          FOR c = 0 TO 63
             d = c * 4
             palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d)
             palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d + 1)
             palet$ = palet$ + CHR$(d) + CHR$(d + 1) + CHR$(d)
             palet$ = palet$ + CHR$(d + 1) + CHR$(d) + CHR$(d)
          NEXT c
       ELSEIF ngroups = 4 THEN
          FOR c = 0 TO 63
             d = c * 4
             palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d) + CHR$(0)
             palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d + 1) + CHR$(0)
             palet$ = palet$ + CHR$(d) + CHR$(d + 1) + CHR$(d) + CHR$(0)
             palet$ = palet$ + CHR$(d + 1) + CHR$(d) + CHR$(d) + CHR$(0)
          NEXT c
       END IF
    END IF

    FOR X = 1 TO LEN(palet$) STEP ngroups
       zb# = INT((ASC(MID$(palet$, X, 1))) / 4)
       zg# = INT((ASC(MID$(palet$, X + 1, 1))) / 4)
       zr# = INT((ASC(MID$(palet$, X + 2, 1))) / 4)
       Blue% = zb# \ (36 / 4)
       Green% = zg# \ (84 / 4)
       Red% = zr# \ (36 / 4)
       FOR Count% = 0 TO 255
         SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 1), 1)) \ 36)
           CASE IS = Red%
             SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 2), 1)) \ 84)
               CASE IS = Green%
                 SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 3), 1)) \ 36)
                   CASE IS = Blue%
                     RGBPaletteTranslator((X - 1) / ngroups) = Count%
                 END SELECT
             END SELECT
         END SELECT
       NEXT Count%
       zc# = zb# * 65536# + zg# * 256# + zr#
       cres = ASC(MID$(palet$, X + 3, 1))
       'PALETTE ((x - 1) / ngroups), zc#
    NEXT X

    IF nbits = 24 THEN
       y = picheight - 1
       X = 0
       dat$ = "   "
       WHILE y >= 0
          WHILE X < picwidth
             GET 1, , dat$
             p1 = INT((ASC(MID$(dat$, 1, 1)) + ASC(MID$(dat$, 1, 1)) + ASC(MID$(dat$, 1, 1))) / 3)
             IF X < VGAWidth% AND y < VGAHeight% THEN PSETX X, y, RGBPaletteTranslator(p1)
             X = X + 1
          WEND
          y = y - 1
          X = 0
       WEND
    ELSEIF nbits = 8 THEN
       y = picheight - 1
       X = 0
       dat$ = " "
       WHILE y >= 0
          WHILE X < picwidth
             GET 1, , dat$
             IF X < VGAWidth% AND y < VGAHeight% THEN PSETX X, y, RGBPaletteTranslator(ASC(dat$))
             X = X + 1
          WEND
          y = y - 1
          X = 0
       WEND
    ELSEIF nbits = 4 THEN
       y = picheight - 1
       X = 0
       dat$ = " "
       WHILE y >= 0
          WHILE X < picwidth
            GET 1, , dat$
            LOCATE 1, 1
            p1 = ASC(dat$) AND 15
            p2 = ASC(dat$) AND 240 / 16
            IF X < VGAWidth% AND y < VGAHeight% THEN PSETX X, y, RGBPaletteTranslator(p1)
            IF X < VGAWidth% AND y < VGAHeight% THEN PSETX X + 1, y, RGBPaletteTranslator(p2)
            X = X + 2
          WEND
          y = y - 1
          X = 0
       WEND
    ELSEIF nbits = 1 THEN
       y = picheight - 1
       X = 0
       dat$ = " "
       WHILE y >= 0
          WHILE X < picwidth
            GET 1, , dat$
            FOR P = 0 TO 7
               IF X < VGAWidth% AND y < VGAHeight% THEN PSETX X + (7 - P), y, RGBPaletteTranslator((p1 AND 2 ^ P) / 2 ^ P)
               NEXT P
            X = X + 8
          WEND
          y = y - 1
          X = 0
       WEND
    END IF

    'CLOSE
  CASE IS = "GIF"
    '
    'DEGIF6.BAS - No frills GIF decoder for the VGA's 320x200x256 mode.
    'By Rich Geldreich 1993 (Public domain, use as you wish.)
    'This version should properly decode all LZW encoded images in
    'GIF image files. I've finally added GIF89a and local colormap
    'support, so it more closely follows the GIF specification. It
    'still doesn't support the entire GIF89a specification, but it'll
    'show most GIF files fine.
    'The GIF decoding speed of this program isn't great, but I'd say
    'for an all QB/PDS decoder it's not bad!
    'Note: This program does not stop decoding the GIF image after the
    'rest of the scanlines become invisible! This happens with images
    'larger than the 320x200 screen. So if the program seems to be
    'just sitting there, accessing your hard disk, don't worry...
    'It'll beep when it's done.
    'DEFINT A-Z
    'Prefix() and Suffix() hold the LZW phrase dictionary.
    'OutStack() is used as a decoding stack.
    'ShiftOut() as a power of two table used to quickly retrieve the LZW
    'multibit codes.
    DIM Prefix(4095), Suffix(4095), OutStack(4095), ShiftOut(8)

    'The following line is for the QB environment(slow).
    DIM YBase AS LONG, Powersof2(11) AS LONG, WorkCode AS LONG
    'For a little more speed, unremark the next line and remark the one
    'above, before you compile... You'll get an overflow error if the
    'following line is used in the QB environment, so change it back.
    'DIM YBase AS INTEGER, Powersof2(11) AS INTEGER, WorkCode AS INTEGER

    'Precalculate power of two tables for fast shifts.
    FOR a = 0 TO 8: ShiftOut(8 - a) = 2 ^ a: NEXT
    FOR a = 0 TO 11: Powersof2(a) = 2 ^ a: NEXT

    'Get GIF filename.
    'A$ = COMMAND$: IF A$ = "" THEN INPUT "GIF file"; A$: IF A$ = "" THEN END
    'Add GIF extension if the given filename doesn't have one.
    'FOR A = LEN(filename$) TO 1 STEP -1
    'SELECT CASE MID$(filename$, A, 1)
        'CASE "\", ":": EXIT FOR
        'CASE ".": Extension = -1: EXIT FOR
        'END SELECT
    'NEXT
    'IF Extension = 0 THEN filename$ = filename$ + ".GIF"

    'Open file for input so QB stops with an error if it doesn't exist.
    'OPEN A$ FOR INPUT AS #FILEXFile%: CLOSE #FILEXFile%
    'OPEN A$ FOR BINARY AS #FILEXFile%

    'Check to see if GIF file. Ignore GIF version number.
    a$ = "      ": GET #FILEXFile%, 1, a$
    'IF LEFT$(A$, 3) <> "GIF" THEN PRINT "Not a GIF file.": END

    'Get logical screen's X and Y resolution.
    GET #FILEXFile%, , TotalX: GET #FILEXFile%, , TotalY: GOSUB GetByte
    'Calculate number of colors and find out if a global palette exists.
    NumColors = 2 ^ ((a AND 7) + 1): NoPalette = (a AND 128) = 0
    'Retrieve background color.
    GOSUB GetByte: Background = a

    'Get aspect ratio and ignore it.
    GOSUB GetByte

    'Retrieve global palette if it exists.
    IF NoPalette = 0 THEN P$ = SPACE$(NumColors * 3): GET #FILEXFile%, , P$

    DO 'Image decode loop

    'Skip by any GIF extensions.
    '(With a few modifications this code could also fetch comments.)
    DO
        'Skip by any zeros at end of image (why must I do this? the
        'GIF spec never mentioned it)
        DO
            IF EOF(FILEXFile%) THEN GOTO AllDone 'if at end of file, exit
            GOSUB GetByte
        LOOP WHILE a = 0           'loop while byte fetched is zero

        SELECT CASE a
        CASE 44  'We've found an image descriptor!
            EXIT DO
        CASE 59  'GIF trailer, stop decoding.
            GOTO AllDone
        CASE IS <> 33
            'PRINT "Unknown GIF extension type."': END
        END SELECT
        'Skip by blocked extension data.
        GOSUB GetByte
        DO: GOSUB GetByte: a$ = SPACE$(a): GET #FILEXFile%, , a$: LOOP UNTIL a = 0
    LOOP
    'Get image's start coordinates and size.
    GET #FILEXFile%, , XStart: GET #FILEXFile%, , YStart: GET #FILEXFile%, , XLength: GET #FILEXFile%, , YLength
    XEnd = XStart + XLength: YEnd = YStart + YLength

    'Check for local colormap, and fetch it if it exists.
    GOSUB GetByte
    IF (a AND 128) THEN
        NoPalette = 0
        NumColors = 2 ^ ((a AND 7) + 1)
        P$ = SPACE$(NumColors * 3): GET #FILEXFile%, , P$
    END IF

    'Check for interlaced image.
    Interlaced = (a AND 64) > 0: PassNumber = 0: PassStep = 8

    'Get LZW starting code size.
    GOSUB GetByte

    'Calculate clear code, end of stream code, and first free LZW code.
    ClearCode = 2 ^ a
    EOSCode = ClearCode + 1
    FirstCode = ClearCode + 2: NextCode = FirstCode
    StartCodeSize = a + 1: CodeSize = StartCodeSize

    'Find maximum code for the current code size.
    StartMaxCode = 2 ^ (a + 1) - 1: MaxCode = StartMaxCode

    BitsIn = 0: BlockSize = 0: BlockPointer = 1

    X = XStart: y = YStart: YBase = y * CLNG(VGAWidth%)

    'Set screen 13 in not set yet.
    IF FirstTime = 0 THEN
        'Go to VGA mode 13 (320x200x256).
        'SCREEN 13: DEF SEG = &HA000
    END IF

    'Set palette, if there was one.
    IF NoPalette = 0 THEN
        'Use OUTs for speed.
        'OUT &H3C8, 0
        'FOR A = 1 TO NumColors * 3: OUT &H3C9, ASC(MID$(P$, A, 1)) \ 4: NEXT
        FOR a% = 0 TO (NumColors - 1)
          Blue% = ASC(MID$(P$, (a * 3) + 1, 1)) \ 36
          Green% = ASC(MID$(P$, (a * 3) + 2, 1)) \ 84
          Red% = ASC(MID$(P$, (a * 3) + 3, 1)) \ 36
          FOR Count% = 0 TO 255
            SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 1), 1)) \ 36)
              CASE IS = Blue%
                SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 2), 1)) \ 84)
                  CASE IS = Green%
                    SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 3), 1)) \ 36)
                      CASE IS = Red%
                        RGBPaletteTranslator(a%) = Count%
                    END SELECT
                END SELECT
            END SELECT
          NEXT Count%
        NEXT a%
        'Save palette of image to disk.
        'OPEN "pal." FOR BINARY AS #2: PUT #2, , P$: CLOSE #2
    END IF

    'IF FirstTime = 0 THEN
      'Clear entire screen to background color. This isn't
      'done until the image's palette is set, to avoid flicker
      'on some GIFs.
        'LINE (0, 0)-(319, 199), Background, BF
        'FirstTime = -1
    'END IF

    'Decode LZW data stream to screen.
    DO
        'Retrieve one LZW code.
        GOSUB GetCode
        'Is it an end of stream code?
        IF Code <> EOSCode THEN
            'Is it a clear code? (The clear code resets the sliding
            'dictionary - it *should* be the first LZW code present in
            'the data stream.)
            IF Code = ClearCode THEN
                NextCode = FirstCode
                CodeSize = StartCodeSize
                MaxCode = StartMaxCode
                DO: GOSUB GetCode: LOOP WHILE Code = ClearCode
                IF Code = EOSCode THEN GOTO ImageDone
                LastCode = Code: LastPixel = Code
                'IF X < 320 AND y < 200 THEN POKE X + YBase, LastPixel
                'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, LastPixel
                IF X < VGAWidth% AND y < VGAHeight% THEN PSETX X, y, RGBPaletteTranslator(LastPixel)
                X = X + 1: IF X = XEnd THEN GOSUB NextScanLine
            ELSE
                CurCode = Code: StackPointer = 0

                'Have we entered this code into the dictionary yet?
                IF Code >= NextCode THEN
                    IF Code > NextCode THEN GOTO AllDone 'Bad GIF if this happens.
                   'mimick last code if we haven't entered the requested
                   'code into the dictionary yet
                    CurCode = LastCode
                    OutStack(StackPointer) = LastPixel
                    StackPointer = StackPointer + 1
                END IF

                'Recursively get each character of the string.
                'Since we get the characters in reverse, "push" them
                'onto a stack so we can "pop" them off later.
                'Hint: There is another, much faster way to accomplish
                'this that doesn't involve a decoding stack at all...
                DO WHILE CurCode >= FirstCode
                    OutStack(StackPointer) = Suffix(CurCode)
                    StackPointer = StackPointer + 1
                    CurCode = Prefix(CurCode)
                LOOP

                LastPixel = CurCode
                'IF X < 320 AND y < 200 THEN POKE X + YBase, LastPixel
                'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, LastPixel
                IF X < VGAWidth% AND y < VGAHeight% THEN PSETX X, y, RGBPaletteTranslator(LastPixel)
                X = X + 1: IF X = XEnd THEN GOSUB NextScanLine

                '"Pop" each character onto the display.
                FOR a = StackPointer - 1 TO 0 STEP -1
                    'IF X < 320 AND y < 200 THEN POKE X + YBase, OutStack(A)
                    'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, OutStack(A)
                    IF X < VGAWidth% AND y < VGAHeight% THEN PSETX X, y, RGBPaletteTranslator(OutStack(a))
                    X = X + 1: IF X = XEnd THEN GOSUB NextScanLine
                NEXT

                'Can we put this new string into our dictionary? (Some GIF
                'encoders will wait a bit when the dictionary is full
                'before sending a clear code- this increases compression
                'because the dictionary's contents are thrown away less
                'often.)
                IF NextCode < 4096 THEN
                    'Store new string in the dictionary for later use.
                    Prefix(NextCode) = LastCode
                    Suffix(NextCode) = LastPixel
                    NextCode = NextCode + 1
                    'Time to increase the LZW code size?
                    IF (NextCode > MaxCode) AND (CodeSize < 12) THEN
                        CodeSize = CodeSize + 1
                        MaxCode = MaxCode * 2 + 1
                    END IF
                END IF
                LastCode = Code
            END IF
        END IF
    LOOP UNTIL Code = EOSCode
ImageDone:

    LOOP

AllDone:

    'Save image and palette to BSAVE file.
    'DEF SEG = &HA000
    'OUT &H3C7, 0
    'FOR a = 0 TO 767
    '    POKE a + 64000, INP(&H3C9)
    'NEXT
    'BSAVE "pic.bas", 0, 64768

    'Load images saved with the above code with this:
    'DEF SEG= &HA000
    'BLOAD "Pic.Bas"
    'OUT &H3C8, 0
    'FOR a = 0 To 767
    '     OUT &H3C9, Peek(a+ 64000)
    'NEXT

    'BEEP: DO: LOOP WHILE INKEY$ <> "": DO: LOOP UNTIL INKEY$ <> ""
    'END
    GOTO EndGIFRoutine 'Yes, I know...This is poorly structured programming


    'Slowly reads one byte from the GIF file...
GetByte: a$ = " ": GET #FILEXFile%, , a$: a = ASC(a$): RETURN

    'Moves down one scanline. If the GIF is interlaced, then the number
    'of scanlines skipped is based on the current pass.
NextScanLine:
        IF Interlaced THEN
            y = y + PassStep
            IF y >= YEnd THEN
                PassNumber = PassNumber + 1
                SELECT CASE PassNumber
                CASE 1: y = 4: PassStep = 8
                CASE 2: y = 2: PassStep = 4
                CASE 3: y = 1: PassStep = 2
                END SELECT
            END IF
        ELSE
            y = y + 1
        END IF
        X = XStart: YBase = y * CLNG(VGAWidth%)
    RETURN

    'Reads a multibit code from the data stream.
GetCode:
        WorkCode = LastChar \ ShiftOut(BitsIn)
      'Loop while more bits are needed.
        DO WHILE CodeSize > BitsIn
    'Reads a byte from the LZW data stream. Since the data stream is
    'blocked, a check is performed for the end of the current block
    'before each byte is fetched.
            IF BlockPointer > BlockSize THEN
              'Retrieve block's length
                GOSUB GetByte: BlockSize = a
                a$ = SPACE$(BlockSize): GET #FILEXFile%, , a$
                BlockPointer = 1
            END IF
          'Yuck, ASC() and MID$() aren't that fast.
            LastChar = ASC(MID$(a$, BlockPointer, 1))
            BlockPointer = BlockPointer + 1
          'Append 8 more bits to the input buffer
            WorkCode = WorkCode OR LastChar * Powersof2(BitsIn)
            BitsIn = BitsIn + 8
        LOOP
      'Take away x number of bits.
        BitsIn = BitsIn - CodeSize
      'Return code to caller.
        Code = WorkCode AND MaxCode
    RETURN
EndGIFRoutine:
  CASE ELSE
    'Assume the file is in RAW format
    AfterHeader& = CLNG(VGAWidth%) * VGAHeight%
    SELECT CASE AfterHeader&
      CASE IS > LOF(FILEXFile%)
        AfterHeader& = FILEXFile%
    END SELECT
    BufferSize% = (32767 - (32767 MOD VGAWidth%))
    Buffer$ = SPACE$(BufferSize%)
    'Centering disregards the XCord%
    'and YCord% values specified
    DO
      GET #FILEXFile%, , Buffer$
      Counter& = (LEN(Buffer$) + Counter&)
      SELECT CASE Counter&
        CASE IS > AfterHeader&
          Counter& = (Counter& - (Counter& MOD AfterHeader&))
      END SELECT
      PUTX (XCord% + (Counter& MOD VGAWidth%)), (YCord% + ((OldCounter&) / VGAWidth%)), VGAWidth%, (LEN(Buffer$) / VGAWidth%), Buffer$, UseZero%
      OldCounter& = Counter&
    LOOP UNTIL EOF(FILEXFile%)
END SELECT
CLOSE #FILEXFile%
END SUB

SUB FreeObject (ObjectNumber%)
ObjectLock(ObjectNumber%) = 0
FOR Count% = 0 TO 15
  SELECT CASE Object(Count%)
    CASE IS = Object(ObjectNumber%)
      SELECT CASE ObjectLock(Count%)
        CASE IS = 1
          PreserveSprite% = 1
      END SELECT
  END SELECT
NEXT Count%
SELECT CASE PreserveSprite%
  CASE IS = 0
    SpriteLoaded(Object(ObjectNumber%)) = 0
  CASE ELSE
END SELECT
END SUB

FUNCTION GETX% (XCord%, YCord%)
'Select the plane from which we must read the pixel color:
outport &H3CE, 4
outport &H3CF, (XCord% AND 3)
DEF SEG = &HA000&
GETX% = PEEK((VGAWidthBytes% * YCord%) + (XCord% \ 4) + ActiveStart&)
DEF SEG
END FUNCTION

SUB GPRINT (DUMMY$)
SELECT CASE LEN(DUMMY$)
  CASE IS = 0
    CursorX% = 0
    CursorY% = CursorY% + 1
    SELECT CASE CursorY%
      CASE IS < 0
        CursorY% = 0
      CASE IS > ((VGAHeight% / 5) - 1)
        CursorX% = ((VGAWidth% / 5) - 1)
        CursorY% = ((VGAHeight% / 5) - 1)
    END SELECT
  CASE ELSE
    SELECT CASE CursorX%
      CASE IS < 0
        CursorX% = 0
      CASE IS > ((VGAWidth% / 5) - 1)
        CursorX% = ((VGAWidth% / 5) - 1)
    END SELECT
    SELECT CASE CursorY%
      CASE IS < 0
        CursorY% = 0
      CASE IS > ((VGAHeight% / 5) - 1)
        CursorY% = ((VGAHeight% / 5) - 1)
    END SELECT
    Count% = 1
    DO
      Parse$ = MID$(DUMMY$, Count%, 1)
      Count% = Count% + 1
      FOR CountY% = 1 TO 5
        FOR CountX% = 1 TO 5
          SELECT CASE CharSet(ASC(Parse$), CountY%, CountX%)
            CASE IS = 1
              PSETX ((CursorX% * 5) + (CountX% - 1)), ((CursorY% * 5) + (CountY% - 1)), CurrentColour%
          END SELECT
      NEXT CountX%, CountY%
      CursorX% = CursorX% + 1
      SELECT CASE CursorX%
        CASE IS > ((VGAWidth% / 5) - 1)
          CursorX% = 0
          CursorY% = CursorY% + 1
      END SELECT
      SELECT CASE CursorY%
        CASE IS > ((VGAHeight% / 5) - 1)
          CursorX% = ((VGAWidth% / 5) - 1)
          CursorY% = ((VGAHeight% / 5) - 1)
      END SELECT
    LOOP UNTIL Count% > LEN(DUMMY$)
END SELECT
END SUB

FUNCTION inport$ (Addr&)
WORD& = ((INP(Addr& + 1) * 256) + INP(Addr&))
inport$ = HEX$(WORD&)
END FUNCTION

SUB LoadBG (filename$, headersize%)
BGFile% = FREEFILE
OPEN filename$ FOR BINARY AS #BGFile%
header$ = SPACE$(headersize%)
GET #BGFile%, , header$
BGWidth% = ((ASC(LEFT$(header$, 1)) * 256) + ASC(MID$(header$, 2, 1)))
BGHeight% = ((ASC(MID$(header$, 3, 1)) * 256) + ASC(RIGHT$(header$, 1)))
BGSize& = (CLNG(BGWidth%) * BGHeight%)
End32K% = (32767 - (32767 MOD BGWidth%))
End4K% = (4096 - (4096 MOD BGWidth%))

'The file being read should not exceed 262,144 bytes

'Some of the redimming below are not really needed.  They are
'placed there to free memory (if possible).

'The last Counter& command in each CASE are not needed either.
'They are there in case future commands in this sub-routine
'accesses Counter&.

SELECT CASE ((BGSize& - 1) \ 32767)
  CASE IS = 0
    REDIM Background0(0 TO 0) AS Custom32K
    REDIM Background1(0 TO 0) AS Custom32K
    REDIM Background2(0 TO 0) AS Custom32K
    REDIM Background3(0 TO 0) AS Custom32K
    REDIM Background4(0 TO 0) AS Custom4K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
  CASE IS = 1
    REDIM Background0(0 TO 1) AS Custom32K
    REDIM Background1(0 TO 0) AS Custom32K
    REDIM Background2(0 TO 0) AS Custom32K
    REDIM Background3(0 TO 0) AS Custom32K
    REDIM Background4(0 TO 0) AS Custom4K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background0(1).Each32K
    Counter& = End32K% + Counter&
  CASE IS = 2
    REDIM Background0(0 TO 1) AS Custom32K
    REDIM Background1(0 TO 0) AS Custom32K
    REDIM Background2(0 TO 0) AS Custom32K
    REDIM Background3(0 TO 0) AS Custom32K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background0(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(0).Each32K
    Counter& = End32K% + Counter&
  CASE IS = 3
    REDIM Background0(0 TO 1) AS Custom32K
    REDIM Background1(0 TO 1) AS Custom32K
    REDIM Background2(0 TO 0) AS Custom32K
    REDIM Background3(0 TO 0) AS Custom32K
    REDIM Background4(0 TO 0) AS Custom4K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background0(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(1).Each32K
    Counter& = End32K% + Counter&
  CASE IS = 4
    REDIM Background0(0 TO 1) AS Custom32K
    REDIM Background1(0 TO 1) AS Custom32K
    REDIM Background2(0 TO 0) AS Custom32K
    REDIM Background3(0 TO 0) AS Custom32K
    REDIM Background4(0 TO 0) AS Custom4K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background0(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(0).Each32K
    Counter& = End32K% + Counter&
  CASE IS = 5
    REDIM Background0(0 TO 1) AS Custom32K
    REDIM Background1(0 TO 1) AS Custom32K
    REDIM Background2(0 TO 1) AS Custom32K
    REDIM Background3(0 TO 0) AS Custom32K
    REDIM Background4(0 TO 0) AS Custom4K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background0(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(1).Each32K
    Counter& = End32K% + Counter&
  CASE IS = 6
    REDIM Background0(0 TO 1) AS Custom32K
    REDIM Background1(0 TO 1) AS Custom32K
    REDIM Background2(0 TO 1) AS Custom32K
    REDIM Background3(0 TO 0) AS Custom32K
    REDIM Background4(0 TO 0) AS Custom4K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background0(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background3(0).Each32K
    Counter& = End32K% + Counter&
  CASE IS = 7
    REDIM Background0(0 TO 1) AS Custom32K
    REDIM Background1(0 TO 1) AS Custom32K
    REDIM Background2(0 TO 1) AS Custom32K
    REDIM Background3(0 TO 1) AS Custom32K
    REDIM Background4(0 TO 0) AS Custom4K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background0(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background3(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background3(1).Each32K
    Counter& = End32K% + Counter&
  CASE IS = 8
    REDIM Background0(0 TO 1) AS Custom32K
    REDIM Background1(0 TO 1) AS Custom32K
    REDIM Background2(0 TO 1) AS Custom32K
    REDIM Background3(0 TO 1) AS Custom32K
    REDIM Background4(0 TO 0) AS Custom4K
    GET #BGFile%, (Counter& + headersize% + 1), Background0(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background0(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background1(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background2(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background3(0).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background3(1).Each32K
    Counter& = End32K% + Counter&
    GET #BGFile%, (Counter& + headersize% + 1), Background4(0).Each4K
    Counter& = End4K% + Counter&
END SELECT
CLOSE #BGFile%

END SUB

SUB LoadCharSet
CharacterSet$ = SPACE$(5 * 5 * 256)
FFile% = FREEFILE
OPEN "5x5Chars.Map" FOR BINARY AS FFile%
GET #FFile%, , CharacterSet$
CLOSE #FFile%
FOR CountC% = 0 TO 255
  FOR CountY% = 1 TO 5
    FOR CountX% = 1 TO 5
      CharSet(CountC%, CountY%, CountX%) = ASC(MID$(CharacterSet$, ((CountC% * 25) + ((CountY% - 1) * 5) + CountX%), 1))
NEXT CountX%, CountY%, CountC%
END SUB

SUB LoadSprites (filename$, SpriteNum%)
'All Sprite files are in GN2 format
SpriteFile% = FREEFILE
OPEN filename$ FOR BINARY AS #SpriteFile%
headersize% = 3
header$ = SPACE$(headersize%)
GET #SpriteFile%, , header$
TotalFrames% = ASC(LEFT$(header$, 1))
SELECT CASE TotalFrames%
  CASE IS > 15
    TotalFrames% = 15
END SELECT
SpriteWidth% = ASC(MID$(header$, 2, 1))
SpriteHeight% = ASC(RIGHT$(header$, 1))
SpriteSize% = (SpriteWidth% * SpriteHeight%)
OneLessThanTF% = (TotalFrames% - 1)

SpriteNum% = -1 'Assume no sprites are currently available
FOR Count% = 0 TO 7
  SELECT CASE SpriteLoaded(Count%)
    CASE IS = 0
      SpriteNum% = Count%
      EXIT FOR
  END SELECT
NEXT Count%
SELECT CASE SpriteNum%
  CASE IS = 0
    REDIM Sprite0(0 TO OneLessThanTF%) AS Custom4K
    FOR FrameNumber% = 0 TO OneLessThanTF%
      GET #SpriteFile%, (Counter& + headersize% + 1), Sprite0(FrameNumber%).Each4K
      Counter& = SpriteSize% + Counter&
    NEXT FrameNumber%
  CASE IS = 1
    REDIM Sprite1(0 TO OneLessThanTF%) AS Custom4K
    FOR FrameNumber% = 0 TO OneLessThanTF%
      GET #SpriteFile%, (Counter& + headersize% + 1), Sprite1(FrameNumber%).Each4K
      Counter& = SpriteSize% + Counter&
    NEXT FrameNumber%
  CASE IS = 2
    REDIM Sprite2(0 TO OneLessThanTF%) AS Custom4K
    FOR FrameNumber% = 0 TO OneLessThanTF%
      GET #SpriteFile%, (Counter& + headersize% + 1), Sprite2(FrameNumber%).Each4K
      Counter& = SpriteSize% + Counter&
    NEXT FrameNumber%
  CASE IS = 3
    REDIM Sprite3(0 TO OneLessThanTF%) AS Custom4K
    FOR FrameNumber% = 0 TO OneLessThanTF%
      GET #SpriteFile%, (Counter& + headersize% + 1), Sprite3(FrameNumber%).Each4K
      Counter& = SpriteSize% + Counter&
    NEXT FrameNumber%
  CASE IS = 4
    REDIM Sprite4(0 TO OneLessThanTF%) AS Custom4K
    FOR FrameNumber% = 0 TO OneLessThanTF%
      GET #SpriteFile%, (Counter& + headersize% + 1), Sprite4(FrameNumber%).Each4K
      Counter& = SpriteSize% + Counter&
    NEXT FrameNumber%
  CASE IS = 5
    REDIM Sprite5(0 TO OneLessThanTF%) AS Custom4K
    FOR FrameNumber% = 0 TO OneLessThanTF%
      GET #SpriteFile%, (Counter& + headersize% + 1), Sprite5(FrameNumber%).Each4K
      Counter& = SpriteSize% + Counter&
    NEXT FrameNumber%
  CASE IS = 6
    REDIM Sprite6(0 TO OneLessThanTF%) AS Custom4K
    FOR FrameNumber% = 0 TO OneLessThanTF%
      GET #SpriteFile%, (Counter& + headersize% + 1), Sprite6(FrameNumber%).Each4K
      Counter& = SpriteSize% + Counter&
    NEXT FrameNumber%
  CASE IS = 7
    REDIM Sprite7(0 TO OneLessThanTF%) AS Custom4K
    FOR FrameNumber% = 0 TO OneLessThanTF%
      GET #SpriteFile%, (Counter& + headersize% + 1), Sprite7(FrameNumber%).Each4K
      Counter& = SpriteSize% + Counter&
    NEXT FrameNumber%
END SELECT

CLOSE #SpriteFile%

SELECT CASE SpriteNum%
  CASE 0 TO 7
    SpriteLoaded(SpriteNum%) = 1
    MaxSpriteFrame(SpriteNum%) = OneLessThanTF%
    SpriteWidth(SpriteNum%) = SpriteWidth%
    SpriteHeight(SpriteNum%) = SpriteHeight%
END SELECT
END SUB

SUB memset (Segment&, Addr&, BYTE%, Size&)
DEF SEG = Segment&
FOR Count& = 1 TO Size&
  POKE Addr& + (Count& - 1), BYTE%
NEXT Count&
DEF SEG
END SUB

SUB ObjectCopy (FromObject%, ToObject%)

END SUB

SUB outport (Addr&, WORD&)
OUT Addr&, (WORD& MOD 256)
OUT Addr& + 1, (WORD& \ 256)
END SUB

SUB PageCopy (FromPage%, ToPage%)
ToPage% = ToPage% MOD TotalPages%
FromPage% = FromPage% MOD TotalPages%
VGAHeight& = VGAHeight% 'This prevents an overflow error
ToPageStart& = (ToPage% * VGAWidthBytes% * CLNG(VGAHeight%))
FromPageStart& = (FromPage% * VGAWidthBytes% * CLNG(VGAHeight))
CountEnd& = ((VGAWidthBytes% * VGAHeight&) - 1)

OUT &H3C4, 2
OUT &H3CE, 4

DEF SEG = &HA000&

OUT &H3C5, 1
OUT &H3CF, 0
FOR Count& = 0 TO CountEnd&
  POKE (Count& + ToPageStart&), PEEK(Count& + FromPageStart&)
NEXT Count&

OUT &H3C5, 2
OUT &H3CF, 1
FOR Count& = 0 TO CountEnd&
  POKE (Count& + ToPageStart&), PEEK(Count& + FromPageStart&)
NEXT Count&

OUT &H3C5, 4
OUT &H3CF, 2
FOR Count& = 0 TO CountEnd&
  POKE (Count& + ToPageStart&), PEEK(Count& + FromPageStart&)
NEXT Count&

OUT &H3C5, 8
OUT &H3CF, 3
FOR Count& = 0 TO CountEnd&
  POKE (Count& + ToPageStart&), PEEK(Count& + FromPageStart&)
NEXT Count&

DEF SEG

END SUB

SUB PAGEFLIP
SetVisiblePage ActivePage%
SetActivePage (ActivePage% + 1)
END SUB

SUB PSETX (XCord%, YCord%, PixelColor%)
'Each address accesses four neighboring pixels, so set
'Write Plane Enable according to which pixel we want
'to modify.  The plane is determined by the two least
'significant bits of the x-coordinate:
OUT &H3C4, 2
OUT &H3C5, (2 ^ (XCord% AND 3))

'The offset of the pixel into the video segment is
'offset = (width * y + x) / 4, and write the given
'color to the plane we selected above.  Heed the active
'page start selection.
VGAWidthBytes& = VGAWidthBytes% 'This is quicker than calling up CLNG
                                'within the FOR/NEXT loops
AfterActiveEnd& = ActiveStart& + (VGAWidthBytes& * VGAHeight%)
Address& = ((VGAWidthBytes% * YCord%) + (XCord% \ 4) + ActiveStart&)
DEF SEG = &HA000&
SELECT CASE Address&
  CASE IS < AfterActiveEnd&
    POKE Address&, PixelColor%
END SELECT
DEF SEG
END SUB

SUB PUTBG
YIncrement32K% = ((32767 - (32767 MOD BGWidth%)) / BGWidth%)
YIncrement4K% = ((4096 - (4096 MOD BGWidth%)) / BGWidth%)
BGBlocks% = (BGSize& \ 32767)

SELECT CASE BGBlocks%
  CASE 0
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
  CASE 1
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
    PUTX 0, YIncrement32K%, BGWidth%, YIncrement32K%, Background0(1).Each32K, 1
  CASE 2
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
    PUTX 0, YIncrement32K%, BGWidth%, YIncrement32K%, Background0(1).Each32K, 1
    PUTX 0, (2 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(0).Each32K, 1
  CASE 3
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
    PUTX 0, YIncrement32K%, BGWidth%, YIncrement32K%, Background0(1).Each32K, 1
    PUTX 0, (2 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(0).Each32K, 1
    PUTX 0, (3 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(1).Each32K, 1
  CASE 4
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
    PUTX 0, YIncrement32K%, BGWidth%, YIncrement32K%, Background0(1).Each32K, 1
    PUTX 0, (2 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(0).Each32K, 1
    PUTX 0, (3 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(1).Each32K, 1
    PUTX 0, (4 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(0).Each32K, 1
  CASE 5
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
    PUTX 0, YIncrement32K%, BGWidth%, YIncrement32K%, Background0(1).Each32K, 1
    PUTX 0, (2 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(0).Each32K, 1
    PUTX 0, (3 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(1).Each32K, 1
    PUTX 0, (4 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(0).Each32K, 1
    PUTX 0, (5 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(1).Each32K, 1
  CASE 6
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
    PUTX 0, YIncrement32K%, BGWidth%, YIncrement32K%, Background0(1).Each32K, 1
    PUTX 0, (2 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(0).Each32K, 1
    PUTX 0, (3 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(1).Each32K, 1
    PUTX 0, (4 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(0).Each32K, 1
    PUTX 0, (5 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(1).Each32K, 1
    PUTX 0, (6 * YIncrement32K%), BGWidth%, YIncrement32K%, Background3(0).Each32K, 1
  CASE 7
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
    PUTX 0, YIncrement32K%, BGWidth%, YIncrement32K%, Background0(1).Each32K, 1
    PUTX 0, (2 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(0).Each32K, 1
    PUTX 0, (3 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(1).Each32K, 1
    PUTX 0, (4 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(0).Each32K, 1
    PUTX 0, (5 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(1).Each32K, 1
    PUTX 0, (6 * YIncrement32K%), BGWidth%, YIncrement32K%, Background3(0).Each32K, 1
    PUTX 0, (7 * YIncrement32K%), BGWidth%, YIncrement32K%, Background3(1).Each32K, 1
  CASE 8
    PUTX 0, 0, BGWidth%, YIncrement32K%, Background0(0).Each32K, 1
    PUTX 0, YIncrement32K%, BGWidth%, YIncrement32K%, Background0(1).Each32K, 1
    PUTX 0, (2 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(0).Each32K, 1
    PUTX 0, (3 * YIncrement32K%), BGWidth%, YIncrement32K%, Background1(1).Each32K, 1
    PUTX 0, (4 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(0).Each32K, 1
    PUTX 0, (5 * YIncrement32K%), BGWidth%, YIncrement32K%, Background2(1).Each32K, 1
    PUTX 0, (6 * YIncrement32K%), BGWidth%, YIncrement32K%, Background3(0).Each32K, 1
    PUTX 0, (7 * YIncrement32K%), BGWidth%, YIncrement32K%, Background3(1).Each32K, 1
    PUTX 0, (8 * YIncrement4K%), BGWidth%, YIncrement4K%, Background4(0).Each4K, 1
END SELECT
END SUB

SUB PUTX (XCord%, YCord%, xsize%, ysize%, Buffer$, UseZero%)

'If the following address position checks work correctly,
'this sub-routine will not write to a video page other
'than the current active page.

DEF SEG = &HA000&
OUT &H3C4, 2
VGAWidthBytes& = VGAWidthBytes% 'This is quicker than calling up CLNG
                                'within the FOR/NEXT loops

AfterActiveEnd& = ActiveStart& + (VGAWidthBytes& * VGAHeight%)

OUT &H3C5, (2 ^ (XCord% AND 3))
'The calculations below are the causes for the slowness
'of the PUTX sub-routine.
FOR CountY% = 0 TO (ysize% - 1)
  FOR CountX% = 0 TO (xsize% - 1) STEP 4
    PixelColor% = ASC(MID$(Buffer$, ((CountY% * xsize%) + (CountX% + 1)), 1))
    Address& = ((VGAWidthBytes& * (YCord% + CountY%)) + ((XCord% + CountX%) \ 4) + ActiveStart&)
    SELECT CASE PixelColor%
      CASE IS = 0
        SELECT CASE UseZero%
          CASE IS = 1
            SELECT CASE Address&
              CASE IS < AfterActiveEnd&
                POKE Address&, PixelColor%
            END SELECT
        END SELECT
      CASE ELSE
        SELECT CASE Address&
          CASE IS < AfterActiveEnd&
            POKE Address&, PixelColor%
        END SELECT
    END SELECT
NEXT CountX%, CountY%

OUT &H3C5, (2 ^ ((XCord% + 1) AND 3))
'The calculations below are the causes for the slowness
'of the PUTX sub-routine.
FOR CountY% = 0 TO (ysize% - 1)
  FOR CountX% = 1 TO (xsize% - 1) STEP 4
    PixelColor% = ASC(MID$(Buffer$, ((CountY% * xsize%) + (CountX% + 1)), 1))
    Address& = ((VGAWidthBytes& * (YCord% + CountY%)) + ((XCord% + CountX%) \ 4) + ActiveStart&)
    SELECT CASE PixelColor%
      CASE IS = 0
        SELECT CASE UseZero%
          CASE IS = 1
            SELECT CASE Address&
              CASE IS < AfterActiveEnd&
                POKE Address&, PixelColor%
            END SELECT
        END SELECT
      CASE ELSE
        SELECT CASE Address&
          CASE IS < AfterActiveEnd&
            POKE Address&, PixelColor%
        END SELECT
    END SELECT
NEXT CountX%, CountY%

OUT &H3C5, (2 ^ ((XCord% + 2) AND 3))
'The calculations below are the causes for the slowness
'of the PUTX sub-routine.
FOR CountY% = 0 TO (ysize% - 1)
  FOR CountX% = 2 TO (xsize% - 1) STEP 4
    PixelColor% = ASC(MID$(Buffer$, ((CountY% * xsize%) + (CountX% + 1)), 1))
    Address& = ((VGAWidthBytes& * (YCord% + CountY%)) + ((XCord% + CountX%) \ 4) + ActiveStart&)
    SELECT CASE PixelColor%
      CASE IS = 0
        SELECT CASE UseZero%
          CASE IS = 1
            SELECT CASE Address&
              CASE IS < AfterActiveEnd&
                POKE Address&, PixelColor%
            END SELECT
        END SELECT
      CASE ELSE
        SELECT CASE Address&
          CASE IS < AfterActiveEnd&
            POKE Address&, PixelColor%
        END SELECT
    END SELECT
NEXT CountX%, CountY%

OUT &H3C5, (2 ^ ((XCord% + 3) AND 3))
'The calculations below are the causes for the slowness
'of the PUTX sub-routine.
FOR CountY% = 0 TO (ysize% - 1)
  FOR CountX% = 3 TO (xsize% - 1) STEP 4
    PixelColor% = ASC(MID$(Buffer$, ((CountY% * xsize%) + (CountX% + 1)), 1))
    Address& = ((VGAWidthBytes& * (YCord% + CountY%)) + ((XCord% + CountX%) \ 4) + ActiveStart&)
    SELECT CASE PixelColor%
      CASE IS = 0
        SELECT CASE UseZero%
          CASE IS = 1
            SELECT CASE Address&
              CASE IS < AfterActiveEnd&
                POKE Address&, PixelColor%
            END SELECT
        END SELECT
      CASE ELSE
        SELECT CASE Address&
          CASE IS < AfterActiveEnd&
            POKE Address&, PixelColor%
        END SELECT
    END SELECT
NEXT CountX%, CountY%

DEF SEG
END SUB

SUB ReadyFrame (Object%, Repeat%, Direction%, SX%, SY%, EX%, EY%)
ObjectUseCount(Object%) = ObjectUseCount(Object%) + 1
SpriteNum% = Object(Object%)
ObjectLock(Object%) = 1
ObjectRepeat(Object%) = ObjectRepeat(Object%) + 1
SELECT CASE MaxFrameNumber(Object%)
  CASE IS = 0
    Direction% = 0
END SELECT
SELECT CASE ObjectRepeat(Object%)
  CASE IS > Repeat%
    ObjectRepeat(Object%) = 0
  CASE ELSE
    CurrentFrame(Object%) = LastFrame(Object%)
    ObjectUseCount(Object%) = ObjectLastCount(Object%)
END SELECT
SELECT CASE ObjectUseCount(Object%)
  CASE IS < 0
    Numerator% = 0
  CASE ELSE
    Numerator% = ObjectUseCount(Object%) + 1
END SELECT
SELECT CASE Direction%
  CASE IS <= 0 'One frame
    NewFrame% = ABS(Direction%)
    ObjectLock(Object%) = 0
    DrawFrame SpriteNum%, NewFrame%, SX%, SY%
    ObjectLock(Object%) = 0
  CASE IS = 1 'Forward
    NewFrame% = CurrentFrame(Object%) + 1
    SELECT CASE NewFrame%
      CASE IS = MaxFrameNumber(SpriteNum%)
        XCord% = (SX% + ((Numerator% / MaxFrameNumber(Object%)) * (EX% - SX%)))
        YCord% = (SY% + ((Numerator% / MaxFrameNumber(Object%)) * (EY% - SY%)))
        DrawFrame SpriteNum%, NewFrame%, XCord%, YCord%
        ObjectLock(Object%) = 2
      CASE IS < (MaxFrameNumber(SpriteNum%))
        XCord% = (SX% + ((Numerator% / MaxFrameNumber(Object%)) * (EX% - SX%)))
        YCord% = (SY% + ((Numerator% / MaxFrameNumber(Object%)) * (EY% - SY%)))
        DrawFrame SpriteNum%, NewFrame%, XCord%, YCord%
      CASE ELSE
        ObjectLock(Object%) = 0
    END SELECT
  CASE IS = 2 'Backward
    SELECT CASE CurrentFrame(Object%)
      CASE IS = -1
        CurrentFrame(Object%) = MaxFrameNumber(Object%) + 1
    END SELECT
    NewFrame% = CurrentFrame(Object%) - 1
    SELECT CASE NewFrame%
      CASE IS > 0
        XCord% = (SX% + ((Numerator% / MaxFrameNumber(Object%)) * (EX% - SX%)))
        YCord% = (SY% + ((Numerator% / MaxFrameNumber(Object%)) * (EY% - SY%)))
        DrawFrame SpriteNum%, NewFrame%, XCord%, YCord%
      CASE IS = 0
        XCord% = (SX% + ((Numerator% / MaxFrameNumber(Object%)) * (EX% - SX%)))
        YCord% = (SY% + ((Numerator% / MaxFrameNumber(Object%)) * (EY% - SY%)))
        DrawFrame SpriteNum%, NewFrame%, XCord%, YCord%
        ObjectLock(Object%) = 2
      CASE ELSE
        ObjectLock(Object%) = 0
    END SELECT
END SELECT
SELECT CASE ObjectLock(Object%)
  CASE IS = 1
    LastFrame(Object%) = CurrentFrame(Object%)
    CurrentFrame(Object%) = NewFrame%
    ObjectLastCount(Object%) = ObjectUseCount(Object%)
  CASE IS = 2
    LastFrame(Object%) = CurrentFrame(Object%)
    CurrentFrame(Object%) = NewFrame%
    ObjectLastCount(Object%) = ObjectUseCount(Object%)
    SELECT CASE ObjectRepeat(Object%)
      CASE IS >= Repeat%
        CurrentFrame(Object%) = -1
        LastFrame(Object%) = -1
        ObjectUseCount(Object%) = -1
        ObjectLastCount(Object%) = -1
        ObjectRepeat(Object%) = -1
        ObjectSX(Object%) = 0
        ObjectEX(Object%) = 0
        ObjectSY(Object%) = 0
        ObjectEY(Object%) = 0
    END SELECT
  CASE ELSE
    CurrentFrame(Object%) = -1
    LastFrame(Object%) = -1
    ObjectUseCount(Object%) = -1
    ObjectLastCount(Object%) = -1
    ObjectRepeat(Object%) = -1
    ObjectSX(Object%) = 0
    ObjectEX(Object%) = 0
    ObjectSY(Object%) = 0
    ObjectEY(Object%) = 0
END SELECT
END SUB

SUB RGBLoad
RGBFile% = FREEFILE
OPEN "RGB.PAL" FOR BINARY AS #RGBFile%
RGBPalette$ = SPACE$(768)
GET #RGBFile%, , RGBPalette$
CLOSE #RGBFile%
DIM RGBPal(0 TO 255) AS LONG
FOR Count% = 0 TO 255
  Red% = (ASC(MID$(RGBPalette$, ((Count% * 3) + 1), 1)) \ 4)
  Green% = (ASC(MID$(RGBPalette$, ((Count% * 3) + 2), 1)) \ 4)
  Blue% = (ASC(MID$(RGBPalette$, ((Count% * 3) + 3), 1)) \ 4)
  RGBPal(Count%) = (65536 * Blue% + 256 * Green% + Red%)
NEXT Count%
PALETTE USING RGBPal
END SUB

SUB RGBSave
RGBFile% = FREEFILE
OPEN "RGB.PAL" FOR BINARY AS #RGBFile%
PRINT "Saving";
FOR CountA% = 0 TO 1
  FOR CountB% = 0 TO 1
    FOR CountC% = 0 TO 1
      FOR CountD% = 0 TO 1
        FOR CountE% = 0 TO 1
          FOR CountF% = 0 TO 1
            FOR CountG% = 0 TO 1
              FOR CountH% = 0 TO 1
                FirstColor% = 0
                SecondColor% = 0
                ThirdColor% = 0
                PaletteColor% = 0
                SELECT CASE CountH%
                  CASE IS = 1
                    FirstColor% = FirstColor% + (2 ^ 0)
                END SELECT
                SELECT CASE CountG%
                  CASE IS = 1
                    FirstColor% = FirstColor% + (2 ^ 1)
                END SELECT
                SELECT CASE CountF%
                  CASE 1
                    FirstColor% = FirstColor% + (2 ^ 2)
                END SELECT
                SELECT CASE CountE%
                  CASE 1
                    SecondColor% = SecondColor% + (2 ^ 0)
                END SELECT
                SELECT CASE CountD%
                  CASE 1
                    SecondColor% = SecondColor% + (2 ^ 1)
                END SELECT
                SELECT CASE CountC%
                  CASE IS = 1
                    ThirdColor% = ThirdColor% + (2 ^ 0)
                END SELECT
                SELECT CASE CountB%
                  CASE IS = 1
                    ThirdColor% = ThirdColor% + (2 ^ 1)
                END SELECT
                SELECT CASE CountA%
                  CASE 1
                    ThirdColor% = ThirdColor% + (2 ^ 2)
                END SELECT
                Red% = ((ThirdColor% * 8) / (7 * 8) * 63)
                'Switch ThirdColor% above with FirstColor%
                'below for RGB mode.  Otherwise, it is
                'currently in BGR mode.
                Green% = ((SecondColor% * 16) / (3 * 16) * 63)
                Blue% = ((FirstColor% * 8) / (7 * 8) * 63)
                Red$ = CHR$(Red% * 4)
                Green$ = CHR$(Green% * 4)
                Blue$ = CHR$(Blue% * 4)
                PUT #RGBFile%, , Red$
                PUT #RGBFile%, , Green$
                PUT #RGBFile%, , Blue$
                SELECT CASE (((((CountA%) * 128) + ((CountB%) * 64) + ((CountC%) * 32) + ((CountD%) * 16) + ((CountE%) * 8) + ((CountF%) * 4) + ((CountG%) * 2) + CountH%) + 1) MOD 8)
                  CASE 0
                    PRINT ".";
                END SELECT
NEXT CountH%, CountG%, CountF%, CountE%, CountD%, CountC%, CountB%, CountA%
CLOSE #RGBFile%
END SUB

'Shift byte to the left
FUNCTION SBL& (DWORD&, Shifter%)
Number& = DWORD& * (2 ^ Shifter%)
SELECT CASE Number&
  CASE IS > 65535
    Number& = BIND&(RIGHT$(BIN$(Number&), LEN(BIN$(Number&)) - 1))
END SELECT
SBL& = Number&
END FUNCTION

'Shift byte to the right
FUNCTION SBR& (DWORD&, Shifter%)
SBR& = DWORD& \ (2 ^ Shifter%)
END FUNCTION

'
'setXXXPage() sets the specified page by multiplying the page number
'with the size of one page at the current resolution, then handing the
'resulting offset value over to the corresponding setXXXStart()
'function.  The first page is number 0.
'
SUB SetActivePage (PAGE%)
PAGE% = PAGE% MOD TotalPages%
SELECT CASE UseReservedPage%
  CASE IS = 1
    SELECT CASE PAGE%
      CASE IS = ReservedPage%
        PAGE% = (PAGE% + 1) MOD TotalPages%
        SELECT CASE PAGE%
          CASE IS = ReservedPage% 'This mode must only
            UseReservedPage% = 0  'have one page
          CASE ELSE
            SELECT CASE PAGE%
              CASE IS = VisiblePage%                'This mode must
                UseReservedPage% = 0                'only have two
                PAGE% = (PAGE% + 1) MOD TotalPages% 'pages
            END SELECT
        END SELECT
    END SELECT
END SELECT
ActivePage% = PAGE%
PAGE& = PAGE% 'The use of Page& prevents an overflow error.
SetActiveStart (PAGE& * VGAWidthBytes% * VGAHeight%)
END SUB

'
'SetActiveStart tells our graphics operations which address
'in video memory should be considered the top left corner.
'
SUB SetActiveStart (offset&)
ActiveStart& = offset&
END SUB

SUB SetObject (Object%, Repeat%, Direction%, SX%, SY%, EX%, EY%)
SpriteNum% = Object(Object%)
SELECT CASE ObjectLock(Object%)
  CASE IS = 0
    SELECT CASE (EX% + SpriteWidth(SpriteNum%))
      CASE IS > VGAWidth%
        EX% = VGAWidth% - SpriteWidth(SpriteNum%)
    END SELECT
    SELECT CASE (EY% + SpriteHeight(SpriteNum%))
      CASE IS > VGAHeight%
        EY% = VGAHeight% - SpriteHeight(SpriteNum%)
    END SELECT
    ObjectRepeat(Object%) = -1
    ObjectSX(Object%) = SX%
    ObjectEX(Object%) = EX%
    ObjectSY(Object%) = SY%
    ObjectEY(Object%) = EY%
    ObjectLock(SpriteNum%) = 1
    MaxFrameNumber(Object%) = MaxSpriteFrame(SpriteNum%)
    CurrentFrame(Object%) = -1
    LastFrame(Object%) = -1
    ObjectUseCount(Object%) = -1
    ObjectLastCount(Object%) = -1
END SELECT
ReadyFrame Object%, Repeat%, Direction%, SX%, SY%, EX%, EY%
END SUB

'
'setXXXPage() sets the specified page by multiplying the page number
'with the size of one page at the current resolution, then handing the
'resulting offset value over to the corresponding setXXXStart()
'function.  The first page is number 0.
'
SUB SetVisiblePage (PAGE%)
PAGE% = PAGE% MOD TotalPages%
VisiblePage% = PAGE%
PAGE& = PAGE% 'The use of Page& prevents an overflow error.
SetVisibleStart (PAGE& * VGAWidthBytes% * VGAHeight%)
END SUB

'
'SetVisibleStart tells the VGA from which byte to fetch the first
'pixel when starting refresh at the top of the screen.
'
SUB SetVisibleStart (offset&)
VisibleStart& = offset&

'Here's the WaitRetrace routine
WaitRetrace

outport &H3D4, &HC  'Set high byte
outport &H3D5, SBR(VisibleStart&, 8)
outport &H3D4, &HD
outport &H3D5, VisibleStart& AND &HFF
END SUB

SUB VGA (ModeName$)
SELECT CASE ModeName$
  CASE IS = "256x200x256"
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE3        'Dot clock
    outport &H3D4, &H5F00  'Horizontal total
    outport &H3D4, &H3F01  'Horizontal displayed
    outport &H3D4, &H4202  'Horizontal blanking start
    outport &H3D4, &H9F03& 'Horizontal blanking end
    outport &H3D4, &H4C04  'Horizontal sync/retrace start
    outport &H3D4, &H5     'Horizontal sync/retrace end
    outport &H3D4, &H2013  'Offset/logical width
    VGAWidth% = 256
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "256x200x256"
 
  CASE IS = "256x224x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE3        'Dot clock
    outport &H3D4, &H5F00  'Horizontal total
    outport &H3D4, &H3F01  'Horizontal displayed
    outport &H3D4, &H4002  'Horizontal blanking start
    outport &H3D4, &H8203& 'Horizontal blanking end
    outport &H3D4, &H4A04  'Horizontal sync/retrace start
    outport &H3D4, &H9A05& 'Horizontal sync/retrace end
    outport &H3D4, &HB06   'Vertical total
    outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4109  'Maximum scanline/character height
    outport &H3D4, &HDA10& 'Vertical sync/retrace start
    outport &H3D4, &H9C11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &HBF12& 'Vertical displayed
    outport &H3D4, &H2013  'Offset/logical width
    outport &H3D4, &HC715& 'Vertical blanking start
    outport &H3D4, &H416   'Vertical blanking end
    VGAWidth% = 256
    VGAHeight% = 224
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "256x224x256"

  CASE IS = "256x240x256"
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE3        'Dot clock
    outport &H3D4, &H5F00  'Horizontal total
    outport &H3D4, &H3F01  'Horizontal displayed
    outport &H3D4, &H4202  'Horizontal blanking start
    outport &H3D4, &H9F03& 'Horizontal blanking end
    outport &H3D4, &H4C04  'Horizontal sync/retrace start
    outport &H3D4, &H5     'Horizontal sync/retrace end
    outport &H3D4, &HD06   'Vertical total
    outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4109  'Maximum scanline/character height
    outport &H3D4, &HEA10& 'Vertical sync/retrace start
    outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &HDF12& 'Vertical displayed
    outport &H3D4, &H2013  'Offset/logical width
    outport &H3D4, &HE715& 'Vertical blanking start
    outport &H3D4, &H616   'Vertical blanking end
    VGAWidth% = 256
    VGAHeight% = 240
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "256x240x256"
 
  CASE IS = "256x256x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE3        'Dot clock
    outport &H3D4, &H5F00  'Horizontal total
    outport &H3D4, &H3F01  'Horizontal displayed
    outport &H3D4, &H4002  'Horizontal blanking start
    outport &H3D4, &H8203& 'Horizontal blanking end
    outport &H3D4, &H4A04  'Horizontal sync/retrace start
    outport &H3D4, &H9A05& 'Horizontal sync/retrace end
    outport &H3D4, &H2306  'Vertical total
    outport &H3D4, &HB207& 'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H6109  'Maximum scanline/character height
    outport &H3D4, &HA10   'Vertical sync/retrace start
    outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &HFF12& 'Vertical displayed
    outport &H3D4, &H2013  'Offset/logical width
    outport &H3D4, &H715   'Vertical blanking start
    outport &H3D4, &H1A16& 'Vertical blanking end
    VGAWidth% = 256
    VGAHeight% = 256
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "256x256x256"

  CASE IS = "256x400x256"
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE3        'Dot clock
    outport &H3D4, &H5F00  'Horizontal total
    outport &H3D4, &H3F01  'Horizontal displayed
    outport &H3D4, &H4202  'Horizontal blanking start
    outport &H3D4, &H9F03& 'Horizontal blanking end
    outport &H3D4, &H4C04  'Horizontal sync/retrace start
    outport &H3D4, &H5     'Horizontal sync/retrace end
    outport &H3D4, &H4009  'Maximum scanline/character height
    outport &H3D4, &H2013  'Offset/logical width
    VGAWidth% = 256
    VGAHeight% = 400
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "256x400x256"

  CASE IS = "256x480x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE3        'Dot clock
    outport &H3D4, &H5F00  'Horizontal total
    outport &H3D4, &H3F01  'Horizontal displayed
    outport &H3D4, &H4202  'Horizontal blanking start
    outport &H3D4, &H9F03& 'Horizontal blanking end
    outport &H3D4, &H4C04  'Horizontal sync/retrace start
    outport &H3D4, &H5     'Horizontal sync/retrace end
    outport &H3D4, &HD06   'Vertical total
    outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4009  'Maximum scanline/character height
    outport &H3D4, &HEA10& 'Vertical sync/retrace start
    outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &HDF12& 'Vertical displayed
    outport &H3D4, &H2013  'Offset/logical width
    outport &H3D4, &HE715& 'Vertical blanking start
    outport &H3D4, &H616   'Vertical blanking end
    VGAWidth% = 256
    VGAHeight% = 480
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "256x480x256"

  CASE IS = "320x200x256"
    'Sets mode 13h, then turns it into an unchained (planar), 4-page
    '320x200x256 mode.
    SCREEN 0                          'SCREEN 0 is needed to make
    SCREEN 13                         'the command SCREEN 13 effective
                                      'because QuickBASIC will not
                                      'reset the VGA card if QuickBASIC
                                      'thinks it is still in SCREEN &H13
                                       
    RGBLoad                           'Loads the RGB palette
    LoadCharSet                       'Loads the 5x5 character set
    COLOUR 255
    outport &H3C4, &H604              'Put the chain-4 mode of sequencer off
    memset &HA000&, 0, 0, &H10000     'SCREEN 13 only clears every
                                      'fourth byte of each plane
  
    OUT &H3D4, &H11                            'Initializes the VGA to
    TempByte& = (INP(&H3D5) AND &H7F)          'accept any combination
    outport &H3D4, (&H11 OR SBL(TempByte&, 8)) 'of configuration
                                               'register settings.
  
    outport &H3C4, &H100              'Reset sequencer
    outport &H3C4, &H300              'Re-reset sequencer
    OUT &H3C0, &H20                   'Reenable display data
  
    outport &H3D4, &H2C11             'Turn off write protect
    OUT &H3C2, &HE3                   'Dot clock
    outport &H3D4, &H5F00             'Horizontal total
    outport &H3D4, &H4F01             'Horizontal displayed
    outport &H3D4, &H5002             'Horizontal blanking start
    outport &H3D4, &H8203&            'Horizontal blanking end
    outport &H3D4, &H5404             'Horizontal sync/retrace start
    outport &H3D4, &H8005&            'Horizontal sync/retrace end
    outport &H3D4, &HBF06&            'Vertical total
    outport &H3D4, &H1F07             'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H8                'Present row scan
    outport &H3D4, &H4109             'Maximum scanline/character height
    outport &H3D4, &H9C10&            'Vertical sync/retrace start
    outport &H3D4, &H8E11&            'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &H8F12&            'Vertical displayed
    outport &H3D4, &H2813             'Offset/logical width
    outport &H3D4, &H14               'Turn off CRT controller's
                                      'double-word (or long) mode
    outport &H3D4, &H9615&            'Vertical blanking start
    outport &H3D4, &HB916&            'Vertical blanking end
    outport &H3D4, &HE317&            'Turn on CRT controller's byte mode
    outport &H3D4, &H14               'Turn off CRT controller's
                                      'double-word (or long) mode
  
    'ActiveStart& specifies the start of the page being accessed by
    'drawing operations.  VisibleStart& specifies the contents of the
    'screen start register, i.e. the start of the visible page.
    'By default, we want screen refreshing and drawing operations
    'to be based at offset 0 in the video segment.
    ActiveStart& = 0
    VisibleStart& = 0

    VGAWidth% = 320
    VGAHeight% = 200
    'Each byte addresses four pixels, so the width of a scan line in
    '*bytes* is one fourth of the number of pixels on a line.
    VGAWidthBytes% = (VGAWidth% / 4)
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0                  'This sets up the PAGEFLIP sub-routine
    SetActivePage 1                   '(for use with multipage modes)
    ModeName$ = "320x200x256"
  
  CASE IS = "320x240x256"
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &HE3, &HE3         'Dot clock
    outport &H3D4, &HD06   'Vertical total
    outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4109  'Maximum scanline/character height
    outport &H3D4, &HEA10& 'Vertical sync/retrace start
    outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &HDF12& 'Vertical displayed
    outport &H3D4, &HE715& 'Vertical blanking start
    outport &H3D4, &H616   'Vertical blanking end
    VGAHeight% = 240
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "320x240x256"

  CASE IS = "320x400x256"
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE3        'Dot clock
    outport &H3D4, &H4009  'Maximum scanline/character height
    VGAWidth% = 320
    VGAHeight% = 400
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "320x400x256"

  CASE IS = "320x480x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE3        'Dot clock
    outport &H3D4, &HD06   'Vertical total
    outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4009  'Maximum scanline/character height
    outport &H3D4, &HEA10& 'Vertical sync/retrace start
    outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &HDF12& 'Vertical displayed
    outport &H3D4, &HE715& 'Vertical blanking start
    outport &H3D4, &H616   'Vertical blanking end
    VGAWidth% = 320
    VGAHeight% = 480
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This mode has less
    SetActivePage 0        'than two pages
    ModeName$ = "320x480x256"

  CASE IS = "360x200x256"
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6B00  'Horizontal total
    outport &H3D4, &H5901  'Horizontal displayed
    outport &H3D4, &H5A02  'Horizontal blanking start
    outport &H3D4, &H8E03& 'Horizontal blanking end
    outport &H3D4, &H5E04  'Horizontal sync/retrace start
    outport &H3D4, &H8A05& 'Horizontal sync/retrace end
    outport &H3D4, &H2D13  'Offset/logical width
    VGAWidth% = 360
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "360x200x256"

  CASE IS = "360x240x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6B00  'Horizontal total
    outport &H3D4, &H5901  'Horizontal displayed
    outport &H3D4, &H5A02  'Horizontal blanking start
    outport &H3D4, &H8E03& 'Horizontal blanking end
    outport &H3D4, &H5E04  'Horizontal sync/retrace start
    outport &H3D4, &H8A05& 'Horizontal sync/retrace end
    outport &H3D4, &HD06   'Vertical total
    outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4109  'Maximum scanline/character height
    outport &H3D4, &HEA10& 'Vertical sync/retrace start
    outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &HDF12& 'Vertical displayed
    outport &H3D4, &H2D13  'Offset/logical width
    outport &H3D4, &HE715& 'Vertical blanking start
    outport &H3D4, &H616   'Vertical blanking end
    VGAWidth% = 360
    VGAHeight% = 240
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "360x240x256"

  CASE IS = "360x270x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6B00  'Horizontal total
    outport &H3D4, &H5901  'Horizontal displayed
    outport &H3D4, &H5A02  'Horizontal blanking start
    outport &H3D4, &H8E03& 'Horizontal blanking end
    outport &H3D4, &H5E04  'Horizontal sync/retrace start
    outport &H3D4, &H8A05& 'Horizontal sync/retrace end
    outport &H3D4, &H3006  'Vertical total
    outport &H3D4, &HF007& 'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H6109  'Maximum scanline/character height
    outport &H3D4, &H2010  'Vertical sync/retrace start
    outport &H3D4, &HA911& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &H1B12  'Vertical displayed
    outport &H3D4, &H2D13  'Offset/logical width
    outport &H3D4, &H1F15  'Vertical blanking start
    outport &H3D4, &H2F16  'Vertical blanking end
    VGAWidth% = 360
    VGAHeight% = 270
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "360x270x256"

  CASE IS = "376x282x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6E00  'Horizontal total
    outport &H3D4, &H5D01  'Horizontal displayed
    outport &H3D4, &H5E02  'Horizontal blanking start
    outport &H3D4, &H9103& 'Horizontal blanking end
    outport &H3D4, &H6204  'Horizontal sync/retrace start
    outport &H3D4, &H8F05& 'Horizontal sync/retrace end
    outport &H3D4, &H6206  'Vertical total
    outport &H3D4, &HF007& 'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H6109  'Maximum scanline/character height
    outport &H3D4, &H3710  'Vertical sync/retrace start
    outport &H3D4, &H8911& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &H3312  'Vertical displayed
    outport &H3D4, &H2F13  'Offset/logical width
    outport &H3D4, &H3C15  'Vertical blanking start
    outport &H3D4, &H5C16  'Vertical blanking end
    VGAWidth% = 376
    VGAHeight% = 282
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "376x282x256"

  CASE IS = "376x308x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6E00  'Horizontal total
    outport &H3D4, &H5D01  'Horizontal displayed
    outport &H3D4, &H5E02  'Horizontal blanking start
    outport &H3D4, &H9103& 'Horizontal blanking end
    outport &H3D4, &H6204  'Horizontal sync/retrace start
    outport &H3D4, &H8F05& 'Horizontal sync/retrace end
    outport &H3D4, &H6206  'Vertical total
    outport &H3D4, &HF07   'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4009  'Maximum scanline/character height
    outport &H3D4, &H3710   'Vertical sync/retrace start
    outport &H3D4, &H8911& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &H3312  'Vertical displayed
    outport &H3D4, &H2F13  'Offset/logical width
    outport &H3D4, &H3C15  'Vertical blanking start
    outport &H3D4, &H5C16& 'Vertical blanking end
    VGAWidth% = 376
    VGAHeight% = 308
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "376x308x256"

  CASE IS = "360x360x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6B00  'Horizontal total
    outport &H3D4, &H5901  'Horizontal displayed
    outport &H3D4, &H5A02  'Horizontal blanking start
    outport &H3D4, &H8E03& 'Horizontal blanking end
    outport &H3D4, &H5E04  'Horizontal sync/retrace start
    outport &H3D4, &H8A05& 'Horizontal sync/retrace end
    outport &H3D4, &H4009  'Maximum scanline/character height
    outport &H3D4, &H8810& 'Vertical sync/retrace start
    outport &H3D4, &H8511& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &H6712  'Vertical displayed
    outport &H3D4, &H2D13  'Offset/logical width
    outport &H3D4, &H6D15  'Vertical blanking start
    outport &H3D4, &HBA16& 'Vertical blanking end
    VGAWidth% = 360
    VGAHeight% = 360
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "360x360x256"

  CASE IS = "360x400x256"
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6B00  'Horizontal total
    outport &H3D4, &H5901  'Horizontal displayed
    outport &H3D4, &H5A02  'Horizontal blanking start
    outport &H3D4, &H8E03& 'Horizontal blanking end
    outport &H3D4, &H5E04  'Horizontal sync/retrace start
    outport &H3D4, &H8A05& 'Horizontal sync/retrace end
    outport &H3D4, &H4009  'Maximum scanline/character height
    outport &H3D4, &H2D13  'Offset/logical width
    VGAWidth% = 360
    VGAHeight% = 400
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This mode has less
    SetActivePage 0        'than two pages
    ModeName$ = "360x400x256"

  CASE IS = "360x480x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6B00  'Horizontal total
    outport &H3D4, &H5901  'Horizontal displayed
    outport &H3D4, &H5A02  'Horizontal blanking start
    outport &H3D4, &H8E03& 'Horizontal blanking end
    outport &H3D4, &H5E04  'Horizontal sync/retrace start
    outport &H3D4, &H8A05& 'Horizontal sync/retrace end
    outport &H3D4, &HD06   'Vertical total
    outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4009  'Maximum scanline/character height
    outport &H3D4, &HEA10& 'Vertical sync/retrace start
    outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &HDF12& 'Vertical displayed
    outport &H3D4, &H2D13  'Offset/logical width
    outport &H3D4, &HE715& 'Vertical blanking start
    outport &H3D4, &H616   'Vertical blanking end
    VGAWidth% = 360
    VGAHeight% = 480
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This mode has less
    SetActivePage 0        'than two pages
    ModeName$ = "360x480x256"

  CASE IS = "376x564x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H6E00  'Horizontal total
    outport &H3D4, &H5D01  'Horizontal displayed
    outport &H3D4, &H5E02  'Horizontal blanking start
    outport &H3D4, &H9103& 'Horizontal blanking end
    outport &H3D4, &H6204  'Horizontal sync/retrace start
    outport &H3D4, &H8F05& 'Horizontal sync/retrace end
    outport &H3D4, &H6206  'Vertical total
    outport &H3D4, &HF007& 'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H6009  'Maximum scanline/character height
    outport &H3D4, &H3710  'Vertical sync/retrace start
    outport &H3D4, &H8911& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &H3312  'Vertical displayed
    outport &H3D4, &H2F13  'Offset/logical width
    outport &H3D4, &H3C15  'Vertical blanking start
    outport &H3D4, &H5C16  'Vertical blanking end
    VGAWidth% = 376
    VGAHeight% = 564
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This mode has less
    SetActivePage 0        'than two pages
    ModeName$ = "376x564x256"

  CASE IS = "400x300x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HA7        'Dot clock
    outport &H3D4, &H7100  'Horizontal total
    outport &H3D4, &H6301  'Horizontal displayed
    outport &H3D4, &H6402  'Horizontal blanking start
    outport &H3D4, &H9203& 'Horizontal blanking end
    outport &H3D4, &H6504  'Horizontal sync/retrace start
    outport &H3D4, &H8205& 'Horizontal sync/retrace end
    outport &H3D4, &H4606  'Vertical total
    outport &H3D4, &H1F07  'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H4009  'Maximum scanline/character height
    outport &H3D4, &H3110  'Vertical sync/retrace start
    outport &H3D4, &H8011& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &H2B12  'Vertical displayed
    outport &H3D4, &H3213  'Offset/logical width
    outport &H3D4, &H2F15  'Vertical blanking start
    outport &H3D4, &H4416  'Vertical blanking end
    VGAWidth% = 400
    VGAHeight% = 300
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This sets up the PAGEFLIP sub-routine
    SetActivePage 1        '(for use with multipage modes)
    ModeName$ = "400x300x256"

  CASE IS = "400x600x256"
    'Note: This mode may require monitors that
    '      support adjustible vertical height
    VGA "320x200x256"      'First, set the unchained version of mode &H13
    outport &H3D4, &H2C11  'Second, turn off write protect
    OUT &H3C2, &HE7        'Dot clock
    outport &H3D4, &H7000  'Horizontal total
    outport &H3D4, &H6301  'Horizontal displayed
    outport &H3D4, &H6402  'Horizontal blanking start
    outport &H3D4, &H9203& 'Horizontal blanking end
    outport &H3D4, &H6504  'Horizontal sync/retrace start
    outport &H3D4, &H8205& 'Horizontal sync/retrace end
    outport &H3D4, &H7006  'Vertical total
    outport &H3D4, &HF007& 'Overflow (bit 8 of vertical counts)
    outport &H3D4, &H6009  'Maximum scanline/character height
    outport &H3D4, &H5B10  'Vertical sync/retrace start
    outport &H3D4, &H8C11& 'Vertical sync/retrace end and protect cr0-cr7
    outport &H3D4, &H5712  'Vertical displayed
    outport &H3D4, &H3213  'Offset/logical width
    outport &H3D4, &H5815  'Vertical blanking start
    outport &H3D4, &H7016  'Vertical blanking end
    VGAWidth% = 400
    VGAHeight% = 600
    VGAWidthBytes% = (VGAWidth% / 4) 'Offset/logical width was changed
    TotalPages% = 65536 \ (CLNG(VGAWidthBytes%) * VGAHeight%)
    SetVisiblePage 0       'This mode has less
    SetActivePage 0        'than two pages
    ModeName$ = "400x600x256"

  CASE ELSE
    VGA "320x200x256"      'Set the unchained version of mode 13h
END SELECT
END SUB

SUB WaitRetrace

'WaitRetrace waits until the video
'card is in a vertical retrace.
'This prevents flickers from
'occurring.

DO
LOOP UNTIL (INP(&H3DA) AND &H8) = 0
DO
LOOP WHILE (INP(&H3DA) AND &H8) = 0
END SUB

SUB WARNING
'Warning message/loop
SCREEN 0
CLS
WIDTH 80, 25
COLOR 2
PRINT "slix version ";
slixVERSION$ = RIGHT$(STR$(slixVERSION%), LEN(STR$(slixVERSION%)) - 1)
SELECT CASE LEN(slixVERSION$)
  CASE IS < 4
    PRINT "0.";
    PRINT STRING$((3 - LEN(slixVERSION$)), "0") + slixVERSION$
  CASE ELSE
    PRINT LEFT$(slixVERSION$, LEN(slixVERSION$) - 3);
    PRINT ".";
    PRINT RIGHT$(slixVERSION$, 3)
END SELECT
COLOR 3
PRINT slixDATE$
PRINT
COLOR 9
PRINT "Written by Lloyd Chang"
PRINT
COLOR 6
PRINT "*** PLEASE READ THE DISCLAIMER BEFORE YOU USE slix ***"
PRINT
COLOR 14
PRINT "!!!USE AT YOUR OWN RISK!!!"
COLOR 7
PRINT "This program may ";
COLOR 28
PRINT "!!!CRASH!!! ";
COLOR 7
PRINT "under certain shells and"
PRINT "certain operating systems.  ";
COLOR 14
PRINT "!!!USE AT YOUR OWN RISK!!!"
PRINT
COLOR 10
PRINT "Some modes may not line up perfectly with certain monitors."
PRINT "The user might have to adjust the size and position of the"
PRINT "screen with the monitor's control knobs."
PRINT
COLOR 13
PRINT "Some modes may not work on certain monitors and certain VGA"
PRINT "cards.  Discoloration may also occur."
PRINT
COLOR 11
PRINT "Recommended: 100% VGA compatible card"
PRINT "             SVGA monitor"
PRINT
COLOR 8
PRINT "Press [/] three times to continue..."
COLOR 15
PRINT "OR HOLD DOWN [Q] TO QUIT (after any " + CHR$(34) + "to continue..." + CHR$(34) + ")"
DO
  Keyed$ = INKEY$
  SELECT CASE Keyed$
    CASE IS = "Q", "q"
      END
    CASE "/"
      Count% = Count% + 1
  END SELECT
LOOP UNTIL Count% = 3
END SUB

