DEFINT A-Z
OPTION BASE 0
OPTION EXPLICIT

TYPE LocationStr
 NameV AS STRING * 50
 Plural AS INTEGER
 TypeV AS STRING * 1
 x AS INTEGER
 y AS INTEGER
END TYPE

TYPE RegistersStr
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
END TYPE

CONST FALSE = 0
CONST LEFT_MOUSE_BUTTON = 1
CONST NO_MOUSE_BUTTON = 0
CONST NONE = -1
CONST NULL = 0
CONST TRUE = -1

DECLARE FUNCTION AskTurns ()
DECLARE FUNCTION DisplayError (Index)
DECLARE FUNCTION Extension (NewExtension AS STRING) AS STRING
DECLARE FUNCTION GetErrorText (Index) AS STRING
DECLARE FUNCTION GetLanguages (Extensions() AS STRING) AS STRING
DECLARE FUNCTION GetLocationAt (MapX, MapY, Locations() AS LocationStr, MouseX, MouseY, MarginX, MarginY)
DECLARE FUNCTION GetMouseStatus (MouseX, MouseY)
DECLARE FUNCTION GetNumber (x, y, Prompt AS STRING, MaximumLength)
DECLARE FUNCTION GetTextItem (Index) AS STRING
DECLARE FUNCTION IsCorrectAnswer (MapX, MapY, Location, Locations() AS LocationStr, MouseX, MouseY)
DECLARE FUNCTION LoadTitle () AS STRING
DECLARE FUNCTION SelectMap (Maps() AS STRING, MouseX, MouseY)
DECLARE FUNCTION SelectLocation (Locations() AS LocationStr)
DECLARE FUNCTION YesNoDialog (Text AS STRING)
DECLARE SUB AlignLeft (Text AS STRING, x, y)
DECLARE SUB Center (x, Text AS STRING, Padding)
DECLARE SUB DialogBox (y, Height, Text AS STRING, Padding)
DECLARE SUB DisplayCorrectAnswer (MapX, MapY, Location, Locations() AS LocationStr, MarginX, MarginY)
DECLARE SUB DisplayImage (x, y, ImageFile AS STRING, Mask AS STRING, ImageWidth, ImageHeight)
DECLARE SUB DisplayInformation ()
DECLARE SUB DisplayMap (x, y, MapFile AS STRING, MapWidth, MapHeight)
DECLARE SUB DisplayMenu (Maps() AS STRING)
DECLARE SUB DisplayMistake (MapX, MapY, Locations() AS LocationStr, MouseX, MouseY, MarginX, MarginY)
DECLARE SUB DisplaySymbol (MapX, MapY, Location AS LocationStr)
DECLARE SUB DisplaySymbols (MapX, MapY, Locations() AS LocationStr)
DECLARE SUB DisplayTitle ()
DECLARE SUB DisplayWorldMap ()
DECLARE SUB DrawLegend ()
DECLARE SUB DrawTextBox (x, y, Wdth, Height)
DECLARE SUB GetTextSize (Text AS STRING, Columns, Rows)
DECLARE SUB HighlightOnMap (x, y, MarginX, MarginY)
DECLARE SUB Initialize ()
DECLARE SUB InitializeMouse ()
DECLARE SUB Interrupt (InterruptNr, RegistersIn AS RegistersStr, RegistersOut AS RegistersStr)
DECLARE SUB LoadLocationData (MapFile AS STRING, Locations() AS LocationStr)
DECLARE SUB LoadLocationNames (MapFile AS STRING, Locations() AS LocationStr)
DECLARE SUB LoadMaps (Maps() AS STRING)
DECLARE SUB MakeButton (x, y, Wdth, Text AS STRING, HasEdge)
DECLARE SUB Menu ()
DECLARE SUB Practice (MapFile AS STRING, TurnCount)
DECLARE SUB PressButton (MouseX, MouseY, ButtonWidth)
DECLARE SUB Quit ()
DECLARE SUB SelectLanguage ()
DECLARE SUB WaitForMouseClick ()
''ON ERROR GOTO FatalErrorTrap
DIM Choice
DIM ErrorCode
DIM KeyStroke AS STRING
DIM Text AS STRING
Text = GetErrorText(NONE)
''ON ERROR GOTO GeneralErrorTrap
Initialize
InitializeMouse
DisplayTitle
DisplayWorldMap
Menu
END

FatalErrorTrap:
SCREEN 0: WIDTH 80, 25: COLOR 7, 0: CLS
PRINT " Topography 3 - Error:"
PRINT
PRINT " The program could not start."
PRINT
PRINT " Check if the file " + CHR$(34) + "TOPO3.SET" + CHR$(34) + " is missing."
PRINT " If so, try starting Topography 3 using"
PRINT " the following command: " + CHR$(34) + ".\TOPO3.EXE eng" + CHR$(34) + "."
PRINT " This should create the settings file"
PRINT " and set English as the default language."
PRINT
PRINT " Press any key to quit."
KeyStroke = INPUT$(1)
Quit

GeneralErrorTrap:
ErrorCode = ERR
SELECT CASE ErrorCode
 CASE 5
  Choice = DisplayError(1)
 CASE 7, 14
  Choice = DisplayError(3)
 CASE 9
  Choice = DisplayError(4)
 CASE 52, 62
  Choice = DisplayError(5)
 CASE 53
  Choice = DisplayError(6)
 CASE 55
  Choice = DisplayError(7)
 CASE 61
  Choice = DisplayError(8)
 CASE 70
  Choice = DisplayError(9)
 CASE 71
  Choice = DisplayError(10)
 CASE 72
  Choice = DisplayError(11)
 CASE 75, 76
  Choice = DisplayError(12)
 CASE ELSE
  Choice = DisplayError(13)
END SELECT
SELECT CASE Choice
 CASE 1: RESUME
 CASE 2: RESUME NEXT
 CASE 3: Quit
END SELECT

SUB AlignLeft (Text AS STRING, x, y)
DIM Columns
DIM Rows

 GetTextSize Text, Columns, Rows
 LOCATE x, y - Columns
 PRINT Text
END SUB

FUNCTION AskTurns ()
DIM Registers AS RegistersStr
DIM TurnCount

 Registers.ax = &H2: Interrupt &H33, Registers, Registers

 CLS
 LINE (0, 0)-(639, 479), 5, BF
 COLOR 15
 TurnCount = GetNumber(5, 5, GetTextItem(3), 2)
 AskTurns = TurnCount
END FUNCTION

SUB Center (x, Text AS STRING, Padding)
DIM Centered AS STRING
DIM Fragment AS STRING
DIM Index

 Centered = Text
 IF NOT RIGHT$(Centered, 1) = CHR$(13) THEN Centered = Centered + CHR$(13)
 IF x > 0 THEN LOCATE x

 DO
  Index = INSTR(Centered, CHR$(13))
  IF Index = 0 THEN EXIT DO
  Fragment = LEFT$(Centered, Index - 1)
  IF Padding > LEN(Fragment) THEN Fragment = Fragment + SPACE$(Padding - LEN(Fragment))
  LOCATE , INT(40 - (LEN(Fragment) / 2))
  PRINT Fragment
  Centered = MID$(Centered, Index + 1)
 LOOP
END SUB

SUB DialogBox (y, Height, Text AS STRING, Padding)
DIM Box(&H0 TO &H3FFF) AS LONG
DIM Columns
DIM Rows
DIM Wdth
DIM x
STATIC Background() AS LONG
STATIC BoxX
STATIC BoxY

 IF y = NONE THEN
  PUT (BoxX, BoxY), Background, PSET
 ELSE
  REDIM Background(&H0 TO &H3FFF) AS LONG
  GetTextSize Text, Columns, Rows

  Wdth = (Columns + 2) * 8
  x = CINT(312 - (Wdth / 2))
  y = (y - 1) * 8
  IF Height = NONE THEN Height = (Rows + 2) * 8

  GET (x, y)-STEP(Wdth + 10, Height + 2), Background
  LINE (x + 2, y + 2)-STEP(Wdth, Height), 8, BF
  LINE (x, y)-STEP(Wdth, Height), 0, BF
  LINE (x + 1, y + 1)-STEP(Wdth - 2, Height - 2), 15, B
  Center ((y / 8) + 2), Text, Padding
  GET (x, y)-STEP(Wdth, Height), Box
  PUT (x, y), Box, PRESET
 
  BoxX = x
  BoxY = y
 END IF
END SUB

SUB DisplayCorrectAnswer (MapX, MapY, Location, Locations() AS LocationStr, MarginX, MarginY)
 LOCATE 2, 2: PRINT SPACE$(78): LOCATE 2, 3
 IF Locations(Location).Plural THEN
  PRINT GetTextItem(21); RTRIM$(Locations(Location).NameV); GetTextItem(23);
 ELSEIF NOT Locations(Location).Plural THEN
  PRINT GetTextItem(20); RTRIM$(Locations(Location).NameV); GetTextItem(22);
 END IF
 PRINT GetTextItem(24);

 HighlightOnMap Locations(Location).x + MapX, Locations(Location).y + MapY, MarginX, MarginY
END SUB

FUNCTION DisplayError (Index)
DIM Choice AS STRING
DIM Columns
DIM ErrorBackground(&H0 TO &H3FFF) AS LONG
DIM KeyStroke AS STRING
DIM Rows

 GetTextSize GetErrorText(Index), Columns, Rows

 IF Index = 1 THEN
  SCREEN 0: WIDTH 80, 25: PALETTE
  COLOR 0, 1: CLS : COLOR , 7
  IF NOT Columns MOD 2 = 0 THEN Columns = Columns + 1
  DrawTextBox 4, INT(40 - (Columns / 2)) - 1, Columns, Rows
  Center 5, GetErrorText(Index), Columns
  KeyStroke = INPUT$(1)
  Choice = "3"
 ELSE
  GET (125, 20)-STEP(365, 185), ErrorBackground
  COLOR 15
  DialogBox 4, NONE, GetErrorText(Index), Columns
  COLOR 7: Center 10, GetErrorText(0), 0
  DO
   Choice = INPUT$(1)
  LOOP WHILE INSTR("123", Choice) = 0
  PUT (125, 20), ErrorBackground, PSET
 END IF

 DisplayError = VAL(Choice)
END FUNCTION

SUB DisplayImage (x, y, ImageFile AS STRING, Mask AS STRING, ImageWidth, ImageHeight)
DIM Image(&H0 TO &H7FF0)

 DEF SEG = VARSEG(Image(0))
 IF Mask = "" THEN
  BLOAD ".\Images\" + ImageFile, VARPTR(Image(0))
  PUT (x, y), Image, PSET
 ELSE
  BLOAD ".\Images\" + Mask, VARPTR(Image(0))
  PUT (x, y), Image, AND
  BLOAD ".\Images\" + ImageFile, VARPTR(Image(0))
  PUT (x, y), Image, XOR
 END IF

 ImageWidth = Image(0)
 ImageHeight = Image(1)
END SUB

SUB DisplayInformation ()
DIM Index
DIM Registers AS RegistersStr
DIM Text AS STRING

 Text = ""
 FOR Index = 31 TO 33
  Text = Text + GetTextItem(Index) + CHR$(13)
 NEXT Index
 DialogBox 9, 40, Text, 0

 WaitForMouseClick
 Registers.ax = &H2: Interrupt &H33, Registers, Registers
 DialogBox NONE, 0, "", 0
 Registers.ax = &H1: Interrupt &H33, Registers, Registers
END SUB

SUB DisplayMap (x, y, MapFile AS STRING, MapWidth, MapHeight)
DIM AddedHeight

 IF RIGHT$(MapFile, 1) = "*" THEN
  MapFile = LEFT$(MapFile, LEN(MapFile) - 1)
  DisplayImage x, y, MapFile + ".map", "", MapWidth, MapHeight
  DisplayImage x, y + MapHeight, MapFile + ".ext", "", MapWidth, AddedHeight
  MapHeight = MapHeight + AddedHeight
 ELSE
  DisplayImage x, y, MapFile + ".map", "", MapWidth, MapHeight
 END IF
END SUB

SUB DisplayMenu (Maps() AS STRING)
DIM AreaName AS STRING
DIM FileHandle
DIM Index
DIM MapFile AS STRING
DIM x
DIM y

 CLS
 LINE (0, 0)-(639, 479), 5, BF
 COLOR 15
 LoadMaps Maps()
 x = 9
 y = 5
 FileHandle = FREEFILE
 OPEN ".\Data\Maps." + Extension("") FOR INPUT LOCK READ WRITE AS FileHandle
  FOR Index = LBOUND(Maps) TO UBOUND(Maps)
   MapFile = Maps(Index)
   IF RIGHT$(Maps(Index), 1) = "*" THEN MapFile = LEFT$(MapFile, LEN(MapFile) - 1)
   DisplayImage ((y * 8) + 45), ((x * 8) - 60), MapFile + ".flg", "", 0, 0
   LINE INPUT #FileHandle, AreaName
   MakeButton x, y, 21, AreaName, 0
   IF x = 45 THEN x = 9: y = 55 ELSE x = x + 9
  NEXT Index
 CLOSE FileHandle

 MakeButton 49, 5, 10, GetTextItem(0), FALSE
 MakeButton 52, 5, 14, GetTextItem(1), FALSE
 MakeButton 49, 55, 8, GetTextItem(2), FALSE
END SUB

SUB DisplayMistake (MapX, MapY, Locations() AS LocationStr, MouseX, MouseY, MarginX, MarginY)
DIM LocationFound

 LocationFound = GetLocationAt(MapX, MapY, Locations(), MouseX, MouseY, MarginX, MarginY)

 LOCATE 2, 2: PRINT SPACE$(78)
 IF LocationFound = TRUE THEN
  LOCATE 2, 3: PRINT GetTextItem(19);
 ELSE
  LOCATE 2, 3
  IF Locations(LocationFound).Plural THEN
   PRINT GetTextItem(18); RTRIM$(Locations(LocationFound).NameV); GetTextItem(23);
  ELSEIF NOT Locations(LocationFound).Plural THEN
   PRINT GetTextItem(17); RTRIM$(Locations(LocationFound).NameV); GetTextItem(22);
  END IF
 END IF
 PRINT GetTextItem(24);

 WaitForMouseClick
 LOCATE 2, 2: PRINT SPACE$(78)
END SUB

SUB DisplaySymbol (MapX, MapY, Location AS LocationStr)
DIM x
DIM y

 x = MapX + Location.x
 y = MapY + Location.y

 SELECT CASE Location.TypeV
  CASE "C"
   LINE (x - 5, y - 2)-STEP(10, 5), 4, BF
  CASE "c"
   CIRCLE (x, y), 3, 4
   PAINT (x, y), 4
  CASE "m"
   DisplayImage x - 14, y - 7, "Mountain.sym", "Mountain.msk", 0, 0
  CASE "v"
   DisplayImage x - 14, y - 7, "Volcano.sym", "Volcano.msk", 0, 0
  CASE "w"
   CIRCLE (x, y), 3, 1
   PAINT (x, y), 1
 END SELECT
END SUB

SUB DisplaySymbols (MapX, MapY, Locations() AS LocationStr)
DIM Location

 FOR Location = LBOUND(Locations) TO UBOUND(Locations) - 1
  DisplaySymbol MapX, MapY, Locations(Location)
 NEXT Location
END SUB

SUB DisplayTitle ()
DIM Bitmap AS STRING
DIM x
DIM y

 Bitmap = LoadTitle()

 CLS
 COLOR 15
 LOCATE 1
 FOR y = 0 TO 11
  FOR x = 0 TO 79
   LINE (x * 8, y * 4)-STEP(7, 3), VAL("&H" + MID$(Bitmap, ((y * 80) + x) + 1, 1)), BF
  NEXT x
 NEXT y

 COLOR 7: Center 38, GetTextItem(35), 0
END SUB

SUB DisplayWorldMap ()
DIM Map1(&H0 TO &H6400)
DIM Map2(&H0 TO &H1D0)

 DisplayImage 95, 60, "World.map", "", 0, 0
 DO
  GET (95, 60)-STEP(445, 225), Map1
  GET (541, 60)-STEP(2, 225), Map2
  PUT (95, 60), Map2, PSET
  PUT (98, 60), Map1, PSET
 LOOP WHILE INKEY$ = ""
END SUB

SUB DrawLegend ()
DIM Label
DIM Labels(0 TO 5) AS STRING
DIM Legend(&H0 TO &H15BF)
DIM Wdth
DIM y
DIM x

 Wdth = 0
 FOR Label = LBOUND(Labels) TO UBOUND(Labels)
  Labels(Label) = GetTextItem(5 + Label)
  IF LEN(Labels(Label)) > Wdth THEN Wdth = LEN(Labels(Label))
 NEXT Label

 y = 78 - Wdth
 Wdth = (7 + Wdth) * 8
 x = 632 - Wdth
 LINE (x, 80)-STEP(Wdth, 120), 8, BF
 LINE (x + 2, 82)-STEP(Wdth, 120), 8, BF
 LINE (x + 1, 81)-STEP(Wdth - 2, 118), 15, B
 LOCATE 12, y: PRINT Labels(LBOUND(Labels))
 PRINT
 FOR Label = LBOUND(Labels) + 1 TO UBOUND(Labels)
  LOCATE , y + 1: PRINT Labels(Label)
  PRINT
 NEXT Label
 GET (x, 80)-STEP(Wdth, 120), Legend
 PUT (x, 80), Legend, PRESET
 LINE (x + 19, 106)-STEP(10, 5), 4, BF
 CIRCLE (x + 24, 124), 3, 4: PAINT (x + 24, 124), 4
 CIRCLE (x + 24, 140), 3, 1: PAINT (x + 24, 140), 1

 DisplayImage x + 12, 152, "Mountain.sym", "Mountain.msk", 0, 0
 DisplayImage x + 12, 168, "Volcano.sym", "Volcano.msk", 0, 0
END SUB

SUB DrawTextBox (x, y, Wdth, Height)
DIM Row

 LOCATE x, y: PRINT CHR$(218); STRING$(Wdth, 196); CHR$(191)
 FOR Row = 1 TO Height
  LOCATE , y: PRINT CHR$(179);
  LOCATE , y + Wdth + 1: PRINT CHR$(179)
 NEXT Row
 LOCATE , y: PRINT CHR$(192); STRING$(Wdth, 196); CHR$(217);
END SUB

FUNCTION Extension (NewExtension AS STRING) AS STRING
DIM FileHandle
STATIC CurrentExtension AS STRING

 IF CurrentExtension = "" THEN
  IF COMMAND$ = "" THEN
   FileHandle = FREEFILE
   OPEN "Topo3.set" FOR INPUT LOCK READ WRITE AS FileHandle
    LINE INPUT #FileHandle, CurrentExtension
   CLOSE FileHandle
  ELSE
   NewExtension = COMMAND$
  END IF
 END IF

 IF NOT NewExtension = "" THEN
  FileHandle = FREEFILE
  OPEN "Topo3.set" FOR OUTPUT LOCK READ WRITE AS FileHandle
   PRINT #FileHandle, NewExtension;
  CLOSE FileHandle
  CurrentExtension = NewExtension
 END IF

 Extension = CurrentExtension
END FUNCTION

FUNCTION GetErrorText (Index) AS STRING
DIM FileHandle
DIM Text AS STRING
STATIC ErrorText() AS STRING

 IF Index = NONE THEN
  REDIM ErrorText(0 TO 13) AS STRING
  Index = LBOUND(ErrorText)
  FileHandle = FREEFILE
  OPEN "Errors." + Extension("") FOR INPUT LOCK READ WRITE AS FileHandle
   DO UNTIL EOF(FileHandle)
    LINE INPUT #FileHandle, Text
    IF RTRIM$(Text) = "-" THEN
     Index = Index + 1
    ELSE
     IF NOT ErrorText(Index) = "" THEN Text = CHR$(13) + Text
     ErrorText(Index) = ErrorText(Index) + Text
    END IF
   LOOP
  CLOSE FileHandle
 ELSE
  GetErrorText = ErrorText(Index)
  EXIT FUNCTION
 END IF

 GetErrorText = ""
END FUNCTION

FUNCTION GetLanguages (Extensions() AS STRING) AS STRING
DIM FileHandle
DIM Index
DIM Language AS STRING
DIM Languages AS STRING

 Languages = ""
 FileHandle = FREEFILE
 OPEN ".\Data\Lnguages.dat" FOR INPUT LOCK READ WRITE AS FileHandle
  FOR Index = LBOUND(Extensions) TO UBOUND(Extensions)
   IF EOF(FileHandle) THEN EXIT FOR
   INPUT #FileHandle, Language, Extensions(Index)
   Languages = Languages + CHR$(13) + Language$
  NEXT Index
 CLOSE FileHandle

 GetLanguages = Languages
END FUNCTION

FUNCTION GetLocationAt (MapX, MapY, Locations() AS LocationStr, MouseX, MouseY, MarginX, MarginY)
DIM Location
DIM LocationFound

 LocationFound = TRUE

 FOR Location = LBOUND(Locations) TO UBOUND(Locations) - 1
  IF Locations(Location).NameV = "" THEN EXIT FOR
  IF MouseX >= (Locations(Location).x - MarginX) + MapX AND MouseX <= (Locations(Location).x + MarginX) + MapX THEN
   IF MouseY >= (Locations(Location).y - MarginY) + MapY AND MouseY <= (Locations(Location).y + MarginY) + MapY THEN
    LocationFound = Location
    EXIT FOR
   END IF
  END IF
 NEXT Location

 GetLocationAt = LocationFound
END FUNCTION

FUNCTION GetMouseStatus (MouseX, MouseY)
DIM KeyStroke AS STRING
DIM Registers AS RegistersStr

 KeyStroke = INKEY$
 Registers.ax = &H3: Interrupt &H33, Registers, Registers
 MouseX = Registers.cx: MouseY = Registers.dx
 GetMouseStatus = Registers.bx
END FUNCTION

FUNCTION GetNumber (x, y, Prompt AS STRING, MaximumLength)
DIM Background(&H0 TO &H600)
DIM KeyStroke AS STRING
DIM Length
DIM Number AS STRING

 GET (0, (x - 1) * 8)-STEP(639, 8), Background
 Number = ""
 LOCATE x, y: PRINT SPACE$(LEN(Prompt) + 4)
 DO
  LOCATE x, y: PRINT Prompt; Number; "_ "
  DO
   KeyStroke = INKEY$
  LOOP WHILE KeyStroke = ""
  Length = LEN(Number)
  SELECT CASE ASC(KeyStroke)
   CASE 8
    IF Length > 0 THEN Number = LEFT$(Number, Length - 1)
   CASE 13
    EXIT DO
   CASE 27
    Number = "": EXIT DO
   CASE 48 TO 57
    IF Length < MaximumLength THEN Number = Number + KeyStroke
  END SELECT
 LOOP

 PUT (0, (x - 1) * 8), Background, PSET
 GetNumber = VAL(Number)
END FUNCTION

FUNCTION GetTextItem (Index) AS STRING
DIM CurrentIndex
DIM FileHandle
STATIC Text() AS STRING

 IF Index = NONE THEN
  REDIM Text(0 TO 35) AS STRING
  CurrentIndex = 0
  FileHandle = FREEFILE
  OPEN ".\Data\Text." + Extension("") FOR INPUT LOCK READ WRITE AS FileHandle
   DO UNTIL EOF(FileHandle)
    LINE INPUT #FileHandle, Text(CurrentIndex)
    CurrentIndex = CurrentIndex + 1
   LOOP
  CLOSE FileHandle
 ELSE
  GetTextItem = Text(Index)
  EXIT FUNCTION
 END IF

 GetTextItem = ""
END FUNCTION

SUB GetTextSize (Text AS STRING, Columns, Rows)
DIM Index
DIM PreviousIndex
DIM Widest

 Columns = 0
 Index = NULL
 Rows = 0
 Widest = 0
 DO UNTIL Index >= LEN(Text)
  PreviousIndex = Index
  Index = INSTR(PreviousIndex + 1, Text, CHR$(13))
  IF Index = NULL THEN Index = LEN(Text) + 1
  IF Index - PreviousIndex > Widest THEN Widest = Index - PreviousIndex
  Rows = Rows + 1
 LOOP
 IF Widest = 0 THEN Columns = LEN(Text) ELSE Columns = Widest - 1
END SUB

SUB HighlightOnMap (x, y, MarginX, MarginY)
DIM Background(&H0 TO &H130)

 GET (x - MarginX - 1, y - MarginY - 1)-STEP((MarginX + 1) * 2, (MarginY + 1) * 2), Background
 LINE (x - MarginX, y - MarginY)-STEP(MarginX * 2, MarginY * 2), 0, B
 LINE (x - MarginX - 1, y - MarginY - 1)-STEP((MarginX + 1) * 2, (MarginY + 1) * 2), 0, B
 WaitForMouseClick
 PUT (x - MarginX - 1, y - MarginY - 1), Background, PSET
END SUB

SUB Initialize ()
DIM Text AS STRING

 SCREEN 12
 WIDTH 80, 60
 PALETTE 5, &H1E0000
 COLOR 15
 CLS
 Text = GetTextItem(NONE)
END SUB

SUB InitializeMouse ()
DIM Choice
DIM Registers AS RegistersStr

 DO
  Registers.ax = &H0: Interrupt &H33, Registers, Registers
  IF Registers.ax = &H0 THEN
   Choice = DisplayError(2)
   IF NOT Choice = 1 THEN
    SELECT CASE Choice
     CASE 2
      EXIT DO
     CASE 3
      Quit
    END SELECT
   END IF
  ELSE
   EXIT DO
  END IF
 LOOP

 Registers.ax = &H7
 Registers.cx = 0
 Registers.dx = 639
 Interrupt &H33, Registers, Registers
 Registers.ax = &H8
 Registers.cx = 0
 Registers.dx = 479
 Interrupt &H33, Registers, Registers
END SUB

FUNCTION IsCorrectAnswer (MapX, MapY, Location, Locations() AS LocationStr, MouseX, MouseY)
DIM IsCorrect
DIM MarginX
DIM MarginY
DIM Registers AS RegistersStr
DIM x
DIM y

 x = Locations(Location).x + MapX
 y = Locations(Location).y + MapY

 SELECT CASE Locations(Location).TypeV
  CASE "C"
   MarginX = 15: MarginY = 10
  CASE "c", "m", "w"
   MarginX = 5: MarginY = 5
  CASE "v"
   MarginX = 15: MarginY = 13
 END SELECT

 IsCorrect = FALSE
 IF MouseX >= x - MarginX AND MouseX <= x + MarginX AND MouseY >= y - MarginY AND MouseY <= y + MarginY THEN
  IsCorrect = TRUE
 ELSE
  Registers.ax = &H2: Interrupt &H33, Registers, Registers
  SOUND 37, 5
  DisplayMistake 10, 20, Locations(), MouseX, MouseY, MarginX, MarginY
  DisplayCorrectAnswer MapX, MapY, Location, Locations(), MarginX, MarginY
  Registers.ax = &H1: Interrupt &H33, Registers, Registers
 END IF

 IsCorrectAnswer = IsCorrect
END FUNCTION

SUB LoadLocationData (MapFile AS STRING, Locations() AS LocationStr)
DIM FileHandle
DIM Location
DIM LocationData AS STRING

 FileHandle = FREEFILE
 OPEN ".\Data\Locatns.dat" FOR INPUT LOCK READ WRITE AS FileHandle
  DO UNTIL EOF(FileHandle)
   LINE INPUT #FileHandle, LocationData
   IF LTRIM$(RTRIM$(LCASE$(LocationData))) = LCASE$(MapFile) THEN EXIT DO
  LOOP

  FOR Location = LBOUND(Locations) TO UBOUND(Locations) - 1
   INPUT #FileHandle, Locations(Location).x
   INPUT #FileHandle, Locations(Location).y
   INPUT #FileHandle, Locations(Location).TypeV
   INPUT #FileHandle, Locations(Location).Plural
  NEXT Location
 CLOSE FileHandle
END SUB

SUB LoadLocationNames (MapFile AS STRING, Locations() AS LocationStr)
DIM FileHandle

 FileHandle = FREEFILE
 OPEN ".\Data\" + MapFile + "." + Extension("") FOR INPUT LOCK READ WRITE AS FileHandle
  DO UNTIL EOF(FileHandle)
   LINE INPUT #FileHandle, Locations(UBOUND(Locations)).NameV
   IF RTRIM$(Locations(UBOUND(Locations)).NameV) = "" THEN EXIT DO
   REDIM PRESERVE Locations(LBOUND(Locations) TO UBOUND(Locations) + 1) AS LocationStr
  LOOP
 CLOSE FileHandle
END SUB

SUB LoadMaps (Maps() AS STRING)
DIM FileHandle
DIM Index

 FileHandle = FREEFILE
 OPEN ".\Data\MapFiles.dat" FOR INPUT LOCK READ WRITE AS FileHandle
  FOR Index = LBOUND(Maps) TO UBOUND(Maps)
   LINE INPUT #FileHandle, Maps(Index)
  NEXT Index
 CLOSE FileHandle
END SUB

FUNCTION LoadTitle () AS STRING
DIM Bitmap AS STRING
DIM BitmapRow AS STRING
DIM FileHandle

 Bitmap = ""
 FileHandle = FREEFILE
 OPEN ".\Data\Title." + Extension("") FOR INPUT LOCK READ WRITE AS FileHandle
  DO UNTIL EOF(FileHandle)
   LINE INPUT #FileHandle, BitmapRow
   Bitmap = Bitmap + BitmapRow
  LOOP
 CLOSE FileHandle

 LoadTitle = Bitmap
END FUNCTION

SUB MakeButton (x, y, Wdth, Text AS STRING, HasEdge)
DIM Button(&H0 TO &H600)
DIM ButtonX
DIM ButtonY
DIM PixelWidth

 ButtonX = x
 ButtonY = y

 Text = " " + Text
 LOCATE ButtonX, ButtonY: PRINT Text; SPACE$(Wdth - LEN(Text));

 PixelWidth = Wdth * 8
 SWAP ButtonY, ButtonX
 ButtonX = (ButtonX - 1) * 8
 ButtonY = (ButtonY - 1) * 8

 GET (ButtonX, ButtonY)-STEP(PixelWidth - 1, 7), Button
 LINE (ButtonX, ButtonY)-STEP(PixelWidth, 8), 0, BF
 DRAW "C8 BM" + STR$(ButtonX + 2) + "," + STR$(ButtonY + 9) + " R" + STR$(PixelWidth - 1) + "U7 R1 D8 L" + STR$(PixelWidth)
 PUT (ButtonX, ButtonY), Button, PRESET
 IF HasEdge THEN DRAW "C0 BM" + STR$(ButtonX - 1) + "," + STR$(ButtonY - 1) + " D8 U8 R" + STR$(PixelWidth)
END SUB

SUB Menu ()
DIM Index
DIM Maps(0 TO 9) AS STRING
DIM MouseX
DIM MouseY
DIM Registers AS RegistersStr
DIM TurnCount

 DO
  DisplayMenu Maps()

  Registers.ax = &H1: Interrupt &H33, Registers, Registers
  DO
   DO
   LOOP UNTIL GetMouseStatus(MouseX, MouseY) = LEFT_MOUSE_BUTTON
   MouseX = INT(MouseX / 8) + 1
   MouseY = INT(MouseY / 8) + 1
   IF MouseX > 4 AND MouseX < 16 AND MouseY = 49 THEN
    PressButton 5, 49, 11
    SelectLanguage
    EXIT DO
   ELSEIF MouseX > 54 AND MouseX < 64 AND MouseY = 49 THEN
    PressButton 55, 49, 9
    IF YesNoDialog(GetTextItem(28)) THEN Quit
   ELSEIF MouseX > 4 AND MouseX < 19 AND MouseY = 52 THEN
    PressButton 5, 52, 15
    DisplayInformation
   ELSE
    Index = SelectMap(Maps(), MouseX, MouseY)
    IF Index >= 0 THEN
     TurnCount = AskTurns()
     IF TurnCount > 0 THEN Practice Maps(Index), TurnCount
     EXIT DO
    END IF
   END IF
  LOOP
 LOOP
END SUB

SUB Practice (MapFile AS STRING, TurnCount)
DIM Location
DIM Locations(0 TO 0) AS LocationStr
DIM MapHeight
DIM MapWidth
DIM MouseX
DIM MouseY
DIM Registers AS RegistersStr
DIM RightAnswerCount
DIM WrongAnswerCount

 RANDOMIZE TIMER
 RightAnswerCount = 0
 WrongAnswerCount = 0

 CLS
 LINE (0, 0)-(639, 479), 5, BF

 DisplayMap 10, 20, MapFile, MapWidth, MapHeight
 LoadLocationNames MapFile, Locations()
 LoadLocationData MapFile, Locations()
 DisplaySymbols 10, 20, Locations()

 MakeButton 47, 60, 8, GetTextItem(4), FALSE

 AlignLeft GetTextItem(11), 4, 76
 AlignLeft GetTextItem(12), 6, 76
 AlignLeft GetTextItem(13), 8, 76

 DrawLegend
 Location = SelectLocation(Locations())
 Registers.ax = &H1: Interrupt &H33, Registers, Registers
 DO
  IF TurnCount = 0 THEN
   LOCATE 2, 2: PRINT SPACE$(78)
   LOCATE 2, 2: PRINT GetTextItem(25)
   WaitForMouseClick
   EXIT DO
  END IF

  LOCATE 4, 77: PRINT USING "###"; RightAnswerCount
  LOCATE 6, 77: PRINT USING "###"; WrongAnswerCount
  LOCATE 8, 77: PRINT USING "###"; TurnCount
  DO
  LOOP UNTIL GetMouseStatus(MouseX, MouseY) = LEFT_MOUSE_BUTTON
  IF MouseX > 10 AND MouseY > 20 AND MouseX < MapWidth + 10 AND MouseY < MapHeight + 20 THEN
   DO: LOOP UNTIL GetMouseStatus(MouseX, MouseY) = NO_MOUSE_BUTTON
   IF IsCorrectAnswer(10, 20, Location, Locations(), MouseX, MouseY) THEN
    RightAnswerCount = RightAnswerCount + 1
   ELSE
    WrongAnswerCount = WrongAnswerCount + 1
   END IF
   Location = SelectLocation(Locations())
   IF TurnCount > 0 THEN TurnCount = TurnCount - 1
  ELSE
   MouseX = INT(MouseX / 8) + 1
   MouseY = INT(MouseY / 8) + 1
   IF MouseX > 59 AND MouseX < 69 AND MouseY = 47 THEN
    PressButton 60, 47, 9
    IF YesNoDialog(GetTextItem(26)) THEN EXIT DO
   END IF
  END IF
 LOOP

 Registers.ax = &H2: Interrupt &H33, Registers, Registers
END SUB

SUB PressButton (MouseX, MouseY, ButtonWidth)
DIM Pressed(&H0 TO &H600)
DIM Raised(&H0 TO &H600)
DIM Registers AS RegistersStr
DIM x
DIM y

 Registers.ax = &H2: Interrupt &H33, Registers, Registers
 x = (MouseX - 1) * 8
 y = (MouseY - 1) * 8
 ButtonWidth = (ButtonWidth - 1) * 8
 GET (x, y)-STEP(ButtonWidth, 8), Pressed
 GET (x, y)-STEP(ButtonWidth + 2, 10), Raised
 LINE (x, y)-STEP(ButtonWidth, 8), 0, BF
 PUT (x + 2, y + 2), Pressed, PSET
 DO
  Registers.ax = &H3: Interrupt &H33, Registers, Registers
 LOOP UNTIL Registers.bx = &H0
 PUT (x, y), Raised, PSET
 Registers.ax = &H1: Interrupt &H33, Registers, Registers
END SUB

SUB Quit ()
 SCREEN 0
 WIDTH 80, 25
 PALETTE
 COLOR 7, 0
 CLS
 SYSTEM
END SUB

SUB SelectLanguage ()
DIM Cursor(&H0 TO &H44B)
DIM CursorWidth
DIM CursorX
DIM CursorY
DIM Extensions(0 TO 9) AS STRING
DIM Index
DIM KeyStroke AS STRING
DIM Languages AS STRING
DIM Registers AS RegistersStr
DIM Rows
DIM Text AS STRING

 Text = GetTextItem(27)
 GetTextSize Text, CursorWidth, Rows
 CursorWidth = (CursorWidth + 1) * 8
 CursorX = 312 - (CursorWidth / 2)
 Languages = Text + GetLanguages(Extensions())
 Registers.ax = &H2: Interrupt &H33, Registers, Registers
 DialogBox 9, 100, Languages, LEN(Text) - 1
 Index = LBOUND(Extensions)

 DO
  CursorY = (Index + 10) * 8
  GET (CursorX, CursorY)-STEP(CursorWidth, 8), Cursor
  PUT (CursorX, CursorY), Cursor, PRESET
  DO
   KeyStroke = INKEY$
  LOOP WHILE KeyStroke = ""
  PUT (CursorX, CursorY), Cursor, PSET
  SELECT CASE KeyStroke
   CASE CHR$(0) + "H"
    IF Index = LBOUND(Extensions) THEN Index = UBOUND(Extensions) ELSE Index = Index - 1
   CASE CHR$(0) + "P"
    IF Index = UBOUND(Extensions) THEN Index = LBOUND(Extensions) ELSE Index = Index + 1
   CASE CHR$(13)
    IF NOT Extensions(Index) = "" THEN
     ON ERROR GOTO FatalErrorTrap
     Languages = Extension(Extensions(Index))
     Languages = GetErrorText(NONE)
     ON ERROR GOTO GeneralErrorTrap
     Languages = GetTextItem(NONE)
     EXIT DO
    END IF
   CASE CHR$(27)
    DialogBox NONE, 0, "", 0
    EXIT DO
  END SELECT
 LOOP
END SUB

FUNCTION SelectLocation (Locations() AS LocationStr)
DIM Location
STATIC LastLocation

 DO
  Location = INT(RND * UBOUND(Locations))
  IF UBOUND(Locations) < 2 THEN EXIT DO
 LOOP WHILE Location = LastLocation
 LastLocation = Location

 LOCATE 2, 2: PRINT SPACE$(78)
 LOCATE 2, 3: PRINT GetTextItem(14); RTRIM$(Locations(Location).NameV);

 IF NOT Locations(Location).Plural THEN
  PRINT GetTextItem(15);
 ELSEIF Locations(Location).Plural THEN
  PRINT GetTextItem(16);
 END IF

 SelectLocation = Location
END FUNCTION

FUNCTION SelectMap (Maps() AS STRING, MouseX, MouseY)
DIM Index
DIM Map
DIM x
DIM y

 Map = NONE
 x = 9
 y = 5
 FOR Index = 0 TO 9
  IF MouseX >= y AND MouseX <= y + 21 AND MouseY = x THEN
   PressButton y, x, 22
   Map = Index
   EXIT FOR
  END IF
  IF x = 45 THEN x = 9: y = 55 ELSE x = x + 9
 NEXT Index

SelectMap = Map
END FUNCTION

SUB WaitForMouseClick ()
 DO: LOOP UNTIL GetMouseStatus(NULL, NULL) = NO_MOUSE_BUTTON
 DO: LOOP UNTIL GetMouseStatus(NULL, NULL) = LEFT_MOUSE_BUTTON
 DO: LOOP UNTIL GetMouseStatus(NULL, NULL) = NO_MOUSE_BUTTON
END SUB

FUNCTION YesNoDialog (Text AS STRING)
DIM Answer
DIM MouseX
DIM MouseY
DIM No AS STRING
DIM NoY
DIM Registers AS RegistersStr
DIM Wdth
DIM Yes AS STRING
DIM YesY

 DialogBox 15, 45, Text, 0
 Yes = GetTextItem(29) + " "
 No = GetTextItem(30) + " "
 IF LEN(Yes) >= LEN(No) THEN Wdth = LEN(Yes) ELSE Wdth = LEN(No)
 Wdth = Wdth + 1

 YesY = 41 - (LEN(Text) / 2)
 NoY = (39 + (LEN(Text) / 2)) - Wdth

 MakeButton 18, YesY, Wdth, Yes, TRUE
 MakeButton 18, NoY, Wdth, No, TRUE

 Answer = -2
 DO WHILE Answer = -2
  DO
  LOOP UNTIL GetMouseStatus(MouseX, MouseY) = LEFT_MOUSE_BUTTON
  MouseX = INT(MouseX / 8) + 1
  MouseY = INT(MouseY / 8) + 1
  IF MouseX >= YesY AND MouseX < YesY + Wdth AND MouseY = 18 THEN
   PressButton YesY, 18, Wdth + 1
   Answer = TRUE
  ELSEIF MouseX >= NoY AND MouseX < NoY + Wdth AND MouseY = 18 THEN
   PressButton NoY, 18, Wdth + 1
   Answer = FALSE
  END IF
 LOOP

 Registers.ax = &H2: Interrupt &H33, Registers, Registers
 DialogBox NONE, 0, "", 0
 Registers.ax = &H1: Interrupt &H33, Registers, Registers

 YesNoDialog = Answer
END FUNCTION

