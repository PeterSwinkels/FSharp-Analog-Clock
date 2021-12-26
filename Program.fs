//This namespaces used by this program.
open System
open System.Diagnostics
open System.Drawing
open System.Reflection
open System.Windows.Forms

let private ProgramInformation = FileVersionInfo.GetVersionInfo(Assembly.GetExecutingAssembly().Location) //Contains this program's information.

//This module contains the analog clock class and related functions.
module private AnalogClockModule =
   let private CLOCK_LINE_WIDTH = 2.0f         //Defines the width of the lines used to draw the clock.
   let private CLOCK_SIZE = 120.0              //Defines the clock face's diameter in pixels.
   let private HAND_NUT_SIZE = 3               //Defines the hands// nut size.
   let private HOURS_TO_DEGREES = 30           //Defines the value used to convert hours to degrees.
   let private LARGE_MARK_INTERVAL = 3         //Defines the interval between the large marks in hours.
   let private MINUTES_TO_DEGREES = 6          //Defines the value used to convert minutes to degrees.
   let private MINUTES_TO_FRACTION = 1.0/ 60.0 //Defines the value used to convert minutes to the fractional part of an hour.
   let private NO_HOUR = -1                    //Defines a value indicating "No hour.".
   let private NO_MINUTE = -1                  //Defines a value indicating "No minute.".
   let private NO_SECOND = -1                  //Defines a value indicating "No second.".
   let private SECONDS_TO_DEGREES = 6          //Defines the value used to convert seconds to degrees.
   let private TWELVE_HOUR_ANGLE = -90         //Defines the angle for noon/midnight in degrees.

   let private CLOCK_X = CLOCK_SIZE * 1.1 |> int            //Defines the clock face's horizontal center in pixels.
   let private CLOCK_Y = CLOCK_SIZE * 1.1 |> int            //Defines the clock face's vertical center in pixels.
   let private HOUR_HAND_LENGTH = CLOCK_SIZE / 1.6          //Defines the hour hand's length.
   let private LARGE_MARK_LENGTH = HOUR_HAND_LENGTH / 2.5   //Defines the size of the marking's used to mark every third hour.
   let private MINUTE_HAND_LENGTH = HOUR_HAND_LENGTH * 1.5  //Defines the minutes hand's length.
   let private SECOND_HAND_LENGTH = HOUR_HAND_LENGTH * 1.5  //Defines the seconds hand's length.
   let private SMALL_MARK_LENGTH = LARGE_MARK_LENGTH / 2.0  //Defines the size of the marking's used to mark the hours.
   let private TO_RADIANS = 180.0 / Math.PI                 //Defines the value used to convert degrees to radians.

   //This record defines the time displayed by the clock.
   type private TimeRec =
      {
      mutable Hour: int;     //Contains the hour.
      mutable Minute: int;   //Contains the minute.
      mutable Second: int;   //Contains the second.
      }

   let private AnalogClockTimer = new Timer(Enabled = true, Interval = 1000)                                 //Contains the timer that powers the clock.
   let private ControlToolTip = new ToolTip()                                                                //Contains this control's tooltip.
   let private Time = {Hour = DateTime.Now.Hour; Minute = DateTime.Now.Minute; Second = DateTime.Now.Second} //The current time.

   let mutable private HourHandX = 0     //The hour hand's horizontal position.
   let mutable private HourHandY = 0     //The hour hand's vertical position.
   let mutable private MinuteHandX = 0   //The minute hand's horizontal position.
   let mutable private MinuteHandY = 0   //The minute hand's vertical position.
   let mutable private SecondHandX = 0   //The second hand's horizontal position.
   let mutable private SecondHandY = 0   //The second hand's vertical position.

   //This function manages the time displayed by the clock.
   let private CurrentTime advance newHour newMinute newSecond = 
      if advance then
         if Time.Second = 59 then
            Time.Second <- 0
            if Time.Minute = 59 then
               Time.Minute <- 0
               Time.Hour <- if Time.Hour = 11 then 0 else Time.Hour + 1
            else
               Time.Minute <- Time.Minute + 1
         else
            Time.Second <- Time.Second + 1
      else
         if not (newHour = NO_HOUR) then Time.Hour <- newHour
         if not (newMinute = NO_MINUTE) then Time.Minute <- newMinute
         if not (newSecond = NO_SECOND) then Time.Second <- newSecond

      Time
 
   //This function converts the specified hand tip position to an angle relative to the clock's center.
   let private GetAngle handTipX handTipY =
      let HandTip = new Point(handTipX - CLOCK_X, handTipY - CLOCK_Y)
      let Hypotenuse = Math.Sqrt(((HandTip.X |> float) ** 2.0) + ((HandTip.Y |> float) ** 2.0))
      let Cosine = Math.Min((HandTip.X |> float), Hypotenuse) / Math.Max((HandTip.X |> float), Hypotenuse)
      let Sine = Math.Min((HandTip.Y |> float), Hypotenuse) / Math.Max((HandTip.Y |> float), Hypotenuse)
      let Asine = Math.Asin(Sine)
      let Angle = if (Sine <= 0.0 && Cosine <= 0.0) || (Sine >= 0.0 && Cosine <= 0.0) then
                     Math.PI - Asine
                  else if Sine <= 0.0 && Cosine >= 0.0 then
                     (Math.PI * 2.0) + Asine
                  else if Sine >= 0.0 && Cosine >= 0.0 then
                     Asine
                  else
                     0.0            
      (Angle * (180.0 / Math.PI)) |> int

   //This class contains the procedures for drawing an analog clock.
   type public AnalogClockControl() as Me =
      inherit UserControl()  
      do Me.Control_Initialize
      
      //This procedure draws an analog clock displaying the specified time.
      member private Me.DrawClock (displayedTime:TimeRec) =
         let Canvas = Me.CreateGraphics()
                  
         Canvas.DrawLine(new Pen(Me.BackColor, CLOCK_LINE_WIDTH), CLOCK_X, CLOCK_Y, HourHandX, HourHandY)
         Canvas.DrawLine(new Pen(Me.BackColor, CLOCK_LINE_WIDTH), CLOCK_X, CLOCK_Y, MinuteHandX, MinuteHandY)
         Canvas.DrawLine(new Pen(Me.BackColor, CLOCK_LINE_WIDTH), CLOCK_X, CLOCK_Y, SecondHandX, SecondHandY)

         for HourOnFace = 0 to 11 do
            let HourAsRadians = (((HourOnFace * HOURS_TO_DEGREES) + TWELVE_HOUR_ANGLE) |> float) / TO_RADIANS
            let MarkLength = if HourOnFace % LARGE_MARK_INTERVAL = 0 then LARGE_MARK_LENGTH else SMALL_MARK_LENGTH            
            Canvas.DrawLine(new Pen(Color.Yellow, CLOCK_LINE_WIDTH), ((Math.Cos(HourAsRadians) * CLOCK_SIZE) |> int) + CLOCK_X, ((Math.Sin(HourAsRadians) * CLOCK_SIZE) |> int) + CLOCK_Y, ((Math.Cos(HourAsRadians) * (CLOCK_SIZE - MarkLength)) |> int) + CLOCK_X, ((Math.Sin(HourAsRadians) * (CLOCK_SIZE - MarkLength)) |> int) + CLOCK_Y)
            Canvas.DrawEllipse(new Pen(Color.Blue, CLOCK_LINE_WIDTH), 12, 12, (CLOCK_SIZE |> int) * 2, (CLOCK_SIZE |> int) * 2)

         let HourAsRadians = ((((displayedTime.Hour |> float) + ((displayedTime.Minute |> float) * MINUTES_TO_FRACTION)) * (HOURS_TO_DEGREES |> float))+ (TWELVE_HOUR_ANGLE |> float)) / TO_RADIANS
         let SecondAsRadians = (((displayedTime.Second * SECONDS_TO_DEGREES) + TWELVE_HOUR_ANGLE) |> float) / TO_RADIANS
         let MinuteAsRadians = (((displayedTime.Minute * MINUTES_TO_DEGREES) + TWELVE_HOUR_ANGLE) |> float) / TO_RADIANS

         HourHandX <- (((Math.Cos(HourAsRadians) * HOUR_HAND_LENGTH) |> int) + CLOCK_X)
         HourHandY <- (((Math.Sin(HourAsRadians) * HOUR_HAND_LENGTH)  |> int) + CLOCK_Y)
         MinuteHandX <- (((Math.Cos(MinuteAsRadians) * MINUTE_HAND_LENGTH) |> int) + CLOCK_X)
         MinuteHandY <- (((Math.Sin(MinuteAsRadians) * MINUTE_HAND_LENGTH) |> int) + CLOCK_Y)
         SecondHandX <- (((Math.Cos(SecondAsRadians) * SECOND_HAND_LENGTH) |> int) + CLOCK_X)
         SecondHandY <- (((Math.Sin(SecondAsRadians) * SECOND_HAND_LENGTH) |> int) + CLOCK_Y)

         Canvas.DrawLine(new Pen(Color.Green, CLOCK_LINE_WIDTH), CLOCK_X, CLOCK_Y, HourHandX, HourHandY)
         Canvas.DrawLine(new Pen(Color.Green, CLOCK_LINE_WIDTH), CLOCK_X, CLOCK_Y, MinuteHandX, MinuteHandY)
         Canvas.DrawLine(new Pen(Color.Red, CLOCK_LINE_WIDTH), CLOCK_X, CLOCK_Y, SecondHandX, SecondHandY)
         Canvas.DrawEllipse(Pens.White, CLOCK_X - (HAND_NUT_SIZE / 2), CLOCK_Y - (HAND_NUT_SIZE / 2), HAND_NUT_SIZE, HAND_NUT_SIZE)         

      //This procedure initializes this window.
      member private Me.Control_Initialize =
         AnalogClockTimer.Tick.AddHandler(new EventHandler (fun sender e -> Me.AnalogClockTimer_Tick(sender, e)))
         ControlToolTip.SetToolTip(Me, "Click near the face's edge or use the plus key to set the time.")
         Me.BackColor <- Color.Black
         Me.Height <- (CLOCK_SIZE * 2.2) |> int
         Me.KeyDown.AddHandler(new KeyEventHandler(fun sender e -> Me.AnalogClockControl_KeyDown(sender, e)))
         Me.MouseUp.AddHandler(new MouseEventHandler(fun sender e -> Me.AnalogClockControl_MouseUp(sender, e)))
         Me.Width <- (CLOCK_SIZE * 2.2) |> int

         Me.DrawClock(CurrentTime false NO_HOUR NO_MINUTE NO_SECOND)

      //This procedure handles the user's keystrokes.
      member private Me.AnalogClockControl_KeyDown(sender:Object, e:KeyEventArgs) = 
         if e.KeyCode = Keys.Add then
            if e.Shift then
               Time.Hour <- if Time.Hour = 11 then 0 else Time.Hour + 1
            else
               Time.Minute <-
                  if Time.Minute = 59 then                     
                     Time.Hour <- if Time.Hour = 11 then 0 else Time.Hour + 1
                     0
                  else
                     Time.Minute + 1
         Me.DrawClock(CurrentTime false Time.Hour Time.Minute NO_SECOND)
 
      //This procedure gives the command to change the time being displayed.
      member private Me.AnalogClockControl_MouseUp(sender:Object, e:MouseEventArgs) = 
         if e.Button = MouseButtons.Middle then
            CurrentTime false (((GetAngle e.X e.Y) - TWELVE_HOUR_ANGLE) / HOURS_TO_DEGREES) 0 NO_SECOND |> ignore
         else if e.Button = MouseButtons.Left then
            CurrentTime false (((GetAngle e.X e.Y) - TWELVE_HOUR_ANGLE) / HOURS_TO_DEGREES) NO_MINUTE NO_SECOND |> ignore
         else if e.Button = MouseButtons.Right then
            CurrentTime false NO_HOUR (((GetAngle e.X e.Y) - TWELVE_HOUR_ANGLE) / MINUTES_TO_DEGREES) NO_SECOND |> ignore

         Me.DrawClock(CurrentTime false NO_HOUR NO_MINUTE NO_SECOND)         

      //This procedure gives the command to display an analog clock.
      member private Me.AnalogClockTimer_Tick(sender:Object, e:EventArgs) =
         Me.DrawClock(CurrentTime true NO_HOUR NO_MINUTE NO_SECOND)

//This module contains the interface window class.
module private InterfaceModule =   
   let private MenuBar = new MenuStrip()                                                                                  //Contains the menu bar.
   let private AnalogClock = new AnalogClockModule.AnalogClockControl(Top = MenuBar.Height)                               //Contains a reference to an instance of the analog clock control.
   let private InformationMenu = new ToolStripMenuItem(Text = "&Information", ShortcutKeys = (Keys.Control ||| Keys.I))   //Contains the information menu item.
   let private ProgramMainMenu = new ToolStripMenuItem(Text = "&Program")                                                 //Contains the program main menu.
   let private QuitMenu = new ToolStripMenuItem(Text = "&Quit", ShortcutKeys = (Keys.Control ||| Keys.Q))                 //Contains the quit menu item.

   //This class contains this program's main interface window.
   type public InterfaceWindow() as Me =
      inherit Form()
      do Me.Form_Initialize

      //This procedure initializes this window.
      member public Me.Form_Initialize =       
         InformationMenu.Click.AddHandler(new EventHandler (fun sender e -> Me.InformationMenu_Click(sender, e)))
         Me.Controls.AddRange([|AnalogClock; MenuBar|])
         Me.FormBorderStyle <- FormBorderStyle.Fixed3D
         Me.MaximizeBox <- false
         Me.StartPosition <- FormStartPosition.CenterScreen
         Me.Text <- ProgramInformation.ProductName + " v" + ProgramInformation.ProductVersion + " by: " + ProgramInformation.CompanyName 
         MenuBar.Items.Add(ProgramMainMenu) |> ignore
         ProgramMainMenu.DropDownItems.Add(InformationMenu) |> ignore
         ProgramMainMenu.DropDownItems.Add(QuitMenu) |> ignore
         QuitMenu.Click.AddHandler(new EventHandler (fun sender e -> Me.QuitMenu_Click(sender, e)))

         let mutable NewSize = AnalogClock.Size
         NewSize.Height <- NewSize.Height + MenuBar.Height
         Me.ClientSize <- NewSize

      //This procedure displays information about this program.
      member private Me.InformationMenu_Click(sender:Object, e:EventArgs) = 
         MessageBox.Show(ProgramInformation.Comments, Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Information) |> ignore

      //This procedure closes this window.
      member private Me.QuitMenu_Click(sender:Object, e:EventArgs) = 
         Me.Close()

//This module contains this program's entry point.
module private CoreModule = 
   [<STAThread>]
   do Application.Run(new InterfaceModule.InterfaceWindow())

