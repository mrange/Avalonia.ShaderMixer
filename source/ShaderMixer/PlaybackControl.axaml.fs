namespace ShaderMixer

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Markup.Xaml
open Avalonia.Threading

open System
open System.Diagnostics
open System.ComponentModel
open System.Globalization
open System.Runtime.CompilerServices
open System.Text
open System.Windows.Input

open Lib.ShaderMixer

type ExecuteCommand(action : obj -> unit) =
  class
    let canExecuteChanged = new Event<EventHandler, EventArgs>()
    interface ICommand with
      member x.CanExecute p       = true
      member x.Execute    p       = action p
      [<CLIEvent>]
      member x.CanExecuteChanged  = canExecuteChanged.Publish
  end

type PlaybackControlViewModel(mixer: Mixer, playback : Playback) =
  class
    let propertyChanged = new Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()

    let culture = CultureInfo.InvariantCulture
    let sb      = StringBuilder 32

    let mutable beatLabel         = "0.00"
    let mutable currentTimeLabel  = "0.00s"
    let mutable pitchLabel        = "1.0x"

    member x.BeatLabel  = beatLabel

    member x.BPM        = mixer.BPM

    member x.CurrentTime
      with get ()       = playback.Time ()
      and set  value    =
        ignore <| playback.SetTime value

        ignore <| sb.Clear ()
        ignore <| sb.AppendFormat (culture, "{0:0.00}s", value)
        currentTimeLabel  <- sb.ToString ()

        ignore <| sb.Clear ()
        ignore <| sb.AppendFormat (culture, "{0:0.00}", mixer.TimeToBeat value)
        beatLabel         <- sb.ToString ()

        x.OnPropertyChanged ()
        x.OnPropertyChanged "CurrentTimeLabel"
        x.OnPropertyChanged "BeatLabel"


    member x.CurrentTimeLabel = currentTimeLabel

    member x.EndTime          = mixer.BeatToTime (float32 mixer.LengthInBeats)

    member x.Pitch
      with get ()       = playback.Pitch ()
      and set  value    = 
        playback.SetPitch value

        ignore <| sb.Clear ()
        ignore <| sb.AppendFormat (culture, "{0:0.0}x", value)
        pitchLabel          <- sb.ToString ()

        x.OnPropertyChanged ()
        x.OnPropertyChanged "PitchLabel"

    member x.PitchLabel         = pitchLabel

    member x.PauseCommand       = ExecuteCommand (fun p ->
        playback.Pause ()
      )

    member x.PlayCommand       = ExecuteCommand (fun p ->
        playback.Play ()
      )

    member x.ResetPitchCommand  =
      ExecuteCommand (fun p -> 
        x.Pitch <- 1.F
      )

    member x.DragStarted () =
      playback.StartSeeking ()


    member x.DragCompleted () =
      playback.StopSeeking ()

    member x.OnPropertyChanged ([<CallerMemberName>] ?propertyName) =
        let propertyName = defaultArg propertyName ""
        propertyChanged.Trigger (x, PropertyChangedEventArgs propertyName)

    interface INotifyPropertyChanged with
      [<CLIEvent>]
      member x.PropertyChanged = propertyChanged.Publish
  end


type PlaybackControl (mixer: Mixer, playback : Playback) as this = 
    inherit UserControl ()

    let viewModel = PlaybackControlViewModel (mixer, playback)

    let onRefresh o e =
      viewModel.CurrentTime <- playback.Time ()

    let refreshTimer = 
      let dt = DispatcherTimer ()
      dt.Interval <- TimeSpan.FromMilliseconds 500
      dt.Tick.AddHandler (EventHandler onRefresh)
      dt

    do this.InitializeComponent()

    member private x.InitializeComponent() =
        AvaloniaXamlLoader.Load(x)

        let timeSlider = x.GetControl<Slider> "_timeSlider"

        let dragStartedHandler o a =
          viewModel.DragStarted ()

        let dragCompletedHandler o a =
          viewModel.DragCompleted ()

        timeSlider.AddHandler (
            Thumb.DragStartedEvent
          , EventHandler<VectorEventArgs> dragStartedHandler
          , RoutingStrategies.Bubble
          )

        timeSlider.AddHandler (
            Thumb.DragCompletedEvent
          , EventHandler<VectorEventArgs> dragCompletedHandler
          , RoutingStrategies.Bubble
          )

        this.DataContext <- viewModel

        refreshTimer.IsEnabled <- true
