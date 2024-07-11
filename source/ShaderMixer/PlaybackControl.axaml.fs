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

type PlaybackControlViewModel(mixer: Mixer)
  =
  class
    let propertyChanged = new Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()

    let culture = CultureInfo.InvariantCulture
    let sb      = StringBuilder 32

    let mutable beatLabel         = "0.00"
    let mutable currentTime       = 0.F
    let mutable currentTimeLabel  = "0.00s"
    let mutable pitch             = 1.F
    let mutable pitchLabel        = "1.0x"
    let mutable isPlaying         = true
    let mutable isDragging        = false

    let stopMusic p =
      match GlobalState.openALAudioMixer with
      | None         -> ()
      | Some   oalm  ->
        if not isDragging then
          AudioMixer.pauseAudio oalm

    let startMusic p =
      match GlobalState.openALAudioMixer with
      | None         -> ()
      | Some   oalm  ->
        if not isDragging then
          AudioMixer.playAudio oalm


    member x.BeatLabel  = beatLabel

    member x.BPM        = mixer.BPM

    member x.CurrentTime
      with get ()       = currentTime
      and set  value    =
        if currentTime <> value then
          currentTime       <- value

          ignore <| sb.Clear ()
          ignore <| sb.AppendFormat (culture, "{0:0.00}s", value)
          currentTimeLabel  <- sb.ToString ()

          ignore <| sb.Clear ()
          ignore <| sb.AppendFormat (culture, "{0:0.00}", mixer.TimeToBeat value)
          beatLabel         <- sb.ToString ()

          x.OnPropertyChanged ()
          x.OnPropertyChanged "CurrentTimeLabel"
          x.OnPropertyChanged "BeatLabel"

          if isDragging then
            match GlobalState.openALAudioMixer with
            | None         -> ()
            | Some   oalm  ->
              AudioMixer.setAudioPositionInSec oalm value

    member x.CurrentTimeLabel = currentTimeLabel

    member x.EndTime          = mixer.BeatToTime (float32 mixer.LengthInBeats)

    member x.Pitch
      with get ()       = pitch
      and set  value    =
        if pitch <> value then
          pitch               <- value

          ignore <| sb.Clear ()
          ignore <| sb.AppendFormat (culture, "{0:0.0}x", value)
          pitchLabel          <- sb.ToString ()

          x.OnPropertyChanged ()
          x.OnPropertyChanged "PitchLabel"

          match GlobalState.openALAudioMixer with
          | None         -> ()
          | Some   oalm  ->
            AudioMixer.setAudioPitch oalm value

    member x.PitchLabel         = pitchLabel

    member x.PauseCommand       = ExecuteCommand (fun p ->
        isPlaying <- false
        stopMusic ()
      )

    member x.PlayCommand       = ExecuteCommand (fun p ->
        isPlaying <- true
        startMusic ()
      )

    member x.ResetPitchCommand  =
      ExecuteCommand (fun p -> 
        x.Pitch <- 1.F
      )

    member x.DragStarted () =
      stopMusic ()
      isDragging <- true


    member x.DragCompleted () =
      isDragging <- false
      if isPlaying then startMusic () else stopMusic ()

    member x.OnPropertyChanged ([<CallerMemberName>] ?propertyName) =
        let propertyName = defaultArg propertyName ""
        propertyChanged.Trigger (x, PropertyChangedEventArgs propertyName)

    interface INotifyPropertyChanged with
      [<CLIEvent>]
      member x.PropertyChanged = propertyChanged.Publish
  end


type PlaybackControl (mixer: Mixer) as this = 
    inherit UserControl ()

    let viewModel = PlaybackControlViewModel mixer

    let onRefresh o e =
      match GlobalState.openALAudioMixer with
      | None         -> ()
      | Some   oalm  ->
        viewModel.CurrentTime <- AudioMixer.getAudioPositionInSec oalm

    let refreshTimer = 
      let dt = DispatcherTimer ()
      dt.Interval <- TimeSpan.FromMilliseconds 1000
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
