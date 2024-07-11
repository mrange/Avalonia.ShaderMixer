namespace ShaderMixer

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml

open System.Diagnostics
open System.ComponentModel
open System.Globalization
open System.Runtime.CompilerServices
open System.Text

open Lib.ShaderMixer

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

    member x.BeatLabel
      with get ()       = beatLabel

    member x.BPM
      with get ()       = mixer.BPM

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

    member x.CurrentTimeLabel
      with get ()       = currentTimeLabel

    member x.EndTime
      with get ()       = mixer.BeatToTime (float32 mixer.LengthInBeats)

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

    member x.PitchLabel
      with get ()       = pitchLabel

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

    do this.InitializeComponent()

    member private this.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)

        this.DataContext <- viewModel

