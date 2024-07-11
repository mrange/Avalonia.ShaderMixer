(*
ShaderMixer for Avalonia - Mixes ShaderToy like shaders
Copyright (C) 2024  M�rten R�nge

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses
*)

namespace ShaderMixer

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Markup.Xaml

open System
open System.Diagnostics

open Lib.ShaderMixer

type MainWindow () as this =
  inherit Window ()

  do this.InitializeComponent()

  member private this.InitializeComponent() =
#if DEBUG
    this.AttachDevTools()
#endif
    AvaloniaXamlLoader.Load(this)

    let sw = Stopwatch.StartNew ()
    let clock () = float32 sw.ElapsedMilliseconds/1000.F

    (*
    let clock () =
      match GlobalState.openALAudioMixer with
      | None      -> 0.F
      | Some oalm -> AudioMixer.getAudioPositionInSec oalm
    *)

    let shaderMixer = ShaderMixerControl (Setup.mixer, clock)
    shaderMixer.RenderScaling <- this.RenderScaling

    match this.GetControl<ContentControl> "_content" with
    | null  -> ()
    | cc    -> cc.Content          <- shaderMixer


    match this.GetControl<Grid> "_grid" with
    | null  -> ()
    | grid  ->
      let playBackControl = PlaybackControl Setup.mixer
      playBackControl.HorizontalAlignment <- HorizontalAlignment.Stretch
      playBackControl.VerticalAlignment   <- VerticalAlignment.Bottom
      grid.Children.Add playBackControl
