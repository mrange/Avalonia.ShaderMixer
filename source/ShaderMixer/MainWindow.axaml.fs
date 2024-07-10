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
open Avalonia.Markup.Xaml

open Lib.ShaderMixer

type MainWindow () as this =
  inherit Window ()

  do this.InitializeComponent()

  member private this.InitializeComponent() =
#if DEBUG
    this.AttachDevTools()
#endif
    AvaloniaXamlLoader.Load(this)

    let shaderMixer = ShaderMixerControl Setup.mixer
    shaderMixer.RenderScaling <- this.RenderScaling

    match this.GetControl<ContentControl> "_content" with
    | null  -> ()
    | cc    -> cc.Content          <- shaderMixer
