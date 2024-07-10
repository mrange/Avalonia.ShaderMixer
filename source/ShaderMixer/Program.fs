(*
ShaderMixer for Avalonia - Mixes ShaderToy like shaders
Copyright (C) 2024  Mårten Rånge

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

open System
open Avalonia

open Lib.ShaderMixer

module Program =

  [<CompiledName "BuildAvaloniaApp">]
  let buildAvaloniaApp () =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .WithInterFont()
        .LogToTrace(areas = Array.empty)

  [<EntryPoint; STAThread>]
  let main argv =
    let audioMixer : AudioMixer =
      let tau = 2.*Math.PI |> float32
      let hz  = 440.F*tau/44100.F
      let audio : byte array = Array.init 44100 (fun i -> byte (128.F+127.F*sin (hz*float32 i)))


      {
        AudioChannels       = Mono
        Frequency           = 44100
        Looping             = true
        AudioBits           = AudioBits8 audio
      }

    let openALAudioMixer = AudioMixer.setupOpenALAudioMixer audioMixer
    try
      buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
    finally
      AudioMixer.tearDownOpenALAudioMixer openALAudioMixer

