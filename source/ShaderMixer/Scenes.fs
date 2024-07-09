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

module Scenes
open Lib.ShaderMixer

let faderID         = Mixer.faderPresenterID
let blackID         = Mixer.redSceneID
let redID           = Mixer.redSceneID
let gravitySucksID  = SceneID "gravitySucks"

let gravitySucks    =
  {
    Defines = [||]
    Common  = None
    BufferA = None
    BufferB = None
    BufferC = None
    BufferD = None
    Image   =
      {
        FragmentSource  = ShaderSources.gravitySucks
        Channel0        = None
        Channel1        = None
        Channel2        = None
        Channel3        = None
      }
  }

let mixer : Mixer =
  {
    NamedBitmapImages = Map.empty
    NamedPresenters = Mixer.defaultPresenters
    NamedScenes =
      [|
        Mixer.blackSceneID , Mixer.blackScene
        Mixer.redSceneID   , Mixer.redScene
        gravitySucksID     , gravitySucks
      |] |> Map.ofArray
    BPM           = 60
    LengthInBeats = 60
    Script        =
      [|
        0   , SelectPresenter faderID
        0   , SelectStage0    blackID
        0   , SelectStage1    gravitySucksID
        0   , FadeToStage1    4
      |]
  }
