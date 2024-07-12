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
module Setup =
  open Lib.ShaderMixer

  open Avalonia
  open Avalonia.Controls
  open Avalonia.Media
  open Avalonia.Media.Imaging

  open System.Globalization

  open Scripting

  let renderText
    (width      : int         )
    (height     : int         )
    (textHeight : int         )
    (fontFamily : string      )
    (fontSize   : float       )
    (texts      : string array)
    =
    use rtb = new RenderTargetBitmap (PixelSize (width,height), Vector (96., 96.))

    do
      let ro = RenderOptions (
          EdgeMode          = EdgeMode.Antialias
        , TextRenderingMode = TextRenderingMode.Antialias
        )
      use dc = rtb.CreateDrawingContext ()
      use popRo = dc.PushRenderOptions ro

      let tf = Typeface fontFamily

      for i = 0 to texts.Length-1 do
        let text = texts.[i]

        let ft = FormattedText (
            text
          , CultureInfo.InvariantCulture
          , FlowDirection.LeftToRight
          , tf
          , fontSize
          , Brushes.White
          )
        ft.TextAlignment  <- TextAlignment.Center
        ft.MaxTextWidth   <- width
        ft.MaxTextHeight  <- height
        ft.MaxLineCount   <- 1

        dc.DrawText (ft, Point (0., float (i*textHeight)))

    rtb.Save (@"D:\assets\testing.png")

    ()
  let gravitySucksID  = SceneID "gravitySucks"
  let gravitySucks    = basicScene ShaderSources.gravitySucks

  let mixer : Mixer =
    {
      NamedBitmapImages = Map.empty
      NamedPresenters   = defaultPresenters
      NamedScenes       =
        [|
          blackSceneID    , blackScene
          redSceneID      , redScene
          gravitySucksID  , gravitySucks
        |] |> Map.ofArray
      BPM           = 142.F
      LengthInBeats = 576

      InitialPresenter  = faderPresenterID
      InitialStage0     = blackSceneID
      InitialStage1     = gravitySucksID

      Script        =
        [|
          0   , ApplyFader  <| fadeToStage1 4.F
        |]
    }
