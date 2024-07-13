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

#nowarn "9"

module Setup =
  open Lib.ShaderMixer

  open Avalonia
  open Avalonia.Controls
  open Avalonia.Media
  open Avalonia.Media.Imaging
  open Avalonia.Platform

  open System.Globalization

  open FSharp.NativeInterop

  open Scripting

  let bitmapToMixerBitmapImage 
    (bitmap : Bitmap) 
    : MixerBitmapImage =

    let sz      = bitmap.Size
    let height  = int sz.Height
    let width   = int sz.Width

    let pixels : byte array = Array.create (width*height*4) 0uy

    do
      use ptr = fixed pixels
      bitmap.CopyPixels (
          PixelRect (0,0,width,height)
        , NativePtr.toNativeInt ptr
        , pixels.Length
        , 4*width
        )

    // rtb.Save (@"D:\assets\testing.png")

    let mbi : MixerBitmapImage = 
      {
        Width     = width 
        Height    = height
        Format    = RGBA
        RGBABits  = pixels
      }

    mbi.Validate ()

    mbi
  
  let loadBitmapFromFile
    (fileName : string)
    : MixerBitmapImage =

    use bitmap = new Bitmap (fileName)

    bitmapToMixerBitmapImage bitmap

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
      use _  = dc.PushRenderOptions ro

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

    bitmapToMixerBitmapImage rtb

  let gravitySucksID  = SceneID "gravitySucks"
  let gravitySucks    = basicScene ShaderSources.gravitySucks

  let fireLord1       = loadBitmapFromFile @"d:\assets\ai-firelord-1.jpg"

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
