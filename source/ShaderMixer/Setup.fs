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

  module Loops =
    let rec flipXZ (pixels : byte array) i =
      if i < pixels.Length then
        let tmp     = pixels.[i]
        pixels.[i]  <- pixels.[i + 2]
        pixels.[i+2]<- tmp
        flipXZ pixels (i + 4)

    let rec flipYAxis bwidth (line : byte array) (pixels : byte array) f t =
      if f < t then
        let fi = f*bwidth
        let ti = t*bwidth
        System.Array.Copy (pixels, fi, line  ,  0, bwidth)
        System.Array.Copy (pixels, ti, pixels, fi, bwidth)
        System.Array.Copy (line  ,  0, pixels, ti, bwidth)

        flipYAxis bwidth line pixels (f + 1) (t - 1)

  let dpi96 = Vector (96., 96.)

  let bitmapImageToBitmap 
    (bitmapImage : MixerBitmapImage)
    : Bitmap =

    use ptr = fixed bitmapImage.Bits
    
    let pf =
      match bitmapImage.Format with
      | RGBA8 -> PixelFormats.Rgba8888
      | R8    -> PixelFormats.Gray8

    new Bitmap (
        pf
      , AlphaFormat.Unpremul
      , NativePtr.toNativeInt ptr
      , PixelSize (bitmapImage.Width, bitmapImage.Height)
      , dpi96
      , bitmapImage.Width*bitmapImage.PixelByteSize ()
      )

  let bitmapToMixerBitmapImage 
    (bitmap     : Bitmap    )
    (flipYAxis  : bool      )
    : MixerBitmapImage =

    let flipXZ =
      if bitmap.Format.HasValue then
        let pf = bitmap.Format.Value
        if    pf = PixelFormats.Bgra8888 then true
        elif  pf = PixelFormats.Rgba8888 then false
        else  failwithf "Unsupported pixel format on Bitmap: %s" (pf.ToString ())
      else
        failwith "No pixel format set on bitmap"

    let sz      = bitmap.Size
    let height  = int sz.Height
    let width   = int sz.Width
    let bwidth  = width*4

    let pixels : byte array = Array.create (bwidth*height) 0uy

    do
      use ptr = fixed pixels
      bitmap.CopyPixels (
          PixelRect (0,0,width,height)
        , NativePtr.toNativeInt ptr
        , pixels.Length
        , 4*width
        )

    // rtb.Save (@"D:\assets\testing.png")

    let line : byte array = Array.create bwidth 0uy

    if flipXZ then
      Loops.flipXZ pixels 0
    if flipYAxis then
      Loops.flipYAxis bwidth line pixels 0 (height - 1)

    let mbi : MixerBitmapImage = 
      {
        Width     = width 
        Height    = height
        Format    = RGBA8
        Bits      = pixels
      }

    mbi.Validate ()

    mbi
  
  let loadBitmapFromFile
    (fileName : string)
    : MixerBitmapImage =

    use bitmap = new Bitmap (fileName)

    bitmapToMixerBitmapImage bitmap true

  let renderTextImage
    (width      : int         )
    (height     : int         )
    (textHeight : int         )
    (fontFamily : string      )
    (fontSize   : float       )
    (texts      : string array)
    =
    use rtb = new RenderTargetBitmap (PixelSize (width, height), dpi96)

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

    bitmapToMixerBitmapImage rtb true

  let createMixer () : Mixer = 
    let gravitySucksID  = SceneID "gravitySucks"
    let jezID           = SceneID "jez"

    let darkHero1ID       = BitmapImageID "ai-dark-hero-1"
    let darkHero1         = loadBitmapFromFile @"d:\assets\ai-dark-hero-1.jpg"
    let impulseMembersID  = BitmapImageID "impulse-members"

    let upScale = 4
    let impulseMembers    = 
      renderTextImage 
        (upScale*512)
        (upScale*512) 
        (upScale*128)
        "Helvetica" 
        (float (upScale*84 ))
        [|
          "Jez"
          "Glimglam"
          "Lance"
          "Longshot" 
        |]

    let impulseMembersDistanceField = 
      DistanceField.createDistanceField
        (upScale*64)
        0.25
        0
        impulseMembers

    use bmp = bitmapImageToBitmap impulseMembersDistanceField
    let smallerBmp = bmp.CreateScaledBitmap (PixelSize (512, 512), BitmapInterpolationMode.HighQuality)

    let impulseMembersDistanceField = 
      bitmapToMixerBitmapImage smallerBmp false

#if DEBUG
    Debug.saveAsPng impulseMembers              @"d:\assets\impulse-members.png"
    Debug.saveAsPng impulseMembersDistanceField @"d:\assets\impulse-members-distance.png"
#endif
    let namedBitmapImages = 
      [|
        darkHero1ID     , darkHero1
        impulseMembersID, impulseMembers
      |] |> Map.ofArray

    let gravitySucks    = basicScene ShaderSources.gravitySucks
    let jez             = basicScene ShaderSources.jez
    let jez             =
      { jez with
          Image =
            { jez.Image with
                Channel0 = basicImageBufferChannel' impulseMembersID
            }
      }

    {
      NamedBitmapImages = namedBitmapImages
      NamedPresenters   = defaultPresenters
      NamedScenes       =
        [|
          blackSceneID    , blackScene
          redSceneID      , redScene
          gravitySucksID  , gravitySucks
          jezID           , jez
        |] |> Map.ofArray
      BPM           = 142.F
      LengthInBeats = 576

      InitialPresenter  = faderPresenterID
      InitialStage0     = blackSceneID
      InitialStage1     = jezID

      Script        =
        [|
          0   , ApplyFader  <| fadeToStage1 4.F
        |]
    }
