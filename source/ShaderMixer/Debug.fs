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

#if DEBUG
module Debug =

  open Lib.ShaderMixer

  open SixLabors.ImageSharp
  open SixLabors.ImageSharp.PixelFormats
  open SixLabors.ImageSharp.Formats.Png

  let saveAsPng 
    (bitmapImage  : MixerBitmapImage)
    (fileName     : string          )
    : unit =

    bitmapImage.Validate ()

    let encoder = PngEncoder ()

    match bitmapImage.Format with
    | RGBA8 ->
      use image = Image.LoadPixelData<Rgba32> (bitmapImage.Bits, bitmapImage.Width, bitmapImage.Height)
      image.Save (fileName, encoder)
    | R8    -> 
      use image = Image.LoadPixelData<L8> (bitmapImage.Bits, bitmapImage.Width, bitmapImage.Height)
      image.Save (fileName, encoder)

#endif