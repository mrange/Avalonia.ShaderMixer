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

namespace Lib.ShaderMixer
#nowarn "9"

open FSharp.NativeInterop

open Silk.NET.OpenAL

type AudioSpecification =
  | MonoFloat8
  | MonoFloat16
  | StereoFloat8
  | StereoFloat16


type AudioMixer =
  {
    AudioSpecification  : AudioSpecification
    Frequency           : int
    Looping             : bool
    AudioBits           : byte[]
  }

type OpenALAudioMixer  =
  {
    AudioMixer : AudioMixer
    Device     : nativeptr<Device>
    Context    : nativeptr<Context>
    Buffer     : uint32
    Source     : uint32
    Al         : AL
    Alc        : ALContext
  }


module AudioMixer =
  module internal Internals =
    let checkAL (al : AL) : unit =

      let err = al.GetError ()
      if err <> AudioError.NoError then
        failwithf "OpenAL is in an error state: %A" err

    let assertAL (al : AL) : unit =

#if DEBUG
      let err = al.GetError ()
      assert (err = AudioError.NoError)
#else
      ()
#endif

    let checkALC (alc : ALContext) device : unit =

      let err = alc.GetError device
      if err <> ContextError.NoError then
        failwithf "OpenAL is in an error state: %A" err

    let assertALC (alc : ALContext) device : unit =

#if DEBUG
      let err = alc.GetError device
      assert (err = ContextError.NoError)
#else
      ()
#endif

  open Internals

  let setupAudioMixer
    (audioMixer : AudioMixer)
    : OpenALAudioMixer =

    let al      = AL.GetApi ()
    let alc     = ALContext.GetApi ()

    checkAL al

    let device  = alc.OpenDevice ""
    checkAL   al
    checkALC  alc device


    let context = alc.CreateContext (device, NativePtr.nullPtr)
    checkAL   al
    checkALC  alc device

    let result  = alc.MakeContextCurrent context
    checkAL   al
    checkALC  alc device

    if not result then
      failwith "Failed to make OpenAL context current"

    let buffer = al.GenBuffer ()
    checkAL   al
    checkALC  alc device

    let source = al.GenSource ()
    checkAL   al
    checkALC  alc device

    use ptr = fixed audioMixer.AudioBits

    let bufferFormat =
      match audioMixer.AudioSpecification with
      | MonoFloat8    -> BufferFormat.Mono8
      | MonoFloat16   -> BufferFormat.Mono16
      | StereoFloat8  -> BufferFormat.Stereo8
      | StereoFloat16 -> BufferFormat.Stereo16
    al.BufferData (buffer, bufferFormat, NativePtr.toVoidPtr ptr, audioMixer.AudioBits.Length, audioMixer.Frequency)
    checkAL   al
    checkALC  alc device

    al.SetSourceProperty (source, SourceInteger.Buffer, buffer)
    checkAL   al
    checkALC  alc device

    al.SetSourceProperty (source, SourceBoolean.Looping, audioMixer.Looping)
    checkAL   al
    checkALC  alc device

    al.SourcePlay source
    checkAL   al
    checkALC  alc device

    {
      AudioMixer  = audioMixer
      Device      = device
      Context     = context
      Buffer      = buffer
      Source      = source
      Al          = al
      Alc         = alc
    }

  let tearDownAudioMixer
    (audioMixer : OpenALAudioMixer)
    : unit =

    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    assertAL   al
    assertALC  alc audioMixer.Device

    al.SourceStop       audioMixer.Source
    assertAL   al
    assertALC  alc audioMixer.Device

    al.DeleteSource     audioMixer.Source
    assertAL   al
    assertALC  alc audioMixer.Device

    al.DeleteBuffer     audioMixer.Buffer
    assertAL   al
    assertALC  alc audioMixer.Device

    alc.DestroyContext  audioMixer.Context
    assertAL   al
    assertALC  alc audioMixer.Device

    let result = alc.CloseDevice     audioMixer.Device
    assertAL   al

    assert result

    alc.Dispose ()
    al.Dispose ()

  let getAudioPositionInSec
    (audioMixer : OpenALAudioMixer)
    : float32 =
    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    checkAL   al
    checkALC  alc audioMixer.Device

    let mutable pos = 0.F
    audioMixer.Al.GetSourceProperty (audioMixer.Source, SourceFloat.SecOffset, &pos)
    checkAL   al
    checkALC  alc audioMixer.Device

    pos

