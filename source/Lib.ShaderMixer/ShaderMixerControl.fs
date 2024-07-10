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

open System
open System.Numerics

open Avalonia
open Avalonia.Controls
open Avalonia.OpenGL
open Avalonia.Platform
open Avalonia.Rendering.Composition
open Avalonia.Skia

module internal Internals =
  type ShaderMixMessage =
  | ChangeRenderScalingMessage  of float
  | DisposeMessage

  type ShaderMixerVisual(mixer: Mixer, initialRenderScaling : float, clock: unit -> float32) =
    class
      inherit CompositionCustomVisualHandler()

      let mutable frameNo                               = 0
      let mutable renderScaling                         = initialRenderScaling
      let mutable OpenGLMixer  : OpenGLMixer voption  = ValueNone

      let dispose () =
        let oglm = OpenGLMixer
        OpenGLMixer <- ValueNone
        match oglm with
        | ValueNone       -> ()
        | ValueSome oglm  -> Mixer.tearDownOpenGLMixer oglm

      interface IDisposable with
        member x.Dispose () = dispose ()

      override x.OnAnimationFrameUpdate () =
        x.Invalidate ()
        base.OnAnimationFrameUpdate ()

      override x.OnMessage message =
        match message with
        | :? ShaderMixMessage as sm ->
          match sm with
          | ChangeRenderScalingMessage  rs          -> renderScaling  <- rs
          | DisposeMessage                          -> dispose ()
        | _                                         -> ()

        base.OnMessage message

      override x.OnRender context =
        x.RegisterForNextAnimationFrameUpdate ()

        let bounds      = x.GetRenderBounds ()
        let pixelRect   = PixelRect.FromRect (bounds, renderScaling)


        if pixelRect.Width > 1 && pixelRect.Height > 1 then

          match context.TryGetFeature<ISkiaSharpApiLeaseFeature> () with
          | null              -> ()
          | skiaLeaseFeature  ->
            use skiaLease = skiaLeaseFeature.Lease ()

            if not (isNull skiaLease.GrContext) then

              use platformLease = skiaLease.TryLeasePlatformGraphicsApi ()

              match platformLease with
              | null  -> ()
              | plal  ->
                match plal.Context with
                | :? IGlContext as glContext  ->
                  let gl = glContext.GlInterface
                  let resolution = Vector2 (float32 pixelRect.Width, float32 pixelRect.Height)

                  let oglm =
                    match OpenGLMixer with
                    | ValueNone       ->
                      Mixer.setupOpenGLMixer glContext gl resolution mixer
                    | ValueSome oglm ->
                      if not (oglm.ContextIsSame glContext) then
                        Mixer.tearDownOpenGLMixer oglm
                        Mixer.setupOpenGLMixer glContext gl resolution mixer
                      else if  ((frameNo &&& 0x3F) = 0) && resolution <> oglm.Resolution then
                        // We don't want to resize too often
                        Mixer.resizeOpenGLMixer resolution oglm
                      else
                        oglm

                  OpenGLMixer <- ValueSome oglm

                  let time = clock ()
                  Mixer.renderOpenGLMixer
                    pixelRect
                    0.0F
                    time
                    frameNo
                    oglm

                  frameNo <- frameNo + 1
                | _                           ->
                  ()

    end
open Internals

type ShaderMixerControl(mixer: Mixer, clock: unit -> float32) =
  class
    inherit Control()

    let mutable shaderMixerVisual : CompositionCustomVisual voption = ValueNone
    let mutable renderScaling                                       = 1.


    member x.RenderScaling
      with get () = renderScaling
      and  set v  =
        renderScaling <- v
        match shaderMixerVisual with
        | ValueNone     -> ()
        | ValueSome vis -> vis.SendHandlerMessage (ChangeRenderScalingMessage v)


    override x.OnAttachedToVisualTree e =
      let visual = ElementComposition.GetElementVisual x
      if not (isNull visual) then
        assert shaderMixerVisual.IsNone
        let composedVisual =
          new ShaderMixerVisual (mixer, renderScaling, clock)
          |> visual.Compositor.CreateCustomVisual
        ElementComposition.SetElementChildVisual (x, composedVisual)
        composedVisual.Size <- Vector2 (float32 x.Bounds.Width, float32 x.Bounds.Height)
        shaderMixerVisual <- ValueSome composedVisual

      base.OnAttachedToVisualTree e

    override x.OnDetachedFromVisualTree e =
      assert shaderMixerVisual.IsSome
      match shaderMixerVisual with
      | ValueNone     -> ()
      | ValueSome vis ->
        vis.SendHandlerMessage DisposeMessage
        ElementComposition.SetElementChildVisual (x, null)
        shaderMixerVisual <- ValueNone

      base.OnDetachedFromVisualTree e

    override x.ArrangeOverride finalSize =
      let size = base.ArrangeOverride finalSize
      match shaderMixerVisual with
      | ValueNone     -> ()
      | ValueSome vis ->
        vis.Size <- new Vector2 (float32 size.Width, float32 size.Height)

      size

  end
