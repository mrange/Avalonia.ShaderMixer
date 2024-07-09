namespace Lib.ShaderMix

open System
open System.Diagnostics
open System.Numerics

open Avalonia
open Avalonia.Controls
open Avalonia.OpenGL
open Avalonia.Platform
open Avalonia.Rendering.Composition
open Avalonia.Skia

module internal Internals =
  type ShaderMixMessage =
  | ChangePresenterMessage      of PresenterID
  | ChangeRenderScalingMessage  of float
  | ChangeSceneMessage          of SceneID
  | DisposeMessage

  type ShaderMixerVisual(start : Stopwatch, scenes : Scenes, initialRenderScaling : float, initialPresenter : PresenterID, initialScene : SceneID) =
    class
      inherit CompositionCustomVisualHandler()

      let mutable presenterID                           = initialPresenter
      let mutable sceneID                               = initialScene
      let mutable frameNo                               = 0
      let mutable renderScaling                         = initialRenderScaling
      let mutable openGLScenes  : OpenGLScenes voption  = ValueNone

      let dispose () =
        let oglScenes = openGLScenes
        openGLScenes <- ValueNone
        match oglScenes with
        | ValueNone       -> ()
        | ValueSome ogls  -> OpenGL.tearDownOpenGLScenes ogls

      interface IDisposable with
        member x.Dispose () = dispose ()

      override x.OnAnimationFrameUpdate () =
        x.Invalidate ()
        base.OnAnimationFrameUpdate ()

      override x.OnMessage message =
        match message with
        | :? ShaderMixMessage as sm ->
          match sm with
          | ChangePresenterMessage      pid -> presenterID    <- pid
          | ChangeRenderScalingMessage  rs  -> renderScaling  <- rs
          | ChangeSceneMessage          sid -> sceneID        <- sid
          | DisposeMessage                  -> dispose ()
        | _                                 -> ()

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

                  let oglss = 
                    match openGLScenes with
                    | ValueNone       -> 
                      OpenGL.setupOpenGLScenes glContext gl resolution scenes
                    | ValueSome oglss -> 
                      if not (oglss.ContextIsSame glContext) then
                        OpenGL.tearDownOpenGLScenes oglss
                        OpenGL.setupOpenGLScenes glContext gl resolution scenes
                      else if resolution <> oglss.Resolution then
                        OpenGL.resizeOpenGLScenes resolution oglss
                      else
                        oglss

                  openGLScenes <- ValueSome oglss
                  
                  let time = float32 start.ElapsedMilliseconds/1000.F
                  OpenGL.renderOpenGLScenes time frameNo oglss presenterID sceneID

                  frameNo <- frameNo + 1
                | _                           ->
                  ()

    end
open Internals

type ShaderMixerControl(scenes : Scenes, initialPresenter : PresenterID, initialScene : SceneID) =
  class
    inherit Control()

    let start                                                       = Stopwatch.StartNew ()
    let mutable presenterID                                         = initialPresenter
    let mutable sceneID                                             = initialScene
    let mutable shaderMixerVisual : CompositionCustomVisual voption = ValueNone
    let mutable renderScaling                                       = 1.

    member x.Presenter
      with get () = presenterID
      and  set v  = 
        presenterID <- v
        match shaderMixerVisual with
        | ValueNone     -> ()
        | ValueSome vis -> vis.SendHandlerMessage (ChangePresenterMessage presenterID)

    member x.Scene
      with get () = sceneID
      and  set v  = 
        sceneID <- v
        match shaderMixerVisual with
        | ValueNone     -> ()
        | ValueSome vis -> vis.SendHandlerMessage (ChangeSceneMessage v)

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
          new ShaderMixerVisual (start, scenes, renderScaling, presenterID, sceneID)
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
