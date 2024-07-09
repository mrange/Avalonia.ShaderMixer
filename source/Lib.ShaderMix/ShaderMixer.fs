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

namespace Lib.ShaderMix
#nowarn "9"

open Lib.ShaderMixer.OpenGLExt

open System
open System.Diagnostics
open System.Numerics
open System.Runtime.InteropServices
open System.Text

open FSharp.NativeInterop
open FSharp.Core.Printf

open Avalonia
open Avalonia.OpenGL

open type Avalonia.OpenGL.GlConsts

open type Lib.ShaderMixer.OpenGLExt.GlConstsExt

type BitmapImage =
  {
    Width     : int
    Height    : int
    RGBABits  : byte[]
  }

  member x.Validate () =
    if x.Width*x.Height*4 <> x.RGBABits.Length then
      failwithf "BitmapImage dimensions don't match the bits"

type BitmapImageID  = BitmapImageID of string
type PresenterID    = PresenterID   of string
type SceneID        = SceneID       of string

type ChannelSource =
  | BufferA
  | BufferB
  | BufferC
  | BufferD
  | Image0
  | BitmapImage of  BitmapImageID

type ChannelFilter =
  | Nearest
  | Linear

type ChannelWrap =
  | Clamp
  | Repeat
  | MirroredRepeat

type BufferChannel = 
  {
    Filter  : ChannelFilter
    Source  : ChannelSource
    Wrap    : ChannelWrap
  }

type SceneBuffer = 
  {
    FragmentSource  : string
    Channel0        : BufferChannel option
    Channel1        : BufferChannel option
    Channel2        : BufferChannel option
    Channel3        : BufferChannel option
  }

type Presenter = 
  {
    FragmentSource  : string
    Defines         : string array
    Channel0        : BufferChannel
  }

type Scene =
  {
    Defines       : string array
    Common        : string option
    BufferA       : SceneBuffer option
    BufferB       : SceneBuffer option
    BufferC       : SceneBuffer option
    BufferD       : SceneBuffer option
    Image         : SceneBuffer
  }

type Mixer =
  {
    NamedBitmapImages : Map<BitmapImageID , BitmapImage >
    NamedPresenters   : Map<PresenterID   , Presenter   >
    NamedScenes       : Map<SceneID       , Scene       >
  }

[<Struct>]
type OpenGLBuffer         =
  {
    BufferID      : int
    Target        : int
    ElementSize   : int
  }

[<Struct>]
type OpenGLFrameBuffer    =
  {
    FrameBufferID : int
  }

[<Struct>]
type OpenGLProgram        = 
  {
    ProgramID : int
  }

[<Struct>]
type OpenGLShader         = 
  {
    ShaderID : int
    GLEnum   : int
  }

[<Struct>]
type OpenGLTexture        = 
  {
    TextureID : int
  }

[<Struct>]
type OpenGLVertexArray    = 
  {
    VertexArrayID : int
  }

[<Struct>]
type OpenGLUniformLocation= 
  {
    UniformLocationID : int
  }

type OpenGLBitmapImage = 
  {
    BitmapImage : BitmapImage
    Texture     : OpenGLTexture
  }

type OpenGLBufferTexture = 
  {
    Texture0 : OpenGLTexture
    Texture1 : OpenGLTexture
  }
  member x.ForegroundTexture frameNo =
    if (frameNo &&& 1 = 0) then x.Texture0 else x.Texture1

  member x.BackgroundTexture frameNo =
    if (frameNo &&& 1 = 0) then x.Texture1 else x.Texture0

type OpenGLBufferChannel =
  {
    BufferChannel   : BufferChannel
    Location        : OpenGLUniformLocation voption
  }

type OpenGLSceneBuffer =
  {
    SceneBuffer         : SceneBuffer

    Channel0            : OpenGLBufferChannel voption
    Channel1            : OpenGLBufferChannel voption
    Channel2            : OpenGLBufferChannel voption
    Channel3            : OpenGLBufferChannel voption

    VertexShader        : OpenGLShader
    FragmentShader      : OpenGLShader
    Program             : OpenGLProgram

    TimeLocation        : OpenGLUniformLocation voption
    ResolutionLocation  : OpenGLUniformLocation voption
  }

type OpenGLPresenter =
  {
    Presenter           : Presenter

    Channel0            : OpenGLBufferChannel

    VertexShader        : OpenGLShader
    FragmentShader      : OpenGLShader
    Program             : OpenGLProgram

    TimeLocation        : OpenGLUniformLocation voption
    ResolutionLocation  : OpenGLUniformLocation voption
  }

type OpenGLScene =
  {
    Scene           : Scene

    BufferA         : OpenGLSceneBuffer voption
    BufferB         : OpenGLSceneBuffer voption
    BufferC         : OpenGLSceneBuffer voption
    BufferD         : OpenGLSceneBuffer voption
    Image           : OpenGLSceneBuffer
  }

type OpenGLMixer =
  {
    Mixer             : Mixer
    Resolution        : Vector2

    NamedBitmapImages : Map<BitmapImageID , OpenGLBitmapImage >
    NamedPresenters   : Map<PresenterID   , OpenGLPresenter   >
    NamedScenes       : Map<SceneID       , OpenGLScene       >

    BufferA           : OpenGLBufferTexture
    BufferB           : OpenGLBufferTexture
    BufferC           : OpenGLBufferTexture
    BufferD           : OpenGLBufferTexture
    Image0            : OpenGLBufferTexture

    FrameBuffer       : OpenGLFrameBuffer
    IndexBuffer       : OpenGLBuffer
    VertexBuffer      : OpenGLBuffer
    VertexArray       : OpenGLVertexArray

    GlContext         : IGlContext
    Gl                : GlInterface
    GlExt             : GlInterfaceExt
  }

  member x.ContextIsSame (o : IGlContext) =
    obj.ReferenceEquals (x.GlContext, o)


module OpenGL =
  let trace (msg : string) : unit = Trace.WriteLine ("Lib.ShaderMixer: " + msg)
  let tracef fmt                  = kprintf trace fmt
  module internal Internals = 

    let positionLocation  = 0
    let texCoordLocation  = 1

    let vertices : Vertex array = 
        let nv x y z u v : Vertex = 
          let mutable vv = Vertex ()
          vv.Position <- Vector3 (float32 x, float32 y, float32 z)
          vv.TexCoord <- Vector2 (float32 u, float32 v)
          vv
        [|
          nv -1 -1 0 0 0
          nv  1 -1 0 1 0
          nv -1  1 0 0 1
          nv  1  1 0 1 1
        |]
    let indices : uint16 array =
      [|
        uint16 0
        uint16 1
        uint16 2
        uint16 1
        uint16 3
        uint16 2
      |]

    let errorLookup = 
      [|
        GL_NO_ERROR                       , "No error"
        GL_INVALID_ENUM                   , "Invalid enum"
        GL_INVALID_VALUE                  , "Invalid value"
        GL_INVALID_OPERATION              , "Invalid operation"
        GL_INVALID_FRAMEBUFFER_OPERATION  , "Invalid framebuffer operation"
        GL_OUT_OF_MEMORY                  , "Out of memory"
        GL_STACK_UNDERFLOW                , "Stack underflow"
        GL_STACK_OVERFLOW                 , "Stack overflow"
      |] |> Map.ofArray

    let checkGL (gl : GlInterface) : unit =
      let err = gl.GetError ()
      if err <> GL_NO_ERROR then 
        let msg = 
          match errorLookup.TryGetValue err with 
          | false, _ -> sprintf "Unknown error: 0x%x" err
          | true , v -> v

        failwithf "OpenGL is in an error state: %s" msg

    let assertGL (gl : GlInterface) : unit =
#if DEBUG
      let err = gl.GetError ()
      assert (err = GL_NO_ERROR)
#else
      ()
#endif

    let traceIntegerv (gl : GlInterface) (glext : GlInterfaceExt) (id : int) : unit =
      let v = gl.GetIntegerv id
      tracef "GetIntegerv 0x%x = 0x%x" id v

    let createAndBindBuffer<'T when 'T : unmanaged> (gl : GlInterface) (target : int) (vs : 'T array) : OpenGLBuffer =
      let elementSize = sizeof<'T>
      let buffer = 
        {
          BufferID    = gl.GenBuffer ()
          Target      = target
          ElementSize = elementSize
        }

      gl.BindBuffer (buffer.Target, buffer.BufferID)
      checkGL gl

      do
        use ptr = fixed vs
        gl.BufferData(
                buffer.Target
            ,   nativeint (vs.Length*buffer.ElementSize)
            ,   NativePtr.toNativeInt ptr
            ,   GL_STATIC_DRAW
            )
        checkGL gl

      buffer

    let createShader (gl : GlInterface) (glEnum : int) (source : string) : OpenGLShader =
      let shader =
        {
          ShaderID  = gl.CreateShader glEnum
          GLEnum    = glEnum
        }
      let error = gl.CompileShaderAndGetError (shader.ShaderID, source)

      if not (isNull error) then
        failwithf "Failed to compile shader due to: %s" error
      checkGL gl

      shader

    let getUniformLocation (gl : GlInterface) (program : OpenGLProgram) name : OpenGLUniformLocation voption =
      let loc = 
        {
          UniformLocationID = gl.GetUniformLocationString (program.ProgramID, name)
        }
      checkGL gl

      // -1 indicates no matching uniform exists
      if loc.UniformLocationID <> -1 then
        ValueSome loc
      else
        ValueNone
    
    let createTexture (gl : GlInterface) (resolution : Vector2) : OpenGLTexture =
      let texture = 
        {
          TextureID = gl.GenTexture ()
        }
      checkGL gl

      gl.BindTexture (GL_TEXTURE_2D, texture.TextureID)
      checkGL gl

      gl.TexImage2D (GL_TEXTURE_2D, 0, GL_RGBA32F, int resolution.X, int resolution.Y, 0, GL_RGBA, GL_FLOAT, 0)
      checkGL gl

      gl.BindTexture (GL_TEXTURE_2D, 0)
      checkGL gl

      texture

    let createTextureFromBitmapImage (gl : GlInterface) (bitmapImage : BitmapImage) : OpenGLTexture =
      bitmapImage.Validate ()

      let texture = 
        {
          TextureID = gl.GenTexture ()
        }
      checkGL gl

      gl.BindTexture (GL_TEXTURE_2D, texture.TextureID)
      checkGL gl

      do
        use ptr = fixed bitmapImage.RGBABits
        let data = NativePtr.toNativeInt ptr
        gl.TexImage2D (GL_TEXTURE_2D, 0, GL_RGBA32F, bitmapImage.Width, bitmapImage.Height, 0, GL_RGBA, GL_FLOAT, data)
        checkGL gl

      gl.BindTexture (GL_TEXTURE_2D, 0)
      checkGL gl

      texture

    let createOpenGLBufferTexture (gl : GlInterface) (resolution : Vector2) : OpenGLBufferTexture =
      {
        Texture0 = createTexture gl resolution
        Texture1 = createTexture gl resolution
      }

    let createOpenGLBufferChannel (gl : GlInterface) (program : OpenGLProgram) (name : string) (bufferChannel : BufferChannel) : OpenGLBufferChannel =
      {
        BufferChannel = bufferChannel
        Location      = getUniformLocation gl program name
      }

    let createOpenGLBufferChannel' (gl : GlInterface) (program : OpenGLProgram) (name : string)(bufferChannel : BufferChannel option) : OpenGLBufferChannel voption =
      match bufferChannel with
      | None    -> ValueNone
      | Some bc -> createOpenGLBufferChannel gl program name bc |> ValueSome

    let createFragmentSource (common : string option) (defines : string array) (fragmentSource : string) : string =
      let prelude = "#define "
      let common  = match common with | None -> "" | Some s -> s
      let capacity =
          ShaderSources.fragmentShaderSourcePrelude.Length
        + common.Length
        + (Array.sumBy (fun (define : string) -> define.Length + prelude.Length) defines)
        + fragmentSource.Length
        + 2*(3 + defines.Length)        // Line endings
        + 16                            // Some extra bytes to be sure we don't reallocate
      let sb = StringBuilder capacity
      ignore <| sb.AppendLine ShaderSources.fragmentShaderSourcePrelude
      ignore <| sb.AppendLine common
      for define in defines do
        ignore <| sb.Append prelude
        ignore <| sb.AppendLine define
      ignore <| sb.AppendLine fragmentSource

      sb.ToString ()

    let createOpenGLSceneBuffer (gl : GlInterface) (scene : Scene) (sceneBuffer : SceneBuffer) : OpenGLSceneBuffer =
      let vertexShader    = createShader gl GL_VERTEX_SHADER    ShaderSources.vertexShader
      let fragmentShader  = createShader gl GL_FRAGMENT_SHADER  <| createFragmentSource scene.Common scene.Defines sceneBuffer.FragmentSource

      let program         =
        {
          ProgramID = gl.CreateProgram()
        }
      checkGL gl

      gl.AttachShader (program.ProgramID, vertexShader.ShaderID)
      checkGL gl

      gl.AttachShader (program.ProgramID, fragmentShader.ShaderID)
      checkGL gl

      gl.BindAttribLocationString (program.ProgramID, positionLocation, "a_position")
      checkGL gl

      gl.BindAttribLocationString (program.ProgramID, texCoordLocation, "a_texcoord")
      checkGL gl

      let error = gl.LinkProgramAndGetError program.ProgramID
      if not (isNull error) then
        failwithf "Failed to link shader program due to: %s" error
      checkGL gl

      let timeUniformLocation       = getUniformLocation gl program "iTime"
      let resolutionUniformLocation = getUniformLocation gl program "iResolution"

      {
        SceneBuffer         = sceneBuffer
        Channel0            = createOpenGLBufferChannel' gl program "iChannel0" sceneBuffer.Channel0
        Channel1            = createOpenGLBufferChannel' gl program "iChannel1" sceneBuffer.Channel1
        Channel2            = createOpenGLBufferChannel' gl program "iChannel2" sceneBuffer.Channel2
        Channel3            = createOpenGLBufferChannel' gl program "iChannel3" sceneBuffer.Channel3

        VertexShader        = vertexShader
        FragmentShader      = fragmentShader
        Program             = program

        TimeLocation        = timeUniformLocation
        ResolutionLocation  = resolutionUniformLocation
      }

    let createOpenGLSceneBuffer' (gl : GlInterface) (scene : Scene) (sceneBuffer : SceneBuffer option) : OpenGLSceneBuffer voption =
      match sceneBuffer with 
      | None    -> ValueNone
      | Some sb -> createOpenGLSceneBuffer gl scene sb |> ValueSome

    let createOpenGLPresenter (gl : GlInterface) (presenter : Presenter) : OpenGLPresenter =
      let vertexShader    = createShader gl GL_VERTEX_SHADER    ShaderSources.vertexShader
      let fragmentShader  = createShader gl GL_FRAGMENT_SHADER  <| createFragmentSource None presenter.Defines presenter.FragmentSource

      let program         =
        {
          ProgramID = gl.CreateProgram()
        }
      checkGL gl

      gl.AttachShader (program.ProgramID, vertexShader.ShaderID)
      checkGL gl

      gl.AttachShader (program.ProgramID, fragmentShader.ShaderID)
      checkGL gl

      gl.BindAttribLocationString (program.ProgramID, positionLocation, "a_position")
      checkGL gl

      gl.BindAttribLocationString (program.ProgramID, texCoordLocation, "a_texcoord")
      checkGL gl

      let error = gl.LinkProgramAndGetError program.ProgramID
      if not (isNull error) then
        failwithf "Failed to link shader program due to: %s" error
      checkGL gl

      let timeUniformLocation       = getUniformLocation gl program "iTime"
      let resolutionUniformLocation = getUniformLocation gl program "iResolution"

      {
        Presenter           = presenter
        Channel0            = createOpenGLBufferChannel gl program "iChannel0" presenter.Channel0

        VertexShader        = vertexShader
        FragmentShader      = fragmentShader
        Program             = program

        TimeLocation        = timeUniformLocation
        ResolutionLocation  = resolutionUniformLocation
      }

    let createOpenGLScene (gl : GlInterface) (scene : Scene) : OpenGLScene =
      {
        Scene      = scene
        BufferA    = createOpenGLSceneBuffer'  gl scene scene.BufferA
        BufferB    = createOpenGLSceneBuffer'  gl scene scene.BufferB
        BufferC    = createOpenGLSceneBuffer'  gl scene scene.BufferC
        BufferD    = createOpenGLSceneBuffer'  gl scene scene.BufferD
        Image      = createOpenGLSceneBuffer   gl scene scene.Image
      }

    let createOpenGLBitmapImage (gl : GlInterface) (bitmapImage : BitmapImage) : OpenGLBitmapImage =
      {
        BitmapImage = bitmapImage
        Texture     = createTextureFromBitmapImage gl bitmapImage
      }

    let tearDownOpenGLBufferChannel (mixer : OpenGLMixer) (bufferChannel : OpenGLBufferChannel) : unit =
      ()

    let tearDownOpenGLBufferChannel' (mixer : OpenGLMixer) (bufferChannel : OpenGLBufferChannel voption) : unit =
      match bufferChannel with
      | ValueNone     -> ()
      | ValueSome bc  -> tearDownOpenGLBufferChannel mixer bc

    let tearDownOpenGLBufferTexture (mixer : OpenGLMixer) (bufferTexture : OpenGLBufferTexture) : unit =
      let gl    = mixer.Gl

      gl.DeleteTexture bufferTexture.Texture1.TextureID
      assertGL gl

      gl.DeleteTexture bufferTexture.Texture0.TextureID
      assertGL gl

    let tearDownOpenGLBitmapImage (mixer : OpenGLMixer) (bitmapImage : OpenGLBitmapImage) : unit =
      let gl    = mixer.Gl

      gl.DeleteTexture bitmapImage.Texture.TextureID
      assertGL gl

    let tearDownOpenGLSceneBuffer (mixer : OpenGLMixer) (sceneBuffer : OpenGLSceneBuffer) : unit =
      let gl    = mixer.Gl

      tearDownOpenGLBufferChannel' mixer sceneBuffer.Channel3
      tearDownOpenGLBufferChannel' mixer sceneBuffer.Channel2
      tearDownOpenGLBufferChannel' mixer sceneBuffer.Channel1
      tearDownOpenGLBufferChannel' mixer sceneBuffer.Channel0

      gl.DeleteProgram                sceneBuffer.Program.ProgramID
      assertGL gl

      gl.DeleteShader                 sceneBuffer.FragmentShader.ShaderID
      assertGL gl

      gl.DeleteShader                 sceneBuffer.VertexShader.ShaderID
      assertGL gl

    let tearDownOpenGLSceneBuffer' (mixer : OpenGLMixer) (sceneBuffer : OpenGLSceneBuffer voption) : unit =
      match sceneBuffer with
      | ValueNone     -> ()
      | ValueSome sb  -> tearDownOpenGLSceneBuffer mixer sb

    let tearDownOpenGLPresenter (mixer : OpenGLMixer) (presenter : OpenGLPresenter) : unit =
      let gl    = mixer.Gl

      tearDownOpenGLBufferChannel mixer presenter.Channel0

      gl.DeleteProgram presenter.Program.ProgramID
      assertGL gl

      gl.DeleteShader  presenter.FragmentShader.ShaderID
      assertGL gl

      gl.DeleteShader  presenter.VertexShader.ShaderID
      assertGL gl

    let tearDownOpenGLScene (mixer : OpenGLMixer) (scene : OpenGLScene) : unit =
      tearDownOpenGLSceneBuffer' mixer scene.BufferD
      tearDownOpenGLSceneBuffer' mixer scene.BufferC
      tearDownOpenGLSceneBuffer' mixer scene.BufferB
      tearDownOpenGLSceneBuffer' mixer scene.BufferA
      tearDownOpenGLSceneBuffer  mixer scene.Image

    let resizeOpenGLBufferTexture (mixer : OpenGLMixer) (resolution : Vector2) (bufferTexture : OpenGLBufferTexture) : OpenGLBufferTexture =
      let gl = mixer.Gl

      tearDownOpenGLBufferTexture mixer bufferTexture
      createOpenGLBufferTexture   gl    resolution

    let renderOpenGLBufferChannel (mixer : OpenGLMixer) (frameNo : int) (textureUnitNo : int) (bufferChannel : OpenGLBufferChannel) : unit =
      let gl    = mixer.Gl
      let glext = mixer.GlExt

      match bufferChannel.Location with
      | ValueNone     ->  ()
      | ValueSome loc ->
        let sourceTexture =
          match bufferChannel.BufferChannel.Source with
          | BufferA       -> mixer.BufferA.BackgroundTexture frameNo
          | BufferB       -> mixer.BufferB.BackgroundTexture frameNo
          | BufferC       -> mixer.BufferC.BackgroundTexture frameNo
          | BufferD       -> mixer.BufferD.BackgroundTexture frameNo
          | Image0        -> mixer.Image0.BackgroundTexture  frameNo
          | BitmapImage ii-> 
            let image = mixer.NamedBitmapImages.[ii]
            image.Texture
        gl.ActiveTexture (GL_TEXTURE0 + textureUnitNo)
        checkGL gl

        gl.BindTexture (GL_TEXTURE_2D, sourceTexture.TextureID)
        checkGL gl

        match bufferChannel.BufferChannel.Filter with
        | Nearest -> 
          gl.TexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
          gl.TexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
        | Linear  ->
          gl.TexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
          gl.TexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
        checkGL gl
        checkGL gl

        match bufferChannel.BufferChannel.Wrap with
        | Clamp   -> 
          gl.TexParameteri(GL_TEXTURE_2D  , GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
          gl.TexParameteri(GL_TEXTURE_2D  , GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
        | Repeat  ->
          gl.TexParameteri(GL_TEXTURE_2D  , GL_TEXTURE_WRAP_S, GL_REPEAT)
          gl.TexParameteri(GL_TEXTURE_2D  , GL_TEXTURE_WRAP_T, GL_REPEAT)
        | MirroredRepeat ->
          gl.TexParameteri(GL_TEXTURE_2D  , GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT)
          gl.TexParameteri(GL_TEXTURE_2D  , GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT)
        checkGL gl
        checkGL gl
          

        glext.Uniform1i (loc.UniformLocationID, textureUnitNo)
        checkGL gl

    let renderOpenGLBufferChannel' (mixer : OpenGLMixer)  (frameNo : int) (textureUnitNo : int) (bufferChannel : OpenGLBufferChannel voption) : unit =
      match bufferChannel with
      | ValueNone     -> ()
      | ValueSome bc  -> renderOpenGLBufferChannel mixer frameNo textureUnitNo bc

    let renderOpenGLSceneBuffer (mixer : OpenGLMixer) (time : float32) (frameNo : int) (bufferTexture : OpenGLBufferTexture) (sceneBuffer : OpenGLSceneBuffer) : unit =
      let gl    = mixer.Gl
      let glext = mixer.GlExt


      let targetTexture = bufferTexture.ForegroundTexture frameNo

      gl.FramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, targetTexture.TextureID, 0)
      checkGL gl

      gl.UseProgram sceneBuffer.Program.ProgramID
      checkGL gl

      renderOpenGLBufferChannel' mixer frameNo 0 sceneBuffer.Channel0
      renderOpenGLBufferChannel' mixer frameNo 1 sceneBuffer.Channel1
      renderOpenGLBufferChannel' mixer frameNo 2 sceneBuffer.Channel2
      renderOpenGLBufferChannel' mixer frameNo 3 sceneBuffer.Channel3

      match sceneBuffer.TimeLocation with
      | ValueNone   -> ()
      | ValueSome tl->
        gl.Uniform1f (tl.UniformLocationID, time)
        checkGL gl

      match sceneBuffer.ResolutionLocation with
      | ValueNone   -> ()
      | ValueSome rl->
        glext.Uniform2f (rl.UniformLocationID, mixer.Resolution.X, float32 mixer.Resolution.Y)
        checkGL gl

      gl.DrawElements (GL_TRIANGLES, indices.Length, GL_UNSIGNED_SHORT, 0)
      checkGL gl

    let renderOpenGLSceneBuffer' (mixer : OpenGLMixer) (time : float32) (frameNo : int) (bufferTexture : OpenGLBufferTexture) (sceneBuffer : OpenGLSceneBuffer voption) : unit =
      match sceneBuffer with
      | ValueNone     -> ()
      | ValueSome sb  -> renderOpenGLSceneBuffer mixer time frameNo bufferTexture sb

    let renderOpenGLPresenter (mixer : OpenGLMixer) (time : float32) (frameNo : int) (presenter : OpenGLPresenter) : unit =
      let gl    = mixer.Gl
      let glext = mixer.GlExt

      gl.UseProgram presenter.Program.ProgramID
      checkGL gl

      renderOpenGLBufferChannel mixer frameNo 0 presenter.Channel0

      match presenter.TimeLocation with
      | ValueNone   -> ()
      | ValueSome tl->
        gl.Uniform1f (tl.UniformLocationID, time)
        checkGL gl

      match presenter.ResolutionLocation with
      | ValueNone   -> ()
      | ValueSome rl->
        glext.Uniform2f (rl.UniformLocationID, mixer.Resolution.X, float32 mixer.Resolution.Y)
        checkGL gl

      gl.DrawElements (GL_TRIANGLES, indices.Length, GL_UNSIGNED_SHORT, 0)
      checkGL gl

  open Internals

  let setupOpenGLMixer (glContext : IGlContext) (gl : GlInterface) (resolution : Vector2) (mixer : Mixer) : OpenGLMixer =
#if DEBUG
    trace "setupOpenGLMixer called"
#endif

    checkGL gl

    let glext = GlInterfaceExt gl

#if DEBUG
#if CAPTURE_OPENGL_LOGS
    use _ = new OpenGlDebugMode (gl, glext)
#endif
#endif

    let frameBuffer = 
      {
        FrameBufferID = gl.GenFramebuffer ()
      }
    checkGL gl

    let vertexBuffer  = createAndBindBuffer gl GL_ARRAY_BUFFER vertices
    let indexBuffer   = createAndBindBuffer gl GL_ELEMENT_ARRAY_BUFFER indices

    let vertexArray   =
      {
        VertexArrayID = gl.GenVertexArray ()
      }
    checkGL gl

    gl.BindVertexArray vertexArray.VertexArrayID
    checkGL gl

    gl.VertexAttribPointer(
            positionLocation
        ,   3
        ,   GL_FLOAT
        ,   0
        ,   vertexBuffer.ElementSize
        ,   0
        )
    checkGL gl

    gl.VertexAttribPointer(
            texCoordLocation
        ,   2
        ,   GL_FLOAT
        ,   0
        ,   vertexBuffer.ElementSize
        ,   12
        )
    checkGL gl

    gl.EnableVertexAttribArray positionLocation
    checkGL gl

    gl.EnableVertexAttribArray texCoordLocation
    checkGL gl

    let namedBitmapImages = 
      mixer.NamedBitmapImages
      |> Map.map (fun k v -> createOpenGLBitmapImage gl v)

    let namedPresenters = 
      mixer.NamedPresenters
      |> Map.map (fun k v -> createOpenGLPresenter gl v)

    let namedScenes = 
      mixer.NamedScenes
      |> Map.map (fun k v -> createOpenGLScene gl v)

    {
      Mixer             = mixer
      Resolution        = resolution
      NamedBitmapImages = namedBitmapImages
      NamedPresenters   = namedPresenters
      NamedScenes       = namedScenes
      BufferA           = createOpenGLBufferTexture gl resolution
      BufferB           = createOpenGLBufferTexture gl resolution
      BufferC           = createOpenGLBufferTexture gl resolution
      BufferD           = createOpenGLBufferTexture gl resolution
      Image0            = createOpenGLBufferTexture gl resolution
      FrameBuffer       = frameBuffer
      IndexBuffer       = indexBuffer
      VertexBuffer      = vertexBuffer
      VertexArray       = vertexArray
      GlContext         = glContext
      Gl                = gl
      GlExt             = glext
    }

  let tearDownOpenGLMixer (mixer : OpenGLMixer) : unit =
#if DEBUG
    trace "tearDownOpenGLMixer called"
#endif

    let gl    = mixer.Gl
    let glext = mixer.GlExt
    checkGL gl

#if DEBUG
#if CAPTURE_OPENGL_LOGS
    use _ = new OpenGlDebugMode (gl, glext)
#endif
#endif

    tearDownOpenGLBufferTexture mixer mixer.Image0
    tearDownOpenGLBufferTexture mixer mixer.BufferD
    tearDownOpenGLBufferTexture mixer mixer.BufferC
    tearDownOpenGLBufferTexture mixer mixer.BufferB
    tearDownOpenGLBufferTexture mixer mixer.BufferA

    for kv in mixer.NamedScenes do
      let scene = kv.Value
      tearDownOpenGLScene mixer scene

    for kv in mixer.NamedPresenters do
      let presenter = kv.Value
      tearDownOpenGLPresenter mixer presenter

    for kv in mixer.NamedBitmapImages do
      let bitmapImage = kv.Value
      tearDownOpenGLBitmapImage mixer bitmapImage

    gl.DeleteVertexArray mixer.VertexArray.VertexArrayID
    assertGL gl

    gl.DeleteBuffer       mixer.VertexBuffer.BufferID
    assertGL gl

    gl.DeleteBuffer       mixer.IndexBuffer.BufferID
    assertGL gl

    gl.DeleteFramebuffer  mixer.FrameBuffer.FrameBufferID
    assertGL gl

    checkGL gl

  let resizeOpenGLMixer (resolution : Vector2) (mixer : OpenGLMixer) : OpenGLMixer =
#if DEBUG
    trace "resizeOpenGLMixer called"
#endif

    let gl    = mixer.Gl
    let glext = mixer.GlExt
    checkGL gl


#if DEBUG
#if CAPTURE_OPENGL_LOGS
    use _ = new OpenGlDebugMode (gl, glext)
#endif
#endif

    { mixer with 
        Resolution  = resolution
        BufferA     = resizeOpenGLBufferTexture mixer resolution mixer.BufferA 
        BufferB     = resizeOpenGLBufferTexture mixer resolution mixer.BufferB
        BufferC     = resizeOpenGLBufferTexture mixer resolution mixer.BufferC 
        BufferD     = resizeOpenGLBufferTexture mixer resolution mixer.BufferD
    }

  let renderOpenGLMixer (view : PixelRect) (time : float32) (frameNo : int) (mixer : OpenGLMixer) (presenterID : PresenterID) (sceneID : SceneID) : unit =
    let gl    = mixer.Gl
    let glext = mixer.GlExt
    checkGL gl

    let glext = mixer.GlExt

#if DEBUG
#if CAPTURE_OPENGL_LOGS
    use _ = new OpenGlDebugMode (gl, glext)
#endif
#endif

    let presenter = mixer.NamedPresenters.[presenterID]
    let scene     = mixer.NamedScenes.[sceneID]

    gl.Viewport (view.X, view.Y, view.Width, view.Height)
    checkGL gl

    let oldFbo = gl.GetIntegerv GL_FRAMEBUFFER_BINDING

    gl.BindFramebuffer (GL_FRAMEBUFFER, mixer.FrameBuffer.FrameBufferID)
    checkGL gl

    gl.BindBuffer (mixer.VertexBuffer.Target, mixer.VertexBuffer.BufferID)
    checkGL gl

    gl.BindBuffer (mixer.IndexBuffer.Target, mixer.IndexBuffer.BufferID)
    checkGL gl

    gl.BindVertexArray mixer.VertexArray.VertexArrayID
    checkGL gl

    renderOpenGLSceneBuffer' mixer time frameNo mixer.BufferA scene.BufferA
    renderOpenGLSceneBuffer' mixer time frameNo mixer.BufferB scene.BufferB
    renderOpenGLSceneBuffer' mixer time frameNo mixer.BufferC scene.BufferC
    renderOpenGLSceneBuffer' mixer time frameNo mixer.BufferD scene.BufferD
    renderOpenGLSceneBuffer  mixer time frameNo mixer.Image0  scene.Image

    gl.FramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0)
    checkGL gl

    gl.BindFramebuffer (GL_FRAMEBUFFER, oldFbo)
    checkGL gl

    renderOpenGLPresenter mixer time frameNo presenter

  let noBitmapImages  : Map<BitmapImageID , BitmapImage > = Map.empty

  let simplePresenterID = PresenterID "simple"
  let simplePresenter : Presenter =
    {
      FragmentSource  = ShaderSources.fragmentShaderSimplePresenter
      Defines         = [||]
      Channel0        = 
        {
          Filter  = Linear
          Source  = Image0
          Wrap    = Clamp
        }
    }

  let defaultPresenters : Map<PresenterID, Presenter> =
    [|
      simplePresenterID, simplePresenter
    |] |> Map.ofArray
