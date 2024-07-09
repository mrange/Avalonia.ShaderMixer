module Scenes
open Lib.ShaderMix

let presenterID     = OpenGL.simplePresenterID
let gravitySucksID  = SceneID "gravitySucks"
    
let scenes = 
  {
    NamedBitmapImages = Map.empty
    NamedPresenters = OpenGL.defaultPresenters
    NamedScenes =
      [|
          gravitySucksID
        , {
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
      |] |> Map.ofArray
  }
