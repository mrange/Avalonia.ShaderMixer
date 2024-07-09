namespace ShaderMixer

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml

open Lib.ShaderMix

type MainWindow () as this = 
    inherit Window ()

    do this.InitializeComponent()

    member private this.InitializeComponent() =
#if DEBUG
        this.AttachDevTools()
#endif
        AvaloniaXamlLoader.Load(this)

        let shaderMixer = ShaderMixerControl (Scenes.scenes, Scenes.presenterID, Scenes.gravitySucksID)
        shaderMixer.RenderScaling <- this.RenderScaling

        match this.GetControl<ContentControl> "_content" with
        | null  -> ()
        | cc    -> cc.Content          <- shaderMixer
