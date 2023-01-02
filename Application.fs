[<RequireQualifiedAccess>]
module Application

open Elmish
open Elmish.UrlParser

open Feliz
open Extensions
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Feliz.ReactApi

//npm install three @types/three @react-three/fiber @react-three/drei
[<RequireQualifiedAccess>]
type Url =
    | Home

[<RequireQualifiedAccess>]
type Page =
    | Home

type State =
    { 
      CurrentPage: Page
      CurrentUrl: Url option }

type Msg =
    | Test

let toHash (url: Url) =
    match url with
    | Url.Home -> "#home"

let parseUrl: Parser<Url -> Url, Url> =
    oneOf [ map Url.Home (s "home") ]

let updateUrl (url: Url option) state =
     { state with CurrentPage = Page.Home }, Cmd.none

let init (url: Url option) =
    updateUrl
        url
        {
          CurrentPage = Page.Home
          CurrentUrl = None }

let update (msg: Msg) (state: State) =
    match msg with
    | Test -> state, Cmd.none








[<Erase>]
type ICanvasProperty =
    interface
    end

[<Erase>]
type IOrbitControlsProperty =
    interface
    end

[<Erase>]
type IMeshProperty =
    interface
    end

[<Erase>]
type IPointLightProperty =
    interface
    end

[<Erase>]
type IMeshStandardMaterialProperty  =
    interface 
    end

[<Erase>]
type IBoxGeometryProperty  =
    interface 
    end
[<Erase>]
type ITorusGeometryProperty  =
    interface 
    end

[<Erase>]
type IRoundedBoxProperty  =
    interface 
    end

[<Erase>]
type IStatsProperty  =
    interface 
    end

[<Erase>]
type IAmbientLightProperty  =
    interface 
    end

[<Erase>]
type IEnvironmentProperty  =
    interface 
    end

[<Erase>]
type IAxesHelperProperty  =
    interface 
    end


[<Erase>]
type IGridHelperProperty  =
    interface 
    end

[<Erase>]
type IPlaneGeometryProperty  =
    interface 
    end
[<Erase; RequireQualifiedAccess>]
module Interop =
    let inline mkCanvasAttr (key: string) (value: obj) : ICanvasProperty = unbox (key, value)
    let inline mkOrbitControlsAttr (key: string) (value: obj) : IOrbitControlsProperty = unbox (key, value)
    let inline mkMeshAttr (key: string) (value: obj) : IMeshProperty = unbox (key, value)
    let inline mkPointLightAttr (key: string) (value: obj) : IPointLightProperty = unbox (key, value)
    let inline mkMeshStandardMaterialAttr (key: string) (value: obj) : IMeshStandardMaterialProperty = unbox (key, value)
    let inline mkBoxGeometryAttr (key: string) (value: obj) : IBoxGeometryProperty = unbox (key, value)
    let inline mkTorusGeometryAttr (key: string) (value: obj) : ITorusGeometryProperty = unbox (key, value)
    let inline mkRoundedBoxAttr (key: string) (value: obj) : IRoundedBoxProperty = unbox (key, value)
    let inline mkStatsAttr (key: string) (value: obj) : IStatsProperty = unbox (key, value)

    let inline mkAmbientLightAttr (key: string) (value: obj) : IAmbientLightProperty = unbox (key, value)
    let inline mkEnvironmentAttr (key: string) (value: obj) : IEnvironmentProperty = unbox (key, value)
    let inline mkAxesHelperAttr (key: string) (value: obj) : IAxesHelperProperty = unbox (key, value)
    let inline mkGridHelperAttr (key: string) (value: obj) : IGridHelperProperty = unbox (key, value)
    let inline mkPlaneGeometry  (key: string) (value: obj) : IPlaneGeometryProperty = unbox (key, value)


    let inline reactElementWithChildren (name: string) (children: #seq<ReactElement>) =
        let reactApi: IReactApi = importDefault "react"
        let reactElement (name: string) (props: 'a) : ReactElement = import "createElement" "react"

        reactElement
            name
            (createObj [
                "children"
                ==> reactApi.Children.toArray (Array.ofSeq children)
             ])

[<Erase>]
type canvas =
    static member inline dpr(value: int) = Interop.mkCanvasAttr "dpr" value
    static member inline legacy(value: bool) = Interop.mkCanvasAttr "legacy" value

    static member inline linear(value: bool) = Interop.mkCanvasAttr "linear" value

    static member inline children(elements: ReactElement list) =  unbox<ICanvasProperty>    (prop.children elements)

[<Erase>]
type mesh =
    static member inline intensity(value: float) = Interop.mkMeshAttr "intensity" value
    static member inline receiveShadow(value: bool) = Interop.mkMeshAttr "receiveShadow" value
    static member inline children(elements: ReactElement list) =  unbox<IMeshProperty>    (prop.children elements)


[<Erase>]
type pointLight =
    static member inline position(lst: int * int * int) = Interop.mkPointLightAttr "position" lst
    static member inline color(value: string ) = Interop.mkPointLightAttr "color" value
    static member inline intensity(value: float) = Interop.mkPointLightAttr "intensity" value
    static member inline castShadow(value: bool) = Interop.mkPointLightAttr "castShadow" value
    
[<Erase>]
type meshStandardMaterial =
    static member inline color(value: string ) = Interop.mkMeshStandardMaterialAttr "color" value

[<Erase>]
type stats =
    static member inline showPanel(value: int) = Interop.mkStatsAttr "showPanel" value

[<Erase>]
type axesHelper =
    static member inline args1(value: int) = Interop.mkAxesHelperAttr "args" value

    static member inline args2(value: int * int) = Interop.mkAxesHelperAttr "args" value

[<Erase>]
type environment =
    static member inline preset(value: string) = Interop.mkEnvironmentAttr "preset" value
    static member inline files(value: string) = Interop.mkEnvironmentAttr "files" value
    static member inline background = Interop.mkEnvironmentAttr "background" () 
    static member inline blur(value: float) = Interop.mkEnvironmentAttr "blur" value

[<Erase>]
type orbitControls =
    static member inline autoRotate(value:string) = Interop.mkOrbitControlsAttr "autoRotate" value
   // static member inline files(value: string) = Interop.mkEnvironmentAttr "files" value



[<Erase>]
type torusGeometry =
    static member inline args(value: float * float * float * float) = Interop.mkTorusGeometryAttr "args" value
    
[<Erase>]
type planeGeometry =
    static member inline args(value: float * float * float * float) = Interop.mkPlaneGeometry "args" value
   // static member inline files(value: string) = Interop.mkEnvironmentAttr "files" value

[<Erase>]
type boxGeometry =
    static member inline args(value: float * float * float) = Interop.mkBoxGeometryAttr "args" value
    

[<Erase>]
type roundedBox =
    static member inline args(value: float * float * float) = Interop.mkRoundedBoxAttr "args" value

    static member inline radius(value: float) = Interop.mkRoundedBoxAttr "radius" value



let reactElement (name: string) (props: 'a) : ReactElement = import "createElement" "react"
let reactApi: IReactApi = importDefault "react"

let createChildren children = 
                (createObj [
                "children"
                ==> reactApi.Children.toArray (Array.ofSeq children)
                ])

[<Erase>]
type Three  =

    static member inline canvas(properties: ICanvasProperty list) =
        Interop.reactApi.createElement (import "Canvas" "@react-three/fiber", createObj !!properties)

    static member inline orbitControls(properties: IOrbitControlsProperty list) = Interop.reactApi.createElement (import "OrbitControls" "@react-three/drei", createObj !!properties)

    static member inline roundedBox(properties: IRoundedBoxProperty list) = Interop.reactApi.createElement (import "RoundedBox" "@react-three/drei", createObj !!properties)

    
    static member inline stats(properties: IStatsProperty list) = Interop.reactApi.createElement (import "Stats" "@react-three/drei", createObj !!properties)

    static member inline environment(properties: IEnvironmentProperty list) = Interop.reactApi.createElement (import "Environment" "@react-three/drei", createObj !!properties)


    static member inline mesh children = 
        reactElement "mesh" (createObj !!children)

    static member inline sphereGeometry children = 
        reactElement "sphereGeometry" (createChildren children)

    static member inline pointLight (properties: IPointLightProperty list) = 
        reactElement "pointLight" (createObj !!properties)

    static member inline ambientLight (properties: IAmbientLightProperty list) = 
        reactElement "ambientLight" (createObj !!properties)

    static member inline meshStandardMaterial (properties: IMeshStandardMaterialProperty list) = 
        reactElement "meshStandardMaterial" (createObj !!properties)

    static member inline boxGeometry (properties: IBoxGeometryProperty list) =  
        reactElement "boxGeometry" (createObj !!properties)

    static member inline torusGeometry (properties: ITorusGeometryProperty list) =  
        reactElement "torusGeometry" (createObj !!properties)

    static member inline planeGeometry (properties: IPlaneGeometryProperty list) =  
        reactElement "planeGeometry" (createObj !!properties)

    static member inline axesHelper (properties: IAxesHelperProperty list) =  
        reactElement "axesHelper" (createObj !!properties)


    static member inline gridHelper (properties: IGridHelperProperty list) =  
        reactElement "gridHelper" (createObj !!properties)


let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [
            style.height (length.vh 100)
        ]
        prop.children [
            Three.canvas 
                [   

                    canvas.dpr 2
                    canvas.legacy true
                    canvas.linear true
                    canvas.children [
                        Three.axesHelper [
                            axesHelper.args2 (30,50)
                        ]
                        Three.gridHelper []
                        Three.environment [
                            //environment.preset "forest"
                            environment.files "https://cdn.jsdelivr.net/gh/Sean-Bradley/React-Three-Fiber-Boilerplate@environment/public/img/venice_sunset_1k.hdr"
                            //environment.background
                            //environment.blur 0.5
                        ]
                        Three.pointLight [
                            pointLight.position (5,3,3)
                            pointLight.color "#8884d8"
                            pointLight.intensity 0.8
                            pointLight.castShadow true
                        ]
                        //Three.ambientLight []
                        Three.stats [
                            stats.showPanel 1
                        ]
                        
                        Three.mesh [
                                mesh.receiveShadow true         
                                //Three.sphereGeometry []
                                (*Three.boxGeometry [
                                    boxGeometry.args (14,10,10)
                                ]*)
                                //<planeBufferGeometry args={[10, 10, 1, 1]} />       
                                mesh.children [
                                    Three.planeGeometry [
                                        planeGeometry.args (10,10,1,1) 
                                    ]
                                    
                                    Three.torusGeometry [
                                        torusGeometry.args (1,0.5, 30, 94000)
                                    ]
                                    
                                
                                    (*
                                    Three.roundedBox [
                                        roundedBox.args (1, 1, 0.1)
                                        roundedBox.radius 0.1
                                    ]

                                    *)
                                    Three.meshStandardMaterial [meshStandardMaterial.color "hotpink"]
                                ]
                            ]
                            
                        Three.orbitControls [
                            orbitControls.autoRotate "true"
                        ]
                    ]
                ]
 
             ] 
             ]
 
