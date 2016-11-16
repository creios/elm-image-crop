// To use this type declarations in your TypeScript project, create the
// following declaration file:
//
//     declare interface Elm {
//         ImageCrop: ElmImageCrop.ImageCrop;
//     }
//     
//     declare const Elm: Elm;
//
//  This way, you can declare more than one elm modules

declare namespace ElmImageCrop {

    export interface ImageCrop {
        Interop: Interop;
    }

    export interface Interop {
        embed(element: Element, flags: Flags): App
    }
  
    export interface Flags {
        image: Size;
        cropAreaWidth: number;
        offset: Point;
        selection?: Rectangle;
        aspectRatio?: Size;
    }
  
    export interface Size {
        width: number;
        height: number;
    }
  
    export interface Rectangle {
        topLeft: Point;
        bottomRight: Point;
    }
  
    export interface Point {
        x: number;
        y: number;
    }
  
    export interface App {
        ports: Ports;
    }
  
    export interface Ports {
        viewportChanged: ViewportChanged;
        selectionChanged: SelectionChanged;
        changeAspectRatio: ChangeAspectRatio;
    }
  
    export interface ViewportChanged {
        send(width: number): void;
    }
  
    export interface SelectionChanged {
        subscribe(callback: (selection: Selection) => void): void;
    }
  
    export interface ChangeAspectRatio {
        send(aspectRatio?: Size): void;
    }
}
