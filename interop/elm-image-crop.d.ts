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
        selection: Rectangle | null;
        aspectRatio: Size | null;
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
        requestOffset: RequestOffset;
        receiveOffset: ReceiveOffset;
    }
  
    export interface ViewportChanged {
        send(width: number): void;
    }
  
    export interface SelectionChanged {
        subscribe(callback: (selection: Rectangle | null) => void): void;
    }
  
    export interface ChangeAspectRatio {
        send(aspectRatio: Size | null): void;
    }

    export interface RequestOffset {
        subscribe(callback: () => void): void;
    }

    export interface ReceiveOffset {
        send(offset: Point): void
    }
}
