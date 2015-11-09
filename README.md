# hcj-hs

Composable website layout library.  Haskell implementation of [hcj](https://github.com/jeffersoncarpenter/hcj).

Layouts arrange components.  A layout, when applied to some components, itself yields a component.

## How to Build

1. Install [ghcjs](https://github.com/ghcjs/ghcjs) and [uglifyjs](https://github.com/mishoo/UglifyJS2).
2. Git clone the source: `git clone https://github.com/jeffersoncarpenter/hcj-hs.git`.
3. Run `make init`, to install cabal dependencies.
4. Run `make` to build.

## Planned Features

* Library of functions that generate components and layouts, emulating most of CSS
* Haskell and Javascript interfaces for defining new components and layouts, adding to the library
* Drag-and-drop interface for building website front-ends and back-ends

## Words

**Component**

Function that draws onto a web page.

Takes one argument: a parent element to append stuff to.

Returns two values:
* A `Component Instance`
* A function that, when called, removes the stuff from the page.  Must be called only once.  Once called, the `Component Instance` must not be used.

**Component Instance**

Something that is taking up space on a page.  An instance consists of:
* Stream of `Dimensions` objects giving the minimum size of the component over time
* `Place` function that, whenever called, resizes this component instance to a particular actual width and height

**Dimensions**

A `Dimensions` of a `Component Instance` has two properties:
* `width : Int`, the minimum width required by a component
* `height: Int -> IO Int`, the height required by a component should it be given a particular width

**Layout**

Any function that takes `Component`s and returns a `Component`.

A layout instance produces a `Dimensions` stream, usually based on the `Dimensions` streams of its children.  It must call `Place` on each child instance at least once, and then as necessary whenever its own `Place` funciton is called.

**Root Component Instance**

A `Component Instance` that has been bootstrapped onto a web page, rather than passed into a layout.  Its `Dimensions` strteam may be ignored, and its `Place` function is called on the window's resize event.

Typically a website only has one `Root Component`.  It is attached to the body element.  Its width is set to the window width, and its height is set to its own minimum height at that width.
