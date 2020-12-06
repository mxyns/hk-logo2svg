# hk-logo2svg
Haskell (simplified) Logo program to Svg converter

# compiling :
`stack ghc -- -o logoskell Svg.hs`
or
`ghc -o logoskell Svg.hs`

# running
`.\logoskell.exe < prog.logo > out.svg`  
input file `prog.logo`'s content :
  * line 1 - Logo program
  * line 2 - image dimensions tuple : `(width::float, height::float)` (e.g.: `(500.0,500.0)`)
  * line 3 - initial parameters : `(x::float, y::float, angle::float, color::string)` (e.g.: `(100,100,0,"red")`)

# supported logo commands
  * `Forward <distance::float>` moves forward, drawing a line from previous to next position
  * `Left <angle::float>` turns cursor left (counter-clockwise), angle in degrees
  * `Right <angle::float>` turns cursor right (clockwise), angle in degress
  * `Repeat <count::int> <toRepeat::[Instruction]>` repeats `count` time the provided instruction list
  * `Color <color::string>` changes cursor color. color can be anything supported by the svg format such as `red` or `#FF0000`
  * `Ellipse <rx::float> <ry::float>` draws an ellipse of `rx` and `ry` radii at current cursor position.
  * `Cirle <r::float>` same as ellipse but with `rx` = `ry`
  * `Rect <w::float> <h::float>` draws a rectangle with `w` width and `h` height with its top-left corner at current cursor position.
  * `CRect <w::float> <h::float>` draws a rectangle with `w` width and `h` height with centered on the cursor's current position.
 
NB : cursor's angle is applied as center transformation to all "complex" shapes (ie not to lines)
NB2 : more commands could be easily implemented
