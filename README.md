# hk-logo2svg
# ELP - Projet Haskell - Logoskell
Haskell (simplified) Logo program to Svg converter

## Compilation
`stack ghc -- -o logoskell Svg.hs`
ou
`ghc -o logoskell Svg.hs`

## Execution
### 1ere option
1.	`.\logoskell.exe < programme.logo > out.svg`
2.	Contenu de `programme.logo` :
	1. programme logo 		-> `[Instr]`				e.g. : `[Repeat 36 [Right 10, Repeat 8 [Forward 25, Left 45]]]`
	2. taille de l'image 	-> `(width, height)`		e.g. : `(500,500)`
	3. conditions initiales	-> `(x, y, angle, color)`	e.g. : `(100,100,0,"blue")`

### 2eme option
1. `.\logoskell.exe > out.svg`
2. Entrer les valeurs suivantes : 
	1. programme logo 		-> `[Instr]`				e.g. : `[Repeat 36 [Right 10, Repeat 8 [Forward 25, Left 45]]]`
	2. taille de l'image 	-> `(width, height)`		e.g. : `(500,500)`
	3. conditions initiales	-> `(x, y, angle, color)`	e.g. : `(100,100,0,"blue")`

## supported logo commands
  * Basic movements
    * `Forward <distance::float>` moves forward, drawing a line from previous to next position
    * `Left <angle::float>` turns cursor left (counter-clockwise), angle in degrees
    * `Right <angle::float>` turns cursor right (clockwise), angle in degress

 * Advanced shapes
   * `Ellipse <rx::float> <ry::float>` draws an ellipse of `rx` and `ry` radii at current cursor position.
   * `Cirle <r::float>` same as ellipse but with `rx` = `ry`
   * `Rect <w::float> <h::float>` draws a rectangle with `w` width and `h` height with its top-left corner at current cursor position.
   * `CRect <w::float> <h::float>` draws a rectangle with `w` width and `h` height with centered on the cursor's current position.

 * Control instructions
   * `Repeat <count::int> <toRepeat::[Instruction]>` repeats `count` time the provided instruction list
   * `Color <color::string>` changes cursor color. color can be anything supported by the svg format such as `red` or `#FF0000`
   * `To <tx::float> <ty::float>` transports cursor to the point (`tx`,`ty`) without drawing a line (as opposed to `Forward`)
  
NB : cursor's angle is applied as center transformation to all "complex" shapes (ie not to lines)
NB2 : more commands could be easily implemented
