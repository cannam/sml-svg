
structure Svg = struct

    type coords = real * real
    type dimens = real * real

    datatype 'a absrel =
             ABS of 'a |
             REL of 'a

    datatype path_element =
             MOVE_TO of coords absrel |
             LINE_TO of coords list absrel |
             HORIZONTAL_TO of real absrel |
             VERTICAL_TO of real absrel |
             CUBIC_TO of { control1: coords, control2: coords, target: coords } list absrel |
             CUBIC_SMOOTH_TO of { control2: coords, target: coords } list absrel |
             QUADRATIC_TO of { control: coords, target: coords } list absrel |
             QUADRATIC_SMOOTH_TO of { target: coords } list absrel |
             ARC_TO of { radii: dimens, rotation: real, largeArc: bool,
                         sweep: bool, target: coords } list absrel |
             CLOSE_PATH

    type path = path_element list

    type text = { origin: coords, rotation: real, text: string }

    datatype paint =
             NO_PAINT |
             COLOUR of string |
             RGB of real * real * real
                                      
    datatype linecap = BUTT | ROUND_CAP | SQUARE
    datatype linejoin = MITRE | ROUND_JOIN | BEVEL
    datatype fontstyle = NORMAL | ITALIC | OBLIQUE
    datatype fillrule = NON_ZERO | EVEN_ODD

    datatype property =
             STROKE of paint |
             STROKE_WIDTH of real |
             STROKE_LINECAP of linecap |
             STROKE_LINEJOIN of linejoin |
             STROKE_OPACITY of real |
             FILL of paint |
             FILL_RULE of fillrule |
             FILL_OPACITY of real |
             OPACITY of real |
             FONT_FAMILY of string list |
             FONT_STYLE of fontstyle |
             FONT_WEIGHT of int | (* !!! iffy *)
             FONT_SIZE of real (* !!! nope *)

    type 'a decorated = 'a * property list
                                     
    datatype element =
             PATH of path |
             RECT of { origin: coords, size: dimens } |
             ROUNDED_RECT of { origin: coords, size: dimens, radii: dimens } |
             CIRCLE of { centre: coords, radius: real } |
             ELLIPSE of { centre: coords, radii: dimens } |
             LINE of { source: coords, target: coords } |
             POLYLINE of coords list |
             POLYGON of coords list |
             TEXT of text |
             GROUP of element decorated list

    type content = element decorated list
                              
    type svg = { size: dimens, content: content } (* !!! + metadata like title, etc *)
                                 
end

structure SvgPathShorthand = struct

    val M = Svg.MOVE_TO o Svg.ABS
    val m = Svg.MOVE_TO o Svg.REL
    val L = Svg.LINE_TO o Svg.ABS
    val l = Svg.LINE_TO o Svg.REL

    val H = Svg.HORIZONTAL_TO o Svg.ABS
    val h = Svg.HORIZONTAL_TO o Svg.REL
    val V = Svg.VERTICAL_TO o Svg.ABS
    val v = Svg.VERTICAL_TO o Svg.REL

    val C = Svg.CUBIC_TO o Svg.ABS
    val c = Svg.CUBIC_TO o Svg.REL
    val S = Svg.CUBIC_SMOOTH_TO o Svg.ABS
    val s = Svg.CUBIC_SMOOTH_TO o Svg.REL

    val Q = Svg.QUADRATIC_TO o Svg.ABS
    val q = Svg.QUADRATIC_TO o Svg.REL
    val T = Svg.QUADRATIC_SMOOTH_TO o Svg.ABS
    val t = Svg.QUADRATIC_SMOOTH_TO o Svg.REL

    val A = Svg.ARC_TO o Svg.ABS
    val a = Svg.ARC_TO o Svg.REL
                             
    val Z = Svg.CLOSE_PATH
    val z = Z

end

structure SvgSerialise :> sig

    val serialiseContent : Svg.content -> string
    val serialiseDocument : Svg.svg -> string
                                                     
end = struct

    open Svg

    fun elementName e =
        case e of
            PATH _ => "path"
          | RECT _ => "rect"
          | ROUNDED_RECT _ => "rect"
          | CIRCLE _ => "circle" 
          | ELLIPSE _ => "ellipse"
          | LINE _ => "line" 
          | POLYLINE _ => "polyline"
          | POLYGON _ => "polygon"
          | TEXT _ => "text"
          | GROUP _ => "g"

    fun realString r =
        implode (map (fn #"~" => #"-" | c => c) (explode (Real.toString r)))

    fun joinMap j f xs = String.concatWith j (map f xs)
                
    fun coordString (x, y) =
        realString x ^ "," ^ realString y

    fun coordAttrString (x, y) =
        "x=\"" ^ realString x ^ "\" y=\"" ^ realString y ^ "\""

    fun dimenAttrString (x, y) =
        "width=\"" ^ realString x ^ "\" height=\"" ^ realString y ^ "\""

    fun escapeTextData str =
        String.translate (fn #"<" => "&lt;"
                           | #">" => "&gt;"
                           | #"&" => "&amp;"
                           | c => Char.toString c) str
                                                                        
    fun pathElementText pe =
        case pe of
            MOVE_TO (ABS c) => "M " ^ coordString c
          | MOVE_TO (REL c) => "m " ^ coordString c
          | LINE_TO (ABS cc) => joinMap " " (fn c => "L " ^ coordString c) cc
          | LINE_TO (REL cc) => joinMap " " (fn c => "l " ^ coordString c) cc
          | HORIZONTAL_TO (ABS x) => "H " ^ realString x
          | HORIZONTAL_TO (REL x) => "h " ^ realString x
          | VERTICAL_TO (ABS y) => "V " ^ realString y
          | VERTICAL_TO (REL y) => "v " ^ realString y
          | CLOSE_PATH => "Z"
          | other => "" (*!!!*)
                           
    fun elementAttributes e =
        case e of
            PATH pp => " d=\"" ^ joinMap " " pathElementText pp ^ "\""
          | RECT { origin, size } => " " ^ coordAttrString origin ^
                                     " " ^ dimenAttrString size
          | TEXT { origin, rotation, text } => " " ^ coordAttrString origin ^
                                               " rotate=\"" ^
                                               realString rotation ^ "\""
          | POLYLINE cc => " points=\"" ^ joinMap " " coordString cc ^ "\""
          | POLYGON cc => " points=\"" ^ joinMap " " coordString cc ^ "\""
          | _ => "" (*!!! *)
                           
    fun propertyName p =
        case p of
            STROKE _ => "stroke"
          | STROKE_WIDTH _ => "stroke-width"
          | STROKE_LINECAP _ => "stroke-linecap"
          | STROKE_LINEJOIN _ => "stroke-linejoin"
          | STROKE_OPACITY _ => "stroke-opacity"
          | FILL _ => "fill"
          | FILL_RULE _ => "fill-rule"
          | FILL_OPACITY _ => "fill-opacity"
          | OPACITY _ => "opacity"
          | FONT_FAMILY _ => "font-family"
          | FONT_STYLE _ => "font-style"
          | FONT_WEIGHT _ => "font-weight"
          | FONT_SIZE _ => "font-size"

    fun rgbString (r, g, b) =
        "rgb(" ^
        joinMap "," (fn x => Int.toString (Real.round (x * 255.0))) [r, g, b] ^
        ")"
                               
    fun paintString p =
        case p of
            NO_PAINT => "none"
          | COLOUR str => str
          | RGB rgb => rgbString rgb
                               
    fun lineCapName c =
        case c of
            BUTT => "butt"
          | ROUND_CAP => "round"
          | SQUARE => "square"

    fun lineJoinName j =
        case j of
            MITRE => "miter"
          | ROUND_JOIN => "round"
          | BEVEL => "bevel"

    fun fontStyleName f =
        case f of
            NORMAL => "normal"
          | ITALIC => "italic"
          | OBLIQUE => "oblique"

    fun fillRuleName r =
        case r of
            NON_ZERO => "non-zero"
          | EVEN_ODD => "even-odd"
                                    
    fun propertyValueString p =
        case p of
            STROKE x => paintString x
          | STROKE_WIDTH x => Real.toString x
          | STROKE_LINECAP x => lineCapName x
          | STROKE_LINEJOIN x => lineJoinName x
          | STROKE_OPACITY x => Real.toString x
          | FILL x => paintString x
          | FILL_RULE x => fillRuleName x
          | FILL_OPACITY x => Real.toString x
          | OPACITY x => Real.toString x
          | FONT_FAMILY x => String.concatWith ", " x
          | FONT_STYLE x => fontStyleName x
          | FONT_WEIGHT x => Int.toString x
          | FONT_SIZE x => Real.toString x

    fun serialiseProperty prop =
        propertyName prop ^ "=\"" ^ propertyValueString prop ^ "\""

    fun serialiseProperties [] = ""
      | serialiseProperties props =
        " " ^ String.concatWith " " (map serialiseProperty props)

    and serialiseElementWith proptext elt =
        "<" ^ elementName elt ^ elementAttributes elt ^ proptext ^
        (case elt of
             GROUP content =>
             ">" ^ serialiseContent content ^ "</" ^ elementName elt ^ ">"
           | TEXT { text, ... } =>
             ">" ^ escapeTextData text ^ "</" ^ elementName elt ^ ">" (*!!! handle common close-element logic *)
           | other => "/>")

    and serialiseDecoratedElement (elt, props) =
        serialiseElementWith (serialiseProperties props) elt

    and serialiseContent svg =
        String.concatWith "\n" (List.map serialiseDecoratedElement svg) ^ "\n"

    fun serialiseDocument ({ size, content } : svg) =
        "<?xml version=\"1.0\" standalone=\"no\"?>\n" ^
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ^
        "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" " ^ dimenAttrString size ^ ">\n" ^
        serialiseContent content ^
        "</svg>\n"

end
                  
