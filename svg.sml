
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
    datatype vectoreffect = DEFAULT_STROKE | NON_SCALING_STROKE
    datatype shaperendering =
             DEFAULT_RENDERING | OPTIMIZE_SPEED | CRISP_EDGES | GEOMETRIC_PRECISION

    datatype transform =
             MATRIX of real * real * real * real * real * real |
             TRANSLATE of coords |
             SCALE of dimens |
             ROTATE of real |
             SKEW_X of real |
             SKEW_Y of real

    datatype property =
             STROKE of paint |
             STROKE_WIDTH of real |
             STROKE_LINECAP of linecap |
             STROKE_LINEJOIN of linejoin |
             STROKE_DASHARRAY of real list |
             STROKE_OPACITY of real |
             VECTOR_EFFECT of vectoreffect |
             SHAPE_RENDERING of shaperendering |
             FILL of paint |
             FILL_RULE of fillrule |
             FILL_OPACITY of real |
             OPACITY of real |
             FONT_FAMILY of string list |
             FONT_STYLE of fontstyle |
             FONT_WEIGHT of int | (* !!! iffy *)
             FONT_SIZE of real | (* !!! nope *)
             TRANSFORM of transform list

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

structure SvgPathShorthandInt = struct

local
    val toReal = Real.fromInt
    fun toReals (x, y) = (toReal x, toReal y)

    fun cubicToReals { control1, control2, target } =
        { control1 = toReals control1,
          control2 = toReals control2,
          target = toReals target }

    fun cubicSmoothToReals { control2, target } =
        { control2 = toReals control2,
          target = toReals target }

    fun quadraticToReals { control, target } =
        { control = toReals control,
          target = toReals target }

    fun quadraticSmoothToReals { target } =
        { target = toReals target }

    fun arcToReals { radii, rotation, largeArc, sweep, target } =
        { radii = toReals radii,
          rotation = rotation,
          largeArc = largeArc,
          sweep = sweep,
          target = toReals target }
            
    structure S = SvgPathShorthand
in

    val M = S.M o toReals
    val m = S.m o toReals

    val L = S.L o (map toReals)
    val l = S.l o (map toReals)

    val H = S.H o toReal
    val h = S.h o toReal
    val V = S.V o toReal
    val v = S.v o toReal

    val C = S.C o (map cubicToReals)
    val c = S.c o (map cubicToReals)
    val S = S.S o (map cubicSmoothToReals)
    val s = S.s o (map cubicSmoothToReals)

    val Q = S.Q o (map quadraticToReals)
    val q = S.q o (map quadraticToReals)
    val T = S.T o (map quadraticSmoothToReals)
    val t = S.t o (map quadraticSmoothToReals)

    val A = S.A o (map arcToReals)
    val a = S.a o (map arcToReals)

    val Z = S.Z
    val z = S.z
                
end
end

structure SvgSerialise :> sig

    val serialiseContent : Svg.content -> string
    val serialiseDocumentAtScale : real -> Svg.svg -> string
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
        if r < 0.0
        then "-" ^ realString (~r)
        else 
            let val epsilon = 1E~9
            in
                if Real.abs (r - Real.realRound r) < epsilon
                then Int.toString (Real.round r)
                else Real.fmt (StringCvt.FIX (SOME 6)) r
            end

    fun joinMap j f xs = String.concatWith j (map f xs)

    fun joinMapElement code f [] = ""
      | joinMapElement code f xs = code ^ " " ^ joinMap " " f xs
                
    fun coordString (x, y) =
        realString x ^ "," ^ realString y

    fun coordAttrString (x, y) =
        "x=\"" ^ realString x ^ "\" y=\"" ^ realString y ^ "\""

    fun dimenAttrStringWithUnit unit (x, y) =
        "width=\"" ^ realString x ^ unit ^
        "\" height=\"" ^ realString y ^ unit ^ "\""

    val dimenAttrString = dimenAttrStringWithUnit ""

    fun cubicString { control1 = (x1, y1),
                      control2 = (x2, y2),
                      target = (x, y) } =
        realString x1 ^ " " ^ realString y1 ^ "," ^
        realString x2 ^ " " ^ realString y2 ^ "," ^
        realString x ^ " " ^ realString y

    fun quadraticString { control = (x1, y1),
                          target = (x, y) } =
        realString x1 ^ " " ^ realString y1 ^ "," ^
        realString x ^ " " ^ realString y

    fun arcString { radii = (rx, ry),
                    rotation, largeArc, sweep,
                    target = (x, y) } =
        realString rx ^ "," ^ realString ry ^ " " ^
        realString rotation ^ " " ^
        (if largeArc then "1 " else "0 ") ^
        (if sweep then "1 " else "0 ") ^
        realString x ^ "," ^ realString y
                                        
    fun escapeTextData str =
        String.translate (fn #"<" => "&lt;"
                           | #">" => "&gt;"
                           | #"&" => "&amp;"
                           | c => Char.toString c) str
                                                                        
    fun pathElementText pe =
        case pe of
            MOVE_TO (ABS c) => "M " ^ coordString c
          | MOVE_TO (REL c) => "m " ^ coordString c
          | LINE_TO (ABS cc) => joinMapElement "L" coordString cc
          | LINE_TO (REL cc) => joinMapElement "l" coordString cc
          | HORIZONTAL_TO (ABS x) => "H " ^ realString x
          | HORIZONTAL_TO (REL x) => "h " ^ realString x
          | VERTICAL_TO (ABS y) => "V " ^ realString y
          | VERTICAL_TO (REL y) => "v " ^ realString y
          | CUBIC_TO (ABS pp) => joinMapElement "C" cubicString pp
          | CUBIC_TO (REL pp) => joinMapElement "c" cubicString pp
          | QUADRATIC_TO (ABS pp) => joinMapElement "Q" quadraticString pp
          | QUADRATIC_TO (REL pp) => joinMapElement "q" quadraticString pp
          | ARC_TO (ABS aa) => joinMapElement "A" arcString aa
          | ARC_TO (REL aa) => joinMapElement "a" arcString aa
          | CLOSE_PATH => "Z"
          | other => "" (*!!!*)
                           
    fun elementAttributes e =
        case e of
            PATH pp => " d=\"" ^ joinMap " " pathElementText pp ^ "\""
          | RECT { origin, size } => " " ^ coordAttrString origin ^
                                     " " ^ dimenAttrString size
          | CIRCLE { centre, radius } => " cx=\"" ^ realString (#1 centre) ^
                                         "\" cy=\"" ^ realString (#2 centre) ^
                                         "\" r=\"" ^ realString radius ^ "\""
          | ELLIPSE { centre, radii } => " cx=\"" ^ realString (#1 centre) ^
                                         "\" cy=\"" ^ realString (#2 centre) ^
                                         "\" rx=\"" ^ realString (#1 radii) ^
                                         "\" ry=\"" ^ realString (#2 radii) ^ "\""
          | LINE { source, target } => " x1=\"" ^ realString (#1 source) ^
                                       "\" y1=\"" ^ realString (#2 source) ^
                                       "\" x2=\"" ^ realString (#1 target) ^
                                       "\" y2=\"" ^ realString (#2 target) ^ "\""
          | POLYLINE cc => " points=\"" ^ joinMap " " coordString cc ^ "\""
          | POLYGON cc => " points=\"" ^ joinMap " " coordString cc ^ "\""
          | TEXT { origin, rotation, text } => " " ^ coordAttrString origin ^
                                               " rotate=\"" ^
                                               realString rotation ^ "\""
          | _ => "" (*!!! *)
                           
    fun propertyName p =
        case p of
            STROKE _ => "stroke"
          | STROKE_WIDTH _ => "stroke-width"
          | STROKE_LINECAP _ => "stroke-linecap"
          | STROKE_LINEJOIN _ => "stroke-linejoin"
          | STROKE_DASHARRAY _ => "stroke-dasharray"
          | STROKE_OPACITY _ => "stroke-opacity"
          | VECTOR_EFFECT _ => "vector-effect"
          | SHAPE_RENDERING _ => "shape-rendering"
          | FILL _ => "fill"
          | FILL_RULE _ => "fill-rule"
          | FILL_OPACITY _ => "fill-opacity"
          | OPACITY _ => "opacity"
          | FONT_FAMILY _ => "font-family"
          | FONT_STYLE _ => "font-style"
          | FONT_WEIGHT _ => "font-weight"
          | FONT_SIZE _ => "font-size"
          | TRANSFORM _ => "transform"

    fun transformName t =
        case t of
            MATRIX _ => "matrix"
          | TRANSLATE _ => "translate"
          | SCALE _ => "scale"
          | ROTATE _ => "rotate"
          | SKEW_X _ => "skewX"
          | SKEW_Y _ => "skewY"

    fun transformArgString t =
        String.concatWith
            ","
            (map realString
                 (case t of
                      MATRIX (a,b,c,d,e,f) => [a,b,c,d,e,f]
                    | TRANSLATE (x,y) => [x,y]
                    | SCALE (sx,sy) => [sx,sy]
                    | ROTATE r => [r]
                    | SKEW_X s => [s]
                    | SKEW_Y s => [s]))
                            
    fun transformString t =
        transformName t ^ "(" ^ transformArgString t ^ ")"
                            
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

    fun vectorEffectName e =
        case e of
            DEFAULT_STROKE => "auto"
          | NON_SCALING_STROKE => "non-scaling-stroke"

    fun shapeRenderingName r =
        case r of
            DEFAULT_RENDERING => "auto"
          | OPTIMIZE_SPEED => "optimizeSpeed"
          | CRISP_EDGES => "crispEdges"
          | GEOMETRIC_PRECISION => "geometricPrecision"
                                      
    fun propertyValueString p =
        case p of
            STROKE x => paintString x
          | STROKE_WIDTH x => realString x
          | STROKE_LINECAP x => lineCapName x
          | STROKE_LINEJOIN x => lineJoinName x
          | STROKE_DASHARRAY x => String.concatWith "," (map realString x)
          | STROKE_OPACITY x => realString x
          | FILL x => paintString x
          | FILL_RULE x => fillRuleName x
          | FILL_OPACITY x => realString x
          | OPACITY x => realString x
          | VECTOR_EFFECT e => vectorEffectName e
          | SHAPE_RENDERING r => shapeRenderingName r
          | FONT_FAMILY x => String.concatWith ", " x
          | FONT_STYLE x => fontStyleName x
          | FONT_WEIGHT x => Int.toString x
          | FONT_SIZE x => realString x
          | TRANSFORM tt => String.concatWith " " (map transformString tt)

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

    val header = 
        "<?xml version=\"1.0\" standalone=\"no\"?>\n" ^
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
                                                                              
    fun serialiseDocumentAtScale mmPerPixel ({ size, content } : svg) =
        let val scale = mmPerPixel * (9.6 / 2.54)
        in
            header ^
            "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" " ^ dimenAttrStringWithUnit "mm" size ^ ">\n" ^
            "<g transform=\"scale(" ^ realString scale ^ "," ^ realString scale ^ ")\">\n" ^
            serialiseContent content ^
            "</g>\n" ^
            "</svg>\n"
        end

    fun serialiseDocument ({ size, content } : svg) =
        header ^
        "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" " ^ dimenAttrString size ^ ">\n" ^
        serialiseContent content ^
        "</svg>\n"

end
                  
