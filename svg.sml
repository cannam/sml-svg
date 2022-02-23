
structure Svg = struct

    type coords = real * real  (* x, y *)
    type dimens = real * real  (* width, height *)

    datatype 'a absrel =
             ABS of 'a |
             REL of 'a

    datatype stroke_form =
             MOVE_TO of coords |
             LINE_TO of coords list |
             HORIZONTAL_TO of real |
             VERTICAL_TO of real |
             CUBIC_TO of { control1: coords, control2: coords, target: coords } list |
             CUBIC_SMOOTH_TO of { control2: coords, target: coords } list |
             QUADRATIC_TO of { control: coords, target: coords } list |
             QUADRATIC_SMOOTH_TO of { target: coords } list |
             ARC_TO of { radii: dimens, rotation: real, largeArc: bool,
                         sweep: bool, target: coords } list |
             CLOSE_PATH

    type path_element = stroke_form absrel
                 
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
             FONT_SIZE of real |
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

    val M = Svg.ABS o Svg.MOVE_TO
    val m = Svg.REL o Svg.MOVE_TO
    val L = Svg.ABS o Svg.LINE_TO
    val l = Svg.REL o Svg.LINE_TO

    val H = Svg.ABS o Svg.HORIZONTAL_TO
    val h = Svg.REL o Svg.HORIZONTAL_TO
    val V = Svg.ABS o Svg.VERTICAL_TO
    val v = Svg.REL o Svg.VERTICAL_TO

    val C = Svg.ABS o Svg.CUBIC_TO
    val c = Svg.REL o Svg.CUBIC_TO
    val S = Svg.ABS o Svg.CUBIC_SMOOTH_TO
    val s = Svg.REL o Svg.CUBIC_SMOOTH_TO

    val Q = Svg.ABS o Svg.QUADRATIC_TO
    val q = Svg.REL o Svg.QUADRATIC_TO
    val T = Svg.ABS o Svg.QUADRATIC_SMOOTH_TO
    val t = Svg.REL o Svg.QUADRATIC_SMOOTH_TO

    val A = Svg.ABS o Svg.ARC_TO
    val a = Svg.REL o Svg.ARC_TO
                             
    val Z = Svg.ABS Svg.CLOSE_PATH
    val z = Svg.REL Svg.CLOSE_PATH

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

signature SVG_SERIALISER = sig

    type outstream
    
    val serialiseContent : outstream -> Svg.content -> unit
    val serialiseDocumentAtScale : outstream -> real -> Svg.svg -> unit
    val serialiseDocument : outstream -> Svg.svg -> unit
                                           
end

signature SVG_STRING_SERIALISER = sig

    val serialiseContent : Svg.content -> string
    val serialiseDocumentAtScale : real -> Svg.svg -> string
    val serialiseDocument : Svg.svg -> string
    
end
                               
functor SvgSerialiserFn (S : sig
                             type outstream
                             val output : outstream * string -> unit
                         end)
        :> SVG_SERIALISER where type outstream = S.outstream = struct

    type outstream = S.outstream

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
        if Real.isFinite r andalso
           Real.<= (Real.abs r, 1e6)
        then 
            let val epsilon = 1E~8
            in
                if Real.abs (r - Real.realRound r) < epsilon
                then StringInterpolate.I (Real.round r)
                else StringInterpolate.R r
            end
        else
            StringInterpolate.R r

    val R = realString
    val Q = "\""
    fun QR r = Q ^ R r ^ Q
    val SP = " "
    val COMMA = ","
                             
    fun writeOne out str =
        S.output (out, str)
           
    fun write out strings =
        List.app (fn s => S.output (out, s)) strings

    fun writeSequenceWith out writer separator [] = ()
      | writeSequenceWith out writer separator [x] =
        writer out x
      | writeSequenceWith out writer separator (x::xs) =
        (writer out x;
         writeOne out separator;
         writeSequenceWith out writer separator xs)

    fun writeList out writer values =
        writeSequenceWith out writer COMMA values

    fun writeSequence out writer values =
        writeSequenceWith out writer SP values
            
    fun writeSequenceElement out code writer values =
        (writeOne out code; writeSequence out writer values)

    fun writeReal out r =
        writeOne out (R r)
            
    fun writeCoords out (x, y) =
        write out [R x, COMMA, R y]

    fun writeCoordAttrs out (x, y) =
        write out ["x=", QR x, " ", "y=", QR y]
              
    fun writeDimenAttrsWithUnit out unit (w, h) =
        write out ["width=",  Q, R w, unit, Q, " ", "height=", Q, R h, unit, Q]

    fun writeDimenAttrs out =
        writeDimenAttrsWithUnit out ""

    fun writeCubic out { control1 = (x1, y1),
                         control2 = (x2, y2),
                         target = (x, y) } =
        write out [R x1, SP, R y1, COMMA,
                   R x2, SP, R y2, COMMA,
                   R x, SP, R y]

    fun writeCubicSmooth out { control2 = (x2, y2),
                               target = (x, y) } =
        write out [R x2, SP, R y2, COMMA,
                   R x, SP, R y]

    fun writeQuadratic out { control = (x1, y1),
                             target = (x, y) } =
        write out [R x1, SP, R y1, COMMA,
                   R x, SP, R y]

    fun writeQuadraticSmooth out { target = (x, y) } =
        write out [R x, SP, R y]

    fun writeArc out { radii = (rx, ry),
                       rotation, largeArc, sweep,
                       target = (x, y) } =
        write out [R rx, COMMA, R ry, SP, R rotation, SP,
                   (if largeArc then "1 " else "0 "),
                   (if sweep then "1 " else "0 "),
                   R x, COMMA, R y]
                                        
    fun escapeTextData str =
        String.translate (fn #"<" => "&lt;"
                           | #">" => "&gt;"
                           | #"&" => "&amp;"
                           | #"\n" => "&#10;"
                           | c => String.str c) str
                                                                        
    fun writePathElement out pe =
        let val w = writeOne out
            val seqElt = writeSequenceElement
        in
            case pe of
                ABS (MOVE_TO c) => (w "M"; writeCoords out c)
              | REL (MOVE_TO c) => (w "m"; writeCoords out c)
              | ABS (LINE_TO cc) => seqElt out "L" writeCoords cc
              | REL (LINE_TO cc) => seqElt out "l" writeCoords cc
              | ABS (HORIZONTAL_TO x) => (w "H"; writeReal out x)
              | REL (HORIZONTAL_TO x) => (w "h"; writeReal out x)
              | ABS (VERTICAL_TO y) => (w "V"; writeReal out y)
              | REL (VERTICAL_TO y) => (w "v"; writeReal out y)
              | ABS (CUBIC_TO pp) => seqElt out "C" writeCubic pp
              | REL (CUBIC_TO pp) => seqElt out "c" writeCubic pp
              | ABS (CUBIC_SMOOTH_TO pp) => seqElt out "S" writeCubicSmooth pp
              | REL (CUBIC_SMOOTH_TO pp) => seqElt out "s" writeCubicSmooth pp
              | ABS (QUADRATIC_TO pp) => seqElt out "Q" writeQuadratic pp
              | REL (QUADRATIC_TO pp) => seqElt out "q" writeQuadratic pp
              | ABS (QUADRATIC_SMOOTH_TO pp) => seqElt out "T" writeQuadraticSmooth pp
              | REL (QUADRATIC_SMOOTH_TO pp) => seqElt out "t" writeQuadraticSmooth pp
              | ABS (ARC_TO aa) => seqElt out "A" writeArc aa
              | REL (ARC_TO aa) => seqElt out "a" writeArc aa
              | ABS CLOSE_PATH => w "Z"
              | REL CLOSE_PATH => w "z"
        end
                           
    fun writeElementAttributes out e =
        let val w = writeOne out
            val seq = writeSequence
        in
            case e of
                PATH pp => (w " d="; w Q; seq out writePathElement pp; w Q)
              | RECT { origin, size } => (w SP; writeCoordAttrs out origin;
                                          w SP; writeDimenAttrs out size)
              | ROUNDED_RECT { origin, size,
                               radii } => (w SP; writeCoordAttrs out origin;
                                           w SP; writeDimenAttrs out size;
                                           write out [" rx=", QR (#1 radii),
                                                      " ry=", QR (#2 radii)])
              | CIRCLE { centre, radius } => write out [" cx=", QR (#1 centre),
                                                        " cy=", QR (#2 centre),
                                                        " r=", QR radius]
              | ELLIPSE { centre, radii } => write out [" cx=", QR (#1 centre),
                                                        " cy=", QR (#2 centre),
                                                        " rx=", QR (#1 radii),
                                                        " ry=", QR (#2 radii)]
              | LINE { source, target } => write out [" x1=", QR (#1 source),
                                                      " y1=", QR (#2 source),
                                                      " x2=", QR (#1 target),
                                                      " y2=", QR (#2 target)]
              | POLYLINE cc => (w " points="; w Q; seq out writeCoords cc; w Q)
              | POLYGON cc => (w " points="; w Q; seq out writeCoords cc; w Q)
              | TEXT { origin, rotation, text } => (w SP;
                                                    writeCoordAttrs out origin;
                                                    w " rotate=";
                                                    w (QR rotation))
              | GROUP _ => ()
        end
            
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

    fun writeTransformArgs out t =
        writeList out writeReal 
                  (case t of
                       MATRIX (a,b,c,d,e,f) => [a,b,c,d,e,f]
                     | TRANSLATE (x,y) => [x,y]
                     | SCALE (sx,sy) => [sx,sy]
                     | ROTATE r => [r]
                     | SKEW_X s => [s]
                     | SKEW_Y s => [s])
                            
    fun writeTransform out t =
        (writeOne out (transformName t ^ "(");
         writeTransformArgs out t;
         writeOne out ")")
                            
    fun rgbString (r, g, b) =
        StringInterpolate.interpolate
            "rgb(%1,%2,%3)"
            (map (fn c => StringInterpolate.I (Real.round (c * 255.0)))
                 [r, g, b])
                               
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
          | TRANSFORM tt => "" (* should have been dealt with separately by caller *)

    fun writeProperty out prop =
        (write out [propertyName prop, "=", Q];
         case prop of
             TRANSFORM tt => writeSequence out writeTransform tt
           | other => writeOne out (propertyValueString prop);
         writeOne out Q)

    fun writeProperties out [] = ()
      | writeProperties out props =
        (writeOne out SP;
         writeSequence out writeProperty props)

    and writeClosingTag out elt =
        write out ["</", elementName elt, ">"]
            
    and writeElementWith out properties elt =
        (write out ["<", elementName elt];
         writeElementAttributes out elt;
         writeProperties out properties;
         case elt of
             GROUP content => (writeOne out ">";
                               writeContent out content;
                               writeClosingTag out elt)
           | TEXT { text, ... } => (writeOne out ">";
                                    writeOne out (escapeTextData text);
                                    writeClosingTag out elt)
           | other => (writeOne out "/>")
        )

    and writeDecoratedElement out (elt, properties) =
        writeElementWith out properties elt

    and writeContent out svg =
        writeSequenceWith out writeDecoratedElement "" svg

    fun writeHeaderOpening out =
        write out ["<?xml version=\"1.0\" standalone=\"no\"?>",
                   "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">",
                   "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" "]

    val serialiseContent = writeContent
              
    fun serialiseDocumentAtScale out mmPerPixel ({ size, content } : svg) =
        let val scale = mmPerPixel * (9.6 / 2.54)
        in
            writeHeaderOpening out;
            writeDimenAttrsWithUnit out "mm" size;
            write out [">",
                       "<g transform=\"scale(",
                       realString scale,
                       COMMA,
                       realString scale,
                       ")\">"];
            writeContent out content;
            write out ["</g>",
                       "</svg>"]
        end

    fun serialiseDocument out ({ size, content } : svg) =
        (writeHeaderOpening out;
         writeDimenAttrs out size;
         writeOne out ">";
         writeContent out content;
         writeOne out "</svg>")
            
end

structure SvgStdoutSerialise :> sig

    val serialiseContent : Svg.content -> unit
    val serialiseDocumentAtScale : real -> Svg.svg -> unit
    val serialiseDocument : Svg.svg -> unit

          end = struct

    structure S = SvgSerialiserFn(struct
                                   type outstream = TextIO.outstream
                                   val output = TextIO.output
                                   end)

    val serialiseContent = S.serialiseContent TextIO.stdOut
    val serialiseDocumentAtScale = S.serialiseDocumentAtScale TextIO.stdOut
    val serialiseDocument = S.serialiseDocument TextIO.stdOut
             
end

structure SvgFileSerialise :> sig

    val serialiseContent : string -> Svg.content -> unit
    val serialiseDocumentAtScale : string -> real -> Svg.svg -> unit
    val serialiseDocument : string -> Svg.svg -> unit

          end = struct

    structure S = SvgSerialiserFn(struct
                                   type outstream = TextIO.outstream
                                   val output = TextIO.output
                                   end)

    fun serialiseContent filename =
        let val stream = TextIO.openOut filename
        in
            fn content => 
               S.serialiseContent stream content before
               TextIO.closeOut stream
        end

    fun serialiseDocumentAtScale filename =
        let val stream = TextIO.openOut filename
        in
            fn scale => fn document =>
               S.serialiseDocumentAtScale stream scale document before
               TextIO.closeOut stream
        end

    fun serialiseDocument filename =
        let val stream = TextIO.openOut filename
        in
            fn document => 
               S.serialiseDocument stream document before
               TextIO.closeOut stream
        end
end

structure SvgStringSerialise :> SVG_STRING_SERIALISER = struct
                         
    structure S = SvgSerialiserFn(struct
                                   type outstream = StringBuffer.buffer
                                   val output = StringBuffer.appendTo
                                   end)

    fun serialiseContent (content : Svg.content) : string =
        let val outstream = StringBuffer.new ()
        in
            S.serialiseContent outstream content;
            StringBuffer.toString outstream
        end
       
    fun serialiseDocumentAtScale (scale : real) (document : Svg.svg) : string =
        let val outstream = StringBuffer.new ()
        in
            S.serialiseDocumentAtScale outstream scale document;
            StringBuffer.toString outstream
        end
       
    fun serialiseDocument (document : Svg.svg) : string =
        let val outstream = StringBuffer.new ()
        in
            S.serialiseDocument outstream document;
            StringBuffer.toString outstream
        end
                                 
end
