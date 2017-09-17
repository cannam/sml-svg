
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
                                      
    datatype linecap = BUTT | ROUND | SQUARE
    datatype linejoin = MITRE | ROUND | BEVEL
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

    type svg = element decorated list (* !!! + width, height, metadata like title, etc *)
                                 
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

structure SvgSerialise = struct

    open Svg

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

    fun serialiseProperty prop = ""
                           
    fun serialiseProperties props =
        String.concatWith " " (map serialiseProperty props)

    and serialiseElementWith proptext elt =
        "<" ^ elementName elt ^ " " ^ proptext ^
        (case elt of
             GROUP content => ">" ^ serialiseContent content ^ 
                              "</" ^ elementName elt ^ ">"
           | other => "/>")

    and serialiseDecoratedElement (elt, props) =
        serialiseElementWith (serialiseProperties props) elt

    and serialiseContent svg =
        String.concatWith "\n" (List.map serialiseDecoratedElement svg) ^ "\n"

    fun serialiseDocument svg =
        "<?xml version=\"1.0\" standalone=\"no\"?>\n" ^
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ^
        "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" ^
        serialiseContent svg ^
        "</svg>\n"

end
                                 
open Svg
open SvgPathShorthand
         
val mypath = [ M (1.0, 2.0),
               l [(3.0, 0.0), (~1.0, 1.5)],
               Z
             ]

val mysvg = [(PATH mypath, [STROKE (RGB (1.0, 1.0, 0.0))]),
             (GROUP [(PATH [M (3.0, 4.0),
                            L [(0.0, 0.0)]], []),
                     (POLYLINE [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (0.0, 0.0)], [STROKE (RGB (0.0, 0.0, 1.0))]),
                     (TEXT { origin = (0.0, 0.0),
                             rotation = 0.0,
                             text = "Help me!" }, [FONT_FAMILY ["Helvetica"]])],
              [])]
                             
fun main () = print (SvgSerialise.serialiseDocument mysvg)
                  
