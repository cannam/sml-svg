
type coords = real * real
type dimens = real * real

datatype 'a absrel =
         ABS of 'a |
         REL of 'a

datatype path_element =
         MOVE_TO of coords absrel |
         LINE_TO of coords list absrel |
         CUBIC_TO of { control1: coords, control2: coords, target: coords } list absrel |
         CUBIC_SMOOTH_TO of { control2: coords, target: coords } list absrel |
         QUADRATIC_TO of { control: coords, target: coords } list absrel |
         QUADRATIC_SMOOTH_TO of { target: coords } list absrel |
         ARC_TO of { radii: dimens, rotation: real, largeArc: bool,
                  sweep: bool, target: coords } list absrel |
         CLOSE_PATH

type path = path_element list

datatype shape =
         RECT of { origin: coords, size: dimens } |
         ROUNDED_RECT of { origin: coords, size: dimens, radii: dimens } |
         CIRCLE of { centre: coords, radius: real } |
         ELLIPSE of { centre: coords, radii: dimens } |
         LINE of { source: coords, target: coords } |
         POLYLINE of coords list |
         POLYGON of coords list

type text = { origin: coords, rotation: real, text: string }
                           
val mypath = [ MOVE_TO (ABS (1.0, 2.0)),
               LINE_TO (REL [(3.0, 0.0), (~1.0, 1.5)]),
               CLOSE_PATH
             ]
                         
fun main () = ()
                  
