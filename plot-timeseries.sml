
structure PlotTimeseries :> sig
    val plot : Svg.dimens -> real vector -> Svg.svg
end = struct

    fun extents (series : real vector) =
        Vector.foldl
            (fn (v, ee) =>
                case ee of
                    NONE => SOME (v, v)
                  | SOME (min, max) =>
                    SOME (if v < min then v else min,
                          if v > max then v else max))
            NONE
            series

    fun nothing size =
        { size = size, content = [] }
            
    fun plot (width, height) series =
        case extents series of
            NONE => nothing (width, height)
          | SOME (min, max) =>
            let val xscale = width / Real.fromInt (Vector.length series)
                val yscale = if Real.== (min, max) then 1.0
                             else height / (max - min)
                val (i, points) =
                    Vector.foldl
                        (fn (v, (i, points)) =>
                            (i + 1,
                             (xscale * Real.fromInt i,
                              yscale * (v - min)) :: points))
                        (0, [])
                        series
                val points = rev points
                open Svg
                open SvgPathShorthand
            in
                case points of
                    [] => nothing (width, height)
                  | first::rest =>
                    { size = (width, height),
                      content = [
                          (GROUP 
                               [(PATH [M first, L rest],
                                 [STROKE (RGB (1.0, 0.0, 0.0)),
                                  STROKE_WIDTH 1.0])],
                           [TRANSFORM
                                [MATRIX (1.0, 0.0, 0.0, ~1.0, 0.0, height)],
                            FILL NO_PAINT])
                      ]
                    }
        end
end
          
